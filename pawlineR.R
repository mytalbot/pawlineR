###############################################################################
#  pawlineR – Cat Breeding Management                                         #
#  v0.02
###############################################################################
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DBI)
library(RSQLite)
library(DT)
library(dplyr)
library(purrr)
library(ribd)
library(markdown)
library(igraph)
library(pedtools)
library(readxl)       
library(openxlsx)     

# ── database bootstrap (blank if new clone) ─────────────────────────────────
db_path <- file.path("db", "animals_db.sqlite")

if (!file.exists(db_path)) {
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  con_tmp <- dbConnect(SQLite(), db_path)
  
  dbExecute(con_tmp, "
    CREATE TABLE animals (
      numeric_id  INTEGER PRIMARY KEY AUTOINCREMENT,
      original_id TEXT UNIQUE,
      sex_code    INTEGER,
      inbreeding  REAL,
      added_at    TEXT
    );
  ")
  
  dbExecute(con_tmp, "
    CREATE TABLE pedigree (
      ped_id    INTEGER PRIMARY KEY AUTOINCREMENT,
      animal_id INTEGER,
      sire_id   INTEGER,
      dam_id    INTEGER
    );
  ")
  
  dbDisconnect(con_tmp)
}

con <- dbConnect(SQLite(), db_path)
onStop(function() dbDisconnect(con))

# ── helpers ─────────────────────────────────────────────────────────────────
loadAnimalsDB <- function() dbReadTable(con, "animals")

loadPedDataFromDB <- function() {
  sql <- "
    SELECT a.original_id AS child,
           s.original_id AS father,
           d.original_id AS mother,
           a.sex_code    AS sex_db
    FROM   animals a
    LEFT JOIN pedigree p ON p.animal_id = a.numeric_id
    LEFT JOIN animals  s ON p.sire_id   = s.numeric_id
    LEFT JOIN animals  d ON p.dam_id    = d.numeric_id
  "
  tmp <- dbGetQuery(con, sql)
  
  if (nrow(tmp) == 0)
    return(data.frame(
      id  = character(), fid = character(),
      mid = character(), sex = integer(),
      stringsAsFactors = FALSE))
  
  tmp$child  [tmp$child  == ""] <- NA
  tmp$father [tmp$father == ""] <- NA
  tmp$mother [tmp$mother == ""] <- NA
  tmp <- tmp[!is.na(tmp$child), ]
  tmp$sex <- ifelse(tmp$sex_db == 0, 0, tmp$sex_db)
  
  df <- tmp %>% select(id = child, fid = father, mid = mother, sex)
  addMissingFounders(df)
}

addMissingFounders <- function(df) {
  df$id  <- as.character(df$id)
  df$fid <- as.character(df$fid)
  df$mid <- as.character(df$mid)
  
  missing <- setdiff(unique(c(df$fid, df$mid)), df$id)
  missing <- missing[!is.na(missing)]
  
  if (length(missing) > 0) {
    founders <- data.frame(
      id  = missing,
      fid = NA_character_,
      mid = NA_character_,
      sex = 0L,
      stringsAsFactors = FALSE
    )
    df <- bind_rows(df, founders)
  }
  ## keep first occurrence *with* its data
  distinct(df, id, .keep_all = TRUE)
}

# NEW: helper to classify inbreeding coefficient into categories --------------
# Based on established genetic relationships (Reviewer 1, Comment 11)
classifyInbreeding <- function(f_value) {
  if (is.na(f_value) || f_value == 0) {
    return(list(category = "None", color = "#28a745", emoji = "\u2705"))
  } else if (f_value < 0.0625) {
    return(list(category = "Low", color = "#28a745", emoji = "\u2705"))
  } else if (f_value < 0.125) {
    return(list(category = "Moderate", color = "#ffc107", emoji = "\u26a0\ufe0f"))
  } else if (f_value < 0.25) {
    return(list(category = "High", color = "#fd7e14", emoji = "\u26a0\ufe0f"))
  } else {
    return(list(category = "Very High", color = "#dc3545", emoji = "\u274c"))
  }
}

# NEW: helper to fold planned offspring into a
#      pedigree‑style data frame --------------------------------------------
plannedToPed <- function(plan_df) {
  if (nrow(plan_df) == 0) {
    return(data.frame(
      id = character(), fid = character(), mid = character(), sex = integer()
    ))
  }
  plan_df %>%
    transmute(
      id  = child_name,
      fid = ifelse(father_label == "unknown", NA_character_, father_label),
      mid = ifelse(mother_label == "unknown", NA_character_, mother_label),
      sex = as.integer(child_sex_code)
    )
}

# ── custom CSS ──────────────────────────────────────────────────────────────
customCSS <- tags$head(tags$style(HTML("
  .main-header .logo   { height: 100px; line-height: 100px; }
  .main-header .navbar { min-height: 100px; }
  .main-sidebar .sidebar { padding-top: 30%; }
")))

# ── UI ----------------------------------------------------------------------
header <- dashboardHeader(
  title = tags$img(src = "logo.png", height = "100px")
)

sidebar <- dashboardSidebar(
  tags$div(style = "padding:10px;text-align:center;", tags$h3("pawlineR")),
  sidebarMenu(id = "tabs",
              menuItem("Breeding Management",  tabName = "breeding",   icon = icon("paw")),
              menuItem("Instructions",         tabName = "instructions", icon = icon("book")),
              menuItem("Disclaimer & License", tabName = "disclaimer", icon = icon("exclamation-triangle")),
              menuItem("Import / Export Data", tabName = "io",         icon = icon("exchange-alt")),
              menuItem("Purge Database",       tabName = "purge",      icon = icon("trash-alt"))
  )
)

body <- dashboardBody(
  customCSS,
  tabItems(
    # ── breeding ------------------------------------------------------------
    tabItem(tabName = "breeding",
            fluidPage(
              titlePanel("Cat Breeding Management"),
              tabsetPanel(id = "breedingTabs",
                          tabPanel("Master Data",            br(), DTOutput("table_animals")),
                          tabPanel("Pedigrees",              br(), DTOutput("table_pedigrees")),
                          tabPanel("Pedigree Visualisation",
                                   h3("Pedigree for the selected animal"),
                                   plotOutput("pedPlot", height = "600px")
                          ),
                          tabPanel("Breeding Plan",
                                   fluidRow(
                                     column(4,
                                            h3("Create New Pairing"),
                                            selectizeInput("selFather", "Father:", choices = NULL),
                                            selectizeInput("selMother", "Mother:", choices = NULL),
                                            textInput("txtChildName", "New Animal Name (ID):", ""),
                                            radioButtons("rdoChildSex",
                                                         "Sex (1 = male, 2 = female, 0 = unknown):",
                                                         choices  = c("male" = 1, "female" = 2, "unknown" = 0),
                                                         selected = 0
                                            ),
                                            actionButton("btnAddPairing", "Create Pairing")
                                     ),
                                     column(8,
                                            h3("Planned Offspring (in-memory)"),
                                            DTOutput("table_planned"), br(),
                                            actionButton("btnCommit",        "Confirm Selected Animal"), br(), br(),
                                            actionButton("btnResetPlanned", "Reset Offspring Table",
                                                         class = "btn-warning")
                                     )
                                   )
                          )
              )
            )
    ),
    
    # ── static markdown -----------------------------------------------------
    tabItem(tabName = "instructions",
            fluidPage(h2("Instructions"), includeMarkdown("instructions.md"))
    ),
    tabItem(tabName = "disclaimer",
            fluidPage(h2("Disclaimer & License"), includeMarkdown("disclaimer.md"))
    ),
    
    # ── import / export -----------------------------------------------------
    tabItem(tabName = "io",
            fluidPage(
              h2("Import / Export Data"),
              h3("Export current database"),
              downloadButton("dlExcel", "Download .xlsx"),
              downloadButton("dlCSV",   "Download .csv"),
              tags$hr(),
              h3("Import .xlsx file"),
              fileInput("upExcel", "Choose Excel workbook", accept = ".xlsx"),
              actionButton("btnImport", "Import now", class = "btn-primary"),
              verbatimTextOutput("importStatus")
            )
    ),
    
    # ── purge ---------------------------------------------------------------
    tabItem(tabName = "purge",
            fluidPage(
              h2("Purge Database"),
              p("This will delete ALL animals and pedigree records and cannot be undone."),
              actionButton("btnPurge", "Purge now", class = "btn-danger"),
              verbatimTextOutput("purgeStatus")
            )
    )
  )
)

# ── NEW footer --------------------------------------------------------------
footer <- tags$footer(
  class = "main-footer",
  style = "padding:8px 0;width:100%;text-align:left;font-weight:600;",
  HTML("pawlineR&copy; 2025  v0.01")
)

# ── Assemble the page -------------------------------------------------------
ui <- tagList(                        #  ⬅️ wrap the dashboard in a tagList
  dashboardPage(header, sidebar, body, skin = "blue"),
  footer                               #  ⬅️ the new footer element
)


# ── server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # reactive caches ---------------------------------------------------------
  animalsDB  <- reactiveVal(loadAnimalsDB())
  rv_peddata <- reactiveVal(loadPedDataFromDB())
  planned_df <- reactiveVal(
    data.frame(child_name     = character(),
               child_sex_code = integer(),
               father_label   = character(),
               mother_label   = character(),
               inbreeding     = numeric(),
               stringsAsFactors = FALSE)
  )
  
  # dynamic empty-db banner --------------------------------------------------
  output$purgeStatus <- renderText({
    if (nrow(animalsDB()) == 0) {
      "✅ Database is empty"
    } else {
      ""
    }
  })
  
  # pedigree helper ---------------------------------------------------------
  buildPedSubgraph <- function(df, rootId) {
    visited <- character(0)
    edges   <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
    queue   <- rootId
    while (length(queue) > 0) {
      current <- queue[1]; queue <- queue[-1]
      if (!current %in% visited) {
        visited <- c(visited, current)
        row_ch <- df[df$id == current, , drop = FALSE]
        if (nrow(row_ch) > 0) {
          father <- row_ch$fid[1]; mother <- row_ch$mid[1]
          if (!is.na(father) && father != current) {
            edges <- rbind(edges, data.frame(from = father, to = current))
            if (!father %in% visited) queue <- c(queue, father)
          }
          if (!is.na(mother) && mother != current) {
            edges <- rbind(edges, data.frame(from = mother, to = current))
            if (!mother %in% visited) queue <- c(queue, mother)
          }
        }
      }
    }
    edges <- unique(edges)
    all_nodes <- unique(c(edges$from, edges$to, rootId))
    if (length(all_nodes) == 0)
      return(make_empty_graph(n = 1, directed = TRUE))
    graph_from_data_frame(edges, directed = TRUE,
                          vertices = data.frame(name = all_nodes))
  }
  
  # master-data table -------------------------------------------------------
  output$table_animals <- renderDT({
    df <- animalsDB()
    if (nrow(df) == 0)
      return(datatable(data.frame(Notice = "No master data available."),
                       options = list(dom = "t")))
    ped_df <- rv_peddata()
    df$Actions <- sapply(df$original_id, function(x) {
      paste(
        as.character(actionButton(
          inputId = paste0("edit_", x), label = "Edit",
          onclick = sprintf("Shiny.setInputValue('edit_click','%s',{priority:'event'})", x)
        )),
        as.character(actionButton(
          inputId = paste0("del_",  x), label = "Delete",
          onclick = sprintf("Shiny.setInputValue('delete_click','%s',{priority:'event'})", x)
        ))
      )
    })
    canPlotPed <- function(orig_id) {
      if (nrow(ped_df) == 0) return(FALSE)
      row_p <- ped_df[ped_df$id == orig_id, ]
      nrow(row_p) > 0 && all(!is.na(row_p$fid), !is.na(row_p$mid))
    }
    df$Pedigree <- sapply(df$original_id, function(x) {
      if (canPlotPed(x))
        sprintf("<button class='btn btn-default'
                 onclick=\"Shiny.setInputValue('pedView','%s',{priority:'event'})\">
                 <i class='fa fa-sitemap'></i></button>", x)
      else ""
    })
    base_cols <- setdiff(names(df), c("added_at", "Actions", "Pedigree"))
    if ("added_at" %in% names(df))
      df <- df[, c(base_cols, "added_at", "Actions", "Pedigree")]
    else
      df <- df[, c(base_cols, "Actions", "Pedigree")]
    datatable(df, escape = FALSE, selection = "none",
              options = list(pageLength = 10))
  })
  
  # pedigree plot -----------------------------------------------------------
  observeEvent(input$pedView, {
    chosenID <- input$pedView; req(chosenID)
    
    ## combine stored + planned – so the plot can already show uncommitted cats
    df_ped <- bind_rows(rv_peddata(), plannedToPed(planned_df()))
    if (nrow(df_ped) == 0) {
      showNotification("No pedigree data available.", type = "error"); return()
    }
    
    g_sub <- buildPedSubgraph(df_ped, chosenID)
    
    ## collect inbreeding & sex for all vertices ----------------------------
    adf  <- animalsDB()
    pldf <- planned_df()
    getInb <- function(animalID) {
      rowA <- adf[adf$original_id == animalID, ]
      if (nrow(rowA) > 0) return(rowA$inbreeding[1])
      rowP <- pldf[pldf$child_name == animalID, ]
      if (nrow(rowP) > 0) return(rowP$inbreeding[1])
      NA
    }
    getSex <- function(animalID) {
      row1 <- df_ped[df_ped$id == animalID, ]
      if (nrow(row1) > 0) return(as.integer(row1$sex[1]))
      0L
    }
    V(g_sub)$inb <- sapply(V(g_sub)$name, getInb)
    V(g_sub)$sex <- sapply(V(g_sub)$name, getSex)
    
    output$pedPlot <- renderPlot({
      if (gorder(g_sub) == 1 && gsize(g_sub) == 0) {
        plot.new(); text(0.5, 0.5, paste("No ancestors found for:", chosenID))
      } else {
        ## improved layout: Sugiyama for layered (almost-tree) graphs --------
        coords <- layout_with_sugiyama(g_sub)$layout
        coords[, 2] <- max(coords[, 2]) - coords[, 2]  # root at top
        
        ## vertex shapes & colours by sex -----------------------------------
        sex_vec   <- V(g_sub)$sex
        shape_vec <- ifelse(sex_vec == 1, "square",
                            ifelse(sex_vec == 2, "circle", "circle"))
        colour_vec <- ifelse(sex_vec == 1, "lightblue",
                             ifelse(sex_vec == 2, "pink", "lightgrey"))
        
        node_labels <- paste0(V(g_sub)$name,
                              ifelse(is.na(V(g_sub)$inb),
                                     "", paste0("\nF = ", round(V(g_sub)$inb, 3))))
        
        plot(g_sub, layout = coords,
             vertex.shape = shape_vec,
             vertex.color = colour_vec,
             vertex.frame.color = "black",
             vertex.size = 40,
             vertex.label = node_labels,
             vertex.label.color = "black",
             vertex.label.cex = 1.1,
             edge.arrow.size = 0.8,
             main = paste("Pedigree of:", chosenID))
      }
    })
    
    updateTabsetPanel(session, "breedingTabs",
                      selected = "Pedigree Visualisation")
  })
  
  # edit handlers -----------------------------------------------------------
  observeEvent(input$edit_click, {
    sel_orig_id <- input$edit_click
    df <- animalsDB()
    sel_row <- df[df$original_id == sel_orig_id, ]
    if (nrow(sel_row) == 0) {
      showNotification("Animal not found.", type = "error"); return()
    }
    current_numeric <- sel_row$numeric_id[1]
    showModal(modalDialog(
      title = paste("Edit Animal:", sel_orig_id),
      textInput("edit_name", "New Name (original_id):",
                value = sel_row$original_id[1]),
      selectInput("edit_sex", "Sex (0 = unknown, 1 = male, 2 = female):",
                  choices = c("0" = 0, "1" = 1, "2" = 2),
                  selected = sel_row$sex_code[1]),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirmEdit", "Save")),
      easyClose = TRUE
    ))
    session$userData$edit_id <- current_numeric
  })
  
  observeEvent(input$confirmEdit, {
    req(session$userData$edit_id)
    dbExecute(con,
              "UPDATE animals SET original_id = ?, sex_code = ?
       WHERE numeric_id = ?",
              params = list(input$edit_name,
                            as.integer(input$edit_sex),
                            session$userData$edit_id))
    removeModal()
    session$userData$edit_id <- NULL
    animalsDB(loadAnimalsDB())
    rv_peddata(loadPedDataFromDB())
  })
  
  # pedigree table ----------------------------------------------------------
  output$table_pedigrees <- renderDT({
    dummy <- animalsDB()
    sql <- "
      SELECT p.ped_id,
             a.original_id AS Child,
             s.original_id AS Sire,
             d.original_id AS Dam
      FROM pedigree p
      LEFT JOIN animals a ON p.animal_id = a.numeric_id
      LEFT JOIN animals s ON p.sire_id   = s.numeric_id
      LEFT JOIN animals d ON p.dam_id    = d.numeric_id
    "
    datatable(dbGetQuery(con, sql),
              options = list(pageLength = 10), rownames = FALSE)
  })
  
  # select-box refresh ------------------------------------------------------
  observe({
    ## include new (planned) animals so you can select them as parents
    df_ped <- bind_rows(rv_peddata(), plannedToPed(planned_df()))
    if (nrow(df_ped) == 0) {
      updateSelectizeInput(session, "selFather",
                           choices = c("Unknown" = "NA"), selected = "NA")
      updateSelectizeInput(session, "selMother",
                           choices = c("Unknown" = "NA"), selected = "NA")
      return()
    }
    all_ids  <- sort(unique(df_ped$id))
    plan_ids <- planned_df()$child_name
    choice_lbls <- sapply(all_ids, function(x)
      if (x %in% plan_ids) paste0(x, " (new)") else x)
    choices <- c("Unknown" = "NA", setNames(all_ids, choice_lbls))
    updateSelectizeInput(session, "selFather", choices = choices, selected = "NA")
    updateSelectizeInput(session, "selMother", choices = choices, selected = "NA")
  })
  
  # btnAddPairing -----------------------------------------------------------
  observeEvent(input$btnAddPairing, {
    ## existing and planned pedigree
    df_ped      <- rv_peddata()
    df_planned  <- planned_df()
    df_combined <- bind_rows(df_ped, plannedToPed(df_planned))   # NEW
    
    child_name <- input$txtChildName
    child_sex  <- as.integer(input$rdoChildSex)
    selFather  <- input$selFather
    selMother  <- input$selMother
    
    if (child_name == "") {
      showNotification("Please enter a new animal name (ID).", type = "error"); return()
    }
    if (child_name %in% df_combined$id || child_name %in% df_planned$child_name) {
      showNotification("This name (ID) already exists.", type = "error"); return()
    }
    
    father_known <- selFather != "NA"
    mother_known <- selMother != "NA"
    if (father_known && mother_known && selFather == selMother) {
      showNotification("Father and mother cannot be the same animal.", type = "error"); return()
    }
    if (xor(father_known, mother_known)) {
      showNotification("Specify either both parents or none.", type = "error"); return()
    }
    
    father_id <- if (father_known) selFather else NA_character_
    mother_id <- if (mother_known) selMother else NA_character_
    
    ## check sexes also among *planned* animals  ----------------------------
    getSexOf <- function(id) {
      if (is.na(id)) return(0L)
      row_comb <- df_combined[df_combined$id == id, ]
      if (nrow(row_comb) == 0) return(0L)
      row_comb$sex[1]
    }
    if (father_known && getSexOf(father_id) == 2) {
      showNotification("Selected father is female.", type = "error"); return()
    }
    if (mother_known && getSexOf(mother_id) == 1) {
      showNotification("Selected mother is male.", type = "error"); return()
    }
    
    new_row <- data.frame(
      id = child_name, fid = father_id, mid = mother_id,
      sex = child_sex, stringsAsFactors = FALSE
    )
    
    df_ext <- addMissingFounders(bind_rows(df_combined, new_row))   # NEW: includes *all*
    ## build pedtools object
    ped_obj <- tryCatch(
      pedtools::ped(id  = df_ext$id,
                    fid = df_ext$fid,
                    mid = df_ext$mid,
                    sex = df_ext$sex),
      error = function(e) { showNotification(e$message, type = "error"); NULL }
    )
    if (is.null(ped_obj)) return()
    
    inb_val <- tryCatch(
      inbreeding(ped_obj, ids = child_name),
      error = function(e) { showNotification(e$message, type = "error"); NA_real_ }
    )
    
    ## store in planned table + refresh -------------------------------------
    new_plan <- data.frame(
      child_name     = child_name,
      child_sex_code = child_sex,
      father_label   = if (father_known) father_id else "unknown",
      mother_label   = if (mother_known) mother_id else "unknown",
      inbreeding     = round(inb_val, 4),
      stringsAsFactors = FALSE
    )
    planned_df(bind_rows(planned_df(), new_plan))
    
    ## clear input widgets
    updateTextInput(session, "txtChildName", value = "")
    updateRadioButtons(session, "rdoChildSex", selected = 0)
    
    # Classify inbreeding and show color-coded notification (Reviewer 1, Comment 11)
    f_class <- classifyInbreeding(inb_val)
    showNotification(
      sprintf("%s New animal '%s' added. F = %.4f (%s)",
              f_class$emoji, child_name, inb_val, f_class$category),
      type = if (f_class$category %in% c("High", "Very High")) "warning" else "message"
    )
  })

  # reset planned -----------------------------------------------------------
  observeEvent(input$btnResetPlanned, {
    planned_df(planned_df()[0, ])
    showNotification("Planned offspring table has been reset.", type = "message")
  })
  
  # table_planned -----------------------------------------------------------
  # Enhanced with F-value interpretation column (Reviewer 1, Comment 11)
  output$table_planned <- renderDT({
    df <- planned_df()
    if (nrow(df) == 0)
      return(datatable(data.frame(Notice = "No offspring planned."),
                       options = list(dom = "t")))

    # Add interpretation column with color-coded categories
    df$F_interpretation <- sapply(df$inbreeding, function(f) {
      f_class <- classifyInbreeding(f)
      sprintf('<span style="color:%s;font-weight:bold;">%s %s</span>',
              f_class$color, f_class$emoji, f_class$category)
    })

    # Reorder columns to show interpretation after inbreeding
    col_order <- c("child_name", "child_sex_code", "father_label",
                   "mother_label", "inbreeding", "F_interpretation")
    df <- df[, col_order]

    datatable(df, selection = "single", escape = FALSE,
              options = list(pageLength = 5),
              colnames = c("Name", "Sex", "Father", "Mother", "F", "Risk Level"))
  })
  
  # commit planned ----------------------------------------------------------
  observeEvent(input$btnCommit, {
    sel <- input$table_planned_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showNotification("Select an animal in the table first.", type = "error"); return()
    }
    df_plan <- planned_df()
    selectedRow <- df_plan[sel, ]
    showModal(modalDialog(
      title = "Confirm Adoption",
      paste("Do you want to permanently add", selectedRow$child_name, "to the database?"),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirmCommit", "Confirm"))
    ))
  })
  
  observeEvent(input$confirmCommit, {
    removeModal()
    sel <- isolate(input$table_planned_rows_selected)
    if (is.null(sel) || length(sel) == 0) return()
    df_plan     <- planned_df()
    selectedRow <- df_plan[sel, ]
    
    child_name <- selectedRow$child_name
    child_sex  <- selectedRow$child_sex_code
    father_id  <- if (selectedRow$father_label == "unknown") NA else selectedRow$father_label
    mother_id  <- if (selectedRow$mother_label == "unknown") NA else selectedRow$mother_label
    inb_val    <- selectedRow$inbreeding
    
    dbExecute(con,
              "INSERT INTO animals (original_id, sex_code, inbreeding, added_at)
       VALUES (?,?,?,?)",
              params = list(child_name, child_sex, inb_val,
                            format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    
    new_id <- dbGetQuery(con, "SELECT last_insert_rowid() AS new_id")$new_id[1]
    
    getNumericId <- function(orig_id) {
      if (is.null(orig_id) || is.na(orig_id)) return(NA)
      res <- dbGetQuery(con,
                        "SELECT numeric_id FROM animals WHERE original_id = ?",
                        params = list(orig_id))
      if (nrow(res) == 0) NA else res$numeric_id[1]
    }
    
    dbExecute(con,
              "INSERT INTO pedigree (animal_id, sire_id, dam_id) VALUES (?,?,?)",
              params = list(new_id,
                            if (is.na(father_id)) NULL else getNumericId(father_id),
                            if (is.na(mother_id)) NULL else getNumericId(mother_id)))
    
    showNotification(paste("Animal", child_name, "has been added to the database."),
                     type = "message")
    
    planned_df(df_plan[-sel, ])
    animalsDB(loadAnimalsDB())
    rv_peddata(loadPedDataFromDB())
  })
  
  # delete handlers ---------------------------------------------------------
  observeEvent(input$delete_click, {
    sel_id <- input$delete_click
    showModal(modalDialog(
      title = "Confirm Deletion",
      paste("Do you really want to delete", sel_id, "?"),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirmDelete", "Delete"))
    ))
  })
  
  observeEvent(input$confirmDelete, {
    removeModal()
    sel_id <- isolate(input$delete_click)
    res <- dbGetQuery(con,
                      "SELECT numeric_id FROM animals WHERE original_id = ?",
                      params = list(sel_id))
    if (nrow(res) == 0) {
      showNotification("Animal not found.", type = "error"); return()
    }
    numeric_id <- res$numeric_id[1]
    dbExecute(con, "DELETE FROM animals  WHERE numeric_id = ?", params = list(numeric_id))
    dbExecute(con, "DELETE FROM pedigree WHERE animal_id = ?", params = list(numeric_id))
    showNotification(paste("Animal", sel_id, "has been deleted."), type = "message")
    animalsDB(loadAnimalsDB()); rv_peddata(loadPedDataFromDB())
  })
  
  # CSV export --------------------------------------------------------------
  output$dlCSV <- downloadHandler(
    filename = function() sprintf("MasterData-%s.csv", Sys.Date()),
    content  = function(file) {
      sql <- "
        SELECT a.original_id,
               a.inbreeding,
               s.original_id AS sire_id,
               d.original_id AS dam_id
        FROM animals a
        LEFT JOIN pedigree p ON a.numeric_id = p.animal_id
        LEFT JOIN animals s  ON p.sire_id   = s.numeric_id
        LEFT JOIN animals d  ON p.dam_id    = d.numeric_id
      "
      write.csv(dbGetQuery(con, sql), file, row.names = FALSE)
    }
  )
  
  # Excel export ------------------------------------------------------------
  output$dlExcel <- downloadHandler(
    filename = function() sprintf("pawlineR-%s.xlsx", Sys.Date()),
    content  = function(file) {
      animals <- dbReadTable(con, "animals")
      ped_raw <- dbReadTable(con, "pedigree")
      id_map <- setNames(animals$original_id, animals$numeric_id)
      pedigree <- ped_raw %>%
        transmute(
          child_id = id_map[as.character(animal_id)],
          sire_id  = id_map[as.character(sire_id)],
          dam_id   = id_map[as.character(dam_id)]
        )
      wb <- createWorkbook()
      addWorksheet(wb, "animals")
      writeData(wb, "animals",
                animals %>% select(original_id, sex_code, inbreeding))
      addWorksheet(wb, "pedigree")
      writeData(wb, "pedigree", pedigree)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # Excel import (crash-proof) ---------------------------------------------
  observeEvent(input$btnImport, {
    req(input$upExcel)
    tmp <- input$upExcel$datapath
    
    animals_df <- tryCatch(
      read_excel(tmp, "animals"),
      error = function(e) NULL
    )
    pedigree_df <- tryCatch(
      read_excel(tmp, "pedigree"),
      error = function(e) NULL
    )
    
    if (is.null(animals_df) || is.null(pedigree_df)) {
      output$importStatus <- renderText(
        "❌ Workbook must contain sheets named 'animals' and 'pedigree'")
      return()
    }
    
    needA <- c("original_id", "sex_code")
    needP <- c("child_id")
    if (!all(needA %in% names(animals_df)) ||
        !all(needP %in% names(pedigree_df))) {
      output$importStatus <- renderText("❌ Missing mandatory columns")
      return()
    }
    
    dbExecute(con, "BEGIN")
    tryCatch({
      
      dbExecute(con, "DELETE FROM pedigree")
      dbExecute(con, "DELETE FROM animals")
      
      pwalk(animals_df, function(original_id, sex_code, inbreeding, ...) {
        dbExecute(con,
                  "INSERT INTO animals (original_id, sex_code, inbreeding, added_at)
           VALUES (?,?,?,datetime('now'))",
                  params = list(original_id,
                                as.integer(sex_code),
                                as.numeric(inbreeding)))
      })
      
      id_tbl <- dbGetQuery(con,
                           "SELECT numeric_id, original_id FROM animals")
      id_map <- setNames(id_tbl$numeric_id, id_tbl$original_id)
      idLookup <- function(x) {
        if (is.na(x) || x == "") NA_integer_ else unname(id_map[x])
      }
      
      pwalk(pedigree_df, function(child_id, sire_id, dam_id, ...) {
        dbExecute(con,
                  "INSERT INTO pedigree (animal_id, sire_id, dam_id) VALUES (?,?,?)",
                  params = list(
                    idLookup(child_id),
                    idLookup(sire_id),
                    idLookup(dam_id)))
      })
      
      dbExecute(con, "COMMIT")
      animalsDB(loadAnimalsDB())
      rv_peddata(loadPedDataFromDB())
      planned_df(planned_df()[0, ])
      output$importStatus <- renderText("✅ Import finished")
      
    }, error = function(e) {
      dbExecute(con, "ROLLBACK")
      output$importStatus <- renderText(paste("❌ Import failed:", e$message))
    })
  })
  
  # purge database ----------------------------------------------------------
  observeEvent(input$btnPurge, {
    showModal(modalDialog(
      title   = "Confirm purge",
      "Are you absolutely sure you want to delete ALL data?",
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirmPurge", "Delete everything",
                                    class = "btn-danger")),
      easyClose = FALSE
    ))
  })
  
  observeEvent(input$confirmPurge, {
    removeModal()
    dbExecute(con, "DELETE FROM pedigree")
    dbExecute(con, "DELETE FROM animals")
    animalsDB(loadAnimalsDB())
    rv_peddata(loadPedDataFromDB())
    planned_df(planned_df()[0, ])
    showNotification("Database purged.", type = "message")
    updateTabItems(session, "tabs", "breeding")
  })
}

shinyApp(ui, server)
