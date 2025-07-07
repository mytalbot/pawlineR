# 1 · Welcome to pawlineR

**pawlineR** is a Shiny-based electronic stud-book and “what-if” simulator for 
(domestic/institutional-) cat breeding.  
The app remembers every animal in a local SQLite file, draws pedigrees on request, 
calculates Wright’s inbreeding coefficient on the fly, and lets you rehearse new 
matings in a risk-free sandbox. Nothing is written to disk until you decide to commit.

First start-up is deliberately blank: no demo data is pre-loaded. Add founders
manually, import an Excel workbook, or restore a backup CSV – it is up to you.

---

# 2 · Your Command Centre – the Sidebar

The menu on the left follows you everywhere. Each entry opens a dedicated workspace:

**Breeding Management** – the hub for daily work. All master data, pedigrees, 
visual graphs and the mating sandbox live here.<sup><a href="#fn1">1</a></sup>  
**Instructions** – this very document, rendered inside the app so you never have 
to switch windows.  
**Disclaimer & Licence** – the legal fine print that shows who built what and 
what you are allowed to do with it.  
**Import / Export Data** – one page that covers Excel and CSV backups in both directions.  
**Purge Database** – a last-resort red zone; hit the button, confirm, and every
row in both tables is deleted. The app instantly shows “✅ Database is empty” at the top of that page until you add new cats.

<p id="fn1"><sup>1 </sup>Yes, we know that the pedigree visualisation is awful 
and not yet finished. But this isn’t the main focus of this application anyway.</p>

---

# 3 · Life inside Breeding Management

Open the hub and you will see four subtabs along the top.

## Master Data

A searchable, sortable table that lists every cat stored so far. Inline **Edit**
and **Delete** buttons appear at the end of each row, and a tiny sitemap icon 
shows up whenever both parents are known. Click the icon to jump straight to a
pedigree graph.

## Pedigrees

A raw relationship table (one row per child with sire and dam IDs). 
It is handy for quick audits, spotting gaps and copying IDs into external reports.

## Pedigree Visualisation

A full ancestry tree drawn with **igraph**. The selected cat sits at the bottom; 
ancestors rise upward. Each node shows the ID and the stored inbreeding coefficient.
Use your browser’s right-click menu to save the plot as a PNG.

## Breeding Plan

A sandbox that lives only in memory. Choose a father and mother from the 
drop-downs (or leave both as “Unknown” for founders), give the kitten an ID and 
a sex, then press **Create Pairing**. The virtual kitten appears in a separate 
table with an automatically calculated F-value. When you are satisfied, select 
the row and hit **Confirm Selected Animal** to move the kitten into the real database.
If you change your mind, press **Reset Offspring Table** to clear the sandbox.

---

# 4 · A Typical Day with pawlineR

Start by reviewing the Master Data list, fixing any typos or wrong sex codes.  
Move to Breeding Plan and draft one or more matings. The moment you create a pairing, 
pawlineR checks for obvious errors: the same cat cannot be both parents; you may not
specify only one parent; fathers must be male, mothers female. The live inbreeding 
coefficient helps you decide whether the cross is acceptable.

After validation you either confirm the best kittens – which stores them permanently
and updates all pedigree links – or you reset the sandbox and try again. Before 
closing the app, switch to Import / Export Data and download an Excel or CSV snapshot; 
you now have a human-readable backup should anything go wrong tomorrow.

---

# 5 · Importing and Exporting

The Import / Export Data page offers two download buttons and one upload workflow.

**Download .xlsx** writes two sheets named “animals” and “pedigree”. The animals 
sheet contains original ID, sex code and stored F-value; the pedigree sheet maps 
each child to sire and dam IDs.  
**Download .csv** flattens the same information into a single comma-separated file.  
**Import now** expects a workbook with those exact sheet names. Missing sheets or 
columns trigger a clear error message instead of crashing the app. During import
pawlineR wraps all inserts in a database transaction – if anything fails the entire
operation is rolled back and your previous data remain intact. 
A green “✅ Import finished” line appears when everything succeeds.

---

# 6 · Purging the Database

Need a clean slate? Visit **Purge Database**, press **Purge now** and confirm in 
the modal dialog. Both tables are erased, the in-memory sandbox is cleared, and 
you are redirected to Breeding Management with a fresh “No master data available” 
notice. The status line “✅ Database is empty” remains visible on the Purge page 
until at least one new cat is stored or imported.

---

# 7 · Understanding the Columns

Every table uses the same naming conventions:

* **original_id** – your preferred label: call-name, registry number, or both.  
* **sex_code** – 0 unknown, 1 male, 2 female.  
* **inbreeding** – Wright’s F-coefficient; founders show blank.  
* **added_at** – automatic timestamp or the text “MasterData” for legacy rows.  
* **fid / mid / sire_id / dam_id** – father and mother IDs. Empty if unknown or not yet recorded.

The sandbox table mirrors these fields under slightly different names 
(`child_name`, `father_label`, etc.) to emphasise that nothing is final until confirmed.

---

# 8 · What Happens Behind the Curtain

All data live in `db/animals_db.sqlite`, a file that ships empty in a fresh clone.
The animals table stores static attributes and the latest F-value; the pedigree 
table links each kitten (by numeric key) to its numeric sire and dam keys. 
Whenever you create a virtual pairing, pawlineR assembles a temporary pedigree 
with `pedtools::ped()`, runs `inbreeding()` for the child, and rounds the result 
to four decimals.

Pedigree plots use a breadth-first walk (`buildPedSubgraph`) to collect ancestors,
then `igraph::layout_as_tree()` for a tidy top-down drawing. 
Reactivity is handled by three `reactiveVal` objects – one for the animals table
on disk, one for the pedigree view, and one for the in-memory sandbox – 
so the UI refreshes instantly without reading from SQLite on every click.

---

# 9 · Troubleshooting in Plain Language

If the app refuses to create a pairing, double-check that the father is male 
(sex = 1), the mother is female (sex = 2), and that you did not select the same ID twice.  
If the Import button shows a red error, open the workbook and make sure you kept 
the sheet names “animals” and “pedigree” and did not rename or delete the mandatory columns.  
If you see the green “Database is empty” banner unexpectedly, verify that 
`animals_db.sqlite` is still in the `db/` folder and has not been deleted or 
replaced. Re-import a backup if necessary.

---

# 10 · Final Notes

pawlineR never phones home; everything you type stays on your own machine unless
you deploy the app to a public server. Feel free to fork the repository, 
translate the UI, or extend the database schema – the code is under a permissive 
licence as explained in the Disclaimer page.

Have fun improving your lines and keep an eye on that F-value!
