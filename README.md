# 🐾 pawlineR

**Cat Breeding Management Shiny App**  
Version 0.9002 — July 2025

pawlineR is an open‑source, non‑commercial Shiny application that helps small catteries manage animals, plan matings, calculate inbreeding coefficients, and visualise pedigrees — all in a lightweight, local SQLite database that is created automatically on first run.

> **Disclaimer** — pawlineR is **research / hobby software** and provides no professional or veterinary advice. See the full disclaimer in [`LICENSE`](LICENSE).

---

## ✨Features

| Module | What it does |
|--------|--------------|
| **Master Data** | CRUD interface for all animals (ID, sex, inbreeding, actions). |
| **Pedigrees** | Table of child–sire–dam relations. |
| **Pedigree Visualisation** | Interactive tree with per‑node inbreeding values. |
| **Breeding Plan** | Offline pairing planner with inbreeding preview before committing. |
| **Import / Export** | One‑click Excel/CSV export and Excel bulk import. |
| **Purge** | Wipes the local database (dev & testing convenience). |

---

## 🚀 Quick Start

```r
# 1. Clone the repository
$ git clone https://github.com/<your‑username>/pawlineR.git
$ cd pawlineR

# 2. Open R (or RStudio) and install dependencies once
install.packages(c("shiny", "shinydashboard", "shinyjs", "DBI", "RSQLite",
                   "DT", "dplyr", "purrr", "ribd", "igraph", "pedtools",
                   "readxl", "openxlsx", "markdown"))

# 3. Run the app
shiny::runApp()
```

The first launch creates `db/animals_db.sqlite` and boots with an empty dataset. 
You’re ready to start adding cats 🐱.

---

## 🛠 Project Structure

```text
pawlineR/
├── app.R             # Main Shiny script (UI + server)
├── LICENSE           # FUNCSA licence & disclaimer
├── README.md         # You’re reading it
├── db/               # Auto‑generated SQLite database (git‑ignored)
├── www/              # Static assets (logo.png, custom CSS, …)
└── docs/             # Markdown docs (instructions.md, etc.)
```

---

## 🤝 Contributing

1. Fork → feature branch → pull request.  
2. Honour the **Pull‑Request Obligation** in the FUNCSA licence (§4).  
3. Use the GitHub issue tracker for bugs or feature requests.

Styling guide: tidyverse format, 80‑column soft wrap, snake_case variable names in R.

---

## 🔒 Licence

**Free‑Use‑Non‑Commercial‑Share‑Alike Licence (FUNCSA) v1.0** — see [`LICENSE`](LICENSE) for the full text.

Commercial use requires prior written consent: <talbot.steven@mh‑hannover.de>.

---

## 🧑 💻 Author & Contact

**Steven R. Talbot** — Hannover Medical School (MHH)  
<talbot.steven@mh‑hannover.de>

Feel free to reach out with questions, collaboration ideas, or just to say hi!

---

## 🗓 Changelog

* **0.9002** (2025‑07‑01) – Initial public release.

---

