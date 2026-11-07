# Growth Rate Estimation and Trend Analysis — build notes, boot test & TODO

New RAISINS module created on **2026-11-04** by cloning the greyskull-based
`rm-anova-oneway` app and swapping its ANOVA engine for a growth-rate / trend
engine. It reuses RM-ANOVA's Firebase / login / invoice / feedback / SSO config
(as requested, "for now use firebase and all of RMANOVA").

## What this module does
Time-series analysis for yearly **Area / Production / Yield** data:

- **Growth rates** — Compound Growth Rate (log-linear CGR), endpoint CAGR, average annual growth.
- **Trend fitting** — linear / quadratic / cubic / exponential, ranked by adjusted R², with a fitted-trend figure.
- **Trend test** — non-parametric Mann-Kendall + Theil-Sen slope with CI.
- **Instability** — CV and the Cuddy-Della Valle Index (CDVI).
- **Area–Yield decomposition** — splits production change into area, yield and interaction effects (auto-detects columns named like `Area` / `Production`; yield is derived as Production/Area so the effects sum exactly to the total change).
- **Interpretation** — auto-written plain-language summary.
- **Download report** — Word (`styles.docx`) / PDF / HTML via `rmarkdown::render`, with every table, the figure and the interpretation.

All statistics are **base R** — no new package installs beyond RM-ANOVA's `renv.lock`.

## Files I wrote / changed
- `dir/gre_ui.R` — analysis tab UI (NEW)
- `dir/gre_server.R` — analysis engine + View Data + FAQs + logout + report download (NEW)
- `growth_rate_report.Rmd` — params-driven report (NEW)
- `global.R` — config = Growth Rate; sources `gre_*` instead of the ANOVA engine; AI sources dropped
- `ui.R` — `analysis = gre_ui()`, RA-One tab dropped (`ra_one` omitted)
- `server.R` — calls `gre_server()`; invoice wired to `rcbd_file1` / `rcbd_submit` / `demo_data_select`
- `data/dataset1-3.csv` — coconut time-series demo data (replaced the ANOVA samples)
- `dir/rcbd_dataset_*.R` — Datasets tab text/links updated for the coconut series
- `.Rprofile` — made `renv/activate.R` sourcing tolerant (renv/ not copied)

Reused unchanged from RM-ANOVA: the greyskull shell, `dir/core_aliases.R`,
`dir/doe_presentation.R` (table/font styles the report uses), the "Arrange Data"
CSV creator (`rcbd_gen_*`), invoice, feedback, SSO, `styles.docx`, `www/`.

## Boot test (do this in RStudio, where pandoc + renv are available)
1. Open the project in RStudio. Restore packages: `renv::restore()`.
2. Install greyskull (>= 1.0.3) where this R can see it, e.g. from the local
   source: `remotes::install_local("../RAISINS-Core/.../greyskull")` or
   `remotes::install_github("STATOBERRY/greyskull")`.
3. `shiny::runApp(".", port = 8793, launch.browser = TRUE)`.
4. Pass = log prints `Listening on ...` with no R error; Welcome renders; navbar
   shows green **G R E**; tabs Analysis / Arrange Data / Datasets / Feedback /
   User / Quick Tour / View Data / FAQs appear.
5. Smoke test the Analysis tab: pick demo "Coconut – Production & Area", set Year =
   `Year`, Series = Area/Production/Yield, tick all analyses, Run. Then Download
   Report in each format.

Headless note: login / Firebase / invoice / feedback / report-render were **not**
run here (no pandoc / greyskull / Firebase secrets on the command line). The R
syntax of every file was checked and the statistics were verified on the demo
data (CGR 2.37 %/yr, MK τ=0.945, decomposition shares sum to 100 %).

## TODO before public deployment
- [ ] **Dedicated identity/Firebase.** `global.R` currently reuses RM-ANOVA's
  `app_id`, `individual_db` (`RMAOW_Zap/Sheet2`), `module_code`, `website_page`
  (`RMAOW.html`), `logo_url`, `subscribe_url`, `demo_video_url`. Swap these for
  Growth-Rate values + a new Firebase node + a raisins.live handoff page.
- [ ] **RA-One AI assistant** is not wired in v1 (its chat server was invoked from
  the old ANOVA engine). To enable: port `dir/AI/*` to a growth-rate chat context,
  call the chat server from `gre_server`, add `ra_one = chat_ui(...)` in `ui.R`,
  restore `RA-One` to `protected_tabs`, and set the OpenAI key.
- [ ] **Quick Tour** steps are not defined for this module (the retake-tour tab is
  inert). Add `raisins_run_tour(session, steps = ...)` in `gre_server` if wanted.
- [ ] `service_account.json` was copied from RM-ANOVA and is gitignored — replace
  with the new app's credentials (or keep shared intentionally).
- [ ] Replace the demo CSVs with the Coconut Development Board's real figures.
- [ ] Minor stats caveat: exponential-vs-polynomial ranking mixes adj R² on log and
  original scales; fine for guidance, flag if you want a stricter criterion.
