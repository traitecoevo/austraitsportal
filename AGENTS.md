# austraits.portal — agent & contributor guide

`austraits.portal` is an R package providing a **Shiny interface to the AusTraits Plant Traits
database** — a code-free web portal for exploring and downloading trait data, deployed at
<https://unsw.shinyapps.io/austraits-portal/>.

## Repo-local guidance

- **App entry point:** `app.R` runs `pkgload::load_all(".")` then
  `shiny::shinyApp(ui = app_ui, server = app_server)`. The top-level UI/server live in
  `R/app_ui.R` and `R/app_server.R`.
- **Modular Shiny (golem-style):** functionality is split into `mod_*` modules (e.g.
  `mod_filters`, `mod_data_table`, `mod_taxon_view`, `mod_trait_view`, `mod_citations`,
  `mod_app_info`), `fct_*` business logic (`fct_filter_parsing`, `fct_filter_application`,
  `fct_prepare_data`), `srv_*` server helpers, and `utils_*` helpers — all under `R/`.
- **Data layer:** trait data is stored as Parquet and queried via DuckDB/Arrow. Bundled data
  lives in `inst/extdata/austraits/` — a small `austraits-5.0.0-lite` set ships in the repo; the
  `austraits-7.0.0-full` set is prepared locally (see README "Data Preparation").
- **Data prep:** `prepare_data_for_portal()` (in `R/fct_prepare_data.R`) flattens the AusTraits
  database, computes species averages, exports Parquet, and caches metadata/dropdowns.
- **Config:** `config.yml` (via the `config` package); deployment manifest in `manifest.json`.
- **Tests:** `tests/testthat/` (testthat edition 3) + `spelling`; see `tests/TEST_COVERAGE.md`.

Dev follows the standard R-package workflow plus a Shiny launch step: `devtools::load_all()` /
`pkgload::load_all()`, then `shiny::shinyApp(ui = app_ui, server = app_server)` (or just source
`app.R`); `devtools::test()`; `devtools::check()`. Default development branch is `develop`.

> Heads-up: the app needs prepared data files to run — only the `lite` set is in the repo, so a
> fresh checkout shows the demo subset until you build the full set. Deployment to shinyapps.io
> (`rsconnect::deployApp()`) requires `austraits.portal` itself to be installed from GitHub first
> (`remotes::install_github("traitecoevo/austraits.portal@develop")`) and dependencies tracked in
> `manifest.json` (`rsconnect::writeManifest()`).

---

## AusTraits family — cross-package context

`austraits.portal` is part of the **AusTraits family** (a subset of the
[`traitecoevo`](https://github.com/traitecoevo) org) — here, the AusTraits web portal (interactive
data exploration/access). Family-wide concerns are documented centrally in
**[austraits-meta](https://github.com/traitecoevo/austraits-meta)** — don't restate them here, read
them there:

- **Start with [`AGENTS.md`](https://github.com/traitecoevo/austraits-meta/blob/main/AGENTS.md)** —
  pipeline order, who owns what, dependency direction, source-of-truth rules, cross-boundary
  artifacts, gotchas.
- **[`dependencies.yml`](https://github.com/traitecoevo/austraits-meta/blob/main/dependencies.yml)** —
  machine-readable package graph + cross-boundary artifacts.
- **[`governance/`](https://github.com/traitecoevo/austraits-meta/tree/main/governance)** —
  label taxonomy, board #9 conventions, release playbooks, triage.

**Filing issues:** the whole family is tracked on one board,
[AusTraits #9](https://github.com/orgs/traitecoevo/projects/9) (new issues auto-add to it). Follow
the [issue & labelling guide](https://github.com/traitecoevo/austraits-meta/blob/main/governance/issue-guide.md):
pick one work-type label (`bug` / `task` / `epic`); Status and Priority are set on the board, not as
labels.

> austraits-meta is hand-maintained prose — a map, not ground truth. Verify specifics against the
> actual repos.
