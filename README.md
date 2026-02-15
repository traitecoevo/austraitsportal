
<!-- README.md is generated from README.Rmd. Please edit that file -->

# austraits.portal

<!-- badges: start -->

<!-- badges: end -->

The goal of austraits.portal is to create a code-free interface for
users to access the AusTraits Plant Trait database. The portal is
visible at

<https://unsw.shinyapps.io/austraits-portal/>

## About AusTraits

The AusTraits database is a comprehensive collection of plant trait data
for Australian flora. It includes measurements of various traits such as
leaf area, plant height, seed mass, and more, collected from a wide
range of sources including published literature, field studies, and
herbarium records. The database is designed to support research in
ecology, evolution, and conservation by providing standardized trait
data for thousands of plant species across Australia.

TODO: Lizzy to complete, including acknowledgements and links to the
main AusTraits repository and website.

## To open the data portal locally

``` r
pkgload::load_all()

shiny::shinyApp(ui = app_ui, server = app_server)
```

## Deploying to shinyapps.io

App is deployed at <https://unsw.shinyapps.io/austraits-portal/> with
configuration details stored at
`rsconnect/shinyapps.io/unsw/austraits.portal.dcf`.

To update deployment, open in RStudio and run:

``` r
rsconnect::deployApp()
```

Dependencies are managed via the file `manifest.json`. To update
dependencies, run:

``` r
rsconnect::writeManifest()
```

Note that successful installation requires that the `austraits.portal`
package itself first be installed from GitHub:

``` r
remotes::install_github("traitecoevo/austraits.portal@develop")
```

## App Design Overview

### Architecture

The AusTraits Data Portal is built as a modular Shiny application
inspired by the {golem} framework. The application provides an
interactive interface to explore and download trait data from the
AusTraits database.

#### Core Components

**Data Layer** - **Storage**: Data stored as Parquet files for efficient
querying - **Query Engine**: DuckDB in-memory database for fast
filtering and aggregation - **Two Datasets**: - Raw observations
(individual measurements) - Species averages (aggregated means per
species) - **Arrow Integration**: Arrow datasets registered with DuckDB
for zero-copy data access

**UI Structure** (`app_ui.R`) - **Sidebar**: Filtering controls via
`mod_filters_ui` - **Main Panel**: Tabbed interface with 5 views: - Data
Preview (`mod_data_table`) - App Information (`mod_app_info`) - Taxon
View (`mod_taxon_view`) - Trait View (`mod_trait_view`) - Citations
(`mod_citations`)

**Server Logic** (`app_server.R`) - Reactive data flow coordinating
filters, queries, and display - Debounced filtering to reduce
computational overhead - Lazy data loading (100 rows initially, load
more on demand) - Cached computations using `memoise` for performance

### Key Modules

| Module | Purpose | Key Features |
|----|----|----|
| **mod_filters** | User filter controls | Taxonomy, traits, location, custom filters |
| **mod_data_table** | Interactive data table | Sortable, paginated, DT with truncated cells |
| **mod_taxon_view** | Taxon profile pages | Species info, trait summary, external links |
| **mod_trait_view** | Trait profile pages | Trait definitions, distributions, maps |
| **mod_citations** | Citation information | Dynamic reference generation for filtered data |
| **mod_app_info** | Portal documentation | Usage guide, attribution, telemetry metrics |

### Data Processing Pipeline

1.  **Filter Parsing** (`fct_filter_parsing.R`)
    - Converts UI inputs into structured filter objects
    - Validates and normalizes filter values
2.  **Filter Application** (`fct_filter_application.R`)
    - Applies filters to DuckDB queries
    - Optimized single-pass filtering
    - Supports regex patterns for text searches
3.  **Query Execution**
    - Initial load: First 100 rows for display
    - Background: Count total matching rows
    - On-demand: Load additional batches as needed
4.  **Data Display**
    - Format columns for presentation
    - Generate interactive visualizations
    - Cache expensive computations

### Performance Optimizations

- **DuckDB**: High-performance analytical queries on Parquet files
- **Lazy Loading**: Only loads visible data (100 rows at a time)
- **Memoization**: Caches repeated computations (trait groups, dropdown
  values)
- **Debouncing**: Delays filter execution until user input settles
  (300-1000ms)
- **Precomputed Dropdowns**: All filter options cached at startup
- **Efficient String Matching**: DuckDB’s optimized regex for filtering

### Data Preparation

The `prepare_data_for_portal()` function (in `fct_prepare_data.R`)
processes the raw AusTraits database for portal use:

- Flattens nested database structure
- Computes species-level averages for core traits
- Exports to Parquet format (display and full versions)
- Generates cached metadata (definitions, sources, trait groups,
  dropdown values)
- Pre-processes flora links and state/territory distributions

### Telemetry

Optional analytics tracking via Supabase or local SQLite: - Session
starts - Search events (when filters applied) - Download events -
Displayed in real-time on App Information tab

### URL Parameters

The app supports deep linking via URL query parameters: -
`?taxon_name=Eucalyptus+globulus` - `?trait_name=leaf_area` -
`?tab=Trait+View` - All filter states can be encoded in URLs for sharing
