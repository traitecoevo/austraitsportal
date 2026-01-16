
<!-- README.md is generated from README.Rmd. Please edit that file -->

# austraits.portal

<!-- badges: start -->

<!-- badges: end -->

The goal of austraits.portal is to create a code-free interface for
users to access the AusTraits Plant Trait database

## To open the data portal locally

``` r
devtools::load_all()

open_data_portal()
```

## Deploy to shinyapps.io

App is deployed at https://unsw.shinyapps.io/austraits-portal/ with configuration 
details stored at `rsconnect/shinyapps.io/unsw/austraits.portal.dcf`.

To update deployment, open in Rstudio and run 

``` r
rsconnect::deployApp()
```
