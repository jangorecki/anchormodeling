# Anchor Modeling [![Build Status](https://travis-ci.org/jangorecki/anchormodeling.svg?branch=master)](https://travis-ci.org/jangorecki/anchormodeling)

**Current version:** [0.4.1](NEWS.md)  

- [x] Anchor Model metadata manager
- [x] Export model to XML loadable in [Anchor Modeler](https://roenbaeck.github.io/anchor/)
- [x] In-memory AM Data Warehouse instances
- [x] High performance engine thanks to [data.table](https://github.com/Rdatatable/data.table/wiki)
- [x] Web based dashboard thanks to [shiny](http://shiny.rstudio.com)
- [x] Built-in Identity Management to generate surrogate keys

Anchor Modeling Data Warehouse instance supported features:  

- [x] unitemporal
- [x] views: *current*, *latest*, *timepoint*, *difference*
- [ ] restatement / idempotency
- [x] dynamically shared knots
- [ ] 2+ anchor ties
- [x] high level ui
  - map source columns to AM entities
  - query 3NF views
  - views output mirrors postgres anchor model sql views
  - auto identitiy management for anchors and knots
  - auto loading knots
  - auto metadata stamping
  - web overview dashboard: views, cube, pivoting, ETL logs

## Installation

You need to have [R](http://www.r-project.org/) installed. You need to have R `devtools` package installed: `install.packages("devtools")`. On windows it requires [Rtools](http://cran.r-project.org/bin/windows/Rtools/).  

```r
library(devtools)
## install if not installed:
#install_github("Rdatatable/data.table")
#install_github("smartinsightsfromdata/rpivotTable")
install_github("jangorecki/anchormodeling")
```

Deployment script on fresh environment [Setup Guide](inst/doc/setup.md), useful for Cloud or VM.  

## How to use

See [introduction doc](inst/doc/anchormodeling.md) which will go through all features of anchor modeling in R.

## License

GPL-3  

## Contact

`J.Gorecki@wit.edu.pl`
