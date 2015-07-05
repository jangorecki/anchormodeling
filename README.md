# Anchor Modeling [![Build Status](https://travis-ci.org/jangorecki/anchormodeling.svg?branch=master)](https://travis-ci.org/jangorecki/anchormodeling)

**Current version:** [0.3.9](NEWS.md)  

- [x] Anchor Model metadata manager.  
- [x] Export model to XML loadable in [Anchor Modeler](https://roenbaeck.github.io/anchor/).
- [x] In-memory AM Data Warehouse instances.
- [x] Built-in Identity Management to generate surrogate keys.
- [x] High performance engine thanks to [data.table](https://github.com/Rdatatable/data.table/wiki).

Anchor Modeling Data Warehouse instance supported features:  

- [x] unitemporal
- [ ] views: current, last, point-in-time, difference
- [ ] restatement/idempotency
- [x] dynamically shared knots
- [ ] 2+ role ties
- [x] high level ui
  - map source columns to AM entities
  - auto identitiy management for anchors and knots
  - auto loading knots
  - auto metadata stamping
  - query 3NF views
  - anchor model and etl overview dashboard in shiny

## Installation

For configuring fresh linux environment see [Setup Guide](inst/doc/setup.md).

```r
library(devtools)
install_github("jangorecki/anchormodeling")
```

## How to use

See [introduction doc](inst/doc/anchormodeling.md) which will go through all features of anchor modeling in R.

## License

GPL-3  

## Contact

`J.Gorecki@wit.edu.pl`
