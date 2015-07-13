
# Setup guide for non-R users

Below guide is designed for Ubuntu. It will likely work on similar distributions.

## Install R

```sh
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
sudo add-apt-repository 'deb  http://cran.stat.ucla.edu/bin/linux/ubuntu trusty/'
sudo apt-get update
sudo apt-get -y install r-base-dev libcurl4-gnutls-dev libxml2-dev libssl-dev git
```

Git is optional, but suggested if you want to follow *Guide to Anchor Modeling in R* or use it against your own repo.  

## Install R packages

Run R by single letter command in shell.

```sh
R
```

Install R packages.

```r
install.packages(c("R6","devtools","shinydashboard","microbenchmark","DT"), repos="http://cran.stat.ucla.edu")
# if asked accept installing to personal library
library(devtools)
install_github("Rdatatable/data.table")
install_github("smartinsightsfromdata/rpivotTable")
install_github("jangorecki/anchormodeling")
```

Your R environment is already configured.  

## Install IDE

If you are going not only execute scripts but also write them then the IDE is helpful.  
RStudio is undoubtedly best IDE for R.  

- If you use windows or linux with GUI, like Ubuntu desktop, you can install RStudio Desktop.
- If you use cli linux without GUI like Ubuntu server then you can install RStudio Server and access IDE remotely from web browser over http.

[RStudio download page](http://www.rstudio.com/products/RStudio/)

## Anchor Modeling in R

There is a [separate document](anchormodeling.md) which will go through the full process of anchor modeling in R.
