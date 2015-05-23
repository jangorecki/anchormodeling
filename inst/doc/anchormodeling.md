
# Guide on Anchor Modeling in R

You should have already installed [anchormodeling](https://github.com/jangorecki/anchormodeling) R package. For configuring fresh linux environment see [Setup Guide](setup.md).  

## Define domain

Package already has working built-in example model for *Stage performances* domain. Use function `actor.am` to populate various iterations of the model. Use `actor.data` to populate example source data.  

In the document I will define basic model for git repository data.  
You can use git's *traveling in time* ability to easily produce data which evolves over time.  

## Extract data from git

In the working directory of the R session create subdir inside it git clone your repo.  
I will use [bitcoin](https://github.com/bitcoin/bitcoin) repo as it is a good example of distributed project: 300 contributors, 8400 commits, 4000 pull requests!

You can use shell script to generate source data directly from git repo.  

```sh
mkdir wd
cd wd
git clone https://github.com/bitcoin/bitcoin
cd bitcoin
# modified https://gist.github.com/textarcana/1306223
git log \
    --pretty=format:'{%n  "commit": "%H",%n  "author": "%an",%n  "author_email": "%ae",%n  "timestamp": "%at",%n  "message": "%f"%n},' \
    $@ | \
    perl -pe 'BEGIN{print "["}; END{print "]\n"}' | \
    perl -pe 's/},]/}]/' > ../commits.json
git log \
    --numstat \
    --format='%H' \
    $@ | \
    perl -lawne '
        if (defined $F[1]) {
            print qq#{"insertions": "$F[0]", "deletions": "$F[1]", "path": "$F[2]"},#
        } elsif (defined $F[0]) {
            print qq#],\n"$F[0]": [#
        };
        END{print qq#],#}' | \
    tail -n +2 | \
    perl -wpe 'BEGIN{print "{"}; END{print "}"}' | \
    tr '\n' ' ' | \
    perl -wpe 's#(]|}),\s*(]|})#$1$2#g' | \
    perl -wpe 's#,\s*?}$#}#' > ../numstats.json
cd ..
```

Make sure you navigate back to your `wd` directory and run R.  

```sh
R
```

Source data *json* files are ready to be picked up.  
Can be easiy loaded using [jsonlite](https://github.com/jeroenooms/jsonlite).  

```r
library(jsonlite)
extract.commits <- function(x){
    char_cols <- c("commit","author","author_email","message") # handle encoding
    setDT(fromJSON(x))[, `:=`(timestamp = as.POSIXct(as.integer(timestamp), origin="1970-01-01"))
                       ][, c(char_cols) := lapply(.SD, function(x) {Encoding(x) <- "unknown"; x}), .SDcols = char_cols]
}
extract.numstats <- function(x){
    char_cols <- c("commit","path") # handle encoding
    rbindlist(fromJSON(x), idcol = "commit")[insertions=="-", insertions := NA_character_
                                             ][deletions=="-", deletions := NA_character_
                                               ][, `:=`(insertions=as.integer(insertions), deletions=as.integer(deletions))
                                                 ][, c(char_cols) := lapply(.SD, function(x) {Encoding(x) <- "unknown"; x}), .SDcols = char_cols]
}

```

## Define Anchor Model

```r
library(anchormodeling)

am <- AM$new()
am$add$A(mne = "AU", desc = "Author")
am$add$a(mne = "NAM", desc = "Name", hist = TRUE, anchor = "AU")
am$add$a(mne = "EMA", desc = "Email", hist = TRUE, anchor = "AU")
am$add$A(mne = "BR", desc = "Branch")
am$add$A(mne = "CM", desc = "Commit")
am$add$a(mne = "TIM", desc = "Time", anchor = "CM")
am$add$a(mne = "MES", desc = "Message", anchor = "CM")
am$add$a(mne = "TAG", desc = "Tag", anchor = "CM")
am$add$a(mne = "HSH", desc = "Hash", anchor = "CM")
```

## Run AM Data Warehouse instance

```r
am$run()
```

## Load data to DW

You need to define mapping.  
Each symbol nested in below list substitutes the character scalar provided by user while defining mapping.  

```r
mapping <- list(anchor1_mne = list(natural_key_column_names,
                                   attr1_mne = column_name,
                                   attr2_mne = c(column_name, "hist" = historize_column_name),
                                   attr3_mne = c(column_name, "knot" = knot_mne),
                                   attr3_mne = c(column_name, "hist" = historize_column_name, "knot" = knot_mne)),
                anchor2_mne = list(natural_key_column_names,
                                   attr1_mne = column_name,
                                   attr2_mne = c(column_name, "hist" = historize_column_name),
                                   attr3_mne = c(column_name, "knot" = knot_mne),
                                   attr3_mne = c(column_name, "hist" = historize_column_name, "knot" = knot_mne)))
```

So on git data model.

```r
mapping <- list(CM = list("commit",
                          MES = "message",
                          TIM = "timestamp",
                          HSH = "commit"),
                AU = list(c("author","author_email"), # composite natural key
                          NAM = c("author", hist = "timestamp"),
                          EMA = c("author_email", hist = "timestamp")))
```

`meta` argument is used as processing batch metadata. Can be integer `meta` or list, see example.  

```r
commits <- extract.commits("commits.json")
am$load(mapping, commits, meta = 1L)
am$load(mapping, commits, meta = list(meta = 2L, user = "manual", src = "csv1")) # log custom details
```

Incremental loading.

```r
# numstat.json

meta <- 3L # incrementing batch
commit.filenames <- c("commits.json")
for(filename in commit.filenames){
    commits <- extract.commits(filename)
    am$load(mapping, commits, meta = list(meta=meta, user = "job1", src = filename))
    meta <- meta + 1L
}
```

We loaded first file triple. This is not a big deal for Anchor Model DW.  
If we would use *restatement* feature by `options("am.restatability" = FALSE)` globally or locally on create attributes/knots by `rest = FALSE` argument, then we can totally ignore such cases as no duplicates, including temporal duplicates, will be inserted.  

## Query data

3NF are not yet ready.  

```r
# am$view() # TO DO
```

## Query metadata

Check all the below commands.

```r
am
am$etl
am$log # you should notice your OS user name on `meta==2L`
am$IM()
```

## Save AM instance

```r
am$stop()
save(am, file = format(Sys.time(),"AM_%Y%m%d_%H%M%S.RData"))
```

Loading anchor model from the *RData* was not yet tested.

## Export model

```r
am$xml()
```

File can be loaded in official Anchor Modeler Test: [roenbaeck.github.io/anchor](http://roenbaeck.github.io/anchor/)  
Exported xml does not contain non-model fields like data types. Also it won't export restatability definition as it is part of metadata of model. This may be subject to change in Anchor Modeling itself, see [anchor#4](https://github.com/Roenbaeck/anchor/issues/4).  

## Questions

- Why it is so fast?  

[data.table](https://github.com/Rdatatable/data.table) design assumes from the early days to handle time series data well.  

  - fast joins
  - fast rolling joins
  - fast overlaping joins (range)

All of them are going to be employed in R session memory AM instance.  
You can use your favorite data analysis packages on top of AM 3NF views.  

- How to start?

Model any data, your model can non-destructivly grow over time so you can think about it later!  
In this doc you have example on loading git repo data. If you have some DWH data already, you can just use a minimal subset of it.  
I've found [video tutorial by Clint Huijbers](https://clinthuijbers.wordpress.com/2013/06/14/ssis-anchor-modeling-example-tutorial/) nice and simple.

- Stability of project?

In development, ready for testing.  
You are welcome to add your unit tests which can be included in automated unit testing on package build.  

- Waiting for more questions to add here

## Importants notes vs SQL anchor model

- data stored in-memory
- built-in ETL
- triggers are replaced by functions
- does not support *concurrent-reliance-temporal*
- built-in Identity Management
- built-in dashboard
- high performance data processing using [data.table](https://github.com/Rdatatable/data.table)
- easy multiple AM instances in R session

## Project road map

Writing it here as the Pull Requests are very welcome.  

- [ ] import anchor model from xml
- [ ] optional postgres db for data storage
- [ ] lot of unit tests
- [ ] idempotency
- [ ] better shiny dashboard
- [ ] concurrent-reliance-temporal
- [ ] parallel processing
