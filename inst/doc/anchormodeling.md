
# Guide on Anchor Modeling in R

You should have already installed [anchormodeling](https://github.com/jangorecki/anchormodeling) R package. For configuring fresh linux environment see [Setup Guide](setup.md).  

## Define domain

Inside the package there is already working example model for *Stage performances* domain, see `?actor.am`, `?actor.data` to populate model/data.  

In this document I will define basic model for git repository data.  
You can use git's *traveling in time* ability to easily produce data which evolves over time.  
You can follow example locally on your machines.  

## Extract data from git

In the working directory create subdir for git clone your repo.  
I will use [bitcoin](https://github.com/bitcoin/bitcoin) repo as it is a good example of distributed project: 300 contributors, 8400 commits, 4000 pull requests.

You can use shell script to generate source data directly from git repo.  

Initial load to DW.  
Limited by `--before="2014-01-01"`.   

```sh
mkdir wd
cd wd
git clone https://github.com/bitcoin/bitcoin
cd bitcoin
# modified https://gist.github.com/textarcana/1306223
git log \
    --before="2014-01-01" \
    --pretty=format:'{%n  "commit": "%H",%n  "author": "%an",%n  "author_email": "%ae",%n  "timestamp": "%at",%n  "message": "%f"%n},' \
    $@ | \
    perl -pe 'BEGIN{print "["}; END{print "]\n"}' | \
    perl -pe 's/},]/}]/' > ../commits2013.json
```

Data for incremental loading of DW.  
Limited by `--before="2015-01-01"`.  

```sh
git log \
    --before="2015-01-01" \
    --pretty=format:'{%n  "commit": "%H",%n  "author": "%an",%n  "author_email": "%ae",%n  "timestamp": "%at",%n  "message": "%f"%n},' \
    $@ | \
    perl -pe 'BEGIN{print "["}; END{print "]\n"}' | \
    perl -pe 's/},]/}]/' > ../commits2014.json
```

Model evolution.  
Add individual file insertions/deletions details - `git log --numstat`.  

```sh
git log \
    --before="2015-01-01" \
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
    perl -wpe 's#,\s*?}$#}#' > ../numstats2014.json
```



In new session navigate to your `wd` directory and run R.    

```sh
R
```

Source data *json* files are ready to be picked up.  
Can be easiy loaded using [jsonlite](https://github.com/jeroenooms/jsonlite).  


```r
#library(devtools)
#load_all()
library(anchormodeling)
library(jsonlite)
```


```r
extract.commits <- function(x){
    stopifnot(file.exists(x))
    char_cols <- c("commit","author","author_email","message") # handle encoding
    setDT(fromJSON(x))[, `:=`(timestamp = as.POSIXct(as.integer(timestamp), origin="1970-01-01"))
                       ][, c(char_cols) := lapply(.SD, function(x) {Encoding(x) <- "unknown"; x}), .SDcols = char_cols]
}
```


```r
extract.numstats <- function(x){
    stopifnot(file.exists(x))
    char_cols <- c("commit","path") # handle encoding
    rbindlist(fromJSON(x), idcol = "commit")[insertions=="-", insertions := NA_character_
                                             ][deletions=="-", deletions := NA_character_
                                               ][, `:=`(insertions=as.integer(insertions), deletions=as.integer(deletions))
                                                 ][, c(char_cols) := lapply(.SD, function(x) {Encoding(x) <- "unknown"; x}), .SDcols = char_cols]
}
```

## Define Anchor Model


```r
am <- AM$new()
am$add$A(mne = "AU", desc = "Author")
am$add$a(mne = "NAM", desc = "Name", hist = TRUE, anchor = "AU")
am$add$a(mne = "EMA", desc = "Email", anchor = "AU")
am$add$A(mne = "CM", desc = "Commit")
am$add$a(mne = "TIM", desc = "Time", anchor = "CM")
am$add$a(mne = "MES", desc = "Message", anchor = "CM")
am$add$a(mne = "TAG", desc = "Tag", anchor = "CM")
am$add$a(mne = "HSH", desc = "Hash", anchor = "CM")
am$add$t(anchors = c("AU","CM"), roles = c("authoring","of"), identifier = c(1,Inf))
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
                                   attr3_mne = column_name,
                                   attr4_mne = c(column_name, "hist" = historize_column_name)),
                anchor2_mne = list(natural_key_column_names,
                                   attr1_mne = column_name,
                                   attr2_mne = c(column_name, "hist" = historize_column_name),
                                   attr3_mne = column_name,
                                   attr4_mne = c(column_name, "hist" = historize_column_name)),
                tie_mne1_mne2_mne2 = list("knot" = knot_column_name,
                                          "hist" = historize_column_name))
```

If somebody wants to learn new function function for everything then it can used by `A()` for anchors and `a()` for attributes.  

```r
mapping <- list(Amne = A(natural_key_column_names,
                         attr_mne = a(column_name, hist = "historize_column_name"),
                         ...),
                ...)
```

On our simple git repo data model.


```r
mapping <- list(CM = list("commit",
                          MES = "message",
                          TIM = "timestamp",
                          HSH = "commit"),
                AU = list("author_email", # natural key
                          NAM = c("author", hist = "timestamp"),
                          EMA = "author_email"))
```

`meta` argument is used as processing batch metadata. Can be integer `meta` or list, see example.  


```r
commits1 <- extract.commits("commits2013.json")
am$load(mapping, commits1, meta = 1L)
am$load(mapping, commits1, meta = list(meta = 2L, user = "manual", src = "git log")) # log custom details
```

Duplicate inserts were handled automatically.  
To controling temporal duplicates use *restatement* feature when create *attributes*/*knots* by `rest = FALSE` argument or at start `options("am.restatability" = FALSE)` globally.  

## Query data

3NF are not yet ready.  


```r
am$view("AU", type = "current") # default type
```

```
##      AU_ID Metadata_AU                                AU_EMA_Author_Email
##   1:     1           1                                      greg@xiph.org
##   2:     2           1                                    git@bluematt.me
##   3:     3           1                        thomas.holenstein@gmail.com
##   4:     4           1                                   laanwj@gmail.com
##   5:     5           1                          phil.kaufmann@t-online.de
##  ---                                                                     
## 199:   199           1 gavinandresen@1a98c847-1fd6-4fd8-948a-caf3550aa51b
## 200:   200           1    s_nakamoto@1a98c847-1fd6-4fd8-948a-caf3550aa51b
## 201:   201           1                                   satoshin@gmx.com
## 202:   202           1       laszloh@1a98c847-1fd6-4fd8-948a-caf3550aa51b
## 203:   203           1      sirius-m@1a98c847-1fd6-4fd8-948a-caf3550aa51b
##      Metadata_AU_EMA       AU_NAM_Author_Name    AU_NAM_ChangedAt
##   1:               1          Gregory Maxwell 2013-12-29 11:14:06
##   2:               1             Matt Corallo 2013-12-17 23:03:43
##   3:               1        Thomas Holenstein 2013-12-21 15:54:43
##   4:               1 Wladimir J. van der Laan 2013-12-24 09:06:10
##   5:               1          Philip Kaufmann 2013-12-20 17:58:15
##  ---                                                             
## 199:               1            gavinandresen 2010-12-17 20:16:27
## 200:               1               s_nakamoto 2010-12-15 22:43:51
## 201:               1         Satoshi Nakamoto 2010-08-28 01:55:50
## 202:               1                  laszloh 2010-08-04 05:15:38
## 203:               1                 sirius-m 2010-02-04 15:31:46
##      Metadata_AU_NAM
##   1:               1
##   2:               1
##   3:               1
##   4:               1
##   5:               1
##  ---                
## 199:               1
## 200:               1
## 201:               1
## 202:               1
## 203:               1
```

```r
am$view("AU", type = "latest")
```

```
##      AU_ID Metadata_AU                                AU_EMA_Author_Email
##   1:     1           1                                      greg@xiph.org
##   2:     2           1                                    git@bluematt.me
##   3:     3           1                        thomas.holenstein@gmail.com
##   4:     4           1                                   laanwj@gmail.com
##   5:     5           1                          phil.kaufmann@t-online.de
##  ---                                                                     
## 199:   199           1 gavinandresen@1a98c847-1fd6-4fd8-948a-caf3550aa51b
## 200:   200           1    s_nakamoto@1a98c847-1fd6-4fd8-948a-caf3550aa51b
## 201:   201           1                                   satoshin@gmx.com
## 202:   202           1       laszloh@1a98c847-1fd6-4fd8-948a-caf3550aa51b
## 203:   203           1      sirius-m@1a98c847-1fd6-4fd8-948a-caf3550aa51b
##      Metadata_AU_EMA       AU_NAM_Author_Name    AU_NAM_ChangedAt
##   1:               1          Gregory Maxwell 2013-12-29 11:14:06
##   2:               1             Matt Corallo 2013-12-17 23:03:43
##   3:               1        Thomas Holenstein 2013-12-21 15:54:43
##   4:               1 Wladimir J. van der Laan 2013-12-24 09:06:10
##   5:               1          Philip Kaufmann 2013-12-20 17:58:15
##  ---                                                             
## 199:               1            gavinandresen 2010-12-17 20:16:27
## 200:               1               s_nakamoto 2010-12-15 22:43:51
## 201:               1         Satoshi Nakamoto 2010-08-28 01:55:50
## 202:               1                  laszloh 2010-08-04 05:15:38
## 203:               1                 sirius-m 2010-02-04 15:31:46
##      Metadata_AU_NAM
##   1:               1
##   2:               1
##   3:               1
##   4:               1
##   5:               1
##  ---                
## 199:               1
## 200:               1
## 201:               1
## 202:               1
## 203:               1
```

```r
am$view("AU", type = "timepoint", time = as.POSIXct("2011-01-01 00:00:00", origin="1970-01-01"))
```

```
##    AU_ID Metadata_AU                                AU_EMA_Author_Email
## 1:    11           1                            gavinandresen@gmail.com
## 2:   198           1                             witchspace81@gmail.com
## 3:   199           1 gavinandresen@1a98c847-1fd6-4fd8-948a-caf3550aa51b
## 4:   200           1    s_nakamoto@1a98c847-1fd6-4fd8-948a-caf3550aa51b
## 5:   201           1                                   satoshin@gmx.com
## 6:   202           1       laszloh@1a98c847-1fd6-4fd8-948a-caf3550aa51b
## 7:   203           1      sirius-m@1a98c847-1fd6-4fd8-948a-caf3550aa51b
##    Metadata_AU_EMA AU_NAM_Author_Name    AU_NAM_ChangedAt Metadata_AU_NAM
## 1:               1     Gavin Andresen 2010-12-20 19:44:54               1
## 2:               1         Witchspace 2010-12-24 09:25:21               1
## 3:               1      gavinandresen 2010-12-17 20:16:27               1
## 4:               1         s_nakamoto 2010-12-15 22:43:51               1
## 5:               1   Satoshi Nakamoto 2010-08-28 01:55:50               1
## 6:               1            laszloh 2010-08-04 05:15:38               1
## 7:               1           sirius-m 2010-02-04 15:31:46               1
```

```r
# am$view("AU", type = "difference", time = c(as.POSIXct("2012-01-01 00:00:00", origin="1970-01-01"), as.POSIXct("2012-02-01 00:00:00", origin="1970-01-01")))
```

## Query metadata

Check all the below commands.


```r
am
```

```
##                  code                  name     class mne    desc
## 1:             AU_NAM    AU_NAM_Author_Name attribute NAM    Name
## 2:             AU_EMA   AU_EMA_Author_Email attribute EMA   Email
## 3:                 AU             AU_Author    anchor  AU  Author
## 4:             CM_TIM    CM_TIM_Commit_Time attribute TIM    Time
## 5:             CM_MES CM_MES_Commit_Message attribute MES Message
## 6:             CM_HSH    CM_HSH_Commit_Hash attribute HSH    Hash
## 7:                 CM             CM_Commit    anchor  CM  Commit
## 8: AU_authoring_CM_of    AU_authoring_CM_of       tie  NA      NA
## 9:             CM_TAG     CM_TAG_Commit_Tag attribute TAG     Tag
##            obj  hist knot      size rows meta           last_load in_nrow
## 1: <attribute>  TRUE   NA  128.7 KB 4892    2 2015-06-08 00:45:50    5009
## 2: <attribute> FALSE   NA   18.8 KB  203    2 2015-06-08 00:45:50    5009
## 3:    <anchor> FALSE   NA      3 KB  203    2 2015-06-08 00:45:50    5009
## 4: <attribute> FALSE   NA   80.4 KB 5009    2 2015-06-08 00:45:50    5009
## 5: <attribute> FALSE   NA  563.5 KB 5009    2 2015-06-08 00:45:50    5009
## 6: <attribute> FALSE   NA  510.4 KB 5009    2 2015-06-08 00:45:50    5009
## 7:    <anchor> FALSE   NA   40.6 KB 5009    2 2015-06-08 00:45:50    5009
## 8:       <tie> FALSE   NA 904 bytes    0   NA                <NA>      NA
## 9: <attribute> FALSE   NA 904 bytes    0   NA                <NA>      NA
##    unq_nrow load_nrow   load_time
## 1:     4892         0 0.008034052
## 2:      203         0 0.006740732
## 3:      203         0 0.005160852
## 4:     5009         0 0.009199773
## 5:     5009         0 0.009601850
## 6:     5009         0 0.009315081
## 7:     5009         0 0.005984490
## 8:       NA        NA          NA
## 9:       NA        NA          NA
```

```r
am$etl # you should notice your OS user name on `meta==1L`
```

```
##     meta      src   user           timestamp   code in_nrow unq_nrow
##  1:    1 commits1    jan 2015-06-08 00:45:50     AU    5009      203
##  2:    1 commits1    jan 2015-06-08 00:45:50 AU_EMA    5009      203
##  3:    1 commits1    jan 2015-06-08 00:45:50 AU_NAM    5009     4892
##  4:    1 commits1    jan 2015-06-08 00:45:50     CM    5009     5009
##  5:    1 commits1    jan 2015-06-08 00:45:50 CM_HSH    5009     5009
##  6:    1 commits1    jan 2015-06-08 00:45:50 CM_MES    5009     5009
##  7:    1 commits1    jan 2015-06-08 00:45:50 CM_TIM    5009     5009
##  8:    2  git log manual 2015-06-08 00:45:50     AU    5009      203
##  9:    2  git log manual 2015-06-08 00:45:50 AU_EMA    5009      203
## 10:    2  git log manual 2015-06-08 00:45:50 AU_NAM    5009     4892
## 11:    2  git log manual 2015-06-08 00:45:50     CM    5009     5009
## 12:    2  git log manual 2015-06-08 00:45:50 CM_HSH    5009     5009
## 13:    2  git log manual 2015-06-08 00:45:50 CM_MES    5009     5009
## 14:    2  git log manual 2015-06-08 00:45:50 CM_TIM    5009     5009
##     load_nrow   load_time
##  1:       203 0.003845461
##  2:       203 0.004135582
##  3:      4892 0.006062921
##  4:      5009 0.005637030
##  5:      5009 0.006363728
##  6:      5009 0.006374484
##  7:      5009 0.004663373
##  8:         0 0.005160852
##  9:         0 0.006740732
## 10:         0 0.008034052
## 11:         0 0.005984490
## 12:         0 0.009315081
## 13:         0 0.009601850
## 14:         0 0.009199773
```

```r
am$log
```

```
##                 event                obj           timestamp
##  1:     initialize AM                 NA 2015-06-08 00:45:49
##  2:     initialize IM                 NA 2015-06-08 00:45:49
##  3:            create                 AU 2015-06-08 00:45:49
##  4:            create             AU_NAM 2015-06-08 00:45:49
##  5:            create             AU_EMA 2015-06-08 00:45:49
##  6:            create                 CM 2015-06-08 00:45:49
##  7:            create             CM_TIM 2015-06-08 00:45:49
##  8:            create             CM_MES 2015-06-08 00:45:49
##  9:            create             CM_TAG 2015-06-08 00:45:49
## 10:            create             CM_HSH 2015-06-08 00:45:49
## 11:            create AU_authoring_CM_of 2015-06-08 00:45:49
## 12: start AM instance                 NA 2015-06-08 00:45:49
```

```r
am$IM()
```

```
## <IM>
##   Identities:
##     CM: commit (5009 rows)
##     AU: author_email (203 rows)
##   Size: 508.6 KB
```

## Save AM instance


```r
# saving
am$stop()
save(am, mapping, extract.commits, extract.numstats, file = "git-am.RData")

# now you can shutdown your machine
rm(am, mapping, extract.commits, extract.numstats)

# loading
load("git-am.RData")
am$run()
```

## Export model


```r
am$xml()
```

File can be loaded in official Anchor Modeler Test: [roenbaeck.github.io/anchor](http://roenbaeck.github.io/anchor/)  
After loading xml file don't forget to click *Play* button.  
Exported xml does not contain non-model fields like data types. Also it won't export restatability definition as it is part of metadata of model in current XML schema. This may be subject to change in Anchor Modeling, see [anchor#4](https://github.com/Roenbaeck/anchor/issues/4).  

Current model can look like that.  

![Git repo AM first iteration](git-am1.png)

If you don't like how your model looks like you can use *Layout* menu and *Release all fixed* or *Randomize layout* options.  

## Incremental loading


```r
commits2 <- extract.commits("commits2014.json")
am$load(mapping, commits2, meta = list(meta = 3L, src = "git log"))
```


```r
am
```

```
##                  code                  name     class mne    desc
## 1:             AU_NAM    AU_NAM_Author_Name attribute NAM    Name
## 2:             AU_EMA   AU_EMA_Author_Email attribute EMA   Email
## 3:                 AU             AU_Author    anchor  AU  Author
## 4:             CM_TIM    CM_TIM_Commit_Time attribute TIM    Time
## 5:             CM_MES CM_MES_Commit_Message attribute MES Message
## 6:             CM_HSH    CM_HSH_Commit_Hash attribute HSH    Hash
## 7:                 CM             CM_Commit    anchor  CM  Commit
## 8: AU_authoring_CM_of    AU_authoring_CM_of       tie  NA      NA
## 9:             CM_TAG     CM_TAG_Commit_Tag attribute TAG     Tag
##            obj  hist knot      size rows meta           last_load in_nrow
## 1: <attribute>  TRUE   NA  198.7 KB 7577    3 2015-06-08 00:45:52    7714
## 2: <attribute> FALSE   NA   30.2 KB  337    3 2015-06-08 00:45:52    7714
## 3:    <anchor> FALSE   NA    4.1 KB  337    3 2015-06-08 00:45:52    7714
## 4: <attribute> FALSE   NA  122.7 KB 7714    3 2015-06-08 00:45:52    7714
## 5: <attribute> FALSE   NA  843.5 KB 7714    3 2015-06-08 00:45:51    7714
## 6: <attribute> FALSE   NA  785.1 KB 7714    3 2015-06-08 00:45:51    7714
## 7:    <anchor> FALSE   NA   61.7 KB 7714    3 2015-06-08 00:45:51    7714
## 8:       <tie> FALSE   NA 904 bytes    0   NA                <NA>      NA
## 9: <attribute> FALSE   NA 904 bytes    0   NA                <NA>      NA
##    unq_nrow load_nrow   load_time
## 1:     7577      2685 0.010541634
## 2:      337       134 0.007446198
## 3:      337       134 0.005687874
## 4:     7714      2705 0.010970180
## 5:     7714      2705 0.011007196
## 6:     7714      2705 0.010932326
## 7:     7714      2705 0.006988528
## 8:       NA        NA          NA
## 9:       NA        NA          NA
```

```r
am$etl
```

```
##     meta      src   user           timestamp   code in_nrow unq_nrow
##  1:    1 commits1    jan 2015-06-08 00:45:50     AU    5009      203
##  2:    1 commits1    jan 2015-06-08 00:45:50 AU_EMA    5009      203
##  3:    1 commits1    jan 2015-06-08 00:45:50 AU_NAM    5009     4892
##  4:    1 commits1    jan 2015-06-08 00:45:50     CM    5009     5009
##  5:    1 commits1    jan 2015-06-08 00:45:50 CM_HSH    5009     5009
##  6:    1 commits1    jan 2015-06-08 00:45:50 CM_MES    5009     5009
##  7:    1 commits1    jan 2015-06-08 00:45:50 CM_TIM    5009     5009
##  8:    2  git log manual 2015-06-08 00:45:50     AU    5009      203
##  9:    2  git log manual 2015-06-08 00:45:50 AU_EMA    5009      203
## 10:    2  git log manual 2015-06-08 00:45:50 AU_NAM    5009     4892
## 11:    2  git log manual 2015-06-08 00:45:50     CM    5009     5009
## 12:    2  git log manual 2015-06-08 00:45:50 CM_HSH    5009     5009
## 13:    2  git log manual 2015-06-08 00:45:50 CM_MES    5009     5009
## 14:    2  git log manual 2015-06-08 00:45:50 CM_TIM    5009     5009
## 15:    3  git log    jan 2015-06-08 00:45:52     AU    7714      337
## 16:    3  git log    jan 2015-06-08 00:45:52 AU_EMA    7714      337
## 17:    3  git log    jan 2015-06-08 00:45:52 AU_NAM    7714     7577
## 18:    3  git log    jan 2015-06-08 00:45:51     CM    7714     7714
## 19:    3  git log    jan 2015-06-08 00:45:51 CM_HSH    7714     7714
## 20:    3  git log    jan 2015-06-08 00:45:51 CM_MES    7714     7714
## 21:    3  git log    jan 2015-06-08 00:45:52 CM_TIM    7714     7714
##     meta      src   user           timestamp   code in_nrow unq_nrow
##     load_nrow   load_time
##  1:       203 0.003845461
##  2:       203 0.004135582
##  3:      4892 0.006062921
##  4:      5009 0.005637030
##  5:      5009 0.006363728
##  6:      5009 0.006374484
##  7:      5009 0.004663373
##  8:         0 0.005160852
##  9:         0 0.006740732
## 10:         0 0.008034052
## 11:         0 0.005984490
## 12:         0 0.009315081
## 13:         0 0.009601850
## 14:         0 0.009199773
## 15:       134 0.005687874
## 16:       134 0.007446198
## 17:      2685 0.010541634
## 18:      2705 0.006988528
## 19:      2705 0.010932326
## 20:      2705 0.011007196
## 21:      2705 0.010970180
##     load_nrow   load_time
```

```r
am$log
```

```
##                 event                    obj           timestamp
##  1:     initialize AM                     NA 2015-06-08 00:45:49
##  2:     initialize IM                     NA 2015-06-08 00:45:49
##  3:            create                     AU 2015-06-08 00:45:49
##  4:            create                 AU_NAM 2015-06-08 00:45:49
##  5:            create                 AU_EMA 2015-06-08 00:45:49
##  6:            create                     CM 2015-06-08 00:45:49
##  7:            create                 CM_TIM 2015-06-08 00:45:49
##  8:            create                 CM_MES 2015-06-08 00:45:49
##  9:            create                 CM_TAG 2015-06-08 00:45:49
## 10:            create                 CM_HSH 2015-06-08 00:45:49
## 11:            create     AU_authoring_CM_of 2015-06-08 00:45:49
## 12: start AM instance                     NA 2015-06-08 00:45:49
## 13:  stop AM instance                     NA 2015-06-08 00:45:51
## 14: start AM instance                     NA 2015-06-08 00:45:51
## 15: AM model exported AM_20150608_004551.xml 2015-06-08 00:45:51
```

```r
am$IM()
```

```
## <IM>
##   Identities:
##     CM: commit (7714 rows)
##     AU: author_email (337 rows)
##   Size: 783.7 KB
```

## Evolving model

Insertions and deletions to files.  
FileCommit anchor built on composite key.  


```r
am$add$A(mne = "FI", desc = "FileEvolution")
am$add$a(mne = "PAT", desc = "Path", anchor = "FI")
am$add$a(mne = "INS", desc = "Insertions", anchor = "FI")
am$add$a(mne = "DEL", desc = "Deletions", anchor = "FI")
am$add$t(anchors = c("FI","CM"), roles = c("changed","in"), identifier = c(Inf,Inf))
mapping.numstat <- list(FI = list(c("commit","path"),
                                  PAT = "path",
                                  INS = "insertions",
                                  DEL = "deletions"),
                        CM = list("commit"),
                        FI_CM = list())
am$run()
```


```r
numstats1 <- extract.numstats("numstats2014.json")
am$load(mapping.numstat, numstats1, meta = list(meta = 4L, src = "git log"))
```


```r
am$view("FI")
```


```r
am
```

```
##                   code                            name     class mne
##  1:   FI_changed_CM_in                FI_changed_CM_in       tie  NA
##  2:                 CM                       CM_Commit    anchor  CM
##  3:             FI_PAT       FI_PAT_FileEvolution_Path attribute PAT
##  4:             FI_INS FI_INS_FileEvolution_Insertions attribute INS
##  5:             FI_DEL  FI_DEL_FileEvolution_Deletions attribute DEL
##  6:                 FI                FI_FileEvolution    anchor  FI
##  7:             AU_NAM              AU_NAM_Author_Name attribute NAM
##  8:             AU_EMA             AU_EMA_Author_Email attribute EMA
##  9:                 AU                       AU_Author    anchor  AU
## 10:             CM_TIM              CM_TIM_Commit_Time attribute TIM
## 11:             CM_MES           CM_MES_Commit_Message attribute MES
## 12:             CM_HSH              CM_HSH_Commit_Hash attribute HSH
## 13: AU_authoring_CM_of              AU_authoring_CM_of       tie  NA
## 14:             CM_TAG               CM_TAG_Commit_Tag attribute TAG
##              desc         obj  hist knot      size  rows meta
##  1:            NA       <tie> FALSE   NA  230.7 KB 19538    4
##  2:        Commit    <anchor> FALSE   NA   61.7 KB  7714    4
##  3:          Path <attribute> FALSE   NA  438.4 KB 19538    4
##  4:    Insertions <attribute> FALSE   NA  230.6 KB 19538    4
##  5:     Deletions <attribute> FALSE   NA  230.6 KB 19538    4
##  6: FileEvolution    <anchor> FALSE   NA  154.1 KB 19538    4
##  7:          Name <attribute>  TRUE   NA  198.7 KB  7577    3
##  8:         Email <attribute> FALSE   NA   30.2 KB   337    3
##  9:        Author    <anchor> FALSE   NA    4.1 KB   337    3
## 10:          Time <attribute> FALSE   NA  122.7 KB  7714    3
## 11:       Message <attribute> FALSE   NA  843.5 KB  7714    3
## 12:          Hash <attribute> FALSE   NA  785.1 KB  7714    3
## 13:            NA       <tie> FALSE   NA 904 bytes     0   NA
## 14:           Tag <attribute> FALSE   NA 904 bytes     0   NA
##               last_load in_nrow unq_nrow load_nrow   load_time
##  1: 2015-06-08 00:45:54   19538    19538     19538 0.008051653
##  2: 2015-06-08 00:45:54   19538     5033         0 0.008553253
##  3: 2015-06-08 00:45:54   19538    19538     19538 0.007137570
##  4: 2015-06-08 00:45:54   19538    19538     19538 0.006312604
##  5: 2015-06-08 00:45:54   19538    19538     19538 0.006781448
##  6: 2015-06-08 00:45:54   19538    19538     19538 0.006386357
##  7: 2015-06-08 00:45:52    7714     7577      2685 0.010541634
##  8: 2015-06-08 00:45:52    7714      337       134 0.007446198
##  9: 2015-06-08 00:45:52    7714      337       134 0.005687874
## 10: 2015-06-08 00:45:52    7714     7714      2705 0.010970180
## 11: 2015-06-08 00:45:51    7714     7714      2705 0.011007196
## 12: 2015-06-08 00:45:51    7714     7714      2705 0.010932326
## 13:                <NA>      NA       NA        NA          NA
## 14:                <NA>      NA       NA        NA          NA
```

```r
am$etl
```

```
##     meta      src   user           timestamp             code in_nrow
##  1:    1 commits1    jan 2015-06-08 00:45:50               AU    5009
##  2:    1 commits1    jan 2015-06-08 00:45:50           AU_EMA    5009
##  3:    1 commits1    jan 2015-06-08 00:45:50           AU_NAM    5009
##  4:    1 commits1    jan 2015-06-08 00:45:50               CM    5009
##  5:    1 commits1    jan 2015-06-08 00:45:50           CM_HSH    5009
##  6:    1 commits1    jan 2015-06-08 00:45:50           CM_MES    5009
##  7:    1 commits1    jan 2015-06-08 00:45:50           CM_TIM    5009
##  8:    2  git log manual 2015-06-08 00:45:50               AU    5009
##  9:    2  git log manual 2015-06-08 00:45:50           AU_EMA    5009
## 10:    2  git log manual 2015-06-08 00:45:50           AU_NAM    5009
## 11:    2  git log manual 2015-06-08 00:45:50               CM    5009
## 12:    2  git log manual 2015-06-08 00:45:50           CM_HSH    5009
## 13:    2  git log manual 2015-06-08 00:45:50           CM_MES    5009
## 14:    2  git log manual 2015-06-08 00:45:50           CM_TIM    5009
## 15:    3  git log    jan 2015-06-08 00:45:52               AU    7714
## 16:    3  git log    jan 2015-06-08 00:45:52           AU_EMA    7714
## 17:    3  git log    jan 2015-06-08 00:45:52           AU_NAM    7714
## 18:    3  git log    jan 2015-06-08 00:45:51               CM    7714
## 19:    3  git log    jan 2015-06-08 00:45:51           CM_HSH    7714
## 20:    3  git log    jan 2015-06-08 00:45:51           CM_MES    7714
## 21:    3  git log    jan 2015-06-08 00:45:52           CM_TIM    7714
## 22:    4  git log    jan 2015-06-08 00:45:54               CM   19538
## 23:    4  git log    jan 2015-06-08 00:45:54               FI   19538
## 24:    4  git log    jan 2015-06-08 00:45:54           FI_DEL   19538
## 25:    4  git log    jan 2015-06-08 00:45:54           FI_INS   19538
## 26:    4  git log    jan 2015-06-08 00:45:54           FI_PAT   19538
## 27:    4  git log    jan 2015-06-08 00:45:54 FI_changed_CM_in   19538
##     meta      src   user           timestamp             code in_nrow
##     unq_nrow load_nrow   load_time
##  1:      203       203 0.003845461
##  2:      203       203 0.004135582
##  3:     4892      4892 0.006062921
##  4:     5009      5009 0.005637030
##  5:     5009      5009 0.006363728
##  6:     5009      5009 0.006374484
##  7:     5009      5009 0.004663373
##  8:      203         0 0.005160852
##  9:      203         0 0.006740732
## 10:     4892         0 0.008034052
## 11:     5009         0 0.005984490
## 12:     5009         0 0.009315081
## 13:     5009         0 0.009601850
## 14:     5009         0 0.009199773
## 15:      337       134 0.005687874
## 16:      337       134 0.007446198
## 17:     7577      2685 0.010541634
## 18:     7714      2705 0.006988528
## 19:     7714      2705 0.010932326
## 20:     7714      2705 0.011007196
## 21:     7714      2705 0.010970180
## 22:     5033         0 0.008553253
## 23:    19538     19538 0.006386357
## 24:    19538     19538 0.006781448
## 25:    19538     19538 0.006312604
## 26:    19538     19538 0.007137570
## 27:    19538     19538 0.008051653
##     unq_nrow load_nrow   load_time
```

```r
am$log
```

```
##                 event                    obj           timestamp
##  1:     initialize AM                     NA 2015-06-08 00:45:49
##  2:     initialize IM                     NA 2015-06-08 00:45:49
##  3:            create                     AU 2015-06-08 00:45:49
##  4:            create                 AU_NAM 2015-06-08 00:45:49
##  5:            create                 AU_EMA 2015-06-08 00:45:49
##  6:            create                     CM 2015-06-08 00:45:49
##  7:            create                 CM_TIM 2015-06-08 00:45:49
##  8:            create                 CM_MES 2015-06-08 00:45:49
##  9:            create                 CM_TAG 2015-06-08 00:45:49
## 10:            create                 CM_HSH 2015-06-08 00:45:49
## 11:            create     AU_authoring_CM_of 2015-06-08 00:45:49
## 12: start AM instance                     NA 2015-06-08 00:45:49
## 13:  stop AM instance                     NA 2015-06-08 00:45:51
## 14: start AM instance                     NA 2015-06-08 00:45:51
## 15: AM model exported AM_20150608_004551.xml 2015-06-08 00:45:51
## 16:  stop AM instance                     NA 2015-06-08 00:45:52
## 17:            create                     FI 2015-06-08 00:45:52
## 18:            create                 FI_PAT 2015-06-08 00:45:52
## 19:            create                 FI_INS 2015-06-08 00:45:52
## 20:            create                 FI_DEL 2015-06-08 00:45:52
## 21:            create       FI_changed_CM_in 2015-06-08 00:45:52
## 22: start AM instance                     NA 2015-06-08 00:45:52
##                 event                    obj           timestamp
```

```r
am$IM()
```

```
## <IM>
##   Identities:
##     CM: commit (7714 rows)
##     AU: author_email (337 rows)
##     FI: commit, path (19538 rows)
##   Size: 1.7 MB
```

## Questions

- Why it is so fast?  

[data.table](https://github.com/Rdatatable/data.table) design assumed from the early days to handle time series data well. Thanks to clustered key it can perform multiple types of joins very efficiently: equi join, rolling join (cross apply/cross join lateral), overlapping join (range join), nonequi join can be done by overlapping join or cross join and filter. See [implementation doc](inst/doc/implementation.md) for details.  

- How to start?

Model any data. Your model can have even single anchor. Model can non-destructivly grow over time so you can think about more entities later!  
In this doc you have example on loading git repo data, you can reproduce the same process for any git repo you want.  
If you have some DWH data already, you can just use a minimal subset of it.  

- Stability of project?

In development, ready for testing.  
You are welcome to add your unit tests which can be included in automated unit testing on package build.  

- Waiting for more questions to add here

## Importants notes vs SQL anchor model

- data stored in-memory
- built-in ETL
- triggers are replaced by functions
- built-in Identity Management
- built-in dashboard
- high performance data processing using [data.table](https://github.com/Rdatatable/data.table)
- does not support *concurrent-reliance-temporal*
- easy multiple AM instances in R session
