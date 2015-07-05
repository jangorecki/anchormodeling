.onLoad <- function(libname, pkgname){

    options("am.size.format" = NULL) # NULL means auto on individual rows, e.g. set to "MB" for all rows in MB
    options("am.key.verbose" = FALSE) # interactive monitor for key/key2 usage
    options("am.restatability" = TRUE) # by default load restatements

    setNumericRounding(1) # to correct order by 0.001 timestamps

    options("datatable.auto.index" = FALSE) # prevent unwanted indexes

}
