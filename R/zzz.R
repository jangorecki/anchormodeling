.onLoad <- function(libname, pkgname){

    options("am.size.format" = NULL) # NULL means auto on individual rows
    options("am.key.verbose" = FALSE) # interactive monitor for key/key2 usage, will be changed to FALSE on stable
    options("am.parallel" = FALSE)
    options("am.restatability" = TRUE) # by default load restatements

    setNumericRounding(1) # to correct order by 0.001 timestamps

}
