is.AM <- function(x) "AM" %chin% class(x)
is.IM <- function(x) "IM" %chin% class(x)
is.am <- is.maobj <- function(x) any(c("anchor","attribute","tie","knot") %chin% class(x))
is.anchor <- function(x) "anchor" %chin% class(x)
is.attribute <- function(x) "attribute" %chin% class(x)
is.tie <- function(x) "tie" %chin% class(x)
is.knot <- function(x) "knot" %chin% class(x)

paste_ <- function(..., sep="_", collapse="_") paste(..., sep=sep, collapse=collapse)

am.size.format <-  function(x, units = "auto"){
    # modified: utils:::format.object_size
    if(is.null(units)) return(sapply(x, function(x) format(x, units="auto")))
    x <- unlist(x)
    units <- match.arg(units, c("b", "auto", "Kb", "Mb", "Gb", "B", "KB", "MB", "GB"))
    if (units == "auto") {
        xm <- mean(x,na.rm=TRUE)
        if (xm >= 1024^3) units <- "GB"
        else if (xm >= 1024^2) units <- "MB"
        else if (xm >= 1024) units <- "KB"
        else units <- "B"
    }
    switch(units,
           "b" =, "B" = paste(x, "Bytes"),
           "Kb" =, "KB" = paste(round(x/1024, 1L), "KB"),
           "Mb" =, "MB" = paste(round(x/1024^2, 1L), "MB"),
           "Gb" =, "GB" = paste(round(x/1024^3, 1L), "GB")
    )
}

class1 <- function(x) class(x)[1L]

# #' @title Guess class based on mnemonic
# #' @param mne character mnemonic in AM naming convention
# #' @details Limited only to mnemonics AM naming convention and ties with no more than 2 anchors.
# #' @return character anchor/attribute/tie/knot
# class.mne <- function(mne){
#     nchar_mne <- nchar(mne)
#     classes <- rep(NA_character_,length(mne))
#     classes[nchar_mne==2L] <- "anchor"
#     classes[nchar_mne==3L] <- "knot"
#     classes[nchar_mne==5L] <- "tie"
#     classes[nchar_mne==6L] <- "attribute"
#     classes[nchar_mne==9L] <- "tie" # knotted tie
#     classes
# }
