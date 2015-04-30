is.am <- is.maobj <- function(x) any(c("anchor","attribute","tie","knot") %chin% class(x))
is.anchor <- function(x) "anchor" %chin% class(x)
is.attribute <- function(x) "attribute" %chin% class(x)
is.tie <- function(x) "tie" %chin% class(x)
is.knot <- function(x) "knot" %chin% class(x)

#' @title AM object method router
#' @param class character, one of: \emph{anchor, attribute, tie, knot}.
#' @param method character, method name
#' @details Used for \code{am.fun(class, "new")(mne, desc, ...)}.
#' @return function
am.fun <- function(class, method) switch(class, "anchor" = anchor, "attribute" = attribute, "tie" = tie, "knot" = knot, stop("Anchor model objects must be anchor/attribute/tie/knot."))[[method]]

#' @title Format object sie for vector of values
#' @description Format object size numbers for vector of values using median auto units matching.
#' @param x numeric vector in bytes or \code{object.size()} function results, a \emph{object_size} class.
#' @param units character scalar, one of \emph{B, KB, MB, GB}. Default \emph{auto}.
#' @return character
am.size.format <-  function(x, units = "auto"){
    # modified: utils:::format.object_size
    units <- match.arg(units, c("b", "auto", "Kb", "Mb", "Gb", "B", "KB", "MB", "GB"))
    if (units == "auto") {
        xmed <- median(x,na.rm=TRUE)
        if (xmed >= 1024^3) units <- "Gb"
        else if (xmed >= 1024^2) units <- "Mb"
        else if (xmed >= 1024) units <- "Kb"
        else units <- "b"
    }
    switch(units,
           "b" =, "B" = paste(x, "bytes"),
           "Kb" =, "KB" = paste(round(x/1024, 1L), "Kb"),
           "Mb" =, "MB" = paste(round(x/1024^2, 1L), "Mb"),
           "Gb" =, "GB" = paste(round(x/1024^3, 1L), "Gb")
    )
}

#' @title Extracts anchor name from mnemonic
#' @param mne character
#' @param i integer location of anchor to extract, for attributes always 1, for ties 1+.
#' @return character mne of anchor
get.anchor <- function(mne, i = 1L){
    sapply(strsplit(mne, split="_", fixed=TRUE), `[`, i)
}

#' @title Guess class based on mnemonic
#' @param mne character mnemonic in AM naming convention
#' @details Limited only to mnemonics AM naming convention and ties with no more than 2 anchors.
#' @return character anchor/attribute/tie/knot
class.mne <- function(mne){
    nchar_mne <- nchar(mne)
    classes <- rep(NA_character_,length(mne))
    classes[nchar_mne==2L] <- "anchor"
    classes[nchar_mne==3L] <- "knot"
    classes[nchar_mne==5L] <- "tie"
    classes[nchar_mne==6L] <- "attribute"
    classes[nchar_mne==9L] <- "tie" # knotted tie
    classes
}

#' @title Columns names of 6NF table
#' @param mne character
#' @param desc character, not required for anchor and tie
#' @param class character optional
#' @return character vector of target generic column names per class
cols.mne <- function(mne, desc, class){
    if(missing(class)) class <- class.mne(mne)
    switch(class,
           "anchor" = paste(mne, "ID", sep="_"),
           "attribute" = c(paste(get.anchor(mne),"ID",sep="_"), paste(mne,desc,sep="_")),
           "tie" = c(paste(get.anchor(mne),"ID",sep="_"), paste(get.anchor(mne,2L),"ID",sep="_")),
           "knot" = c(paste(mne,"ID",sep="_"), paste(mne, desc, sep="_")))
}
