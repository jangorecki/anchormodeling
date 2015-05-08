is.AM <- function(x) "AM" %chin% class(x)
is.IM <- function(x) "IM" %chin% class(x)
is.am <- is.maobj <- function(x) any(c("anchor","attribute","tie","knot") %chin% class(x))
is.anchor <- function(x) "anchor" %chin% class(x)
is.attribute <- function(x) "attribute" %chin% class(x)
is.tie <- function(x) "tie" %chin% class(x)
is.knot <- function(x) "knot" %chin% class(x)

paste_ <- function(..., sep="_", collapse="_") paste(..., sep=sep, collapse=collapse)

# #' @title AM object method router
# #' @param class character, one of: \emph{anchor, attribute, tie, knot}.
# #' @param method character, method name
# #' @details Used for \code{am.fun(class, "new", ...)}.
# #' @return function
# am.fun <- function(class, method, ...) switch(class, "anchor" = anchor, "attribute" = attribute, "tie" = tie, "knot" = knot, stop("Anchor model objects must be anchor/attribute/tie/knot."))[[method]](...)

#' @title Format object sie for vector of values
#' @description Format object size numbers for vector of values using mean auto units matching.
#' @param x numeric vector in bytes or \code{object.size()} function results, a \emph{object_size} class.
#' @param units character scalar, one of \emph{auto, B, KB, MB, GB}. Default \emph{NULL} with results \emph{units} on individual rows.
#' @return character
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

# #' @title Extracts usually anchor name from mnemonic
# #' @param x character with at least one underscore, for vector param it should expects constant underscore number in each element.
# #' @param i integer location of character to extract, for attributes always 1, for ties 1+, -1 extracts last element - e.g. knot from tie mnemonic.
# #' @return character
# sub_ <- function(x, i = 1L){
#     if(i > 0L){
#         sapply(strsplit(x, split="_", fixed=TRUE), `[`, i)
#     } else if(i == -1L){
#         x <- strsplit(x, split="_", fixed=TRUE)
#         n <- length(x[[1L]])
#         sapply(x, `[`, n)
#     }
# }

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

#' @title First class of object
#' @param x any object
#' @return character
class1 <- function(x) class(x)[1L]
