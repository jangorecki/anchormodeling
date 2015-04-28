is.am <- function(x) any(c("anchor","attribute","tie","knot") %chin% class(x))
is.anchor <- function(x) "anchor" %chin% class(x)
is.attribute <- function(x) "attribute" %chin% class(x)
is.tie <- function(x) "tie" %chin% class(x)
is.knot <- function(x) "knot" %chin% class(x)

#' @title AM object method router
#' @param class character, one of: \emph{anchor, attribute, tie, knot}.
#' @param method character, method name
#' @details Used for \code{AMfun(class, "new")(mne, desc, ...)}.
#' @return function
AMfun <- function(class, method) switch(class, "anchor" = anchor, "attribute" = attribute, "tie" = tie, "knot" = knot, stop("Anchor model objects must be anchor/attribute/tie/knot."))[[method]]

#' @title Object size units
#' @description Format object size numbers.
#' @param x character, one of \emph{B, KB, MB, GB}. Default \code{options("am.size.units")}.
#' @return integer value to use in \code{bytes/1024^size.units()}.
size.units <- function(x=getOption("am.size.units")) switch(x, "B"=0L, "KB"=1L, "MB"=2L, "GB"=3L)
