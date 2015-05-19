#' @title anchormodeling-package
#' @docType package
#' @import data.table R6
#' @author Jan Gorecki
#' @description Anchor Modeling metadata manager. AM in-memory Data Warehouse instances. Identity Management instaces. Automated ETL for AM loading.
#' @seealso \link{AM}, \link{IM}
#' @name anchormodeling
NULL

anchor_modeler_url <- function() "http://roenbaeck.github.io/anchor/"

#' @title Open browser on official Anchor Modeler
#' @export
#' @return invisibly NULL
run_anchor_modeler <- function(){
    browseURL(anchor_modeler_url())
}

#' @title Example Actor anchor model
#' @param iteration numeric 1+
#' @details Tree iterations of model available: Stage Performances, New Example Model, New startup example in 0.98
#' @export actor.am
#' @return AM class object populated with anchors, attributes, ties and knots from the anchor modeling examples
actor.am <- function(iteration = 1){
    stopifnot(iteration > 0)
    # AM
    am <- AM$new()
    # actor
    am$create("anchor", mne = "AC", desc = "Actor")
    am$create("attribute", anchor = "AC", mne = "NAM", desc = "Name", hist = TRUE)
    am$create("attribute", anchor = "AC", mne = "GEN", desc = "Gender", knot = "GEN")
    am$create("knot", mne = "GEN", desc = "Gender")
    am$create("attribute", anchor = "AC", mne = "PLV", desc = "ProfessionalLevel", knot = "PLV", hist = TRUE)
    am$create("knot", mne = "PLV", desc = "ProfessionalLevel")
    if(iteration>=2) am$create("tie", anchors = c("AC","AC"), roles = c("exclusive","with"), identifier = c(1,1))
    am$create("tie", anchors = c("AC","AC"), knot = "PAT", roles = c("parent","child","rating"), identifier = c(Inf,Inf,Inf))
    am$create("knot", mne = "PAT", desc = "ParentalType")
    # program
    am$create("anchor", mne = "PR", desc = "Program")
    am$create("attribute", anchor = "PR", mne = "NAM", desc = "Name")
    am$create("tie", anchors = c("AC","PR"), knot = "RAT", roles = c("part","in","got"), identifier = c(Inf,Inf,1))
    am$create("knot", mne = "RAT", desc = "Rating")
    # performance
    am$create("anchor", mne = "PE", desc = "Performance")
    am$create("attribute", anchor = "PE", mne = "DAT", desc = "Date")
    am$create("attribute", anchor = "PE", mne = "AUD", desc = "Audience")
    am$create("attribute", anchor = "PE", mne = "REV", desc = "Revenue")
    am$create("tie", anchors = c("PE","AC"), roles = c("in","wasCast"), identifier = c(Inf,Inf))
    am$create("tie", anchors = c("PE","PR"), roles = c("at","wasPlayed"), identifier = c(Inf,1))
    # event
    if(iteration>=2) am$create("anchor", mne = "EV", desc = "Event")
    if(iteration>=2) am$create("tie", anchors = c("PE","EV"), roles = c("subset","of"), identifier = c(1,1))
    # stage
    am$create("anchor", mne = "ST", desc = "Stage")
    am$create("attribute", anchor = "ST", mne = "LOC", desc = "Location")
    am$create("attribute", anchor = "ST", mne = "NAM", desc = "Name", hist = TRUE)
    if(iteration>=3) am$create("attribute", anchor = "ST", mne = "MIN", desc = "Minimum", knot = "UTL")
    if(iteration>=3) am$create("attribute", anchor = "ST", mne = "AVG", desc = "Average", hist = TRUE, knot = "UTL")
    if(iteration>=3) am$create("knot", mne = "UTL", desc = "Utilization")
    am$create("tie", anchors = c("PE","ST"), roles = c("wasHeld","at"), identifier = c(Inf,1))
    am$create("tie", anchors = c("ST","PR"), roles = c("at","isPlaying"), identifier = c(Inf,Inf))
    if(iteration>=3) am$create("tie", anchors = c("PR","ST","PE"), roles = c("content","location","of"), identifier = c(1,1,Inf))
    # return
    am
}

#' @title Example data for actor model
#' @param src character \code{"Stage"} or \code{"Actor"} currently.
#' @param iteration numeric 1+
#' @details Two iterations of data loads.
#' @export actor.data
#' @return data.table of populated source data load
actor.data <- function(src = "Stage", iteration = 1L){
    r <- list()
    if(src == "Stage" & iteration==1L){
        r[[1L]] <- list(name = 'The Shakespeare Theatre', address = '154bis, boulevard de la Chapelle')
        r[[2L]] <- list('Theatre Victor Hugo','11, rue du Temple')
        r[[3L]] <- list('The Dostojevskij theatre','10, rue Fontaine')
        r[[4L]] <- list('Theatre Sokrates','1, rue du Temple')
        r[[5L]] <- list('Teater August Strindberg','Storgatan 2')
        r[[6L]] <- list('Teater Ingmar Bergman','Drottninggatan 55')
    } else if(src == "Stage" & iteration==2L){
        r[[1L]] <- list(name = 'The Shakespeare Theatre', address = '154bis, boulevard de la Chapelle')
        r[[2L]] <- list('Theatre Victor Hugo','11, rue du Temple')
        r[[3L]] <- list('The Dostojevskij theatre','10, rue Fontaine')
        r[[4L]] <- list('Theatre Sokrates','1, rue du Temple')
        r[[5L]] <- list('August Strindbergs','Storgatan 2') # name changed
        r[[6L]] <- list('Teater Ingmar Bergman','Drottninggatan 55')
    } else if(src == "Actor" & iteration==1L){
        r[[1L]] <- list(code = '1', name = 'Mike', gender = 'M', level = 4L)
        r[[2L]] <- list('2', 'Bob', 'M', 1L)
        r[[3L]] <- list('3', 'Alice', 'F', 3L)
        r[[4L]] <- list('4', 'Lee', 'M', 4L)
    } else if(src == "Actor" & iteration==2L){
        r[[1L]] <- list(code = '1', name = 'Mike', gender = 'M', level = 5L) # prof level changed
        r[[2L]] <- list('2', 'Ben', 'M', 1L) # name changed
        r[[3L]] <- list(code = '3', 'Alice', 'F', 3L) # unchanged
    } else stop("unknown src/iteration")
    rbindlist(r)
}

monitor <- function(am){
    options("am.share" = am)
    if(!requireNamespace("shiny", quietly=TRUE) | !requireNamespace("DT", quietly=TRUE)){
        stop(paste0("install required packages: ",paste(c("shiny","DT")[c(requireNamespace("shiny", quietly=TRUE), requireNamespace("DT", quietly=TRUE))], collapse=", ")))
    } else {
        shiny::runApp(system.file("app","monitor", package = "anchormodeling"))
    }
}

