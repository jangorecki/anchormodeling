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

# # @title Guess class based on mnemonic
# # @param mne character mnemonic in AM naming convention
# # @details Limited only to mnemonics AM naming convention and ties with no more than 2 anchors.
# # @return character anchor/attribute/tie/knot
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

valid_entity_params <- function(x, data.names){
    # valid_entity_params
    # all(sapply(mapping, function(x) sapply(x, valid_entity_params)))
    # to apply on each entity def:
    # AC = list("name",
    #           NAM = c("name", hist="name_date"))
    if(is.null(names(x)) && length(x)==1L) return(TRUE)
    else if(is.null(names(x)) && length(x)>1L) stop("Provide named character vector of entity definition, use `knot`, `hist`")
    else if(!all(x[names(x) %chin% c("","hist")] %chin% data.names)) stop(paste0("All unnamed elements of the mapping list must be existing column names of data. Fix mapping definition for src fields: ", paste(x[!x[names(x) %chin% c("","hist")] %chin% data.names], collapse=",")))
    else if(!all(names(x) %chin% c("","hist","knot"))) stop(paste0('Mapping of entity must be character vector named in: "", hist, knot.'))
    TRUE
}

selfNames <- function(x) setNames(x, x)
