
# class related -----------------------------------------------------------

class1 <- function(x) class(x)[1L]
is.AM <- function(x) "AM" %chin% class(x)
is.IM <- function(x) "IM" %chin% class(x)
is.am <- is.maobj <- function(x) any(c("anchor","attribute","tie","knot") %chin% class(x))
is.anchor <- function(x) "anchor" %chin% class(x)
is.attribute <- function(x) "attribute" %chin% class(x)
is.tie <- function(x) "tie" %chin% class(x)
is.knot <- function(x) "knot" %chin% class(x)

# various helpers ---------------------------------------------------------

paste_ <- function(..., sep="_", collapse="_") paste(..., sep=sep, collapse=collapse)

am.size.format <-  function(x, units = "auto"){
    # modified: utils:::format.object_size
    if(is.null(units)) return(sapply(x, function(x) format.object_size(x, units="auto")))
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

valid_entity_params <- function(x, data.names){
    # valid_entity_params
    # all(sapply(mapping, function(x) sapply(x, valid_entity_params)))
    # to apply on each entity def:
    # AC = list("name",
    #           NAM = c("name", hist="name_date"))
    if(is.null(names(x)) && length(x)==1L) return(TRUE)
    #else if(is.null(names(x)) && length(x)>1L) browser()#stop("Provide named character vector of entity definition, use `knot`, `hist`") # commented because blocks composite nk
    else if(!all(x[names(x) %chin% c("","hist")] %chin% data.names)) stop(paste0("All unnamed elements of the mapping list must be existing column names of data. Fix mapping definition for src fields: ", paste(x[!x[names(x) %chin% c("","hist")] %chin% data.names], collapse=",")))
    else if(!all(names(x) %chin% c("","hist"))) stop(paste0('Mapping of entity must be character vector named in: "", hist, knot.'))
    TRUE
}

selfNames <- function(x) setNames(x, x)

exclude.cols <- function(x, cols = c("obj"), .escape=FALSE) if(.escape) return(x) else x[, .SD, .SDcols = names(x)[!names(x) %chin% eval(cols)]]

format.object_size <- function (x, units = "b", ...) {
    # makes valid units on output: bytes instead of bits, declared locally for case when units=NULL and redirects to utils:::format.object_size
    units <- match.arg(units, c("b", "auto", "Kb", "Mb", "Gb",
                                "B", "KB", "MB", "GB"))
    if (units == "auto") {
        if (x >= 1024^3)
            units <- "GB"
        else if (x >= 1024^2)
            units <- "MB"
        else if (x >= 1024)
            units <- "KB"
        else units <- "B"
    }
    switch(units,
           b = , B = paste(x, "bytes"),
           Kb = , KB = paste(round(x/1024, 1L), "KB"),
           Mb = , MB = paste(round(x/1024^2, 1L), "MB"),
           Gb = , GB = paste(round(x/1024^3, 1L), "GB"))
}

exclude.last <- function(x){
    if(is.data.table(x)){
        x[, exclude.last(names(x)), with=FALSE]
    } else x[-length(x)]
}

exclude.first <- function(x){
    if(is.data.table(x)){
        x[, exclude.first(names(x)), with=FALSE]
    } else x[-1L]
}

now <- function(class = "POSIXct") switch(class, "POSIXct" = Sys.time(), "Date" = Sys.Date())

nowchar <- function(class = "POSIXct") as.character(now(class=class))

length0 <- function(x) length(x)==0L

try_nanotime <- function(){
    if(requireNamespace("microbenchmark", quietly=TRUE)) microbenchmark::get_nanotime() else proc.time()[[3L]]
}

temporal_filter <- function(x, cols){
    if(missing(cols) || length(cols)==0) x else x[!(x[, .(`.na` = all(is.na(.SD))), seq_len(nrow(x)), .SDcols = c(cols)]$.na)]
}

#' @export
technical_filter <- function(x){
    coltypes <- attr(x,"coltypes",TRUE)
    if(is.null(coltypes)) return(x)
    if(!identical(sort(names(x)),sort(names(coltypes)))) return(x)
    x[, c(names(coltypes)[coltypes %chin% c("data","hist")]), with=FALSE]
}

# mapping -----------------------------------------------------------------

a <- function(src, knot, hist){
    stopifnot(is.character(src))
    if(!missing(knot) & !missing(hist)){
        l <- list(src = src, knot = knot, hist = hist)
    } else if(missing(knot) & !missing(hist)){
        l <- list(src = src, hist = hist)
    } else if(!missing(knot) & missing(hist)){
        l <- list(src = src, knot = knot)
    } else if(missing(knot) & missing(hist)){
        l <- list(src = src)
    } else {
        stop("invalid knot, hist inputs")
    }
    l
}

A <- function(nk, ...){
    dots <- list(...)
    stopifnot(is.character(nk))
    c(list(nk = nk), dots)
}

A.dt <- function(Aname, x){
    anames <- names(x[[Aname]])
    if(is.null(anames)){
        data.table(class = character(), anchor = character(), mne = character(), src_col = character(), hist = logical(), hist_col = character())
    } else {
        data.table(class = "attribute", anchor = Aname, rbindlist(lapply(anames[-1L], a.dt, x = x[[Aname]])))
    }
}

a.dt <- function(aname, x){
    if(is.null(aname)) aname <- "src"
    data.table(
        mne = aname,
        src_col = as.character(x[[aname]][[1L]])[1L],
        hist = !is.na(as.character(as.list(x[[aname]])[["hist"]])[1L]),
        hist_col = as.character(as.list(x[[aname]])[["hist"]])[1L]
    )
}

T.dt <- function(Tname, x){
    data.table(
        code = Tname,
        src_col = as.character(as.list(x[[Tname]])[["knot"]])[1L],
        hist = !is.na(as.character(as.list(x[[Tname]])[["hist"]])[1L]),
        knot = NA_character_, # knot mne should be taken from AM model
        hist_col = as.character(as.list(x[[Tname]])[["hist"]])[1L]
    )
}

# sql related -------------------------------------------------------------

format.postgres.value <- function(x){
    ifelse(is.null(x),
           "NULL",
           ifelse(is.na(x),
                  "NULL",
                  ifelse(is.logical(x),
                         as.character(x),
                         ifelse(is.numeric(x),
                                as.character(x),
                                ifelse(is.character(x),
                                       paste0("'",x,"'"),
                                       ifelse(inherits(x, "POSIXt"),
                                              paste0("'",format(x, "%Y-%m-%d %H:%M:%S"),"'::timestamp"),
                                              ifelse(inherits(x, "Date"),
                                                     paste0("'",format(x, "%Y-%m-%d"),"'::date"),
                                                     {warning(paste("unsupported column of class:", class(x)[1L])); "NULL"})))))))
}

insert.postgres <- function(x, tbl = "schema.table"){
    paste(
        "INSERT INTO",
        tbl,
        paste0("(", paste(names(x), collapse=", "), ")"),
        "VALUES",
        x[, paste0("(", paste(lapply(.SD, format.postgres.value), collapse=", "), ");"), by = seq_len(nrow(x))]$V1
    )
}

# shiny -------------------------------------------------------------------

dashboard_dummy_data <- function(){
    am <- anchormodeling::AM$new()
    am$create("anchor", mne = "AC", desc = "Actor")
    am$create("attribute", anchor = "AC", mne = "NAM", desc = "Name", hist = TRUE)
    am$create("attribute", anchor = "AC", mne = "GEN", desc = "Gender", knot = "GEN")
    am$create("knot", mne = "GEN", desc = "Gender")
    am$create("attribute", anchor = "AC", mne = "PLV", desc = "ProfessionalLevel", knot = "PLV", hist = TRUE)
    am$create("knot", mne = "PLV", desc = "ProfessionalLevel")
    am$create("anchor", mne = "PR", desc = "Program")
    am$create(class = "knot", mne = "RAT", desc = "Rating")
    am$create(class = "tie", anchors = c("AC","PR"), knot = "RAT", roles = c("part","in","got"), identifier = c(Inf,Inf,1), hist = TRUE)
    am$create("attribute", anchor = "PR", mne = "NAM", desc = "Name")
    am$run()

    actor_mapping <- list(AC = list("code",
                                    NAM = c("name", hist = "date"),
                                    GEN = "gender",
                                    PLV = c("level", hist = "date")))

    actor_data <- data.table(code = c("1", "2", "3", "4"),
                             name = c("Mike", "Bob", "Alice", "Lee"),
                             gender = c("M", "M", "F", "M"),
                             level = c(4L, 1L, 3L, 4L),
                             date = as.Date("2015-07-05"))
    am$load(mapping = actor_mapping,
            data = actor_data,
            meta = 1L)

    actor_data <- data.table(code = c("1", "2", "3"),
                             name = c("Mike", "Ben", "Alice"), # 1 name changed
                             gender = c("M", "M", "F"),
                             level = c(5L, 1L, 3L), # 1 level change
                             date = as.Date("2015-07-06"))
    am$load(mapping = actor_mapping,
            data = actor_data,
            meta = 2L)

    actor_program_mapping <- list(PR = list("prog_code"),
                                  AC = list("acto_code"),
                                  AC_PR_RAT = list(hist = "date", knot = "score"))
    actor_program_data <- data.table(prog_code = c(1:2,3L,3L),
                                     acto_code = as.character(c(1:2,2L,2L)),
                                     score = c("A","D","E","D"),
                                     date = as.Date("2015-07-03")+c(0:1,0:1))
    am$load(mapping = actor_program_mapping,
            data = actor_program_data,
            meta = 3L)

    program_mapping <- list(PR = list("code",
                                      NAM = "name"))
    program_data <- data.table(code = 1:3, name = c("show1","show2","show3"))
    am$load(mapping = program_mapping,
            data = program_data,
            meta = 4L)

    am
}
