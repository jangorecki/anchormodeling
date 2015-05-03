#' @title Anchor Modeling metadata manager
#' @docType class
#' @format An R6 class object.
#' @name AM
#' @description Generate an instance of Anchor Model data warehouse.
#' @export
#' @examples
#' am <- AM$new()
#' am$log
#' am$create(class = "anchor", mne = "AC", desc = "Actor")
#' am$create(class = "attribute", mne = "AC_GEN", desc = "Gender")
#' am$data
#' am$log
AM <- R6Class(
    classname = "AM",
    public = list(
        data = data.table(NULL),
        initialize = function(){
            private$log_list <- list(list(event = "initialize AM", timestamp = Sys.time()))
            invisible(self)
        },
        print = function(size.units = getOption("am.size.format")){
            self$read()[, size := am.size.format(lapply(obj, function(obj) obj$size()), units = size.units)
                        ][, rows := sapply(obj, function(obj) nrow(obj$data))
                          ][, print(.SD)]#[, label := sapply(obj, function(obj) obj$label)][, cols := lapply(obj, function(obj) obj$cols)]
            invisible(self)
        },
        # CRUD
        create = function(mne, desc, ..., class){
            private$instance_run <- FALSE
            if(length(mne) != 1L) stop("create currently support scalar inputs only")
            if(missing(class)) class <- class.mne(mne)
            self$data <- rbindlist(list(self$data, data.table(mne = mne, desc = desc, class = class, obj = list(am.fun(class, "new")(mne, desc, ...)))))
            setkeyv(self$data, "mne")[]
            private$log_list <- c(private$log_list, list(list(event = paste("create", mne), timestamp = Sys.time())))
            invisible(self)
        },
        read = function(mne){
            if(missing(mne)) mne <- TRUE
            self$data[eval(mne)]
        }, # used for self lookup
        update = function(mne){
            stop("update method not available, use $delete and $create")
            invisible(self)
        },
        delete = function(mne){
            private$instance_run <- FALSE
            self$data <- self$data[!.(mne)]
            setkeyv(self$data, "mne")[]
            private$log_list <- c(private$log_list, list(list(event = paste("delete", mne), timestamp = Sys.time())))
            invisible(self)
        },
        # RUN
        validate = function(){
            all(
                # mne unique
                self$data[, length(mne) == length(unique(mne))],
                # each attribute is linked to existing anchor
                all(self$data[class=="attribute", unique(sub_(mne))] %chin% self$data[class=="anchor", mne]),
                # each tie linked to existing anchors
                all(self$data[class=="tie", unique(unlist(lapply(obj, function(obj) obj$anchors)))] %chin% self$data[class=="anchor", mne]),
                # each knotted attribute linked to existing knot
                all(self$data[class=="attribute", unique(unlist(lapply(obj, function(obj) obj$knot)))] %chin% self$data[class=="knot", mne]),
                # each knotted tie linked to existing knot
                all(self$data[class=="tie", unique(unlist(lapply(obj, function(obj) obj$knot)))] %chin% self$data[class=="knot", mne])
            )
        },
        run = function(){
            if(!self$validate()) stop("AM definition is invalid, see am$validate body for conditions")
            self$data[class!="attribute", obj := sapply(obj, function(obj) obj$setcols())]
            self$data[class=="attribute", obj := mapply(function(obj, anchor_desc) obj$setcols(anchor_desc = anchor_desc), obj, anchor_desc = self$read(mne = sub_(mne))$desc)]
            self$data[class!="attribute", obj := sapply(obj, function(obj) obj$setlabel())]
            self$data[class=="attribute", obj := mapply(function(obj, anchor_desc) obj$setlabel(anchor_desc = anchor_desc), obj, anchor_desc = self$read(mne = sub_(mne))$desc)]
            private$instance_run <- TRUE
            invisible(self)
        },
        # ETL
        load = function(mapping, data, meta = NA_integer_, .args){
            if(!isTRUE(private$instance_run)) stop("Run DW instance by am$run")
            if(!missing(.args)){
                data <- .args[["data"]]
                meta <- .args[["meta"]]
                mapping <- .args[["mapping"]]
            } # easier to call `load` programmatically
            stopifnot(
                is.list(mapping),
                length(mapping)>0L,
                is.data.table(data),
                all(names(mapping) %chin% self$data$mne), # exists in defined AM
                all(sapply(mapping, function(x) length(x) <= 3L)) # only two columns allowed
            )
            if(!all(unlist(mapping) %chin% names(data))){
                stop(paste0("Following columns defined in mapping don't exists in income data: ", paste(unlist(mapping)[!unlist(mapping) %chin% names(data)], collapse=", ")))
            } # src columns exists in data to load
            if(!all(r <- sapply(mapping, function(x) if(!is.null(names(x))) all(names(x) %chin% c("","x","id","hist")) else TRUE))) stop(paste0("Unexpected elements in mapping definition, expected source column names: '' or 'x' and 'id' and 'hist'. Check following mappings: ",paste(names(r)[r], collapse=", ")))
            if(!all(r <- sapply(mapping, function(x) if(length(x) > 1L){
                if(is.null(names(x))) return(FALSE)
                any(c("","x") %chin% names(x)) && any(c("id","hist") %chin% names(x))
            } else TRUE))) stop(paste0("Unexpected elements in mapping definition, expected source column names: '' or 'x' and 'id' and 'hist'. Check following mappings: ",paste(names(r)[r], collapse=", "))) # allowed names: ""/"x", "id", "hist"
            am.order <- c("anchor" = 1L, "knot" = 2L, "attribute" = 3L, "tie" = 4L)
            ordered_mne <- self$read(names(mapping))[,mne,keyby=.(am.order[class])]$mne
            # first pass loop, check check if mapping matches, fill defaults
            for(mne in ordered_mne){
                # check if knotted
                if(length(self$read(mne)$obj[[1L]]$knot)){
                    if(!self$read(mne)$obj[[1L]]$knot %chin% ordered_mne) stop("Cannot load knotted attribute/tie without also loading knot for it, provide knot mapping.")
                }
                # check if historized
                if(isTRUE(self$read(mne)$obj[[1L]]$hist)){
                    if(length(mapping[[mne]]) < 2L || !"hist" %chin% names(mapping[[mne]])) stop("Cannot load historized attribute wihtout mapping of columns on which historize attribute/tie. Provide historize source in the data set by adding `c('AC_NAM', hist = 'HireName')` into attribute/tie mapping.")
                }
                # automapping anchor by name convention
                if(self$read(mne)$class=="anchor"){
                    if(length(mapping[[mne]]) == 0L){
                        if(!paste(mne,"ID",sep="_") %chin% names(data)) stop(paste0("If anchor ID field was not provided it has to be in naming convention '",paste(mne,"ID",sep="_"),"' (as IM class produces, see ?IM), otherwise provide anchor ID source in the data set by adding `'AC'` or `c(x = 'AC')` into anchor mapping."))
                        mapping[[mne]] <- paste(mne,"ID",sep="_")
                    } # use automapping for default naming convention column, [mne]_ID
                }
                # automapping for knots ID by name convention
                if(self$read(mne)$class=="knot"){
                    if(length(mapping[[mne]]) == 1L){
                        if(!paste(mne,"ID",sep="_") %chin% names(data)) stop(paste0("If knot ID field was not provided it has to be in naming convention '",paste(mne,"ID",sep="_"),"' (as IM class produces, see ?IM), otherwise provide knot ID source in the data set by adding `c('GEN', id = 'GEN_ID')` into knot mapping."))
                        mapping[[mne]] <- c(mapping[[mne]], id = paste(mne,"ID",sep="_"))
                    } # use automapping for default naming convention column, [mne]_ID
                }
                # check automapping for attributes by anchor mapping lookup
                if(self$read(mne)$class=="attribute"){
                    if(!"id" %chin% names(mapping[[mne]])){
                        mapping[[mne]] <- c(mapping[[mne]], id = mapping[[sub_(mne)]])
                    }
                }
            }
            # second pass loop, subset columns and provide to am objects classes for loading
            for(mne in ordered_mne){
                src_cols <- mapping[[mne]]
                src_cols <- c(src_cols[is.null(names(src_cols))], src_cols[names(src_cols) %chin% c("id")], src_cols[names(src_cols) %chin% c("","x")], src_cols[names(src_cols) %chin% c("hist")]) # reorder cols
                # check if historized
                #if(isTRUE(self$read(mne)$obj[[1L]]$hist)){
                #    src_cols <- c(src_cols,mapping[[mne]][["hist"]])
                #}
                tgt_cols <- self$read(mne)$obj[[1L]]$cols # already have hist if required
                #if(length(self$read(mne)$obj[[1L]]$knot)){
                #}
                #self$ID[[mne]][data[, unique(.SD), .SDcols=self$NK[[mne]]]][is.na(eval(as.name(paste(mne,"ID",sep="_"))))]
                #if(self$read(mne)$class=="attribute"){
                #
                #}
                if(length(src_cols)+1!=length(tgt_cols)) browser()
                ifUnq <- function(unq, x) if(unq) unique(x) else x
                self$data[mne, obj][[1L]]$load(
                    data = setnames(ifUnq(
                        unq = self$read(mne)$class %chin% c("anchor","knot"),
                        x = data[, src_cols, with=FALSE]
                    )[, metadata := meta], c(src_cols,"metadata"), tgt_cols),
                    type = if(self$read(mne)$class %chin% c("anchor","knot")) "upsert" else "insert"
                )
            }
            invisible(self)
        },
        query = function(){

        }
    ),
    private = list(
        log_list = list(),
        instance_run = FALSE
    ),
    active = list(
        log = function() rbindlist(private$log_list)
    )
)

