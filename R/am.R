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
        print = function(){
            print(self$read())
            invisible(self)
        },
        # CRUD
        create = function(mne, desc, ..., class){
            if(length(mne) != 1L) stop("create currently support scalar inputs only")
            if(missing(class)) class <- class.mne(mne)
            self$data <- rbindlist(list(self$data, data.table(mne = mne, desc = desc, class = class, obj = list(am.fun(class, "new")(mne, desc, ...)))))
            setkeyv(self$data, "mne")[]
            private$log_list <- c(private$log_list, list(list(event = paste("create", mne), timestamp = Sys.time())))
            invisible(self)
        },
        read = function(units = getOption("am.size.format")){
            copy(self$data)[, label := mapply(self$label, mne = mne, desc = desc, class = class)
                            ][, c("size") := am.size.format(sapply(obj, function(x) x$size()), units = units)
                              ][, class := NULL
                                ]
        },
        update = function(mne){
            invisible(self)
        },
        delete = function(mne){
            self$data <- self$data[!.(mne)]
            setkeyv(self$data, "mne")[]
            private$log_list <- c(private$log_list, list(list(event = paste("delete", mne), timestamp = Sys.time())))
            invisible(self)
        },
        # ETL
        load = function(mapping, data, src = NULL, batch = NULL, .args){
            if(!missing(.args)){
                data <- .args[["data"]]
                mapping <- .args[["mapping"]]
                src <- .args[["src"]]
                batch <- .args[["batch"]]
            } # easier to call `load` programmatically
            stopifnot(is.list(mapping), is.character(src), length(mapping)>0L, length(src)>0L, is.data.table(data))
            mne_to_load <- names(mapping)
            # chose first to process attributes of natural key and then anchors
            mne_order <- self$data[mne_to_load][class=="anchor", c(obj[[1L]]$nk, mne)]
            mne_order <- self$data[mne_to_load][class=="knot", c(mne_order, mne)]
            mne_order <- self$data[mne_to_load][!mne %chin% mne_order, c(mne_order, mne)]
            for(mnei in mne_order){
                cols <- self$data[mnei, cols.mne(mne = mne, desc = desc, class = class)]
                #setnames(, cols)
                self$data[mnei, obj][[1L]]$load(data = data[, mapping[[mnei]], with=FALSE], src = src, batch = batch)
            }
            invisible(self)
        },
        label = function(mne, desc, class){
            switch(class,
                   "anchor" = paste(mne, desc, sep="_"),
                   "attribute" = paste(mne, self$data[eval(get.anchor(mne)), desc], desc, sep="_"), # lookup for anchor description
                   "tie" = paste(get.anchor(mne), get.anchor(desc), get.anchor(mne,2L), get.anchor(desc,2L), sep="_"),  # two lookups for 2 anchors desc # TO DO
                   "knot" = paste(mne, desc, sep="_"))
        },
        query = function(){

        }
    ),
    private = list(
        log_list = list()
    ),
    active = list(
        log = function() rbindlist(private$log_list)
    )
)

