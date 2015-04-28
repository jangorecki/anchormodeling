#' @title anchormodeling-package
#' @docType package
#' @import data.table R6
#' @author Jan Gorecki
#' @description Anchor Modeling metadata manager. AM in-memory Data Warehouse instances. Automated ETL for AM loading.
#' @seealso \link{AM}
#' @name anchormodeling
NULL

#' @title Anchor Modeling metadata manager
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
            print(self$data)
            invisible(self)
        },
        # CRUD
        create = function(class, mne, desc, ...){
            if(length(mne) != 1L) stop("create currently support scalar inputs only")
            self$data <- rbindlist(list(self$data, data.table(mne = mne, desc = desc, class = class, obj = list(AMfun(class, "new")(mne, desc, ...)))))
            setkeyv(self$data, "mne")[]
            private$log_list <- c(private$log_list, list(list(event = paste("create", mne), timestamp = Sys.time())))
            invisible(self)
        },
        read = function(){
            copy(self$data)[, size := sapply(obj, function(x) x$size())]
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
        load = function(mapping, data, src = NA_character_, .args){
            if(!missing(.args)){
                data <- .args[["data"]]
                mapping <- .args[["mapping"]]
                src <- .args[["src"]]
            } # easier to call `load` programmatically
            stopifnot(is.list(mapping), is.character(src), length(mapping), length(src), is.data.table(data))
            # iterate over mappings
            mnes <- names(mapping)
            for(i in seq_len(length(mnes))){ # i <- 1L
                mne <- mnes[i]
                if(i==1L) if(self$data[mne, class!="anchor"]) stop("First field in the mapping should point to anchor")
                self$data[mne, obj][[1L]]$load(data = data[, src[[mne]], with=FALSE])
            }
            invisible(self)

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
