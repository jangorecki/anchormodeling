
# AM object superclass --------------------------------------------------------

#' @title AM object
#' @description Superclass for common features for anchors, attributes, ties and knots.
AMobj <- R6Class(
    classname = "AMobj",
    public = list(
        mne = character(),
        desc = character(),
        data = data.table(),
        initialize = function(class, ...){
            invisible(self)
        },
        print = function(){
            cat(self$mne,":",self$desc,"\n")
            invisible(self)
        },
        load = function(data, src = NA_character_, .args){
            if(!missing(.args)){
                data <- .args[["data"]]
                src <- .args[["src"]]
            } # easier to call `load` programmatically
            stopifnot(is.character(src), length(src)==1L, is.data.table(data))
            private$log_list <- c(private$log_list, list(list(mne = self$mne, timestamp = Sys.time(), event = "load", src = src, nrow = nrow(data))))
            self$data <- rbindlist(list(self$data, data.table(data, self$meta)))
            invisible(self)
        },
        size = function(){
            unlist(object.size(self$data))/1024^size.units()
        }
    ),
    private = list(
        log_list = list()
    ),
    active = list(
        log = function() rbindlist(private$log_list)
    )
)

# AM objects ----------------------------------------------------------------

#' @title Anchor class
anchor <- R6Class(
    classname = "anchor",
    inherit = AMobj,
    public = list(
        initialize = function(mne, desc){
            self$mne <- mne
            self$desc <- desc
            invisible(self)
        }
    ),
    active = list(
        meta = function() data.table(src = NA_character_, batch = NA_integer_, ts = Sys.time())
    )
)

#' @title Attribute class
attribute <- R6Class(
    classname = "attribute",
    inherit = AMobj,
    public = list(
        parent_mne = character(),
        initialize = function(mne, desc){
            self$mne <- mne
            self$desc <- desc
            self$parent_mne <- strsplit(mne, "_", fixed=TRUE)[[1L]][1L]
            invisible(self)
        }
    ),
    active = list(
        meta = function() data.table(knot = NA_character_, hist = NA_character_, src = NA_character_, batch = NA_integer_, ts = Sys.time())
    )
)

#' @title Tie class
tie <- R6Class(
    classname = "tie",
    inherit = AMobj,
    public = list(
        lhs = character(),
        rhs = character(),
        initialize = function(mne, desc){
            self$mne <- mne
            self$desc <- desc
            self$lhs <- strsplit(mne, "_", fixed=TRUE)[[1L]][1L]
            self$rhs <- strsplit(mne, "_", fixed=TRUE)[[1L]][2L]
            invisible(self)
        }
    ),
    active = list(
        meta = function() data.table(anchors = list(), knot = NA_character_, hist = NA_character_, src = NA_character_, batch = NA_integer_, ts = Sys.time())
    )
)

#' @title Knot class
knot <- R6Class(
    classname = "knot",
    inherit = AMobj,
    public = list(
        initialize = function(mne, desc){
            self$mne <- mne
            self$desc <- desc
            invisible(self)
        }
    ),
    active = list(
        meta = function() data.table(src = NA_character_, batch = NA_integer_, ts = Sys.time())
    )
)
