
# AM object superclass --------------------------------------------------------

#' @title AM object
#' @docType class
#' @format An R6 class object.
#' @name AMobj
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
            cat("<",class(self)[1L],">\n",sep="")
            cat("  ",self$mne," : ",self$desc,"\n",sep="")
            invisible(self)
        },
        load = function(data, src = NULL, batch = NULL, .args){
            if(!missing(.args)){
                data <- .args[["data"]]
                src <- .args[["src"]]
                batch <- .args[["batch"]]
            } # easier to call `load` programmatically
            stopifnot(is.data.table(data))
            private$log_list <- c(private$log_list, list(list(mne = self$mne, timestamp = Sys.time(), event = "load", src = src, nrow = nrow(data))))
            local.meta <- list(src = src, batch = batch, ts = Sys.time())
            self$data <- rbindlist(list(self$data, data.table(data, as.data.table(local.meta))), fill=TRUE)
            invisible(self)
        },
        size = function(){
            object.size(self$data)
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
#' @docType class
#' @format An R6 class object.
#' @name anchor
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
        meta = function() list()
    )
)

#' @title Attribute class
#' @docType class
#' @format An R6 class object.
#' @name attribute
attribute <- R6Class(
    classname = "attribute",
    inherit = AMobj,
    public = list(
        knot = character(),
        hist = logical(),
        initialize = function(mne, desc, knot = character(), hist = FALSE){
            self$mne <- mne
            self$desc <- desc
            self$knot <- knot
            self$hist <- hist
            invisible(self)
        }
    ),
    active = list(
        meta = function() list(knot = self$knot, hist = self$hist)
    )
)

#' @title Tie class
#' @docType class
#' @format An R6 class object.
#' @name tie
tie <- R6Class(
    classname = "tie",
    inherit = AMobj,
    public = list(
        knot = character(),
        hist = logical(),
        anchors = character(),
        initialize = function(mne, desc, knot = character(), hist = FALSE){
            self$mne <- mne
            self$desc <- desc
            self$knot <- knot
            self$hist <- hist
            self$anchors <- if(length(knot)){
                x <- strsplit(mne, "_", fixed=TRUE)[[1L]]
                x[-length(x)]
            } else strsplit(mne, "_", fixed=TRUE)[[1L]]
            invisible(self)
        }
    ),
    active = list(
        meta = function() list(knot = self$knot, hist = self$hist)
    )
)

#' @title Knot class
#' @docType class
#' @format An R6 class object.
#' @name knot
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
        meta = function() list()
    )
)
