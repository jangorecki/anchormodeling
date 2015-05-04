
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
        cols = character(),
        keys = character(),
        label = character(),
        initialize = function(class, ...){
            invisible(self)
        },
        insert = function(data) if(nrow(data) > 0L) self$data <- setkeyv(rbindlist(list(self$data, data)),self$keys),
        query = function(){
            # get branch of DW
            self$data
        },
        load = function(data, meta, .args){
            if(!missing(.args)){
                data <- .args[["data"]]
                meta <- .args[["meta"]]
            } # easier to call `load` programmatically
            stopifnot(is.data.table(data))

            if(nrow(data) == 0L){
                private$log_list <- c(private$log_list, list(list(meta = meta, timestamp = Sys.time(), mne = self$mne, event = "load", in_nrow = 0L, unq_nrow = 0L, load_nrow = 0L)))
                returns(invisible(self))
            }
            in_nrow <- nrow(data)
            data <- copy(data[, unique(.SD, by=self$keys)])[, c(self$cols[length(self$cols)]) := meta]
            unq_nrow <- nrow(data)
            # check if first time used
            init <- length(self$data)==0L
            if(!init){
                # check data types
                stopifnot(identical(sapply(self$data, typeof), sapply(data, typeof)))
                # keep only new
                setkeyv(data, self$keys)
                # insert unique by PK including historization col, check if is.na on first key col, then subset
                data <- data[!self$data] # data[self$data, key1 := eval(as.name(paste0("i.",self$keys[1L])))][is.na(key1)][, key1 := NULL]
            }
            self$insert(data)
            private$log_list <- c(private$log_list, list(list(meta = meta, timestamp = Sys.time(), mne = self$mne, event = "load", in_nrow = in_nrow, unq_nrow = unq_nrow, load_nrow = nrow(data))))
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
        },
        print = function(){
            cat("<",class(self)[1L],">\n",sep="")
            cat("  mnemonic: ",self$mne,"\n",sep="")
            cat("  descriptor: ",self$desc,"\n",sep="")
            cat("  label: ",self$label,"\n",sep="")
            cat("  columns: ",paste(self$cols, collapse=", "),"\n",sep="")
            invisible(self)
        },
        setcols = function(){
            self$cols <- c(paste(self$mne, "ID", sep="_"), paste("Metadata", self$mne, sep="_"))
            self$keys <- self$cols[1L]
            invisible(self)
        },
        setlabel = function(){
            self$label <- paste(self$mne, self$desc, sep="_")
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
            self$knot <- if(is.character(knot)) knot else if(is.logical(knot) & knot==TRUE) sub_(mne, 2L)
            self$hist <- hist
            invisible(self)
        },
        print = function(){
            cat("<",class(self)[1L],">\n",sep="")
            cat("  mnemonic: ",self$mne,"\n",sep="")
            cat("  descriptor: ",self$desc,"\n",sep="")
            if(length(self$knot)) cat("  knotted: ",self$knot,"\n",sep="")
            if(self$hist) cat("  historized: ",self$hist,"\n",sep="")
            cat("  label: ",self$label,"\n",sep="")
            cat("  columns: ",paste(self$cols, collapse=", "),"\n",sep="")
            invisible(self)
        },
        setcols = function(anchor_desc){
            self$cols <- c(
                paste(self$mne, sub_(self$mne), "ID", sep="_"),
                if(length(self$knot)) paste(self$mne, self$knot, "ID", sep="_") else paste(self$mne, anchor_desc, self$desc, sep="_"),
                if(self$hist) paste(self$mne, "ChangedAt", sep="_"),
                paste("Metadata", self$mne, sep="_")
            )
            self$keys <- if(self$hist) c(self$cols[1L], self$cols[3L]) else self$cols[1L]
            invisible(self)
        },
        setlabel = function(anchor_desc){
            self$label <- paste(self$mne, anchor_desc, self$desc, sep="_")
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
            self$knot <- if(is.character(knot)) knot else if(is.logical(knot) & knot==TRUE) sub_(mne, -1L)
            self$hist <- hist
            mnes <- strsplit(mne, "_", fixed=TRUE)[[1L]]
            self$anchors <- if(length(self$knot)) mnes[-length(mnes)] else mnes
            invisible(self)
        },
        print = function(){
            cat("<",class(self)[1L],">\n",sep="")
            cat("  mnemonic: ",self$mne,"\n",sep="")
            cat("  descriptor: ",self$desc,"\n",sep="")
            if(length(self$knot)) cat("  knotted: ",self$knot,"\n",sep="")
            if(self$hist) cat("  historized: ",self$hist,"\n",sep="")
            cat("  anchors: ",paste(self$anchors, collapse=", "),"\n",sep="")
            cat("  label: ",self$label,"\n",sep="")
            cat("  columns: ",paste(self$cols, collapse=", "),"\n",sep="")
            invisible(self)
        },
        setcols = function(){
            mnes <- strsplit(self$mne, "_", fixed=TRUE)[[1L]]
            roles <- strsplit(self$desc, "_", fixed=TRUE)[[1L]]
            self$cols <- c(
                paste(paste(mnes, "ID", sep="_"), roles, sep="_"),
                if(self$hist) paste(paste(paste(mnes, roles, sep="_"), collapse="_"), "ChangedAt", sep="_"),
                paste("Metadata", paste(paste(mnes, roles, sep="_"), collapse="_"), sep="_")
            )
            self$keys <- if(self$hist) c(self$cols[1L], self$cols[2L]) else self$cols[1L]
            invisible(self)
        },
        setlabel = function(){
            mnes <- strsplit(self$mne, "_", fixed=TRUE)[[1L]]
            roles <- strsplit(self$desc, "_", fixed=TRUE)[[1L]]
            self$label <- paste(paste(mnes, roles, sep="_"), collapse="_")
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
        },
        print = function(){
            cat("<",class(self)[1L],">\n",sep="")
            cat("  mnemonic: ",self$mne,"\n",sep="")
            cat("  descriptor: ",self$desc,"\n",sep="")
            cat("  label: ",self$label,"\n",sep="")
            cat("  columns: ",paste(self$cols, collapse=", "),"\n",sep="")
            invisible(self)
        },
        setcols = function(){
            self$cols <- c(
                paste(self$mne, "ID", sep="_"),
                paste(self$mne, self$desc, sep="_"),
                paste("Metadata", self$mne, sep="_")
            )
            self$keys <- self$cols[1L]
            invisible(self)
        },
        setlabel = function(){
            self$label <- paste(self$mne, self$desc, sep="_")
            invisible(self)
        }
    ),
    active = list(
        meta = function() list()
    )
)
