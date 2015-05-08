
# AM object superclass --------------------------------------------------------

#' @title AM object
#' @docType class
#' @format An R6 class object.
#' @name AMobj
#' @description Superclass for common features for anchors, attributes, ties and knots.
AMobj <- R6Class(
    classname = "AMobj",
    public = list(
        name = character(),
        code = character(),
        data = data.table(),
        cols = character(),
        keys = character(),
        initialize = function() invisible(self),
        insert = function(data, db = FALSE){
            if(nrow(data) > 0L) return(invisible(self))
            if(!db) self$data <- setkeyv(rbindlist(list(self$data, data)), self$keys) else NULL
            return(invisible(self))
        },
        query = function(db = FALSE){
            if(!db) self$data else setDT(NULL)
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
        size = function() object.size(self$data)
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
        mne = character(),
        desc = character(),
        initialize = function(mne, desc, ...){
            self$mne <- mne
            self$desc <- desc
            self$code <- self$mne # AC
            self$name <- paste_(self$mne, self$desc) # AC_Actor
            self$cols <- c(paste_(self$mne,"ID"), paste_("Metadata", self$mne)) # AC_ID, Metadata_AC
            self$keys <- self$cols[1L]
            invisible(self)
        },
        print = function(){
            cat("<",class1(self),">\n",sep="")
            cat("  name: ",self$name,"\n",sep="")
            cat("  mnemonic: ",self$mne,"\n",sep="")
            cat("  descriptor: ",self$desc,"\n",sep="")
            cat("  columns: ",paste(self$cols, collapse=", "),"\n",sep="")
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
        mne = character(),
        desc = character(),
        knot = character(),
        hist = logical(),
        anchor = character(),
        initialize = function(anchor, mne, desc, knot = character(), hist = FALSE, ...){
            stopifnot(all(c("mne","desc") %chin% names(anchor))) # anchor can be self$data anchor's row or named character vector with anchor mne and desc
            self$anchor <- anchor[["mne"]]
            self$mne <- mne
            self$desc <- desc
            self$code <- paste_(self$anchor, self$mne) # AC_NAM
            self$knot <- knot
            self$hist <- hist
            self$name <- paste_(self$anchor, self$mne, anchor[["desc"]], self$desc) # AC_NAM_Actor_Name
            self$cols <- c(
                paste_(self$anchor, self$mne, self$anchor, "ID"), # AC_NAM_AC_ID
                if(length(self$knot)) paste_(self$anchor, self$mne, self$knot, "ID") else self$name, # AC_GEN_GEN_ID else AC_NAM_Actor_Name
                if(self$hist) paste_(self$anchor, self$mne, "ChangedAt"), # AC_PLV_ChangedAt
                paste_("Metadata", self$anchor, self$mne) # Metadata_AC_NAM
            )
            self$keys <- if(self$hist) c(self$cols[1L], self$cols[3L]) else self$cols[1L]
            invisible(self)
        },
        print = function(){
            cat("<",class1(self),">\n",sep="")
            cat("  name: ",self$name,"\n",sep="")
            cat("  mnemonic: ",self$mne,"\n",sep="")
            cat("  descriptor: ",self$desc,"\n",sep="")
            if(length(self$knot)) cat("  knotted: ",self$knot,"\n",sep="")
            if(self$hist) cat("  historized: ",self$hist,"\n",sep="")
            cat("  columns: ",paste(self$cols, collapse=", "),"\n",sep="")
            invisible(self)
        }
    ),
    active = list(
        meta = function() list()
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
        anchors = character(), # c("AC","PE")
        knot = character(), # "GEN"
        roles = character(), # c("wasHeld","at")
        hist = logical(), # TRUE / FALSE
        identifier = numeric(), # c(1,Inf) / c(Inf,Inf)
        initialize = function(anchors, knot = character(), roles, identifier = numeric(), hist = FALSE, ...){
            self$anchors <- anchors
            self$knot <- knot
            self$roles <- roles
            self$identifier <- identifier
            self$hist <- hist
            self$name <- paste_(c(self$anchors,self$knot), self$roles) # AC_exclusive_AC_with
            self$code <- self$name # AC_exclusive_AC_with
            self$cols <- c(
                paste_(c(self$anchors,self$knot), "ID", self$roles, collapse=NULL), # AC_ID_exclusive, AC_ID_with
                if(self$hist) paste_(self$name, "ChangedAt"), # AC_exclusive_AC_with_ChangedAt
                paste_("Metadata", self$name) # Metadata_AC_exclusive_AC_with
            )
            self$keys <- c(self$cols[seq_along(self$anchors)], if(self$hist) self$cols[length(c(self$anchors,self$knot))+1L] else character())
            invisible(self)
        },
        print = function(){
            cat("<",class1(self),">\n",sep="")
            cat("  name: ",self$name,"\n",sep="")
            cat("  anchors: ",paste(self$anchors, collapse=", "),"\n",sep="")
            if(length(self$knot)) cat("  knotted: ",self$knot,"\n",sep="")
            if(self$hist) cat("  historized: ",self$hist,"\n",sep="")
            cat("  columns: ",paste(self$cols, collapse=", "),"\n",sep="")
            invisible(self)
        }
    ),
    active = list(
        meta = function() list()
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
        mne = character(),
        desc = character(),
        initialize = function(mne, desc, ...){
            self$mne <- mne
            self$desc <- desc
            self$code <- self$mne
            self$name <- paste_(self$mne, self$desc)
            self$cols <- c(
                paste_(self$mne, "ID"), # GEN_ID
                paste_(self$mne, self$desc), # GEN_Gender
                paste_("Metadata", self$mne) # Metadata_GEN
            )
            self$keys <- self$cols[1L]
            invisible(self)
        },
        print = function(){
            cat("<",class1(self),">\n",sep="")
            cat("  name: ",self$name,"\n",sep="")
            cat("  mnemonic: ",self$mne,"\n",sep="")
            cat("  descriptor: ",self$desc,"\n",sep="")
            cat("  columns: ",paste(self$cols, collapse=", "),"\n",sep="")
            invisible(self)
        }
    ),
    active = list(
        meta = function() list()
    )
)
