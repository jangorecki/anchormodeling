
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
            if(nrow(data) == 0L) return(invisible(self))
            if(!db) self$data <- setkeyv(rbindlist(list(self$data, data), use.names=TRUE), self$keys) else stop("db backend not ready")
            return(invisible(self))
        },
        query = function(latest = FALSE, db = FALSE){
            if(!db){
                if(latest) self$data[] else self$data
            } else {
                if(latest) setDT(NULL, key=self$keys) else setDT(NULL, key=self$keys)
            }
        },
        nrow = function(db = FALSE){
            if(!db) nrow(self$data) else NA_integer_ # as.integer(dbGetQuery(conn, paste0("SELECT COUNT(*) AS cnt FROM ",self$name,";"))$cnt)[1L]
        },
        types = function(db = FALSE){
            if(!db) sapply(self$data, class1) else sapply(NULL, class1) # dbGetQuery(conn, paste0("SELECT * FROM ",self$name," LIMIT 1;"))
        },
        load = function(data, meta){
            stopifnot(is.data.table(data))
            ts <- if(requireNamespace("microbenchmark", quietly=TRUE)) microbenchmark::get_nanotime() else proc.time()[[3L]]
            in_nrow <- nrow(data)
            Sys.sleep(0.001) # just to make timestamp better sortable, requires setNumericRounding(1)
            if(in_nrow == 0L){
                private$load_log <- c(private$load_log, list(list(meta = meta, last_load = Sys.time(), code = self$code, in_nrow = 0L, unq_nrow = 0L, load_nrow = 0L,
                                                                  load_time = if(requireNamespace("microbenchmark", quietly=TRUE)) (microbenchmark::get_nanotime() - ts) * 1e-9 else proc.time()[[3L]] - ts)))
                returns(invisible(self))
            }
            data <- copy(unique(data))[, c(self$cols[length(self$cols)]) := meta]
            unq_nrow <- nrow(data)
            setkeyv(data, self$keys)
            # check if first time used
            if(self$nrow() > 0){
                # check data types
                stopifnot(identical(self$types(), sapply(data, class1)))
                # filter out exactly same rows by ID and hist
                data <- data[!self$query()]
                # restatement
                if(identical(self$rest,FALSE)){
                    # get new vs previous row
                    new_vs_prev <- quote(self$query()[data, which(!ST_NAM_Stage_Name == i.ST_NAM_Stage_Name | is.na(ST_NAM_Stage_Name)), roll = Inf])
                    # get new vs  next row
                    new_vs_next <- quote(self$query()[data, which(!ST_NAM_Stage_Name == i.ST_NAM_Stage_Name | is.na(ST_NAM_Stage_Name)), roll = -Inf])
                    # subset
                    data <- data[intersect(eval(new_vs_prev), eval(new_vs_next))]
                }
            }
            self$insert(data)
            private$load_log <- c(private$load_log, list(list(meta = meta, last_load = Sys.time(), code = self$code, in_nrow = in_nrow, unq_nrow = unq_nrow, load_nrow = nrow(data),
                                                              load_time = if(requireNamespace("microbenchmark", quietly=TRUE)) (microbenchmark::get_nanotime() - ts) * 1e-9 else proc.time()[[3L]] - ts)))
            invisible(self)
        },
        size = function() object.size(self$data)
    ),
    private = list(
        load_log = list()
    ),
    active = list(
        log = function() rbindlist(private$load_log)
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
        initialize = function(mne, desc){
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
        },
        xml = function(attributes){
            # anchor node
            lines <- paste0('<anchor mnemonic="',self$mne,'" descriptor="',self$desc,'" identity="int">')
            # anchor attributes
            lines <- c(lines, attributes[, sapply(obj, function(obj) obj$xml())])
            # anchor close node
            c(lines, "</anchor>")
        }
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
        rest = logical(),
        anchor = character(),
        initialize = function(anchor, mne, desc, knot = character(), hist = FALSE, rest = getOption("am.restatability")[hist], hist_col = "ChangedAt"){
            stopifnot(all(c("mne","desc") %chin% names(anchor))) # anchor can be self$data anchor's row or named character vector with anchor mne and desc
            self$anchor <- anchor[["mne"]]
            self$mne <- mne
            self$desc <- desc
            self$code <- paste_(self$anchor, self$mne) # AC_NAM
            self$knot <- knot
            self$hist <- hist
            self$rest <- rest
            self$name <- paste_(self$anchor, self$mne, anchor[["desc"]], self$desc) # AC_NAM_Actor_Name
            self$cols <- c(
                paste_(self$anchor, self$mne, self$anchor, "ID"), # AC_NAM_AC_ID
                if(length(self$knot)) paste_(self$anchor, self$mne, self$knot, "ID") else self$name, # AC_GEN_GEN_ID else AC_NAM_Actor_Name
                if(self$hist) paste_(self$anchor, self$mne, hist_col), # AC_PLV_ChangedAt
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
        },
        xml = function(){
            paste0('<attribute mnemonic="',self$mne,'" descriptor="',self$desc,'"',if(isTRUE(self$hist)) ' timeRange="datetime"', if(as.logical(length(self$knot))) paste0(' knotRange="',self$knot,'"') else ' dataRange="varchar(42)"','></attribute>') # hardcode for timeRange/dataRange, see: http://stackoverflow.com/q/30054615/2490497
        }
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
        rest = logical(), # TRUE / FALSE / logical()
        identifier = numeric(), # c(1,Inf) / c(Inf,Inf)
        initialize = function(anchors, knot = character(), roles, identifier = numeric(), hist = FALSE, rest = getOption("am.restatability")[hist], hist_col = "ChangedAt"){
            self$anchors <- anchors
            self$knot <- knot
            self$roles <- roles
            self$identifier <- identifier
            self$hist <- hist
            self$rest <- rest
            self$name <- paste_(c(self$anchors,self$knot), self$roles) # AC_exclusive_AC_with
            self$code <- self$name # AC_exclusive_AC_with
            self$cols <- c(
                paste_(c(self$anchors,self$knot), "ID", self$roles, collapse=NULL), # AC_ID_exclusive, AC_ID_with
                if(self$hist) paste_(self$name, hist_col), # AC_exclusive_AC_with_ChangedAt
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
        },
        xml = function(){
            lines <- paste0('<tie',if(isTRUE(self$hist)) ' timeRange="datetime"','>')
            prefix <- c(rep("<anchor", length(self$anchors)), rep("<knot", length(self$knot)))
            lines <- c(lines, paste0(prefix,'Role role="',self$roles,'" type="',c(self$anchors, self$knot),'" identifier="',tolower(as.character(!is.finite(self$identifier))),'"','/>'))
            c(lines, '</tie>')
        }
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
        initialize = function(mne, desc){
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
        },
        xml = function(){
            paste0('<knot mnemonic="',self$mne,'" descriptor="',self$desc,'" identity="int" dataRange="varchar(max)"></knot>') # hardcode, see comment near attribute$xml
        }
    )
)
