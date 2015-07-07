
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
        colorder = integer(),
        coltypes = character(),
        initialize = function() invisible(self),
        insert = function(data, db = FALSE){
            if(nrow(data) == 0L) return(invisible(self))
            if(!db) self$data <- setkeyv(rbindlist(list(self$data, data), use.names=TRUE), self$keys) else stop("db backend not ready")
            return(invisible(self))
        },
        query = function(type, timepoint, db = FALSE){
            if(db){
                stop("db backend not implemented")
            }
            if(length(self$data)==0L){ # no data loaded yet
                query <- quote(setkeyv(setnames(setDT(rep(list(integer()), length(self$cols))), self$cols), self$keys))
            } else if(missing(type) || !isTRUE(self$hist)){
                query <- quote(self$data)
            } else if(isTRUE(self$hist)){
                stopifnot(is.character(type))
                query <- switch(type,
                                "latest" = quote(self$data[, tail(.SD, 1L), by=c(exclude.last(self$keys))]),
                                "timepoint" = quote(self$data[eval(as.name(last(self$keys))) <= timepoint, tail(.SD, 1L), by=c(exclude.last(self$keys))]),
                                "current" = quote(self$data[eval(as.name(last(self$keys))) <= now(class1(eval(as.name(last(self$keys))))), tail(.SD, 1L), by=c(exclude.last(self$keys))]),
                                "difference" = quote(self$data[eval(as.name(last(self$keys))) %between% timepoint])
                )
            } # hist=TRUE
            eval(query)[]
        },
        nrow = function(db = FALSE){
            if(!db) nrow(self$data) else NA_integer_ # as.integer(dbGetQuery(conn, paste0("SELECT COUNT(*) AS cnt FROM ",self$name,";"))$cnt)[1L]
        },
        types = function(db = FALSE){
            if(!db) sapply(self$data, class1) else sapply(NULL, class1) # dbGetQuery(conn, paste0("SELECT * FROM ",self$name," LIMIT 1;"))
        },
        load = function(data, meta){
            stopifnot(is.data.table(data))
            ts <- try_nanotime()
            in_nrow <- nrow(data)
            Sys.sleep(0.001) # just to make timestamp better sortable, requires setNumericRounding(1)
            if(in_nrow == 0L){
                private$load_log <- c(private$load_log, list(list(meta = meta[["meta"]], user = meta[["user"]], src = meta[["src"]], timestamp = Sys.time(), code = self$code, in_nrow = 0L, unq_nrow = 0L, load_nrow = 0L,
                                                                  load_time = if(requireNamespace("microbenchmark", quietly=TRUE)) (microbenchmark::get_nanotime() - ts) * 1e-9 else proc.time()[[3L]] - ts)))
                returns(invisible(self))
            }
            data <- unique(data)[, c(self$cols[length(self$cols)]) := meta[["meta"]]]
            unq_nrow <- nrow(data)
            setkeyv(data, self$keys)
            # check if first time used
            if(self$nrow() > 0){
                # check data types
                stopifnot(identical(unname(self$types()), unname(sapply(data, class1))))
                # check if no violation of PK and values for static attribute/tie
                if(identical(self$hist,FALSE)){ # can be TRUE only for attrs or ties
                    value_col <- self$cols[!self$cols %chin% self$keys][1L]
                    bad_idx <- self$query()[data, which(eval(call("!=",as.name(value_col),as.name(paste0("i.",value_col)))))]
                    if(length(bad_idx)){
                        stop(paste0("Duplicate key violates defined model. You are trying to insert different value into ",value_col," for same existing identity. If you want want to have multiple values for that identity you should historize that ",class1(self),"."))
                    }
                }
                # filter out exactly same rows by ID and hist
                data <- data[!self$query()]
                # restatement check and idempotency
                if(isTRUE(self$hist) && identical(self$rest,FALSE)){
                    # TO DO
                    browser()
                    # substitute()
                    # get new vs previous row
                    new_vs_prev <- quote(self$query()[data, which(!ST_NAM_Stage_Name == i.ST_NAM_Stage_Name | is.na(ST_NAM_Stage_Name)), roll = +Inf])
                    # get new vs next row
                    new_vs_next <- quote(self$query()[data, which(!ST_NAM_Stage_Name == i.ST_NAM_Stage_Name | is.na(ST_NAM_Stage_Name)), roll = -Inf])
                    # subset
                    data <- data[intersect(eval(new_vs_prev), eval(new_vs_next))]
                }
            }
            self$insert(data)
            private$load_log <- c(private$load_log, list(list(meta = meta[["meta"]], src = meta[["src"]], user = meta[["user"]], timestamp = Sys.time(), code = self$code, in_nrow = in_nrow, unq_nrow = unq_nrow, load_nrow = nrow(data),
                                                              load_time = if(requireNamespace("microbenchmark", quietly=TRUE)) (microbenchmark::get_nanotime() - ts) * 1e-9 else proc.time()[[3L]] - ts)))
            invisible(self)
        },
        size = function() object.size(self$data)
    ),
    private = list(
        load_log = list(list(meta = integer(), src = character(), user = character(), timestamp = Sys.time()[-1L], code = character(), in_nrow = integer(), unq_nrow = integer(), load_nrow = integer(), load_time = numeric()))
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
            self$colorder <- setNames(1:2,self$cols)
            self$coltypes <- setNames(c("ID","meta"),self$cols)
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
            self$colorder <- setNames(c(1L, length(self$cols), if(self$hist) length(self$cols)-1L, 2L),self$cols)
            self$coltypes <- setNames(c("ID", "meta", if(self$hist) "hist", "data"),self$cols)
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
            self$colorder <- setNames(c(length(self$cols), if(self$hist) length(self$cols)-1L, seq_along(c(self$anchors, self$knot))),self$cols)
            self$coltypes <- setNames(c("meta", if(self$hist) "hist", rep("ID",length(c(self$anchors, self$knot)))),self$cols)
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
            self$colorder <- setNames(c(2L, 3L, 1L),self$cols)
            self$coltypes <- setNames(c("data", "meta", "ID"),self$cols)
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
