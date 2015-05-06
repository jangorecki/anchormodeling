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
        initialize = function(naming = c("anchor" = 2L, "attribute" = 3L, "knot" = 3L), hist_col = "ChangedAt"){
            private$naming <- naming
            private$hist_col <- hist_col
            private$log_list <- list(list(event = "initialize AM", timestamp = Sys.time()))
            invisible(self)
        },
        print = function(size.units = getOption("am.size.format")){
            basic_stats <- quote(self$read()[, size := am.size.format(lapply(obj, function(obj) obj$size()), units = size.units)
                                             ][, rows := sapply(obj, function(obj) nrow(obj$data))
                                               ])
            lkp_etl_logs <- quote(self$etl[order(timestamp), tail(.SD,1L),, name])
            if(nrow(self$etl) == 0L){
                print(eval(basic_stats))
                return(invisible(self))
            }
            eval(basic_stats)[eval(lkp_etl_logs), `:=`(meta = i.meta, last_event_time = i.timestamp, event = i.event, in_nrow = i.in_nrow, unq_nrow = i.unq_nrow, load_nrow = i.load_nrow)
                              ][, print(.SD)]
            invisible(self)
        },
        # CRUD
        create = function(class, ..., anchor){
            private$instance_run <- FALSE
            if(length(class) != 1L) stop("create currently support scalar inputs only")
            if(class=="attribute"){
                if(missing(anchor)) stop("Provide anchor mnemonic of that attribute, use `anchor` argument")
                anch <- anchor
            } else if(!missing(anchor)) stop("anchor attribute is used only by attribute, if you are tryng to create tie, use `anchors` argument")
            rm(anchor)
            obj = switch(class,
                         "anchor" = anchormodeling:::anchor$new(...),
                         "attribute" = anchormodeling:::attribute$new(..., anchor = self$read("anchor", anch)), # lookup for anchor desc using in obj unq name
                         "tie" = anchormodeling:::tie$new(...),
                         "knot" = anchormodeling:::knot$new(...),
                         stop("Anchor model objects must be anchor/attribute/tie/knot."))
            self$data <- rbindlist(list(self$data, data.table(name = obj$name, class = class, mne = as.character(obj$mne)[1L], desc = as.character(obj$desc)[1L], obj = list(obj))))
            setkeyv(self$data, c("class","mne"))[]
            private$log_list <- c(private$log_list, list(list(event = "create", obj = obj$name, timestamp = Sys.time())))
            invisible(self)
        },
        read = function(class = c("anchor","attribute","tie","knot"), mne){
            if(missing(mne)) mne <- self$data[,unique(mne)]
            self$data[eval(CJ(class,mne)), nomatch=0L]
        }, # used for self lookup
        update = function(class = c("anchor","attribute","tie","knot"), mne){
            stop("update method not available, use $delete and $create")
            invisible(self)
        },
        delete = function(class = c("anchor","attribute","tie","knot"), mne){
            private$instance_run <- FALSE
            self$data <- self$data[!.(class, mne)]
            setkeyv(self$data, c("class","mne"))[]
            private$log_list <- c(private$log_list, list(list(event = "delete", obj = name, timestamp = Sys.time())))
            invisible(self)
        },
        # RUN
        validate = function(){
            all(
                # name unique
                self$data[, length(name) == uniqueN(name)],
                # each attribute is linked to existing anchor
                all(self$data[class=="attribute", unique(unlist(lapply(obj, function(obj) obj[["anchor"]])))] %chin% self$data[class=="anchor", mne]),
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
            private$instance_run <- TRUE
            private$log_list <- c(private$log_list, list(list(event = "AM instance started", timestamp = Sys.time())))
            invisible(self)
        },
        # ETL
        load = function(mapping, data, meta = NA_integer_, .args){
            browser() # todo dev
            # some cases to update for self$anchor, self$mne instead
            # example ctrl+f: sub_(mne)
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
                #autohist <- FALSE
                if(isTRUE(self$read(mne)$obj[[1L]]$hist)){
                    if(length(mapping[[mne]]) < 2L || !"hist" %chin% names(mapping[[mne]])){
                        #stop("Cannot load historized attribute wihtout mapping of columns on which historize attribute/tie. Provide historize source in the data set by adding `c('AC_NAM', hist = 'HireName')` into attribute/tie mapping.")
                        # auto historize
                        # mapping[[mne]][["hist"]] <- ".am"
                        #autohist <- TRUE
                    }
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
                # check if historized
                autohist <- FALSE
                if(isTRUE(self$read(mne)$obj[[1L]]$hist)){
                    if(length(mapping[[mne]]) < 2L || !"hist" %chin% names(mapping[[mne]])){
                        #stop("Cannot load historized attribute wihtout mapping of columns on which historize attribute/tie. Provide historize source in the data set by adding `c('AC_NAM', hist = 'HireName')` into attribute/tie mapping.")
                        # auto historize
                        # mapping[[mne]][["hist"]] <- ".am"
                        autohist <- TRUE
                    }
                }

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
                #if(length(src_cols)+1!=length(tgt_cols)) browser()
                # subset cols, rename and load further
                self$data[mne, obj][[1L]]$load(data = data[, src_cols, with=FALSE
                                                           ][, c(.SD, if(isTRUE(autohist)) list(`.am.autohist` = rep(Sys.time(),.N)))
                                                             ][, setnames(.SD, c(src_cols, if(isTRUE(autohist)) ".am.autohist"), tgt_cols[-length(tgt_cols)])],
                                               meta = meta)
            }
            invisible(self)
        },
        query = function(){
            # self$data
            stop("not yet ready")
        }
    ),
    private = list(
        log_list = list(),
        instance_run = FALSE,
        naming = integer(),
        hist_col = character()
    ),
    active = list(
        log = function() rbindlist(private$log_list),
        etl = function() rbindlist(lapply(self$data$obj, function(x) x$log))
    )
)

