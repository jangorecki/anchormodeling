#' @title Anchor Modeling metadata manager
#' @docType class
#' @format An R6 class object.
#' @name AM
#' @description Generate an instance of Anchor Model data warehouse.
#' @export
#' @examples
#' am <- AM$new()
#' am$log
#' am$add$A(mne = "AC", desc = "Actor")
#' am$add$a(anchor = "AC", mne = "GEN", desc = "Gender", knot = "GEN")
#' am$data
#' am$log
AM <- R6Class(
    classname = "AM",
    public = list(
        data = data.table(NULL),
        im = NULL,
        initialize = function(naming = c("anchor" = 2L, "attribute" = 3L, "knot" = 3L), hist_col = "ChangedAt"){
            private$naming <- naming # not yet used
            private$hist_col <- hist_col # not yet used
            private$log_list <- list(list(event = "initialize AM", obj = NA_character_, timestamp = Sys.time()))
            im <<- IM$new()
            private$log_list <- c(private$log_list, list(list(event = "initialize IM", obj = NA_character_, timestamp = Sys.time())))
            invisible(self)
        },
        IM = function() im,
        process = function(pretty = FALSE, size.units = getOption("am.size.format")){
            basic_stats <- quote(self$read()[, size := am.size.format(lapply(obj, function(obj) obj$size()), units = size.units)
                                             ][, rows := sapply(obj, function(obj) obj$nrow())
                                               ][, .(name, class, mne, desc, obj, hist, knot, size, rows),, .(code)
                                                 ])
            lkp_etl_logs <- quote(self$etl[order(timestamp), tail(.SD, 1L),, code])
            if(nrow(self$etl) == 0L){
                return(exclude.cols(eval(basic_stats), .escape = !pretty))
            }
            eval(basic_stats)[eval(lkp_etl_logs), `:=`(meta = i.meta, event_timestamp = i.timestamp, event = i.event, in_nrow = i.in_nrow, unq_nrow = i.unq_nrow, load_nrow = i.load_nrow)
                              ][order(-event_timestamp)
                                ][, exclude.cols(.SD, .escape = !pretty)
                                  ]
        },
        print = function(size.units = getOption("am.size.format")){
            if(nrow(self$data)==0L){
                cat("Empty Anchor Model\n", sep="")
            } else {
                self$process(size.units=size.units)[, print(.SD)]
            }
            invisible(self)
        },
        # CRUD
        create = function(class, ..., anchor){
            private$instance_run <- FALSE
            if(length(class) != 1L) stop("create currently support scalar inputs only")
            if(class=="attribute"){
                if(missing(anchor)) stop("Provide anchor mnemonic of the attribute, use `anchor` argument")
                anch <- anchor
            } else if(!missing(anchor)) stop("`anchor` argument in definition is used only by attributes, if you are tryng to create tie, use `anchors` argument")
            rm(anchor)
            obj = switch(class,
                         "anchor" = anchor$new(...),
                         "attribute" = attribute$new(..., anchor = self$read(anch)), # lookup for anchor desc using in obj unq name
                         "tie" = tie$new(...),
                         "knot" = knot$new(...),
                         stop("Anchor model objects must be anchor/attribute/tie/knot."))
            self$data <- rbindlist(list(self$data, data.table(code = obj$code, name = obj$name, class = class, mne = as.character(obj$mne)[1L], desc = as.character(obj$desc)[1L], obj = list(obj), hist = isTRUE(obj$hist), knot = as.character(obj$knot)[1L])))
            setkeyv(self$data, c("code"))[]
            private$log_list <- c(private$log_list, list(list(event = "create", obj = obj$code, timestamp = Sys.time())))
            invisible(self)
        },
        read = function(code, class){
            if(missing(code) && missing(class)) self$data[TRUE]
            else if(!missing(code) && missing(class)){
                cd <- code; rm(code)
                self$data[eval(cd), verbose = getOption("am.key.verbose")] # key
            }
            else if(missing(code) && !missing(class)){
                cl <- class; rm(class)
                self$data[class %chin% eval(cl), verbose = getOption("am.key.verbose")] # key2
            }
            else if(!missing(code) && !missing(class)){
                cd <- code; cl <- class; rm(code, class)
                self$data[eval(cd), verbose = getOption("am.key.verbose")][class %chin% eval(cl), verbose = getOption("am.key.verbose")]
            }
        }, # used for self lookup
        update = function(code, class = c("anchor","attribute","tie","knot")){
            stop("update method not available, use $delete and $create")
            invisible(self)
        },
        delete = function(code, class = c("anchor","attribute","tie","knot")){
            private$instance_run <- FALSE
            self$data <- self$data[!.(code)]
            setkeyv(self$data, c("code"))[]
            private$log_list <- c(private$log_list, list(list(event = "delete", obj = code, timestamp = Sys.time())))
            invisible(self)
        },
        # RUN
        validate = function(){
            if(nrow(self$data)==0L) stop("Anchor Model objects not defined.")
            setkeyv(self$data, "code")[]
            set2keyv(self$data, "class")[]
            all(
                # code unique
                self$data[, length(code) == uniqueN(code)],
                # name unique
                self$data[, length(name) == uniqueN(name)],
                # each attribute is linked to existing anchor
                all(self$data[class=="attribute", unique(unlist(lapply(obj, function(obj) obj[["anchor"]])))] %chin% self$data[class=="anchor", mne]),
                # each tie linked to existing anchors
                all(self$data[class=="tie", unique(unlist(lapply(obj, function(obj) obj[["anchors"]])))] %chin% self$data[class=="anchor", mne]),
                # each knotted attribute linked to existing knot
                all(self$data[class=="attribute", unique(unlist(lapply(obj, function(obj) obj[["knot"]])))] %chin% self$data[class=="knot", mne]),
                # each knotted tie linked to existing knot
                all(self$data[class=="tie", unique(unlist(lapply(obj, function(obj) obj[["knot"]])))] %chin% self$data[class=="knot", mne])
            )
        },
        run = function(){
            if(!self$validate()) stop("AM definition is invalid, see am$validate body for conditions")
            new.cols <- c("anchor","anchors","parents","childs")
            exist.cols <- new.cols[new.cols %chin% names(self$data)]
            if(length(exist.cols)) self$data[, eval(exist.cols) := NULL]
            self$data[class=="attribute", anchor := sapply(obj, function(obj) as.character(obj$anchor))
                      ][class=="tie", anchors := lapply(obj, function(obj) as.character(obj$anchors))
                        ]
            self$data[class=="attribute", parents := lapply(obj, function(obj) c(as.character(obj$knot), as.character(obj$anchor)))
                      ]
            self$data[self$read(class="attribute")[,.(childs = list(code)),,anchor], childs := i.childs
                      ]
            private$instance_run <- TRUE
            private$log_list <- c(private$log_list, list(list(event = "AM instance started", obj = NA_character_, timestamp = Sys.time())))
            invisible(self)
        },
        # ETL
        load = function(mapping, data, meta = NA_integer_, use.im=TRUE){
            if(!isTRUE(private$instance_run)) stop("Run DW instance by am$run()")
            stopifnot(
                is.list(mapping),
                length(mapping)>0L,
                is.data.table(data),
                all(names(mapping) %chin% self$data$mne), # exists in defined AM
                length(names(data))==uniqueN(names(data)) # no duplicate names in `data` allowed
            )
            if(!all(names(mapping) %chin% self$read(class = c("anchor"))$mne)){
                stop(paste0("In the mapping definition names should be mne of anchors, related to: ", paste(names(mapping)[names(mapping) %chin% self$read(class = c("anchor"))], collapse=", ")))
            } # nodes in mapping only anchors, handle: add tie, attributes nested, knots autoloaded, maybe tie autoloading too?
            if(!all(sapply(mapping, function(x, data.names) sapply(x, valid_entity_params, data.names), data.names = names(data)))){
               stop(paste0("Invalid entity params provided, it should already be catched. A debug on `valid_entity_params`"))
            } # src columns "" / NULL exists in data to load # apply over elements in the mapping and then over names of each attribute definition
            model_all_attr_codes_for_anchors <- setNames(self$read(names(mapping))$childs, names(mapping))
            # key by hist and knot only for batch attributes match to model, by defined hist, knot, mne, anchor
            model_attrs_lkp <- quote(self$read(unique(unlist(model_all_attr_codes_for_anchors)))[, .(code),, .(class, anchor, mne, hist, knot)])
            # transform mapping to data.table
            mapping_attrs_dt <- tryCatch(
                rbindlist(lapply(names(mapping),
                                 function(Aname) data.table(
                                     class = "attribute",
                                     anchor = Aname,
                                     rbindlist(lapply(names(mapping[[Aname]])[names(mapping[[Aname]]) != ""], # exclude anchor NK
                                                      function(aname) data.table(
                                                          mne = aname,
                                                          src_col = sapply(aname, function(anm) mapping[[Aname]][[anm]][1L]),
                                                          hist = sapply(aname, function(anm) !is.na(as.character(as.list(mapping[[Aname]][[anm]])[["hist"]])[1L])),
                                                          knot = sapply(aname, function(anm) as.character(as.list(mapping[[Aname]][[anm]])[["knot"]])[1L]),
                                                          hist_col = sapply(aname, function(anm) as.character(as.list(mapping[[Aname]][[anm]])[["hist"]])[1L])
                                                      )
                                     ))
                                 )
                )),
                warning = function(w) stop("Invalid mapping definition, see examples how to define mapping"),
                error = function(e) stop("Invalid mapping definition, see examples how to define mapping")
            )
            setkeyv(mapping_attrs_dt, c("class","anchor","mne","hist","knot"))
            # first pass loop checks on composite key join
            mapping_attrs_dt[ eval(model_attrs_lkp), `:=`(code = i.code)]
            if(any(is.na(mapping_attrs_dt$code))){
                stop(paste0("Some of the provided attributes have incorrect definition versus model: ", paste(mapping_attrs_dt[is.na(code), paste(anchor, mne, sep="_")], collapse=", ")))
            } # all provided attributes in the mapping exists in model for those anchors, with expected hist and knot
            if(use.im){
                data <- im$use(data, mne = mapping_attrs_dt[, unique(na.omit(c(anchor, knot)))], nk = lapply(mapping, `[[`, 1L), in.place = FALSE)
            } # auto Identity Management: anchors and knots get ID in incoming data and in am$IM() but not yet in obj$data
            # prepare sequence of processing
            load_seq <- setkeyv(rbindlist(
                list(
                    mapping_attrs_dt[, .(class = "anchor", mne = anchor, code = anchor, hist = FALSE, knot = NA_character_, src_col = paste_(anchor,"ID"), hist_col = NA_character_), .(anchor)],
                    mapping_attrs_dt[!is.na(knot), .(class = "knot", anchor = NA_character_, mne = knot, code = knot, hist = FALSE, knot = NA_character_, src_col = NA_character_, hist_col = NA_character_), .(byknot = knot)][, .SD, .SDcols=-"byknot"],
                    mapping_attrs_dt[, .(anchor, class = "attribute", mne, code, hist, knot, src_col = src_col, hist_col = hist_col)]
                )), c("class","anchor"))
            set2keyv(load_seq, "code")
            # first pass loop, only check if mapping matches, fill defaults, etc.
            # ?
            # loading knots
            if.mclapply(load_seq["knot", code, nomatch=0L], function(knot_code){
                src_cols <- load_seq[c("attribute","tie"), nomatch=0L][knot==knot_code, src_cols]
                src_cols <- c(paste_(knot_code,"ID"), src_cols)
                cols <- self$read(knot_code)$obj[[1L]]$cols
                cols <- cols[-length(cols)] # exclude metadata col
                value.name <- self$read(knot_code)$name
                stopifnot(c(src_cols[1L], value.name) == cols)
                self$data[knot_code, obj][[1L]]$load(
                    data = melt(data[, src_cols, with=FALSE], id.vars = src_cols[1L], measure.vars = src_cols[-1L], value.name = value.name)[, .SD, .SDcols=-"variable"], # support for multi child knots, will load from all src_cols into one
                    meta = meta
                )
            })
            # loading anchors
            if.mclapply(load_seq["anchor", code, nomatch=0L], function(anchor_code){
                src_cols <- load_seq[code==anchor_code, src_col]
                cols <- self$read(anchor_code)$obj[[1L]]$cols
                cols <- cols[-length(cols)] # exclude metadata col
                stopifnot(src_cols == cols)
                self$data[anchor_code, obj][[1L]]$load(
                    data = data[, src_cols, with=FALSE],
                    meta = meta
                )
                # loading child attributes
                if.mclapply(load_seq[CJ("attribute", anchor_code), code, nomatch=0L], function(attr_code){
                    src_cols <- load_seq[code==attr_code, c(if(is.na(knot)) src_col else paste_(knot,"ID"), if(hist) hist_col else character())]
                    src_cols <- c(paste_(anchor_code,"ID"), src_cols)
                    cols <- self$read(attr_code)$obj[[1L]]$cols
                    cols <- cols[-length(cols)] # exclude metadata col
                    self$data[attr_code, obj][[1L]]$load(
                        data = data[, src_cols, with=FALSE][, setnames(.SD, src_cols, cols)],
                        meta = meta
                    )
                })
            })
            invisible(self)
        },
        view = function(key){
            # denormalize to 3NF
            # optionally setkey
            stop("not yet ready, use am$read()")
        },
        xml = function(file = format(Sys.time(),"AM_%Y%m%d_%H%M%S.xml")){
            meta_header <- paste0('<schema format="0.98" date="',format(Sys.Date(),"%Y-%m-%d"),'" time="',format(Sys.time(),"%H:%M:%S"),'">')
            tech_header <- '<metadata changingRange="datetime" encapsulation="dbo" identity="int" metadataPrefix="Metadata" metadataType="int" metadataUsage="true" changingSuffix="ChangedAt" identitySuffix="ID" positIdentity="int" positGenerator="true" positingRange="datetime" positingSuffix="PositedAt" positorRange="tinyint" positorSuffix="Positor" reliabilityRange="tinyint" reliabilitySuffix="Reliability" reliableCutoff="1" deleteReliability="0" reliableSuffix="Reliable" partitioning="false" entityIntegrity="true" restatability="true" idempotency="false" assertiveness="true" naming="improved" positSuffix="Posit" annexSuffix="Annex" chronon="datetime2(7)" now="sysdatetime()" dummySuffix="Dummy" versionSuffix="Version" statementTypeSuffix="StatementType" checksumSuffix="Checksum" businessViews="false" equivalence="false" equivalentSuffix="EQ" equivalentRange="tinyint" databaseTarget="SQLServer" temporalization="uni"/>'
            footer <- "</schema>"
            lines <- c(meta_header, tech_header)
            lines <- c(lines, self$read(class="knot")[, sapply(obj, function(obj) obj$xml())])
            matched_attr <- quote(self$read(class="attribute")[sapply(obj, function(obj) obj[["anchor"]])==anchor_code])
            for(anchor_code in self$read(class="anchor")$code){ # for each anchor nest attributes
                if(nrow(eval(matched_attr)) > 0) lines <- c(lines, self$read(code = anchor_code)[, sapply(obj, function(obj) obj$xml(attributes = eval(matched_attr)))]) # batch lookup to all anchors attributes
            }
            lines <- c(lines, unlist(sapply(self$read(class="tie")$obj, function(obj) obj$xml())))
            lines <- c(lines, footer)
            private$log_list <- c(private$log_list, list(list(event = "AM model exported", obj = file, timestamp = Sys.time())))
            write(lines, file=file, append=FALSE)
            invisible(file)
        },
        dashboard = function(){
            if(!isTRUE(private$instance_run)) stop("Run DW instance by am$run()")
            suggests_deps <- c("shiny","shinydashboard","DT")
            if(!all(sapply(suggests_deps, requireNamespace, quietly=TRUE))){
                stop(paste0("install required packages: ",paste(suggests_deps[!sapply(suggests_deps, requireNamespace, quietly=TRUE)], collapse=", ")))
            } else {
                options("am.share" = self)
                shiny::runApp(system.file("app","monitor", package = "anchormodeling"))
            }
            invisible(self)
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
        etl = function() setkeyv(rbindlist(lapply(self$data$obj, function(x) x$log)), c("meta","code")),
        add = function(){
            list("anchor" = function(...) self$create(class = "anchor", ...),
                 "A" = function(...) self$create(class = "anchor", ...),
                 "attribute" = function(..., anchor) self$create(class = "attribute", ..., anchor = anchor), # must provide anchor directly because it is lookedup before initialize AMobj
                 "a" = function(..., anchor) self$create(class = "attribute", ..., anchor = anchor),
                 "tie" = function(...) self$create(class = "tie", ...),
                 "t" = function(...) self$create(class = "tie", ...),
                 "knot" = function(...) self$create(class = "knot", ...),
                 "k" = function(...) self$create(class = "knot", ...))
        }
    )
)

