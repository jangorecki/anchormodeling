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
            private$log_list <- list(list(event = "initialize IM", obj = NA_character_, timestamp = Sys.time()))
            invisible(self)
        },
        process = function(pretty = FALSE, size.units = getOption("am.size.format")){
            basic_stats <- quote(self$read()[, size := am.size.format(lapply(obj, function(obj) obj$size()), units = size.units)
                                             ][, rows := sapply(obj, function(obj) nrow(obj$data))
                                               ][, .(name, class, mne, desc, obj, hist, knot, size, rows),, .(code)
                                                 ])
            lkp_etl_logs <- quote(self$etl[order(timestamp), tail(.SD, 1L),, code])
            if(nrow(self$etl) == 0L){
                return(exclude.cols(eval(basic_stats), .escape = !pretty))
            }
            eval(basic_stats)[eval(lkp_etl_logs), `:=`(meta = i.meta, last_event_time = i.timestamp, event = i.event, in_nrow = i.in_nrow, unq_nrow = i.unq_nrow, load_nrow = i.load_nrow)
                              ][order(-last_event_time)
                                ][, exclude.cols(.SD, .escape = !pretty)
                                  ]
        },
        print = function(size.units = getOption("am.size.format")){
            self$process(size.units=size.units)[, print(.SD)]
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
        load = function(mapping, data, meta = NA_integer_, .args, use.im=FALSE){
            if(!isTRUE(private$instance_run)) stop("Run DW instance by am$run()")
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
                length(names(data))==uniqueN(names(data)) # no duplicate names in `data` allowed
            )
            if(!all(names(mapping) %chin% self$read(class = c("anchor"))$mne)){
                stop(paste0("In the mapping definition names should be mne of anchors, related to: ", paste(names(mapping)[names(mapping) %chin% self$read(class = c("anchor"))], collapse=", ")))
            } # nodes in mapping only anchors, handle: add tie, attributes nested, knots autoloaded, maybe tie autoloading too?
            if(!all(sapply(mapping, function(x, data.names) sapply(x, valid_entity_params, data.names), data.names = names(data)))){
               stop(paste0("Invalid entity params provided, it should already be catched. A debug on `valid_entity_params`"))
            } # src columns "" / NULL exists in data to load # apply over elements in the mapping and then over names of each attribute definition
            model_all_attr_codes_for_anchors <- setNames(self$read(names(mapping))$childs, names(mapping))
            model_attrs_lkp <- quote(self$read(unique(unlist(model_all_attr_codes_for_anchors)))[, .(code, hist, knot),, .(anchor, mne)])
            mapping_attrs_dt <- rbindlist(lapply(names(mapping), function(nm) data.table(anchor = nm, mne = names(mapping[[nm]]))))
            setkeyv(mapping_attrs_dt, c("anchor","mne"))
            mapping_attrs_dt[ eval(model_attrs_lkp), `:=`(code = i.code, hist = i.hist, knot = i.knot)]
            if(any(is.na(mapping_attrs_dt$code))){
                stop(paste0("Some of the provided attributes do not exists in the model: ", paste(mapping_attrs_dt[is.na(code), paste(anchor, mne, sep="_")], collapse=", ")))
            } # all provided attributes in the mapping exists in model for those anchors
            browser()
            #  DEV
            if(isTRUE(use.im)){
                im$use(data, mne = c(NA), in.place = TRUE)
            } # auto Identity Management
            # prepare sequence of processing
            all_codes <- rbindlist(list(top_codes, attr_codes))
            # sort the data to load
            am.order <- c("anchor" = 1L, "knot" = 2L, "attribute" = 3L, "tie" = 4L) # order of AMobj
            all_codes <- all_codes[, .(code, class), keyby=.(am.order[class])] # reorder codes for execution
            # first pass loop, only check if mapping matches, fill defaults, etc.
            for(code in all_codes$code){
                iter <- self$read(code)[, .(mne,name,class,hist = sapply(obj, function(obj) isTRUE(obj$hist)),knot = sapply(obj, function(obj) as.character(obj$knot)[1L]),anchor = sapply(obj, function(obj) as.character(obj$anchor)[1L]))]
                if(!is.na(iter$knot)){
                    if(!iter$knot %chin% top_codes[class=="knot", code]) stop("Cannot load knotted attribute/tie without also loading knot for it, provide knot mapping.")
                } # check if knotted
                if(isTRUE(iter$hist)){
                    if(iter$class=="attribute"){
                        if(!"hist" %chin% names(mapping[[iter$anchor]][[iter$mne]])){
                            stop(paste0("Cannot load historized ",iter$class," wihtout mapping of column on which historize ",iter$class,". Provide to the mapping 'hist' field for that ",iter$class," and give name of source dataset columns."))
                        }
                    }
                    else if(iter$class=="tie"){
                        if(!"hist" %chin% names(mapping[[iter$mne]])){
                            stop(paste0("Cannot load historized ",iter$class," wihtout mapping of column on which historize ",iter$class,". Provide to the mapping 'hist' field for that ",iter$class," and give name of source dataset columns."))
                        }
                    }
                } # check if historized must containt field "hist"
                if(iter$class=="anchor"){
                    if(length(mapping[[iter$mne]]) == 0L){
                        if(!paste(iter$mne,"ID",sep="_") %chin% names(data)) stop(paste0("If anchor ID field was not provided it has to be in naming convention '",paste(iter$mne,"ID",sep="_"),"' (as IM class produces, see ?IM), otherwise provide anchor ID source column as unnamed character 'AC' into anchor mapping."))
                        mapping[[iter$mne]][[paste_(iter$mne,"ID")]] <- paste_(iter$mne,"ID")
                    } # use automapping for default naming convention column, [mne]_ID
                    if(!all(sapply(names(mapping[[iter$mne]]), function(child_mne) is.null(names(mapping[[iter$mne]][[child_mne]])) || all(names(mapping[[iter$mne]][[child_mne]]) %chin% c("","anchors","anchor","roles","identifier","knot","hist"))))){
                        stop(paste0('Element ',code,' in your mapping definition contains some childs elements which names do not match to allowed ',paste(c("","anchors","anchor","roles","identifier","knot","hist"),collapse=", ")))
                    } # attributes nested in anchor character names validation
                } # automapping anchor by name convention, check column names
                if(iter$class=="knot"){
                    if(length(mapping[[iter$mne]]) == 1L){
                        if(!paste(iter$mne,"ID",sep="_") %chin% names(data)) stop(paste0("If knot ID field was not provided it has to be in naming convention '",paste(iter$mne,"ID",sep="_"),"' (as IM class produces, see ?IM), otherwise provide knot ID source in the data set by adding `c('GEN', id = 'GEN_ID')` into knot mapping."))
                        mapping[[iter$mne]] <- c(mapping[[iter$mne]], id = paste(iter$mne,"ID",sep="_"))
                    } # use automapping for default naming convention column, [mne]_ID
                } # automapping for knots ID by name convention
            }
            browser()
            # second pass loop, subset columns and provide to AMobj classes for loading
            for(code in top_code){
                # check if historized
                if(isTRUE(self$read(mne)$obj[[1L]]$hist)){
                    #if(length(mapping[[mne]]) < 2L || !"hist" %chin% names(mapping[[mne]])){
                    #    #stop("Cannot load historized attribute wihtout mapping of columns on which historize attribute/tie. Provide historize source in the data set by adding `c('AC_NAM', hist = 'HireName')` into attribute/tie mapping.")
                    #    # auto historize
                    #    # mapping[[mne]][["hist"]] <- ".am"
                    #    autohist <- TRUE
                    #}
                }

                src_cols <- mapping[[mne]]
                src_cols <- c(src_cols[is.null(names(src_cols))], src_cols[names(src_cols) %chin% c("id")], src_cols[names(src_cols) %chin% c("")], src_cols[names(src_cols) %chin% c("hist")]) # reorder cols
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
        etl = function() rbindlist(lapply(self$data$obj, function(x) x$log)),
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

