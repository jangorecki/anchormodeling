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
            private$naming <- naming
            private$hist_col <- hist_col
            private$log_list <- list(list(event = "initialize AM", obj = NA_character_, timestamp = Sys.time()))
            self$im <- IM$new(naming = naming)
            private$instance_run <- FALSE
            private$log_list <- c(private$log_list, list(list(event = "initialize IM", obj = NA_character_, timestamp = Sys.time())))
            invisible(self)
        },
        IM = function() self$im,
        process = function(pretty = FALSE, size.units = getOption("am.size.format")){
            basic_stats <- quote(self$read()[, size := am.size.format(lapply(obj, function(obj) obj$size()), units = size.units)
                                             ][, rows := sapply(obj, function(obj) obj$nrow())
                                               ][, .(name, class, mne, desc, obj, hist, knot, size, rows),, .(code)
                                                 ])
            lkp_etl_logs <- quote(self$etl[order(timestamp), tail(.SD, 1L),, code])
            if(nrow(self$etl) == 0L){
                return(exclude.cols(eval(basic_stats), .escape = !pretty))
            }
            eval(basic_stats)[eval(lkp_etl_logs), `:=`(meta = i.meta, last_load = i.timestamp, in_nrow = i.in_nrow, unq_nrow = i.unq_nrow, load_nrow = i.load_nrow, load_time = i.load_time)
                              ][order(-last_load)
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
            self$stop()
            # input check
            if(length(class) != 1L) stop("create currently support scalar inputs only")
            if(class=="attribute"){
                if(missing(anchor)) stop("Provide anchor mnemonic of the attribute, use `anchor` argument")
                anch <- anchor
            } else if(!missing(anchor)) stop("`anchor` argument in definition is used only by attributes, if you are tryng to create tie, use `anchors` argument")
            rm(anchor)
            obj = switch(class,
                         "anchor" = anchor$new(...),
                         "attribute" = attribute$new(..., hist_col = private$hist_col, anchor = self$read(anch)), # lookup for anchor desc using in obj unq name
                         "tie" = tie$new(..., hist_col = private$hist_col),
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
                if(is.null(cd)) cd <- FALSE
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
        update = function(code){
            stop("Update method not available, use $delete and $create or simply create the new one.")
            invisible(self)
        },
        delete = function(code){
            private$instance_run <- FALSE
            v.code <- code; rm(code)
            self$data <- self$data[!v.code]
            setkeyv(self$data, c("code"))[]
            private$log_list <- c(private$log_list, list(list(event = "delete", obj = v.code, timestamp = Sys.time())))
            invisible(self)
        },
        # RUN
        validate = function(){
            if(nrow(self$data)==0L) stop("Anchor Model objects not defined.")
            setkeyv(self$data, "code")[]
            set2keyv(self$data, "class")[]
            if(!self$data[, length(code) == uniqueN(code)]) stop("Codes are not unique.")
            if(!self$data[, length(name) == uniqueN(name)]) stop("Names are not unique.")
            if(self$data[class=="anchor", .N] == 0L) stop("At least one anchor must be defined")
            if(!all(self$data[class=="attribute", unique(unlist(lapply(obj, function(obj) obj[["anchor"]])))] %chin% self$data[class=="anchor", mne])) stop("Each attribute must be linked to existing anchor.")
            if(!all(self$data[class=="tie", unique(unlist(lapply(obj, function(obj) obj[["anchors"]])))] %chin% self$data[class=="anchor", mne])) stop("Each tie must be linked to existing anchors.")
            if(!all(self$data[class=="attribute", unique(unlist(lapply(obj, function(obj) obj[["knot"]])))] %chin% self$data[class=="knot", mne])) stop("Each knotted attribute must be linked to existing knot.")
            if(!all(self$data[class=="tie", unique(unlist(lapply(obj, function(obj) obj[["knot"]])))] %chin% self$data[class=="knot", mne])) stop("Each knotted tie must be linked to existing knot.")
            if(self$data[class=="anchor", .N] > 1L){ # exclude single 1 anchors from validation
                if(!all(self$data[class=="anchor", mne] %chin% self$data[class=="tie", unique(unlist(lapply(obj, function(obj) obj[["anchors"]])))])) stop("All anchors must be linked with ties.")
            }
            if(!all(self$data[class=="tie", unique(unlist(lapply(obj, function(obj) obj[["anchors"]])))] %chin% self$data[class=="anchor", mne])) stop("All anchors defined for ties must exists in model.")
            if(!all(self$data[class=="knot", mne] %chin% self$data[class %chin% c("attribute","tie"), unique(na.omit(knot))])) stop("All knots must be connected to tie or attribute.")
            invisible(self$data[class!="tie", .(mne, valid = private$naming[class]==nchar(mne)), class][, if(any(!valid)) stop(paste0("Following entities brakes declared naming convention: ", paste(mne[!valid], collapse=", ")))])
            TRUE
        },
        run = function(){
            if(!self$validate()) stop("AM definition is invalid, see am$validate body for conditions")
            new.cols <- c("anchor","anchors","parents","childs")
            exist.cols <- new.cols[new.cols %chin% names(self$data)]
            if(length(exist.cols)) self$data[, eval(exist.cols) := NULL]
            # refresh metadata
            self$data[class=="attribute", anchor := sapply(obj, function(obj) as.character(obj$anchor))]
            if(self$data[class=="tie",.N > 0L]){
                self$data[class=="tie", `:=`(anchors = lapply(obj, function(obj) c(obj$anchors)))]
            } else {
                self$data[, anchors := list(lapply(code, as.null))]
            }
            self$data[class=="attribute", parents := lapply(obj, function(obj) c(as.character(obj$knot), as.character(obj$anchor)))]
            if(self$data[class=="attribute",.N > 0L]){
                self$data[self$read(class="attribute")[,.(childs = list(code)),,anchor], childs := list(i.childs), by = .EACHI]
            } else {
                self$data[, childs := list(lapply(code, as.null))]
                if(!"anchor" %in% names(self$data)) self$data[, anchor := NA_character_] # workaround for data.table#1166
            }
            private$instance_run <- TRUE
            private$log_list <- c(private$log_list, list(list(event = "start AM instance", obj = NA_character_, timestamp = Sys.time())))
            invisible(self)
        },
        # ETL
        load = function(mapping, data, meta = NA_integer_, use.im=TRUE){
            if(!isTRUE(private$instance_run)) stop("Run DW instance by am$run()")
            data.sub <- substitute(data)
            stopifnot(
                is.list(mapping),
                length(mapping)>0L,
                is.data.table(data),
                length(names(data))==uniqueN(names(data)) # no duplicate names in `data` allowed
            )
            anchors_mne <- names(mapping)[names(mapping) %chin% self$read(class="anchor")$mne]
            not_anchors_mne <- names(mapping)[!names(mapping) %chin% anchors_mne]
            # tie related handling
            ties_code <- names(mapping)[names(mapping) %chin% self$read(class="tie")$code]
            if(length(not_anchors_mne) > 0L && nrow(self$read(class="tie")) > 0){
                tie_duplicate_short_code <- not_anchors_mne[
                    not_anchors_mne %chin% self$read(class="tie")[, .(short_code = paste_(c(unlist(anchors), na.omit(knot)))), code][, .(N = .N), short_code][N > 1L, short_code]
                    ]
                if(length(tie_duplicate_short_code) > 0L){
                    stop(paste0("Non-unique code lookup for short tie code: ",paste(tie_duplicate_short_code, collapse=", "),". Include roles in provided tie entity in defined mapping. Use tie codes from you AM instance."))
                }
                tie_map_short_code <- not_anchors_mne[!not_anchors_mne %chin% ties_code]
                tie_mapped <- self$read(class="tie")[,.(short_code = paste_(c(unlist(anchors), na.omit(knot)))), code][,.SD,, short_code][tie_map_short_code, .(code), .EACHI, nomatch=NA_character_]
                if(tie_mapped[is.na(code), .N > 0L]){
                    stop(paste0("Following short code of tie was not able to map to any tie:",tie_mapped[is.na(code), paste(tie_mapped, short_code=", ")],". See am print method for defined entities and provide tie code."))
                }
                names(mapping)[names(mapping) == tie_map_short_code] <- tie_mapped[tie_map_short_code, code] # mapping elements renamed
                ties_code <- unique(c(ties_code, tie_mapped$code)) # all ties remapped to unique codes
            }
            # general processing
            if(length(mapping)!=length(c(anchors_mne, ties_code))){
                stop("Mapping list definition should contain only anchor memonics or unique codes of ties based on pasted anchors and knot mnes. See AM instance print for defined entities.")
            }
            if(!all(anchors_mne %chin% self$read(class = c("anchor"))$mne)){
                stop(paste0("In the mapping definition names should be mne of anchors, related to: ", paste(anchors_mne[anchors_mne %chin% self$read(class = c("anchor"))], collapse=", ")))
            } # nodes in mapping only anchors, handle: add tie, attributes nested, knots autoloaded, maybe tie autoloading too?
            if(!all(unlist(mapping) %chin% names(data))){
                stop(paste0("Following defined columns do not exists in source data: ",paste(unlist(mapping)[!unlist(mapping) %chin% names(data)],collapse=", ")))
            } # all leafs of mapping should be src col names and exists in data
            if(!all(sapply(mapping, function(x, data.names) all(sapply(x, valid_entity_params, data.names)), data.names = names(data)))){
                stop(paste0("Invalid entity params provided"))
            } # src columns "" / NULL exists in data to load # apply over elements in the mapping and then over names of each attribute definition
            model_all_attr_codes_for_anchors <- setNames(self$read(anchors_mne)$childs, anchors_mne)
            # key by hist and knot only for batch attributes match to model, by defined hist, knot, mne, anchor
            model_attrs_lkp <- quote(self$read(unique(unlist(model_all_attr_codes_for_anchors)))[, .(code, knot),, .(class, anchor, mne, hist)])
            # transform mapping to data.table
            mapping_attrs_dt <- rbindlist(lapply(anchors_mne, A.dt, mapping))
            setkeyv(mapping_attrs_dt, c("class","anchor","mne","hist"))
            # first pass loop checks on composite key join
            mapping_attrs_dt[ eval(model_attrs_lkp), `:=`(code = i.code, knot = i.knot)]
            if(!"code" %chin% names(mapping_attrs_dt)){
                mapping_attrs_dt[, `:=`(code = NA_character_, knot = NA_character_)]
            } # workaround for data.table#1166
            if(any(is.na(mapping_attrs_dt$code))){
                stop(paste0("Some of the provided attributes have incorrect definition versus model: ", paste(mapping_attrs_dt[is.na(code), paste(anchor, mne, sep="_")], collapse=", "),". Check if they are not missing `hist` column when defined in model as historized."))
            } # all provided attributes in the mapping exists in model for those anchors, with expected hist and knot
            # prepare sequence of processing
            load_seq <- rbindlist(list(
                "anchor" = data.table(anchor = NA_character_, class = "anchor", mne = anchors_mne, code = anchors_mne, hist = FALSE, knot = NA_character_, src_col = paste(anchors_mne,"ID",sep="_"), hist_col = NA_character_),
                "knot" = unique(rbindlist(list(
                    "knot of attr" = mapping_attrs_dt[!is.na(knot), .(anchor = NA_character_, class = "knot", mne = knot, code = knot, hist = FALSE, knot = NA_character_, src_col = NA_character_, hist_col = NA_character_), .(byknot = knot)][, unique(.SD), .SDcols=-"byknot"],
                    "knot of tie" = self$read(ties_code)[!is.na(knot), .(anchor = NA_character_, class = "knot", mne = knot, code = knot, hist = FALSE, knot = NA_character_, src_col = NA_character_, hist_col = NA_character_), .(byknot = knot)][, unique(.SD), .SDcols=-"byknot"]
                ))),
                "attr" = mapping_attrs_dt[, .(anchor, class = rep("attribute",length(mne)), mne, code, hist, knot, src_col, hist_col)],
                "tie" = rbindlist(c(
                    list(data.table(code = character(), src_col = character(), hist = logical(), knot = character(), hist_col = character())),
                    lapply(ties_code, T.dt, mapping)
                ))[, .(anchor = rep(NA_character_,length(code)), class = rep("tie",length(code)), mne = rep(NA_character_,length(code)), code, hist, knot, src_col, hist_col)]
            ))
            setkeyv(load_seq, c("class","anchor"))
            set2keyv(load_seq, "code")
            # first pass loop, only check if mapping matches, fill defaults, etc. ?
            if(use.im){
                # build nk mapping
                nk <- lapply(mapping[anchors_mne],`[[`,1L)
                knot_mne <- load_seq["knot", unique(mne), nomatch=0L]
                if(length(knot_mne) > 0L){
                    knot_mapping <- as.list(load_seq[knot==knot_mne, setNames(list(c(src_col)), knot_mne)])
                    nk <- c(nk, knot_mapping)
                }
                data <- self$im$use(data, mne = names(nk), nk = nk, in.place = FALSE)
            } # auto Identity Management: anchors and knots get ID in incoming data and in am$IM() but not yet in obj$data
            if(is.integer(meta) || is.numeric(meta)){
                meta <- list(meta = as.integer(meta), user = as.character(Sys.info()[["user"]])[1L], src = paste(deparse(data.sub), collapse="\n")[1L])
            } else if(is.data.table(meta) || is.list(meta)){
                if(!"meta" %chin% names(meta)) meta[["meta"]] <- NA_integer_
                if(!"user" %chin% names(meta)) meta[["user"]] <- as.character(Sys.info()[["user"]])[1L]
                if(!"src" %chin% names(meta)) meta[["src"]] <- paste(deparse(data.sub), collapse="\n")[1L]
                stopifnot(all(c("meta","user","src") %chin% names(meta)))
            } else stop ("Invalid meta argument")
            # loading knots
            lapply(load_seq["knot", code, nomatch=0L], function(knot_code){
                src_cols <- load_seq[knot==knot_mne, c(src_col)]
                cols <- self$OBJ(knot_code)$cols
                cols <- cols[-length(cols)] # exclude metadata col
                if(length(src_cols)==1L){
                    self$OBJ(knot_code)$load(
                        data = data[, c(paste_(src_cols, knot_code, "ID"), src_cols), with=FALSE][, setnames(.SD, c(paste_(src_cols, knot_code, "ID"), src_cols), cols)],
                        meta = meta
                    )
                } else {
                    # shared knots
                    self$OBJ(knot_code)$load(
                        data = melt(data = data[, c(paste(src_cols, knot_code, "ID", sep="_"), src_cols), with=FALSE],
                                    measure.vars = list(1:2, seq_len(length(src_cols))+2L),
                                    variable.name = "variable",
                                    value.name = cols)[, .SD, .SDcols=-"variable"],
                        meta = meta
                    )
                } # shared knots
            })
            # loading anchors
            lapply(load_seq["anchor", code, nomatch=0L], function(anchor_code){
                src_cols <- load_seq[code==anchor_code, src_col]
                cols <- self$OBJ(anchor_code)$cols
                cols <- cols[-length(cols)] # exclude metadata col
                if(!identical(src_cols,cols)) stop(paste0("Expected columns for anchor ",anchor_code, "do not exist in incoming data, use built-in IM or provide mne_ID columns for anchors."), call. = FALSE)
                self$OBJ(anchor_code)$load(
                    data = data[, src_cols, with=FALSE], # anchor names already maps from auto-im
                    meta = meta
                )
                # loading child attributes
                lapply(load_seq[CJ("attribute", anchor_code), code, nomatch=0L], function(attr_code){
                    src_cols <- load_seq[code==attr_code, c(if(is.na(knot)) src_col else paste_(src_col, knot,"ID"), if(hist) hist_col else character())]
                    src_cols <- c(paste_(anchor_code,"ID"), src_cols)
                    cols <- self$OBJ(attr_code)$cols
                    cols <- cols[-length(cols)] # exclude metadata col
                    self$OBJ(attr_code)$load(
                        data = data[, src_cols, with=FALSE][, setnames(.SD, src_cols, cols)],
                        meta = meta
                    )
                })
            })
            # loading ties
            lapply(load_seq["tie", code, nomatch=0L], function(tie_code){
                if(!all(self$OBJ(tie_code)$anchors %chin% names(mapping))) stop(paste0("Cannot load tie ",tie_code," without loading anchors for it, missing anchor in load: ",paste(self$OBJ(tie_code)$anchors[!self$OBJ(tie_code)$anchors %chin% names(mapping)], collapse=", "),"."), call. = FALSE)
                src_cols <- paste0(names(mapping)[names(mapping) %chin% self$OBJ(tie_code)$anchors], "_ID")
                src_cols <- c(src_cols, mapping[[tie_code]][["knot"]], mapping[[tie_code]][["hist"]])
                cols <- self$OBJ(tie_code)$cols
                cols <- cols[-length(cols)] # exclude metadata col
                self$OBJ(tie_code)$load(
                    data = data[, src_cols, with=FALSE][, setnames(.SD, src_cols, cols)],
                    meta = meta
                )
            })
            invisible(self)
        },
        OBJ = function(code) self$read(code)$obj[[1L]],
        joinv = function(master, join, allow.cartesian){
            # simplified jangorecki/dwtools::joinbyv
            # basic input check: master and join
            stopifnot(all(!missing(master),!missing(join))) # non missing mandatory args
            if(!is.data.table(master)) master <- master[[1]] # conver master list(DT) to DT
            stopifnot(
                is.data.table(master),
                !is.data.table(join),
                is.list(join),
                haskey(master),
                all(sapply(join, haskey))
            )
            # for non-difference view exec main loop with lookups, also difference views where all anchor attributes are non historized
            if(!allow.cartesian){
                master <- copy(master)
                for(i in 1:length(join)){
                    # faster way thanks to: http://stackoverflow.com/questions/30468455/dynamically-build-call-for-lookup-multiple-columns
                    lkp_cols <- names(join[[i]])[-1L]
                    master[join[[i]], c(lkp_cols) := mget(paste0('i.', lkp_cols))]
                    # old slower:
                    # master <- join[[i]][master, allow.cartesian = allow.cartesian][, setnames(.SD,names(join[[i]])[1L],names(master)[1L])]
                }
            }
            # for difference view exec lookup distinct id-time, union and then use as master and rolling to joins
            if(allow.cartesian){
                master <- unique(rbindlist(lapply(join, function(join) if(length(key(join)) == 2L) join[, unique(.SD), .SDcols = c(key(join))])))
                setkeyv(master, names(master))
                browser()
                for(i in 1:length(join)){
                    # TO DO add roll join as substitute of LATERAL JOIN
                    master <- join[[i]][master, allow.cartesian = allow.cartesian][, setnames(.SD,names(join[[i]])[1L],names(master)[1L])]
                }
            }
            master
        },
        view = function(mne, type = "current", timepoint){
            stopifnot(mne %chin% self$read(class=c("anchor","tie"))$mne, type %chin% c("latest","timepoint","current","difference"))
            if(mne %chin% self$read(class="tie")$mne) return(self$OBJ(mne)$query(type = type, timepoint = timepoint)) # TO DO knot lookup and filterNA?
            # denormalize to 3NF
            childs <- self$read(mne)$childs[[1L]]
            if(length(childs)==0L) return(self$OBJ(mne)$query())
            childs.knotted <- self$read(childs)[, code[sapply(knot, isTRUE)]]
            childs.historized <- self$read(childs)[, code[sapply(hist, isTRUE)]]
            filterNA <- function(x, cols){
                if(missing(cols) || length(cols)==0) x else x[!(x[, .(`.na` = all(is.na(.SD))), seq_len(nrow(x)), .SDcols = c(cols)]$.na)]
            } # filter out NA on particular columns - used for historized attributes to filter out future static data when querying past
            filterNA(self$joinv(
                master = self$OBJ(mne)$query(),
                join = lapply(childs, function(child){
                    # this will automatically lookup knots to attributes which are knotted
                    if(child %chin% childs.knotted){
                        knot <- self$read(child)$knot
                        knot_data <- quote(self$OBJ(knot)$query())
                        attr_data <- quote(self$OBJ(child)$query(type = type, timepoint = timepoint)[,.SD,,keyby = c(self$OBJ(knot)$keys)])
                        eval(knot_data)[eval(attr_data)
                                        ][,.SD,,keyby = c(self$OBJ(child)$keys)
                                          ] # rename knots to prefix attributes! # TO DO TEST
                    } else {
                        self$OBJ(child)$query(type = type, timepoint = timepoint)
                    }
                }),
                allow.cartesian = (length(childs.historized) > 0L) && isTRUE(type=="difference") # only 'difference' views can explode rows excluding non historized
            ), cols = if(length(childs.historized)) self$read(childs.historized)[, sapply(obj, function(obj) obj$cols[2L])])
        },
        xml = function(file = format(Sys.time(),"AM_%Y%m%d_%H%M%S.xml")){
            if(!self$validate()) stop("AM definition is invalid, see am$validate body for conditions")
            lines <- paste0('<schema format="0.98" date="',format(Sys.Date(),"%Y-%m-%d"),'" time="',format(Sys.time(),"%H:%M:%S"),'">')
            lines <- c(lines, if(nrow(self$read(class="knot")) > 0L) self$read(class="knot")[, sapply(obj, function(obj) obj$xml())])
            matched_attr <- quote(self$read(class="attribute")[sapply(obj, function(obj) obj[["anchor"]])==anchor_code])
            for(anchor_code in self$read(class="anchor")$code){ # for each anchor nest attributes
                if(nrow(eval(matched_attr)) > 0) lines <- c(lines, self$read(code = anchor_code)[, sapply(obj, function(obj) obj$xml(attributes = eval(matched_attr)))]) # batch lookup to all anchors attributes
            }
            lines <- c(lines, unlist(sapply(self$read(class="tie")$obj, function(obj) obj$xml())))
            lines <- c(lines, "</schema>")
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
        },
        stop = function(){
            if(!private$instance_run) return(invisible(self))
            # clean calculated columns
            new.cols <- c("anchor","anchors","parents","childs")
            exist.cols <- new.cols[new.cols %chin% names(self$data)]
            if(length(exist.cols)) self$data[, eval(exist.cols) := NULL]
            private$instance_run <- FALSE
            private$log_list <- c(private$log_list, list(list(event = "stop AM instance", obj = NA_character_, timestamp = Sys.time())))
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
        } # wrapper for faster adding AM objects: am$add$A(mne = "AC", desc = "Actor")
    )
)

