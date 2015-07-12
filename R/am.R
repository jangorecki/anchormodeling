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
            self$stop()
            to_delete <- code %chin% self$data$code
            if(!all(to_delete)) warning(paste0("Following codes cannot be deleted as they not exists in the model: ",paste(code[!to_delete], collapse=", "),"."))
            code_to_delete <- code[to_delete]
            self$data <- self$data[!code_to_delete]
            setkeyv(self$data, c("code"))[]
            for(code_deleted in code_to_delete) private$log_list <- c(private$log_list, list(list(event = "delete", obj = code_deleted, timestamp = Sys.time())))
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
            # tie related handling - match short names PE_PR to long PE_at_PR_wasPlayed
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
                    stop(paste0("Following short code of tie was not able to map to any tie: ",tie_mapped[is.na(code), paste(short_code,collapse=", ")],". See am print method for defined entities and provide tie code."))
                }
                names(mapping)[names(mapping) %chin% tie_map_short_code] <- tie_mapped[tie_map_short_code, code] # mapping elements renamed
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
            # check attributes - mapping vs model
            # key by hist and knot only for batch attributes match to model, by defined hist, knot, mne, anchor
            model_attrs_lkp <- quote(self$read(unique(unlist(model_all_attr_codes_for_anchors)))[, .(code, knot),, .(class, anchor, mne, hist)])
            # transform mapping to data.table
            mapping_attrs_dt <- rbindlist(lapply(anchors_mne, A.dt, mapping))
            setkeyv(mapping_attrs_dt, c("class","anchor","mne","hist"))
            mapping_attrs_dt[ eval(model_attrs_lkp), `:=`(code = i.code, knot = i.knot)]
            if(!"code" %chin% names(mapping_attrs_dt)){
                mapping_attrs_dt[, `:=`(code = NA_character_, knot = NA_character_)]
            } # workaround for data.table#1166
            if(any(is.na(mapping_attrs_dt$code))){
                stop(paste0("Some of the provided attributes have incorrect definition versus model: ", paste(mapping_attrs_dt[is.na(code), paste(anchor, mne, sep="_")], collapse=", "),". Check if they are not missing `hist` column when defined in model as historized."))
            } # all provided attributes in the mapping exists in model for those anchors, with expected hist and knot
            # checks ties - mapping vs model - global `if` due to data.table#1207
            if(length(ties_code) > 0L){
                model_ties_lkp <- quote(self$read(ties_code)[, .(knot, anchors),, .(code, hist)])
                mapping_ties_dt <- rbindlist(c(
                    list(data.table(code = character(), src_col = character(), hist = logical(), knot = character(), hist_col = character())),
                    lapply(ties_code, T.dt, mapping)
                ))
                setkeyv(mapping_ties_dt, c("code","hist"))
                # bad data vs model: by code and hist
                invalid_ties <- mapping_ties_dt[!eval(model_ties_lkp)]
                if(nrow(invalid_ties) > 0L){
                    stop(paste0("Some of the provided ties have incorrect definition versus model: ", paste(invalid_ties$code, collapse=", "),". Check if they are not missing `hist` column when defined in model as historized."))
                } # all provided ties in the mapping exists in model for those anchors, with expected hist and knot
                mapping_ties_dt[, knot := NULL # remove NA knot, workaround for data.table#1166
                                ][ eval(model_ties_lkp), `:=`(knot = i.knot)]
                if(!"knot" %chin% names(mapping_ties_dt)){
                    mapping_ties_dt[, `:=`(knot = NA_character_)]
                } # workaround for data.table#1166
            } else {
                mapping_ties_dt <- data.table(code = character(), src_col = character(), hist = logical(), knot = character(), hist_col = character())
            }
            # prepare sequence of processing
            load_seq <- rbindlist(list(
                "anchor" = data.table(anchor = NA_character_, class = "anchor", mne = anchors_mne, code = anchors_mne, hist = FALSE, knot = NA_character_, src_col = paste(anchors_mne,"ID",sep="_"), hist_col = NA_character_),
                "knot" = unique(rbindlist(list(
                    "knot of attr" = mapping_attrs_dt[!is.na(knot), .(anchor = NA_character_, class = "knot", mne = knot, code = knot, hist = FALSE, knot = NA_character_, src_col = NA_character_, hist_col = NA_character_), .(byknot = knot)][, unique(.SD), .SDcols=-"byknot"],
                    "knot of tie" = self$read(ties_code)[!is.na(knot), .(anchor = NA_character_, class = "knot", mne = knot, code = knot, hist = FALSE, knot = NA_character_, src_col = NA_character_, hist_col = NA_character_), .(byknot = knot)][, unique(.SD), .SDcols=-"byknot"]
                ))),
                "attr" = mapping_attrs_dt[, .(anchor, class = rep("attribute",length(mne)), mne, code, hist, knot, src_col, hist_col)],
                "tie" = mapping_ties_dt[, .(anchor = rep(NA_character_,length(code)), class = rep("tie",length(code)), mne = rep(NA_character_,length(code)), code, hist, knot, src_col, hist_col)]
            ))
            setkeyv(load_seq, c("class","anchor"))
            set2keyv(load_seq, "code")
            # first pass loop, only check if mapping matches, fill defaults, etc. ?
            if(use.im){
                # build nk mapping
                nk <- lapply(mapping[anchors_mne],`[[`,1L)
                knot_mne <- load_seq["knot", unique(mne), nomatch=0L]
                if(length(knot_mne) > 0L){
                    knot_mapping <- load_seq[knot %chin% knot_mne, list(src_cols = list(c(src_col))), knot]
                    knot_mapping <- setNames(knot_mapping$src_cols, knot_mapping$knot)
                    if(any(sapply(knot_mapping, length0))) stop(paste0("knot not looked up, names: ", paste(names(knot_mapping)[sapply(knot_mapping, length0)], collapse=", ")))
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
                src_cols <- load_seq[knot==knot_code, c(src_col)]
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
                src_cols <- names(mapping)[names(mapping) %chin% self$OBJ(tie_code)$anchors]
                src_cols <- src_cols[order(match(src_cols, self$OBJ(tie_code)$anchors))]
                src_cols <- paste0(src_cols, "_ID")
                src_cols <- c(src_cols, load_seq[code==tie_code, c(if(is.na(knot)) character() else paste_(mapping[[tie_code]][["knot"]], knot,"ID"), if(hist) mapping[[tie_code]][["hist"]] else character())])
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
        joinv = function(master, join, allow.cartesian, time = NULL){
            # simplified jangorecki/dwtools::joinbyv
            stopifnot(!missing(master), !missing(join), is.data.table(master), !is.data.table(join), is.list(join), haskey(master))
            if(length(join)==0L) return(master)
            stopifnot(all(sapply(join, haskey)))
            # avoid copy master in the loop by lookup column and add by reference `:=`
            if(!allow.cartesian){
                master <- copy(master)
                for(i in 1:length(join)){
                    # faster way thanks to: http://stackoverflow.com/questions/30468455/dynamically-build-call-for-lookup-multiple-columns
                    lkp_cols <- names(join[[i]])
                    master[join[[i]], c(lkp_cols) := mget(paste0('i.', lkp_cols))]
                    if(!all(lkp_cols %chin% names(master))){
                        master[, c(lkp_cols) := join[[i]][1L]]
                    } # fix for data.table#1166
                }
            }
            # for non-difference view exec main loop with lookups, also difference views where all anchor attributes are non historized
            if(allow.cartesian){
                nm <- copy(names(master))
                keys <- lapply(join, key)
                temporal_tbl <- sapply(keys, length)==2L
                if(sum(temporal_tbl)==0L){
                    temporal_id <- data.table(mnemonic = character(), id = integer(), inspectedTimepoint = as.Date("2015-07-12")[-1L])
                } else {
                    temporal_id <- unique(rbindlist(lapply(join[temporal_tbl], function(x) x[eval(as.name(key(x)[2L])) %between% time, unique(.SD), .SDcols = c(key(x))]),
                                                    idcol = "mnemonic"))
                }
                setcolorder(temporal_id, c(3L,1L,2L))
                setnames(temporal_id, c("inspectedTimepoint","mnemonic",nm[1L]))
                setkeyv(temporal_id, c(nm[1L],"inspectedTimepoint"))
                # lookup anchor metadata field
                meta_col <- nm[2L]
                master <- temporal_id[master, c(meta_col) := get(paste0("i.",meta_col))]
                if(!meta_col %chin% names(master)) master[, c(meta_col) := NA_integer_] # fix for data.table#116
                id_col <- copy(names(master)[3L])
                setkeyv(master,c(id_col,"inspectedTimepoint"))
                for(i in 1:length(join)){
                    jn.key <- key(join[[i]])
                    if(!"mnemonic" %chin% names(master)) browser()
                    if(length(jn.key)==1L){
                        lkp_cols <- names(join[[i]])
                        master[join[[i]], c(lkp_cols) := mget(paste0('i.', lkp_cols))]
                        if(!all(lkp_cols %chin% names(master))){
                            master[, c(lkp_cols) := join[[i]][1L]]
                        } # fix for data.table#1166
                    } else {
                        master[, c(jn.key[1L]) := get(id_col)][, c(jn.key[2L]) := inspectedTimepoint]
                        join[[i]][, `_hist_tmp` := get(jn.key[2L])
                                  ][, `_id_tmp` := get(jn.key[1L])]
                        setkeyv(master,jn.key)
                        setkeyv(join[[i]],jn.key)
                        # cannot lookup by reference on rolling join: data.table#1217
                        master <- join[[i]][master, roll=+Inf
                                            ][, c(jn.key[2L]) := `_hist_tmp`
                                              ][, `_hist_tmp` := NULL
                                                ][, jn.key[1L] := `_id_tmp`
                                                  ][, `_id_tmp` := NULL]
                        header <- c("inspectedTimepoint","mnemonic",id_col,meta_col)
                        neworder <- c(header,names(master)[!names(master) %chin% header])
                        if(length(neworder)!=length(master)) browser()
                        setcolorder(master, neworder)
                        setkeyv(master,c(id_col,"inspectedTimepoint"))
                    } # historized
                }
            }
            master
        },
        view = function(code, type = "current", time = NULL, selection = NULL, na.rm = FALSE){
            if(type=="now") type <- "current" else if(type=="diff") type <- "difference"
            if(!is.null(selection)) stop("selection argument to difference view is not yet ready")
            if(type=="timepoint" & is.null(time)) stop("Timepoint view must have `time` argument provided.")
            if(type=="difference" & is.null(time)) stop("Difference view must have `time` argument provided as length two vector `c(from, to)`.")
            if(type=="difference" & length(time)!=2L) stop("Difference view must have `time` argument provided as length two vector `c(from, to)`.")
            stopifnot(code %chin% self$read(class=c("anchor","tie"))$code, type %chin% c("latest","timepoint","current","difference"))
            if(self$read(code)$class=="tie"){
                tie_code <- code
                tie_colorder <- self$OBJ(tie_code)$cols[self$OBJ(tie_code)$colorder]
                tie_coltypes <- self$OBJ(tie_code)$coltypes[self$OBJ(tie_code)$colorder]
                tie_data <- quote(self$OBJ(tie_code)$query(type = type, time = time))
                knot_code <- self$read(tie_code)$knot
                if(is.na(knot_code)){
                    coltypes <- tie_coltypes
                    res_data <- eval(tie_data)[, .SD, .SDcols = c(tie_colorder)]
                } else {
                    knot_colorder <- self$OBJ(knot_code)$cols[self$OBJ(knot_code)$colorder]
                    knot_data <- quote(self$OBJ(knot_code)$query())
                    knot_role <- self$OBJ(tie_code)$roles[length(self$OBJ(tie_code)$roles)]
                    non_id_cols <- paste0(knot_role, "_", self$OBJ(knot_code)$cols[-1L])
                    knot_cols <- c(paste0(self$OBJ(knot_code)$cols[1L], "_", knot_role), non_id_cols)
                    knot_key <- knot_cols[1L]
                    colorder <- c(tie_colorder[-length(tie_colorder)], knot_cols[-1L], tie_colorder[length(tie_colorder)])
                    knot_coltypes <- setNames(self$OBJ(knot_code)$coltypes, knot_cols[c(2L,3L,1L)])
                    tie_coltype1 <- self$OBJ(tie_code)$coltypes
                    coltypes <- c(tie_coltype1[-length(tie_coltype1)], knot_coltypes[-length(knot_coltypes)], tie_coltype1[length(tie_coltype1)])
                    res_data <- setnames(eval(knot_data), knot_cols
                                         )[i = eval(tie_data)[,.SD,, keyby = c(knot_key)],
                                           nomatch = NA
                                           ][, .SD,, keyby = c(self$OBJ(tie_code)$keys)
                                             ][, .SD, .SDcols = c(colorder)]
                } # lkp knot
            } else { # anchor
                anchor_code <- code
                anchor_data <- quote(self$OBJ(anchor_code)$query())
                anchor_colorder <- self$OBJ(anchor_code)$cols[self$OBJ(anchor_code)$colorder]
                anchor_coltypes <- self$OBJ(anchor_code)$coltypes[self$OBJ(anchor_code)$colorder]
                childs_code <- self$read(anchor_code)$childs[[1L]]
                if(!is.null(selection) && type=="difference") childs_code <- childs_code[childs_code %chin% selection] # selection argument handlling
                if(length(childs_code)==0L){
                    coltypes <- anchor_coltypes
                    res_data <- eval(anchor_data)
                } else {
                    childs.knotted <- self$read(childs_code)[!is.na(knot), setNames(knot, code)]
                    childs.historized <- self$read(childs_code)[!sapply(hist, is.na), code]
                    attr_data <- quote(self$OBJ(attr_code)$query(type = type, time = time))
                    if(nrow(eval(anchor_data)) == 0L) browser()
                    res_data <- self$joinv(
                        master = eval(anchor_data),
                        join = setNames(lapply(childs_code, function(attr_code){
                            attr_colorder <- self$OBJ(attr_code)$cols[self$OBJ(attr_code)$colorder]
                            # this will automatically lookup knots to attributes which are knotted and prefix with am entity code
                            if(attr_code %chin% names(childs.knotted)){
                                knot_code <- childs.knotted[[attr_code]]
                                knot_cols <- paste0(attr_code, "_", self$OBJ(knot_code)$cols)
                                knot_key <- paste0(attr_code, "_", c(self$OBJ(knot_code)$keys))
                                knot_data <- quote(self$OBJ(knot_code)$query())
                                knot_colorder <- self$OBJ(knot_code)$cols[self$OBJ(knot_code)$colorder]
                                colorder <- c(attr_colorder[-length(attr_colorder)], paste0(attr_code, "_", knot_colorder)[-length(knot_colorder)], attr_colorder[length(attr_colorder)])
                                setnames(eval(knot_data), knot_cols)[i = eval(attr_data)[,.SD,, keyby = c(knot_key)], nomatch=NA
                                                                     ][, .SD,, keyby = c(self$OBJ(attr_code)$keys)
                                                                       ][, .SD, .SDcols = c(colorder)]
                            } else {
                                eval(attr_data)[, .SD, .SDcols = c(attr_colorder)]
                            }
                        }), sapply(strsplit(childs_code, split = "_", fixed = TRUE),`[`,2L)), # auto knot lookup nested here, name the list with mnemonics
                        allow.cartesian = isTRUE(type=="difference"), # 'difference' views can explode rows on multiple historized attributes
                        time = time
                    )
                    attr_coltypes <- unlist(lapply(childs_code, function(attr_code){
                        attr_colorder <- self$OBJ(attr_code)$cols[self$OBJ(attr_code)$colorder]
                        attr_coltypes <- self$OBJ(attr_code)$coltypes
                        if(attr_code %chin% names(childs.knotted)){
                            knot_code <- childs.knotted[[attr_code]]
                            knot_colorder <- self$OBJ(knot_code)$cols[self$OBJ(knot_code)$colorder]
                            knot_coltypes <- setNames(self$OBJ(knot_code)$coltypes, paste0(attr_code, "_", knot_colorder))
                            c(attr_coltypes[-length(attr_coltypes)], knot_coltypes[-length(knot_coltypes)], attr_coltypes[length(attr_coltypes)])
                        } else {
                            attr_coltypes
                        }
                    }))
                    if(isTRUE(type=="difference")){
                        res_data <- res_data[inspectedTimepoint %between% time]
                        diffmeta_coltypes <- setNames(c("hist","meta"),c("inspectedTimepoint","mnemonic"))
                        anchor_coltypes <- c(diffmeta_coltypes, anchor_coltypes)
                    }
                    coltypes <- c(anchor_coltypes, attr_coltypes)
                    if(isTRUE(type=="difference")){
                        setcolorder(res_data, names(coltypes))
                    }
                    if(!identical(names(coltypes), names(res_data))) browser() # check why names not identical
                }
            } # anchor
            setattr(temporal_filter(
                res_data,
                cols = if(na.rm || type == "difference") names(coltypes)[coltypes=="hist"]
                ), "coltypes", coltypes
            )[] # filter only when cols not empty
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
        csv = function(dir = getwd(), nf = 6L){
            if(!self$validate()) stop("AM definition is invalid, see am$validate body for conditions")
            csv.paths <- character()
            if(nf==6L){
                tbls <- self$data[, name,, code]
                for(cd in tbls$code){
                    csv.file <- paste0("AM_csv_6NF_", tbls[cd, name], format(Sys.time(),"_%Y%m%d_%H%M%S.csv"))
                    write.table(x = self$OBJ(cd)$query(),
                                file = file.path(dir, csv.file),
                                append = FALSE,
                                sep = ",",
                                row.names = FALSE,
                                col.names = TRUE)
                    csv.paths <- c(csv.paths, file.path(dir, csv.file))
                }
            } else if(nf==3L){
                tbls <- self$data[class%chin%c("anchor","tie"), name,, code]
                for(cd in tbls$code){
                    csv.file <- paste0("AM_csv_3NF_", tbls[cd, name], format(Sys.time(),"_%Y%m%d_%H%M%S.csv"))
                    write.table(x = self$view(cd),
                                file = file.path(dir, csv.file),
                                append = FALSE,
                                sep = ",",
                                row.names = FALSE,
                                col.names = TRUE)
                    csv.paths <- c(csv.paths, file.path(dir, csv.file))
                }
            } else stop("invalid `nf` argument, accepted 6L and 3L")
            invisible(csv.paths)
        },
        dashboard = function(){
            if(!isTRUE(private$instance_run)) stop("Run DW instance by am$run()")
            suggests_deps <- c("shiny","shinydashboard","DT")
            if(!all(sapply(suggests_deps, requireNamespace, quietly=TRUE))){
                stop(paste0("Dashboard shiny web application requires install required packages: ",paste(suggests_deps[!sapply(suggests_deps, requireNamespace, quietly=TRUE)], collapse=", "),".\nAll available on CRAN by `install.packages(...)`."))
            } else {
                options("am.share" = self)
                shiny::runApp(system.file("app", "dashboard", package = "anchormodeling"))
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
