#' @title Identity Management instance
#' @docType class
#' @format An R6 class object.
#' @name IM
#' @description Keep track on natural keys and generate surrogate keys. It is not part of Anchor Model but complementary identities provider.
#' @export
#' @examples
#' # new IM instance
#' im <- IM$new()
#' # define mnemonics and natural keys
#' im$create(mne = "GEN", nk = "gender")
#' im$create(mne = "UL", nk = "user_login")
#' # populate data
#' data <- data.table(user_login = c("asd","qwe","zxc","asd","zxc","asd"),
#'                    gender = c("F","M","M","F","M","F"),
#'                    balance = rnorm(6,10))
#' data
#' # update and lookup identities from IM
#' data.sk <- im$use(data, mne = c("UL","GEN"))
#' data.sk
#' # incremental load
#' data <- data.table(user_login = c("zxc","qaz","qaz","zaq"),
#'                    gender = c("M","F","F","M"),
#'                    balance = rnorm(4,10))
#' data.sk <- im$use(data, mne = c("UL","GEN"))
#' data.sk
#' # preview identities
#' im$ID
#' # get IM size
#' print(im)
IM <- R6Class(
    classname = "IM",
    public = list(
        ID = list(),
        NK = list(),
        initialize = function() invisible(self),
        print = function(){
            cat("<IM>\n",sep="")
            cat("  Identities:\n",sep="")
            for(mne in names(self$ID)) cat("    ",mne,": ",paste(self$NK[[mne]], collapse=", ")," (",nrow(self$ID[[mne]])," rows)\n",sep="")
            cat("  Size: ",format(self$size(), units = "auto"),"\n",sep="")
            invisible(self)
        },
        create = function(mne, nk){
            self$NK[[mne]] <- nk
            self$ID[[mne]] <- data.table()
            invisible(self)
        },
        delete = function(mne){
            self$NK[mne] <- NULL
            self$ID[mne] <- NULL
            invisible(self)
        },
        size = function(mne){
            if(missing(mne)) object.size(self$ID) else object.size(self$ID[[mne]])
        },
        use = function(data, mne, nk, in.place = FALSE){
            # check inputs
            stopifnot(is.data.table(data), nrow(data) > 0L, is.character(mne), length(mne) > 0L)
            # check if no mne_ID cols in the data which would be overwritten
            stopifnot(all(!paste(mne,"ID",sep="_") %chin% names(data)))
            # iterate over mne to update IM with new ID
            mnes <- mne
            for(mne in mnes){
                # dynamically initilize new ID management
                if(!mne %chin% names(self$NK)){
                    if(missing(nk)) stop("Each new anchor/knot to be handled by auto identity management should have natural keys provided as nk arg, the list of natural key cols named by mne.")
                    if(!mne %chin% names(nk)) stop(paste0("Each new anchor/knot to be handled by auto identity management should have natural keys provided as nk arg, the list of natural key cols named by mne. Missing natural keys for: ",mne))
                    self$create(mne = mne, nk = nk[[mne]])
                }
                if(!missing(nk)){
                    if(!all(nk[[mne]] %chin% names(data))){
                        stop(paste0("Columns provided to nk doesn't exists in data, related to mne: ",mne))
                    }
                    # dynamic rename incoming data
                    if(!identical(self$NK[[mne]], nk[[mne]])){
                        if(length(self$NK[[mne]]) != length(nk[[mne]])){
                            msg <- paste0("Incorrect number of columns provided as natural keys for ",mne,". Defined in model: ", paste(self$NK[[mne]], collapse=", "),". Provided in nk: ", paste(nk[[mne]], collapse=", "),".")
                            stop(msg)
                        }
                        rn_i <- which(self$NK[[mne]] != nk[[mne]])
                        data[, c(self$NK[[mne]][rn_i]) := mget(nk[[mne]][rn_i])] # copy column to simulate previous name of column
                    }
                }
                init_ID <- length(self$ID[[mne]])==0L # check if first time used
                if(!init_ID){
                    # check nk col names
                    if(!all(self$NK[[mne]] %chin% names(data))){
                        stop(paste0("Defined natural key for mnemonic ", mne, " do not exists in incoming data. Expected names: " ,paste(self$NK[[mne]], collapse=", "),". To use different column provide `nk` argument, or update data in your IM instance."))
                    }
                    # check nk data types
                    if(!identical(sapply(self$ID[[mne]][0L, self$NK[[mne]], with=FALSE], class1),sapply(data[0L, self$NK[[mne]], with=FALSE], class1))){
                        stop(paste0("Provided natural key columns are different type than the one stored in Identity Management. Related to mnemonic: ",mne))
                    }
                    # keep only new ID
                    new_nk.data <- self$ID[[mne]][data[, unique(.SD), .SDcols=self$NK[[mne]]]][is.na(eval(as.name(paste(mne,"ID",sep="_"))))]
                } else {
                    # use all NK as new ID in init use
                    new_nk.data <- copy(data[, unique(.SD), .SDcols=self$NK[[mne]]])[, c(paste(mne,"ID",sep="_")) := NA_integer_]
                } # find new IDs
                if(nrow(new_nk.data) > 0L){
                    last_id <- if(init_ID) 0L else max(self$ID[[mne]][[paste(mne,"ID",sep="_")]],0L)
                    new_nk.data[, c(paste(mne,"ID",sep="_")) := seq.int(from = last_id+1L, to = last_id+.N, by = 1L)]
                    self$ID[[mne]] <- setkeyv(rbindlist(list(self$ID[[mne]], new_nk.data)), self$NK[[mne]])
                } # add new ID
            }
            # iterate over mne to lookup ID from IM
            if(!in.place) data <- copy(data)
            for(mne in mnes){
                setkeyv(data, self$NK[[mne]])
                data[self$ID[[mne]], c(paste(mne,"ID",sep="_")) := eval(as.name(paste0("i.",paste(mne,"ID",sep="_"))))]
            }
            setkeyv(data, NULL)[]
        }
    )
)
