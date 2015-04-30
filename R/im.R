#' @title Identity Management instance
#' @description Keep track on natural keys and generate surrogate keys. It is not part of Anchor Model but complementary identities provider.
#' @export
#' @examples
#' # new IM instance
#' im <- IM$new()
#' # define mnemonics and natural keys
#' im$create(mne = "GEN", nk = "gender")
#' im$create(mne = "UL", nk = "user_login")
#' # populate data
#' data <- data.table(user_login = c("asd","qwe","zxc","asd","zxc","asd"), gender = c("F","M","M","F","M","F"), balance = rnorm(6,10))
#' data
#' # update and lookup identities from IM
#' data.sk <- im$use(data, mne = c("UL","GEN"))
#' data.sk
#' # some incremental load
#' data <- data.table(user_login = c("zxc","qaz","qaz","zaq"), gender = c("M","F","F","M"), balance = rnorm(4,10))
#' data.sk <- im$use(data, mne = c("UL","GEN"))
#' data.sk
#' # preview identities
#' im$ID
#' # get IM size
#' im$size()
IM <- R6Class(
  classname = "IM",
  public = list(
    ID = list(),
    NK = list(),
    initialize = function() invisible(self),
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
    use = function(data, mne){
      # check inputs
      stopifnot(is.data.table(data), nrow(data) > 0L, is.character(mne), length(mne) > 0L)
      # check if no mne_ID cols in the data which would be overwritten
      stopifnot(all(!paste(mne,"ID",sep="_") %chin% names(data)))
      # iterate over mne to update IM with new ID
      mnes <- mne
      for(mne in mnes){
        init_ID <- length(self$ID[[mne]])==0L # check if first time used
        if(!init_ID){
          # check nk names
          stopifnot(all(self$NK[[mne]] %chin% names(data)))
          # check nk data types
          stopifnot(identical(sapply(self$ID[[mne]][0L, self$NK[[mne]], with=FALSE], typeof),sapply(data[0L, self$NK[[mne]], with=FALSE], typeof)))
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
      data <- copy(data)
      for(mne in mnes){
        setkeyv(data, self$NK[[mne]])
        data[self$ID[[mne]], c(paste(mne,"ID",sep="_")) := eval(as.name(paste0("i.",paste(mne,"ID",sep="_"))))]
      }
      setkeyv(data, NULL)[]
    }
  )
)
