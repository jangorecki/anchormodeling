library(data.table)
library(R6)
library(devtools)
library(testthat)
#library(anchormodeling)

load_all()

am <- actor.am(3)
am$run()

# status checks -----------------------------------------------------------

if(FALSE){
    am # preview anchor model and its latest status
    am$log # AM instance logs
    am$dashboard() # shiny app: anchor model + 3nf views
    am$etl # etl logs
    am$IM() # identity management instance details
    am$read("ST_NAM")$obj[[1L]]$data
}

# how to define mapping ---------------------------------------------------

# each symbol nested in below list substitutes the character scalar provided by user while defining mapping

# mapping <- list(anchor1_mne = list(natural_key_column_names,
#                                    attr1_mne = c(column_name),
#                                    attr2_mne = c(column_name, "hist" = historize_column_name),
#                                    attr3_mne = c(column_name, "knot" = knot_mne),
#                                    attr3_mne = c(column_name, "hist" = historize_column_name, "knot" = knot_mne)),
#                 anchor2_mne = list(natural_key_column_names,
#                                    attr1_mne = c(column_name),
#                                    attr2_mne = c(column_name, "hist" = historize_column_name),
#                                    attr3_mne = c(column_name, "knot" = knot_mne),
#                                    attr3_mne = c(column_name, "hist" = historize_column_name, "knot" = knot_mne)))

# loading stage -----------------------------------------------------------

mapping <- list(ST = list("address",
                          NAM = c("name", hist="name_date"),
                          LOC = "address"))

# Stage - initial load

data <- actor.data("Stage", iteration = 1L)
data[, name_date := Sys.time()]

am$load(mapping = mapping, data = data, meta = 1L)

# Stage - incremental load (update & new)

data <- rbindlist(list(
    actor.data("Stage", iteration = 2L),
    data.table(name = "new name for new location", address = "new address1") # new
))[, name_date := Sys.time()]

am$load(mapping = mapping, data = data, meta = 2L)

# loading actor -----------------------------------------------------------

mapping = list(AC = list(NAM = "name",
                         GEN = "gender",
                         PLV = "level"),
               GEN = "gender",
               PLV = "level")

# Actor - initial load

data <- actor.data("Actor", iteration = 1L)

am$load(mapping = mapping, data = data, meta = 3L)

# Actor - incremental load

data <- actor.data("Actor", iteration = 2L)
mapping = list(AC = list(NAM = "name",
                         GEN = "gender",
                         PLV = "level"),
               GEN = "gender",
               PLV = "level")

am$load(mapping = mapping, data = data, meta = 4L)
