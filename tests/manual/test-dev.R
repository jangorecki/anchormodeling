library(data.table)
library(R6)
library(devtools)
library(testthat)
#library(anchormodeling)

load_all()

options("am.restatability"=FALSE)
am <- actor.am(3)
am$run()
options("am.parallel"=FALSE)

# status checks -----------------------------------------------------------

if(FALSE){
    am # preview anchor model and its latest status
    am$log # AM instance logs
    am$dashboard() # shiny app: anchor model + 3nf views
    am$etl # etl logs
    am$IM() # identity management instance details
    am$read("ST_NAM")$obj[[1L]]$data
}

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
    actor.data("Stage", iteration = 2L)[1:2], # rest
    data.table(name = "new name for new location1", address = "new address1"), # new
    data.table(name = "new name for new location2", address = "new address2"), # new
    data.table(name = "new name for new location2", address = "new address2") # new rest
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
