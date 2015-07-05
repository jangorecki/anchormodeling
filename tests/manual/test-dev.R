library(data.table)
library(R6)
library(shiny)
library(devtools)
library(testthat)
#library(anchormodeling)

load_all()

options("am.restatability"=TRUE)
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

# dev datasets ------------------------------------------------------------

mapping.list <- list()

actor_performances <- fread("event;program;performance;actor;date
ev1;pr1;perf1;ac1;2015-04-14
ev1;pr1;perf1;ac2;2015-04-14
ev2;pr1;perf2;ac1;2015-04-16
ev2;pr1;perf2;ac3;2015-04-16
ev3;pr2;perf3;ac3;2015-04-14
ev4;pr2;perf4;ac3;2015-04-15
ev5;pr2;perf5;ac3;2015-04-16
ev6;pr2;perf6;ac2;2015-04-17
ev7;pr3;perf7;ac1;2015-04-15
ev7;pr3;perf7;ac3;2015-04-15
ev7;pr3;perf7;ac4;2015-04-15
ev7;pr3;perf7;ac5;2015-04-15
ev8;pr3;perf8;ac1;2015-04-19
ev8;pr3;perf8;ac3;2015-04-19
ev8;pr3;perf8;ac4;2015-04-19")

mapping.list[["AC_PE"]] <- list(EV = list("event"),
                                PR = list("program"),
                                PE = list("performance",
                                          DAT = "date"),
                                AC = list("actor"))

am$load(mapping = mapping.list[["AC_PE"]], actor_performances, meta = 1L)

actor_rating <- fread("actor;program;date;rating
ac1;pr1;2015-04-14;A
ac1;pr1;2015-04-16;C
ac1;pr3;2015-04-15;D
ac1;pr3;2015-04-19;D
ac4;pr3;2015-04-19;A
ac2;pr2;2015-04-17;B
ac2;pr1;2015-04-14;E")

mapping.list[["AC_PR_RAT"]] <- list(PR = list("program"),
                                    AC = list("actor"),
                                    AC_wasCast_PR_in = list(c("program","actor"),
                                                            RAT = c("rating", hist = "date")))

performance_details <- fread("address;performance;audience;revenue
loc1;perf1;25;80
loc1;perf2;18;43
loc2;perf3;5;67
loc3;perf4;10;45
loc1;perf5;70;200
loc1;perf6;55;110
loc2;perf7;125;180
loc2;perf8;105;95")

stage_details <- fread("address;name;date
loc1;st1;2015-01-01
loc2;st2;2015-01-01
loc2;st4;2015-03-01
loc3;st3;2015-02-01")

# built-in datasets -------------------------------------------------------

# loading stage -----------------------------------------------------------

mapping <- list(ST = list("address",
                          NAM = c("name", hist="name_date"),
                          LOC = "address"))

# Stage - initial load

data <- actor.data("Stage", iteration = 1L)
data[, name_date := Sys.time()]

am$load(mapping = mapping, data = data, meta = list(meta=4L, src="some"))

# Stage - incremental load (update & new)

dupa <- data <- rbindlist(list(
    actor.data("Stage", iteration = 2L),
    actor.data("Stage", iteration = 2L)[1:2], # rest
    data.table(name = "new name for new location1", address = "new address1"), # new
    data.table(name = "new name for new location2", address = "new address2"), # new
    data.table(name = "new name for new location2", address = "new address2") # new rest
))[, name_date := Sys.time()]

am$load(mapping = mapping, data = dupa, meta = 2L)

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
