library(data.table)
library(R6)
library(shiny)
library(devtools)
library(testthat)
#library(anchormodeling)

load_all()

# cube dev ----------------------------------------------------------------

am <- AM$new()
am$create(class = "anchor", mne = "PR", desc = "Program")
am$create(class = "anchor", mne = "PE", desc = "Performance")
am$create(class = "tie", anchors = c("PE","PR"), roles = c("at","wasPlayed"), identifier = c(1,Inf))
am$create(class = "anchor", mne = "AC", desc = "Actor")
am$create(class = "tie", anchors = c("AC","PE"), roles = c("wasCasted","in"), identifier = c(Inf,Inf))
am$create(class = "anchor", mne = "ST", desc = "Stage")
am$create(class = "tie", anchors = c("PR","ST"), roles = c("isPlayed","at"), identifier = c(Inf,Inf), hist = TRUE)
am$create(class = "knot", mne = "RAT", desc = "Rating")
am$create(class = "tie", anchors = c("AC","PR"), knot = "RAT", roles = c("part","in","got"), identifier = c(Inf,Inf,1), hist = TRUE)
am$create(class = "knot", mne = "FIP", desc = "FirstPlay")
am$create(class = "tie", anchors = c("AC","ST"), knot = "FIP", roles = c("firstPlayed","at","on"), identifier = c(Inf,Inf,1))
am$run()
am$load(mapping = list(PE = list("perf_code"),
                       PR = list("prog_code"),
                       PE_PR = list()),
        data = data.table(perf_code = 1L, prog_code = c(1L,50L)),
        meta = 1L)
am$load(mapping = list(PE = list("perf_code"),
                       AC = list("acto_code"),
                       AC_PE = list()),
        data = data.table(perf_code = c(1:2,2L), acto_code = c(1L,1:2)),
        meta = 2L)
am$load(mapping = list(PR = list("prog_code"),
                       ST = list("stag_code"),
                       PR_ST = list(hist = "date")),
        data = data.table(prog_code = c(1:2,3L,3L), stag_code = c(1:2,2L,2L), date = as.Date("2015-07-03")+c(0:1,0:1)),
        meta = 3L)
am$load(mapping = list(PR = list("prog_code"),
                       AC = list("acto_code"),
                       AC_PR_RAT = list(hist = "date", knot = "score")),
        data = data.table(prog_code = c(1:2,3L,3L), acto_code = c(1:2,2L,2L), score = c("A","D","E","D"), date = as.Date("2015-07-03")+c(0:1,0:1)),
        meta = 4L)
am$load(mapping = list(ST = list("stag_code"),
                       AC = list("acto_code"),
                       AC_ST_FIP = list(knot = "date")),
        data = data.table(stag_code = c(1:2,2:3), acto_code = c(1L,1:2,2L), date = as.Date("2015-07-03")+c(0:1,0:1)),
        meta = 5L)

am$cube(c("AC_firstPlayed_ST_at_FIP_on","AC","ST","PR_isPlayed_ST_at","PR"))

# status checks -----------------------------------------------------------

if(FALSE){
    am # preview anchor model and its latest status
    am$log # AM instance logs
    am$dashboard() # shiny app: anchor model + 3nf views
    am$etl # etl logs
    am$IM() # identity management instance details
    # am$OBJ("AC_NAM")$data
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
