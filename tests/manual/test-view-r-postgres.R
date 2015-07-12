
# model R -----------------------------------------------------------------

am <- AM$new()
am$create("anchor", mne = "AC", desc = "Actor")
am$create("attribute", anchor = "AC", mne = "NAM", desc = "Name", hist = TRUE)
am$create("attribute", anchor = "AC", mne = "GEN", desc = "Gender", knot = "GEN")
am$create("knot", mne = "GEN", desc = "Gender")
am$create("attribute", anchor = "AC", mne = "PLV", desc = "ProfessionalLevel", knot = "PLV", hist = TRUE)
am$create("knot", mne = "PLV", desc = "ProfessionalLevel")$run()

# model postgres ----------------------------------------------------------

am$xml()
browseURL(anchor_modeler_url())
# load model to anchor modeler in SQLserver mode
# convert loaded model to PostgreSQL
# generate SQL script
# replace varchar(max) -> varchar

# data --------------------------------------------------------------------

actor_data <- data.table(code = c("1", "2", "3", "2", "1", "2"),
                         name = c("Mike", "Bob", "Alice", "Boby", "Mikey","Boby"),
                         gender = c("M", "M", "F", "M", "M", "M"),
                         level = c(4L, 1L, 3L, 1L, 5L, 2L),
                         name_date = c(as.Date("2015-07-05")+c(0:1,3L,3L,2L,3L)),
                         level_date = c(as.Date("2015-07-05")+c(1:2,4,2L,2L,5L)))

# load postgres -----------------------------------------------------------

# insert knots
actor_data[,.(GEN_ID=seq_along(unique(gender)), GEN_Gender=unique(gender), Metadata_GEN=1L)
           ][,cat(insert.postgres(.SD, "dbo._gen_gender"),sep="\n")]
actor_data[,.(PLV_ID=seq_along(unique(level)), PLV_ProfessionalLevel=unique(level), Metadata_PLV=1L)
           ][,cat(insert.postgres(.SD, "dbo._plv_professionallevel"),sep="\n")]
# insert anchor and attributes
actor_data[,.SD
           ][, AC_ID := .GRP, code
             ][, .(AC_ID, Metadata_AC = 1L,
                   AC_NAM_Actor_Name = name, Metadata_AC_NAM = 1L, AC_NAM_ChangedAt = name_date,
                   AC_GEN_GEN_Gender = gender, Metadata_AC_GEN = 1L, AC_GEN_Metadata_GEN = 1L,
                   AC_PLV_PLV_ProfessionalLevel = level, AC_PLV_Metadata_PLV = 1L, Metadata_AC_PLV = 1L, AC_PLV_ChangedAt = level_date)
               ][,cat(insert.postgres(.SD, "dbo.lAC_Actor"),sep="\n")]

# load R ------------------------------------------------------------------

am$load(mapping = list(AC = list("code",
                                 NAM = c("name", hist = "name_date"),
                                 GEN = "gender",
                                 PLV = c("level", hist = "level_date"))),
        data = actor_data,
        meta = 1L)

# view --------------------------------------------------------------------

library(RPostgreSQL)
DBIconn <- dbConnect(PostgreSQL(),
                     host="192.168.56.101",
                     port="5432",
                     dbname="actor",
                     user="jan",
                     password="userpassr")
getTbl <- function(tbl){
    dt <- setDT(dbGetQuery(DBIconn, paste0("select * from dbo.",tbl,";")))
    to_date <- c("ac_nam_changedat","ac_plv_changedat")
    if("inspectedtimepoint" %chin% names(dt)) to_date <- c("inspectedtimepoint",to_date)
    dt[, c(to_date) := lapply(.SD, function(x) as.Date(as.character(x))), .SDcols=c(to_date)
       ][, c("ac_plv_plv_professionallevel") := as.integer(ac_plv_plv_professionallevel)
         ][]
}
checkTbl  <- function(amdt, pgdt){
    amdt <- setkeyv(setnames(amdt,names(amdt),tolower(names(amdt))),tolower(names(amdt)))
    pgdt <- setkeyv(pgdt, names(pgdt))
    stopifnot(nrow(amdt)==nrow(pgdt))
    stopifnot(identical(names(amdt), names(pgdt)))
    all.equal(amdt, pgdt, check.attributes=FALSE)
}
# latest view
checkTbl(amdt = am$view("AC", type="latest"), pgdt = getTbl("lAC_actor"))
# current view
checkTbl(amdt = am$view("AC", type="current"), pgdt = getTbl("nAC_actor"))
# point-in-time view
checkTbl(amdt = am$view("AC", type="timepoint", time=as.Date('2015-07-08')), pgdt = getTbl("pAC_actor('2015-07-08')"))
checkTbl(amdt = am$view("AC", type="timepoint", time=as.Date('2015-07-03')), pgdt = getTbl("pAC_actor('2015-07-03')"))
checkTbl(amdt = am$view("AC", type="timepoint", time=as.Date('2015-06-05')), pgdt = getTbl("pAC_actor('2015-06-05')"))
checkTbl(amdt = am$view("AC", type="timepoint", time=as.Date('2015-12-01')), pgdt = getTbl("pAC_actor('2015-12-01')"))
# difference view
checkTbl(amdt = am$view("AC", type="difference", time=as.Date(c('2015-07-09','2015-07-10'))), pgdt = getTbl("dAC_actor('2015-07-09','2015-07-10')"))
checkTbl(amdt = am$view("AC", type="difference", time=as.Date(c('2015-07-04','2015-07-07'))), pgdt = getTbl("dAC_actor('2015-07-04','2015-07-07')"))
# checkTbl(amdt = am$view("AC", type="difference", time=as.Date(c('2015-06-05','2015-06-15'))), pgdt = getTbl("dAC_actor('2015-06-05','2015-06-15')")) RPostgreSQL not support 0 rows result here - test checked using psql
checkTbl(amdt = am$view("AC", type="difference", time=as.Date(c('2015-06-05','2015-07-08'))), pgdt = getTbl("dAC_actor('2015-06-05','2015-07-08')"))

# test-am-view - difference views
checkTbl(amdt = am$view("AC", type = "difference", time = c(as.Date("2015-07-03"), as.Date("2015-07-07"))), pgdt = getTbl("dAC_actor('2015-07-03','2015-07-07')"))
checkTbl(amdt = am$view("AC", type = "difference", time = c(as.Date("2015-07-06"), as.Date("2015-07-08"))), pgdt = getTbl("dAC_actor('2015-07-06','2015-07-08')"))
checkTbl(amdt = am$view("AC", type = "difference", time = c(as.Date("2015-07-06"), as.Date("2015-07-10"))), pgdt = getTbl("dAC_actor('2015-07-06','2015-07-10')"))
