
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

actor_data <- data.table(
    AC_ID = 1:4,
    Metadata_AC = rep(1L,4),
    #AC_NAM_AC_ID = 1:4,
    AC_NAM_Actor_Name = c("Mike", "Bob", "Alice", "Lee"),
    Metadata_AC_NAM = rep(1L,4),
    AC_NAM_ChangedAt = c(as.Date("2015-07-05")+c(0:1,3:4)),
    #AC_GEN_AC_ID = 1:4,
    Metadata_AC_GEN = rep(1L,4),
    AC_GEN_GEN_Gender = c("M", "M", "F", "M"),
    AC_GEN_Metadata_GEN = rep(1L,4),
    AC_GEN_GEN_ID = c(1L,1L,2L,1L),
    #AC_PLV_AC_ID = 1:4,
    Metadata_AC_PLV = rep(1L,4),
    AC_PLV_ChangedAt = c(as.Date("2015-07-05")+c(1:2,4:5)),
    AC_PLV_PLV_ProfessionalLevel = c(4L, 1L, 3L, 4L),
    AC_PLV_Metadata_PLV = rep(1L,4),
    AC_PLV_PLV_ID = c(1L,2L,3L,1L)
)

# load postgres -----------------------------------------------------------

# insert knots
actor_data[,.(GEN_ID=AC_GEN_GEN_ID, GEN_Gender=AC_GEN_GEN_Gender, Metadata_GEN=AC_GEN_Metadata_GEN)
           ][,unique(.SD)
             ][,cat(insert.postgres(.SD, "dbo._gen_gender"),sep="\n")]
actor_data[,.(PLV_ID=AC_PLV_PLV_ID, PLV_ProfessionalLevel=AC_PLV_PLV_ProfessionalLevel, Metadata_PLV=AC_PLV_Metadata_PLV)
           ][,unique(.SD)
             ][,cat(insert.postgres(.SD, "dbo._plv_professionallevel"),sep="\n")]
# insert attributes
actor_data[,.SD,.SDcols = c("AC_ID", "Metadata_AC",
                            "AC_NAM_Actor_Name", "Metadata_AC_NAM", "AC_NAM_ChangedAt",
                            "AC_GEN_GEN_Gender", "Metadata_AC_GEN", "AC_GEN_Metadata_GEN",
                            "AC_PLV_PLV_ProfessionalLevel", "AC_PLV_Metadata_PLV", "Metadata_AC_PLV", "AC_PLV_ChangedAt")
           ][, cat(insert.postgres(.SD, "dbo.lAC_Actor"),sep="\n")]
"
INSERT INTO dbo.lAC_Actor (AC_ID, Metadata_AC, AC_NAM_Actor_Name, Metadata_AC_NAM, AC_NAM_ChangedAt, AC_GEN_GEN_Gender, Metadata_AC_GEN, AC_GEN_Metadata_GEN, AC_PLV_PLV_ProfessionalLevel, AC_PLV_Metadata_PLV, Metadata_AC_PLV, AC_PLV_ChangedAt) VALUES (1, 1, 'Mike', 1, '2015-07-05'::date, 'M', 1, 1, 4, 1, 1, '2015-07-06'::date);
INSERT INTO dbo.lAC_Actor (AC_ID, Metadata_AC, AC_NAM_Actor_Name, Metadata_AC_NAM, AC_NAM_ChangedAt, AC_GEN_GEN_Gender, Metadata_AC_GEN, AC_GEN_Metadata_GEN, AC_PLV_PLV_ProfessionalLevel, AC_PLV_Metadata_PLV, Metadata_AC_PLV, AC_PLV_ChangedAt) VALUES (2, 1, 'Bob', 1, '2015-07-06'::date, 'M', 1, 1, 1, 1, 1, '2015-07-07'::date);
INSERT INTO dbo.lAC_Actor (AC_ID, Metadata_AC, AC_NAM_Actor_Name, Metadata_AC_NAM, AC_NAM_ChangedAt, AC_GEN_GEN_Gender, Metadata_AC_GEN, AC_GEN_Metadata_GEN, AC_PLV_PLV_ProfessionalLevel, AC_PLV_Metadata_PLV, Metadata_AC_PLV, AC_PLV_ChangedAt) VALUES (3, 1, 'Alice', 1, '2015-07-08'::date, 'F', 1, 1, 3, 1, 1, '2015-07-09'::date);
INSERT INTO dbo.lAC_Actor (AC_ID, Metadata_AC, AC_NAM_Actor_Name, Metadata_AC_NAM, AC_NAM_ChangedAt, AC_GEN_GEN_Gender, Metadata_AC_GEN, AC_GEN_Metadata_GEN, AC_PLV_PLV_ProfessionalLevel, AC_PLV_Metadata_PLV, Metadata_AC_PLV, AC_PLV_ChangedAt) VALUES (4, 1, 'Lee', 1, '2015-07-09'::date, 'M', 1, 1, 4, 1, 1, '2015-07-10'::date);
"

# load R ------------------------------------------------------------------

am$load(mapping = list(AC = list("AC_ID2",
                                 NAM = c("AC_NAM_Actor_Name", hist = "AC_NAM_ChangedAt"),
                                 GEN = "AC_GEN_GEN_Gender",
                                 PLV = c("AC_PLV_PLV_ProfessionalLevel", hist = "AC_PLV_ChangedAt"))),
        data = actor_data[,.(AC_ID2=AC_ID,AC_NAM_Actor_Name,AC_NAM_ChangedAt,AC_GEN_GEN_Gender,AC_PLV_PLV_ProfessionalLevel,AC_PLV_ChangedAt)],
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

# difference view

amdt <- am$view("AC", type="difference", time=as.Date(c('2015-07-09','2015-07-10')))
pgdt <- getTbl("dAC_actor('2015-07-09','2015-07-10')")
checkTbl(amdt = amdt, pgdt = pgdt)

am$view("AC", type="difference", time=as.Date(c('2015-07-05','2015-07-18')))
getTbl("dAC_actor('2015-07-05','2015-07-08')")
setDT(dbGetQuery(DBIconn, paste0("select * from dbo.dAC_actor('2015-07-05','2015-07-08');")))[]

