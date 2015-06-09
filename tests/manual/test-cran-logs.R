# Here's an easy way to get all the URLs in R
start <- as.Date('2013-11-17')
today <- as.Date('2014-01-05')

# If you only want to download the files you don't have, try:
missing_days <- setdiff(all_days, tools::file_path_sans_ext(dir(), TRUE))
# all_days <- seq(start, today, by = 'day')

year <- as.POSIXlt(all_days)$year + 1900
urls <- paste0('http://cran-logs.rstudio.com/', year, '/', all_days, '.csv.gz')
# You can then use download.file to download into a directory.
for(i in seq_along(urls)) download.file(urls[i], destfile = paste(all_days[i], ".csv.gz", sep="."))

csv.files <- paste0(all_days,".csv")
# untar

inittime <- proc.time()
library(data.table)
l <- lapply(setNames(csv.files,csv.files), fread)

dt <- rbindlist(l, idcol = "filename")
dt

library(XML)
R.dim <- function(){
    url <- paste0("http://cran.r-project.org/src/base/R-", 0:3)
    x <- lapply(url, function(x)readHTMLTable(x, stringsAsFactors=FALSE)[[1L]])
    rbindlist(x)[grep("R-(.*)(\\.tar\\.gz|\\.tgz)", Name), c(-1L, -5L), with=FALSE
                 ][, release := gsub("(R-.*)\\.(tar\\.gz|tgz)", "\\1", Name)
                   ][, version := substr(release,3,nchar(release))
                     ][, date := as.POSIXct(`Last modified`, format="%d-%b-%Y %H:%M")
                       ][, `:=`(units = substr(Size, nchar(Size), nchar(Size)), size = as.numeric(substr(Size, 1L, nchar(Size)-1L)))
                         ][, .(release, r_source = Name, date, print_size = Size, size, units),, .(version)]
}
rdim <- R.dim()

am <- AM$new()
am$add$A(mne = "PA", desc = "Package")
am$add$a(anchor = "PA", mne = "VER", desc = "Version")

am$add$A("RR", desc = "Rrelease")
am$add$a(anchor = "RR", mne = "DAT", desc = "Date")

am$add$A(mne = "IP", desc = "Ip") # composite natural key on date and ip_id
am$add$a(anchor = "IP", mne = "COU", desc = "Country")

am$add$A(mne = "SD", desc = "Sourcedata")

am$add$tie(anchors = c("PA","RR","IP","SD"), roles = c("whatPackage","fromRversion","byWhom","fromFile"))
am$run()
map <- list(PA = list("package",
                      VER = "version"),
            RR = list("version",
                      DAT = "date"),
            IP = list(c("date","ip_id"),
                      COU = "country"),
            SD = list("filename"),
            PA_whatPackage_RR_fromRversion_IP_byWhom_SD_fromFile = list())
system.time(
    am$load(map, dt, 1L)
)

am$view("PA")

am$OBJ("PA_whatPackage_RR_fromRversion_IP_byWhom_SD_fromFile")$query()

# this is nice

# answering question

# How many `IP` identities changed their R version within the single day

message("inptime taken in seconds: ",round(proc.time()[[3L]] - inittime[[3L]], 2L))

# Model extension

# It is possible to map `country` to timezones and lookup local time offset
