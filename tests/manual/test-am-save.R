library(anchormodeling)
library(testthat)

am <- AM$new()
am$create(class = "anchor", mne = "AC", desc = "Actor")
am$create(class = "attribute", anchor = "AC", mne = "NAM", desc = "Name")
am$run()

mapping <- list(AC = list("code", NAM = "name"))
data <- data.table(code = "1", name = "Mike")
am$load(mapping, data, 1L)
expect_equal(am$process()$rows, rep(1L,2), info = "Basic load data before save AM")

am$stop()
RData.file <- tempfile(fileext = ".RData")
save(am, mapping, file = RData.file)
rm(am, mapping, data)
expect_equal(ls(), "RData.file", info = "Only path to RData file left in workspace")

load(RData.file)
am$run()

data <- data.table(code = "2", name = "Alice")
am$load(mapping, data, 2L)
expect_equal(am$process()$rows, rep(2L,2), info = "Load data after save AM and load AM")
