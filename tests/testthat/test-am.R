context("AM anchormodeling")

test_that("AM basic construct", {

    am <- AM$new()
    expect_true(nrow(am$log)==2L, info="AM logging: new")
    am$create(class = "anchor", mne = "AC", desc = "Actor")
    am$create(class = "attribute", anchor = "AC", mne = "GEN", desc = "Gender")
    expect_true(nrow(am$log)==4L, info="AM logging: add")
    expect_true(all(sapply(am$data$obj, is.am)), info="AM all obj are MAobj")
    am$run()
    expect_true(nrow(am$log)==5L, info="AM logging: run")
    am$stop()
    expect_true(nrow(am$log)==6L, info="AM logging: stop")

})

test_that("AM load-data stop-im save-im rm-im load-im load-data", {

    am <- AM$new()
    am$create(class = "anchor", mne = "AC", desc = "Actor")
    am$create(class = "attribute", anchor = "AC", mne = "NAM", desc = "Name")
    am$run()
    am$load(mapping = list(AC = list("code", NAM = "name")),
            data = data.table(code = "1", name = "Mike"),
            meta = 1L)
    expect_equal(am$OBJ("AC_NAM")$data, data.table(AC_NAM_AC_ID = 1L, AC_NAM_Actor_Name = "Mike", Metadata_AC_NAM = 1L, key = "AC_NAM_AC_ID"), info = "Basic load data before save AM")
    am$stop()
    RData.file <- tempfile(fileext = ".RData")
    save(am, file = RData.file)
    rm(am)
    expect_equal(ls(), "RData.file", info = "Only path to RData file left in workspace")
    load(RData.file)
    am$run()
    am$load(mapping = list(AC = list("code", NAM = "name")),
            data = data.table(code = "2", name = "Alice"),
            meta = 2L)
    expect_equal(am$OBJ("AC_NAM")$data, data.table(AC_NAM_AC_ID = 1:2, AC_NAM_Actor_Name = c("Mike","Alice"), Metadata_AC_NAM = 1:2, key = "AC_NAM_AC_ID"), info = "Load data after save AM and load AM")

})
