context("AM load method")

test_that("AM load method valid processing scenarios", {

    am <- AM$new()
    am$create(class = "anchor", mne = "PR", desc = "Program")
    am$create(class = "attribute", anchor = "PR", mne = "NAM", desc = "Name")
    am$run()

    # initial loading data, auto IM
    am$load(mapping = list(PR = list("code", NAM = "name")),
            data = data.table(code = 1L, name = "my program"),
            meta = 1L)
    expect_identical(am$process()$rows, rep(1L,2L), info = "loaded first row")
    expect_equal(am$IM()$ID$PR, data.table(code = 1L, PR_ID = 1L, key = "code"), info = "auto IM after first insert")
    # incremental loading new data, auto IM
    am$load(mapping = list(PR = list("code", NAM = "name")),
            data = data.table(code = 50L, name = "my another program"),
            meta = 2L)
    expect_identical(am$process()$rows, rep(2L,2L), info = "loaded second row")
    expect_equal(am$IM()$ID$PR, data.table(code = c(1L,50L), PR_ID = 1:2, key = "code"), info = "auto IM after second insert")
    # incremental loading existing data
    am$load(mapping = list(PR = list("code", NAM = "name")),
            data = data.table(code = 50L, name = "my another program"),
            meta = 3L)
    expect_identical(am$process()$rows, rep(2L,2L), info = "loaded second row twice")
    expect_equal(am$IM()$ID$PR, data.table(code = c(1L,50L), PR_ID = 1:2, key = "code"), info = "auto IM after second insert twice")
    # incremental loading update static attribute
    am$load(mapping = list(PR = list("code", NAM = "name")),
            data = data.table(code = 50L, name = "my another program new"),
            meta = 4L)
    # expect_identical(am$process()$rows, rep(2L,2L), info = "static attribute update, new row not inserted  with update not inserts")

    # expect_true("my another program new" %in% am$data[code == "PR_NAM", obj[[1L]]]$data$PR_NAM_Program_Name, info = "static attribute update")

    # expect_equal(am$IM()$ID$PR, data.table(code = c(1L,50L), PR_ID = 1:2, key = "code"), info = "auto IM after first bad insert")

    # metadata


    #     am$create(class = "tie", anchors = c("AC","PR"), roles = c("part","in"), identifier = c(Inf,Inf,1), knot = "RAT")
    #     am$create(class = "anchor", mne = "AC", desc = "Actor")
    #
    #
    #
    #     am$create(class = "attribute", anchor = "AC", mne = "NAM", desc = "Name", hist = TRUE)
    #
    #
    #     am$create(class = "attribute", anchor = "AC", mne = "GEN", desc = "Gender", knot = "GEN")
    #     am$create(class = "knot", mne = "GEN", desc = "Gender")
    #     am$run()

    # mapping
    #     mapping <- list(AC = list("code",
    #                               NAM = c("name", hist = "date"),
    #                               GEN = "gender"))
    #
    #     # using IM
    #     r <- list()
    #     r[[1L]] <- list(code = "1", name = "Mike", gender = "M", date = Sys.Date()-5L)
    #     r[[2L]] <- list("2", "Bob", "M", date = Sys.Date()-5L)
    #     data <- rbindlist(r)
    #     am$load(mapping, data, 1L)
    # check loaded
    # expect_equal(am$process()[rows >= 2L, code], c("AC_NAM","AC_GEN","AC","GEN"), info = "expected am$process logs loaded")
    # TO DO - LOAD KNOTS
    # expect_equal(am$process()[rows >= 2L, sapply(obj, function(obj) nrow(obj$data)==2L)], rep(TRUE, 3), info = "all of the loaded tables has nrow==2L")
    # load update
    # data <- data.table(code = "1", name = "Mikey", gender = "M", date = Sys.Date()-4L)
    # am$load(mapping, data, 2L)
    #     am$create(class = "anchor", mne = "PE", desc = "Performance")
    #     am$create(class = "tie", anchors = c("AC","PE"), roles = c("wasCasted","in"), identifier = c(Inf,Inf))
    #     am$create(class = "knot", mne = "RAT", desc = "Rating")

})

test_that("multiple AM instances loading including separation of IM instances", {

    am1 <- AM$new()
    am1$create(class = "anchor", mne = "PE", desc = "Person")
    am1$create(class = "attribute", anchor = "PE", mne = "NAM", desc = "Name")
    am1$run()

    am2 <- AM$new()
    am2$create(class = "anchor", mne = "PE", desc = "Performance")
    am2$create(class = "attribute", anchor = "PE", mne = "NAM", desc = "Name")
    am2$run()

    am1$load(mapping = list(PE = list("code", NAM = "name")),
             data = data.table(code = 1L, name = "my person"),
             meta = 1L)
    am2$load(mapping = list(PE = list("code", NAM = "name")),
             data = data.table(code = 5L, name = "my program"),
             meta = 1L)
    expect_identical(am1$process()$rows, rep(1L,2L), info = "multiple AM instances insert 1 row, am1")
    expect_identical(am2$process()$rows, rep(1L,2L), info = "multiple AM instances insert 1 row, am2")
    expect_equal(am1$IM()$ID$PE, data.table(code = 1L, PE_ID = 1, key = "code"), info = "multiple AM instances auto IM insert 1 row, am1")
    expect_equal(am2$IM()$ID$PE, data.table(code = 5L, PE_ID = 1, key = "code"), info = "multiple AM instances auto IM insert 1 row, am2")

})

test_that("AM loading method exception scenarios", {

    # lack of hist
    # lack of knot
    # misspelled col name
    # misspelled attr
    # misspelled anchor
    # col name invalid data type
    # hist invalid data type
    # knot invalid data type
    # anchor invalid data type (non-list)
    # check against defined naming convention for AM instance

})
