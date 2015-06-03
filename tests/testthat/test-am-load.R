context("AM load method")

test_that("AM load method valid processing non-hist scenarios", {

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
    # incremental loading same existing non historized data - should not be loaded
    am$load(mapping = list(PR = list("code", NAM = "name")),
            data = data.table(code = 50L, name = "my another program"),
            meta = 3L)
    expect_identical(am$process()$rows, rep(2L,2L), info = "loaded second row twice")
    expect_equal(am$IM()$ID$PR, data.table(code = c(1L,50L), PR_ID = 1:2, key = "code"), info = "auto IM after second insert twice")

    # TO DO

    # evolve model 1
    am$create(class = "anchor", mne = "PE", desc = "Performance")
    am$create(class = "tie", anchors = c("PE","PR"), roles = c("at","wasPlayed"), identifier = c(1,Inf))
    am$run()

    #     am$load(mapping = list(PE = list("perf_code")),
    #             data = data.table(perf_code = 1L, prog_code = c(1L,50L)),
    #             meta = 4L)

    # evolve model 2
    am$create(class = "anchor", mne = "AC", desc = "Actor")
    am$create(class = "attribute", anchor = "AC", mne = "GEN", desc = "Gender", knot = "GEN")
    am$create(class = "knot", mne = "GEN", desc = "Gender")
    am$create(class = "tie", anchors = c("AC","PE"), roles = c("wasCasted","in"), identifier = c(Inf,Inf))
    am$run()

})

test_that("AM load method valid processing hist scenarios", {

    am <- AM$new()
    am$create(class = "anchor", mne = "AC", desc = "Actor")
    am$create(class = "attribute", anchor = "AC", mne = "NAM", desc = "Name", hist = TRUE)
    am$run()

    # initial loading hits data
    am$load(mapping = list(AC = list("code", NAM = c("name", hist = "date"))),
            data = data.table(code = 1L, name = "Mike", date = as.Date("2015-01-01")),
            meta = 1L)
    expect_identical(am$process()$rows, rep(1L,2L), info = "loaded first hist row")
    # incremental loading new hist data
    am$load(mapping = list(AC = list("code", NAM = c("name", hist = "date"))),
            data = data.table(code = 1L, name = "Mikey", date = as.Date("2015-01-05")),
            meta = 2L)
    expect_equal(am$process()[order(class), .(rows, in_nrow, unq_nrow, load_nrow)], data.table(rows = 1:2, in_nrow = c(1L,1L), unq_nrow = c(1L,1L), load_nrow = c(0L,1L)), info = "loaded second hist row")
    # incremental loading same existing historized data - should not be loaded, but no error
    am$load(mapping = list(AC = list("code", NAM = c("name", hist = "date"))),
            data = data.table(code = 1L, name = "Mikey", date = as.Date("2015-01-05")),
            meta = 3L)
    expect_equal(am$OBJ("AC_NAM")$data,
                 data.table(AC_NAM_AC_ID = rep(1L,2),
                            AC_NAM_Actor_Name = c("Mike","Mikey"),
                            AC_NAM_ChangedAt = c(as.Date("2015-01-01"), as.Date("2015-01-05")),
                            Metadata_AC_NAM = 1:2,
                            key = c("AC_NAM_AC_ID","AC_NAM_ChangedAt")),
                 info = "historized duplicates on PK silently ignored")

    # restatment

    # TO DO

    # evolve
    am$create(class = "anchor", mne = "PR", desc = "Program")
    am$create(class = "tie", anchors = c("AC","PR"), roles = c("part","in"), identifier = c(Inf,Inf,1), knot = "RAT")
    am$create(class = "knot", mne = "RAT", desc = "Rating")
    am$run()

    expect_true(TRUE, info = "dummy")

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

test_that("AM loading method technical exception scenarios", {

    # loading ONLY anchor - not currently supported
    am <- AM$new()
    am$create(class = "anchor", mne = "AC", desc = "Actor")
    am$run()
    expect_error(am$load(mapping = list(AC = list("code")),
                         data = data.table(code = 1L),
                         meta = 1L),
                 "Loading only anchor but no attributes not currently supported.",
                 info = "Loading only anchor but no attributes not currently supported.")
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

test_that("AM loading method data exception scenarios", {

    am <- AM$new()
    am$create(class = "anchor", mne = "PR", desc = "Program")
    am$create(class = "attribute", anchor = "PR", mne = "NAM", desc = "Name")
    am$run()

    # incremental loading new version of static attribute - ERROR
    am$load(mapping = list(PR = list("code", NAM = "name")),
            data = data.table(code = 50L, name = "my program"),
            meta = 1L)
    expect_error(am$load(mapping = list(PR = list("code", NAM = "name")),
                         data = data.table(code = 50L, name = "my program new name"),
                         meta = 2L),
                 "Duplicate key violates defined model. You are trying to insert different value into PR_NAM_Program_Name for same existing identity. If you want want to have multiple values for that identity you should historize that attribute.",
                 info = "incremental loading new version of static attribute should produce ERROR")

})
