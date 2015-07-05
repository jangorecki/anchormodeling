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

test_that("AM csv method", {

    am <- AM$new()
    am$create("anchor", mne = "AC", desc = "Actor")
    am$create("attribute", anchor = "AC", mne = "NAM", desc = "Name", hist = TRUE)
    am$create("attribute", anchor = "AC", mne = "GEN", desc = "Gender", knot = "GEN")
    am$create("knot", mne = "GEN", desc = "Gender")
    am$create("attribute", anchor = "AC", mne = "PLV", desc = "ProfessionalLevel", knot = "PLV", hist = TRUE)
    am$create("knot", mne = "PLV", desc = "ProfessionalLevel")
    am$create("anchor", mne = "PR", desc = "Program")
    am$create("attribute", anchor = "PR", mne = "NAM", desc = "Name") # not used here in am$csv method
    am$create(class = "knot", mne = "RAT", desc = "Rating")
    am$create(class = "tie", anchors = c("AC","PR"), knot = "RAT", roles = c("part","in","got"), identifier = c(Inf,Inf,1), hist = TRUE)
    am$run()

    actor_mapping <- list(AC = list("code",
                                    NAM = c("name", hist = "date"),
                                    GEN = "gender",
                                    PLV = c("level", hist = "date")))

    actor_data <- data.table(code = c("1", "2", "3", "4"),
                             name = c("Mike", "Bob", "Alice", "Lee"),
                             gender = c("M", "M", "F", "M"),
                             level = c(4L, 1L, 3L, 4L),
                             date = as.Date("2015-07-05"))
    am$load(mapping = actor_mapping,
            data = actor_data,
            meta = 1L)

    actor_data <- data.table(code = c("1", "2", "3"),
                             name = c("Mike", "Ben", "Alice"), # 1 name changed
                             gender = c("M", "M", "F"),
                             level = c(5L, 1L, 3L), # 1 level change
                             date = as.Date("2015-07-06"))
    am$load(mapping = actor_mapping,
            data = actor_data,
            meta = 2L)

    actor_program_mapping <- list(PR = list("prog_code"),
                                  AC = list("acto_code"),
                                  AC_PR_RAT = list(hist = "date", knot = "score"))
    actor_program_data <- data.table(prog_code = c(1:2,3L,3L),
                                     acto_code = as.character(c(1:2,2L,2L)),
                                     score = c("A","D","E","D"),
                                     date = as.Date("2015-07-03")+c(0:1,0:1))
    am$load(mapping = actor_program_mapping,
            data = actor_program_data,
            meta = 3L)

    csv.paths <- am$csv(dir = tempdir())

    expect_true(all(file.exists(csv.paths)), info = "AM csv method - file exported")
    file.remove(csv.paths)

})
