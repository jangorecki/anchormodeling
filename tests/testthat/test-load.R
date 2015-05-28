context("AM loading method")

#     am$create(class = "anchor", mne = "PE", desc = "Performance")
#     am$create(class = "tie", anchors = c("AC","PE"), roles = c("wasCasted","in"), identifier = c(Inf,Inf))
#     am$create(class = "anchor", mne = "PR", desc = "Program")
#     am$create(class = "tie", anchors = c("AC","PR"), roles = c("part","in"), identifier = c(Inf,Inf,1), knot = "RAT")
#     am$create(class = "knot", mne = "RAT", desc = "Rating")
#     am$run()


test_that("AM loading method valid processing scenarios", {

    #load_all()
    am <- AM$new()
    am$create(class = "anchor", mne = "AC", desc = "Actor")
    am$create(class = "attribute", anchor = "AC", mne = "NAM", desc = "Name", hist = TRUE)
    am$create(class = "attribute", anchor = "AC", mne = "GEN", desc = "Gender", knot = "GEN")
    am$create(class = "knot", mne = "GEN", desc = "Gender")
    am$run()

    # mapping
    mapping <- list(AC = list("code",
                              NAM = c("name", hist = "date"),
                              GEN = "gender"))

    # using IM
    r <- list()
    r[[1L]] <- list(code = "1", name = "Mike", gender = "M", date = Sys.Date()-5L)
    r[[2L]] <- list("2", "Bob", "M", date = Sys.Date()-5L)
    data <- rbindlist(r)
    am$load(mapping, data, 1L)
    # check loaded
    # expect_equal(am$process()[rows >= 2L, code], c("AC_NAM","AC_GEN","AC","GEN"), info = "expected am$process logs loaded")
    # TO DO - LOAD KNOTS
    # expect_equal(am$process()[rows >= 2L, sapply(obj, function(obj) nrow(obj$data)==2L)], rep(TRUE, 3), info = "all of the loaded tables has nrow==2L")
    # load update
    # data <- data.table(code = "1", name = "Mikey", gender = "M", date = Sys.Date()-4L)
    # am$load(mapping, data, 2L)

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
