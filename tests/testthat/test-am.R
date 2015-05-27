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
