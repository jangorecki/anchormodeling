context("AM anchormodeling")

test_that("AM basic construct", {

    am <- AM$new()
    expect_true(nrow(am$log)==1L, info="AM logging1")
    am$create(class = "anchor", mne = "AC", desc = "Actor")
    am$create(class = "attribute", anchor = "AC", mne = "GEN", desc = "Gender")
    expect_true(nrow(am$log)==3L, info="AM logging2")
    expect_true(all(sapply(am$data$obj, is.am)), info="AM all obj are MAobj")

})
