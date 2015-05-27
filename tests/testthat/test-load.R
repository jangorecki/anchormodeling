context("AM loading method")

test_that("AM loading method cases", {

    am <- AM$new()
    am$create(class = "anchor", mne = "AC", desc = "Actor")
    am$create(class = "attribute", anchor = "AC", mne = "NAM", desc = "Name")
    am$create(class = "attribute", anchor = "AC", mne = "GEN", desc = "Gender", knot = "GEN")
    am$create(class = "knot", mne = "GEN", desc = "Gender")
    am$create(class = "anchor", mne = "PE", desc = "Performance")
    am$create(class = "tie", anchors = c("AC","PE"), roles = c("wasCasted","in"), identifier = c(Inf,Inf))
    am$create(class = "anchor", mne = "PR", desc = "Program")
    am$create(class = "tie", anchors = c("AC","PR"), roles = c("part","in"), identifier = c(Inf,Inf,1), knot = "RAT")
    am$create(class = "knot", mne = "RAT", desc = "Rating")
    am$validate()



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
