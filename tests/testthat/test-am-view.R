context("AM view method")

test_that("AM view cases", {

    am <- AM$new()
    am$create(class = "anchor", mne = "PR", desc = "Program")
    am$create(class = "attribute", anchor = "PR", mne = "NAM", desc = "Name")
    am$run()
    am$load(mapping = list(PR = list("code", NAM = "name")),
            data = data.table(code = 1L, name = "my program"),
            meta = 1L)
    expect_equal(am$view("PR"),
                 data.table(PR_ID = 1L, Metadata_PR = 1L, PR_NAM_Program_Name = "my program", Metadata_PR_NAM = 1L, key = "PR_ID"),
                 info = "basic view")

})
