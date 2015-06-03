context("AM view method")

test_that("AM view cases", {

    am <- AM$new()
    am$create(class = "anchor", mne = "PR", desc = "Program")
    am$run()
    am$load(mapping = list(PR = list("code")),
            data = data.table(code = 1L, name = "not loaded here anyway"),
            meta = 1L)
    expect_equal(am$view("PR"),
                 data.table(PR_ID = 1L, Metadata_PR = 1L, key = "PR_ID"),
                 info = "1A view")

    am$create(class = "attribute", anchor = "PR", mne = "NAM", desc = "Name")
    am$run()
    am$load(mapping = list(PR = list("code", NAM = "name")),
            data = data.table(code = 2L, name = "my program"),
            meta = 1L)
    expect_equal(am$view("PR"),
                 data.table(PR_ID = 1:2, Metadata_PR = c(1L,1L), PR_NAM_Program_Name = c(NA_character_,"my program"), Metadata_PR_NAM = c(NA_integer_, 1L), key = "PR_ID"),
                 info = "1A 1a view")

})
