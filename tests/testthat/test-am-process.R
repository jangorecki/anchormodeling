context("AM process method")

test_that("AM process method", {

    am <- AM$new()
    am$create(class = "anchor", mne = "PR", desc = "Program")
    am$create(class = "attribute", anchor = "PR", mne = "NAM", desc = "Name")
    am$run()

    am$load(mapping = list(PR = list("code", NAM = "name")),
            data = data.table(code = 50L, name = "my program"),
            meta = 1L)
    expect_equal(am$process()[,.(in_nrow, unq_nrow, load_nrow)], data.table(in_nrow = c(1L,1L), unq_nrow = c(1L,1L), load_nrow = c(1L,1L)), info = "am$process etl nrows metadata, first")
    # loading existing data and duplicated to test latest etl metadata
    am$load(mapping = list(PR = list("code", NAM = "name")),
            data = data.table(code = rep(50L,2L), name = rep("my program",2L)),
            meta = 2L)
    expect_equal(am$process()[,.(in_nrow, unq_nrow, load_nrow)], data.table(in_nrow = c(2L,2L), unq_nrow = c(1L,1L), load_nrow = c(0L,0L)), info = "am$process etl nrows metadata, second")

})
