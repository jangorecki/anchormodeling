context("AM read method")

test_that("AM read method", {

    am <- AM$new()
    am$create(class = "anchor", mne = "AC", desc = "Actor")
    am$create(class = "attribute", anchor = "AC", mne = "NAM", desc = "Name")
    # expected fields returned from $read
    expect_true(all(nrow(am$read()) == 2L, length(am$read())), info = "")
    expect_identical(names(am$read()), c("code", "name", "class", "mne", "desc", "obj", "hist", "knot"), info = "Before am$run all columns exists")
    am$run()
    am$load(mapping = list(AC = list("code", NAM = "name")),
            data = data.table(code = "1", name = "Mike"),
            meta = 1L)
    expect_identical(names(am$read()), c("code", "name", "class", "mne", "desc", "obj", "hist", "knot", "anchor", "parents", "childs"), info = "After *am$run* all columns exists")

})
