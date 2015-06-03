context("AM mapping for loading")

test_that("AM mapping for loading", {
    mapping <- list(CM = A("commit",
                           MES = a("message"),
                           TIM = a("timestamp"),
                           HSH = a("commit"),
                           TYP = a("type", knot = "TYP")),
                    AU = A("author_email",
                           NAM = a("author", hist = "timestamp"),
                           EMA = a("author_email"),
                           ACC = a("access", knot = "ACC", hist = "access_date")))
    r <- rbindlist(lapply(selfNames(names(mapping)), A.dt, mapping))
    expect_identical(r$mne, c("MES", "TIM", "HSH", "TYP", "NAM", "EMA", "ACC"), info = "mapping expected mnes")
    expect_identical(r$hist_col, c(NA, NA, NA, NA, "timestamp", NA, "access_date"), info = "mapping expected hist cols")
    expect_identical(r$src_col, c("message", "timestamp", "commit", "type", "author", "author_email", "access"), info = "mapping expected src cols")
    expect_identical(r$anchor, c(rep("CM",4),rep("AU",3)), info = "mapping expected anchors")
    expect_identical(r$knot, c(NA, NA, NA, "TYP", NA, NA, "ACC"), info = "mapping expected knots")

})
