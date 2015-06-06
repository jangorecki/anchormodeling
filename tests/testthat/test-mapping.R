context("AM mapping for loading")

test_that("AM mapping for loading", {

    mapping <- list(CM = A("commit",
                           MES = a("message"),
                           TIM = a("timestamp"),
                           HSH = a("commit"),
                           TYP = a("type")),
                    AU = A("author_email",
                           NAM = a("author", hist = "timestamp"),
                           EMA = a("author_email"),
                           ACC = a("access", hist = "access_date")))
    r <- rbindlist(lapply(names(mapping), A.dt, mapping))
    expect_identical(r$mne, c("MES", "TIM", "HSH", "TYP", "NAM", "EMA", "ACC"), info = "mapping expected mnes")
    expect_identical(r$hist_col, c(NA, NA, NA, NA, "timestamp", NA, "access_date"), info = "mapping expected hist cols")
    expect_identical(r$src_col, c("message", "timestamp", "commit", "type", "author", "author_email", "access"), info = "mapping expected src cols")
    expect_identical(r$anchor, c(rep("CM",4),rep("AU",3)), info = "mapping expected anchors")

    mapping_lists <- list(CM = list("commit",
                                    MES = list("message"),
                                    TIM = list("timestamp"),
                                    HSH = list("commit"),
                                    TYP = list("type")),
                          AU = list("author_email",
                                    NAM = list("author", hist = "timestamp"),
                                    EMA = list("author_email"),
                                    ACC = list("access", hist = "access_date")))
    r_mapping_lists <- rbindlist(lapply(names(mapping_lists), A.dt, mapping_lists))
    expect_equal(r, r_mapping_lists, info = "mapping lists only definition valid")

    mapping_listc <- list(CM = list("commit",
                                    MES = c("message"),
                                    TIM = c("timestamp"),
                                    HSH = c("commit"),
                                    TYP = c("type")),
                          AU = list("author_email",
                                    NAM = c("author", hist = "timestamp"),
                                    EMA = c("author_email"),
                                    ACC = c("access", hist = "access_date")))
    r_mapping_listc <- rbindlist(lapply(names(mapping_listc), A.dt, mapping_listc))
    expect_equal(r, r_mapping_listc, info = "mapping list and vectors definition valid")

})
