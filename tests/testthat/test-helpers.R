context("Unit tests for helpers")

test_that("SQL related", {

    x <- data.table(a = 1:2, b = letters[1:2], d = as.Date("2015-06-05"), e = as.POSIXct("2015-06-05 22:53:34"), l = c(T,F))
    r <- c("INSERT INTO schema.table (a, b, d, e, l) VALUES (1, 'a', '2015-06-05'::date, '2015-06-05 22:53:34'::timestamp, TRUE);", "INSERT INTO schema.table (a, b, d, e, l) VALUES (2, 'b', '2015-06-05'::date, '2015-06-05 22:53:34'::timestamp, FALSE);")
    expect_identical(insert.postgres(x), r, info = "data.table to postgres")

})
