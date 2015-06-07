context("IM identitiy management")

test_that("IM process", {

    set.seed(1)

    im <- IM$new()
    expect_error(im$create(mne = "XX"), "argument \"nk\" is missing, with no default", info = "missing nk arg in im$create")
    im$create(mne = "GEN", nk = "gender")
    im$create(mne = "UL", nk = "user_login")
    data <- data.table(user_login = c("asd","qwe","zxc","asd","zxc","asd"), gender = c("F","M","M","F","M","F"), balance = rnorm(6,10))
    data.sk <- im$use(data, mne = c("UL","GEN"))
    expect_identical(names(data.sk),c("user_login", "gender", "balance", "UL_ID", "gender_GEN_ID"), info="new columns added")
    expect_identical(names(data),c("user_login", "gender", "balance"), info="input data has not been updated by reference")
    expect_true(data.sk[is.na(UL_ID) | is.na(gender_GEN_ID), .N == 0L], info="no NA surrogate keys")
    data <- data.table(user_login = c("zxc","qaz","qaz","zaq"), gender = c("M","F","F","M"), balance = rnorm(4,10)) # incremental load
    data.sk <- im$use(data, mne = c("UL","GEN"))
    expect_true(data.sk[is.na(UL_ID) | is.na(gender_GEN_ID), .N == 0L], info="no NA surrogate keys in incremental")
    expect_identical(sapply(im$ID, nrow), c("GEN" = 2L, "UL" = 5L), info="IM ref data correct amount after incremental")
    im$delete("UL")
    expect_identical(sapply(im$ID, nrow), c("GEN" = 2L), info="IM delete method")
    # composite natural key
    data <- data.table(actor_code1 = sample(3,10,TRUE), actor_name = sample(letters,10,TRUE))[,actor_code2 := seq_len(.N), actor_code1]
    im <- IM$new()
    im$create(mne = "AC", nk = paste0("actor_code",1:2))
    data.sk <- im$use(data, mne = "AC")
    expect_identical(im$NK[["AC"]],c("actor_code1", "actor_code2"), info="composite natural key")
    expect_identical(names(data.sk),c("actor_code1", "actor_name", "actor_code2", "AC_ID"), info="composite natural key use method output")
    expect_true(data.sk[is.na(AC_ID), .N == 0L], info="no NA surrogate keys from composite natural key")

})

test_that("auto im$create while im$use", {

    # basic scenario

    im <- IM$new()
    im$create(mne = "UL", nk = "user_login")
    data <- im$use(data = data.table(user_login = c("nick1","nick2")),
                   mne = "UL")
    expect_identical(names(data), c("user_login","UL_ID"), info = "result of IM$use produce expected columns on auto im$create on im$use")
    expect_equal(im$ID[["UL"]], data.table(user_login = c("nick1","nick2"), UL_ID = 1:2, key = "user_login"), info = "expected IDs produced on auto im$create on im$use")
    expect_error(data <- im$use(data = data.table(user_login = c("nick1","nick2"))), "argument \"mne\" is missing, with no default", info = "missing mne in im$use")
    expect_error(data <- im$use(mne = "XX"), "argument \"data\" is missing, with no default", info = "missing data in im$use")

    # incremental: defined nk in IM model not exist in names(data) different - no `nk` provided, error
    expect_error(data <- im$use(data = data.table(ulogin = c("nick3","nick2")),
                                mne = "UL"),
                 "Defined natural key for mnemonic UL do not exists in incoming data. Expected names: user_login. To use different column provide `nk` argument, or update data in your IM instance.",
                 info = "expected error on diff name and no `nk` arg")

    # incremental: defined nk in IM model not exist in names(data) different - `nk` argument list of columns to use by mnemonic - dynamic src column names
    data <- im$use(data = data.table(ulogin = c("nick3","nick2")),
                   mne = "UL",
                   nk = list(UL = c("ulogin")))
    expect_identical(names(data), c("ulogin","user_login","UL_ID"), info = "IM$use auto-create dynamic column rename on new src column, expected cols")
    expect_true(data[, identical(ulogin,user_login)], info = "copied columns identical")

    # composite key

    im <- IM$new()
    im$create(mne = "UA", nk = c("user_login","user_action"))
    data <- im$use(data = data.table(user_login = c("nick1","nick2","nick1"), user_action = c("click A","click A","click B")),
                   mne = "UA")
    expect_identical(names(data), c("user_login","user_action","UA_ID"), info = "composite key as defined")

    # incremental: defined nk in IM model not exist in names(data) different - no `nk` provided, error
    expect_error(data <- im$use(data = data.table(User_login = c("nick1","nick2","nick1"), User_action = c("click A","click A","click B")),
                                mne = "UA"),
                 "Defined natural key for mnemonic UA do not exists in incoming data. Expected names: user_login, user_action. To use different column provide `nk` argument, or update data in your IM instance.",
                 info = "composite key expected error on diff name and no `nk` arg")

    # incremental: defined nk in IM model not exist in names(data) different - `nk` argument list of columns to use by mnemonic - dynamic src column names
    data <- im$use(data = data.table(User_login = c("nick1","nick2","nick1"), User_action = c("click A","click A","click B")),
                   mne = "UA",
                   nk = list(UA = c("User_login","User_action")))
    expect_identical(names(data), c("User_login","User_action","user_login","user_action","UA_ID"), info = "composite key after dynamic remap by `nk` argument")



})

test_that("IM naming convention to detect AMobj class", {

    # detection of AMobj will allow the IM to manage:
    ## keep multiple cols for anchor composite key
    ## union multiple cols for shared knots

    ## anchor composite key dynamic

    im <- IM$new() # default naming: 2L, 3L
    im$create(mne = "UA", nk = c("user_login","user_action"))
    im$create(mne = "TOK", nk = "token")
    expect_identical(im$am.class, list(UA = "anchor", TOK = "knot"), info = "default values class detected")
    data <- im$use(data = data.table(User_login = c("nick1","nick2","nick1"), user_action = c("click A","click A","click B")),
                   nk = list(UA = c("User_login","user_action")),
                   mne = "UA")
    expect_identical(names(data), c("User_login","user_action","user_login","UA_ID"), info = "Columns check, extra remapped column and ID")
    expect_identical(names(im$ID[["UA"]]), c("user_login","user_action","UA_ID"), info = "Idenetity storage col names static after dynamic remapping on composite key")
    expect_identical(nrow(im$ID[["UA"]]), 3L, info = "first loading on auto remap and composite key")

    data <- im$use(data = data.table(Zuser_login = c("nick2","nick3"), user_action = c("click A","click A")),
                   nk = list(UA = c("Zuser_login","user_action")),
                   mne = "UA")
    expect_identical(names(data), c("Zuser_login","user_action","user_login","UA_ID"), info = "Columns check, extra remapped column and ID, 2nd.")
    expect_identical(nrow(im$ID[["UA"]]), 4L, info = "second loading")

    ## shared knot dynamic - union tables

    im <- IM$new() # default naming: 2L, 3L
    im$create(mne = "UL", nk = "user_login")
    im$create(mne = "CO", nk = "content_id")
    im$create(mne = "RAT", nk = c("rating1","rating2"))
    expect_identical(im$am.class, list(UL = "anchor", CO = "anchor", RAT = "knot"), info = "default values class detected")
    set.seed(1)
    smpr <- function(n) sample(x = LETTERS, size = n, TRUE)
    data <- data.table(user_login = c("nick1","nick2","nick1"), content_id = c(1L,1L,2L), rating1_date = as.Date("2015-06-07")+c(0L,0L,1L), rating1 = smpr(3), rating2_date = as.Date("2015-06-07")+c(1L,2L,1L), rating2 = smpr(3))
    data <- im$use(data = data, mne = c("UL","CO","RAT")) # default defined
    expect_identical(names(data),
                     c("user_login", "content_id", "rating1_date", "rating1", "rating2_date", "rating2", "UL_ID", "CO_ID", "rating1_RAT_ID", "rating2_RAT_ID"),
                     info = "Columns check for shared knots")
    expect_identical(names(im$ID[["RAT"]]), c("rating1","RAT_ID"), info = "knots with composite natural keys gets union'ed and stored in single column to get ID")

    # remap columns on demand
    nk <- list(UL = "user_login",
               CO = "content_id",
               RAT = c("rating_avg1","rating_avg2"))
    data <- data.table(user_login = c("nick3","nick4","nick4"), content_id = c(1L,2L,2L), rating_avg1_date = as.Date("2015-06-07")+c(0L,0L,3L), rating_avg1 = smpr(3), rating_avg2_date = as.Date("2015-06-07")+c(3L,3L,4L), rating_avg2 = smpr(3))
    data <- im$use(data = data, nk = nk, mne = c("UL","CO","RAT")) # use nk to remap cols
    expect_identical(names(data),
                     c("user_login", "content_id", "rating_avg1_date", "rating_avg1", "rating_avg2_date", "rating_avg2", "UL_ID", "CO_ID", "rating_avg1_RAT_ID", "rating_avg2_RAT_ID"),
                     info = "Columns check for shared knots after remap")
    expect_identical(names(im$ID[["RAT"]]), c("rating1","RAT_ID"), info = "knots with composite natural keys gets union'ed and stored in single column to get ID after remap")

})

test_that("multiple IM instances evolution process", {

    set.seed(1)

    im1 <- IM$new()
    im1$create(mne = "UL", nk = "user_login")
    im2 <- IM$new()
    im2$create(mne = "UL", nk = "user_login")

    ## first use - initial load
    # 1
    data <- im1$use(data = data.table(user_login = c("nick1","nick2")),
                    mne = "UL")
    expect_identical(names(data), c("user_login","UL_ID"), info = "result of IM$use produce expected columns, im1 1st use")
    expect_equal(im1$ID[["UL"]], data.table(user_login = c("nick1","nick2"), UL_ID = 1:2, key = "user_login"), info = "expected IDs produced, im1 1st use")
    # 2
    data <- im2$use(data = data.table(user_login = c("nick5","nick6")),
                    mne = "UL")
    expect_identical(names(data), c("user_login","UL_ID"), info = "result of IM$use produce expected columns, im2 st use")
    expect_equal(im2$ID[["UL"]], data.table(user_login = c("nick5","nick6"), UL_ID = 1:2, key = "user_login"), info = "expected IDs produced, im2 1st use")

    ## second use - data evolution
    # 1
    data <- im1$use(data = data.table(user_login = c("nick1","nick3")),
                    mne = "UL")
    expect_identical(names(data), c("user_login","UL_ID"), info = "result of IM$use produce expected columns, im1 2nd use")
    expect_equal(im1$ID[["UL"]], data.table(user_login = paste0(c("nick"),1:3), UL_ID = 1:3, key = "user_login"), info = "expected IDs produced, im1 2nd use")
    # 2
    data <- im2$use(data = data.table(user_login = c("nick5","nick9")),
                    mne = "UL")
    expect_identical(names(data), c("user_login","UL_ID"), info = "result of IM$use produce expected columns, im1 2nd use")
    expect_equal(im2$ID[["UL"]], data.table(user_login = paste0(c("nick"),c(5:6,9)), UL_ID = 1:3, key = "user_login"), info = "expected IDs produced, im2 2nd use")

    ## third use - model evolution
    # 1
    im1$create(mne = "CU", nk = "currency_code")
    data <- im1$use(data = tmp_dt <- data.table(client = 1:3, currency_code = c("GBP","BTC","BTC"), value = rnorm(3)),
                    mne = "CU")
    expect_identical(names(data), c(names(tmp_dt), "CU_ID"), info = "result of IM$use produce expected columns, im1 3nd use")
    expect_equal(im1$ID[["CU"]], data.table(currency_code = c("GBP","BTC"), CU_ID = 1:2, key = "currency_code"), info = "expected IDs produced, im1 3nd use")
    # 2
    im2$create(mne = "CU", nk = "currency_code")
    data <- im2$use(data = tmp_dt <- data.table(client = 2:4, currency_code = c("GBP","GBP","BTC"), value = rnorm(3)),
                    mne = "CU")
    expect_identical(names(data), c(names(tmp_dt), "CU_ID"), info = "result of IM$use produce expected columns, im2 3nd use")
    expect_equal(im2$ID[["CU"]], data.table(currency_code = c("GBP","BTC"), CU_ID = 1:2, key = "currency_code"), info = "expected IDs produced, im2 3nd use")

})

test_that("IM naming convention", {

    im1 <- IM$new()
    im1$create(mne = "UL", nk = "some_anchor")
    im1$create(mne = "ULZ", nk = "some_knot")
    expect_error(im1$create(mne = "ULZA", nk = "unknown"),
                 "Provided mnemonic to mne argument does not match to defined naming convention nchar: anchor 2, knot 3",
                 info = "Invalid mne vs naming convention")

})
