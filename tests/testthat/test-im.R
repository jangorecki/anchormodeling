context("IM identitiy management")

test_that("IM process", {

    set.seed(1)

    im <- IM$new()
    im$create(mne = "GEN", nk = "gender")
    im$create(mne = "UL", nk = "user_login")
    data <- data.table(user_login = c("asd","qwe","zxc","asd","zxc","asd"), gender = c("F","M","M","F","M","F"), balance = rnorm(6,10))
    data.sk <- im$use(data, mne = c("UL","GEN"))
    expect_identical(names(data.sk),c("user_login", "gender", "balance", "UL_ID", "GEN_ID"), info="new columns added")
    expect_identical(names(data),c("user_login", "gender", "balance"), info="input data has not been updated by reference")
    expect_true(data.sk[is.na(UL_ID) | is.na(GEN_ID), .N == 0L], info="no NA surrogate keys")
    data <- data.table(user_login = c("zxc","qaz","qaz","zaq"), gender = c("M","F","F","M"), balance = rnorm(4,10)) # incremental load
    data.sk <- im$use(data, mne = c("UL","GEN"))
    expect_true(data.sk[is.na(UL_ID) | is.na(GEN_ID), .N == 0L], info="no NA surrogate keys in incremental")
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
