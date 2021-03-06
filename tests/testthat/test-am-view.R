context("AM view method")

test_that("AM view cases", {

    # workflow with data evolution

    actor_data <- data.table(code = c("1", "2", "3", "4"),
                             name = c("Mike", "Bob", "Alice", "Lee"),
                             gender = c("M", "M", "F", "M"),
                             level = c(4L, 1L, 3L, 4L),
                             date = as.Date("2015-07-05"))

    am <- AM$new()
    am$create("anchor", mne = "AC", desc = "Actor")$run()
    am$load(mapping = list(AC = list("code")),
            data = actor_data[1],
            meta = 1L)
    expect_equal(am$view("AC"),
                 data.table(AC_ID = 1L, Metadata_AC = 1L, key = "AC_ID"),
                 check.attributes = FALSE,
                 info = "currenct view AC anchor only, step 1")

    am$create("attribute", anchor = "AC", mne = "NAM", desc = "Name", hist = TRUE)$run()
    am$load(mapping = list(AC = list("code",
                                     NAM = c("name", hist = "date"))),
            data = actor_data[1:2],
            meta = 2L)
    expect_equal(am$view("AC"),
                 data.table(AC_ID = 1:2, Metadata_AC = 1:2, AC_NAM_AC_ID = 1:2, Metadata_AC_NAM = c(2L,2L), AC_NAM_ChangedAt = as.Date("2015-07-05")[rep(1,2)], AC_NAM_Actor_Name = c("Mike","Bob"), key = "AC_ID"),
                 check.attributes = FALSE,
                 info = "currenct view AC + NAM, step 2")

    am$create("attribute", anchor = "AC", mne = "GEN", desc = "Gender", knot = "GEN")
    am$create("knot", mne = "GEN", desc = "Gender")$run()
    am$load(mapping = list(AC = list("code",
                                     NAM = c("name", hist = "date"),
                                     GEN = "gender")),
            data = actor_data[2:3],
            meta = 3L)
    expect_equal(am$view("AC"),
                 data.table(AC_ID = 1:3,
                            Metadata_AC = 1:3,
                            AC_GEN_AC_ID = c(NA, 2L, 3L),
                            Metadata_AC_GEN = c(NA, 3L, 3L),
                            AC_GEN_GEN_Gender = c(NA, "M", "F"),
                            AC_GEN_Metadata_GEN = c(NA, 3L, 3L),
                            AC_GEN_GEN_ID = c(NA, 1L, 2L),
                            AC_NAM_AC_ID = 1:3,
                            Metadata_AC_NAM = c(2L,2L,3L),
                            AC_NAM_ChangedAt = as.Date("2015-07-05")[rep(1,3)],
                            AC_NAM_Actor_Name = c("Mike","Bob","Alice"),
                            key = "AC_ID"),
                 check.attributes = FALSE,
                 info = "currenct view AC + NAM + GEN, step 3")

    am$create("attribute", anchor = "AC", mne = "PLV", desc = "ProfessionalLevel", knot = "PLV", hist = TRUE)
    am$create("knot", mne = "PLV", desc = "ProfessionalLevel")$run()
    am$load(mapping = list(AC = list("code",
                                     NAM = c("name", hist = "date"),
                                     GEN = "gender",
                                     PLV = c("level", hist = "date"))),
            data = actor_data[3:4],
            meta = 4L)
    expect_equal(am$view("AC"),
                 data.table(AC_ID = 1:4,
                            Metadata_AC = 1:4,
                            AC_GEN_AC_ID = c(NA, 2L, 3L, 4L),
                            Metadata_AC_GEN = c(NA, 3L, 3L, 4L),
                            AC_GEN_GEN_Gender = c(NA, "M", "F", "M"),
                            AC_GEN_Metadata_GEN = c(NA, 3L, 3L, 3L),
                            AC_GEN_GEN_ID = c(NA, 1L, 2L, 1L),
                            AC_NAM_AC_ID = 1:4,
                            Metadata_AC_NAM = c(2L,2L,3L,4L),
                            AC_NAM_ChangedAt = as.Date("2015-07-05")[rep(1,4)],
                            AC_NAM_Actor_Name = c("Mike","Bob","Alice","Lee"),
                            AC_PLV_AC_ID = c(NA, NA, 3L, 4L),
                            Metadata_AC_PLV = c(NA, NA, 4L, 4L),
                            AC_PLV_ChangedAt = as.Date("2015-07-05")[c(NA,NA,1L,1L)],
                            AC_PLV_PLV_ProfessionalLevel = c(NA, NA, 3L, 4L),
                            AC_PLV_Metadata_PLV = c(NA, NA, 4L, 4L),
                            AC_PLV_PLV_ID = c(NA, NA, 1L, 2L),
                            key = "AC_ID"),
                 check.attributes = FALSE,
                 info = "currenct view AC + NAM + GEN + PLV, step 4")

    am$create("anchor", mne = "PR", desc = "Program")
    am$create(class = "knot", mne = "RAT", desc = "Rating")
    am$create(class = "tie", anchors = c("AC","PR"), knot = "RAT", roles = c("part","in","got"), identifier = c(Inf,Inf,1), hist = TRUE)$run()
    am$load(mapping = list(PR = list("prog_code"),
                           AC = list("acto_code"),
                           AC_PR_RAT = list(hist = "date", knot = "score")),
            data = data.table(prog_code = c(1:2,3L,3L),
                              acto_code = as.character(c(1:2,2L,2L)),
                              score = c("A","D","E","D"),
                              date = as.Date("2015-07-03")+c(0:1,0:1)),
            meta = 5L)
    expect_equal(names(am$view("AC_part_PR_in_RAT_got")),
                 c("Metadata_AC_part_PR_in_RAT_got","AC_part_PR_in_RAT_got_ChangedAt","AC_ID_part","PR_ID_in","got_RAT_Rating","got_Metadata_RAT","RAT_ID_got"),
                 check.attributes = FALSE,
                 info = "currenct view AC_part_PR_in_RAT_got valid column and order, step 5")
    expect_equal(am$view("AC_part_PR_in_RAT_got"),
                 data.table(Metadata_AC_part_PR_in_RAT_got = c(5L, 5L, 5L),
                            AC_part_PR_in_RAT_got_ChangedAt = as.Date("2015-07-03")+c(0:1,1L),
                            AC_ID_part = c(1L, 2L, 2L),
                            PR_ID_in = 1:3,
                            got_RAT_Rating = c("A", "D", "D"),
                            got_Metadata_RAT = c(5L, 5L, 5L),
                            RAT_ID_got = c(1L, 2L, 2L),
                            key = c("AC_ID_part", "PR_ID_in", "AC_part_PR_in_RAT_got_ChangedAt")),
                 check.attributes = FALSE,
                 info = "currenct view AC_part_PR_in_RAT_got data, step 5")

    am$create("attribute", anchor = "PR", mne = "NAM", desc = "Name")$run()
    am$load(mapping = list(PR = list("code",
                                     NAM = "name")),
            data = data.table(code = 1:3, name = c("show1","show2","show3")),
            meta = 6L)
    expect_equal(am$view("PR"),
                 data.table(PR_ID = 1:3,
                            Metadata_PR = c(5L, 5L, 5L),
                            PR_NAM_PR_ID = 1:3,
                            Metadata_PR_NAM = c(6L, 6L, 6L),
                            PR_NAM_Program_Name = c("show1", "show2", "show3"),
                            key = "PR_ID"),
                 check.attributes = FALSE,
                 info = "currenct view PR + NAM, step 6")

})

test_that("AM temporal filter", {

    if(Sys.Date() >= as.Date("2115-07-05")) skip("test invalid after 2115-07-05")

    actor_data <- data.table(code = c("1", "2", "3", "4"),
                             name = c("Mike", "Bob", "Alice", "Lee"),
                             gender = c("M", "M", "F", "M"),
                             level = c(4L, 1L, 3L, 4L),
                             uni_date = c(as.Date("2015-07-05")+0:1,as.Date("2115-07-05")+0:1),
                             name_date = c(as.Date("2015-07-05")+0:1,as.Date("2115-07-05")+0:1),
                             level_date = c(as.Date("2015-07-05"),as.Date("2115-07-05"),as.Date("2015-07-06"),as.Date("2115-07-06")))

    am <- AM$new()
    am$create("anchor", mne = "AC", desc = "Actor")
    am$create("attribute", anchor = "AC", mne = "NAM", desc = "Name", hist = TRUE)
    am$create("attribute", anchor = "AC", mne = "GEN", desc = "Gender", knot = "GEN")
    am$create("knot", mne = "GEN", desc = "Gender")
    am$create("attribute", anchor = "AC", mne = "PLV", desc = "ProfessionalLevel", knot = "PLV", hist = TRUE)
    am$create("knot", mne = "PLV", desc = "ProfessionalLevel")$run()
    am$load(mapping = list(AC = list("code",
                                     NAM = c("name", hist = "uni_date"),
                                     GEN = "gender",
                                     PLV = c("level", hist = "uni_date"))),
            data = actor_data,
            meta = 1L)
    expect_true(nrow(am$view("AC"))==4L, info = "current view NO temporal filter, case 1") # current: 2 rows
    expect_true(nrow(am$view("AC", type = "latest"))==4L, info = "latest view NO temporal filter, case 1") # 4 rows
    expect_true(nrow(am$view("AC", type = "timepoint", time = as.Date("2015-07-05")))==4L, info = "timepoint view NO temporal filter, case 1") # 1 row
    expect_true(nrow(am$view("AC",na.rm=TRUE))==2L, info = "current view temporal filter, case 1") # current: 2 rows
    expect_true(nrow(am$view("AC",na.rm=TRUE, type = "latest"))==4L, info = "latest view temporal filter, case 1") # 4 rows
    expect_true(nrow(am$view("AC",na.rm=TRUE, type = "timepoint", time = as.Date("2015-07-05")))==1L, info = "timepoint view temporal filter, case 1") # 1 row

    am <- AM$new()
    am$create("anchor", mne = "AC", desc = "Actor")
    am$create("attribute", anchor = "AC", mne = "NAM", desc = "Name", hist = TRUE)
    am$create("attribute", anchor = "AC", mne = "GEN", desc = "Gender", knot = "GEN")
    am$create("knot", mne = "GEN", desc = "Gender")
    am$create("attribute", anchor = "AC", mne = "PLV", desc = "ProfessionalLevel", knot = "PLV", hist = TRUE)
    am$create("knot", mne = "PLV", desc = "ProfessionalLevel")$run()
    am$load(mapping = list(AC = list("code",
                                     NAM = c("name", hist = "name_date"),
                                     GEN = "gender",
                                     PLV = c("level", hist = "level_date"))),
            data = actor_data,
            meta = 1L)
    expect_true(nrow(am$view("AC"))==4L, info = "current view NO temporal filter, case 2") # current: 3 rows
    expect_true(nrow(am$view("AC", type = "latest"))==4L, info = "latest view NO temporal filter, case 2") # 4 rows
    expect_true(nrow(am$view("AC", type = "timepoint", time = as.Date("2015-07-05")))==4L, info = "timepoint view NO temporal filter, case 2") # 1 row
    expect_true(nrow(am$view("AC",na.rm=TRUE))==3L, info = "current view temporal filter, case 2") # current: 3 rows
    expect_true(nrow(am$view("AC",na.rm=TRUE, type = "latest"))==4L, info = "latest view temporal filter, case 2") # 4 rows
    expect_true(nrow(am$view("AC",na.rm=TRUE, type = "timepoint", time = as.Date("2015-07-05")))==1L, info = "timepoint view temporal filter, case 2") # 1 row

})

test_that("AM difference view", {

    actor_data <- data.table(code = c("1", "2", "3", "2", "1", "2"),
                             name = c("Mike", "Bob", "Alice", "Boby", "Mikey","Boby"),
                             gender = c("M", "M", "F", "M", "M", "M"),
                             level = c(4L, 1L, 3L, 1L, 5L, 2L),
                             name_date = c(as.Date("2015-07-05")+c(0:1,3L,3L,2L,3L)),
                             level_date = c(as.Date("2015-07-05")+c(1:2,4,2L,2L,5L)))

    am <- AM$new()
    am$create("anchor", mne = "AC", desc = "Actor")
    am$create("attribute", anchor = "AC", mne = "NAM", desc = "Name", hist = TRUE)
    am$create("attribute", anchor = "AC", mne = "GEN", desc = "Gender", knot = "GEN")
    am$create("knot", mne = "GEN", desc = "Gender")
    am$create("attribute", anchor = "AC", mne = "PLV", desc = "ProfessionalLevel", knot = "PLV", hist = TRUE)
    am$create("knot", mne = "PLV", desc = "ProfessionalLevel")$run()
    am$load(mapping = list(AC = list("code",
                                     NAM = c("name", hist = "name_date"),
                                     GEN = "gender",
                                     PLV = c("level", hist = "level_date"))),
            data = actor_data,
            meta = 1L)

    # output of below test validated against postgres db, script in tests/manual/test-view-r-postgres.R

    expect_equal(am$view("AC", type = "difference", time = c(as.Date("2015-07-03"), as.Date("2015-07-07"))),
                 data.table(inspectedTimepoint = structure(c(16621, 16622, 16623, 16623, 16622, 16623), class = "Date"),
                            mnemonic = c("NAM", "PLV", "NAM", "PLV", "NAM", "PLV"),
                            AC_ID = c(1L, 1L, 1L, 1L, 2L, 2L),
                            Metadata_AC = c(1L, 1L, 1L, 1L, 1L, 1L),
                            AC_GEN_AC_ID = c(1L, 1L, 1L, 1L, 2L, 2L),
                            Metadata_AC_GEN = c(1L, 1L, 1L, 1L, 1L, 1L),
                            AC_GEN_GEN_Gender = c("M", "M", "M", "M", "M", "M"),
                            AC_GEN_Metadata_GEN = c(1L, 1L, 1L, 1L, 1L, 1L),
                            AC_GEN_GEN_ID = c(1L, 1L, 1L, 1L, 1L, 1L),
                            AC_NAM_AC_ID = c(1L, 1L, 1L, 1L, 2L, 2L),
                            Metadata_AC_NAM = c(1L, 1L, 1L, 1L, 1L, 1L),
                            AC_NAM_ChangedAt = structure(c(16621, 16621, 16623, 16623, 16622, 16622), class = "Date"),
                            AC_NAM_Actor_Name = c("Mike", "Mike", "Mikey", "Mikey", "Bob", "Bob"),
                            AC_PLV_AC_ID = c(NA, 1L, 1L, 1L, NA, 2L),
                            Metadata_AC_PLV = c(NA, 1L, 1L, 1L, NA, 1L),
                            AC_PLV_ChangedAt = structure(c(NA, 16622, 16623, 16623, NA, 16623), class = "Date"),
                            AC_PLV_PLV_ProfessionalLevel = c(NA, 4L, 5L, 5L, NA, 1L),
                            AC_PLV_Metadata_PLV = c(NA, 1L, 1L, 1L, NA, 1L),
                            AC_PLV_PLV_ID = c(NA, 1L, 4L, 4L, NA, 2L),
                            key = "AC_ID"),
                 check.attributes = FALSE,
                 info = "difference view AC + NAM + GEN + PLV, step 1")

    expect_equal(am$view("AC", type = "difference", time = c(as.Date("2015-07-06"), as.Date("2015-07-08"))),
                 data.table(inspectedTimepoint = structure(c(16622, 16623, 16623, 16622, 16623, 16624, 16624), class = "Date"),
                            mnemonic = c("PLV", "NAM", "PLV", "NAM", "PLV", "NAM", "NAM"),
                            AC_ID = c(1L, 1L, 1L, 2L, 2L, 2L, 3L),
                            Metadata_AC = c(1L, 1L, 1L, 1L, 1L, 1L, 1L),
                            AC_GEN_AC_ID = c(1L, 1L, 1L, 2L, 2L, 2L, 3L),
                            Metadata_AC_GEN = c(1L, 1L, 1L, 1L, 1L, 1L, 1L),
                            AC_GEN_GEN_Gender = c("M", "M", "M", "M", "M", "M", "F"),
                            AC_GEN_Metadata_GEN = c(1L, 1L, 1L, 1L, 1L, 1L, 1L),
                            AC_GEN_GEN_ID = c(1L, 1L, 1L, 1L, 1L, 1L, 2L),
                            AC_NAM_AC_ID = c(1L, 1L, 1L, 2L, 2L, 2L, 3L),
                            Metadata_AC_NAM = c(1L, 1L, 1L, 1L, 1L, 1L, 1L),
                            AC_NAM_ChangedAt = structure(c(16621, 16623, 16623, 16622, 16622, 16624, 16624), class = "Date"),
                            AC_NAM_Actor_Name = c("Mike", "Mikey", "Mikey", "Bob", "Bob", "Boby", "Alice"),
                            AC_PLV_AC_ID = c(1L, 1L, 1L, NA, 2L, 2L, NA),
                            Metadata_AC_PLV = c(1L, 1L, 1L, NA, 1L, 1L, NA),
                            AC_PLV_ChangedAt = structure(c(16622, 16623, 16623, NA, 16623, 16623, NA), class = "Date"),
                            AC_PLV_PLV_ProfessionalLevel = c(4L, 5L, 5L, NA, 1L, 1L, NA),
                            AC_PLV_Metadata_PLV = c(1L, 1L, 1L, NA, 1L, 1L, NA),
                            AC_PLV_PLV_ID = c(1L, 4L, 4L, NA, 2L, 2L, NA),
                            key = "AC_ID"),
                 check.attributes = FALSE,
                 info = "difference view AC + NAM + GEN + PLV, step 2")

    expect_equal(am$view("AC", type = "difference", time = c(as.Date("2015-07-06"), as.Date("2015-07-10"))),
                 data.table(inspectedTimepoint = structure(c(16622, 16623, 16623, 16622, 16623, 16624, 16626, 16624, 16625), class = "Date"),
                            mnemonic = c("PLV", "NAM", "PLV", "NAM", "PLV", "NAM", "PLV", "NAM", "PLV"),
                            AC_ID = c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L),
                            Metadata_AC = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
                            AC_GEN_AC_ID = c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L),
                            Metadata_AC_GEN = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
                            AC_GEN_GEN_Gender = c("M", "M", "M", "M", "M", "M", "M", "F", "F"),
                            AC_GEN_Metadata_GEN = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
                            AC_GEN_GEN_ID = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L),
                            AC_NAM_AC_ID = c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L),
                            Metadata_AC_NAM = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
                            AC_NAM_ChangedAt = structure(c(16621, 16623, 16623, 16622, 16622, 16624, 16624, 16624, 16624), class = "Date"),
                            AC_NAM_Actor_Name = c("Mike", "Mikey", "Mikey", "Bob", "Bob", "Boby", "Boby", "Alice", "Alice"),
                            AC_PLV_AC_ID = c(1L, 1L, 1L, NA, 2L, 2L, 2L, NA, 3L),
                            Metadata_AC_PLV = c(1L, 1L, 1L, NA, 1L, 1L, 1L, NA, 1L),
                            AC_PLV_ChangedAt = structure(c(16622, 16623, 16623, NA, 16623, 16623, 16626, NA, 16625), class = "Date"),
                            AC_PLV_PLV_ProfessionalLevel = c(4L, 5L, 5L, NA, 1L, 1L, 2L, NA, 3L),
                            AC_PLV_Metadata_PLV = c(1L, 1L, 1L, NA, 1L, 1L, 1L, NA, 1L),
                            AC_PLV_PLV_ID = c(1L, 4L, 4L, NA, 2L, 2L, 5L, NA, 3L),
                            key = "AC_ID"),
                 check.attributes = FALSE,
                 info = "difference view AC + NAM + GEN + PLV, step 3")

})

test_that("AM view shared knots", {

    am <- AM$new()
    am$create(class = "anchor", mne = "PE", desc = "Person")
    am$create(class = "anchor", mne = "ZZ", desc = "Zzz")
    am$create(class = "knot", mne = "TYP", desc = "Type")
    am$create(class = "tie", anchors = c("PE","ZZ"), knot = "TYP", roles = c("was","at","is"), identifier = c(Inf,Inf,1), hist = TRUE)
    am$create(class = "attribute", anchor = "ZZ", mne = "ABC", desc = "Abc", knot = "TYP")$run()
    am$load(mapping = list(PE = list("person_code"),
                           ZZ = list("zzz_code"),
                           PE_ZZ_TYP = list(hist = "zzz_date", knot = "zzz_type")),
            data = data.table(person_code = c(1L,1:2,2L),
                              zzz_code = c(1:2,2L,2L),
                              zzz_type = c("a","b","c","b"),
                              zzz_date = as.Date("2015-07-05")+c(0L,0L,0:1)),
            meta = 1L)
    am$load(mapping = list(ZZ = list("zzz_code",
                                     ABC = "abc")),
            data = data.table(zzz_code = c(2L,1L,3L),
                              abc = c("e","a","d")),
            meta = 2L)

    expect_equal(am$view("ZZ"),
                 data.table(ZZ_ID = 1:3,
                            Metadata_ZZ = c(1L, 1L, 2L),
                            ZZ_ABC_ZZ_ID = 1:3,
                            Metadata_ZZ_ABC = c(2L, 2L, 2L),
                            ZZ_ABC_TYP_Type = c("a", "e", "d"),
                            ZZ_ABC_Metadata_TYP = c(1L, 2L, 2L),
                            ZZ_ABC_TYP_ID = c(1L, 4L, 5L),
                            key = c("ZZ_ID")),
                 check.attributes = FALSE,
                 info = "view on shared knot to tie and attribute: view anchor")
    expect_equal(am$view("PE_was_ZZ_at_TYP_is"),
                 data.table(Metadata_PE_was_ZZ_at_TYP_is = c(1L, 1L, 1L),
                            PE_was_ZZ_at_TYP_is_ChangedAt = as.Date("2015-07-05")+c(0L,0L,1L),
                            PE_ID_was = c(1L, 1L, 2L),
                            ZZ_ID_at = c(1L, 2L, 2L),
                            is_TYP_Type = c("a", "b", "b"),
                            is_Metadata_TYP = c(1L, 1L, 1L),
                            TYP_ID_is = c(1L, 2L, 2L),
                            key = c("PE_ID_was","ZZ_ID_at","PE_was_ZZ_at_TYP_is_ChangedAt")),
                 check.attributes = FALSE,
                 info = "view on shared knot to tie and attribute: view tie")

})
