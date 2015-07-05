context("AM load method")

test_that("AM load - attribute tests", {

    am <- AM$new()
    am$create(class = "anchor", mne = "AC", desc = "Actor")
    am$create(class = "attribute", anchor = "AC", mne = "NAM", desc = "Name")
    am$run()

    # initial loading data, auto IM
    am$load(mapping = list(AC = list("code", NAM = "name")),
            data = data.table(code = 1L, name = "Bob"),
            meta = 1L)

    # evolve model 1: add knotted attribute
    am$create(class = "attribute", anchor = "AC", mne = "GEN", desc = "Gender", knot = "GEN")
    am$create(class = "knot", mne = "GEN", desc = "Gender")
    am$run()
    am$load(mapping = list(AC = list("code", NAM = "name", GEN = "gender")),
            data = data.table(code = 2L, name = "Alice", gender = "F"),
            meta = 2L)
    expect_equal(am$OBJ("GEN")$data, data.table(GEN_ID = 1L, GEN_Gender = "F", Metadata_GEN = 2L, key = "GEN_ID"), info = "static knotted attr as expected")

    # incremental load
    am$load(mapping = list(AC = list("code", NAM = "name", GEN = "gender")),
            data = data.table(code = 1:2, name = c("Bob","Alice"), gender = c("M","F")),
            meta = 3L)
    expect_equal(am$OBJ("GEN")$data, data.table(GEN_ID = 1:2, GEN_Gender = c("F","M"), Metadata_GEN = 2:3, key = "GEN_ID"), info = "incremental load, static knotted attr as expected")

    # evolve model 2: shared knot of attribute
    am$create(class = "attribute", anchor = "AC", mne = "RAL", desc = "LowerRating", knot = "RAT")
    am$create(class = "attribute", anchor = "AC", mne = "RAH", desc = "HigherRating", knot = "RAT")
    am$create(class = "knot", mne = "RAT", desc = "Rating")
    am$run()
    am$load(mapping = list(AC = list("code", RAH = "ratingH", RAL = "ratingL")),
            data = data.table(code = 1:2, ratingH = c("Good","VeryGood"), ratingL = c("Bad","Medicore")),
            meta = 4L)
    expect_equal(am$process()[code=="RAT", .(meta, in_nrow, unq_nrow, load_nrow)], data.table(meta = 4L, in_nrow = 4L, unq_nrow = 4L, load_nrow = 4L), info = "expected in_nrow, unq_nrow and load_nrow for shared knot of attributes")
    expect_equal(am$OBJ("RAT")$data, data.table(RAT_ID = 1:4, RAT_Rating = c("Good","VeryGood","Bad","Medicore"), Metadata_RAT = rep(4L,4L), key = "RAT_ID"), info = "static shared knot knotted attr as expected")

    # incremetal load
    am$load(mapping = list(AC = list("code", RAH = "ratingH", RAL = "ratingL")),
            data = data.table(code = 3:4, ratingH = c("Good","Great"), ratingL = c("Bad","Bad")), # one new, one the same
            meta = 5L)
    expect_equal(am$process()[code=="RAT", .(meta, in_nrow, unq_nrow, load_nrow)], data.table(meta = 5L, in_nrow = 4L, unq_nrow = 3L, load_nrow = 1L), info = "incremental load, expected in_nrow, unq_nrow and load_nrow for shared knot of attributes")
    expect_equal(am$OBJ("RAT")$data, data.table(RAT_ID = 1:5, RAT_Rating = c("Good","VeryGood","Bad","Medicore","Great"), Metadata_RAT = c(rep(4L,4L),5L), key = "RAT_ID"), info = "incremental load, static shared knot knotted attr as expected")

    # evolve model 3: historized attribute
    am$create(class = "attribute", anchor = "AC", mne = "HAC", desc = "HairColor", hist = TRUE, rest = TRUE)
    am$run()
    am$load(mapping = list(AC = list("code", HAC = c("hair", hist = "hair_date"))),
            data = data.table(code = c(1:2,1L,3L), hair = c("black","red","white","blue"), hair_date = as.Date("2015-06-07")+c(0L,0L,1L,1L)),
            meta = 6L)
    expect_equal(am$OBJ("AC_HAC")$query(),
                 data.table(AC_HAC_AC_ID = c(1L, 1L, 2L, 3L),
                            AC_HAC_Actor_HairColor = c("black", "white", "red", "blue"),
                            AC_HAC_ChangedAt = structure(c(16593, 16594, 16593, 16594), class = "Date"),
                            Metadata_AC_HAC = c(6L, 6L, 6L, 6L),
                            key = c("AC_HAC_AC_ID","AC_HAC_ChangedAt")),
                 info = "historize attribute 1st load")

    # incremental
    am$load(mapping = list(AC = list("code", HAC = c("hair", hist = "hair_date"))),
            data = data.table(code = c(1L,3L), hair = c("white","blue"), hair_date = as.Date("2015-06-07")+c(2L,-1L)), # both sides restatments
            meta = 7L)
    expect_equal(am$OBJ("AC_HAC")$query(),
                 data.table(AC_HAC_AC_ID = c(1L, 1L, 1L, 2L, 3L, 3L),
                            AC_HAC_Actor_HairColor = c("black", "white", "white", "red", "blue", "blue"),
                            AC_HAC_ChangedAt = structure(c(16593, 16594, 16595, 16593, 16592, 16594), class = "Date"),
                            Metadata_AC_HAC = c(6L, 6L, 7L, 6L, 7L, 6L),
                            key = c("AC_HAC_AC_ID","AC_HAC_ChangedAt")),
                 info = "historize attribute 2st load, both sides restatements")

    # evolve model 4: historized knotted attribute
    am$create(class = "attribute", anchor = "AC", mne = "HC2", desc = "HairColor", hist = TRUE, knot = "COL") # same logical attribute but this time knotted
    am$create(class = "knot", mne = "COL", desc = "Color")
    am$run()
    am$load(mapping = list(AC = list("code", HC2 = c("hair", hist = "hair_date"))),
            data = data.table(code = c(1:2,1L,3L), hair = c("black","red","white","blue"), hair_date = as.Date("2015-06-07")+c(0L,0L,1L,1L)),
            meta = 8L)
    expect_equal(am$OBJ("AC_HC2")$query(),
                 data.table(AC_HC2_AC_ID = c(1L, 1L, 2L, 3L),
                            AC_HC2_COL_ID = c(1L, 3L, 2L, 4L),
                            AC_HC2_ChangedAt = structure(c(16593, 16594, 16593, 16594), class = "Date"),
                            Metadata_AC_HC2 = c(8L, 8L, 8L, 8L),
                            key = c("AC_HC2_AC_ID","AC_HC2_ChangedAt")),
                 info = "knotted historized attribute 1st load")

    # incremental
    am$load(mapping = list(AC = list("code", HC2 = c("hair", hist = "hair_date"))),
            data = data.table(code = c(1L,3L), hair = c("white","blue"), hair_date = as.Date("2015-06-07")+c(2L,-1L)), # both sides restatments
            meta = 9L)
    expect_equal(am$OBJ("AC_HC2")$query(),
                 data.table(AC_HC2_AC_ID = c(1L, 1L, 1L, 2L, 3L, 3L),
                            AC_HC2_COL_ID = c(1L, 3L, 3L, 2L, 4L, 4L),
                            AC_HC2_ChangedAt = structure(c(16593, 16594, 16595, 16593, 16592, 16594), class = "Date"),
                            Metadata_AC_HC2 = c(8L, 8L, 9L, 8L, 9L, 8L),
                            key = c("AC_HC2_AC_ID","AC_HC2_ChangedAt")),
                 info = "knotted historized attribute 2st load, both sides restatements")

    # evolve model 5: (historized attribute + static attribute) to single shared knot
    am$create(class = "attribute", anchor = "AC", mne = "MIN", desc = "Minimum", knot = "UTL")
    am$create(class = "attribute", anchor = "AC", mne = "AVG", desc = "Average", hist = TRUE, knot = "UTL")
    am$create(class = "knot", mne = "UTL", desc = "Utilization")
    am$run()
    am$load(mapping = list(AC = list("code", MIN = "minimum", AVG = c("average", hist = "average_date"))),
            data = data.table(code = 1:2, minimum = c("2","2+"), average = c("2+","4+"), average_date = rep(as.Date("2015-06-08"),2)),
            meta = 10L)
    expect_equal(am$OBJ("AC_MIN")$query(),
                 data.table(AC_MIN_AC_ID = 1:2,
                            AC_MIN_UTL_ID = c(3L, 1L),
                            Metadata_AC_MIN = c(10L, 10L),
                            key = "AC_MIN_AC_ID"),
                 info = "shared knot to one hist attr and one non-hist attr, first load: non-hist attr")
    expect_equal(am$OBJ("AC_AVG")$query(),
                 data.table(AC_AVG_AC_ID = 1:2,
                            AC_AVG_UTL_ID = 1:2,
                            AC_AVG_ChangedAt = structure(c(16594, 16594), class = "Date"),
                            Metadata_AC_AVG = c(10L, 10L),
                            key = c("AC_AVG_AC_ID","AC_AVG_ChangedAt")),
                 info = "shared knot to one hist attr and one non-hist attr, first load: hist attr")
    expect_equal(am$OBJ("UTL")$query(),
                 data.table(UTL_ID = 1:3,
                            UTL_Utilization = c("2+", "4+", "2"),
                            Metadata_UTL = c(10L, 10L, 10L),
                            key = "UTL_ID"),
                 info = "shared knot to one hist attr and one non-hist attr, first load: knot")

    # incremetal load
    am$load(mapping = list(AC = list("code", MIN = "minimum", AVG = c("average", hist = "average_date"))),
          data = data.table(code = 1L, minimum = "2", average = "3+", average_date = as.Date("2015-06-08")+1L),
          meta = 11L)
    expect_equal(am$OBJ("AC_MIN")$query(),
                 data.table(AC_MIN_AC_ID = 1:2,
                            AC_MIN_UTL_ID = c(3L, 1L),
                            Metadata_AC_MIN = c(10L, 10L),
                            key = "AC_MIN_AC_ID"),
                 info = "shared knot to one hist attr and one non-hist attr, incremetal load: non-hist attr")
    expect_equal(am$OBJ("AC_AVG")$query(),
                 data.table(AC_AVG_AC_ID = c(1L,1:2),
                            AC_AVG_UTL_ID = c(1L,4L,2L),
                            AC_AVG_ChangedAt = structure(c(16594, 16595, 16594), class = "Date"),
                            Metadata_AC_AVG = c(10L, 11L, 10L),
                            key = c("AC_AVG_AC_ID","AC_AVG_ChangedAt")),
                 info = "shared knot to one hist attr and one non-hist attr, incremetal load: hist attr")
    expect_equal(am$OBJ("UTL")$query(),
                 data.table(UTL_ID = 1:4,
                            UTL_Utilization = c("2+", "4+", "2", "3+"),
                            Metadata_UTL = c(10L, 10L, 10L, 11L),
                            key = "UTL_ID"),
                 info = "shared knot to one hist attr and one non-hist attr, incremetal load: knot")

    # incremetal loads only first of two attrs
    am$load(mapping = list(AC = list("code", MIN = "minimum")),
            data = data.table(code = 3L, minimum = "4"), # new
            meta = 12L)
    expect_equal(am$OBJ("AC_MIN")$query(),
                 data.table(AC_MIN_AC_ID = 1:3,
                            AC_MIN_UTL_ID = c(3L, 1L, 5L),
                            Metadata_AC_MIN = c(10L, 10L, 12L),
                            key = "AC_MIN_AC_ID"),
                 info = "shared knot to one non-hist attr, incremetal partial load: non-hist attr")
    expect_equal(am$OBJ("UTL")$query(),
                 data.table(UTL_ID = 1:5,
                            UTL_Utilization = c("2+", "4+", "2", "3+", "4"),
                            Metadata_UTL = c(10L, 10L, 10L, 11L, 12L),
                            key = "UTL_ID"),
                 info = "shared knot to one non-hist attr, incremetal partial load: knot")

    # incremetal loads only second of two attrs
    am$load(mapping = list(AC = list("code", AVG = c("average", hist = "average_date"))),
           data = data.table(code = 2L, average = "5", average_date = as.Date("2015-06-08")+1L),
           meta = 13L)
    expect_equal(am$OBJ("AC_AVG")$query(),
                 data.table(AC_AVG_AC_ID = c(1L,1:2,2L),
                            AC_AVG_UTL_ID = c(1L,4L,2L,6L),
                            AC_AVG_ChangedAt = structure(c(16594, 16595, 16594, 16595), class = "Date"),
                            Metadata_AC_AVG = c(10L, 11L, 10L, 13L),
                            key = c("AC_AVG_AC_ID","AC_AVG_ChangedAt")),
                 info = "shared knot to one hist attr, incremetal load: hist attr")
    expect_equal(am$OBJ("UTL")$query(),
                 data.table(UTL_ID = 1:6,
                            UTL_Utilization = c("2+", "4+", "2", "3+", "4", "5"),
                            Metadata_UTL = c(10L, 10L, 10L, 11L, 12L, 13L),
                            key = "UTL_ID"),
                 info = "shared knot to one hist attr, incremetal partial load: knot")

    # same but incrementally changed knot from regular to shared
    am$delete(c("UTL","AC_MIN","AC_AVG"))
    am$create(class = "attribute", anchor = "AC", mne = "AVG", desc = "Average", hist = TRUE, knot = "UTL")
    am$create(class = "knot", mne = "UTL", desc = "Utilization")
    am$run()
    am$load(mapping = list(AC = list("code", AVG = c("average", hist = "average_date"))),
            data = data.table(code = 1:2, minimum = c("2","2+"), average = c("2+","4+"), average_date = rep(as.Date("2015-06-08"),2)),
            meta = 14L)
    expect_equal(am$OBJ("AC_AVG")$query(),
                 data.table(AC_AVG_AC_ID = 1:2,
                            AC_AVG_UTL_ID = 1:2,
                            AC_AVG_ChangedAt = structure(c(16594, 16594), class = "Date"),
                            Metadata_AC_AVG = c(14L, 14L),
                            key = c("AC_AVG_AC_ID","AC_AVG_ChangedAt")),
                 info = "shared knot to one hist attr and one non-hist attr incrementally, first load: hist attr")
    expect_equal(am$OBJ("UTL")$query(),
                 data.table(UTL_ID = 1:2,
                            UTL_Utilization = c("2+", "4+"),
                            Metadata_UTL = c(14L, 14L),
                            key = "UTL_ID"),
                 info = "shared knot to one hist attr and one non-hist attr incrementally, first load: knot")

    # increment model to add attr making knot shared
    am$create(class = "attribute", anchor = "AC", mne = "MIN", desc = "Minimum", knot = "UTL")
    am$run()
    am$load(mapping = list(AC = list("code", MIN = "minimum", AVG = c("average", hist = "average_date"))),
            data = data.table(code = 1:2, minimum = c("2","2+"), average = c("2+","4+"), average_date = as.Date("2015-06-08")+0:1), # one update
            meta = 15L)
    expect_equal(am$OBJ("AC_MIN")$query(),
                 data.table(AC_MIN_AC_ID = 1:2,
                            AC_MIN_UTL_ID = c(3L, 1L),
                            Metadata_AC_MIN = c(15L, 15L),
                            key = "AC_MIN_AC_ID"),
                 info = "shared knot to one hist attr and one non-hist attr incrementally, first load in second load batch: non-hist attr")
    expect_equal(am$OBJ("AC_AVG")$query(),
                 data.table(AC_AVG_AC_ID = c(1:2,2L),
                            AC_AVG_UTL_ID = c(1:2,2L),
                            AC_AVG_ChangedAt = structure(c(16594, 16594, 16595), class = "Date"),  # one update
                            Metadata_AC_AVG = c(14L, 14L, 15L),
                            key = c("AC_AVG_AC_ID","AC_AVG_ChangedAt")),
                 info = "shared knot to one hist attr and one non-hist attr incrementally, second load: hist attr")
    expect_equal(am$OBJ("UTL")$query(),
                 data.table(UTL_ID = 1:3,
                            UTL_Utilization = c("2+", "4+", "2"),
                            Metadata_UTL = c(14L, 14L, 15L),
                            key = "UTL_ID"),
                 info = "shared knot to one hist attr and one non-hist attr incrementally, second load: knot")

})

test_that("AM load - ties tests", {

    am <- AM$new()
    am$create(class = "anchor", mne = "PR", desc = "Program")
    am$create(class = "anchor", mne = "PE", desc = "Performance")
    am$create(class = "tie", anchors = c("PE","PR"), roles = c("at","wasPlayed"), identifier = c(1,Inf))
    am$run()
    am$load(mapping = list(PE = list("perf_code"),
                           PR = list("prog_code"),
                           PE_PR = list()),
            data = data.table(perf_code = 1L, prog_code = c(1L,50L)),
            meta = 1L)
    expect_equal(am$OBJ("PE_at_PR_wasPlayed")$data,
                 data.table(PE_ID_at = rep(1L,2), PR_ID_wasPlayed = 1:2, Metadata_PE_at_PR_wasPlayed = rep(1L,2),
                            key = c("PE_ID_at","PR_ID_wasPlayed")),
                 info = "static tie as expected")

    # evolve model: new static tie
    am$create(class = "anchor", mne = "AC", desc = "Actor")
    am$create(class = "tie", anchors = c("AC","PE"), roles = c("wasCasted","in"), identifier = c(Inf,Inf))
    am$run()
    am$load(mapping = list(PE = list("perf_code"),
                           AC = list("acto_code"),
                           AC_PE = list()),
            data = data.table(perf_code = c(1:2,2L), acto_code = c(1L,1:2)),
            meta = 2L)
    expect_equal(am$OBJ("AC_wasCasted_PE_in")$data,
                 data.table(AC_ID_wasCasted = c(1:2,2L), PE_ID_in = c(1L,1:2), Metadata_AC_wasCasted_PE_in = rep(2L,3),
                            key = c("AC_ID_wasCasted","PE_ID_in")),
                 info = "static tie evolution as expected")

    # evolve model: new historized tie
    am$create(class = "anchor", mne = "ST", desc = "Stage")
    am$create(class = "tie", anchors = c("PR","ST"), roles = c("isPlayed","at"), identifier = c(Inf,Inf), hist = TRUE)
    am$run()
    am$load(mapping = list(PR = list("prog_code"),
                           ST = list("stag_code"),
                           PR_ST = list(hist = "date")),
            data = data.table(prog_code = c(1:2,3L,3L), stag_code = c(1:2,2L,2L), date = as.Date("2015-07-03")+c(0:1,0:1)),
            meta = 3L)
    expect_equal(am$OBJ("PR_isPlayed_ST_at")$data,
                 data.table(PR_ID_isPlayed = c(1L,3:4,4L), ST_ID_at = c(1:2,2L,2L), PR_isPlayed_ST_at_ChangedAt = as.Date("2015-07-03")+c(0:1,0:1), Metadata_PR_isPlayed_ST_at = rep(3L,4),
                            key = c("PR_ID_isPlayed","ST_ID_at","PR_isPlayed_ST_at_ChangedAt")),
                 info = "historized tie evolution as expected")

    # evolve model: new knotted historized tie
    am$create(class = "knot", mne = "RAT", desc = "Rating")
    am$create(class = "tie", anchors = c("AC","PR"), knot = "RAT", roles = c("part","in","got"), identifier = c(Inf,Inf,1), hist = TRUE)
    am$run()
    am$load(mapping = list(PR = list("prog_code"),
                           AC = list("acto_code"),
                           AC_PR_RAT = list(hist = "date", knot = "score")),
            data = data.table(prog_code = c(1:2,3L,3L), acto_code = c(1:2,2L,2L), score = c("A","D","E","D"), date = as.Date("2015-07-03")+c(0:1,0:1)),
            meta = 4L)
    expect_equal(am$OBJ("AC_part_PR_in_RAT_got")$data,
                 data.table(AC_ID_part = c(1L,3:4,4L), PR_ID_in = c(1:2,2L,2L), RAT_ID_got = c(1L,2L,3L,2L), AC_part_PR_in_RAT_got_ChangedAt = as.Date("2015-07-03")+c(0:1,0:1), Metadata_AC_part_PR_in_RAT_got = rep(4L,4),
                            key = c("AC_ID_part","PR_ID_in","AC_part_PR_in_RAT_got_ChangedAt")),
                 info = "knotted historized tie evolution as expected")

    # evolve model: new static knotted tie
    am$create(class = "knot", mne = "FIP", desc = "FirstPlay")
    am$create(class = "tie", anchors = c("AC","ST"), knot = "FIP", roles = c("firstPlayed","at","on"), identifier = c(Inf,Inf,1))
    am$run()
    am$load(mapping = list(ST = list("stag_code"),
                           AC = list("acto_code"),
                           AC_ST_FIP = list(knot = "date")),
            data = data.table(stag_code = c(1:2,2:3), acto_code = c(1L,1:2,2L), date = as.Date("2015-07-03")+c(0:1,0:1)),
            meta = 5L)
    expect_equal(am$OBJ("AC_firstPlayed_ST_at_FIP_on")$data,
                 data.table(AC_ID_firstPlayed = c(1:2,2:3), ST_ID_at = c(1L,1:2,2L), FIP_ID_on = c(1:2,1:2), Metadata_AC_firstPlayed_ST_at_FIP_on = rep(5L,4),
                            key = c("AC_ID_firstPlayed","ST_ID_at")),
                 info = "knotted static tie evolution as expected")

    # evolve model: 2 historized ties to single shared knot
    am <- AM$new()
    am$create(class = "anchor", mne = "PE", desc = "Person")
    am$create(class = "anchor", mne = "RA", desc = "Rel1")
    am$create(class = "anchor", mne = "RB", desc = "Rel2")
    am$create(class = "knot", mne = "UTL", desc = "Utilization")
    am$create(class = "tie", anchors = c("PE","RA"), knot = "UTL", roles = c("was","at","gets"), identifier = c(Inf,Inf,1), hist = TRUE)
    am$create(class = "tie", anchors = c("PE","RB"), knot = "UTL", roles = c("was","at","gets"), identifier = c(Inf,Inf,1), hist = TRUE)
    am$run()
    am$load(mapping = list(PE = list("person_code"),
                           RA = list("rel1_code"),
                           RB = list("rel2_code"),
                           PE_RA_UTL = list(hist = "rel1_date", knot = "rel1_rate"),
                           PE_RB_UTL = list(hist = "rel2_date", knot = "rel2_rate")),
            data = data.table(person_code = c(1L,1:2,2L),
                              rel1_code = c(1:2,2L,2L),
                              rel1_rate = c("2","2+","2","3-"),
                              rel1_date = as.Date("2015-07-05")+c(0L,0L,0:1),
                              rel2_code = c(1:2,2L,2L),
                              rel2_rate = c("4","2+","3","2+"),
                              rel2_date = as.Date("2015-07-05")+c(0L,0L,0:1)),
            meta = 1L)
    expect_equal(am$OBJ("PE_was_RA_at_UTL_gets")$query(),
                 data.table(PE_ID_was = c(1L,1:2,2L),
                            RA_ID_at = c(1:2,2L,2L),
                            UTL_ID_gets = c(1:2,1L,3L),
                            PE_was_RA_at_UTL_gets_ChangedAt = as.Date("2015-07-05")+c(0L,0L,0:1),
                            Metadata_PE_was_RA_at_UTL_gets = rep(1L,4),
                            key = c("PE_ID_was","RA_ID_at","PE_was_RA_at_UTL_gets_ChangedAt")),
                 info = "shared knot to two hist ties: first tie")
    expect_equal(am$OBJ("PE_was_RB_at_UTL_gets")$query(),
                 data.table(PE_ID_was = c(1L,1:2,2L),
                            RB_ID_at = c(1:2,2L,2L),
                            UTL_ID_gets = c(4L,2L,5L,2L),
                            PE_was_RB_at_UTL_gets_ChangedAt = as.Date("2015-07-05")+c(0L,0L,0:1),
                            Metadata_PE_was_RB_at_UTL_gets = rep(1L,4),
                            key = c("PE_ID_was","RB_ID_at","PE_was_RB_at_UTL_gets_ChangedAt")),
                 info = "shared knot to two hist ties: second tie")
    expect_equal(am$OBJ("UTL")$query(),
                 data.table(UTL_ID = 1:5,
                            UTL_Utilization = c("2","2+","3-","4","3"),
                            Metadata_UTL = rep(1L,5),
                            key = "UTL_ID"),
                 info = "shared knot to two hist ties: knot")

    # same but incrementally changed knot from regular to shared
    am <- AM$new()
    am$create(class = "anchor", mne = "PE", desc = "Person")
    am$create(class = "anchor", mne = "RA", desc = "Rel1")
    am$create(class = "knot", mne = "UTL", desc = "Utilization")
    am$create(class = "tie", anchors = c("PE","RA"), knot = "UTL", roles = c("was","at","gets"), identifier = c(Inf,Inf,1), hist = TRUE)
    am$run()
    am$load(mapping = list(PE = list("person_code"),
                           RA = list("rel1_code"),
                           PE_RA_UTL = list(hist = "rel1_date", knot = "rel1_rate")),
            data = data.table(person_code = c(1L,1:2,2L),
                              rel1_code = c(1:2,2L,2L),
                              rel1_rate = c("2","2+","2","3-"),
                              rel1_date = as.Date("2015-07-05")+c(0L,0L,0:1)),
            meta = 1L)
    expect_equal(am$OBJ("PE_was_RA_at_UTL_gets")$query(),
                 data.table(PE_ID_was = c(1L,1:2,2L),
                            RA_ID_at = c(1:2,2L,2L),
                            UTL_ID_gets = c(1:2,1L,3L),
                            PE_was_RA_at_UTL_gets_ChangedAt = as.Date("2015-07-05")+c(0L,0L,0:1),
                            Metadata_PE_was_RA_at_UTL_gets = rep(1L,4),
                            key = c("PE_ID_was","RA_ID_at","PE_was_RA_at_UTL_gets_ChangedAt")),
                 info = "dynamically shared knot to two hist ties: first load, first tie")
    expect_equal(am$OBJ("UTL")$query(),
                 data.table(UTL_ID = 1:3,
                            UTL_Utilization = c("2","2+","3-"),
                            Metadata_UTL = rep(1L,3),
                            key = "UTL_ID"),
                 info = "dynamically shared knot to two hist ties: first load, knot")

    # increment model to add tie making knot shared
    am$create(class = "anchor", mne = "RB", desc = "Rel2")
    am$create(class = "tie", anchors = c("PE","RB"), knot = "UTL", roles = c("was","at","gets"), identifier = c(Inf,Inf,1), hist = TRUE)
    am$run()
    am$load(mapping = list(PE = list("person_code"),
                           RB = list("rel2_code"),
                           PE_RB_UTL = list(hist = "rel2_date", knot = "rel2_rate")),
            data = data.table(person_code = c(1L,1:2,2L),
                              rel2_code = c(1:2,2L,2L),
                              rel2_rate = c("4","2+","3","2+"),
                              rel2_date = as.Date("2015-07-05")+c(0L,0L,0:1)),
            meta = 2L)
    expect_equal(am$OBJ("PE_was_RB_at_UTL_gets")$query(),
                 data.table(PE_ID_was = c(1L,1:2,2L),
                            RB_ID_at = c(1:2,2L,2L),
                            UTL_ID_gets = c(4L,2L,5L,2L),
                            PE_was_RB_at_UTL_gets_ChangedAt = as.Date("2015-07-05")+c(0L,0L,0:1),
                            Metadata_PE_was_RB_at_UTL_gets = rep(2L,4),
                            key = c("PE_ID_was","RB_ID_at","PE_was_RB_at_UTL_gets_ChangedAt")),
                 info = "dynamically shared knot to two hist ties: second load, second tie")
    expect_equal(am$OBJ("UTL")$query(),
                 data.table(UTL_ID = 1:5,
                            UTL_Utilization = c("2","2+","3-","4","3"),
                            Metadata_UTL = c(1L,1L,1:2,2L),
                            key = "UTL_ID"),
                 info = "dynamically shared knot to two hist ties: second load, knot")

    # evolve model: new static knotted self tie - as AC_parent_AC_child_PAT_parentalType from example model
    # am$create(class = "tie", anchors = c("AC","AC"), knot = "PAT", roles = c("parent","child","parentalType"), identifier = c(Inf,Inf,Inf))
    # am$run()
    # am$load(mapping = list(AC = list("acto_code"),
    #                        AC_AC = list(knot = "")),
    #         data = data.table(perf_code = c(1:2,2L), acto_code = c(1L,1:2)),
    #         meta = 5L)
    # expect_equal(am$OBJ("AC_parent_AC_child_PAT_parentalType")$data,
    #              data.table(AC_ID_parent = c(1:2,2L), AC_ID_child = c(1L,1:2), Metadata_AC_parent_AC_child_PAT_parentalType = rep(5L,3),
    #                         key = c("AC_ID_parent","AC_ID_child")),
    #              info = "static knotted self tie evolution as expected")

})

test_that("AM load - restatements", {

    skip("restatements in dev")

    # NO restatement hist=FALSE
    # NO restatement hist=TRUE
    # YES restatement hist=FALSE
    # YES restatement hist=TRUE

    # attribute

    am <- AM$new()
    am$create(class = "anchor", mne = "AC", desc = "Actor")
    am$create(class = "attribute", anchor = "AC", mne = "NAM", desc = "Name", hist = TRUE, rest = FALSE)
    am$run()

    # initial loading hist data
    am$load(mapping = list(AC = list("code", NAM = c("name", hist = "date"))),
            data = data.table(code = 1L, name = "Mike", date = as.Date("2015-01-01")),
            meta = 1L)
    expect_identical(am$process()$rows, rep(1L,2L), info = "loaded first hist row")
    # incremental loading new hist data
    am$load(mapping = list(AC = list("code", NAM = c("name", hist = "date"))),
            data = data.table(code = 1L, name = "Mikey", date = as.Date("2015-01-05")),
            meta = 2L)
    expect_equal(am$process()[order(class), .(rows, in_nrow, unq_nrow, load_nrow)], data.table(rows = 1:2, in_nrow = c(1L,1L), unq_nrow = c(1L,1L), load_nrow = c(0L,1L)), info = "loaded second hist row")
    # incremental loading same existing historized data - should not be loaded, but no error
    am$load(mapping = list(AC = list("code", NAM = c("name", hist = "date"))),
            data = data.table(code = 1L, name = "Mikey", date = as.Date("2015-01-05")),
            meta = 3L)
    expect_equal(am$OBJ("AC_NAM")$data,
                 data.table(AC_NAM_AC_ID = rep(1L,2),
                            AC_NAM_Actor_Name = c("Mike","Mikey"),
                            AC_NAM_ChangedAt = c(as.Date("2015-01-01"), as.Date("2015-01-05")),
                            Metadata_AC_NAM = 1:2,
                            key = c("AC_NAM_AC_ID","AC_NAM_ChangedAt")),
                 info = "historized duplicates on PK silently ignored")

    # incremental loading new hist data but with same value as previous/next value - because of restatement = FALSE they should not be loaded

    # tie

    am$create(class = "anchor", mne = "PR", desc = "Program")
    am$create(class = "tie", anchors = c("AC","PR"), roles = c("part","in"), identifier = c(Inf,Inf,1), knot = "RAT", rest = FALSE)
    am$create(class = "knot", mne = "RAT", desc = "Rating")
    am$run()

})

test_that("AM load - auto IM", {

    am <- AM$new()
    am$create(class = "anchor", mne = "PR", desc = "Program")
    am$create(class = "attribute", anchor = "PR", mne = "NAM", desc = "Name")
    am$run()

    # initial loading data, auto IM
    am$load(mapping = list(PR = list("code", NAM = "name")),
            data = data.table(code = 1L, name = "my program"),
            meta = 1L)
    expect_identical(am$process()$rows, rep(1L,2L), info = "loaded first row")
    expect_equal(am$IM()$ID$PR, data.table(code = 1L, PR_ID = 1L, key = "code"), info = "auto IM after first insert")
    # incremental loading new data, auto IM
    am$load(mapping = list(PR = list("code", NAM = "name")),
            data = data.table(code = 50L, name = "my another program"),
            meta = 2L)
    expect_identical(am$process()$rows, rep(2L,2L), info = "loaded second row")
    expect_equal(am$IM()$ID$PR, data.table(code = c(1L,50L), PR_ID = 1:2, key = "code"), info = "auto IM after second insert")
    # incremental loading same existing non historized data - should not be loaded
    am$load(mapping = list(PR = list("code", NAM = "name")),
            data = data.table(code = 50L, name = "my another program"),
            meta = 3L)
    expect_identical(am$process()$rows, rep(2L,2L), info = "loaded second row twice")
    expect_equal(am$IM()$ID$PR, data.table(code = c(1L,50L), PR_ID = 1:2, key = "code"), info = "auto IM after second insert twice")

})

test_that("multiple AM instances loading including separation of IM instances", {

    am1 <- AM$new()
    am1$create(class = "anchor", mne = "PE", desc = "Person")
    am1$create(class = "attribute", anchor = "PE", mne = "NAM", desc = "Name")
    am1$run()

    am2 <- AM$new()
    am2$create(class = "anchor", mne = "PE", desc = "Performance")
    am2$create(class = "attribute", anchor = "PE", mne = "NAM", desc = "Name")
    am2$run()

    am1$load(mapping = list(PE = list("code", NAM = "name")),
             data = data.table(code = 1L, name = "my person"),
             meta = 1L)
    am2$load(mapping = list(PE = list("code", NAM = "name")),
             data = data.table(code = 5L, name = "my program"),
             meta = 1L)
    expect_identical(am1$process()$rows, rep(1L,2L), info = "multiple AM instances insert 1 row, am1")
    expect_identical(am2$process()$rows, rep(1L,2L), info = "multiple AM instances insert 1 row, am2")
    expect_equal(am1$IM()$ID$PE, data.table(code = 1L, PE_ID = 1, key = "code"), info = "multiple AM instances auto IM insert 1 row, am1")
    expect_equal(am2$IM()$ID$PE, data.table(code = 5L, PE_ID = 1, key = "code"), info = "multiple AM instances auto IM insert 1 row, am2")

})

test_that("AM load - technical exceptions scenarios", {

    # loading only anchor - no attributes etc
    am <- AM$new()
    am$create(class = "anchor", mne = "AC", desc = "Actor")
    am$run()
    am$load(mapping = list(AC = list("code")),
            data = data.table(code = 1L),
            meta = 1L)
    expect_equal(am$OBJ("AC")$data, data.table(AC_ID = 1L, Metadata_AC = 1L, key ="AC_ID"), info = "loading of anchor only")
    # lack of hist
    # lack of knot
    # misspelled col name
    # misspelled attr
    # misspelled anchor
    # col name invalid data type
    # hist invalid data type
    # knot invalid data type
    # anchor invalid data type (non-list)
    # check against defined naming convention for AM instance

})

test_that("AM load - data model validation failures", {

    am <- AM$new()
    am$create(class = "anchor", mne = "PR", desc = "Program")
    am$create(class = "attribute", anchor = "PR", mne = "NAM", desc = "Name")
    am$run()

    # incremental loading new version of static attribute - ERROR
    am$load(mapping = list(PR = list("code", NAM = "name")),
            data = data.table(code = 50L, name = "my program"),
            meta = 1L)
    expect_error(am$load(mapping = list(PR = list("code", NAM = "name")),
                         data = data.table(code = 50L, name = "my program new name"),
                         meta = 2L),
                 "Duplicate key violates defined model. You are trying to insert different value into PR_NAM_Program_Name for same existing identity. If you want want to have multiple values for that identity you should historize that attribute.",
                 info = "incremental loading new version of static attribute should produce ERROR")
    # sql ref:
    #     -- PR_NAM data model identity violation: static attribute with different value for same identity
    #     INSERT INTO dbo.lpr_program (pr_id, metadata_pr, pr_nam_pr_id, metadata_pr_nam, pr_nam_program_name) VALUES (1,1,1,1,'My program');
    #     INSERT INTO dbo.lpr_program (pr_id, metadata_pr, pr_nam_pr_id, metadata_pr_nam, pr_nam_program_name) VALUES (2,2,2,2,'My program2');
    #     INSERT INTO dbo.lpr_program (pr_id, metadata_pr, pr_nam_pr_id, metadata_pr_nam, pr_nam_program_name) VALUES (1,1,1,1,'My program1');
    #     --ERROR:  duplicate key value violates unique constraint "pkpr_nam_program_name"
    #     --DETAIL:  Key (pr_nam_pr_id)=(1) already exists.

    # missing hist to historized tie
    am <- AM$new()
    am$create(class = "anchor", mne = "PR", desc = "Program")
    am$create(class = "anchor", mne = "ST", desc = "Stage")
    am$create(class = "tie", anchors = c("PR","ST"), roles = c("isPlayed","at"), identifier = c(Inf,Inf), hist = TRUE)
    am$run()
    load_call <- quote(am$load(mapping = list(PR = list("prog_code"),
                                              ST = list("stag_code"),
                                              PR_ST = list()),
                               data = data.table(prog_code = c(1:2,3L,3L), stag_code = c(1:2,2L,2L), date = as.Date("2015-07-03")+c(0:1,0:1)),
                               meta = 1L))
    expect_error(eval(load_call), "Some of the provided ties have incorrect definition versus model: PR_isPlayed_ST_at. Check if they are not missing `hist` column when defined in model as historized.", info = "missing hist to historized tie makes good error")

    # missing knot to knotted historized tie
    am$create(class = "anchor", mne = "AC", desc = "Actor")
    am$create(class = "knot", mne = "RAT", desc = "Rating")
    am$create(class = "tie", anchors = c("AC","PR"), knot = "RAT", roles = c("part","in","got"), identifier = c(Inf,Inf,1), hist = TRUE)
    am$run()
    load_call <- quote(am$load(mapping = list(PR = list("prog_code"),
                                              AC = list("acto_code"),
                                              AC_PR = list(hist = "date", knot = "score")),
                               data = data.table(prog_code = c(1:2,3L,3L), acto_code = c(1:2,2L,2L), score = c("A","D","E","D"), date = as.Date("2015-07-03")+c(0:1,0:1)),
                               meta = 2L))
    expect_error(eval(load_call), "Following short code of tie was not able to map to any tie: AC_PR. See am print method for defined entities and provide tie code.", info = "missing knot to knotted historized tie makes good error")

})
