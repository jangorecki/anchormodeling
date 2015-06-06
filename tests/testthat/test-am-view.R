context("AM view method")

test_that("AM 3 main view cases", {

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

test_that("AM different view cases", {

    # source datasets
    actors <- data.table(parent_code = c(2L,NA_character_,NA_character_,NA_character_),
                         actor_code = c(1L,2L,1L,2L),
                         actor_name = c("Bob Lee","Jack Lee","Bob Lee","Jackie Lee"),
                         actor_date = as.Date("2015-06-05")+c(0L,0L,2L,4L))
    shows <- data.table(actor_code = c(1L,2L,2L,2L,1L,2L,1L),
                        program_name = paste0("show", c(1,1,2,3,4,4,5)),
                        actor_program_rating = c("Good","Very Good","Mediocre","Good","Good","Very Good","Good"),
                        rating_date = as.Date("2015-06-05")+c(0L,0L,0L,1L,2L,2L,3L))

    # prepare source to model mapping
    actors_mapping <- list(AC = list("actor_code",
                                     COD = "actor_code", # include natural key as attribute in model
                                     NAM = c("actor_name", hist = "actor_date")))
    shows_mapping <- list(AC = list("actor_code"),
                          PR = list("program_name",
                                    NAM = "program_name"), # include natural key as attribute in model
                          AC_PR_RAT = list(knot = "actor_program_rating",
                                           hist = "rating_date"))

    # build anchor model
    am <- AM$new()
    am$create(class = "anchor", mne = "PR", desc = "Program")
    am$create(class = "attribute", anchor = "PR", mne = "NAM", desc = "Name")
    am$create(class = "anchor", mne = "AC", desc = "Actor")
    am$create(class = "attribute", anchor = "AC", mne = "COD", desc = "Code")
    am$create(class = "attribute", anchor = "AC", mne = "NAM", desc = "Name", hist = TRUE)
    am$create(class = "tie", anchors = c("AC","PR"), knot = "RAT", roles = c("part","in","got"), identifier = c(Inf,Inf,1))
    am$create(class = "knot", mne = "RAT", desc = "Rating")
    am$run()

    # Define tie mapping notes:
    # anchors mne automatically map to src cols based on anchor mapping defined
    # instead of AC_PR_RAT should also accept 'AC_part_PR_in_RAT_got' as mandatory where multiple match on MNE's only

    # load data
    am$load(actors_mapping, actors)
    #     am
    #     am$load(shows_mapping, shows)
    #     am

    # difference view: step 1 confirm expected data in time
    # TO DO

})
