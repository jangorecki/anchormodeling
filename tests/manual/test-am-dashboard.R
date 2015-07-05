library(anchormodeling)

# model -------------------------------------------------------------------

am <- AM$new()
am$create("anchor", mne = "AC", desc = "Actor")
am$create("attribute", anchor = "AC", mne = "NAM", desc = "Name", hist = TRUE)
am$create("attribute", anchor = "AC", mne = "GEN", desc = "Gender", knot = "GEN")
am$create("knot", mne = "GEN", desc = "Gender")
am$create("attribute", anchor = "AC", mne = "PLV", desc = "ProfessionalLevel", knot = "PLV", hist = TRUE)
am$create("knot", mne = "PLV", desc = "ProfessionalLevel")
am$create("anchor", mne = "PR", desc = "Program")
am$create("attribute", anchor = "PR", mne = "NAM", desc = "Name")
am$create(class = "knot", mne = "RAT", desc = "Rating")
am$create(class = "tie", anchors = c("AC","PR"), knot = "RAT", roles = c("part","in","got"), identifier = c(Inf,Inf,1), hist = TRUE)
am$run()

# data --------------------------------------------------------------------

actor_mapping <- list(AC = list("code",
                                NAM = c("name", hist = "date"),
                                GEN = "gender",
                                PLV = c("level", hist = "date")))
am$load(mapping = actor_mapping,
        data = data.table(code = c("1", "2", "3", "4"),
                          name = c("Mike", "Bob", "Alice", "Lee"),
                          gender = c("M", "M", "F", "M"),
                          level = c(4L, 1L, 3L, 4L),
                          date = as.Date("2015-07-05")),
        meta = 1L)
am$load(mapping = actor_mapping,
        data = data.table(code = c("1", "2", "3"),
                          name = c("Mike", "Ben", "Alice"), # 1 name changed
                          gender = c("M", "M", "F"),
                          level = c(5L, 1L, 3L), # 1 level change
                          date = as.Date("2015-07-06")),
        meta = 2L)

actor_program_mapping <- list(PR = list("prog_code"),
                              AC = list("acto_code"),
                              AC_PR_RAT = list(hist = "date", knot = "score"))
am$load(mapping = actor_program_mapping,
        data = data.table(prog_code = c(1:2,3L,3L),
                          acto_code = as.character(c(1:2,2L,2L)),
                          score = c("A","D","E","D"),
                          date = as.Date("2015-07-03")+c(0:1,0:1)),
        meta = 3L)

program_mapping <- list(PR = list("code",
                                  NAM = "name"))
am$load(mapping = program_mapping,
        data = data.table(code = 1:3, name = c("show1","show2","show3")),
        meta = 4L)

# dashboard ---------------------------------------------------------------

am$dashboard()
