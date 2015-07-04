library(anchormodeling)

# model
am <- actor.am(Inf)
am$run()

# data
# am$load()

# dashboard
am$dashboard()
