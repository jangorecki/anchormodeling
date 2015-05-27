library(microbenchmark)
library(data.table) # 1.9.5+

## USE THIS?: http://stackoverflow.com/a/30466237/2490497

# system.time replacement just for elapsed time
# system.time <- nano.time <- function(expr, gcFirst = TRUE){
#     if (gcFirst)
#         gc(FALSE)
#     time <- microbenchmark::get_nanotime()
#     on.exit(cat("Timing stopped at:", (microbenchmark::get_nanotime() - time) * 1e-9, "\n"))
#     expr
#     new.time <- microbenchmark::get_nanotime()
#     on.exit()
#     structure(c("user" = NA_real_, "system" = NA_real_, "elapsed" = (new.time - time) * 1e-9, "sys.child" = NA_real_, "sys.child" = NA_real_), class = "proc_time")
# }
library(R6)
library(parallel)

cl <- R6Class(
    classname = "cl",
    public = list(
        data = data.table(NULL),
        initialize = function() invisible(self),
        insert = function(x) self$data <- rbindlist(list(self$data, x))
    )
)

N <- 8L # number of entities
i <- setNames(seq_len(N),paste0("n",seq_len(N)))

# random data.tables
set.seed(1)
ldt <- lapply(i, function(i) data.table(replicate(sample(3:10,1),sample(letters,1e5,rep=TRUE))))

# entity storage
lcl1 <- lapply(i, function(i) cl$new())
lcl2 <- lapply(i, function(i) cl$new())

# mclapply vs. mcmapply
system.time(
    mclapply(names(i), FUN = function(n) lcl1[[n]]$insert(ldt[[n]]))
)
system.time(
    mcmapply(FUN = function(dt, cl) cl$insert(dt), ldt, lcl2, SIMPLIFY=FALSE)
)
system.time(
    mclapply(names(i), FUN = function(n) lcl1[[n]]$insert(ldt[[n]]))
)
system.time(
    mcmapply(FUN = function(dt, cl) cl$insert(dt), ldt, lcl2, SIMPLIFY=FALSE)
)
system.time(
    mclapply(names(i), FUN = function(n) lcl1[[n]]$insert(ldt[[n]]))
)
system.time(
    mcmapply(FUN = function(dt, cl) cl$insert(dt), ldt, lcl2, SIMPLIFY=FALSE)
)

sapply(lcl1, function(cl) nrow(cl$data))
# unix: 0L
sapply(lcl2, function(cl) nrow(cl$data))
# unix: 0L

# lapply vs mclapply
if.mclapply <- function(..., parallel = getOption("am.parallel")){
    if(is.null(parallel) || parallel==FALSE || !requireNamespace("parallel", quietly = TRUE)){
        return(lapply(...))
    } else if(isTRUE(parallel)){
        if(requireNamespace("parallel", quietly = TRUE)){
            return(parallel::mclapply(...))
        } else {
            stop("am.parallel is set to TRUE for parallel processing but no 'parallel' package installed.")
        }
    }
}

lcl1 <- lapply(i, function(i) cl$new())
lcl2 <- lapply(i, function(i) cl$new())

system.time(
    if.mclapply(names(i), FUN = function(n) lcl1[[n]]$insert(ldt[[n]]), parallel = TRUE)
)
system.time(
    if.mclapply(names(i), FUN = function(n) lcl2[[n]]$insert(ldt[[n]]), parallel = FALSE)
)
system.time(
    if.mclapply(names(i), FUN = function(n) lcl1[[n]]$insert(ldt[[n]]), parallel = TRUE)
)
system.time(
    if.mclapply(names(i), FUN = function(n) lcl2[[n]]$insert(ldt[[n]]), parallel = FALSE)
)

sapply(lcl1, function(cl) nrow(cl$data))
# unix: 0L
sapply(lcl2, function(cl) nrow(cl$data))
# unix: > 0L
