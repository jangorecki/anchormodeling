library(shiny)
library(DT)
AM <- function() getOption("am.share")$process(pretty=TRUE)
# if(is.null(AM())) stop("you must use `monitor` function which add reference for anchor model.")
shinyServer(function(input, output) {
    output$tbl = DT::renderDataTable({
        DT::datatable(AM(), options = list(lengthChange = FALSE), rownames = FALSE)
    })
})
