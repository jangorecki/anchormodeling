context("xml")

test_that("xml valid for Anchor Modeler", {

    anchor_modeler_url <- "http://roenbaeck.github.io/anchor/"
    am <- anchormodeling:::actor.am(Inf)
    file <- am$xml()
    message("File ",file," is waiting in your wd to be loaded in web browser on ",anchor_modeler_url,"\nDon't forget to click Play!")
    browseURL(anchor_modeler_url)

})
