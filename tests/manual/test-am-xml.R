context("xml")

test_that("xml valid for Anchor Modeler", {

    am <- actor.am(Inf)
    file <- am$xml()
    message("File ",file," is waiting in your wd to be loaded in web browser on ",anchor_modeler_url(),"\nDon't forget to click Play!")
    browseURL(anchor_modeler_url())

})
