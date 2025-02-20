# test_that(".onLoad", {
#   # remove all opts
#   opts <- wm_opts()
#   lapply(opts, function(x) { NULL } ) |> wm_opts()
#   
#   # set opt to non-default value
#   wm_opts(fill = "black")
#   
#   # keep non-defaults on load
#   webmorphR:::.onLoad()
#   opts <- wm_opts()
#   expect_equal(opts$fill, "black")
# })

# # onAttach ----
# test_that("onAttach", {
#   devtools::unload()
#   expect_message(webmorphR:::.onAttach())
# })
