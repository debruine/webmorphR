library(webmorphR)
library(webmorphR.stim)

nl <- load_stim_neutral_left()
nl_fpp <- auto_delin(nl, "fpp106")
nl_fpp_adjusted <- delin(nl_fpp)
write_stim(nl_fpp, "~/rproj/debruine/webmorphR.stim/inst/neutral_left_profile/", overwrite = FALSE)

nl_avg <- average_tem(nl_fpp)
nl_avg |> crop_tem() |> draw_tem()



sl <- load_stim_smiling_left(1)
sl_fpp <- auto_delin(sl, "fpp106")

sl_fpp[[1]]$lines <- NULL
sl_fpp[[1]]$closed <- NULL

sl_fpp[[1]] |> crop_tem() |> 
  resize(3) |>
  draw_tem(pt.shape = "index", pt.alpha = 1, pt.size = 25)
