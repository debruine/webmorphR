library(webmorphR)
set.seed(8675309)

# make unstandardised images
s <- webmorphR.stim::load_stim_composite()
bg <- patch(s)
rot <- s |> rotate(seq(-5, 5, length.out = 10) |> sample(), fill = bg)
res <- rot |> resize(seq(.8, 1, length.out = 10) |> sample())
crp <- res |> crop(seq(.8, 1, length.out = 5),
                   seq(1, .8, length.out = 5),
                   y_off = c(0, .05)) |>
  resize(0.5)

lbls <- paste0(width(crp), " x ", height(crp))
crp |> pad(1, fill = "black") |> 
  to_size(keep_rels = TRUE) |> 
  pad(100, 0, 0, 0) |>
  label(lbls) |>
  plot(maxwidth = 600)

write_stim(crp, "inst/extdata/unstandard", 
           format = "jpg", quality = 75,
           overwrite = TRUE)
