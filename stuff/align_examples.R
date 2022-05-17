library(webmorphR)
library(webmorphR.stim)
wm_opts(plot.maxwidth = 800)

wm_opts(fill = "#CFD3D2")

s <- load_stim_london(sample(1:102, 6)) |>
  rotate(sample(-20:20, 10)) |>
  crop(1, 1, 
       x_off = runif(10, -.2, .2),
       y_off = runif(10, -.2, .2)) |>
  resize(runif(10, .5, 1)) |>
  to_size(500, 500, keep_rels = T)

wm_opts(fill = "white")

a1 <- align_1point(s, pt = 55, x = 250, y = 250)

plot(a1, nrow = 2) 

a2 <- align_2point(s, 0, 1, x1 = 200, x2 = 300, y1 = 250, y2 = 250)


p <- align_procrustes(s)

plot_rows(
  "original image set: s" = s, 
  "1-point alignment: align_1point(s, pt = 55, x = 250, y = 250)" = a1, 
  "2-point alignment: align_2point(s, pt1 = 0, pt2 = 1, x1 = 200, x2 = 300, y1 = 250, y2 = 250)" = a2, 
  "procrustes alignment: align_procrustes(s)" = p,
  top_label = TRUE
) |> write_stim("~/Desktop/", "align_3func")

plot_rows(
  "original image set: s" = s, 
  "1-point alignment: align(s, pt1 = 55, pt2 = 55, x1 = 250, y1 = 250)" = a1, 
  "2-point alignment: align(s, pt1 = 0, pt2 = 1, x1 = 200, x2 = 300, y1 = 250, y2 = 250)" = a2, 
  "procrustes alignment: align(s, procrustes = TRUE)" = p,
  top_label = TRUE
) |> write_stim("~/Desktop/", "align_1func")
