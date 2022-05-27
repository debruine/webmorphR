library(webmorphR)
library(webmorphR.stim)
library(rbenchmark)
library(magick) # cannot parallelise magick

stimuli <- load_stim_composite()

bm <- benchmark(
  iter = {
    s1 <- stimuli
    for (i in seq_along(stimuli)) {
      s1[[i]]$img <- image_rotate(stimuli[[i]]$img, 45)
    }
  },
  lapply = {
    s2 <- lapply(stimuli, function(s) {
      s$img <- image_rotate(s$img, 45)
      s
    }) |> as_stimlist()
  },
  replications = 1
)

bm


