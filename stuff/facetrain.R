library(webmorphR)
library(reticulate)

source_python("stuff/facetrain.py")

stimuli <- demo_stim("composite") |>
  resize(0.5) |>
  subset_tem(features("gmm"))

stimuli[1] |> draw_tem() |> plot()

dir <- "~/rproj/facetrain/demo"
tem_to_xml(stimuli, dir)
xml <- file.path(dir, "images.xml") |> normalizePath()

# check xml
readLines(xml, n = 10) |> paste(collapse = "\n") |> cat()

newdat <- normalizePath("~/Desktop/new.dat")
facetrain(training = xml, 
          output = newdat, 
          tree_depth = 5, 
          nu = 0.5, 
          cascade_depth = 15, 
          feature_pool_size = 400, 
          num_test_splits = 50, 
          oversampling_amount = 5, 
          oversampling_translation_jitter = 0.1, 
          be_verbose = True, 
          num_threads = 0
          tree_depth = 5L, 
          nu = 0.5, 
          cascade_depth = 15L)

file.size(newdat)/1024/1024

devtools::load_all(".")
teststim <- c(demo_stim("lisa"), demo_stim("zoom"))
s2 <- auto_delin(teststim, replace = TRUE, dlib_path = newdat)
draw_tem(s2) |> to_size(400, 600) |> plot(nrow = 2)
