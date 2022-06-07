# twitter intro

# install ----

install.packages("webmorphR")
library(webmorphR)

# load randomly rotated and cropped images
us <- demo_unstandard() 

# plot keeping relative size diffs
plot(us, nrow = 2)

# standardise orientation & align
# crop and resize
standard <- us |>
  horiz_eyes() |>
  align() |>
  crop_tem(50, 20, 20, 20) |>
  resize(height = 500)

plot(standard, nrow = 2)
