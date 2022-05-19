test_that("blank", {
  x <- blank()
  img <- x[[1]]$img
  info <- magick::image_info(img)
  
  expect_equal(info$format, "png")
  expect_equal(info$width, 100)
  expect_equal(info$height, 100)
  expect_equal(patch(x)[[1]], "#FFFFFFFF")
})

test_that("blank-args", {
  # add colour and size
  x <- blank(n = 3, 
             width = 50, 
             height = 150, 
             color = c("red", "green", "blue"), 
             names = c("r", "g", "b"))
  
  expect_equal(width(x), c(r = 50, g = 50, b = 50))
  expect_equal(height(x), c(r = 150, g = 150, b = 150))
  
  expect_equal(patch(x), c(r = "#FF0000FF", 
                           g = "#00FF00FF", 
                           b = "#0000FFFF"))
})
