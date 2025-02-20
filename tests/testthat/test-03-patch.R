input <- blank(8, 10, 10, list(rgb(0, 0, 0),
                                rgb(1, 0, 0),
                                rgb(0, 1, 0),
                                rgb(1, 1, 0),
                                rgb(0, 0, 1),
                                rgb(1, 0, 1),
                                rgb(0, 1, 1),
                                rgb(1, 1, 1)))

# black, red,     green, yellow
# blue,  magenta, cyan,  white
img <- plot(input, nrow = 2, padding = 0)

test_that("basic", {
  expect_equal(patch(img)[[1]], "#000000FF")
  expect_equal(patch(img, 10, 10, 0, 0)[[1]], "#000000FF")
  expect_equal(patch(img, 10, 10, 10, 0)[[1]], "#FF0000FF")
  expect_equal(patch(img, 10, 10, 20, 0)[[1]], "#00FF00FF")
  expect_equal(patch(img, 10, 10, 30, 0)[[1]], "#FFFF00FF")
  expect_equal(patch(img, 10, 10, 0, 10)[[1]], "#0000FFFF")
  expect_equal(patch(img, 10, 10, 10, 10)[[1]], "#FF00FFFF")
  expect_equal(patch(img, 10, 10, 20, 10)[[1]], "#00FFFFFF")
  expect_equal(patch(img, 10, 10, 30, 10)[[1]], "#FFFFFFFF")
  
  expect_equal(patch(img, 10, 10, 0, 0, "rgb")[[1]], 
               c(red = 0, green = 0, blue = 0, alpha = 255))
  expect_equal(patch(img, 10, 10, 10, 0, "rgb")[[1]], 
               c(red = 255, green = 0, blue = 0, alpha = 255))
  expect_equal(patch(img, 10, 10, 20, 0, "rgb")[[1]], 
               c(red = 0, green = 255, blue = 0, alpha = 255))
  expect_equal(patch(img, 10, 10, 30, 0, "rgb")[[1]], 
               c(red = 255, green = 255, blue = 0, alpha = 255))
  expect_equal(patch(img, 10, 10, 0, 10, "rgb")[[1]], 
               c(red = 0, green = 0, blue = 255, alpha = 255))
  expect_equal(patch(img, 10, 10, 10, 10, "rgb")[[1]], 
               c(red = 255, green = 0, blue = 255, alpha = 255))
  expect_equal(patch(img, 10, 10, 20, 10, "rgb")[[1]], 
               c(red = 0, green = 255, blue = 255, alpha = 255))
  expect_equal(patch(img, 10, 10, 30, 10, "rgb")[[1]], 
               c(red = 255, green = 255, blue = 255, alpha = 255))
})

test_that("multi-color patches", {
  # median
  med1 <- patch(img, 20, 10, 0, 0, "rgb")[[1]]
  med2 <- patch(img, 19, 10, 0, 0, "rgb")[[1]]
  mean1 <- patch(img, 20, 10, 0, 0, "rgb", mean)[[1]]
  mean2 <- patch(img, 19, 10, 0, 0, "rgb", mean)[[1]]
  
  expect_equal(med1, c(red = 122, green = 27, blue = 12, alpha = 255))
  expect_equal(med2, c(red = 0, green = 0, blue = 0, alpha = 255))
  expect_true(any(mean1 != mean2))
})

test_that("list", {
  # set patch to each square
  args <- list(
    stimuli = rep(img, 8),
    width = rep(10, 8),
    height = rep(10, 8),
    x_off = rep(c(0, 10, 20, 30), times = 2),
    y_off = rep(c(0, 10), each = 4)
 )
  
  p <- do.call(patch, args)
  
  expect_equal(p[[1]], "#000000FF")
  expect_equal(p[[2]], "#FF0000FF")
  expect_equal(p[[3]], "#00FF00FF")
  expect_equal(p[[4]], "#FFFF00FF")
  expect_equal(p[[5]], "#0000FFFF")
  expect_equal(p[[6]], "#FF00FFFF")
  expect_equal(p[[7]], "#00FFFFFF")
  expect_equal(p[[8]], "#FFFFFFFF")
})

