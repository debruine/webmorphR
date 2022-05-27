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
  expect_equal(patch(img, 0, 10, 0, 10)[[1]], "#000000FF")
  expect_equal(patch(img, 0, 10, 11, 20)[[1]], "#0000FFFF")
  expect_equal(patch(img, 11, 20, 0, 10)[[1]], "#FF0000FF")
  expect_equal(patch(img, 11, 20, 11, 20)[[1]], "#FF00FFFF")
  expect_equal(patch(img, 21, 30, 0, 10)[[1]], "#00FF00FF")
  expect_equal(patch(img, 21, 30, 11, 20)[[1]], "#00FFFFFF")
  expect_equal(patch(img, 31, 40, 0, 10)[[1]], "#FFFF00FF")
  expect_equal(patch(img, 31, 40, 11, 20)[[1]], "#FFFFFFFF")

  expect_equal(patch(img, 0, 10, 0, 10, "rgb")[[1]],
               c(red = 0, green = 0, blue = 0, alpha = 255))
  expect_equal(patch(img, 0, 10, 11, 20, "rgb")[[1]],
               c(red = 0, green = 0, blue = 255, alpha = 255))
  expect_equal(patch(img, 11, 20, 0, 10, "rgb")[[1]],
               c(red = 255, green = 0, blue = 0, alpha = 255))
  expect_equal(patch(img, 11, 20, 11, 20, "rgb")[[1]],
               c(red = 255, green = 0, blue = 255, alpha = 255))
  expect_equal(patch(img, 21, 30, 0, 10, "rgb")[[1]],
               c(red = 0, green = 255, blue = 0, alpha = 255))
  expect_equal(patch(img, 21, 30, 11, 20, "rgb")[[1]],
               c(red = 0, green = 255, blue = 255, alpha = 255))
  expect_equal(patch(img, 31, 40, 0, 10, "rgb")[[1]],
               c(red = 255, green = 255, blue = 0, alpha = 255))
  expect_equal(patch(img, 31, 40, 11, 20, "rgb")[[1]],
               c(red = 255, green = 255, blue = 255, alpha = 255))
})

test_that("multi-color patches", {
  # median
  expect_equal(patch(img, 0, 20, 0, 10, "rgb")[[1]],
               c(red = 122, green = 27, blue = 12, alpha = 255))
  expect_equal(patch(img, 0, 19, 0, 10, "rgb")[[1]],
               c(red = 0, green = 0, blue = 0, alpha = 255))

  # mean gives average no matter the balance
  expect_equal(patch(img, 0, 20, 0, 10, "rgb", mean)[[1]],
               c(red = 122, green = 27, blue = 12, alpha = 255))
  expect_equal(patch(img, 0, 19, 0, 10, "rgb", mean)[[1]],
               c(red = 116, green = 27, blue = 12, alpha = 255))
})

test_that("list", {
  # set patch to each square
  args <- list(
    stimuli = rep(img, 8),
    x1 = rep(c(0, 11, 21, 31), each = 2),
    x2 = rep(c(10, 20, 30, 40), each = 2),
    y1 = rep(c(0, 11), times = 4),
    y2 = rep(c(10, 20), times = 4)
 )
  
  p <- do.call(patch, args)
  
  expect_equal(p[[1]], "#000000FF")
  expect_equal(p[[2]], "#0000FFFF")
  expect_equal(p[[3]], "#FF0000FF")
  expect_equal(p[[4]], "#FF00FFFF")
  expect_equal(p[[5]], "#00FF00FF")
  expect_equal(p[[6]], "#00FFFFFF")
  expect_equal(p[[7]], "#FFFF00FF")
  expect_equal(p[[8]], "#FFFFFFFF")
})

