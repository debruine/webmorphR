library(magick)

input <- c(
  image_blank(10, 10, rgb(0, 0, 0)),
  image_blank(10, 10, rgb(1, 0, 0)),
  image_blank(10, 10, rgb(0, 1, 0)),
  image_blank(10, 10, rgb(1, 1, 0)),
  image_blank(10, 10, rgb(0, 0, 1)),
  image_blank(10, 10, rgb(1, 0, 1)),
  image_blank(10, 10, rgb(0, 1, 1)),
  image_blank(10, 10, rgb(1, 1, 1))
)

# black, red,     green, yellow
# blue,  magenta, cyan,  white
img <- image_montage(input, geometry = '10x10+0+0', tile = '4x2')

test_that("basic", {
  expect_equal(patch(img), "#000000FF")
  expect_equal(patch(img, 1, 10, 1, 10), "#000000FF")
  expect_equal(patch(img, 1, 10, 11, 20), "#0000FFFF")
  expect_equal(patch(img, 11, 20, 1, 10), "#FF0000FF")
  expect_equal(patch(img, 11, 20, 11, 20), "#FF00FFFF")
  expect_equal(patch(img, 21, 30, 1, 10), "#00FF00FF")
  expect_equal(patch(img, 21, 30, 11, 20), "#00FFFFFF")
  expect_equal(patch(img, 31, 40, 1, 10), "#FFFF00FF")
  expect_equal(patch(img, 31, 40, 11, 20), "#FFFFFFFF")

  expect_equal(patch(img, 1, 10, 1, 10, "rgb"),
               c(red = 0, green = 0, blue = 0, alpha = 255))
  expect_equal(patch(img, 1, 10, 11, 20, "rgb"),
               c(red = 0, green = 0, blue = 255, alpha = 255))
  expect_equal(patch(img, 11, 20, 1, 10, "rgb"),
               c(red = 255, green = 0, blue = 0, alpha = 255))
  expect_equal(patch(img, 11, 20, 11, 20, "rgb"),
               c(red = 255, green = 0, blue = 255, alpha = 255))
  expect_equal(patch(img, 21, 30, 1, 10, "rgb"),
               c(red = 0, green = 255, blue = 0, alpha = 255))
  expect_equal(patch(img, 21, 30, 11, 20, "rgb"),
               c(red = 0, green = 255, blue = 255, alpha = 255))
  expect_equal(patch(img, 31, 40, 1, 10, "rgb"),
               c(red = 255, green = 255, blue = 0, alpha = 255))
  expect_equal(patch(img, 31, 40, 11, 20, "rgb"),
               c(red = 255, green = 255, blue = 255, alpha = 255))
})

test_that("multi-color patches", {
  # hex values get rounded
  expect_equal(patch(img, 1, 20, 1, 10), "#7F0000FF")

  # median gives average with *exact* balance
  expect_equal(patch(img, 1, 20, 1, 10, "rgb"),
               c(red = 127.5, green = 0, blue = 0, alpha = 255))
  # but not with any imbalance
  expect_equal(patch(img, 1, 19, 1, 10, "rgb"),
               c(red = 0, green = 0, blue = 0, alpha = 255))

  # mean gives average no matter the balance
  expect_equal(patch(img, 1, 20, 1, 10, "rgb", mean),
               c(red = 127.5, green = 0, blue = 0, alpha = 255))
  expect_equal(patch(img, 1, 19, 1, 10, "rgb", mean),
               c(red = 120.7895, green = 0, blue = 0, alpha = 255),
               tolerance = 0.001)
})
