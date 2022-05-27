test_that("defaults", {
  from_col <- list(
    col = "red",
    hexa = "#ff0000ff",
    hex = "#FF0000",
    hex3 = "#F00",
    rgb = c(255,0,0),
    rgba = c(255,0,0,255)
  )

  to_col <- list(
    hexa = "#ff0000ff",
    hex = "#ff0000",
    rgb = c(255,0,0),
    rgba = c(255,0,0,255),
    hsv = c(h=0,s=1,v=1)
  )

  from_fmt = c("col", "hex", "hexa", "hex3", "rgb", "rgba")
  to_fmt = c("hexa", "hex", "rgba", "rgb", "hsv")

  # guess from
  for (from in from_fmt) {
    for (to in to_fmt) {
      # use quasi-labelling !! to get informative error messages in loops
      expect_equal(color_conv(!!from_col[[from]], 1, "guess", !!to), !!to_col[[to]])
    }
  }

  # text rgba and alphas > 1
  expect_equal(color_conv("rgba(1,2,3,4)", 1, "guess", "hexa"), "#01020304")
  expect_equal(color_conv("rgb(1,2,3)", 5, "guess", "hexa"), "#01020305")
  expect_equal(color_conv("rgb(1,2,3)", 1.01, "guess", "hexa"), "#01020301")

  # specify from
  for (from in from_fmt) {
    for (to in to_fmt) {
      expect_equal(color_conv(!!from_col[[from]], 1, !!from, !!to), !!to_col[[to]])
    }
  }
})

# col2lab ----
test_that("col2lab", {
  # http://www.brucelindbloom.com/index.html?ColorCheckerCalcHelp.html
  # values checked with ref white = D65, 
  # RGB Model = sRGB, dom lambda = 611.4 nm (default)
  
  # color name
  red <- col2lab("red") |> unlist()
  lindbloom_red = c(L = 53.24, a = 80.09, b = 67.20)
  expect_equal(lindbloom_red, red, tolerance = 0.05)
  
  # hex color
  green <- col2lab("#00FF00") |> unlist()
  lindbloom_green = c(L = 87.73, a = -86.18, b = 83.18)
  expect_equal(lindbloom_green, green, tolerance = 0.05)
})

# lab2rgb ----
test_that("lab2rgb", {
  # white
  col <- '#FFFFFF'
  lab <- col2lab(col)
  rgb <- lab2rgb(lab)
  comp <- color_conv(col, to = 'rgb')
  names(comp) <- c('red', 'green', 'blue')
  expect_equal(rgb, comp)
  
  # red
  col <- '#FF0000'
  lab <- col2lab(col)
  rgb <- lab2rgb(lab)
  comp <- color_conv(col, to = 'rgb')
  names(comp) <- c('red', 'green', 'blue')
  expect_equal(rgb, comp)
  
  # dodgerblue
  col <- 'dodgerblue'
  lab <- col2lab(col)
  rgb <- lab2rgb(lab)
  comp <- color_conv(col, to = 'rgb')
  names(comp) <- c('red', 'green', 'blue')
  expect_equal(rgb, comp)
  
})
