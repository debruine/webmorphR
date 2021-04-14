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
