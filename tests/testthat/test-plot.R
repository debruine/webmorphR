test_that("one", {
  x <- demo_stim("test", 1)

  p <- plot(x)

  expect_equal(width(p)[[1]], width(x)[[1]] + 20)
})

test_that("two", {
  x <- demo_stim()

  p <- plot(x)
  expect_equal(width(p)[[1]], x[[1]]$width*2+30)
  expect_equal(height(p)[[1]], x[[1]]$height+20)

  p <- plot(x, nrow = 2)
  expect_equal(width(p)[[1]], x[[1]]$width+20)
  expect_equal(height(p)[[1]], x[[1]]$height*2+30)
})

# nrow/ncol ----
test_that("nrow/ncol", {
  wm_opts(plot.maxwidth = 10000, 
          plot.maxheight = 10000)
  
  x <- demo_stim()
  
  r2 <- plot(x, nrow = 2)
  expect_equal(width(r2), c(grid = 520))
  expect_equal(height(r2), c(grid = 1030))
  
  # ignore extra rows
  r3 <- plot(x, nrow = 3)
  expect_equal(width(r3), c(grid = 520))
  expect_equal(height(r3), c(grid = 1030))
  
  c2 <- plot(x, ncol = 2)
  expect_equal(width(c2), c(grid = 1030))
  expect_equal(height(c2), c(grid = 520 ))
  
  # ignore extra columns
  c3 <- plot(x, ncol = 3)
  expect_equal(width(c3), c(grid = 1030))
  expect_equal(height(c3), c(grid = 520 ))
  
  # auto-calculate rows/cols
  w <- 100
  x <- demo_stim() |> resize(w) |> rep(10)
  p <- plot(x)
  
  # should be 5w x 4h with 10px paddings
  expect_equivalent(width(p), 5*w + 6*10)
  expect_equivalent(height(p), 4*w + 5*10)
  
  
})

# padding ----
test_that("padding", {
  w <- 100
  x <- demo_stim() |> resize(w) |> rep(6)
  
  p12 <- plot(x, padding = 0)
  expect_equivalent(width(p12), 4*w)
  expect_equivalent(height(p12), 3*w)
  
  pad <- 5
  p12b <- plot(x, padding = pad)
  expect_equivalent(width(p12b), 4*w + 5*pad)
  expect_equivalent(height(p12b), 3*w + 4*pad)
  
  pad <- 5
  p12c <- plot(x, padding = pad, external_pad = FALSE)
  expect_equivalent(width(p12c), 4*w + 3*pad)
  expect_equivalent(height(p12c), 3*w + 2*pad)
  
  # fill changed padding color
  p12d <- plot(x, fill = "red")
  expect_equal(patch(p12d$grid$img), "#FF0000FF")
})

# byrow ----
test_that("byrow", {
  rb <- blank(6, color = rainbow(6))
  colors <- get_imgs(rb) |> sapply(patch)
  
  p <- plot(rb, padding = 0)
  expect_equal(patch(p$grid$img, 1, 10, 1, 10), colors[1])
  expect_equal(patch(p$grid$img, 101, 110, 1, 10), colors[2])
  expect_equal(patch(p$grid$img, 201, 210, 1, 10), colors[3])
  expect_equal(patch(p$grid$img, 1, 10, 101, 110), colors[4])
  expect_equal(patch(p$grid$img, 101, 110, 101, 110), colors[5])
  expect_equal(patch(p$grid$img, 201, 210, 101, 110), colors[6])
  
  p <- plot_stim(rb, byrow = TRUE, padding = 0)
  expect_equal(patch(p$grid$img, 1, 10, 1, 10), colors[1])
  expect_equal(patch(p$grid$img, 101, 110, 1, 10), colors[2])
  expect_equal(patch(p$grid$img, 201, 210, 1, 10), colors[3])
  expect_equal(patch(p$grid$img, 1, 10, 101, 110), colors[4])
  expect_equal(patch(p$grid$img, 101, 110, 101, 110), colors[5])
  expect_equal(patch(p$grid$img, 201, 210, 101, 110), colors[6])
  
  p <- plot_stim(rb, byrow = FALSE, padding = 0)
  expect_equal(patch(p$grid$img, 1, 10, 1, 10), colors[1])
  expect_equal(patch(p$grid$img, 101, 110, 1, 10), colors[3])
  expect_equal(patch(p$grid$img, 201, 210, 1, 10), colors[5])
  expect_equal(patch(p$grid$img, 1, 10, 101, 110), colors[2])
  expect_equal(patch(p$grid$img, 101, 110, 101, 110), colors[4])
  expect_equal(patch(p$grid$img, 201, 210, 101, 110), colors[6])
})

# maxwidth/maxheight ----
test_that(" maxwidth/maxheight", {
  x <- demo_stim()
  w500 <- plot(x, padding = 0, maxwidth = 500)
  expect_equal(w500$grid$width, 500)
  expect_equal(w500$grid$height, 250)
  
  h100 <- plot(x, padding = 0, maxheight = 100)
  expect_equal(h100$grid$width, 200)
  expect_equal(h100$grid$height, 100)
  
  h100w500 <- plot(x, padding = 0, maxwidth = 500, maxheight = 100)
  expect_equal(h100w500$grid$width, 200)
  expect_equal(h100w500$grid$height, 100)
  
  # infinite max doesn't increase larger than originals
  inf <- plot(x, padding = 0, maxwidth = Inf, maxheight = Inf)
  expect_equal(inf$grid$width, 1000)
  expect_equal(inf$grid$height, 500)
})
