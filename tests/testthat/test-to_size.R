stimuli <- demo_stim()

test_that("basic", {
  s <- c(
    stimuli[1],
    stimuli[1] |> resize(.5),
    stimuli[1] |> crop(0.8),
    stimuli[1] |> crop(0.8) |> resize(0.75)
  )

  new1 <- to_size(s, 300, 400, fill = "red")
  expect_equal(width(new1) |> unname(), rep(300, 4))
  expect_equal(height(new1) |> unname(), rep(400, 4))

  new2 <- to_size(s, 300, 400, fill = "red", keep_rels = TRUE)
  expect_equal(width(new1) |> unname(), rep(300, 4))
  expect_equal(height(new1) |> unname(), rep(400, 4))

  # skip("needs visual check")
  # plot(new1)
  # plot(new2)
})

test_that("width and height", {
  expect_error(to_size(stimuli, 10))
  expect_error(to_size(stimuli, 10, 0))
  expect_error(to_size(stimuli, "A", 10))

  x <- to_size(stimuli, c(100, 200), c(300, 400))
  expect_equal(width(x), c(f_multi = 100, m_multi = 200))
  expect_equal(height(x), c(f_multi = 300, m_multi = 400))

  x <- to_size(stimuli, c(100, 200))
  expect_equal(width(x), c(f_multi = 100, m_multi = 100))
  expect_equal(height(x), c(f_multi = 200, m_multi = 200))

  x <- to_size(stimuli, c(h = 200, w = 100))
  expect_equal(width(x), c(f_multi = 100, m_multi = 100))
  expect_equal(height(x), c(f_multi = 200, m_multi = 200))

  x <- to_size(stimuli, c(height = 200, width = 100))
  expect_equal(width(x), c(f_multi = 100, m_multi = 100))
  expect_equal(height(x), c(f_multi = 200, m_multi = 200))
})

test_that("no tem", {
  notem <- remove_tem(stimuli)
  x <- to_size(notem, 100, 200)
  expect_equal(width(x) |> unname(), c(100, 100))
  expect_equal(height(x) |> unname(), c(200, 200))
})

test_that("crop", {
  s <- c(
    stimuli[1],
    stimuli[1] |> resize(.5),
    stimuli[1] |> crop(0.6),
    stimuli[1] |> crop(1, 0.8)
  ) |> pad(10, fill = "hotpink")
  
  new1 <- to_size(s, 300, 400, crop = TRUE)
  expect_equal(width(new1) |> unname(), rep(300, 4))
  expect_equal(height(new1) |> unname(), rep(400, 4))
  
  # keep_rels = TRUE
  new2 <- to_size(s, 300, 400, crop = TRUE, keep_rels = TRUE)
  expect_equal(width(new2) |> unname(), rep(300, 4))
  expect_equal(height(new1) |> unname(), rep(400, 4))
  
  # skip("needs visual check")
  # 
  # new1f <- to_size(s, 300, 400, crop = FALSE, fill = "dodgerblue")
  # new2f <- to_size(s, 300, 400, crop = FALSE, keep_rels = TRUE, fill = "dodgerblue")
  # 
  # plot_rows(
  #   # brings all image to fit in new size and pads
  #   "crop = F, keep_rels = F" = new1f,
  #   # brings largest image to fit in new size, resizes other propotionally, and pads
  #   "crop = F, keep_rels = T" = new2f,
  #   # brings all images to cover new size and crops
  #   "crop = T, keep_rels = F" = new1,
  #   # brings the smallest image to cover the new size, resizes other proportionally, and crops
  #   "crop = T, keep_rels = T" = new2,
  #   top_label = TRUE
  # )
})
