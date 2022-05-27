test_that("crop_tem", {
  stimuli <- demo_stim()
  
  ct <- crop_tem(stimuli)
  expect_true(all(width(ct) < width(stimuli)))
  expect_true(all(height(ct) < height(stimuli)))
  expect_equal(width(ct[1])[[1]], width(ct[2])[[1]])
  expect_equal(height(ct[1])[[1]], height(ct[2])[[1]])
  
  # change padding
  ct1 <- crop_tem(stimuli, 20, 30)
  expect_true(all(width(ct1) == width(ct) + 40))
  expect_true(all(height(ct1) == height(ct) + 20))
  
  # each separately
  ct2 <- crop_tem(stimuli, each = TRUE)
  expect_true(all(width(ct2) < width(stimuli)))
  expect_true(all(height(ct2) < height(stimuli)))
  expect_true(width(ct2[1])[[1]] != width(ct2[2])[[1]])
  expect_true(height(ct2[1])[[1]] != height(ct2[2])[[1]])
})
