test_that("squash_tem", {
  s <- demo_stim()
  
  crop_no_squash <- crop(s, 200, 200) 
  pts <- crop_no_squash[[1]]$points
  expect_lt(min(pts["x", ]), 0)
  expect_gt(max(pts["x", ]), 200-1)
  expect_lt(min(pts["y", ]), 0)
  expect_gt(max(pts["y", ]), 200-1)
  repad <- crop_no_squash |> pad((width(s)-200)/2)
  expect_equal(s[[1]]$points, repad[[1]]$points)
  #draw_tem(repad)
  
  crop_squash <- crop(s, 200, 200) |> squash_tem() 
  pts <- crop_squash[[1]]$points
  expect_equal(min(pts["x", ]), 0)
  expect_equal(max(pts["x", ]), 200-1)
  expect_equal(min(pts["y", ]), 0)
  expect_equal(max(pts["y", ]), 200-1)
  #crop_squash |> pad((width(s)-200)/2) |> draw_tem()
})
