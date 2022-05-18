test_that("social_media_size", {
  expect_error(social_media_size("nope"))
  
  expect_equal(social_media_size("twitter", "link"),  
               c(width = 1200,  height = 628))
  expect_equal(social_media_size("twitter", "one"),  
               c(width = 1200,  height = 675))
  expect_equal(social_media_size("twitter", "two"),  
               c(width = 700,  height = 800))
  expect_equal(social_media_size("twitter", "three_left"),  
               c(width = 700,  height = 800))
  expect_equal(social_media_size("twitter", "three_right"),  
               c(width = 1200,  height = 686))
  expect_equal(social_media_size("twitter", "four"),  
               c(width = 1200,  height = 600))
  expect_equal(social_media_size("twitter", "nope"),  
               c(width = 1200,  height = 675))
  
  expect_equal(social_media_size("instagram", "feed_large"),  
               c(width = 1080,  height = 1080))
  expect_equal(social_media_size("instagram", "feed_small"),  
               c(width = 612,  height = 612))
  expect_equal(social_media_size("instagram", "nope"),  
               c(width = 1080,  height = 1080))
})
