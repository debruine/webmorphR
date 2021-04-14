test_that("works", {
  stimuli <- demo_stim()

  x <- average_tem(stimuli)
  expect_equal(class(x)[[1]], "stimlist")
  expect_equal(x[[1]]$imgpath, "average")
  expect_equal(dim(x[[1]]$points), c(2, 189))
})

test_that("no tem", {
  tem <- demo_stim()
  notem <- remove_tem(tem)
  expect_error(average_tem(notem))

  # some tems
  expect_warning(a <- average_tem(c(notem, tem)))
})
