stimuli <- demo_stim()
stim <- stimuli[[1]]
stim2 <- stimuli[[2]]

test_that("new_stimlist", {
  y <- new_stimlist(stim, stim2, .names = c("A", "B"))
  expect_equal(names(y), c("A", "B"))
  expect_equal(class(y), c("stimlist", "list"))
})


test_that("validate_stimlist", {
  # valid stimlist
  x <- validate_stimlist(stimuli)
  expect_equal(x, stimuli)

  # valid stim
  x <- validate_stimlist(stim)
  expect_equal(class(x), c("stimlist", "list"))
  expect_equal(names(x), "f_multi")

  # valid stimlist with no class
  x <- validate_stimlist(unclass(stimuli))
  expect_equal(x, stimuli)

  # valid stimlist with no names
  x <- validate_stimlist(unname(stimuli))
  expect_equal(x, stimuli)

  # valid stim with no class
  x <- validate_stimlist(unclass(stim))
  expect_equal(class(x), c("stimlist", "list"))
  expect_equal(names(x), "f_multi")
})

test_that("errors", {
  expect_error(validate_stimlist())
  expect_error(validate_stimlist(c()))
  expect_error(validate_stimlist(list()))
  expect_error(validate_stimlist(list("a")))
})

# reload code currently commented out
# test_that("reload img", {
#   stim$img <- NULL
#   stim2 <- validate_stimlist(stim)
#   expect_equal(class(stim2[[1]]$img), "magick-image")
# })

