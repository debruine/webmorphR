stimuli <- demo_stim()
stim <- stimuli[[1]]
stim2 <- stimuli[[2]]

# new_stimlist ----
test_that("new_stimlist", {
  y <- new_stimlist(stim, stim2, .names = c("A", "B"))
  expect_equal(names(y), c("A", "B"))
  expect_equal(class(y), c("stimlist", "list"))
})

# as_stimlist ----
test_that("as_stimlist", {
  # valid stimlist
  x <- as_stimlist(stimuli)
  expect_equal(x, stimuli)

  # valid stim
  x <- as_stimlist(stim)
  expect_equal(class(x), c("stimlist", "list"))
  expect_equal(names(x), "f_multi")

  # valid stimlist with no class
  x <- as_stimlist(unclass(stimuli))
  expect_equal(x, stimuli)

  # valid stimlist with no names
  x <- as_stimlist(unname(stimuli))
  expect_equal(x, stimuli)

  # valid stim with no class
  x <- as_stimlist(unclass(stim))
  expect_equal(class(x), c("stimlist", "list"))
  expect_equal(names(x), "f_multi")
  
  # stimlist with no img
  noimg <- stimuli
  noimg$f_multi$img <- NULL
  noimg$m_multi$img <- NULL
  x <- as_stimlist(noimg)
  expect_equal(x, noimg)
  
  # stimlist with no w/h
  nodim <- noimg
  nodim$f_multi$width <- NULL
  nodim$m_multi$width <- NULL
  nodim$f_multi$height <- NULL
  nodim$m_multi$height <- NULL
  expect_warning(x <- as_stimlist(nodim))
  expect_true(!is.null(x$f_multi$width))
  expect_true(!is.null(x$f_multi$height))
})

test_that("errors", {
  expect_error(as_stimlist())
  expect_error(as_stimlist(c()))
  expect_error(as_stimlist(list()))
  expect_error(as_stimlist(list("a")))
})

# reload code currently commented out
# test_that("reload img", {
#   stim$img <- NULL
#   stim2 <- as_stimlist(stim)
#   expect_equal(class(stim2[[1]]$img), "magick-image")
# })

# require_tems ----
test_that("require_tems", {
  stimuli <- demo_stim()
  
  # all images have templates
  s <- require_tems(stimuli)
  expect_equal(stimuli, s)
  
  # no images with templates
  notem <- stimuli |> remove_tem()
  expect_error(require_tems(notem))
  
  # some images with templates
  mix <- c(stimuli, notem) |> 
    rename_stim(new_names = c("f_tem", "m_tem", "f_no", "m_no"))
  expect_warning(mix_tem <- require_tems(mix),
                 "Images without templates were removed: f_no, m_no",
                 fixed = TRUE)
  expect_equal(names(mix_tem), c("f_tem", "m_tem"))
  
  # different tems
  multi_tem <- demo_tems()
  mt <- require_tems(multi_tem)
  expect_equal(multi_tem, mt)
  
  expect_error(mt2 <- require_tems(multi_tem, all_same = TRUE))
})
