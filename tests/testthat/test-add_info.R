stimuli <- demo_stim("london", 1:3)
info1 <- list(face_id = "001_03",
              face_age = 24,
              face_gender = "female",
              face_eth = "white",
              width = 1350,
              height = 1350,
              tem = 189)

test_that("table", {
  new <- add_info(stimuli, london_info[1:3, ])
  expect_equal(new$`001_03`$info, info1[1:4])
  info <- get_info(new, .rownames = NULL)
  expect_equal(nrow(info), 3)
  expect_equal(names(info), names(info1))

  gender <- get_info(new[1:2], "face_gender")
  expect_equal(gender, c("001_03" = "female", "002_03" = "female"))
})

test_that("by", {
  li <- london_info
  li <- dplyr::arrange(li, face_age)
  new <- add_info(stimuli, li, .by = "face_id")
  expect_equal(new$`001_03`$info, info1[2:4])
})


test_that("vectors", {
  names <- sample(LETTERS, 3)
  new <- add_info(stimuli, name = names)
  expect_equal(new$`001_03`$info, list(name = names[[1]]))
})

test_that("len1 vectors", {
  new <- add_info(stimuli, emo = "neutral")
  expect_equal(new$`001_03`$info, list(emo = "neutral"))
  expect_equal(new$`002_03`$info, list(emo = "neutral"))
})

test_that("order", {
  names <- sample(LETTERS, 3, T)
  id <- sample(london_info$face_id[1:3])
  new <- add_info(stimuli, id = id, name = names, .by = "id")

  expect_equal(new[[id[[1]]]]$info, list(name = names[[1]]))
  expect_equal(new[[id[[2]]]]$info, list(name = names[[2]]))
  expect_equal(new[[id[[3]]]]$info, list(name = names[[3]]))
})

test_that("partial/no info", {
  stimuli <- demo_stim("test") %>%
    add_info(project = "test", x = 1)

  stimuli$f_multi$info$x <- NULL
  expect_equal(get_info(stimuli, "x"),
               c(f_multi = NA, m_multi = 1))

  stimuli$f_multi$info <- NULL
  expect_equal(get_info(stimuli, "x"),
               c(f_multi = NA, m_multi = 1))
})
