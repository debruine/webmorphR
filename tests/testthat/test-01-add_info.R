stimuli <- demo_stim()
info <- data.frame(
  face_id = c("f_multi", "m_multi"),
  face_age = c(25, 27),
  face_gender = c("female", "male"),
  face_eth = "multi"
)

info1 <- list(
  face_id = "f_multi",
  face_age = 25,
  face_gender = "female",
  face_eth = "multi",
  width = 1350,
  height = 1350,
  tem = 189
)

test_that("table", {
  new <- add_info(stimuli, info)
  expect_equal(new[[1]]$info, info1[1:4])
  info <- get_info(new, .rownames = NULL)
  expect_equal(nrow(info), 2)
  expect_equal(names(info), names(info1))

  gender <- get_info(new[1:2], "face_gender")
  expect_equal(gender, c("f_multi" = "female", "m_multi" = "male"))
})

test_that("by", {
  li <- dplyr::arrange(info, desc(face_age))
  new <- add_info(stimuli, li, .by = "face_id")
  expect_equal(new[[1]]$info, info1[2:4])
})


test_that("vectors", {
  names <- sample(LETTERS, 2)
  new <- add_info(stimuli, name = names)
  expect_equal(new[[1]]$info, list(name = names[[1]]))
})

test_that("len1 vectors", {
  new <- add_info(stimuli, emo = "neutral")
  expect_equal(new[[1]]$info, list(emo = "neutral"))
  expect_equal(new[[2]]$info, list(emo = "neutral"))
})

test_that("order", {
  names <- sample(LETTERS, 2, T)
  id <- c("m_multi", "f_multi")
  new <- add_info(stimuli, id = id, name = names, .by = "id")

  expect_equal(new[[id[[1]]]]$info, list(name = names[[1]]))
  expect_equal(new[[id[[2]]]]$info, list(name = names[[2]]))
})

test_that("partial/no info", {
  stimuli <- demo_stim() |>
    add_info(project = "test", x = 1)

  stimuli$f_multi$info$x <- NULL
  expect_equal(get_info(stimuli, "x"),
               c(f_multi = NA, m_multi = 1))

  stimuli$f_multi$info <- NULL
  expect_equal(get_info(stimuli, "x"),
               c(f_multi = NA, m_multi = 1))
})

test_that("duplicate stim names", {
  stimuli <- demo_stim() |> rep(2)
  s2 <- add_info(stimuli, x = 1:4)
  expect_equal(get_info(s2, "x"),
               c(f_multi = 1, m_multi = 2, f_multi = 3, m_multi = 4))
  
})
