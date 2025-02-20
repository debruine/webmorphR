test_that("rename_stim", {
  a <- demo_stim()
  b <- rename_stim(a, prefix = "a_")
  expect_equal(names(b)[1], "a_f_multi")

  b <- rename_stim(a, suffix = "_c")
  expect_equal(names(b)[2], "m_multi_c")

  b <- rename_stim(a, prefix = "d_", suffix = "_e")
  expect_equal(names(b)[1], "d_f_multi_e")

  b <- rename_stim(a, new_names = paste0("face", 1:2))
  expect_equal(names(b)[2], "face2")

  b <- rename_stim(a, pattern = "f_", replacement = "w-") |>
    rename_stim(pattern = "m_", replacement = "m-")
  expect_equal(names(b), c("w-multi", "m-multi"))
})
