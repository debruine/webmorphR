test_that("setnames", {
  a <- demo_stim()
  b <- setnames(a, prefix = "a_")
  expect_equal(names(b)[1], "a_f_multi")

  b <- setnames(a, suffix = "_c")
  expect_equal(names(b)[2], "m_multi_c")

  b <- setnames(a, prefix = "d_", suffix = "_e")
  expect_equal(names(b)[1], "d_f_multi_e")

  b <- setnames(a, new_names = paste0("face", 1:2))
  expect_equal(names(b)[2], "face2")

  b <- setnames(a, pattern = "f_", replacement = "w-") |>
    setnames(pattern = "m_", replacement = "m-")
  expect_equal(names(b), c("w-multi", "m-multi"))
})
