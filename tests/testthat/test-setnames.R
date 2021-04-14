test_that("setnames", {
  a <- demo_stim("composite")
  b <- setnames(a, prefix = "a_")
  expect_equal(names(b)[1], "a_f_african")

  b <- setnames(a, suffix = "_c")
  expect_equal(names(b)[2], "f_easian_c")

  b <- setnames(a, prefix = "d_", suffix = "_e")
  expect_equal(names(b)[3], "d_f_multi_e")

  b <- setnames(a, new_names = paste0("face", 1:10))
  expect_equal(names(b)[4], "face4")

  b <- setnames(a, pattern = "f_", replacement = "w-") %>%
    setnames(pattern = "m_", replacement = "m-")
  expect_equal(names(b)[5:6], c("w-white", "m-african"))
})
