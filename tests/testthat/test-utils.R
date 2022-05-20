# concatenate c() ----
test_that("c", {
  path <- system.file("extdata/test/", package = "webmorphR")
  a <- read_stim(path, "multi")
  b <- read_stim(path, "f_")

  # temlists
  x <- c(a, b)
  expect_equal(names(x), c("f_multi", "m_multi", "f_multi"))

  # individual tems
  x <- c(a[[2]], b[[1]])
  expect_equal(names(x), c("m_multi", "f_multi"))

  # mixed tems and temlists
  x <- c(a[[2]], b)
  expect_equal(names(x), c("m_multi", "f_multi"))
})

test_that("rep", {
  a <- demo_stim()

  x <- rep(a[[1]], 3)
  expect_equal(length(x), 3)
  expect_equal(names(x), rep("f_multi", 3))

  x <- rep(a, 3)
  expect_equal(length(x), 6)
  expect_equal(names(x), rep(c("f_multi", "m_multi"), 3))

  x <- rep(a, times = 3)
  expect_equal(length(x), 6)
  expect_equal(names(x), rep(c("f_multi", "m_multi"), times = 3))
})

# extract [] ----
test_that("[", {
  x <- demo_stim()
  f1 <- x[1]
  expect_equal(class(f1), c("stimlist", "list"))
  expect_equal(length(f1), 1)

  f_rev <- x[c(2, 1)]
  expect_equal(class(f_rev), c("stimlist", "list"))
  expect_equal(length(f_rev), 2)
  expect_equal(names(f_rev), c("m_multi", "f_multi"))

  m <- x["m_multi"]
  expect_equal(class(m), c("stimlist", "list"))
  expect_equal(names(m), "m_multi")
})

# xget ----
test_that("xget", {
  # vectors
  x <- c(a = 1, b = 2)
  expect_equal(xget(x, "A", "a"), 1)
  expect_equal(xget(x, 2, "a"), 2)
  expect_equal(xget(x, "b", "a"), 2)
  expect_equal(xget(x, "B", "nope", .default = 3), 3)
  expect_null(xget(x, "A", "B"))

  # lists
  x <- list(a = 1, b = 2)
  expect_equal(xget(x, "A", "a"), 1)
  expect_equal(xget(x, 2, "a"), 2)
  expect_equal(xget(x, "b", "a"), 2)
  expect_equal(xget(x, "B", "A", .default = 3), 3)
  expect_null(xget(x, "A", "B"))

  # more complex lists
  x <- list(a = 1, b = TRUE, c = list(1, 2))
  expect_equal(xget(x, "A", "a"), 1)
  expect_equal(xget(x, 2, "a"), TRUE)
  expect_equal(xget(x, "c", "a"), list(1, 2))
  expect_equal(xget(x, "B", "A", .default = 3), 3)
  expect_null(xget(x, "A", "B"))
})

# subset ----
test_that("subset", {
  stimuli <- demo_stim() |>
    rep(2) |>
    add_info(x = 1:4)

  f <- subset(stimuli, "f_")
  m <- subset(stimuli, "^m")
  x2 <- subset(stimuli, x < 3)
  x3 <- subset(stimuli, x %in% c(1,3))
  odd <- subset(stimuli, x%%2 == 1)
  n <- subset(stimuli, 2)

  expect_equal(names(f), names(stimuli)[c(1,3)])
  expect_equal(names(m), names(stimuli)[c(2, 4)])
  expect_equal(names(x2), names(stimuli)[1:2])
  expect_equal(names(x3), names(stimuli)[c(1,3)])
  expect_equal(names(odd), names(stimuli)[c(1,3)])
  expect_equal(names(n), names(stimuli)[2])
})


# get_imgs ----
test_that("get_imgs", {
  stimuli <- demo_stim()
  imgs <- get_imgs(stimuli)

  expect_equal(class(imgs), "magick-image")
  expect_equal(length(imgs), 2L)
})

# format_size ----
test_that("format_size", {
  expect_equal(format_size(0), "0 bytes")
  expect_equal(format_size(1), "1 bytes")
  expect_equal(format_size(1024), "1 Kb")
  expect_equal(format_size(1024^2), "1 Mb")
  expect_equal(format_size(1024^3), "1 Gb")
  expect_equal(format_size(1024^4), "1 Tb")
  expect_equal(format_size(1024^5), "1 Pb")
  expect_equal(format_size(1024^6), "1024 Pb")
  
  # rounding
  mult <- 1.12345432123454321
  expect_equal(format_size(1*mult), "1.1 bytes")
  expect_equal(format_size(1024^2 * mult), "1.1 Mb")
})

# message ----
test_that("message", {
  expect_message( message("hi"), "hi" )
  
  wm_opts("verbose" = FALSE)
  expect_silent( message("hi") )
  
  wm_opts("verbose" = TRUE)
  expect_message( message("hi"), "hi" )
  
})

# print.stim ----
test_that("print.stim", {
  stimuli <- demo_stim()
  
  expect_silent(print(stimuli$f_multi))
  expect_silent(print(stimuli))
  expect_silent(print(stimuli[[1]]))
})

