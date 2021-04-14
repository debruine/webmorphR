path <- system.file("extdata/composite", package = "webmorphR")
nm <- c("f_african", "f_easian", "f_multi", "f_wasian", "f_white",
        "m_african", "m_easian", "m_multi", "m_wasian", "m_white")
temnames <- c("tempath", "points", "lines", "closed")
imgnames <- c("img","imgpath", "desc", "width", "height")

test_that("error", {
  expect_error(read_stim(), 'argument "path" is missing, with no default', fixed= TRUE)
})

test_that("basic", {
  stim <- read_stim(path)
  expect_equal(class(stim), c("stimlist", "list"))
  expect_equal(length(stim), 10)
  expect_equal(names(stim), nm)
  expect_equal(class(stim[[1]]), c("stim", "list"))
  expect_equal(names(stim[[1]]), c(imgnames, temnames))
  expect_equal(class(stim[[1]]$img), "magick-image")
  expect_equal(stim[[1]]$imgpath, file.path(path, "f_african.jpg"))
  expect_equal(stim[[1]]$tempath, file.path(path, "f_african.tem"))
  expect_equal(stim[[1]]$width, 1350)
  expect_equal(stim[[1]]$height, 1350)
  expect_equal(ncol(stim[[1]]$points), 189)
  expect_equal(nrow(stim[[1]]$points), 2)
  expect_equal(length(stim[[1]]$lines), 44)
  expect_equal(length(stim[[1]]$closed), 44)
})

test_that("img only", {
  stim <- read_stim(path, "\\.jpg$")
  expect_equal(length(stim), 10)
  expect_equal(names(stim), nm)
  expect_equal(names(stim[[1]]), imgnames)
  expect_equal(class(stim[[1]]$img), "magick-image")
  expect_equal(stim[[1]]$imgpath, file.path(path, "f_african.jpg"))
  expect_equal(stim[[1]]$width, 1350)
  expect_equal(stim[[1]]$height, 1350)
})

test_that("tem only", {
  stim <- read_stim(path, "\\.tem$")
  expect_equal(length(stim), 10)
  expect_equal(names(stim), nm)
  expect_equal(names(stim[[1]]), temnames)
  expect_equal(stim[[1]]$tempath, file.path(path, "f_african.tem"))
  expect_equal(ncol(stim[[1]]$points), 189)
  expect_equal(nrow(stim[[1]]$points), 2)
  expect_equal(length(stim[[1]]$lines), 44)
  expect_equal(length(stim[[1]]$closed), 44)
})

test_that("mixed img/tem", {
  stim <- read_stim(path, "f_(african\\.tem|easian|white.jpg)")
  expect_equal(class(stim[[1]]), c("stim", "list"))
  expect_equal(names(stim), c("f_easian", "f_white", "f_african"))
  expect_equal(names(stim$f_african), temnames)
  expect_equal(names(stim$f_white), imgnames)
  expect_equal(names(stim$f_easian), c(imgnames, temnames))
})

test_that("multiple files", {
  paths <- paste0(path, "/", c("f_easian", "f_white", "f_african"), ".jpg")
  stim <- read_stim(paths)

  expect_equal(names(stim), c("f_easian", "f_white", "f_african"))
  expect_equal(names(stim$f_white), imgnames)
})

test_that("by index", {
  stim1 <- read_stim(path, 1)
  expect_equal(names(stim1), nm[1])
  expect_equal(dim(stim1$f_african$points), c(2, 189))

  stim2 <- read_stim(path, 2)
  expect_equal(names(stim2), nm[2])

  stim3 <- read_stim(path, 3:5)
  expect_equal(names(stim3), nm[3:5])

  stim4 <- read_stim(path, c(1,3,5))
  expect_equal(names(stim4), nm[c(1,3,5)])
})
