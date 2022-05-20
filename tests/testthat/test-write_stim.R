test_that("works", {
  stimuli <- demo_stim()[1]
  dir <- file.path(tempdir(), "ftest")

  # default format ----
  write_stim(stimuli, dir)
  expect_equal(list.files(dir), c("f_multi.png", "f_multi.tem"))
  unlink(dir, recursive = TRUE)
  
  # rename ----
  write_stim(stimuli, dir, "newname")
  expect_equal(list.files(dir), c("newname.png", "newname.tem"))
  unlink(dir, recursive = TRUE)
  
  # too many names ----
  write_stim(stimuli, dir, c("newname", "newname2"))
  expect_equal(list.files(dir), c("newname.png", "newname.tem"))
  unlink(dir, recursive = TRUE)
  
  write_stim(demo_stim(), dir, "newname")
  expect_equal(list.files(dir), c("newname_1.png", "newname_1.tem", "newname_2.png", "newname_2.tem"))
  unlink(dir, recursive = TRUE)
  
  write_stim(demo_stim(), dir, c("A", "B"))
  expect_equal(list.files(dir), c("A.png", "A.tem", "B.png", "B.tem"))
  unlink(dir, recursive = TRUE)

  # formats ----
  write_stim(stimuli, dir, format = "PNG")
  expect_equal(list.files(dir), c("f_multi.png", "f_multi.tem"))
  unlink(dir, recursive = TRUE)

  write_stim(stimuli, dir, format = "jpg")
  expect_equal(list.files(dir), c("f_multi.jpg", "f_multi.tem"))
  unlink(dir, recursive = TRUE)

  write_stim(stimuli, dir, format = "jpeg")
  expect_equal(list.files(dir), c("f_multi.jpg", "f_multi.tem"))
  unlink(dir, recursive = TRUE)

  write_stim(stimuli, dir, format = "GIF")
  expect_equal(list.files(dir), c("f_multi.gif", "f_multi.tem"))
  unlink(dir, recursive = TRUE)
})

# no tems ----
test_that("no tems", {
  stimuli <- demo_stim() |> remove_tem()
  dir <- tempfile()
  
  x <- write_stim(stimuli, dir)
  expect_equal(list.files(dir), c("f_multi.png", "m_multi.png"))
  expect_null(x[[1]])
  expect_null(x[[3]])
  expect_equal(x[[2]], file.path(dir, "f_multi.png"))
  expect_equal(x[[4]], file.path(dir, "m_multi.png"))
  
  unlink(dir, recursive = TRUE)
})

# no-line tems ----
test_that("no-line tems", {
  dlib7 <- demo_stim("tem_examples", "dlib7")
  d <- tempdir()
  write_stim(dlib7, d)
  
  # make sure 0 is appended
  x <- readLines(file.path(d, "dlib7.tem"))
  expect_equal(9, length(x))
  expect_equal("7", x[[1]])
  expect_equal("0", x[[9]])
})

# overwrite ----
test_that("overwrite", {
  dir <- file.path(tempdir(), "overwritetest")
  
  stim <- demo_stim() |> rename_stim(c("A", "B"))
  files <- write_stim(stim, dir)
  ftime <- unlist(files) |> sapply(file.mtime)
  expect_equal(length(files), 4)
  
  # don't overwrite
  ofiles <- write_stim(stim, dir, overwrite = FALSE)
  expect_null(unlist(ofiles))
  otime <- unlist(files) |> sapply(file.mtime)
  expect_equal(unname(ftime == otime), rep(TRUE, 4))
  
  # do overwrite
  ofiles <- write_stim(stim, dir, overwrite = TRUE)
  expect_equal(ofiles, files)
  otime <- unlist(files) |> sapply(file.mtime)
  expect_equal(unname(ftime == otime), rep(FALSE, 4))
  
  # clean up
  unlink(dir, recursive = TRUE)
})

# interactive ----
test_that("interactive", {
  skip_if_not(interactive())
  
  dir <- file.path(tempdir(), "interactivetest")
  
  stim <- demo_stim() |> rename_stim(c("A", "B"))
  files <- write_stim(stim, dir)
  ftime <- unlist(files) |> sapply(file.mtime)
  expect_equal(length(files), 4)
  
  # set up interactive answers
  f <- file()
  options(webmorph.connection = f)
  on.exit({
    options(webmorph.connection = stdin()) # reset connection
    close(f) # close the file
  })
  
  # ask for all 4 files (and skip)
  lines <- c("1", "1", "1", "1")
  ans <- paste(lines, collapse = "\n")
  write(ans, f)
  
  ol <- capture_output_lines({ afiles <- write_stim(stim, dir, ask = TRUE) })
  expect_equal(length(ol), 5*4)
  atime <- unlist(files) |> sapply(file.mtime)
  expect_equal(atime, ftime)
  expect_null(unlist(afiles))
  
  # ask for all 4 files (and write over tems)
  lines <- c("1", "2", "1", "2")
  ans <- paste(lines, collapse = "\n")
  write(ans, f)
  
  ol <- capture_output_lines({ afiles <- write_stim(stim, dir, ask = TRUE) })
  expect_equal(length(ol), 5*4)
  atime <- unlist(files) |> sapply(file.mtime)
  expect_equal(unname(atime == ftime), c(F, T, F, T))
  expect_equal(afiles[1, ], files[1, ])
  expect_equal(afiles[2, ], list(A = NULL, B = NULL))
  
  # ask for 1st file (and skip rest)
  lines <- c("3")
  ans <- paste(lines, collapse = "\n")
  write(ans, f)
  
  ol <- capture_output_lines({ bfiles <- write_stim(stim, dir, ask = TRUE) })
  expect_equal(length(ol), 5)
  btime <- unlist(files) |> sapply(file.mtime)
  expect_equal(atime, btime)
  expect_null(unlist(bfiles))
  
  # ask for 1st file (and overwite rest)
  lines <- c("4")
  ans <- paste(lines, collapse = "\n")
  write(ans, f)
  
  ol <- capture_output_lines({ bfiles <- write_stim(stim, dir) })
  expect_equal(length(ol), 5)
  btime <- unlist(files) |> sapply(file.mtime)
  expect_equal(unname(atime == btime), rep(F, 4))
  expect_equal(bfiles, files)
  
  # clean up
  unlink(dir, recursive = TRUE)
})


# format from name ----
test_that("format from name", {
  s <- demo_stim() |> 
    rename_stim(suffix = c(".gif", ".jpg"))
  
  imgnames <- names(s)
  dir <- tempfile()
  
  write_stim(s, dir)
  f <- list.files(dir)
  expect_true(all(imgnames %in% f))
  
  x <- read_stim(dir)
  expect_equal(magick::image_info(x$f_multi$img)$format, "GIF")
  expect_equal(magick::image_info(x$m_multi$img)$format, "JPEG")
  
  ## works with different case or JPEG
  s <- demo_stim() |> 
    rename_stim(suffix = c(".JPG", ".JPEG"))
  
  imgnames <- names(s)
  dir <- tempfile()
  
  write_stim(s, dir)
  f <- list.files(dir)
  expect_true("f_multi.jpg" %in% f)
  expect_true("m_multi.jpg" %in% f)
  
  x <- read_stim(dir)
  expect_equal(magick::image_info(x$f_multi$img)$format, "JPEG")
  expect_equal(magick::image_info(x$m_multi$img)$format, "JPEG")
  
  ## defaults to PNG if not jpg/jpeg/gif/png
  ## works with different case or JPEG
  s <- demo_stim() |> 
    rename_stim(suffix = c(".bmp", ".webp"))
  
  imgnames <- names(s)
  dir <- tempfile()
  
  write_stim(s, dir)
  f <- list.files(dir)
  expect_true("f_multi.bmp.png" %in% f)
  expect_true("m_multi.webp.png" %in% f)
  
  x <- read_stim(dir)
  expect_equal(magick::image_info(x[[1]]$img)$format, "PNG")
  expect_equal(magick::image_info(x[[2]]$img)$format, "PNG")
})
