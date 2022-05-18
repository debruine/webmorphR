# readline_check is originally from faux

# Thanks https://stackoverflow.com/users/2752888/znk 
# https://stackoverflow.com/questions/41372146/test-interaction-with-users-in-r-package

f <- file()
wm_opts(connection = f)
on.exit({
  wm_opts(connection = stdin()) # reset connection
  close(f) # close the file
})

test_that("error", {
  expect_error(readline_check(), "argument \"prompt\" is missing, with no default", fixed = TRUE)
})

test_that("interactive", {
  

  q <- list(
    "1" = "Type a number:",
    "-1" = "Type a number less than or equal to 0:",
    "15" = "Type a number from 10 to 20:", 
    "LD" = "Type two characters:",
    "Lisa DeBruine" = "Type at least 3 characters:",
    "LDB" = "Type no more than 4 characters:",
    "D4" = "Type a letter and a number:"
  )
  ans <- paste(names(q), collapse = "\n")
  write(ans, f)

  ol <- capture_output_lines( n <- readline_check(q[[1]], "numeric") )
  expect_true(is.numeric(n))
  expect_equal(ol, q[[1]])
  
  ol <- capture_output_lines( n0 <- readline_check(q[[2]], "numeric", max = 0) )
  expect_true(is.numeric(n0))
  expect_true(n0 <= 0)
  expect_equal(ol, q[[2]])
  
  ol <- capture_output_lines(
    n10 <- readline_check(q[[3]], "numeric", min = 10, max = 20)
  )
  expect_true(is.numeric(n10))
  expect_true(n10 <= 20)
  expect_true(n10 >= 10)
  expect_equal(ol, q[[3]])
  
  ol <- capture_output_lines(
    c2 <- readline_check(q[[4]], "length", min=2, max = 2)
  )
  expect_true(nchar(c2)==2)
  expect_equal(ol, q[[4]])
  
  ol <- capture_output_lines( c3 <- readline_check(q[[5]], "length", min =3) )
  expect_true(nchar(c3) >= 3)
  expect_equal(ol, q[[5]])
  
  ol <- capture_output_lines( c4 <- readline_check(q[[6]], "length", max = 4) )
  expect_true(nchar(c4) <= 4)
  expect_equal(ol, q[[6]])
  
  ol <- capture_output_lines({
    ln <- readline_check(q[[7]], "grep", 
      warning = "\033[31mIncorrect.\033[39m Type only a letter and a number: ", 
      pattern = "^[a-zA-Z]\\d$")
  })
  expect_equal(grep("^[a-zA-Z]\\d$", ln), 1)
  expect_equal(ol, q[[7]])
})

test_that("repeats", {
  # numeric input
  list("A", "1.500") %>% paste(collapse = "\n") %>% write(f)
  ol <- capture_output_lines({ x <- readline_check("", "numeric") })
  err <- "\033[31mError:\033[39m The input must be a number:"
  expect_equal(x, 1.5)
  expect_equal(ol[[2]], err)
  
  # min and max limits
  list("-20", "20", "0") %>% paste(collapse = "\n") %>% write(f)
  ol <- capture_output_lines({ x <- readline_check("", "numeric", min = -10, max = 10) })
  err <- "\033[31mError:\033[39m The input must be a number between -10 and 10:"
  expect_equal(x, 0)
  expect_equal(ol[[2]], err)
  expect_equal(ol[[3]], err)
  
  # integer input
  list("1.500", "2") %>% paste(collapse = "\n") %>% write(f)
  ol <- capture_output_lines({ x <- readline_check("", "integer") })
  err <- "\033[31mError:\033[39m The input must be an integer:"
  expect_equal(x, 2)
  expect_equal(ol[[2]], err)
  
  # min and max limits
  list("-20", "20", "1") %>% paste(collapse = "\n") %>% write(f)
  ol <- capture_output_lines({ x <- readline_check("", "integer", min = -10, max = 10) })
  err <- "\033[31mError:\033[39m The input must be an integer between -10 and 10:"
  expect_equal(x, 1)
  expect_equal(ol[[2]], err)
  expect_equal(ol[[3]], err)
  
  # length with min/max
  list("A", "AAAA", "AA") %>% paste(collapse = "\n") %>% write(f)
  ol <- capture_output_lines({ x <- readline_check("", "length", min = 2, max = 3) })
  err <- "\033[31mError:\033[39m The input must be between 2 and 3 characters long:"
  expect_equal(x, "AA")
  expect_equal(ol[[2]], err)
  expect_equal(ol[[3]], err)
  
  # length with no min
  list("1234567", "AA") %>% paste(collapse = "\n") %>% write(f)
  ol <- capture_output_lines({ x <- readline_check("", "length", max = 5) })
  err <- "\033[31mError:\033[39m The input must be between 0 and 5 characters long:"
  expect_equal(x, "AA")
  expect_equal(ol[[2]], err)
  
  # grep
  err <- "\033[31mError:\033[39m The input is incorrect:"
  list("a", "bA", "Alter") %>% paste(collapse = "\n") %>% write(f)
  pattern <- "^(A|B)"
  ol <- capture_output_lines({ x <- readline_check("", "grep", pattern = pattern) })
  expect_equal(x, "Alter")
  expect_equal(ol[[2]], err)
  expect_equal(ol[[3]], err)
  
  list("x", "xxxxxx", "xxxx") %>% paste(collapse = "\n") %>% write(f)
  pattern <- "^x{2,5}$"
  ol <- capture_output_lines({ x <- readline_check("", "grep", pattern = pattern) })
  expect_equal(x, "xxxx")
  expect_equal(ol[[2]], err)
  expect_equal(ol[[3]], err)
})
