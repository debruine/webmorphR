# create multi-template versions
# frl <- read_stim("inst/extdata/tem_examples/", "frl")
# fpp106 <- auto_delin(frl, "fpp106", replace = TRUE)
# fpp83 <- auto_delin(frl, "fpp83", replace = TRUE)
# dlib7 <- auto_delin(frl, "dlib7", replace = TRUE)
# dlib70 <- auto_delin(frl, "dlib70", replace = TRUE)
# write_stim(fpp106, "inst/extdata/tem_examples/", "fpp106", "jpg")
# write_stim(fpp83, "inst/extdata/tem_examples/", "fpp83", "jpg")
# write_stim(dlib7, "inst/extdata/tem_examples/", "dlib7", "jpg")
# write_stim(dlib70, "inst/extdata/tem_examples/", "dlib70", "jpg")

test_that("guess", {
  demo <- demo_stim()
  expect_silent(convert_tem(demo))
               
  no_tem <- remove_tem(demo)
  expect_error(convert_tem(no_tem), 
               "No images had templates",
               fixed = TRUE)
  
  odd_tem <- subset_tem(demo, features("gmm"))
  expect_error(convert_tem(odd_tem),
               "Some images don't have a recognised template: f_multi, m_multi",
               fixed = TRUE)
})

# convert_tem ----
test_that("convert_tem", {
  tem_examples <- demo_stim("tem_examples") %>% resize(400)
  
  # convert a stimlist with different tems
  dlib7 <- convert_tem(tem_examples, to = "dlib7")
  expect_equal(get_info(dlib7)$tem |> unname(), rep(7, 5))
  
  # check every pairing
  for (stim_id in c("frl", "fpp106", "fpp83", "dlib7", "dlib70")) {
    stimuli <- tem_examples[stim_id]
    
    for (tem_id in c("frl", "fpp106", "fpp83", "dlib7", "dlib70")) {
      if (stim_id == tem_id) next # skip matches
      
      temdef <- tem_def(tem_id)
      newtem <- convert_tem(stimuli, stim_id, tem_id)
      
      stim_pts <- newtem[[1]]$points
      stim_lines <- newtem[[1]]$lines
      
      expect_equal(ncol(stim_pts), nrow(temdef$points))
      expect_equal(dimnames(stim_pts)[[2]], temdef$points$name)
      expect_equal(stim_lines, temdef$lines)
      
      #expect_equal(get_point(stimuli, 0:1), get_point(newtem, 0:1))
    }
  }
})

# convert_tem self----
test_that("convert_tem self", {
  tem_examples <- demo_stim("tem_examples")
  dlib7 <- convert_tem(tem_examples$dlib7, "guess", "dlib7")
  expect_equal(dlib7[[1]]$points, tem_examples$dlib7$points)
  
  dlib70 <- convert_tem(tem_examples$dlib70, "guess", "dlib70")
  expect_equal(dlib70[[1]]$points, tem_examples$dlib70$points)
  
  frl <- convert_tem(tem_examples$frl, "guess", "frl")
  expect_equal(frl[[1]]$points, tem_examples$frl$points)
  
  fpp106 <- convert_tem(tem_examples$fpp106, "guess", "fpp106")
  expect_equal(fpp106[[1]]$points, tem_examples$fpp106$points)
  
  fpp83 <- convert_tem(tem_examples$fpp83, "guess", "fpp83")
  expect_equal(fpp83[[1]]$points, tem_examples$fpp83$points)
})

# convert_tem exact ----
test_that("convert_tem exact", {
  tem_examples <- demo_stim("tem_examples") %>% resize(0.5)
  
  td <- tem_def("dlib7")
  for (tem in c("frl", "fpp106", "fpp83", "dlib70")) {
    x <- convert_tem(tem_examples[tem], "guess", "dlib7")
    new_pt <- x[[1]]$points
    trans_pt <- unlist(td$points[paste0(tem, ".x")] + 1)
    orig_pt <- tem_examples[[tem]]$points[, trans_pt]
    expect_equivalent(new_pt, orig_pt)
  }
})

# convert_tem visual ----
test_that("convert_tem visual", {
  skip("requires visual checks")
  tem_examples <- demo_stim("tem_examples") %>% resize(0.5)
  
  convert_tem(tem_examples$frl, "frl", "dlib7") %>% draw_tem(pt.size = 20, pt.shape = "index")
  convert_tem(tem_examples$fpp106, "fpp106", "dlib7") %>% draw_tem()
  convert_tem(tem_examples$fpp83, "fpp83", "dlib7") %>% draw_tem()
  convert_tem(tem_examples$dlib70, "dlib70", "dlib7") %>% draw_tem()
  convert_tem(tem_examples$dlib7, "dlib7", "dlib7") %>% draw_tem()
  
  convert_tem(tem_examples$frl, "frl", "dlib70") %>% draw_tem()
  convert_tem(tem_examples$fpp106, "fpp106", "dlib70") %>% draw_tem()
  # convert_tem(tem_examples$fpp83, "fpp83", "dlib70") %>% draw_tem()
  # convert_tem(tem_examples$dlib70, "dlib70", "dlib70") %>% draw_tem()
  # convert_tem(tem_examples$dlib7, "dlib7", "dlib70") %>% draw_tem()
})
