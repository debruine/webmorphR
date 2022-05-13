# reorder a template to a more sensible order

reorder <- c(34, 88, 1,9,10,11,12,13,14,15,16,2,3,4,5,6,7,8,0,24,23,22,21,20,19,18,32,31,30,29,28,27,26,25,17, 35,41,40,42,39, 37,33,36, 38, 89,95,94,96,93,91,87,90,92, 43,48,49,51,50,46, 47,45,44, 97,102,103,104,105,101, 100,99,98, 72,73,74,86,75,76,77,78,79,80,85,84,83,82,81, 52,64,63,71,67,68,61,65,66,62,70,69,54,60,57,55,56,53,59,58)

fpp <- tem_def("fpp106")

new_pt <- fpp$points
new_pt$n = sapply(new_pt$n, function(x) which(x ==reorder) - 1)
new_pt$sym = sapply(new_pt$sym, function(x) which(x ==reorder) - 1)
new_pt <- dplyr::arrange(new_pt, n)

View(new_pt)
tmp <- tempfile()
readr::write_csv(new_pt, tmp)
readLines(tmp) |> paste(collapse = "\n") |> cat()


new_ln <- lapply(fpp$lines, sapply, sapply, function(x) which(x == reorder) - 1L)

ln <- data.frame(
  n = 0:14,
  linetype = "open",
  color = "default",
  points = sapply(new_ln, paste, collapse = ",")
)
tmp <- tempfile()
readr::write_csv(ln, tmp)
readLines(tmp) |> paste(collapse = "\n") |> cat()



new_mk <- lapply(fpp$mask, lapply, sapply, function(x) which(x == reorder) - 1L)

mk <- data.frame(
  mask = names(fpp$mask),
  points = sapply(new_mk, sapply, paste, collapse = ",") |> sapply(paste, collapse = ";")
)
tmp <- tempfile()
readr::write_csv(mk, tmp)
readLines(tmp) |> paste(collapse = "\n") |> cat()


