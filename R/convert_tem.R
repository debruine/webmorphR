convert_tem <- function(stimuli, from = "fpp106", to = "FRL") {
  stimuli <- validate_stimlist(stimuli, TRUE)
  
  from_tem <- tem_def(from)
  to_tem <- tem_def(to)
  
  pt <- array(c(to_tem$points$x, to_tem$points$y), 
              c(nrow(to_tem$points), 2),
              dimnames = list(
                to_tem$points$name,
                c("x", "y")
              )) %>% t()
  
  
  for (i in seq_along(stimuli)) {
    stimuli[[i]]$lines <- to_tem$lines
    stimuli[[i]]$closed <- to_tem$closed
    stimuli[[i]]$points <- pt
  }
  
  stimuli
}