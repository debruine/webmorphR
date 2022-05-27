#' Average Images
#'
#' @param stimuli list of stimuli to average 
#' @param texture logical; whether textured should be averaged
#' @param norm how to normalise
#' @param normpoint points for twopoint normalisation
#'
#' @return list of stimuli with the average image and template
#' @export
#' @family webmorph
#'
#' @examples
#' \dontrun{
#'   avg <- demo_stim() |> avg()
#' }
avg <- function(stimuli,
                texture = TRUE,
                norm = c("none", "twopoint", "rigid"),
                normpoint = 0:1) {
  stimuli <- require_tems(stimuli, TRUE)
  if (length(stimuli) > 100) {
    stop("We can't average more than 100 images at a time. You can create sub-averages with equal numbers of faces and average those together.")
  }
  
  norm <- match.arg(norm)
  format <- "jpg" #match.arg(format)
  
  # save images locally
  tdir <- tempfile()
  files <- write_stim(stimuli, tdir, format = "jpg") |> unlist()
  upload <- lapply(files, httr::upload_file)
  names(upload) <- sprintf("upload[%d]", 0:(length(upload)-1))
  
  settings <- list(
    texture = ifelse(isTRUE(as.logical(texture)), "true", "false"),
    norm = norm,
    normPoint0 = normpoint[[1]],
    normPoint1 = normpoint[[2]],
    format = format
  )
  
  # send request to webmorph and handle zip file
  ziptmp <- paste0(tdir, "/avg.zip")
  httr::timeout(30 + 10*length(stimuli))
  httr::set_config( httr::config( ssl_verifypeer = 0L ) )
  url <- paste0(wm_opts("server"), "/scripts/webmorphR_avg")
  r <- httr::POST(url, body = c(upload, settings) ,
                  httr::write_disk(ziptmp, TRUE))
  
  utils::unzip(ziptmp, exdir = paste0(tdir, "/avg"))
  #resp <- httr::content(r)
  
  avg <- paste0(tdir, "/avg") |>
    read_stim() |>
    rename_stim("avg")
  unlink(tdir, recursive = TRUE) # clean up temp directory
  
  avg
}