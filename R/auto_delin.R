#' Auto-Delineation
#'
#' Automatically delineate faces using Face++ (an external service). Since each delineation counts against a daily limit, you need to set up your own Face++ account (see details below).
#'
#' @details 
#' To use Face++ auto-delineation, you need to get your own free API key from https://www.faceplusplus.com. After signing up for an account, go to https://console.faceplusplus.com/app/apikey/list and request a free API key. Add the key and secret to your .Renviron file as follows:
#'
#' FACEPLUSPLUS_KEY="1234567890abcdefghijk"
#'
#' FACEPLUSPLUS_SECRET="1234567890abcdefghijk"
#'
#' @param stimuli list of stimuli
#' @param model Which model (fpp106, fpp83)
#' @param replace logical; whether to replace original templates - if FALSE, only gets templates for images with no template
#' @param face which face to delineate in each image if there is more than 1
#'
#' @return list of stimuli with templates
#' @export
#' @family tem
#'
#' @examples
#' \dontrun{
#' # requires an API key in .Renviron
#' auto_fpp106 <- demo_stim() |>
#'   auto_delin(model = "fpp106", replace = TRUE)
#' }
auto_delin <- function(stimuli, 
                           model = c("fpp106", "fpp83"), 
                           replace = FALSE, 
                           face = 1) {
  stimuli <- as_stimlist(stimuli)
  model <- match.arg(model)
  
  # find out which stimuli need tems ----
  if (isTRUE(replace)) stimuli <- remove_tem(stimuli)
  
  notems <- sapply(stimuli, `[[`, "points") |> sapply(is.null)
  
  if (all(notems == FALSE)) {
    warning("No images needed templates; set replace = TRUE to replace existing templates")
    return(stimuli)
  }
  
  # check for required things ----

  ## face++ version ----
  url <- "https://api-us.faceplusplus.com/facepp/v3/detect"
  key <- Sys.getenv("FACEPLUSPLUS_KEY")
  secret <- Sys.getenv("FACEPLUSPLUS_SECRET")
  if (key == "" || secret == "") {
    stop("You need to set FACEPLUSPLUS_KEY and FACEPLUSPLUS_SECRET in your .Renviron (see ?auto_delin)")
  }
  
  data <- list(
    'api_key' = key,
    'api_secret' = secret,
    'return_landmark' = ifelse(model == "fpp106", 2, 1),
    'image_file' = NULL
  )
  
  # save images to temp file ----
  tempdir <- tempfile()
  paths <- stimuli |>
    remove_tem() |>
    write_stim(tempdir, format = "jpg", overwrite = TRUE) |>
    unlist()
  
  face <- rep(face, length.out = length(stimuli))
  
  if (wm_opts("verbose")) {
    pb <- progress::progress_bar$new(
      total = length(stimuli), clear = FALSE,
      format = "Autodelineating [:bar] :current/:total :elapsedfull"
    )
    pb$tick(0)
    Sys.sleep(0.5)
    pb$tick(0)
  }
  
  # get line definitions 
  fpp <- tem_def(model)
  
  ## Face++ model ----
  for (i in seq_along(stimuli)) {
    imgname <- names(stimuli)[[i]]
    data$image_file <- file.path(tempdir, paste0(imgname, ".jpg")) |>
      httr::upload_file()
    
    r <- httr::POST(url, body = data)
    resp <- httr::content(r)
    
    if (!is.null(resp$error_message)) {
      e <- resp$error_message |>
        paste(collapse = "\n")
      e <- paste0(imgname, ": ", e)
      warning(e, call. = FALSE)
    } else {
      # put in order from fpp
      which_face <- face[i]
      if (length(resp$faces) < which_face) {
        warning(imgname, " did not have ", which_face, " faces", call. = FALSE)
        which_face <- 1
      }
      alpha_order <- resp$faces[[which_face]]$landmark
      order <- sapply(fpp$points$name, function(x) {
        which(x == names(alpha_order))
      })
      landmarks <- alpha_order[order]
      
      # set up points matrix
      x <- sapply(landmarks, `[[`, "x")
      y <- sapply(landmarks, `[[`, "y")
      dnames <- list(c("x", "y"), names(landmarks))
      stimuli[[i]]$points <- matrix(c(x, y), nrow = 2, byrow = TRUE, dimnames = dnames)
      
      # set up lines from fpp
      stimuli[[i]]$lines <- fpp$lines
      stimuli[[i]]$closed <- fpp$closed
    }
    
    if (wm_opts("verbose")) pb$tick()
  }
  
  stimuli
}
