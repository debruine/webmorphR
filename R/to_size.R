#' Resize and crop images to a specified size
#'
#' @param stimuli of class stimlist
#' @param width the target width (or a vector of width and height)
#' @param height the target height (or null if width is dimensions)
#' @param fill background color if cropping goes outside the original image
#' @param patch whether to use the patch function to set the background color
#' @param keep_rels whether to keep the size relationships between images in the set, or make all the maximum size
#'
#' @return stimlist with cropped tems and/or images
#' @export
#'
#' @examples
#'
#' # make images with different aspect ratios and sizes
#' stimuli <- demo_stim() %>% crop(c(0.8, 1.0)) %>% resize(c(1.0, 0.5))
#'
#' to_size(stimuli, 300, 400, fill = "dodgerblue") %>% plot()
#' to_size(stimuli, 300, 400, fill = "dodgerblue", keep_rels = TRUE) %>% plot()
#'
#'
to_size <- function(stimuli, width, height = NULL,
                    fill = wm_opts("fill"), patch = FALSE,
                    keep_rels = FALSE) {
  # process width and height
  if (length(width) == 2 && is.null(height)) {
    dim <- width
    width <- xget(dim, "width", "w", 1)
    height <- xget(dim, "height", "h", 2)
  }

  if (!is.numeric(width) || !is.numeric(height)) {
    stop("width and height must be numeric")
  } else if (any(width < 1) || any(height < 1)) {
    stop("width and height must be positive numbers")
  }

  w <- width(stimuli)
  h <- height(stimuli)

  if (keep_rels) {
    # resize all the same %
    w_pcnt <- width / max(w)
    h_pcnt <- height / max(h)
    pcnt <- min(c(w_pcnt, h_pcnt))
  } else {
    # resize each to fit
    w_pcnt <- width / w
    h_pcnt <- height / h
    pcnt <- mapply(min, w_pcnt, h_pcnt)
  }

  resized <- resize(stimuli, pcnt)

  crop(resized, width, height, fill = fill, patch = patch)
}




#' Social Media Image Sizes
#'
#' A convenience function for getting recommended dimensions for images on social media sites.
#'
#' Twitter:
#'
#' link: Image from a Tweet with shared link
#' one: Tweet sharing a single image (default)
#' two: Tweet sharing two images
#' three_left: Tweet sharing three images, Left image
#' three_tight Tweet sharing three images, Right images
#' four: Tweet sharing four images
#'
#' Instagram:
#'
#' feed_large: (default)
#' feed_small:
#' stories_large:
#' stories_small:
#'
#' @param platform currently only "twitter"
#' @param type which type of image
#'
#' @return named vector of width and height in pixels
#' @export
#'
#' @examples
#' social_media_size("twitter", "link")
#' social_media_size("twitter", "one")
#' social_media_size("twitter", "two")
#'
social_media_size <- function(platform = c("twitter", "instagram"),
                              type = "default") {

  # https://sproutsocial.com/insights/social-media-image-sizes-guide/

  if (match.arg(platform) == "twitter") {
    size <- switch(type,
      # Image from a Tweet with shared link:
      link = c(1200,  628),
      # Tweet sharing a single image:
      one = c(1200, 675),
      # Tweet sharing two images:
      two = c(700, 800),
      # Tweet sharing three images:
      # Left image:
      three_left = c(700, 800),
      # Right images:
      three_right = c(1200, 686),
      # Tweet sharing four images:
      four = c(1200, 600),
      c(1200, 675)
    )
  } else if (match.arg(platform) == "instagram") {
    size <- switch(type,
      feed_large = c(1080, 1080),
      feed_small = c(612, 612),
      stories_large = c(1080, 1920),
      stories_small = c(600, 1067),
      c(1080, 1080)
    )
  }

  names(size) <- c("width", "height")

  size
}
