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
