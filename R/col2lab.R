#' Color to Lab Conversion
#'
#' R color to LAB colourspace conversion. Calculated with Observer. = 2°, Illuminant = D65.
#'
#' @param col vector of hex or color names
#'
#' @return list of L, a and b values
#' @export
#'
#' @examples
#' col2lab(c("red", "green", "blue"))
#'
col2lab <- function(col) {
  # checked at http://www.brucelindbloom.com/index.html?ColorCheckerCalcHelp.html
  
  # RGB to XYZ via http://www.easyrgb.com/index.php?X=MATH&H=02#text2
  
  #change to 0-1
  rgb <- grDevices::col2rgb(col)/255
  
  # inverse sRGB companding
  rgb2 <- apply(rgb, 1, function(v) {
    100 * ifelse( v > 0.04045,
                  `^`( (( v + 0.055 ) / 1.055 ), 2.4),
                  v / 12.92
    )
  }) |> matrix(nrow = ncol(rgb)) # for 1-pixel images
  
  # Observer. = 2°, Illuminant = D65
  X = rgb2[, 1] * 0.4124 + rgb2[, 2] * 0.3576 + rgb2[, 3] * 0.1805
  Y = rgb2[, 1] * 0.2126 + rgb2[, 2] * 0.7152 + rgb2[, 3] * 0.0722
  Z = rgb2[, 1] * 0.0193 + rgb2[, 2] * 0.1192 + rgb2[, 3] * 0.9505
  
  # XYZ to CieL*ab via http://www.easyrgb.com/index.php?X=MATH&H=07#text7
  
  
  # Observer= 2°, Illuminant= D65
  #see http://www.brucelindbloom.com/index.html?ColorCheckerCalcHelp.html for other values
  ref_X =  95.047
  ref_Y = 100.000
  ref_Z = 108.883
  
  xyz <- list(
    'x' = X / ref_X,
    'y' = Y / ref_Y,
    'z' = Z / ref_Z
  ) |> lapply(function(v) {
    ifelse( v > 0.008856,
            `^`(v, 1/3 ),
            (7.787 * v) + (16 / 116))
  })
  
  list(
    L = ( 116 * xyz$y ) - 16,
    a = 500 * ( xyz$x - xyz$y ),
    b = 200 * ( xyz$y - xyz$z )
  )
}