#' Color to Lab Conversion
#'
#' R color to Lab colourspace conversion. Calculated with Observer. = 2°, Illuminant = D65.
#' 
#' @details 
#' The formulas used to convert from RGB to XYZ and XYZ to Lab are from http://www.easyrgb.com/en/math.php and the reference values are from http://www.brucelindbloom.com/index.html?ColorCheckerCalcHelp.html
#'
#' @param col vector of hex or color names
#' @param ref_X,ref_Y,ref_Z Reference values for Observer= 2°, Illuminant= D65
#'
#' @return vector of L, a and b values
#' @export
#' @keywords internal
#' @family color
#'
#' @examples
#' col2lab(c("red", "green", "blue"))
#'
col2lab <- function(col, ref_X =  95.047, ref_Y = 100.000, ref_Z = 108.883) {
  # Observer= 2°, Illuminant= D65
  # see http://www.brucelindbloom.com/index.html?ColorCheckerCalcHelp.html for other values
  
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
  
  xyz <- list(
    'x' = X / ref_X,
    'y' = Y / ref_Y,
    'z' = Z / ref_Z
  ) |> lapply(function(v) {
    ifelse( v > 0.008856,
            `^`(v, 1/3 ),
            (7.787 * v) + (16 / 116))
  })
  
  c(
    L = ( 116 * xyz$y ) - 16,
    a = 500 * ( xyz$x - xyz$y ),
    b = 200 * ( xyz$y - xyz$z )
  )
}

#' Lab to RGB Conversion
#'
#' Lab colourspace to RGB conversion. Calculated with Observer. = 2°, Illuminant = D65.
#' 
#' @details 
#' The formulas used to convert from Lab to XYZ and XYZ to RGB are from http://www.easyrgb.com/en/math.php and the reference values are from http://www.brucelindbloom.com/index.html?ColorCheckerCalcHelp.html
#'
#' @param lab 
#' @param ref_X,ref_Y,ref_Z Reference values for Observer= 2°, Illuminant= D65
#'
#' @return vector of red, green and blue values
#' @export
#' @keywords internal
#' @family color
#'
#' @examples
#' lab <- c(100, 0, 0)
#' rgb <- lab2rgb(lab)
#' rgb
#' 
#' lab <- col2lab("red")
#' rgb <- lab2rgb(lab)
#' rgb
lab2rgb <- function(lab, ref_X =  95.047, ref_Y = 100.000, ref_Z = 108.883) {
  # CIE-L*ab → XYZ via http://www.easyrgb.com/index.php?X=MATH&H=07#text7
  
  # Reference-X, Y and Z refer to specific illuminants and observers.
  # Common reference values are available below in this same page.
  
  var_Y = ( lab[[1]] + 16 ) / 116
  var_X = lab[[2]] / 500 + var_Y
  var_Z = var_Y - lab[[3]] / 200
  
  if ( var_Y^3  > 0.008856 ) {
    var_Y = var_Y^3
  } else {
    var_Y = ( var_Y - 16 / 116 ) / 7.787
  }
  
  if ( var_X^3  > 0.008856 ) { 
    var_X = var_X^3
  } else {
    var_X = ( var_X - 16 / 116 ) / 7.787
  }
  
  if ( var_Z^3  > 0.008856 ) {
    var_Z = var_Z^3
  } else {
    var_Z = ( var_Z - 16 / 116 ) / 7.787
  }
  
  X = var_X * ref_X
  Y = var_Y * ref_Y
  Z = var_Z * ref_Z
  
  # X, Y and Z input refer to a D65/2° standard illuminant.
  # sR, sG and sB (standard RGB) output range = 0 ÷ 255
  
  var_X = X / 100
  var_Y = Y / 100
  var_Z = Z / 100
  
  var_R = var_X *  3.2406 + var_Y * -1.5372 + var_Z * -0.4986
  var_G = var_X * -0.9689 + var_Y *  1.8758 + var_Z *  0.0415
  var_B = var_X *  0.0557 + var_Y * -0.2040 + var_Z *  1.0570
  
  if ( var_R > 0.0031308 ) {
    var_R = 1.055 * ( var_R ^ ( 1 / 2.4 ) ) - 0.055
  } else{
    var_R = 12.92 * var_R
  }
  
  if ( var_G > 0.0031308 ) {
    var_G = 1.055 * ( var_G ^ ( 1 / 2.4 ) ) - 0.055
  } else {
    var_G = 12.92 * var_G
  }
  
  if ( var_B > 0.0031308 ) {
    var_B = 1.055 * ( var_B ^ ( 1 / 2.4 ) ) - 0.055
  } else {
    var_B = 12.92 * var_B
  }
  
  rgb <- c(red = var_R, 
           green = var_G, 
           blue = var_B) * 255
  
  round(rgb) |> pmax(0) |> pmin(255)
}
