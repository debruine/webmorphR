#' Image Comparison
#'
#' This is just a convenient way to use magick::compareare with webmorph stimuli. It defaults to the "MSE" metric, which gives a linearly increasing score to images along a morph continuum.
#'
#'
#' Metric Types
#'
#' * Undefined: ?
#' * AE: Absolute Error
#' * Fuzz: ?
#' * MAE: Mean Absolute Error
#' * MEPP: Mean Error Per Pixel
#' * MSE: Mean Squared Error
#' * NCC: Normalized Cross Correlation
#' * PAE: Peak Absolute Error
#' * PHASH: Perceptual Hash
#' * PSNR: Peak Signal-to-Noise Ratio
#' * RMSE: Root Mean Squared Error
#'
#' How these metrics behave when comparing a morph continuum to its first image.
#'
#' Increases with morph distance:
#'
#' * very strong negative exponential decay at 0 fuzz; more linear with higher fuzz: AE
#' * strong negative exponential decay: PAE
#' * slight negative exponential decay: Fuzz, RMSE
#' * linear: MAE, MEPP, MSE
#' * no idea: PHASH
#'
#' Decreases with morph distance:
#'
#' * linear: NCC, Undefined
#' * slight exponential decay: PSNR
#'
#'
#' @param stimuli Stimuli to compare to the ref_stim
#' @param ref_stim A stim, 1-item stimlist, or the name or index of the comparison item in stim
#' @param metric string with a metric from magick::metric_types(): "Undefined", "AE", "Fuzz", "MAE", "MEPP", "MSE", "NCC", "PAE", "PHASH", "PSNR", "RMSE"
#' @param fuzz relative color distance (value between 0 and 100) to be considered similar in the filling algorithm (only useful for AE)
#' @param scale whether to scale the values so that the maximum value is 1 and the minimum is 0 (only useful when stim is more than 1 image and includes ref_stim)
#'
#' @return Difference metric
#' @export
#'
#' @examples
#' stimuli <- demo_stim()
#' compare(stimuli, stimuli$m_multi)
#' compare(stimuli, stimuli$m_multi, "AE")
#' compare(stimuli, stimuli$m_multi, "AE", fuzz = 5)
compare <- function(stimuli, ref_stim, metric = "MSE", fuzz = 0, scale = FALSE) {
  stimuli <- validate_stimlist(stimuli)
  img1 <- get_imgs(stimuli)
  if (is.numeric(ref_stim) | is.character(ref_stim)) {
    img2 <- stimuli[[ref_stim[[1]]]]$img
  } else {
    img2 <- validate_stimlist(ref_stim)[[1]]$img
  }
  comp <- magick::image_compare(img1, img2, metric, fuzz)

  diff <- attr(comp, "distortion")
  names(diff) <- names(stimuli)
  non_inf <- diff[!is.infinite(diff)]
  if (scale && (max(non_inf)-min(non_inf)) > 0) {
    scalediff <- (diff - min(diff)) / (max(non_inf)-min(non_inf))
    # scalediff[is.infinite(scalediff)] <- 1.0
    scalediff
  } else {
    diff
  }
}
