#' Train a dlib shape predictor
#' 
#' @description
#' Implements a python script from PyImageSearch to train a custom shape predictor model using dlib and OpenCV. Produces a shape predictor file from an xml file containing the image paths, bounding boxes and training points, usually created by [tem_to_xml()].
#' 
#' Adrian Rosebrock, Training a custom dlib shape predictor, PyImageSearch, https://www.pyimagesearch.com/2019/12/16/training-a-custom-dlib-shape-predictor/, accessed on 13 May 2022
#' 
#' @details 
#' NB: The python script will cause R to crash if you try to fit a model with fewer than 8 training faces.
#' 
#' This text is from Adrian Rosebrock's explanation:
#' 
#' - tree_depth: the depth of each regression tree -- typical values are between 2 and 8; there will be a total of 2^tree_depth leaves in each tree; small values of tree_depth will be *faster* but *less accurate* while larger values will generate trees that are *deeper*, *more accurate*, but will run *far slower* when making predictions
#' 
#' - nu: a regularization parameter in the range 0:1 that is used to help our model generalize -- values closer to 1 will make our model fit the training data better, but could cause overfitting; values closer to 0 will help our model generalize but will require us to have training data in the order of 1000s of data points
#' 
#' - cascade_depth: the number of cascades used to train the shape predictor -- typical values are between 6 and 18; this parameter has a *dramatic* impact on both the *accuracy* and *output size* of your model; the more cascades you have, the more accurate your model can potentially be, but also the *larger* the output size
#' 
#' - feature_pool_size: number of pixels used to generate features for the random trees at each cascade -- larger pixel values will make your shape predictor more accurate, but slower; use large values if speed is not a problem, otherwise smaller values for resource constrained/embedded devices
#' 
#' - num_test_splits: selects best features at each cascade when training -- the larger this value is, the *longer* it will take to train but (potentially) the more *accurate* your model will be
#' 
#' - oversampling_amount: controls the number of random deformations per image (i.e., data augmentation) when training the shape predictor -- applies the supplied number of random deformations, thereby performing regularization and increasing the ability of our model to generalize
#' 
#' - oversampling_translation_jitter: amount of translation jitter to apply -- the dlib docs recommend values in the range 0 to 0.5
#' 
#' - num_threads: number of threads/CPU cores to be used when training -- defaults to the number of available cores on the system, but you can supply an integer value
#'
#' @param xml The xml file containing the bounding boxes and training points, usually created by [tem_to_xml]
#' @param output the name of the .dat file to save the model to
#' @param tree_depth the depth of each regression tree; typically 2:8
#' @param nu regularization parameter; must be 0:1
#' @param cascade_depth the number of cascades used to train the shape predictor; typically 6:8
#' @param feature_pool_size number of pixels used to generate features for the random trees at each cascade
#' @param num_test_splits selects best features at each cascade when training
#' @param oversampling_amount controls the number of random deformations per image (i.e., data augmentation) when training the shape predictor; typically 0:50
#' @param jitter amount of oversampling translation jitter to apply; typically 0 to 0.5
#' @param num_threads number of threads/CPU cores to be used when training
#'
#' @return the path to the output file (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' # requires python and dlib
#' stimuli <- demo_stim("composite") |> 
#'   subset_tem(features("face"))
#' 
#' # create xml and image directory for training
#' xml <- tem_to_xml(stimuli)
#' 
#' # train model
#' newmodel <- facetrain(xml)
#' 
#' # check model on new images
#' newdelin <- stimuli |> auto_delin(replace = TRUE, dat_file = newmodel)
#' }
facetrain <- function(xml,
                      output = "shape_predictor.dat", 
                      tree_depth = 5L, 
                      nu = 0.5, 
                      cascade_depth = 15L, 
                      feature_pool_size = 400L, 
                      num_test_splits = 50L, 
                      oversampling_amount = 5L, 
                      jitter = 0.1, 
                      num_threads = 0L) {
  # check parameters
  if (nu < 0 || nu > 1) stop("nu must be between 0 and 1")
  if (jitter < 0 || jitter > 0.5) 
    stop("jitter must be between 0 and 0.5")
  if (tree_depth < 2 || tree_depth > 8) 
    warning("tree_depth is usually between 2 and 8")
  
  
  # make sure paths are absolute for python
  xml <- normalizePath(xml)

  output <- dirname(output) |>
    normalizePath() |>
    file.path(basename(output))
  
  # check number of images in training set
  # R crashes if it's less than 8
  n_images <- readLines(xml) |> grepl("</image>", x = _) |> sum()
  if (n_images < 8) stop("The algorithm crashes if you train with fewer than 8 images.")
  
  # load python code
  py_facetrain <- NULL # stops CMD check from complaining
  pyscript <- system.file("python/facetrain.py", package = "webmorphR")
  reticulate::source_python(pyscript)
  
  py_facetrain(
    training = xml, 
    output = output,
    tree_depth = as.integer(tree_depth),
    nu = as.double(nu),
    cascade_depth = as.integer(cascade_depth),
    feature_pool_size = as.integer(feature_pool_size),
    num_test_splits = as.integer(num_test_splits),
    oversampling_amount = as.integer(oversampling_amount),
    oversampling_translation_jitter = as.double(jitter),
    be_verbose = isTRUE(wm_opts("verbose")),
    num_threads = as.integer(num_threads)
  )
  
  invisible(output)
}