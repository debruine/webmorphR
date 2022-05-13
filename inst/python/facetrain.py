# https://www.pyimagesearch.com/2019/12/16/training-a-custom-dlib-shape-predictor/

# import the necessary packages
import multiprocessing
import argparse
import dlib

def py_facetrain(training, 
                 output, 
                 tree_depth = 5, 
                 nu = 0.5, 
                 cascade_depth = 15, 
                 feature_pool_size = 400, 
                 num_test_splits = 50, 
                 oversampling_amount = 5, 
                 oversampling_translation_jitter = 0.1, 
                 be_verbose = True, 
                 num_threads = 0):

  # grab the default options for dlib's shape predictor
  print("[INFO] setting shape predictor options...")
  options = dlib.shape_predictor_training_options()
  
  # define the depth of each regression tree -- there will be a total
  # of 2^tree_depth leaves in each tree; small values of tree_depth
  # will be *faster* but *less accurate* while larger values will
  # generate trees that are *deeper*, *more accurate*, but will run
  # *far slower* when making predictions
  options.tree_depth = tree_depth
  
  # regularization parameter in the range [0, 1] that is used to help
  # our model generalize -- values closer to 1 will make our model fit
  # the training data better, but could cause overfitting; values closer
  # to 0 will help our model generalize but will require us to have
  # training data in the order of 1000s of data points
  options.nu = nu
  
  # the number of cascades used to train the shape predictor -- this
  # parameter has a *dramtic* impact on both the *accuracy* and *output
  # size* of your model; the more cascades you have, the more accurate
  # your model can potentially be, but also the *larger* the output size
  options.cascade_depth = cascade_depth
  
  # number of pixels used to generate features for the random trees at
  # each cascade -- larger pixel values will make your shape predictor
  # more accurate, but slower; use large values if speed is not a
  # problem, otherwise smaller values for resource constrained/embedded
  # devices
  options.feature_pool_size = feature_pool_size
  
  # selects best features at each cascade when training -- the larger
  # this value is, the *longer* it will take to train but (potentially)
  # the more *accurate* your model will be
  options.num_test_splits = num_test_splits
  
  # controls amount of "jitter" (i.e., data augmentation) when training
  # the shape predictor -- applies the supplied number of random
  # deformations, thereby performing regularization and increasing the
  # ability of our model to generalize
  options.oversampling_amount = oversampling_amount
  
  # amount of translation jitter to apply -- the dlib docs recommend
  # values in the range [0, 0.5]
  options.oversampling_translation_jitter = oversampling_translation_jitter
  
  # tell the dlib shape predictor to be verbose and print out status
  # messages our model trains
  options.be_verbose = be_verbose
  
  # number of threads/CPU cores to be used when training -- we default
  # this value to the number of available cores on the system, but you
  # can supply an integer value here if you would like
  if (num_threads == 0):
    options.num_threads = multiprocessing.cpu_count()
  else:
    options.num_threads = num_threads

  # log our training options to the terminal
  print("[INFO] shape predictor options:")
  print(options)
  
  # train the shape predictor
  print("[INFO] training shape predictor...")
  dlib.train_shape_predictor(training, output, options)
