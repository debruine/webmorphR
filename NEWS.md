# webmorph 0.0.2.9001

* Imports rsvg; needed for `draw_tem()` but called by magick

# webmorph 0.0.2.9000

* moved all python functions to webmorphR.dlib
* `auto_delin()` is now a wrapper for `fpp_auto_delin()` (for Face++) and `webmorphR.dlib::dlib_auto_delin()` (for python/dlib/face_recognition delineations).
* Manuals and vignettes updated

# webmorph 0.0.1.9008

* Reduced package dependencies, but now requires R>=4.2.0
* All code and examples use |> pipe 
* `facetrain()` function for training a dlib shape predictor model (still really experimental)
* Various bug fixes

# webmorph 0.0.1.9007

* Custom masks take lists for the `mask` argument now.
* added `change_lines()` function to update lines in templates.
* Updated `crop_tem()` to handle stimlists with different tems.
* Updated `get_point()` to return a long table.
* Added `gglabel()` for ggplot-style annotations.
* Updated some vignettes for new functions.
* Changed how template definitions are stored

# webmorph 0.0.1.9006

* Added python face detection for `auto_delin()` so there is a local solution

# webmorph 0.0.1.9005

* Changed `label()` to default to `gravity = northwest` and `location = +10+10`
* Added `metrics()` for calculating facial-metrics.

# webmorph 0.0.1.9004

* Fixed bug in `mask_oval()` when setting bounds manually that had reversed top and bottom values.
* `trans()` now returns images in a more sensible order.
* added ability to rename images to `write_stim()`
* `to_size()` crop argument added.
* Images from `plot()` displayed in the correct aspect ratio in RMarkdown files by default now.

# webmorph 0.0.1.9003

* demo imagesets removed so `demo_stim()` needs to load "london", "smiling", "composite", "lisa", "zoom", and "rainbow" from webmorphR.stim  using `remotes::install_github("debruine/webmorphR.stim")`
* `image_comp()` to calculate difference between images in a way that corresponds to morph continuua differences
* crop_pad renamed to `crop_tem()`
* frl_features renamed to `features()`
* as always, bug fixes too numerous to mention

# webmorph 0.0.1.9002

* Deleted all of the webmorph.org functions that required login
* Added webmorph functions that don't require login: `avg()`, `trans()`, `continuum()`, `loop()`

# webmorph 0.0.1.9001

* Fixed some minor bugs
* Fixed bug in converting rgb(a) values to hex when all values < 10
* Added ability to set background in `grid_stim()` and `plot()`
* `draw_tem()` takes a background color for bg now
* Added `plot_size()` function to return size of last plot and a string to paste into Rmd chunks.

# webmorph 0.0.1.0

* Broke all the plotting, but the new `plot()` function is much better and faster
* Added convenience functions: `pad()`, `draw_tem()`, `grid_stim()`, `save_plot()`
* Renamed functions
    - `oval_mask` => `mask_oval()`
    - `faces` => `demo_stim()`
    - `auto_delineate` => `auto_delin()`
* Fixed function examples

# webmorph 0.0.0.9011

* Upgraded Face++ `auto-delin()` to 106 points from 83.
* Masking for auto-delin faces.

# webmorph 0.0.0.9010

* `mask()` and `oval_mask()` functions
* `quick_delin()` 

# webmorph 0.0.0.9009

* Bug fixes for stimuli that have no templates
* `to_size()` now takes a vector of width and height, so you can more easily use it with `social_media_size()`
* `write_stim()` now lets you change format (png, jpeg, or gif)

# webmorph 0.0.0.9008

* Cleaned up function names

# webmorph 0.0.0.9007

* Forgot to document all the changes
* Plotting functions
* webmorph.org integration
* Lots of new vignettes

# webmorph 0.0.0.9001

* Cleaned up functions and examples
* Added `align()` function, includes procrustes align

# webmorph 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
