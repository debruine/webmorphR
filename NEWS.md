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

* demo imagesets removed so `demo_stim()` needs to load "london", "smiling", "composite", "lisa", "zoom", and "rainbow" from stimsets  using `remotes::install_github("debruine/stimsets")`
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
