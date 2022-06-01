## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* I have addressed all requested changes in Gregor Seyer's report (thanks!)
* Examples for un-exported functions have been removed.
* Examples now only use dontrun{} when the functions 
    - write to disk (write_stim), 
    - require XQuartz on Mac (align), 
    - are demonstrating an error (as_stimlist),
    - change user options (wm_opts, wm_opts_defaults)
    - require a personal token for a web API (auto_delin)
* Examples for functions that require access to a web API without a personal token (avg, trans, symmetrise, continuum, loop) now check for site accessibility with a new function (webmorph_up) instead of using dontrun{}. All are also wrapped in \donttest{}.
* The old working directory is now restored in inst/app/app.R using `oldwd <- getwd(); on.exit(oldwd)` in the relevant function. (Sorry; I thought changes to wd in a shiny app didn't affect the user's wd.)

* I found and fixed two small bugs today (while trying to make a pride flag demo, which is an unusual use case), so updated the version to 0.1.1 and hope this gets to you before you look at the previous version.
    - Fixed a bug when cropping `blank()` images with no names
    - Fixed a bug when using `image_func()` when func = "composite" and the length of stimuli and composite_image are the same.