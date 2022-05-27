#' Symmetrize Images
#' 
#' Use webmorph.org to make faces symmetric in shape and/or colour.
#'
#' @param stimuli list of stimuli
#' @param shape,color amount of symmetry (0 for none, 1.0 for perfect)
#' @param tem_id template ID to be passed to [tem_def()] (usually "frl" or "fpp106")
#' @param ... Additional arguments to pass to [trans()]
#'
#' @return list of stimuli with symmetrised images and templates
#' @export
#' @family webmorph
#' @aliases symmetrise
#'
#' @examples
#' \dontrun{
#'   stimuli <- demo_stim(1)
#'
#'   sym_both <- symmetrize(stimuli)
#'   sym_shape <- symmetrize(stimuli, color = 0)
#'   sym_color <- symmetrize(stimuli, shape = 0)
#'   sym_anti <- symmetrize(stimuli, shape = -1.0, color = 0)
#' }
symmetrize <- function(stimuli, shape = 1.0, color = 1.0, tem_id = "frl", ...) {
  stimuli <- require_tems(stimuli, TRUE)
  
  mirror <- mirror(stimuli, tem_id) |>
    rename_stim(prefix = "mirror_")
  
  sym <- trans(trans_img = stimuli, from_img = stimuli, to_img = mirror,
               shape = shape[[1]]/2, color = color[[1]]/2, texture = color[[1]]/2,
               outname = names(stimuli), ...)
  
  sym
}


#' @rdname symmetrize
#' @export
symmetrise <- symmetrize