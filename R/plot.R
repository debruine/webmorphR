#' Plot stimuli
#'
#' @param x list of class stimlist
#' @param y omitted
#' @param ... Arguments to be passed to [plot_stim()]
#'
#' @return stimlist
#' @export
#' @family viz
#' @keywords internal
plot.stimlist <- function(x, y, ...) {
  plot_stim(x, ...)
}

#' Plot stim
#'
#' @param x stim
#' @param y omitted
#' @param ... Arguments to be passed to [plot_stim()]
#'
#' @return stimlist
#' @export
#' @family viz
#' @keywords internal
plot.stim <- function(x, y, ...) {
  stimlist <- as_stimlist(x)
  plot(stimlist, ...)
}


#' Plot stimuli
#'
#' Show all the stimuli in a grid. You can use [plot()] as an alias.
#'
#' @param stimuli list of class stimlist
#' @param nrow number of rows
#' @param ncol number of columns
#' @param byrow fill grid by rows (first ncol images in the first row); if FALSE, fills by columns (first nrow images in the first column)
#' @param padding around each image in pixels
#' @param external_pad whether to include external padding
#' @param fill background color, see [color_conv()]
#' @param maxwidth,maxheight maximum width and height of grid in pixels
#'
#' @return stimlist with the plot image (no templates)
#' @export
#' @family viz
#'
#' @examples
#' stimuli <- demo_stim() |> resize(0.5)
#' plot_stim(stimuli)
#' 
#' \donttest{
#' # default padding is 10px internal and external
#' plot(stimuli, fill = "dodgerblue")
#' plot(stimuli, external_pad = 0, fill = "dodgerblue")
#' plot(stimuli, padding = 0, fill = "dodgerblue")
#' 
#' # make 8 numbered images
#' n <- blank(8, color = grDevices::cm.colors(8)) |> 
#'   label(1:8, gravity = "center", size = 50)
#' 
#' # 2 rows, allocating by row
#' plot(n, nrow = 2)
#' 
#' # 2 rows, allocating by column
#' plot(n, nrow = 2, byrow = FALSE)
#' }
plot_stim <- function(stimuli, nrow = NULL, ncol = NULL, byrow = TRUE,
                      padding = 10, external_pad = TRUE,
                      fill = wm_opts("fill"),
                      maxwidth = wm_opts("plot.maxwidth"),
                      maxheight = wm_opts("plot.maxheight")) {
  stimuli <- as_stimlist(stimuli)
  w <- width(stimuli)
  h <- height(stimuli)
  n <- length(stimuli)
  fill <- color_conv(fill)

  # calculate/validate nrow and ncol ----
  if (is.null(nrow) && is.null(ncol)) {
    if (n < 6) {
      nrow <- 1
      ncol <- n
    } else {
      nrow <- floor(sqrt(n))
      ncol <- ceiling(n / nrow)
    }
  } else if (is.null(nrow)) {
    nrow <- ceiling(n / ncol)
    ncol <- ceiling(n / nrow) # reset in case nrow > n 
  } else {
    # row takes precedence even if ncol is set
    ncol <- ceiling(n / nrow)
    nrow <- ceiling(n / ncol) # reset in case ncol > n 
  }

  # shrink images to fit maxwidth and maxheight ----
  npad <- ifelse(isTRUE(external_pad), 1, -1)
  w_resize <- (maxwidth-padding*(ncol+npad)) / (ncol * max(w))
  h_resize <- (maxheight-padding*(nrow+npad)) / (nrow * max(h))

  if (w_resize < 1 || h_resize < 1) {
    stimuli <- resize(stimuli, min(w_resize, h_resize))
  }

  stimuli <- pad(stimuli, padding/2, fill = fill)

  # get vector of images in magick format
  img <- get_imgs(stimuli)

  # make rows ----
  row_imgs <- list()
  # make matrix of image layout
  img_i <- matrix(1:(nrow*ncol), nrow = nrow, byrow = byrow)
  for (r in 1:nrow) {
    idx <- img_i[r, ] # get row indices
    idx <- idx[idx <= length(img)] # for short rows
    append_img <- magick::image_append(img[idx])
    row_imgs[[r]] <- magick::image_flatten(append_img)
  }

  # make columns ----
  for (i in seq_along(row_imgs)) {
    if (i == 1) {
      rows <- row_imgs[[i]]
    } else {
      rows <- c(rows, row_imgs[[i]])
    }
  }
  # add or remove external padding ----
  epad <- ifelse(isTRUE(external_pad), padding/2, -padding/2)
  plot_img <- magick::image_append(rows, stack = TRUE)
  plot_stim <- new_stim(plot_img, "plot")
  padded_plot <- pad(plot_stim, epad, fill = fill)

  padded_plot
}


#' Plot in rows
#'
#' @param ... stimlists (optionally named) and any arguments to pass on to \code{\link{label}}
#' @param top_label logical; whether to plot row labels above the row (TRUE) or inside (FALSE), if NULL, then TRUE if stimlists are named
#' @param maxwidth,maxheight maximum width and height of each row in pixels
#'
#' @return stimlist with plot
#' @export
#' @family viz
#'
#' @examples
#' s <- demo_unstandard()
#' plot_rows(
#'   female = s[1:3],
#'   male = s[6:8],
#'   maxwidth = 600
#' )
plot_rows <- function(..., top_label = NULL,
                      maxwidth = wm_opts("plot.maxwidth"),
                      maxheight = wm_opts("plot.maxheight")) {
  dots <- list(...)
  is_stimlist <- sapply(dots, inherits, "stimlist")
  rowlist <- dots[is_stimlist]
  
  if (is.null(top_label)) {
    top_label <- !is.null(names(rowlist))
  }

  rows <- lapply(rowlist, plot_stim, 
                 nrow = 1, 
                 external_pad = 0,
                 maxwidth = maxwidth, 
                 maxheight = maxheight)|>
    do.call(what = c)
  rows <- resize(rows, width = min(width(rows)))

  # add top padding
  size <- dots$size %||% (sum(height(rows)) / 25)
  if (isTRUE(top_label)) {
    rows <- pad(rows, (size + 20), 0, 0, 0)
  }

  # label images
  if (!is.null(names(rowlist))) {
    label_args <- list(
      stimuli = rows,
      text = names(rowlist),
      size = size,
      gravity = "northwest",
      location = ifelse(top_label, "+0+10", "+10+10")
    )
    label_args <- utils::modifyList(label_args, dots[!is_stimlist])

    rows <- do.call(label, label_args)
  }

  plot_stim(rows, ncol = 1, 
            maxwidth = Inf, 
            maxheight = Inf)
}



