#' Plot stimuli
#'
#' Plot stimuli in a stimlist.
#'
#' @param x list of class stimlist
#' @param y omitted
#' @param ... Arguments to be passed to \code{\link{plot_stim}}
#'
#' @return stimlist
#' @export
#'
#' @examples
#' demo_stim("test") |> plot()
#'
plot.stimlist <- function(x, y, ...) {
  plot_stim(x, ...)
}

#' Plot stim
#'
#' @param x stim
#' @param y omitted
#' @param ... Arguments to be passed to \code{\link{plot_stim}}
#'
#' @return stimlist
#' @export
#'
#' @examples
#' demo_stim("test")[[1]] |> plot()

plot.stim <- function(x, y, ...) {
  stimlist <- validate_stimlist(x)
  plot(stimlist, ...)
}


#' Plot stimuli
#'
#' Show all your stimuli in a grid.
#'
#' @param stimuli list of class stimlist
#' @param nrow number of rows
#' @param ncol number of columns
#' @param byrow fill grid by rows (first ncol images in the first row); if FALSE, fills by columns (first nrow images in the first column)
#' @param padding around each image in pixels
#' @param external_pad whether to include external padding
#' @param fill background color
#' @param maxwidth,maxheight maximum width and height of grid in pixels
#'
#' @return stimlist with grid image (no templates)
#' @export
#'
#' @examples
#' demo_stim() |> plot_stim()
#'
plot_stim <- function(stimuli, nrow = NULL, ncol = NULL, byrow = TRUE,
                      padding = 10, external_pad = TRUE,
                      fill = wm_opts("fill"),
                      maxwidth = wm_opts("plot.maxwidth"),
                      maxheight = wm_opts("plot.maxheight")) {
  stimuli <- validate_stimlist(stimuli)
  w <- width(stimuli)
  h <- height(stimuli)
  n <- length(stimuli)

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
  } else {
    # row takes precedence even if ncol is set
    ncol <- ceiling(n / nrow)
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

    row_imgs[[r]] <- magick::image_append(img[idx]) |>
      magick::image_flatten()
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
  grid <- magick::image_append(rows, stack = TRUE) |>
    new_stim("grid") |>
    pad(epad, fill = fill)

  grid
}


#' Plot in rows
#'
#' @param ... stimlists (optionally named) and any arguments to pass on to \code{\link{label}}
#' @param top_label whether to plot row labels above the row or inside
#'
#' @return stimlist with plot
#' @export
#'
#' @examples
#' up <- demo_stim()
#' inv <- rotate(up, 180)
#' plot_rows(upright = up, inverted = inv)
#' plot_rows(upright = up, inverted = inv, color = "dodgerblue", top_label = TRUE)
plot_rows <- function(..., top_label = FALSE) {
  dots <- list(...)
  is_stimlist <- lapply(dots, class) |>
    lapply(`==`, "stimlist") |> sapply(any)

  rowlist <- dots[is_stimlist]

  args <- lapply(rowlist, plot_stim, nrow = 1, external_pad = 0)
  rows <- do.call(c, args)
  rows <- resize(rows, width = min(width(rows)))

  # add top padding
  size <- dots$size %||% ((height(rows) |> sum()) / 25)
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
    ) |>
      utils::modifyList(dots[!is_stimlist])

    rows <- do.call(label, label_args)
  }

  plot_stim(rows, ncol = 1)
}



