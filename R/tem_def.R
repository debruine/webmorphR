#' Get template definition
#'
#' Template definitions are specially formatted files that contain information about templates that are needed to do things like symmetrising and masking images. This function is mostly used internally.
#'
#' @param tem_id numeric ID of template to retrieve from webmorph.org
#' @param path path of local tem definition file
#'
#' @return list with template definition
#' @export
#'
#' @examples
#' frl <- tem_def("FRL")
#' frl$points[1:10, ]
#'
#' fpp106 <- tem_def("fpp106")
#' fpp106$lines %>% str()
#'
#' fpp83 <- tem_def("fpp83")
#' fpp83$mask %>% str()
#'
tem_def <- function(tem_id = "FRL", path = NULL) {

  # read file or url ----
  if (!is.null(path)) {
    if (!file.exists(path)) {
      stop(sprintf("The file at %s does not exist", path))
    }
    tem_def <- tryCatch({
      readLines(path)
    }, error = function(e) {
      stop("The file couldn't be read")
    })
  } else if (is.numeric(tem_id)) {
    url <- sprintf("https://webmorph.org/scripts/temDownload?tem_id=%d",
                   tem_id)
    tem_def <- tryCatch({
      readLines(url)
    }, error = function(e) {
      stop("You might not have an internet connection")
    })
  } else if (is.character(tem_id)) {
    temdir <- system.file("extdata/tem_defs/", package = "webmorphR")
    temdefs <- list.files(temdir, full.names = TRUE)
    match <- grepl(tolower(tem_id), tolower(temdefs), fixed = TRUE)
    if (all(match == FALSE)) {
      stop(tem_id, " is not a built-in template")
    }

    tem_def <- readLines(temdefs[match][[1]])
  } else {
    stop("You must supply a numeric tem_id or a valid path to a template definition file.")
  }

  # parse text ----
  infotable <- utils::read.csv(text = tem_def[1:8],
                        header = FALSE,
                        row.names = 1) %>%
    t() %>% as.data.frame()
  tem <- list()
  for (i in 1:8) {
    tem[i] <- strsplit(infotable[ , i], ";") %>%
      utils::type.convert(as.is = TRUE)
  }
  names(tem) <- colnames(infotable)

  # get points (required)
  ptstart <- grepl("n\\s*,\\s*name\\s*,\\s*x\\s*,\\s*y\\s*,\\s*sym\\s*", tem_def) %>% which()
  if (length(ptstart) == 0) {
    stop("No points were found in the template definition")
  }
  ptrange <- ptstart:(ptstart+tem$points)
  tem$points <- utils::read.csv(text = tem_def[ptrange])

  # get lines (optional)
  lnstart <- grepl("n\\s*,\\s*linetype\\s*,\\s*color\\s*,\\s*points\\s*", tem_def) %>% which()
  if (length(lnstart) == 1) {
    linerange <- lnstart:(lnstart+tem$lines)
    lines <- utils::read.csv(text = tem_def[linerange]) %>%
      dplyr::arrange(.data$n) # probably already in order, but make sure
    tem$lines <- sapply(lines$points, strsplit, ",", USE.NAMES = FALSE) %>%
      sapply(as.integer)
    tem$closed <- lines$linetype == "closed"
    tem$linecolor <- lines$color
  } else if (tem$lines == 0) {
    tem$lines <- NULL
  } else {
    warning("No line definitions were found, but the template is meant to have ", tem$lines, " lines")
    tem$lines <- NULL
  }

  # get masks (optional)
  maskstart <- grepl("mask\\s*,\\s*points\\s*", tem_def) %>% which()
  if (length(maskstart) == 1) {
    maskrange <- maskstart:length(tem_def)
    m <- utils::read.csv(text = tem_def[maskrange])
    tem$masks <- lapply(m$points, strsplit, "\\s*;\\s*") %>%
      lapply(sapply, strsplit, "\\s*,\\s*") %>%
      lapply(sapply, as.integer, simplify = FALSE)
    names(tem$masks) <- m$mask
  }

  tem
}
