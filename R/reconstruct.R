### Thx tibbletime for thinking of this                    ###
### This file is a series of S3 methods for panel_data     ###
### Implement generic reconstruct() until sloop is on CRAN ###

reconstruct <- function(new, old) {
  UseMethod("reconstruct", old)
}

#' @import dplyr
#' @export
reconstruct.panel_data <- function(new, old) {
  
  id <- get_id(old)
  wave <- get_wave(old)
  
  if (is.data.frame(new) == FALSE) {
    # warning("The panel_data object is no longer a data frame.")
    return(new)
  }
  
  if (id %nin% names(new)) {
    if ("panel_data" %in% class(new)) class(new) <- class(new) %not% "panel_data"
    return(new)
  }
  
  if (wave %nin% names(new)) {
    if ("panel_data" %in% class(new)) class(new) <- class(new) %not% "panel_data"
    return(new)
  }
  
  if ("panel_data" %nin% class(new) | id %nin% group_vars(new)) {
    atts <- attributes(old)
    return(panel_data(new, id = !! sym(id), wave = !! sym(wave),
                      reshaped = atts$reshaped, varying = atts$varying, 
                      constants = atts$constants)
           )
  } else {
    return(re_attribute(new, old))
  }
  
}

re_attribute <- function(new, old) {
  o <- attributes(old)
  attr(new, "reshaped") <- o$reshaped
  attr(new, "varying") <- o$varying
  attr(new, "constants") <- o$constants
  attr(new, "wave") <- get_wave(old)
  attr(new, "id") <- get_id(old)
  attr(new, "periods") <- get_periods(old)
  return(new)
}

##### dplyr ##################################################################

#' @export
#' @importFrom dplyr mutate
mutate.panel_data <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr summarise
summarise.panel_data <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'   S3method(dplyr::filter, panel_data)
#' } else {
#'   export(filter.panel_data)
#' }
#' @importFrom dplyr filter
filter.panel_data <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

# Required to export filter, otherwise:
# Warning: declared S3 method 'filter.panel_data' not found
# because of stats::filter

#' @export
#'
dplyr::filter

#' @export
#' @importFrom dplyr distinct
distinct.panel_data <- function(.data, ..., .keep_all = FALSE) {
  out <- tibble::as_tibble(.data)
  out <- dplyr::distinct(out, ..., .keep_all = .keep_all)
  reconstruct(out, .data)
}

#' @export
#' @importFrom dplyr full_join
#'
full_join.panel_data <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

#' @export
#' @importFrom dplyr inner_join
#'
inner_join.panel_data <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

#' @export
#' @importFrom dplyr left_join
#'
left_join.panel_data <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

#' @export
#' @importFrom dplyr right_join
#'
right_join.panel_data <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

#' @export
#' @importFrom dplyr anti_join
#'
anti_join.panel_data <- function(x, y, by = NULL, copy = FALSE, ...) {
  reconstruct(NextMethod(), x)
}

#' @export
#' @importFrom dplyr semi_join
#'
semi_join.panel_data <- function(x, y, by = NULL, copy = FALSE, ...) {
  reconstruct(NextMethod(), x)
}

#' @export
#' @importFrom dplyr slice
#'
slice.panel_data <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr group_by
group_by.panel_data <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
`names<-.panel_data` <- function(x, value) {
  reconstruct(NextMethod(), x)
}

#' @export
`[[<-.panel_data` <- function(x, i, j, ..., value) {
  reconstruct(NextMethod(), x)
}

### Backwards compat support for deprecated standard eval dplyr

# Only a few of them need it. arrange_.tbl_df() directly calls arrange_impl()
# causing a problem.

#' @export
#' @importFrom dplyr arrange_
#'
arrange_.panel_data <- function(.data, ..., .dots = list()) {
  reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr mutate_
#'
mutate_.panel_data <- function(.data, ..., .dots = list()) {
  reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr summarise_
#'
summarise_.panel_data <- function(.data, ..., .dots = list()) {
  reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr summarize_
#'
summarize_.panel_data <- function(.data, ..., .dots = list()) {
  reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr slice_
#'
slice_.panel_data <- function(.data, ..., .dots = list()) {
  reconstruct(NextMethod(), .data)
}

##### tidyr ##################################################################

#' @export
#' @importFrom tidyr nest
nest.panel_data <- function(.data, ...) {
  out <- tibble::as_tibble(.data)
  
  groups <- dplyr::groups(.data)
  if (length(groups) != 0L) {
    out <- dplyr::group_by(out, !!!groups)
  }
  
  out <- tidyr::nest(out, ...)
  
  reconstruct(out, .data)
}

