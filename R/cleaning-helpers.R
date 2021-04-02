#' Bind two factors
#'
#' Create a new factor from two existing factors, where the new factor's levels
#' are the union of the levels of the input factors.
#'
#' @export
#' @examples
#' celsify_temp
#' localize_beach
#' outfile_path
usethis::use_package("utils")
utils::globalVariables(c("english", "temp"))
lookup_table <- dplyr::tribble(
  ~where, ~english,
  "beach",     "US",
  "coast",     "US",
  "seashore",     "UK",
  "seaside",     "UK"
)
#' @title Add counrty to beach
#'
#' @param dat A table.
#' @param lookup_table B table.
#' @return A left join B returning new table \code{A}

#' @export
localize_beach <- function(dat) {
  dplyr::left_join(dat, lookup_table)
}

#' @export
f_to_c <- function(x) (x - 32) * 5/9

#' @export
celsify_temp <- function(dat) {
  dplyr::mutate(dat, temp = dplyr::if_else(english == "US", f_to_c(temp), temp))
}

now <- Sys.time()
timestamp <- function(time) format(time, "%Y-%B-%d_%H-%M-%S")

#' @export
outfile_path <- function(infile, time = Sys.time()) {
  ts <- timestamp(time)
  paste0(ts, "_", sub("(.*)([.]csv$)", "\\1_clean\\2", infile))
}
