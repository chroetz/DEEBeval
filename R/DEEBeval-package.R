#' @keywords internal
#' @import ConfigOpts
#' @import DEEBtrajs
#' @import DEEBpath
#' @importFrom dplyr filter select mutate ends_with relocate as_tibble rowwise group_by ungroup distinct pull lst summarize tibble across
#' @importFrom tidyr drop_na
#' @importFrom rlang .data .env
#' @importFrom stringr str_match str_replace_all str_sub str_detect
#' @importFrom readr read_csv write_csv read_lines
"_PACKAGE"

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib DEEBeval, .registration = TRUE
## usethis namespace: end
NULL
