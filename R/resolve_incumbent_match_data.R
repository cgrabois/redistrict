#' Resolve an incumbent-match type into its bundled dataset
#'
#' Resolves an `incumbent_match_type` string into the corresponding bundled
#' incumbent-match dataset, or `NULL` if incumbent matching is not requested.
#' Used internally by [match_crosswalk()], [build_district_panel()], and
#' [compute_incumbency_valid()] — not intended to be called directly.
#'
#' @param incumbent_match_type `"i2i"`, `"i2c"`, or `NULL` (default) to skip
#'   incumbent matching entirely.
#'
#' @return A data.frame (`incumbency_matches_i2i` or `incumbency_matches_i2c`),
#'   or `NULL`.
#'
#' @keywords internal
resolve_incumbent_match_data <- function(incumbent_match_type = NULL) {
  switch(
    if (rlang::is_null(incumbent_match_type)) "none" else incumbent_match_type,
    i2i  = incumbency_matches_i2i,
    i2c  = incumbency_matches_i2c,
    none = NULL,
    stop("incumbent_match_type must be 'i2i', 'i2c', or NULL")
  )
}
