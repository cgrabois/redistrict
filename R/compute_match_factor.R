#' Compute matched-pair overlap values for a match_level panel
#'
#' Adds one column per requested variable to a [build_district_panel()]
#' `shape = "match_level"` panel, each holding the matched pair's
#' overlap/allocation value for that variable, looked up from `data`. Call
#' this yourself on a panel you've already built — it is not run
#' automatically by [build_district_panel()].
#'
#' @param panel A data.frame from `build_district_panel(shape =
#'   "match_level")`, i.e. with columns `source`, `target`, `cycle` (e.g.
#'   `"cd111_cd112"`), `state_abb`.
#' @param vars Character vector of variable names to compute, e.g.
#'   `c("pop", "area")`. Each is added as a column named after the variable
#'   itself.
#' @param data Nested list (`variable > cycle > state > matrix`); defaults to
#'   the package-bundled [overlap].
#'
#' @return `panel` with one added numeric column per entry in `vars`, `NA`
#'   for rows with no source or no target (unmatched districts).
#'
#' @export
compute_match_factor <- function(panel, vars, data = overlap) {

  if (!all(c("source", "target", "cycle", "state_abb") %in% names(panel))) {
    stop("panel must come from build_district_panel(shape = 'match_level')")
  }

  match_factors <- purrr::map(purrr::set_names(vars), function(var) {
    purrr::pmap_dbl(
      list(panel$source, panel$target, panel$cycle, panel$state_abb),
      function(src, tgt, cyc, st) {
        if (is.na(src) || is.na(tgt)) return(NA_real_)
        data[[var]][[cyc]][[st]][src, tgt]
      }
    )
  })

  dplyr::bind_cols(panel, match_factors)
}
