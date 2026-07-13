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
#'   `c("pop", "area")` — pass more than one to add multiple columns at once.
#'   Each must be a variable present in `data`; when using the bundled
#'   [overlap] data, that means one or more of `"pop"`, `"area"`,
#'   `"afact_s2t"`, `"afact_t2s"`, `"afact_s2t_area"`, `"afact_t2s_area"`.
#'   Each is added as a column named after the variable itself.
#' @param data Nested list (`variable > cycle > state > matrix`); defaults to
#'   the package-bundled [overlap]. Supplying a custom `data` is not
#'   recommended — the package's safeguards assume the bundled data's
#'   structure and value ranges.
#'
#' @return `panel` with one added numeric column per entry in `vars`, `NA`
#'   for rows with no source or no target (unmatched districts).
#'
#' @examples
#' match_level_panel <- build_district_panel(
#'   start_congress = 111, end_congress = 114, shape = "match_level"
#' )
#' panel_with_overlap <- compute_match_factor(match_level_panel, vars = c("pop", "area"))
#' panel_with_overlap
#'
#' @export
compute_match_factor <- function(panel, vars, data = overlap) {

  # panel must be a match_level panel, i.e. have these four columns
  if (!all(c("source", "target", "cycle", "state_abb") %in% names(panel))) {
    stop("panel must come from build_district_panel(shape = 'match_level')")
  }

  # every entry in vars must actually exist in data
  missing_vars <- vars[!vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(paste0(
      "No data found for vars: ", paste(missing_vars, collapse = ", ")
    ))
  }

  # for each requested variable, look up the matched pair's value in data —
  # NA for unmatched rows (no source or no target)
  match_factors <- purrr::map(purrr::set_names(vars), function(var) {
    purrr::pmap_dbl(
      list(panel$source, panel$target, panel$cycle, panel$state_abb),
      function(src, tgt, cyc, st) {
        if (is.na(src) || is.na(tgt)) return(NA_real_)
        data[[var]][[cyc]][[st]][src, tgt]
      }
    )
  })

  # add one new column per variable to the original panel
  dplyr::bind_cols(panel, match_factors)
}
