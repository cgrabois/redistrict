#' Validate matches against actual incumbent behavior
#'
#' Adds `source_incumbent_valid` and `target_incumbent_valid` columns to a
#' [build_district_panel()] `shape = "match_level"` panel, reporting whether
#' each matched pair's assignment agrees with the actual incumbent (if any)
#' on each side. Call this yourself on a panel you've already built — it is
#' not run automatically by [build_district_panel()] or [match_crosswalk()].
#'
#' @param panel A data.frame from `build_district_panel(shape =
#'   "match_level")`, i.e. with columns `source`, `target`, `cycle` (e.g.
#'   `"cd111_cd112"`).
#' @param incumbent_match_type `"i2i"` (win-only incumbent-to-incumbent
#'   matches) or `"i2c"` (incumbent-to-candidate matches).
#'
#' @return `panel` with two added columns, each `"agree"`, `"disagree"`, or
#'   `"no incumbent"`:
#'   \describe{
#'     \item{`source_incumbent_valid`}{Whether the algorithm sent this row's
#'       source to where its real incumbent (if any) actually went, in this
#'       row's cycle.}
#'     \item{`target_incumbent_valid`}{Whether the algorithm brought this
#'       row's target from where its real incumbent (if any) actually came
#'       from, in this row's cycle.}
#'   }
#'
#' @export
compute_incumbency_valid <- function(panel, incumbent_match_type = NULL) {

  if (!all(c("source", "target", "cycle") %in% names(panel))) {
    stop("panel must come from build_district_panel(shape = 'match_level')")
  }

  incumbent_match_data <- resolve_incumbent_match_data(incumbent_match_type)

  if (!is.data.frame(incumbent_match_data)) {
    stop("incumbent_match_type must be 'i2i' or 'i2c'")
  }

  panel |>
    dplyr::left_join(incumbent_match_data,
              by = c("source" = "incumbent_from", "cycle"),
              na_matches = "never") |>
    dplyr::left_join(incumbent_match_data,
              by = c("target" = "incumbent_to", "cycle"),
              na_matches = "never") |>
    dplyr::mutate(
      source_incumbent_valid = dplyr::case_when(
        is.na(source)              ~ NA_character_,
        is.na(incumbent_to)        ~ "no incumbent",
        is.na(target)              ~ "disagree",
        incumbent_to == target     ~ "agree",
        TRUE                       ~ "disagree"
      ),
      target_incumbent_valid = dplyr::case_when(
        is.na(target)              ~ NA_character_,
        is.na(incumbent_from)      ~ "no incumbent",
        is.na(source)              ~ "disagree",
        incumbent_from == source   ~ "agree",
        TRUE                       ~ "disagree"
      )
    ) |>
    dplyr::select(-incumbent_to, -incumbent_from)
}
