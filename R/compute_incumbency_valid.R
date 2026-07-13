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
#' @param incumbent_match_type Required — `"i2i"` (incumbent-to-incumbent
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
#' @examples
#' match_level_panel <- build_district_panel(
#'   start_congress = 111, end_congress = 114, shape = "match_level"
#' )
#' validated_panel <- compute_incumbency_valid(match_level_panel, "i2i")
#' table(validated_panel$source_incumbent_valid)
#'
#' @export
compute_incumbency_valid <- function(panel, incumbent_match_type) {

  # panel must be a match_level panel, i.e. have these three columns
  if (!all(c("source", "target", "cycle") %in% names(panel))) {
    stop("panel must come from build_district_panel(shape = 'match_level')")
  }

  # incumbent_match_type must be one of the two valid types — unlike
  # incumbent_lock elsewhere, NULL isn't valid here since there's nothing to
  # validate against without an incumbent dataset
  stopifnot(
    "incumbent_match_type must be 'i2i' or 'i2c'" =
      isTRUE(incumbent_match_type %in% c("i2i", "i2c"))
  )

  # look up the incumbent-match dataset (incumbency_matches_i2i or _i2c)
  incumbent_match_data <- resolve_incumbent_match_data(incumbent_match_type)

  panel |>
    # bring in the real incumbent (if any) for this row's source district
    dplyr::left_join(incumbent_match_data,
              by = c("source" = "incumbent_from", "cycle"),
              na_matches = "never") |>
    # ...and for this row's target district
    dplyr::left_join(incumbent_match_data,
              by = c("target" = "incumbent_to", "cycle"),
              na_matches = "never") |>
    dplyr::mutate(
      # did the algorithm send this row's source to where its real incumbent (if any) went?
      source_incumbent_valid = dplyr::case_when(
        is.na(source)              ~ NA_character_,
        is.na(incumbent_to)        ~ "no incumbent",
        is.na(target)              ~ "disagree",
        incumbent_to == target     ~ "agree",
        TRUE                       ~ "disagree"
      ),
      # did the algorithm bring this row's target from where its real incumbent (if any) came from?
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
