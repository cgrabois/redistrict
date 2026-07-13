#' Build a multi-cycle district panel
#'
#' Builds a panel of matched congressional districts across every consecutive
#' congress pair in the given range, so individual districts can be tracked
#' across cycles. Consecutive pairs are joined on their shared congress
#' column (e.g. cd108 links the 107-108 and 108-109 crosswalks).
#'
#' @param variable String — which quantity to match on:
#'   \describe{
#'     \item{`"pop"`}{Population overlap between the source and target
#'       districts (default).}
#'     \item{`"area"`}{Land area overlap, in square meters, between the
#'       source and target districts.}
#'     \item{`"afact_s2t"`}{Percent of the source district's population that
#'       falls in the target district.}
#'     \item{`"afact_t2s"`}{Percent of the target district's population that
#'       falls in the source district.}
#'     \item{`"afact_s2t_area"`}{Percent of the source district's area that
#'       falls in the target district.}
#'     \item{`"afact_t2s_area"`}{Percent of the target district's area that
#'       falls in the source district.}
#'   }
#'   Matching on an `afact_*` variable directly is not recommended — these
#'   are percentages, not overlap magnitudes, so a tiny sliver district that
#'   falls entirely inside a larger one can dominate the match. Prefer
#'   `"pop"` or `"area"` for matching, and use the `afact_*` variables for
#'   thresholding instead (via `threshold_vars`).
#' @param method String — which algorithm to match with:
#'   \describe{
#'     \item{`"hungarian"`}{Optimal one-to-one assignment that maximizes
#'       total overlap across all matched pairs, via the Hungarian algorithm
#'       ([clue::solve_LSAP()]) (default).}
#'     \item{`"greedy"`}{Iteratively claims the highest remaining overlap
#'       value, removing that source and target from consideration, until
#'       none remain above `threshold`.}
#'     \item{`"naive"`}{Matches districts by district number alone (e.g.
#'       district 3 to district 3), ignoring overlap entirely. Useful as a
#'       baseline for comparison.}
#'   }
#' @param data Nested list (`variable > cycle > state > matrix`); defaults to
#'   the package-bundled [overlap]. Supplying a custom `data` is not
#'   recommended — the package's safeguards assume the bundled data's
#'   structure and value ranges.
#' @param threshold Numeric; a pair is kept only if the allocation factor
#'   (from [threshold_vars_for()], or `threshold_vars` if supplied) is
#'   strictly greater than this value on both sides — pairs at or below it
#'   are zeroed out before matching (default `0`, i.e. exclude pairs with no
#'   allocation). `NA` means no threshold is imposed. Must be non-negative
#'   (or `NA`) when using the bundled [overlap] data, since its allocation
#'   factors are never negative.
#' @param incumbent_lock `"i2i"` (incumbent-to-incumbent matches),
#'   `"i2c"` (incumbent-to-candidate matches), or `NULL` to skip (default `NULL`).
#'   When set, incumbent pairs are locked in before matching runs, for every
#'   congress pair — their rows and columns are removed from the overlap
#'   matrix so the algorithm only matches the remaining districts. To
#'   validate matches against incumbent data after the fact instead, call
#'   [compute_incumbency_valid()] on a match_level panel.
#' @param lock_threshold Numeric; an incumbent pair is locked only if the
#'   allocation factor (from [threshold_vars_for()], or `threshold_vars` if
#'   supplied) is strictly greater than this value — pairs at or below it are
#'   left for the algorithm instead. Only applies when `incumbent_lock` is
#'   `"i2i"` or `"i2c"` (default `0`, i.e. lock pairs with strictly positive
#'   afact). `NA` means no threshold — all incumbent pairs are locked. Must
#'   be non-negative (or `NA`) when using the bundled [overlap] data.
#' @param threshold_vars Character vector of variable names to use for
#'   thresholding (both `lock_threshold` and `threshold`); each must be a
#'   variable present in `data`. Defaults to `NULL`, which means: for `"pop"`
#'   or `"area"`, both directional allocation factors (`afact_s2t` and
#'   `afact_t2s`, or their `_area` equivalents) must exceed the threshold;
#'   for any `afact_*` variable, only that variable itself is checked. See
#'   [threshold_vars_for()] for the exact mapping.
#' @param start_congress Integer; first congress in the panel. Must be
#'   `>= 92`, the earliest congress covered by the bundled [overlap] data,
#'   when using that data (default `92`).
#' @param end_congress Integer; last congress in the panel, greater than
#'   `start_congress`. Must be `<= 119`, the latest congress covered by the
#'   bundled [overlap] data, when using that data (default `119`).
#' @param shape One of `"long"` (default), `"wide"`, `"match_level"` — see
#'   Value below.
#'
#' @return
#' For `shape = "wide"`: a data.frame with columns `lineage_id`, `state_abb`,
#' `cd92` ... `cd119`. Match values are not included — call
#' [compute_match_factor()] on a match_level panel to get them for any
#' variable(s) you want.
#'
#' For `shape = "long"`: a data.frame with one row per district-congress with
#' columns `lineage_id`, `state_abb`, `congress`, `district`.
#'
#' For `shape = "match_level"`: a data.frame with one row per matched pair
#' with columns `source`, `target`, `cycle` (e.g. `"cd102_cd103"`),
#' `state_abb`. Match values are not included — call [compute_match_factor()]
#' to add them for any variable(s) you want.
#'
#' @examples
#' # default "long" shape, a small range for speed
#' panel_long <- build_district_panel(start_congress = 111, end_congress = 114)
#' panel_long
#'
#' # same range, "wide" shape for comparison
#' panel_wide <- build_district_panel(
#'   start_congress = 111, end_congress = 114, shape = "wide"
#' )
#' panel_wide
#'
#' # build a match_level panel, then validate it against incumbent data
#' match_level_panel <- build_district_panel(
#'   start_congress = 111, end_congress = 114, shape = "match_level"
#' )
#' validated_panel <- compute_incumbency_valid(match_level_panel, "i2i")
#' validated_panel
#'
#' # or add overlap values for the matched pairs instead
#' panel_with_overlap <- compute_match_factor(match_level_panel, vars = c("pop", "area"))
#' panel_with_overlap
#'
#' @export
build_district_panel <- function(
    variable = "pop", method = "hungarian", data = overlap, threshold = 0,
    incumbent_lock = NULL, lock_threshold = 0,
    threshold_vars = NULL, start_congress = 92, end_congress = 119, shape = "long"
) {

  # start_congress/end_congress must be numeric before any arithmetic on them
  stopifnot(
    "start_congress and end_congress must be numeric" =
      is.numeric(start_congress) && is.numeric(end_congress)
  )

  # variable/method/shape must be one of their allowed strings
  variable <- match.arg(variable, c("pop", "area", "afact_s2t", "afact_t2s", "afact_s2t_area", "afact_t2s_area"))
  method   <- match.arg(method, c("hungarian", "greedy", "naive"))
  shape    <- match.arg(shape, c("long", "wide", "match_level"))

  # incumbent_lock must be 'i2i'/'i2c'/NULL — checked manually (not match.arg)
  # since match.arg silently treats NULL as "missing" and would coerce it to
  # the first choice instead of preserving it
  stopifnot(
    "incumbent_lock must be 'i2i', 'i2c', or NULL" =
      rlang::is_null(incumbent_lock) || incumbent_lock %in% c("i2i", "i2c")
  )

  last_start_congress <- end_congress - 1

  # end_congress must come after start_congress
  stopifnot(
    "end_congress must be greater than start_congress" =
      start_congress <= last_start_congress
  )

  # bundled data has known bounds/ranges — these checks don't apply to custom data
  if (missing(data)) {
    stopifnot(
      "start_congress and end_congress must fall within the bundled data's range of 92-119" =
        start_congress >= 92 && end_congress <= 119,
      "threshold must be >= 0 (or NA) when using the bundled data" =
        is.na(threshold) || threshold >= 0,
      "lock_threshold must be >= 0 (or NA) when using the bundled data" =
        is.na(lock_threshold) || lock_threshold >= 0
    )
  }

  # variable must actually exist in data (relevant for custom data)
  if(rlang::is_null(data[[variable]])) {
    stop(paste0(
      "No data found for variable='", variable
    ))
  }

  # every entry in threshold_vars must also exist in data
  if (!rlang::is_null(threshold_vars)) {
    missing_vars <- threshold_vars[!threshold_vars %in% names(data)]
    if (length(missing_vars) > 0) {
      stop(paste0(
        "No data found for threshold_vars: ", paste(missing_vars, collapse = ", ")
      ))
    }
  }

  # build the list of consecutive congress-pair cycle names to iterate over
  cycles <- purrr::map_chr(start_congress:last_start_congress, function(num) {
    paste0("cd", num, "_cd", num + 1)
  })

  # run match_crosswalk for each cycle and rename columns to congress-specific names
  cycle_crosswalks <- purrr::map(purrr::set_names(cycles), function(cycle) {
    parts <- strsplit(cycle, "_")[[1]]
    src   <- as.integer(gsub("cd", "", parts[1]))
    tgt   <- as.integer(gsub("cd", "", parts[2]))

    cw <- match_crosswalk(src, variable, method, data,
                          incumbent_lock = incumbent_lock,
                          lock_threshold = lock_threshold,
                          threshold = threshold,
                          threshold_vars = threshold_vars) |>
      dplyr::rename(
        !!paste0("cd", src) := source,
        !!paste0("cd", tgt) := target
      )

    cw
  })

  # stitch every cycle's crosswalk together by full-joining on their shared congress column
  result <- purrr::reduce2(
    utils::tail(cycle_crosswalks, -1),
    utils::tail(names(cycle_crosswalks), -1),
    function(acc, df, key) {
      join_col <- strsplit(key, "_")[[1]][1]
      dplyr::full_join(acc, df, by = c(join_col, "state_abb"), na_matches = "never")
    },
    .init = cycle_crosswalks[[1]]
  )

  # order columns/rows consistently regardless of shape
  result <- result |>
    dplyr::select(sort(names(result))) |>
    dplyr::arrange(state_abb)

  if (shape == "wide") {
    # wide: one row per lineage, one column per congress
    result |>
      dplyr::mutate(lineage_id = dplyr::row_number()) |>
      dplyr::relocate(lineage_id, state_abb)
  } else if (shape == "long") {
    # long: one row per district-congress
    result |>
      dplyr::mutate(lineage_id = dplyr::row_number()) |>
      dplyr::rename_with(~ stringr::str_replace(.x, "^cd(\\d+)$", "district__\\1")) |>
      tidyr::pivot_longer(
        cols            = dplyr::matches("__\\d+$"),
        names_to        = c(".value", "congress"),
        names_sep       = "__",
        names_transform = list(congress = as.integer)
      ) |>
      dplyr::filter(!is.na(district)) |>
      dplyr::relocate(lineage_id, state_abb, congress, district)

  } else {
    # match_level: one row per matched pair, tagged with its cycle
    purrr::map_dfr(names(cycle_crosswalks), function(cycle) {
      parts <- strsplit(cycle, "_")[[1]]
      src   <- as.integer(gsub("cd", "", parts[1]))
      tgt   <- as.integer(gsub("cd", "", parts[2]))

      cw <- cycle_crosswalks[[cycle]] |>
        dplyr::rename(
          source = !!paste0("cd", src),
          target = !!paste0("cd", tgt)
        ) |>
        dplyr::mutate(cycle = .env$cycle)

      cw
    }) |>
      dplyr::select(source, target, cycle, state_abb) |>
      dplyr::arrange(cycle, source, target)

  }
}
