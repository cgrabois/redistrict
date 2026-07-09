#' Run a district-matching method across all states for one congress pair
#'
#' Runs a district-matching method across all states for a given congress
#' pair and returns the combined results as a single data.frame.
#'
#' @param source_congress Integer, e.g. 112. Matched against the next
#'   congress, `source_congress + 1`.
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
#'   the package-bundled [overlap].
#' @param incumbent_lock `"i2i"` (win-only incumbent-to-incumbent matches),
#'   `"i2c"` (incumbent-to-candidate matches), or `NULL` to skip (default `NULL`).
#'   When set, incumbent pairs are locked in before the matching algorithm
#'   runs — their rows and columns are removed from the overlap matrix so the
#'   algorithm only matches the remaining districts. To validate matches
#'   against incumbent data after the fact instead, call
#'   [compute_incumbency_valid()] on a match_level panel.
#' @param lock_threshold Numeric; an incumbent pair is locked only if the
#'   allocation factor (from [threshold_vars_for()], or `threshold_vars` if
#'   supplied) is strictly greater than this value — pairs at or below it are
#'   left for the algorithm instead. Only applies when `incumbent_lock` is
#'   `"i2i"` or `"i2c"` (default `0`, i.e. lock pairs with strictly positive
#'   afact). `NA` means no threshold — all incumbent pairs are locked.
#' @param threshold Numeric; a pair is kept only if the allocation factor
#'   (from [threshold_vars_for()], or `threshold_vars` if supplied) is
#'   strictly greater than this value on both sides — pairs at or below it
#'   are zeroed out before matching (default `0`, i.e. exclude pairs with no
#'   allocation). `NA` means no threshold is imposed.
#' @param threshold_vars Character vector of variable names to use for
#'   thresholding (both `lock_threshold` and `threshold`). Defaults to
#'   `NULL`, which means: for `"pop"` or `"area"`, both directional
#'   allocation factors (`afact_s2t` and `afact_t2s`, or their `_area`
#'   equivalents) must exceed the threshold; for any `afact_*` variable,
#'   only that variable itself is checked. See [threshold_vars_for()] for
#'   the exact mapping.
#'
#' @return A data.frame with columns `source`, `target`, `state_abb`. Call
#'   [compute_match_factor()] on a match_level panel if you need the matched
#'   pairs' overlap values.
#'
#' @export
match_crosswalk <- function(
    source_congress, variable = "pop", method = "hungarian", data = overlap,
    incumbent_lock = NULL, lock_threshold = 0, threshold = 0,
    threshold_vars = NULL
) {

  incumbent_match_data <- resolve_incumbent_match_data(incumbent_lock)

  if(!is.na(lock_threshold) && lock_threshold != 0 && rlang::is_null(incumbent_lock)) {
    stop("lock_threshold has no effect unless incumbent_lock is 'i2i' or 'i2c'")
  }

  method <- match.arg(method, c("hungarian", "greedy", "naive"))
  cycle  <- paste0("cd", source_congress, "_cd", source_congress + 1)

  if(rlang::is_null(data[[variable]][[cycle]])) {
    stop(paste0(
      "No data found for variable='", variable,
      "', cycle='", cycle
    ))
  }
  states <- names(data[[variable]][[cycle]])

  matches <- data.frame()

  if(!rlang::is_null(incumbent_lock)) {

    # Lock all incumbent pairs, then drop any whose allocation factor falls at
    # or below lock_threshold — those are left in the matrix for the algorithm.
    matches <- incumbent_match_data |>
      dplyr::filter(cycle == .env$cycle) |>
      dplyr::transmute(
        source    = incumbent_from,
        target    = incumbent_to,
        state_abb = substr(incumbent_from, 1, 2)
      )

    if (!is.na(lock_threshold)) {
      lock_tvars <- if (rlang::is_null(threshold_vars)) threshold_vars_for(variable) else threshold_vars
      for (tv in lock_tvars) {
        matches <- matches |>
          dplyr::mutate(
            .tv = purrr::map2_dbl(
              source, target,
              ~ data[[tv]][[cycle]][[substr(.x, 1, 2)]][.x, .y]
            )
          ) |>
          dplyr::filter(.tv > lock_threshold) |>
          dplyr::select(-.tv)
      }
    }

    pop_matrix <- function(matrix, row_names, col_names) {
      matrix[!rownames(matrix) %in% row_names, !colnames(matrix) %in% col_names, drop = FALSE]
    }

    for(state in states) {

      inc_matches_state <- matches |>
        dplyr::filter(state_abb == state)

      source_dists <- inc_matches_state |> dplyr::pull(source)
      target_dists <- inc_matches_state |> dplyr::pull(target)

      data[[variable]][[cycle]][[state]] <-
        pop_matrix(
          data[[variable]][[cycle]][[state]],
          source_dists, target_dists
        )

    }
  }

  match_fn <- switch(method,
                     hungarian = function(st) hungarian_match(source_congress, variable, st, data, threshold, threshold_vars),
                     greedy    = function(st) greedy_match(source_congress, variable, st, data, threshold, threshold_vars),
                     naive     = function(st) naive_match(source_congress, variable, st, data)
  )

  matches <- purrr::map_dfr(states, match_fn) |>
    dplyr::mutate(
      state_abb = dplyr::case_when(
        !is.na(source) ~ substr(source, 1, 2),
        !is.na(target) ~ substr(target, 1, 2)
      )
    ) |>
    dplyr::bind_rows(matches) |>
    dplyr::arrange(state_abb, source, target)

  return(matches)

}
