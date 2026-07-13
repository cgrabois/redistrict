#' Match districts by greedily claiming the highest overlap
#'
#' Matches congressional districts across redistricting cycles by iteratively
#' claiming the highest available overlap value, then removing that source
#' row and target column so no district is matched twice. Stops when no
#' remaining value exceeds the threshold. Most users should start with
#' [build_district_panel()] instead — it calls this function internally for
#' every state and congress pair in a range.
#'
#' @param source_congress Integer, e.g. 111. Matched against the next
#'   congress, `source_congress + 1`.
#' @param variable String — which quantity to match on:
#'   \describe{
#'     \item{`"pop"`}{Population overlap between the source and target
#'       districts.}
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
#' @param state Postal abbreviation string, e.g. `"AL"`.
#' @param data Nested list (`variable > cycle > state > matrix`); defaults to
#'   the package-bundled [overlap]. Supplying a custom `data` is not
#'   recommended — the package's safeguards assume the bundled data's
#'   structure and value ranges.
#' @param threshold Numeric; a pair is kept only if the allocation factor
#'   (from [threshold_vars_for()], or `threshold_vars` if supplied) is
#'   strictly greater than this value — pairs at or below it are zeroed out
#'   before the loop so the greedy pass skips them. Default `0` excludes
#'   zero-allocation pairs. `NA` imposes no threshold. Must be non-negative
#'   (or `NA`) when using the bundled [overlap] data.
#' @param threshold_vars Character vector of variable names to use for
#'   thresholding; each must be a variable present in `data`. Defaults to
#'   `NULL`, which means: for `"pop"` or `"area"`, both directional
#'   allocation factors (`afact_s2t` and `afact_t2s`, or their `_area`
#'   equivalents) must exceed the threshold; for any `afact_*` variable,
#'   only that variable itself is checked. See [threshold_vars_for()] for
#'   the exact mapping.
#'
#' @return A data.frame with columns `source`, `target`.
#'   Call [compute_match_factor()] on a match_level panel if you need the
#'   matched pairs' overlap values.
#'
#' @export
greedy_match <- function(source_congress, variable, state, data = overlap, threshold = 0, threshold_vars = NULL) {

  # source_congress must be numeric before any arithmetic on it
  stopifnot(
    "source_congress must be numeric" =
      is.numeric(source_congress)
  )

  # variable must be one of its allowed strings
  variable <- match.arg(variable, c("pop", "area", "afact_s2t", "afact_t2s", "afact_s2t_area", "afact_t2s_area"))

  # bundled data's allocation factors are never negative
  if (missing(data)) {
    stopifnot(
      "threshold must be >= 0 (or NA) when using the bundled data" =
        is.na(threshold) || threshold >= 0
    )
  }

  # e.g. "cd112_cd113"
  cycle <- paste0("cd", source_congress, "_cd", source_congress + 1)

  mat <- data[[variable]][[cycle]][[state]]

  # variable/cycle/state must actually exist in data (relevant for custom data)
  if (is.null(mat)) {
    stop(paste0(
      "No data found for variable='", variable,
      "', cycle='", cycle,
      "', state='", state, "'"
    ))
  }

  # the matching algorithm doesn't handle missing overlap values
  if (anyNA(mat)) {
    stop(paste0(
      "data[['", variable, "']][['", cycle, "']][['", state, "']] contains NA values"
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

  remaining <- mat

  # zero out pairs at or below threshold so the greedy pass skips them
  if (!is.na(threshold)) {
    tvars <- if (rlang::is_null(threshold_vars)) threshold_vars_for(variable) else threshold_vars
    for (tv in tvars) {
      tmat <- data[[tv]][[cycle]][[state]]
      tmat <- tmat[rownames(remaining), colnames(remaining), drop = FALSE]
      remaining[is.na(tmat) | tmat <= threshold] <- 0
    }
  }

  matches   <- list()

  # repeatedly claim the highest remaining value, removing its row/column so
  # neither district can be matched again
  while (nrow(remaining) > 0 && ncol(remaining) > 0) {
    best_val <- max(remaining)
    if (!is.na(threshold) && best_val <= 0) break

    # arr.ind can return more than one row when several pairs tie for best_val;
    # [1, ] breaks the tie by taking the first in column-major order (R's
    # default array order — down each row within the first column, then the
    # next column, etc.)
    pos <- which(remaining == best_val, arr.ind = TRUE)[1, ]
    matches[[length(matches) + 1]] <- data.frame(
      source           = rownames(remaining)[pos["row"]],
      target           = colnames(remaining)[pos["col"]],
      stringsAsFactors = FALSE
    )

    remaining <- remaining[-pos["row"], -pos["col"], drop = FALSE]
  }

  # combine the claimed matches into one data.frame (empty if none were claimed)
  result <- if (length(matches) > 0) {
    do.call(rbind, matches)
  } else {
    data.frame(
      source           = character(0),
      target           = character(0),
      stringsAsFactors = FALSE
    )
  }

  # Unmatched sources: rows still present when loop exited
  if (nrow(remaining) > 0) {
    result <- rbind(result, data.frame(
      source           = rownames(remaining),
      target           = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  # Unmatched targets: columns still present when loop exited
  if (ncol(remaining) > 0) {
    result <- rbind(result, data.frame(
      source           = NA_character_,
      target           = colnames(remaining),
      stringsAsFactors = FALSE
    ))
  }

  result |> dplyr::arrange(source) |> `rownames<-`(NULL)
}
