#' Match districts by maximizing total overlap (Hungarian algorithm)
#'
#' Matches congressional districts across redistricting cycles by maximizing
#' total overlap (of whichever variable is specified) using the Hungarian
#' algorithm ([clue::solve_LSAP()]). Most users should start with
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
#' @param threshold Numeric; a pair is kept only if either allocation factor
#'   (from [threshold_vars_for()], or `threshold_vars` if supplied) is
#'   strictly greater than this value — pairs at or below it are zeroed out
#'   before solving and un-matched if assigned (both districts returned with
#'   `NA`). Default `0` excludes pairs where either afact is zero. `NA`
#'   imposes no threshold. Must be non-negative (or `NA`) when using the
#'   bundled [overlap] data.
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
#' @examples
#' # cd112_cd113 spans the post-2010-census redistricting cycle
#' hungarian_result <- hungarian_match(112, "pop", "CA")
#' hungarian_result
#'
#' # compare against greedy_match() on the same inputs — greedy isn't
#' # guaranteed to find the same (optimal) assignment
#' greedy_result <- greedy_match(112, "pop", "CA")
#' greedy_result
#'
#' @export
hungarian_match <- function(
  source_congress, variable, state, data = overlap,
  threshold = 0, threshold_vars = NULL
) {

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

  # nothing to match if either side has no districts
  if (nrow(mat) == 0 && ncol(mat) == 0) {
    return(data.frame(
      source = character(0),
      target = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # solve_LSAP requires ncol >= nrow; transpose if source has more districts
  # than target, then reconstruct the assignment in original orientation.
  transposed <- nrow(mat) > ncol(mat)
  cost_mat   <- if (transposed) t(mat) else mat

  # zero out pairs at or below threshold so solve_LSAP won't assign them
  if (!is.na(threshold)) {
    tvars <- if (rlang::is_null(threshold_vars)) threshold_vars_for(variable) else threshold_vars
    for (tv in tvars) {
      tmat <- data[[tv]][[cycle]][[state]]
      tmat <- if (transposed) t(tmat) else tmat
      tmat <- tmat[rownames(cost_mat), colnames(cost_mat), drop = FALSE]
      cost_mat[is.na(tmat) | tmat <= threshold] <- 0
    }
  }

  # solve for the assignment that maximizes total overlap
  assignment <- clue::solve_LSAP(cost_mat, maximum = TRUE)
  idx        <- as.integer(assignment)

  if (transposed) {
    # cost_mat rows = target districts, cols = source districts
    # assignment[i] = j  =>  target[i] matched to source[j]
    matched <- data.frame(
      source           = colnames(cost_mat)[idx],
      target           = rownames(cost_mat),
      match_factor     = cost_mat[cbind(seq_len(nrow(cost_mat)), idx)],
      stringsAsFactors = FALSE
    )
    # Sources with no target counterpart (more source districts than target)
    unmatched_idx <- setdiff(seq_len(ncol(cost_mat)), idx)
    if (length(unmatched_idx) > 0) {
      matched <- rbind(matched, data.frame(
        source           = colnames(cost_mat)[unmatched_idx],
        target           = NA_character_,
        match_factor     = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
  } else {
    # cost_mat rows = source districts, cols = target districts
    # assignment[i] = j  =>  source[i] matched to target[j]
    matched <- data.frame(
      source           = rownames(cost_mat),
      target           = colnames(cost_mat)[idx],
      match_factor     = cost_mat[cbind(seq_len(nrow(cost_mat)), idx)],
      stringsAsFactors = FALSE
    )
    # Targets with no source counterpart (more target districts than source)
    unmatched_idx <- setdiff(seq_len(ncol(cost_mat)), idx)
    if (length(unmatched_idx) > 0) {
      matched <- rbind(matched, data.frame(
        source           = NA_character_,
        target           = colnames(cost_mat)[unmatched_idx],
        match_factor     = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
  }

  # Un-match any pair the algorithm assigned to a 0-value cell: split into
  # two separate unmatched rows so neither district is treated as matched.
  # Skipped when threshold is NA (0-overlap pairs are intentionally kept).
  if (!is.na(threshold)) {
    zero_mask <- !is.na(matched$match_factor) & matched$match_factor == 0
    if (any(zero_mask)) {
      zeroed <- matched[zero_mask, ]
      matched <- matched[!zero_mask, ]
      matched <- rbind(
        matched,
        data.frame(source = zeroed$source, target = NA_character_, match_factor = NA_real_, stringsAsFactors = FALSE),
        data.frame(source = NA_character_, target = zeroed$target, match_factor = NA_real_, stringsAsFactors = FALSE)
      )
    }
  }

  matched |> dplyr::select(-match_factor) |> dplyr::arrange(source) |> `rownames<-`(NULL)
}
