#' Match districts by district number alone
#'
#' Matches congressional districts across redistricting cycles by district
#' number alone (e.g. source district 3 -> target district 3), ignoring
#' overlap entirely. Useful as a baseline for comparison. Districts with no
#' same-numbered counterpart appear with `NA` in the unmatched column. Most
#' users should start with [build_district_panel()] instead — it calls this
#' function internally for every state and congress pair in a range.
#'
#' @param source_congress Integer, e.g. 112. Matched against the next
#'   congress, `source_congress + 1`.
#' @param variable String — one of `"pop"`, `"area"`, `"afact_s2t"`,
#'   `"afact_t2s"`, `"afact_s2t_area"`, `"afact_t2s_area"`. Only used to pick
#'   which matrix's district list to match against — since district-number
#'   matching ignores overlap values entirely, the choice makes no difference
#'   to the result. Call [compute_match_factor()] afterward if you want an
#'   overlap value for these pairs.
#' @param state Postal abbreviation string, e.g. `"AL"`.
#' @param data Nested list (`variable > cycle > state > matrix`); defaults to
#'   the package-bundled [overlap]. Supplying a custom `data` is not
#'   recommended — the package's safeguards assume the bundled data's
#'   structure and value ranges.
#'
#' @return A data.frame with columns `source`, `target`.
#'   Call [compute_match_factor()] on a match_level panel if you need the
#'   matched pairs' overlap values.
#'
#' @export
naive_match <- function(source_congress, variable, state, data = overlap) {

  # source_congress must be numeric before any arithmetic on it
  stopifnot(
    "source_congress must be numeric" =
      is.numeric(source_congress)
  )

  # variable must be one of its allowed strings
  variable <- match.arg(variable, c("pop", "area", "afact_s2t", "afact_t2s", "afact_s2t_area", "afact_t2s_area"))

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

  # nothing to match if either side has no districts
  if (nrow(mat) == 0 && ncol(mat) == 0) {
    return(data.frame(
      source = character(0),
      target = character(0),
      stringsAsFactors = FALSE
    ))
  }

  src_districts <- rownames(mat)
  tgt_districts <- colnames(mat)

  # pull the district number off the end of each dimname (e.g. "CA-03" -> 3)
  src_nums <- as.integer(sub(".*-", "", src_districts))
  tgt_nums <- as.integer(sub(".*-", "", tgt_districts))

  # pair each source district to the target with the same number (NA if none)
  tgt_matched <- tgt_districts[match(src_nums, tgt_nums)]

  result <- data.frame(
    source = src_districts,
    target = tgt_matched,
    stringsAsFactors = FALSE
  )

  # append target districts with no same-numbered source
  orphan_tgts <- tgt_districts[!tgt_nums %in% src_nums]
  if (length(orphan_tgts) > 0) {
    result <- rbind(result, data.frame(
      source = NA_character_,
      target = orphan_tgts,
      stringsAsFactors = FALSE
    ))
  }

  result |> `rownames<-`(NULL)
}
