#' Count the district comparisons in a panel
#'
#' Returns the number of district pairs (comparisons) a
#' [build_district_panel()] panel contains. For `"wide"` and `"long"` panels,
#' every lineage contributes one comparison for each pair of congresses in
#' which it is observed (not just adjacent ones). For `"match_level"` panels,
#' lineages are first rebuilt by chaining matches across cycles.
#'
#' @param panel A data.frame from [build_district_panel()].
#' @param shape The shape `panel` was built with: `"long"`, `"wide"`, or
#'   `"match_level"`. If `NA` (default), it is inferred from the panel's column
#'   names, and an error is raised if they match no shape.
#'
#' @return A single number: the total comparison count.
#'
#' @examples
#' match_level_panel <- build_district_panel(
#'   start_congress = 111, end_congress = 114, shape = "match_level"
#' )
#' compute_comparison_count(match_level_panel)  # shape inferred
#' compute_comparison_count(match_level_panel, "match_level")
#'
#' @export
compute_comparison_count <- function(panel, shape = NA) {

  if (length(shape) == 1 && is.na(shape)) {
    # infer the shape from the column names build_district_panel() gives it
    cols <- names(panel)
    if (all(c("source", "target", "cycle") %in% cols)) {
      shape <- "match_level"
    } else if (all(c("lineage_id", "congress", "district") %in% cols)) {
      shape <- "long"
    } else if ("lineage_id" %in% cols && any(grepl("^cd\\d{2,3}$", cols))) {
      shape <- "wide"
    } else {
      stop(
        "Could not infer the panel's shape from its columns (",
        paste(cols, collapse = ", "), "). Expected `source`, `target`, `cycle` ",
        '(match_level); `lineage_id`, `congress`, `district` (long); or ',
        "`lineage_id` and `cd<congress>` columns (wide). Is this a ",
        "build_district_panel() panel? Otherwise, set `shape` explicitly.",
        call. = FALSE
      )
    }
  }

  shape <- match.arg(shape, c("long", "wide", "match_level"))

  if (shape == "wide") {

    # one row per lineage: count its non-NA cd* columns (times observed),
    # then n choose 2 pairs per lineage, summed
    panel |>
      dplyr::mutate(
        n_times_observed = rowSums(!is.na(dplyr::pick(dplyr::matches("^cd\\d{2,3}$")))),
        n_comparisons = choose(n_times_observed, 2)
      ) |>
      dplyr::pull(n_comparisons) |>
      sum()

  } else if (shape == "long") {

    # one row per lineage-congress: rows per lineage = times observed,
    # then n choose 2 pairs per lineage, summed
    panel |>
      dplyr::group_by(lineage_id) |>
      dplyr::summarise(
        n_times_observed = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        n_comparisons = choose(n_times_observed, 2)
      ) |>
      dplyr::pull(n_comparisons) |>
      sum()

  } else {

    # The match_level panel has no lineage_id: each row only links a district
    # to its match in the next congress. So we first rebuild the lineages by
    # chaining those links, then count pairs per lineage as in the other shapes.

    # Step 1: turn each matched row into an edge between two "nodes". A node is
    # a district in a specific congress (e.g. "AL-01 92"), since the same
    # district name shows up in many congresses. Rows with an NA on either
    # side are unmatched, so they link nothing and are dropped.
    edges <- panel |>
      dplyr::filter(!is.na(source), !is.na(target)) |>
      dplyr::mutate(
        # pull the two congress numbers out of the cycle label, e.g. "cd92_cd93"
        src_congress = as.integer(sub("^cd(\\d+)_cd\\d+$", "\\1", cycle)),
        tgt_congress = as.integer(sub("^cd\\d+_cd(\\d+)$", "\\1", cycle)),
        src_node = paste(source, src_congress),
        tgt_node = paste(target, tgt_congress)
      ) |>
      # earliest cycles first, so a lineage is labeled before it is extended
      dplyr::arrange(src_congress)

    # Step 2: label every node with a lineage. Going cycle by cycle, a source
    # not seen before starts a new lineage (labeled with its own name), and its
    # target joins that same lineage. `lineage` is a named vector: node -> label.
    lineage <- character(0)
    for (i in seq_len(nrow(edges))) {
      src <- edges$src_node[i]
      if (is.na(lineage[src])) lineage[src] <- src
      lineage[edges$tgt_node[i]] <- lineage[[src]]
    }

    # Step 3: nodes per lineage = times observed. Then n choose 2 per lineage,
    # summed. Districts that never matched aren't in `lineage`, which is fine
    # since a lineage observed once has zero pairs.
    sum(choose(as.vector(table(lineage)), 2))

  }

}
