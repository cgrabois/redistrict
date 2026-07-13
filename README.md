# redistrict

Match U.S. congressional districts across redistricting cycles, based on population/area overlap.

`redistrict` provides three matching algorithms (Hungarian, greedy, naive) for tracking which districts in one congress correspond to which districts in the next, plus helpers for building multi-cycle panels and validating matches against actual incumbent behavior.

## Installation

```r
pak::pkg_install("cgrabois/redistrict")
```

or, with devtools:

```r
devtools::install_github("cgrabois/redistrict")
```

## Getting started

[`build_district_panel()`](R/build_district_panel.R) is the main entry point — it builds a full multi-cycle panel of matched districts in a single call:

```r
library(redistrict)

panel <- build_district_panel(
  variable       = "pop",
  method         = "hungarian"
)
```

`shape` controls the output format: `"long"` (default, one row per district-congress), `"wide"` (one row per lineage, one column per congress), or `"match_level"` (one row per matched pair, tagged with its cycle).

## Matching algorithms

- **`hungarian_match()`** — optimal one-to-one assignment that maximizes total overlap across all matched pairs ([`clue::solve_LSAP()`]).
- **`greedy_match()`** — iteratively claims the highest remaining overlap value.
- **`naive_match()`** — matches districts by district number alone, ignoring overlap; useful as a baseline.

`match_crosswalk()` runs any of these across every state for a single congress pair; `build_district_panel()` runs `match_crosswalk()` across a whole range of congresses and stitches the results into one panel.

## Validating matches

- **`compute_incumbency_valid()`** — checks matched pairs against actual incumbent behavior (did the algorithm send a district to where its real incumbent went?).
- **`compute_match_factor()`** — adds overlap/allocation values (for any variable) to a `match_level` panel.

## Bundled data

The package ships with pre-computed overlap data so it works out of the box:

- **`overlap`** — nested list of overlap matrices (`variable > cycle > state > matrix`), covering every consecutive congress pair from the 92nd through the 119th Congress.
- **`incumbency_matches_i2i`** — incumbent-to-incumbent district matches (the same person served as incumbent on both sides of the cycle).
- **`incumbency_matches_i2c`** — incumbent-to-candidate district matches (an incumbent ran for the district after redistricting).

See `?overlap`, `?incumbency_matches_i2i`, `?incumbency_matches_i2c` for details.

## License

MIT
