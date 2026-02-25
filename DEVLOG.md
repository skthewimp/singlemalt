# Development Log — Single Malt Whisky Recommender

---

## 2026-02-25 — Radar colour palette

**Problem.** The default plotly colour cycle has arbitrary hue spacing and, more critically,
`opacity = 0.5` was being applied to the entire trace — meaning both the fill *and* the border
line became semi-transparent. When two traces overlap you lose the ability to clearly distinguish
their edges.

**Fix.** Two separate concerns, handled separately:

*Hue selection.* Used the **Okabe-Ito** palette — 8 colours designed for maximum perceptual
distance and full accessibility under the two most common forms of colour-blindness (deuteranopia,
protanopia). Amber is assigned position 1 so "your picks" always gets the warm whisky-toned
colour; recommendations get blue, teal, vermillion, etc.

```r
radar_palette <- c(
  "#E69F00",  # amber   — your picks
  "#0072B2",  # blue
  "#009E73",  # teal
  "#D55E00",  # vermillion
  "#CC79A7",  # mauve
  "#56B4E9",  # sky blue
  "#F0E442",  # yellow
  "#000000"   # black
)
```

*Fill vs. line opacity.* Solid lines (`width = 2`, full opacity) let you trace each shape's
boundary clearly. Fills use `adjustcolor(col, alpha.f = 0.15)` — light enough that overlapping
regions remain visible through each other.  `opacity = 1` on the trace keeps the line crisp.

```r
fill_col <- adjustcolor(col, alpha.f = 0.15)
add_trace(p, ..., fillcolor = fill_col, line = list(color = col, width = 2), opacity = 1)
```

The palette cycles via `(i - 1) %% length(radar_palette) + 1`, so it handles any number of
traces without error (though beyond 8 traces the cycle repeats, which is an acceptable edge case
given typical usage).

---

This file documents the decisions, reasoning, and code changes made to this app.
New entries go at the top.

---

## 2026-02-25 — Initial clean-up, UI redesign, and GitHub publish

### Starting point

The app was a single 55-line `app.R` sitting in a large, flat working directory (`data work/whisky/singlemalt/`). It worked, but had accumulated technical debt:

- Used deprecated `tidyr::gather()` instead of `pivot_longer()`
- `tableOutput` / `renderTable` — no sorting, no formatting
- No theming, plain Bootstrap default
- No README, no `.gitignore`, no version control
- The title still read "Bespoke Data Insights Ltd"
- `whiskies.RData` was the only copy of the data — binary, not human-readable on GitHub

### What we changed and why

#### 1. Project housekeeping

Added `.gitignore` (ignores `.Rhistory`, `.DS_Store`, `rsconnect/`), exported `whiskies.csv` alongside the `.RData` so the data is readable in GitHub's browser, and wrote a `README.md` with a usage guide and data source credit. Initialised git and published to [`skthewimp/singlemalt`](https://github.com/skthewimp/singlemalt).

#### 2. Modernised data prep (`pivot_longer`)

`gather()` was soft-deprecated in tidyr 1.0 (2019) and removed in 1.3. Replaced with the direct equivalent:

```r
# before
wh %>% gather(Character, Score, -Distillery)

# after
wh |> pivot_longer(all_of(flavor_cols), names_to = "flavor", values_to = "score")
```

#### 3. Theming with `bslib`

Switched to `bslib::page_sidebar()` with `bs_theme(bootswatch = "sandstone", version = 5)`. Sandstone uses warm amber/tan tones that suit a whisky theme and is readable on all screen sizes. This required no CSS; bslib handles it via Bootstrap 5.

#### 4. Interactive table with `DT`

Replaced `tableOutput` / `renderTable` with `DT::datatable`. Benefits: client-side sorting, pagination, and cleaner formatting. The `dom = "tip"` option hides the search box (not needed here) and keeps just the table, info line, and pagination controls.

#### 5. Single-page layout

Started with a three-tab layout (Recommendations / Flavour Profiles / About). Discarded after review — tabs add a navigation step that interrupts the flow. Replaced with `bslib::layout_columns()` putting the table (5 columns) and radar (7 columns) side by side on one screen.

#### 6. "Because" column — why is this a recommendation?

The raw similarity score tells you *how much* a distillery matches; it doesn't tell you *what* drives that match. Added a `Because` column showing the top 3 flavors that contributed most to each recommendation's similarity score.

The per-flavor contribution is the product of each dimension's values in the dot product used for cosine similarity. After unit-normalising both the reference profile and each candidate:

```
similarity = Σᵢ (score_zᵢ · ref_zᵢ)
```

Each term `score_zᵢ · ref_zᵢ` is that flavor's individual contribution. We take the top 3 positive contributors per candidate distillery:

```r
why <- per_flavor |>
  filter(contribution > 0) |>
  group_by(Distillery) |>
  slice_max(contribution, n = 3) |>
  summarise(Because = paste(tolower(flavor), collapse = ", "), .groups = "drop")
```

#### 7. Radar chart linked to table row selection

The initial radar showed a fixed comparison: user's picks averaged vs. the #1 recommendation. We added `selection = "multiple"` to the DT table so clicking rows drives what appears on the radar. When no rows are selected it falls back to showing the top recommendation; when rows are selected each one gets its own trace so you can compare multiple candidates side by side.

```r
selected_rows <- input$tbl_recommendations_rows_selected
chart_recos <- if (length(selected_rows) > 0) {
  reco()$Distillery[selected_rows]
} else {
  reco()$Distillery[1]
}
```

#### 8. Data-driven circular ordering of radar labels

**The problem.** The order of axes around a radar chart is not cosmetic — it determines which flavors are adjacent (visually similar) and which are opposite (visually contrasted). Arbitrary or alphabetical ordering makes the shapes hard to read and compare across distilleries.

**The principle.** We want:
- Flavors that tend to co-occur in the same whiskies → placed adjacent on the wheel
- Flavors that are anti-correlated → placed on opposite sides of the wheel

**The method.** We compute the Pearson correlation matrix of all 12 flavor columns across the 86 distilleries, convert to a dissimilarity matrix (`1 − r`), and run Ward's-linkage hierarchical clustering. Reading off the dendrogram's leaf order gives a linear sequence that maximises adjacency similarity. Wrapping that sequence into a circle is the radar axis order.

```r
flavor_cor   <- wh |> select(all_of(flavor_cols)) |> cor()
flavor_order <- flavor_cols[hclust(as.dist(1 - flavor_cor), method = "ward.D2")$order]
```

**The result.** The resulting order, with adjacent-pair correlations:

| Pair | r |
|------|---|
| Tobacco → Smoky | 0.37 |
| Smoky → Medicinal | 0.69 |
| Medicinal → Nutty | −0.11 |
| Nutty → Body | 0.13 |
| Body → Winey | 0.41 |
| Winey → Sweetness | 0.12 |
| Sweetness → Honey | 0.13 |
| Honey → Malty | 0.31 |
| Malty → Spicy | 0.04 |
| Spicy → Fruity | 0.14 |
| Fruity → Floral | 0.26 |
| Floral → Tobacco (wrap) | −0.21 |

Three natural clusters emerge:
- **Peaty/smoky**: Tobacco, Smoky, Medicinal — the Islay signature
- **Rich/full**: Nutty, Body, Winey
- **Light/sweet**: Sweetness, Honey, Malty, Spicy, Fruity, Floral

The wrap-around pair (Floral ↔ Tobacco, r = −0.21) is the most anti-correlated adjacent pair, meaning the lightest, most floral malts sit directly opposite the smokiest ones — which is exactly the perceptual reality. This ordering is computed once at startup and applied consistently to every chart render.

### What we did not change

- **The similarity algorithm itself.** Cosine similarity on z-score-normalised flavor vectors is statistically sound and gives sensible results (e.g. Talisker + Ardbeg → Oban, Lagavulin, Laphroaig). No reason to change it.
- **The data.** Same 86 distilleries, 12 flavor columns, originally from the [University of Strathclyde Scotch Whisky dataset](https://www.mathstat.strath.ac.uk/outreach/nessie/nessie_whisky.html).
- **The shinyapps.io deployment URL.** The `rsconnect/` config is gitignored but kept locally so re-deploying to `kart.shinyapps.io/singlemalt` requires no reconfiguration.

### Packages added

| Package | Why |
|---------|-----|
| `bslib` | Bootstrap 5 theming, `page_sidebar`, `layout_columns`, `card` |
| `DT` | Interactive sortable table |
| `plotly` | Interactive radar (scatterpolar) chart |
