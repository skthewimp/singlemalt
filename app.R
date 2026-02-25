library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(plotly)

# ── Data ────────────────────────────────────────────────────────────────────

load("whiskies.RData")

flavor_cols <- setdiff(names(wh), "Distillery")
distillery_list <- sort(unique(wh$Distillery))

# Z-score normalize each flavor across all distilleries
wh_long <- wh |>
  pivot_longer(all_of(flavor_cols), names_to = "flavor", values_to = "score") |>
  group_by(flavor) |>
  mutate(score_z = (score - mean(score)) / sd(score)) |>
  ungroup()

# Fixed circular ordering of flavors for the radar chart:
# cluster flavors by their inter-correlation across distilleries so that
# similar flavors sit adjacent on the wheel and dissimilar ones face each other.
flavor_cor   <- wh |> select(all_of(flavor_cols)) |> cor()
flavor_order <- flavor_cols[hclust(as.dist(1 - flavor_cor), method = "ward.D2")$order]

# Radar chart palette — Okabe-Ito (colorblind-safe, maximum perceptual spacing).
# Amber is first so "your picks" always gets the warm whisky-toned colour.
# Traces use solid lines at full opacity + very light fills so overlaps stay readable.
radar_palette <- c(
  "#E69F00",  # amber        — your picks
  "#0072B2",  # blue
  "#009E73",  # teal
  "#D55E00",  # vermillion
  "#CC79A7",  # mauve
  "#56B4E9",  # sky blue
  "#F0E442",  # yellow
  "#000000"   # black
)

# ── Similarity helpers ───────────────────────────────────────────────────────

# Unit-normalise a numeric vector
unit_norm <- function(x) x / sqrt(sum(x^2))

# Compute cosine similarity + top contributing flavors for each distillery
compute_similarity <- function(ref_distilleries) {
  ref_profile <- wh_long |>
    filter(Distillery %in% ref_distilleries) |>
    group_by(flavor) |>
    summarise(ref_z = mean(score_z), .groups = "drop") |>
    mutate(ref_z = unit_norm(ref_z))

  per_flavor <- wh_long |>
    filter(!Distillery %in% ref_distilleries) |>
    group_by(Distillery) |>
    mutate(score_z = unit_norm(score_z)) |>
    ungroup() |>
    inner_join(ref_profile, by = "flavor") |>
    mutate(contribution = score_z * ref_z)

  # Top 3 positively contributing flavors per distillery
  why <- per_flavor |>
    filter(contribution > 0) |>
    group_by(Distillery) |>
    slice_max(contribution, n = 3) |>
    summarise(Because = paste(tolower(flavor), collapse = ", "), .groups = "drop")

  per_flavor |>
    group_by(Distillery) |>
    summarise(similarity = sum(contribution), .groups = "drop") |>
    left_join(why, by = "Distillery") |>
    arrange(desc(similarity))
}

# ── UI ──────────────────────────────────────────────────────────────────────

ui <- page_sidebar(
  title = "Single Malt Whisky Recommender",
  theme = bs_theme(bootswatch = "sandstone", version = 5),

  sidebar = sidebar(
    width = 280,
    selectizeInput(
      "ref_malts",
      "Your favourite single malts",
      choices  = distillery_list,
      multiple = TRUE,
      options  = list(placeholder = "Start typing a distillery…")
    ),
    numericInput("n_reco", "Max recommendations", value = 10, min = 1, max = 50),
    hr(),
    helpText("Select one or more whiskies you enjoy to see what else you might like.")
  ),

  layout_columns(
    col_widths = c(5, 7),
    card(
      card_header("Recommendations"),
      helpText("Click rows to compare their flavour profiles on the right."),
      DTOutput("tbl_recommendations")
    ),
    card(
      card_header("Flavour profiles"),
      plotlyOutput("plot_radar", height = "420px")
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Reactive: similarity scores for the selected reference malts
  reco <- reactive({
    req(length(input$ref_malts) > 0)
    compute_similarity(input$ref_malts) |>
      head(input$n_reco)
  })

  # ── Recommendations table ──────────────────────────────────────────────────
  output$tbl_recommendations <- renderDT({
    req(reco())
    reco() |>
      mutate(
        Rank       = row_number(),
        Similarity = paste0(round(similarity * 100, 1), "%")
      ) |>
      select(Rank, Distillery, Similarity, Because) |>
      datatable(
        rownames  = FALSE,
        selection = "multiple",
        options   = list(pageLength = 10, dom = "tip"),
        class     = "hover stripe"
      )
  })

  # ── Flavour radar chart ────────────────────────────────────────────────────
  output$plot_radar <- renderPlotly({
    req(length(input$ref_malts) > 0, nrow(reco()) > 0)

    # Distilleries to show from the table: selected rows, or top 1 as default
    selected_rows <- input$tbl_recommendations_rows_selected
    chart_recos <- if (length(selected_rows) > 0) {
      reco()$Distillery[selected_rows]
    } else {
      reco()$Distillery[1]
    }

    # Label for the user's picks
    sel <- input$ref_malts
    sel_label <- if (length(sel) <= 3) {
      paste(sel, collapse = " + ")
    } else {
      paste0(paste(sel[1:3], collapse = " + "), " + ", length(sel) - 3, " more")
    }

    # Build profile data: user's picks averaged into one trace, each reco as its own
    user_profile <- wh_long |>
      filter(Distillery %in% sel) |>
      group_by(flavor) |>
      summarise(score = mean(score), .groups = "drop") |>
      mutate(group = sel_label)

    reco_profiles <- wh_long |>
      filter(Distillery %in% chart_recos) |>
      rename(group = Distillery) |>
      group_by(group, flavor) |>
      summarise(score = mean(score), .groups = "drop")

    profile <- bind_rows(user_profile, reco_profiles)
    flavors <- flavor_order  # fixed data-driven order, consistent across all charts

    # Build one trace per group — solid lines, light fills so overlaps stay legible
    groups <- unique(profile$group)
    p <- plot_ly(type = "scatterpolar")
    for (i in seq_along(groups)) {
      grp       <- groups[i]
      col       <- radar_palette[(i - 1) %% length(radar_palette) + 1]
      fill_col  <- adjustcolor(col, alpha.f = 0.15)
      df        <- filter(profile, group == grp)
      scores    <- df$score[match(flavors, df$flavor)]
      # +1 offset so zero-scored flavors don't collapse to the centre of the wheel.
      # Tick labels are remapped back to the original 0–4 scale in the layout below.
      p <- add_trace(
        p,
        r         = c(scores + 1, scores[1] + 1),
        theta     = c(flavors, flavors[1]),
        name      = grp,
        fill      = "toself",
        fillcolor = fill_col,
        line      = list(color = col, width = 2),
        opacity   = 1
      )
    }
    p |>
      layout(
        polar = list(
          radialaxis = list(
            range       = c(0, 5),
            tickvals    = c(1, 2, 3, 4, 5),
            ticktext    = c("0", "1", "2", "3", "4"),
            tickfont    = list(size = 10)
          )
        ),
        legend = list(orientation = "h"),
        margin = list(t = 40, r = 80, l = 80, b = 40)
      ) |>
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)
