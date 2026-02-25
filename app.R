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

# ── Similarity helpers ───────────────────────────────────────────────────────

# Unit-normalise a numeric vector
unit_norm <- function(x) x / sqrt(sum(x^2))

# Compute cosine similarity between a reference profile and all distilleries
compute_similarity <- function(ref_distilleries) {
  ref_profile <- wh_long |>
    filter(Distillery %in% ref_distilleries) |>
    group_by(flavor) |>
    summarise(ref_z = mean(score_z), .groups = "drop") |>
    mutate(ref_z = unit_norm(ref_z))

  wh_long |>
    filter(!Distillery %in% ref_distilleries) |>
    group_by(Distillery) |>
    mutate(score_z = unit_norm(score_z)) |>
    inner_join(ref_profile, by = "flavor") |>
    summarise(similarity = sum(score_z * ref_z), .groups = "drop") |>
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

  navset_card_underline(
    nav_panel(
      "Recommendations",
      DTOutput("tbl_recommendations")
    ),
    nav_panel(
      "Flavour Profiles",
      uiOutput("ui_flavor_note"),
      plotlyOutput("plot_radar", height = "500px")
    ),
    nav_panel(
      "About",
      card(
        card_header("About this app"),
        p("This app recommends Scottish single malt whiskies based on flavour similarity.
           It uses cosine similarity on 12 flavour characteristics — Body, Sweetness, Smoky,
           Medicinal, Tobacco, Honey, Spicy, Winey, Nutty, Malty, Fruity, and Floral — scored
           0–4 for 86 distilleries."),
        p("Select your favourite malts in the sidebar and the app will find others with the
           closest flavour profile."),
        p(strong("Data source: "),
          a("Scotch Whisky dataset, University of Strathclyde",
            href = "https://www.mathstat.strath.ac.uk/outreach/nessie/nessie_whisky.html",
            target = "_blank"))
      )
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
      select(Rank, Distillery, Similarity) |>
      datatable(
        rownames  = FALSE,
        options   = list(pageLength = 10, dom = "tip"),
        class     = "hover stripe"
      )
  })

  # ── Flavour radar chart ────────────────────────────────────────────────────
  output$ui_flavor_note <- renderUI({
    if (length(input$ref_malts) == 0) {
      helpText("Select at least one whisky to see its flavour profile compared with
               the top recommendation.")
    }
  })

  output$plot_radar <- renderPlotly({
    req(length(input$ref_malts) > 0, nrow(reco()) > 0)

    top_reco <- reco()$Distillery[1]

    # Raw (un-normalised) flavor scores for plotting
    profile <- wh_long |>
      filter(Distillery %in% c(input$ref_malts, top_reco)) |>
      mutate(group = if_else(Distillery %in% input$ref_malts, "Your selection", top_reco)) |>
      group_by(group, flavor) |>
      summarise(score = mean(score), .groups = "drop")

    flavors <- sort(unique(profile$flavor))

    # Build one trace per group using a loop
    groups <- unique(profile$group)
    p <- plot_ly(type = "scatterpolar")
    for (grp in groups) {
      df     <- filter(profile, group == grp)
      scores <- df$score[match(flavors, df$flavor)]
      p <- add_trace(
        p,
        r       = c(scores, scores[1]),
        theta   = c(flavors, flavors[1]),
        name    = grp,
        fill    = "toself",
        opacity = 0.6
      )
    }
    p |>
      layout(
        polar  = list(radialaxis = list(visible = TRUE, range = c(0, 4))),
        legend = list(orientation = "h"),
        margin = list(t = 40)
      ) |>
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)
