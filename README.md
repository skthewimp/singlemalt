# Single Malt Whisky Recommender

A Shiny app that recommends Scottish single malt whiskies based on flavour similarity to your favourites.

**Live app:** [kart.shinyapps.io/singlemalt](https://kart.shinyapps.io/singlemalt)

## How it works

Select one or more whiskies you enjoy. The app computes [cosine similarity](https://en.wikipedia.org/wiki/Cosine_similarity) between the flavour profile of your selections and all other distilleries in the dataset, then ranks the rest by how closely they match.

Flavour profiles cover 12 characteristics scored 0–4:

> Body · Sweetness · Smoky · Medicinal · Tobacco · Honey · Spicy · Winey · Nutty · Malty · Fruity · Floral

## Features

- **Recommendations tab** — ranked table of similar distilleries with similarity score
- **Flavour Profiles tab** — radar chart comparing your selections' average profile against the top recommendation
- **About tab** — data source and methodology

## Running locally

```r
# Install dependencies (once)
install.packages(c("shiny", "bslib", "tidyverse", "DT", "plotly"))

# Run the app
shiny::runApp()
```

## Data

`whiskies.csv` / `whiskies.RData` — 86 Scottish distilleries with 12 flavour scores each.

**Source:** [Scotch Whisky dataset, University of Strathclyde](https://www.mathstat.strath.ac.uk/outreach/nessie/nessie_whisky.html)
