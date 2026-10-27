# =============================================================================
# Discards & Stock Status — Shiny app, v0.1
# -----------------------------------------------------------------------------
# Reads output/discards_data.rds (made by build_discards_data()).
# MRIP recreational discards + as-known-at-the-time stock status.
#
# Run from the project folder:  shiny::runApp()
# =============================================================================

library(shiny)
library(bslib)
library(tidyverse)

# Data -------------------------------------------------------------------------

d <- readRDS("output/discards_data.rds")

# One display name per TSN (the most common MRIP name for it)
species_names <- d$mrip %>%
  filter(!is.na(tsn), !is.na(common_name)) %>%
  count(tsn, common_name) %>%
  group_by(tsn) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(tsn, species = str_to_title(common_name))

# Species-level rows only for v0.1; unidentified groups are left out
mrip <- d$mrip %>%
  filter(!is.na(tsn)) %>%
  left_join(species_names, by = "tsn") %>%
  mutate(
    year = as.integer(year),
    status_label = case_when(
      any_poor %in% TRUE  ~ "Poor condition",
      any_poor %in% FALSE ~ "Not in poor condition",
      TRUE                ~ "No assessment"
    )
  )

region_choices <- sort(unique(mrip$region_merged))
names(region_choices) <- str_to_title(str_replace_all(region_choices, "_", " "))

status_cols <- c(
  "Poor condition"        = "#c0392b",
  "Not in poor condition" = "#2e86c1",
  "No assessment"         = "grey65"
)

pct  <- function(x) ifelse(is.na(x), "—", scales::percent(x, accuracy = 0.1))
num2 <- function(x) ifelse(is.na(x), "—", formatC(x, format = "f", digits = 2))

# UI ---------------------------------------------------------------------------

ui <- page_sidebar(
  title = "Recreational discards and stock status",
  sidebar = sidebar(
    selectInput("region", "Region", choices = region_choices),
    selectizeInput("species", "Species", choices = NULL),
    sliderInput("years", "Years",
                min = min(mrip$year), max = max(mrip$year),
                value = range(mrip$year), step = 1, sep = "")
  ),
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Discard proportion",
      value = textOutput("vb_discard"),
      p(textOutput("vb_discard_sub"))
    ),
    value_box(
      title = "Stock status (last selected year)",
      value = textOutput("vb_status"),
      p(textOutput("vb_status_sub"))
    ),
    value_box(
      title = "F / Flimit (worst stock)",
      value = textOutput("vb_f"),
      p("Above 1 = overfishing")
    ),
    value_box(
      title = "B / Blimit (worst stock)",
      value = textOutput("vb_b"),
      p("Below 1 = overfished")
    )
  ),
  card(card_header("Discard proportion by year"), plotOutput("ts_plot")),
  card(card_header("Year by year"), tableOutput("detail"))
)

# Server -----------------------------------------------------------------------

server <- function(input, output, session) {

  # Species list depends on region
  observeEvent(input$region, {
    sp <- mrip %>%
      filter(region_merged == input$region) %>%
      distinct(tsn, species) %>%
      arrange(species)
    updateSelectizeInput(session, "species",
                         choices = setNames(sp$tsn, sp$species), server = TRUE)
  })

  sel <- reactive({
    req(input$region, input$species)
    mrip %>%
      filter(
        region_merged == input$region,
        tsn == as.numeric(input$species),
        year >= input$years[1], year <= input$years[2]
      ) %>%
      arrange(year)
  })

  latest <- reactive({
    s <- sel()
    req(nrow(s) > 0)
    slice_tail(s, n = 1)
  })

  # Value boxes
  output$vb_discard <- renderText({
    s <- sel(); req(nrow(s) > 0)
    pct(sum(s$released_b2) / sum(s$total_caught)) # catch-weighted over the range
  })

  output$vb_discard_sub <- renderText({
    s <- sel(); req(nrow(s) > 0)
    yrs <- if (min(s$year) == max(s$year)) min(s$year) else paste0(min(s$year), "–", max(s$year))
    paste0(scales::comma(sum(s$released_b2)), " of ",
           scales::comma(sum(s$total_caught)), " fish released, ", yrs)
  })

  output$vb_status <- renderText(latest()$status_label)

  output$vb_status_sub <- renderText({
    l <- latest()
    if (is.na(l$any_poor)) {
      paste0("No assessment available as of ", l$year)
    } else {
      paste0(l$n_poor, " of ", l$n_stocks, " assessed stock(s) in poor condition, as of ",
             l$year, " (", l$latest_assessment_year, " assessment)")
    }
  })

  output$vb_f <- renderText(num2(latest()$max_f_flimit))
  output$vb_b <- renderText(num2(latest()$min_b_blimit))

  # Time series
  output$ts_plot <- renderPlot({
    s <- sel(); req(nrow(s) > 0)
    ggplot(s, aes(year, discard_proportion)) +
      geom_line(colour = "grey60") +
      geom_point(aes(colour = status_label), size = 3.5) +
      scale_colour_manual(values = status_cols, limits = names(status_cols), name = NULL) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      scale_x_continuous(breaks = function(x) unique(floor(pretty(x)))) +
      labs(x = NULL, y = "Released (B2) / total caught") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })

  # Detail table
  output$detail <- renderTable({
    sel() %>%
      transmute(
        Year              = year,
        Harvested         = scales::comma(total_harvest_a_b1),
        Released          = scales::comma(released_b2),
        `Discard %`       = pct(discard_proportion),
        Status            = status_label,
        `Assessment used` = ifelse(is.na(latest_assessment_year), "—",
                                   as.character(latest_assessment_year))
      )
  })
}

shinyApp(ui, server)
