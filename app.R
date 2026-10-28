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
library(plotly)
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
  layout_columns(
    col_widths = c(6, 6),
    card(card_header("Harvest v. discards"), plotlyOutput("catch_plot")),
    card(card_header("Discard proportion and stock status"), plotlyOutput("ts_plot"))
  ),
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
  output$ts_plot <- renderPlotly({
    s <- sel(); req(nrow(s) > 0)
    p <- ggplot(s, aes(factor(year), discard_proportion, group = 1)) +
      geom_line(colour = "grey60") +
      geom_point(aes(colour = status_label,
                     text = paste0(year, ": ", pct(discard_proportion), " released<br>", status_label)), 
                 size = 3.5) +
      scale_colour_manual(values = status_cols, name = NULL) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      labs(x = NULL, y = "Discard proportion") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15)) %>%
      config(displayModebar = FALSE)
  })
  
  # Harvest v. discards, stacked bar charts
  output$catch_plot <- renderPlotly({
    s <- sel(); req(nrow(s) > 0)
    
    # one row per year x fate, for stacked bar charts
    s_long <- s %>%  
      select(year, Harvested = total_harvest_a_b1, Released = released_b2) %>%
      pivot_longer(-year, names_to = "fate", values_to = "fish") %>%
      mutate(fate = factor(fate, levels = c("Released", "Harvested"))) %>%
      mutate(share = fish / sum(fish), .by = year)
    
    p <- ggplot(s_long, aes(factor(year), fish, fill = fate)) +
      geom_col(
        aes(text = paste0(fate, ": ", scales::comma(fish), " fish (", pct(share), ")")),
        width = 0.7
      ) +
      geom_text(
        aes(label = if_else(share >= 0.08, pct(share), "")),
        position = position_stack(vjust = 0.5),
        color = "white", size = 4.5
      ) +
      scale_fill_manual(values = c(Harvested = "dodgerblue4", Released = "steelblue"), name = NULL) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, y = "Number of fish") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15)) %>%
      config(displayModeBar = FALSE)
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
