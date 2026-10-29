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

foss <- d$foss %>%
  mutate(
    year = as.integer(year),
    across(c(recpounds, compounds), ~ replace_na(.x, 0))
  )

species_lookup <- bind_rows(
  mrip %>% distinct(region_merged, tsn, species),
  foss %>% distinct(region_merged, tsn, species = str_to_title(name_merged))
) %>%
  distinct(region_merged, tsn, .keep_all = TRUE)

region_choices <- sort(unique(species_lookup$region_merged))
names(region_choices) <- str_to_title(str_replace_all(region_choices, "_", " "))

all_years <- range(c(mrip$year, foss$year))

status_cols <- c(
  "Poor condition"        = "orangered2",
  "Not in poor condition" = "steelblue",
  "No assessment"         = "grey65"
)

pct  <- function(x) ifelse(is.na(x), "—", scales::percent(x, accuracy = 0.1))
num2 <- function(x) ifelse(is.na(x), "—", formatC(x, format = "f", digits = 2))

# UI ---------------------------------------------------------------------------

ui <- page_sidebar(
  title = "Discards, landings and stock status",
  sidebar = sidebar(
    selectInput("region", "Region", choices = region_choices),
    selectizeInput("species", "Species", choices = NULL),
    sliderInput("years", "Years",
                min = all_years[1], max = all_years[2],
                value = all_years, step = 1, sep = "")
  ),
  
navset_underline(
  
  # Tab 1: Stock status + MRIP
  nav_panel(
  "Recreational discards",
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
    card(card_header("Harvest v. discards (number of fish)"), plotlyOutput("catch_plot")),
    card(card_header("Discard proportion and stock status"), plotlyOutput("ts_plot"))
  ),
  card(card_header("Year by year"), tableOutput("detail"))
),

# Tab 2: FOSS?
nav_panel(
  "Commercial vs. recreational",
  layout_columns(
    col_widths = c(8, 4),
    card(card_header("Landings by sector (lb)"), plotlyOutput("sector_plot")),
    card(card_header("Share of landings, selected years"), plotlyOutput("sector_donut"))
  )
)

)
)

# Server -----------------------------------------------------------------------

server <- function(input, output, session) {

  # Species list depends on region
  observeEvent(input$region, {
    sp <- species_lookup %>%
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
  
  sel_foss <- reactive({
    req(input$region, input$species)
    foss %>%
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
    s <- sel()
    validate(need(nrow(s) > 0, "No MRIP recreational catch for this species and region."))
    p <- ggplot(s, aes(factor(year), discard_proportion, group = 1)) +
      geom_line(colour = "grey60") +
      geom_point(aes(colour = status_label,
                     text = paste0(year, ": ", pct(discard_proportion), " released<br>", status_label)), 
                 size = 3.5) +
      scale_colour_manual(values = status_cols, name = NULL) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.02, yanchor = "bottom")) %>%
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
      scale_fill_manual(values = c(Harvested = "dodgerblue4", Released = "steelblue"), name = NULL) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.02, yanchor = "bottom")) %>%
      config(displayModeBar = FALSE)
      })
  
  # FOSS rec v. commercial landings
  output$sector_plot <- renderPlotly({
    f <- sel_foss()
    validate(need(nrow(f) > 0,
                  "No FOSS landings for this species and region in the selected years."))
    
    f_long <- f %>%
      select(year, Recreational = recpounds, Commercial = compounds) %>%
      pivot_longer(-year, names_to = "sector", values_to = "lb") %>%
      mutate(sector = factor(sector, levels = c("Recreational", "Commercial"))) %>%
      mutate(share = lb / sum(lb), .by = year)
    
    p <- ggplot(f_long, aes(factor(year), lb, fill = sector)) +
      geom_col(
        aes(text = paste0(sector, ": ", scales::comma(lb), " lb (", pct(share), ")")),
        width = 0.7
      ) +
      scale_fill_manual(values = c(Commercial = "darkgreen", Recreational = "cyan3"), name = NULL) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center",
                           y = 1.02, yanchor = "bottom")) %>%
      config(displayModeBar = FALSE)
  })
  
  # Rec v. commercial share over selected years
  output$sector_donut <- renderPlotly({
    f <- sel_foss()
    validate(need(nrow(f) > 0, "No FOSS landings for this selection."))
    
    totals <- tibble(
      sector = c("Recreational", "Commercial"),
      lb = c(sum(f$recpounds), sum(f$compounds))
    )
    validate(need(sum(totals$lb) > 0, "No landings reported."))
    
    plot_ly(
      totals,
      labels = ~sector, values = ~lb,
      type = "pie", hole = 0.55, sort = FALSE,
      marker = list(colors = c("darkgreen", "cyan3")),
      textinfo = "percent",
      hovertemplate = "%{label}: %{value:,.0f} lb<extra></extra>"
    ) %>%
      layout(
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.05),
        annotations = list(text = paste0(scales::comma(sum(totals$lb)), "<br>lb total"),
                           showarrow = FALSE, font = list(size = 16))
      ) %>%
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
