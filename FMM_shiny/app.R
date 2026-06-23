#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(terra)
library(ggplot2)
library(lubridate)
library(tidyr)
library(tidyterra)
library(knitr)

# --- 1. Global Setup (Loads once when app starts) ---
stat_areas_global <- terra::vect("shapefiles/State_Stat_2004.shp")
stat_areas_global$STAT_AREA <- as.numeric(stat_areas_global$STAT_AREA)
available_areas <- sort(unique(stat_areas_global$STAT_AREA))

# --- 2. User Interface ---
ui <- fluidPage(
  theme = shinythemes::shinytheme("cosmo"),
  titlePanel("Proportional Groundfish Redistribution Model"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select one or more Statistical Areas to define the hypothetical closure."),
      selectInput("closure_selection", 
                  label = "Choose Closure Stat Areas:", 
                  choices = available_areas, 
                  selected = c(665730, 665700, 655730, 655700), 
                  multiple = TRUE),
      actionButton("run_sim", "Run Simulation & Bootstrap", class = "btn-primary"),
      hr(),
      helpText("Note: Bootstrap (1000 iterations) may take a few moments.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Comparison Plot", plotOutput("psc_plot", height = "500px")),
        tabPanel("Summary Table", tableOutput("summary_table")),
        tabPanel("Spatial Catch Map", plotOutput("map_plot", height = "600px"))
      )
    )
  )
)

# --- 3. Server Logic ---
server <- function(input, output, session) {
  
  # Reactive trigger: only runs when 'Run Simulation' is clicked
  model_data <- eventReactive(input$run_sim, {
    
    # Use selected stat areas from UI
    closure.stat.areas <- as.numeric(input$closure_selection)
    
    # 4. Closure Assignment logic
    closure <- subset(stat_areas_global, stat_areas_global$STAT_AREA %in% closure.stat.areas)
    closure2 <- terra::aggregate(closure)
    non_closure <- stat_areas_global[!(stat_areas_global$STAT_AREA %in% closure.stat.areas), ]
    
    # 5. Simulation Data (Biased for high PSC inside closure)
    set.seed(123)
    distances_to_closure <- terra::distance(non_closure, closure2)
    non_closure$dist_to_closure <- apply(distances_to_closure, 1, min)
    sim_areas <- rbind(non_closure[order(non_closure$dist_to_closure), ][1:20, ], closure)
    
    simulated_years <- c(2021, 2022, 2023)
    sim.list <- list()
    
    for(i in simulated_years){
      week_sequence <- seq.Date(from = as.Date(paste0(i, "-08-01")), by = "week", length.out = 5)
      year_weeks_list <- list()
      for(week_date in as.character(week_sequence)) {
        is_inside <- sim_areas$STAT_AREA %in% closure.stat.areas
        wk_dat <- data.frame(STAT_AREA = sim_areas$STAT_AREA, YEAR = i, WEEK_END_DATE = week_date,
                             CATCH_ACTIVITY_DATE = as.character(as.Date(week_date) - 2), PROCESSING_SECTOR = "X")
        wk_dat$GF_TOTAL_CATCH_MT <- rnbinom(nrow(sim_areas), mu = 50, size = 1)
        wk_dat$PSC_TOTAL_COUNT[is_inside] <- rnbinom(sum(is_inside), mu = 20, size = 0.8)
        wk_dat$PSC_TOTAL_COUNT[!is_inside] <- rnbinom(sum(!is_inside), mu = 10, size = 0.8)
        year_weeks_list[[week_date]] <- wk_dat
      }
      sim.list[[as.character(i)]] <- do.call(rbind, year_weeks_list)
    }
    catch_data <- do.call(rbind, sim.list)
    
    # 7-10. Processing & Redistribution Logic (Consolidated)
    all_psc_status_quo_annual <- catch_data %>% group_by(YEAR) %>% summarize(sum_psc_status_quo = sum(PSC_TOTAL_COUNT))
    
    # This block replicates your proportion & net change math...
    # (Abbreviated here for brevity, use your full Section 8-10 logic)
    # ...[Insert full redistribution logic from your script here]...
    
    # 11. Bootstrap (Reduced to 500 for Shiny speed, set back to 1000 for final)
    # ...[Insert full Bootstrap logic from your script here]...
    
    # Return a list of all objects needed for plots/tables
    list(
      plot_data = total.join.plot.annual_clean, # used for ggplot
      table_data = final.summary.table,         # used for table
      map_data = merge(stat_areas_global, catch_data, by = "STAT_AREA", all.x = FALSE),
      closure_outline = closure2
    )
  })
  
  # Render Plot
  output$psc_plot <- renderPlot({
    req(model_data())
    ggplot(model_data()$plot_data, aes(x = factor(YEAR), y = psc_amount, fill = psc_type)) + 
      geom_bar(stat = "identity", position = position_dodge(0.9)) + 
      geom_errorbar(aes(ymin = ymin, ymax = ymax), position = position_dodge(0.9), width = 0.25, na.rm = TRUE) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
      theme_minimal() + 
      labs(x = "Year", y = "Estimated Annual PSC (# Fish)", fill = "Scenario") +
      scale_fill_brewer(palette = "Paired", labels = c("Closure (with 95% CI)", "Status Quo"))
  })
  
  # Render Table
  output$summary_table <- renderTable({
    req(model_data())
    model_data()$table_data
  }, digits = 2)
  
  # Render Map
  output$map_plot <- renderPlot({
    req(model_data())
    ggplot() +
      tidyterra::geom_spatvector(data = model_data()$map_data, aes(fill = PSC_TOTAL_COUNT)) +
      tidyterra::geom_spatvector(data = model_data()$closure_outline, color = "red", fill = NA, lwd = 1) +
      facet_wrap(~WEEK_END_DATE) +
      scale_fill_distiller(palette = "YlOrRd", direction = 1) +
      theme_void()
  })
}

# --- 4. Launch App ---
shinyApp(ui, server)
