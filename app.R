library(shiny)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(viridis)
library(DT)

# ---- MAIN DATA ----

setwd("~/FFSBC work docs/Git_projects/KokaneeSpawnerApp")

# Load main data objects
load("data/spawn_ests.rda")     # loads 'spawn_ests'
load("data/spawn_counts.rda")   # loads 'spawn_counts'

min_year <- min(spawn_ests$YEAR, na.rm = TRUE)
max_year <- max(spawn_ests$YEAR, na.rm = TRUE)

# ---- UI ----

ui <- fluidPage(
  titlePanel("Okanagan Lake Spawner Abundance Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("stream_mode", "Stream Selection Mode:",
                   choices = c("All Streams" = "all", "Select Streams" = "select"),
                   selected = "all"),
      
      conditionalPanel(
        condition = "input.stream_mode == 'select'",
        selectInput("stream_select", "Select Stream(s):",
                    choices = NULL, multiple = TRUE)
      ),
      
      sliderInput("year_range", "Select Year Range:",
                  min = min_year, max = max_year,
                  value = c(min_year, max_year), sep = ""),
      
      downloadButton("download_csv_abundance", "Download selected spawner abundance data"),
      downloadButton("download_csv_raw", "Download selected raw count data"),
      br(), br(),
      h5("Need an input template?"),
      downloadLink("download_template", "Download Spawner Counts Template")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plots",
                 plotOutput("plot_absolute", height = "800px")
        ),
        tabPanel("Data",
                 fluidRow(
                   column(6,
                          h4("Spawner Abundance Estimates"),
                          dataTableOutput("table_abundance")
                   ),
                   column(6,
                          h4("Raw Count Data"),
                          dataTableOutput("table_raw")
                   )
                 )
        )
      )
    )
  )
)

# ---- SERVER ----

server <- function(input, output, session) {
  
  # Provide Excel template download
  output$download_template <- downloadHandler(
    filename = function() {
      "spawner_count_template.xlsx"
    },
    content = function(file) {
      file.copy("data/spawner_count_template.xlsx", file)
    }
  )
  
  # Dynamically update stream list
  observe({
    updateSelectInput(session, "stream_select",
                      choices = unique(spawn_ests$STREAM),
                      selected = NULL)
  })
  
  # Determine selected streams
  selected_streams <- reactive({
    if (input$stream_mode == "all") {
      unique(spawn_ests$STREAM)
    } else {
      req(input$stream_select)
      input$stream_select
    }
  })
  
  # Filtered abundance data
  filtered_abundance <- reactive({
    spawn_ests %>%
      filter(STREAM %in% c(selected_streams(), "TREND"),
             YEAR >= input$year_range[1],
             YEAR <= input$year_range[2])
  })
  
  # Filtered raw count data
  filtered_raw <- reactive({
    spawn_counts %>%
      filter(STREAM %in% selected_streams(),
             YEAR >= input$year_range[1],
             YEAR <= input$year_range[2])
  })
  
  # ---- Plots ----
  output$plot_absolute <- renderPlot({
    data <- filtered_abundance() %>% filter(STREAM != "TREND")
    
    # Stacked bar for absolute abundance
    p1 <- ggplot(data, aes(x = YEAR, y = peak_est, fill = STREAM)) +
      geom_bar(stat = "identity", position = "stack", na.rm = TRUE) +
      scale_fill_viridis_d() +
      theme_minimal() +
      labs(title = "Absolute Spawner Abundance (Stacked by Stream)",
           x = "Year", y = "Peak Estimate") +
      theme(panel.grid = element_blank(),
            axis.line = element_line(),
            axis.ticks = element_line())
    
    # Relative abundance line plot
    p2 <- ggplot() +
      geom_line(data = filtered_abundance()[filtered_abundance()$STREAM == "TREND",],
                aes(x = YEAR, y = peak_percent),
                colour = "black", lwd = 2, na.rm = TRUE) +
      geom_line(data = data, aes(x = YEAR, y = peak_percent, group = STREAM, color = STREAM), na.rm = TRUE) +
      geom_point(data = data, aes(x = YEAR, y = peak_percent, color = STREAM),
                 shape = 21, fill = "white", size = 2, stroke = 1) +
      scale_color_viridis_d() +
      theme_minimal() +
      labs(title = "Spawner Abundance (% of Max Peak by Stream)",
           subtitle = "Black line = Average across all streams",
           x = "Year", y = "Relative Peak (%)") +
      theme(panel.grid = element_blank(),
            axis.line = element_line(),
            axis.ticks = element_line())
    
    ggarrange(p1, p2, ncol = 1)
  })
  
  # ---- Data Tables ----
  output$table_abundance <- renderDataTable({
    datatable(
      filtered_abundance(),
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  output$table_raw <- renderDataTable({
    datatable(
      filtered_raw(),
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  # ---- Downloads ----
  output$download_csv_abundance <- downloadHandler(
    filename = function() {
      paste0("Spawner_Abundance_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_abundance(), file, row.names = FALSE)
    }
  )
  
  output$download_csv_raw <- downloadHandler(
    filename = function() {
      paste0("Raw_Spawner_Counts_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_raw(), file, row.names = FALSE)
    }
  )
}

# ---- Run the App ----
shinyApp(ui, server)
