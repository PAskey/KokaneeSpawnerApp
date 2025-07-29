library(shiny)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(viridis)
library(DT)

# ---- MAIN DATA ----

# Load main data objects
load("data/spawn_ests.rda")
spawn_ests = spawn_ests%>%
  dplyr::mutate(SITE = if_else(is.na(STREAM),QUADRANT,STREAM))%>%
  ungroup()    # loads 'spawn_ests' and adds SITE to plot shorespawners and streamspaners together
load("data/spawn_counts.rda")   # loads 'spawn_counts'
spawn_counts = spawn_counts%>%
  dplyr::mutate(SITE = if_else(is.na(STREAM),QUADRANT,STREAM))%>%
  ungroup() 

min_year <- min(spawn_ests$YEAR, na.rm = TRUE)
max_year <- max(spawn_ests$YEAR, na.rm = TRUE)

# ---- UI ----

ui <- fluidPage(
  titlePanel("Kokanee Spawner Abundance Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      # New ECOTYPE selector
      radioButtons("ecotype_select", "Ecotype:",
                   choices = c("All Ecotypes" = "all", "Stream" = "STREAM", "Shore" = "SHORE"),
                   selected = "all"),
      
      # Updated stream/site selection label
      radioButtons("stream_mode", "Site Selection Mode:",
                   choices = c("All Sites" = "all", "Select Sites" = "select"),
                   selected = "all"),
      
      conditionalPanel(
        condition = "input.stream_mode == 'select'",
        selectInput("stream_select", "Select Site(s):",
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
                   column(12, style = "overflow-x: auto;",
                          h4("Spawner Abundance Estimates"),
                          dataTableOutput("table_abundance")
                   )
                 ),
                 br(),
                 fluidRow(
                   column(12, style = "overflow-x: auto;",
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
  
  output$download_template <- downloadHandler(
    filename = function() { "spawner_count_template.xlsx" },
    content = function(file) {
      file.copy("data/spawner_count_template.xlsx", file)
    }
  )
  
  observe({
    filtered_choices <- spawn_ests
    if (input$ecotype_select != "all") {
      filtered_choices <- filtered_choices %>% filter(ECOTYPE == input$ecotype_select)
    }
    updateSelectInput(session, "stream_select",
                      choices = sort(unique(filtered_choices$SITE)),
                      selected = NULL)
  })
  
  selected_streams <- reactive({
    all_sites <- spawn_ests
    if (input$ecotype_select != "all") {
      all_sites <- all_sites %>% filter(ECOTYPE == input$ecotype_select)
    }
    if (input$stream_mode == "all") {
      unique(all_sites$SITE)
    } else {
      req(input$stream_select)
      input$stream_select
    }
  })
  
  filtered_abundance <- reactive({
    data <- spawn_ests %>%
      filter(YEAR >= input$year_range[1],
             YEAR <= input$year_range[2])
    
    if (input$ecotype_select != "all") {
      data <- data %>%
        filter(
          (SITE != "TREND" & ECOTYPE == input$ecotype_select) |
            (SITE == "TREND" & ECOTYPE == input$ecotype_select)
        )
    } else {
      data <- data %>%
        filter(SITE %in% c(selected_streams(), "TREND"))
    }
    
    data %>% filter(SITE %in% c(selected_streams(), "TREND"))
  })
  
  
  filtered_raw <- reactive({
    spawn_counts %>%
      filter(SITE %in% selected_streams(),
             YEAR >= input$year_range[1],
             YEAR <= input$year_range[2])
  })
  
  # ---- Plots ----
  output$plot_absolute <- renderPlot({
    full_data <- filtered_abundance()
    data <- full_data %>% filter(SITE != "TREND")
    
    # Split by ECOTYPE
    stream_data <- data %>% filter(ECOTYPE == "STREAM")
    shore_data  <- data %>% filter(ECOTYPE == "SHORE")
    
    # Get unique SITE values
    stream_sites <- unique(stream_data$SITE)
    shore_sites  <- unique(shore_data$SITE)
    
    # Desired order: all shore sites first (or last)
    ordered_sites <- c(shore_sites, stream_sites)  # <- SHORE on top
    # ordered_sites <- c(stream_sites, shore_sites)  # <- SHORE on bottom if you prefer
    
    # Reassign SITE factor to enforce order in legend
    data$SITE <- factor(data$SITE, levels = ordered_sites)
    stream_data$SITE <- factor(stream_data$SITE, levels = ordered_sites)
    shore_data$SITE  <- factor(shore_data$SITE,  levels = ordered_sites)
    
    # Generate separate viridis color palettes
    stream_colors <- setNames(viridis(length(stream_sites), option = "D"), stream_sites)
    shore_colors  <- setNames(viridis(length(shore_sites), option = "A"), shore_sites)
    custom_colors <- c(shore_colors, stream_colors)
    
    # ---- Plot 1: Absolute Abundance ----
    p1 <- ggplot(data, aes(x = YEAR, y = peak_est / 1000, fill = SITE)) +
      geom_bar(stat = "identity", position = "stack", na.rm = TRUE) +
      scale_fill_manual(values = custom_colors, name = "Spawner Site") +
      theme_minimal() +
      labs(title = "Absolute Spawner Abundance",
           x = "", y = "Kokanee spawners (thousands)") +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(),
        axis.ticks = element_line(),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13)
      )
    
    # ---- Plot 2: Relative Abundance ----
    p2 <- ggplot() +
      # Black TREND line
      geom_line(data = full_data %>% filter(SITE == "TREND"),
                aes(x = YEAR, y = peak_percent, group = ECOTYPE, linetype = ECOTYPE),
                colour = "black", lwd = 2, na.rm = TRUE) +
      
      # Stream lines and points
      geom_line(data = stream_data,
                aes(x = YEAR, y = peak_percent, group = SITE, color = SITE),
                size = 1, na.rm = TRUE) +
      geom_point(data = stream_data,
                 aes(x = YEAR, y = peak_percent, color = SITE),
                 shape = 21, fill = "white", size = 2, stroke = 1, na.rm = TRUE) +
      
      # Shore lines and points
      geom_line(data = shore_data,
                aes(x = YEAR, y = peak_percent, group = SITE, color = SITE),
                size = 1, na.rm = TRUE) +
      geom_point(data = shore_data,
                 aes(x = YEAR, y = peak_percent, color = SITE),
                 shape = 22, fill = "white", size = 2.5, stroke = 0.7, na.rm = TRUE) +
      
      scale_color_manual(values = custom_colors, name = "Spawner Site") +
      theme_minimal() +
      labs(title = "Relative Spawner Abundance (% of all year max)",
           subtitle = "Black lines = Average trends all sites",
           x = "Year", y = "Relative Kokanee Spawners (% of max)") +
      theme(
        panel.grid = element_blank(),
        axis.line = element_line(),
        axis.ticks = element_line(),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13)
      )
    
    # Combine vertically
    #ggarrange(p1, p2, ncol = 1)
    
    ggarrange(
      p1 + theme(legend.position = "none"),
      p2 +
        theme(
          legend.position = "right",
          legend.box = "vertical",
          legend.key.height = unit(0.5, "cm")
        ) +
        guides(
          color = guide_legend(ncol = 1),
          fill = guide_legend(ncol = 1)
        ),
      ncol = 1,
      common.legend = TRUE,
      legend = "right",
      align = "v"
    )
    
    
    
    
    
  })
  
  
  # ---- Data Tables ----
  output$table_abundance <- renderDataTable({
    datatable(
      filtered_abundance(),
      options = list(pageLength = 10, autowidth = TRUE),
      width = "100%",
      rownames = FALSE
    )
  })
  
  output$table_raw <- renderDataTable({
    datatable(
      filtered_raw(),
      options = list(pageLength = 10, autowidth = TRUE),
      width = "100%",
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
