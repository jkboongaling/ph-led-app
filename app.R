library(shiny)

# Load functions and configuration
source("code/loader.R")

# Load data
mx_data  <- readr::read_csv("data/mx_data.csv", show_col_types = FALSE)
cod_data <- readr::read_csv("data/cod_data.csv", show_col_types = FALSE)

# Years available in both datasets
years <- sort(intersect(mx_data$year, cod_data$year))

# ------------------------------------------------------------------------------
# UI

ui <- bslib::page_fluid(
  
  titlePanel("Philippine Life Expectancy Decomposition Explorer"),
  
  tags$head(tags$style(
    HTML(
    "
    .sidebar .btn {padding: 0.5rem 0.75rem; font-size: 0.875rem;}
    .shiny-table {width: 100%; table-layout: fixed; font-size: 0.7rem;} 
    .shiny-table {border-bottom: 1px solid #000; margin-bottom: 0} 
    .shiny-table th {width: 7.14%; padding: 2px; text-align: center !important;}
    .shiny-table td {width: 7.14%; padding: 2px; text-align: right;}
    .shiny-table td:first-child {text-align: center;}
    .shiny-table tr:last-child {font-weight: bold;}
    .table-note {font-size: 0.7rem; margin-top: -10px;}
    "
    ))), 
  
  bslib::layout_sidebar(sidebar = bslib::sidebar(
    
    radioButtons("type", "Differentials",
      choices = c("Time" = "time", "Sex" = "sex"),
      selected = "time",
      inline = TRUE), 
    
    selectInput("yr1", "Year 1", choices = years[-length(years)], 
                selected = min(years)), 
    
    uiOutput("yr2_ui"),
    
    sliderInput("y_range", "Y Range", ticks = FALSE,
                min = -2, max = 2, value = c(-0.6, 1.1), step = 0.1),
    
    actionButton("run", "Run"),
    
    uiOutput("y_warning")),

    h5("Summary plot"),
    plotOutput("main_plot", height = "600px"),
    
    h5("Age-by-cause decomposition matrix"),
    uiOutput("matrix_tabs"),
    div(class = "table-note",
        HTML(
            "<strong>Note:</strong><br>
            (1) ∆x = age-specific contributions<br>
            (2) 1 = infections; 2 = neoplasms; 3 = diabetes; 
                4 = IHD; 5 = stroke; 6 = other CVDs; 7 = respiratory; 
                8 = suicide; 9 = transport; 10 = other external; 
                11 = COVID-19; 12 = others")))
    )

# ------------------------------------------------------------------------------
# Server

server <- function(input, output, session) {
  
  # Year 2 depends on Year 1
  output$yr2_ui <- renderUI({
    
    end_years <- years[years > input$yr1]
    selectInput("yr2", "Year 2", choices = end_years, selected = max(end_years))})
  
  # Default results
  result <- reactiveVal(
    
    list(out1 = decompose_time(1993, 2023, "f", mx_data, cod_data),
         out2 = decompose_time(1993, 2023, "m", mx_data, cod_data)))
  
  # Run decomposition
  observeEvent(input$run, {
    
    req(input$yr2)
    
    if (input$type == "time") {
    result(list(
        out1 = decompose_time(input$yr1, input$yr2, "f", mx_data, cod_data),
        out2 = decompose_time(input$yr1, input$yr2, "m", mx_data, cod_data)))
      
    } else {
    result(list(
        out1 = decompose_sex(input$yr1, mx_data, cod_data),
        out2 = decompose_sex(input$yr2, mx_data, cod_data)))}})

  # Decomposition plots
  output$main_plot <- renderPlot({

    out1 <- result()$out1
    out2 <- result()$out2

    p1 <- out_fig(out1, input$y_range[1], input$y_range[2])
    p2 <- out_fig(out2, input$y_range[1], input$y_range[2])

    main <- patchwork::wrap_plots(p1, p2, ncol = 2, 
                                  guides = "collect", axes = "collect")

    main
  })
  
  # Slider warnings
  output$y_warning <- renderUI({
    
    out1 <- result()$out1
    out2 <- result()$out2
    
    pos <- c(
      rowSums(out1$cmat_grp[, 3:14] * (out1$cmat_grp[, 3:14] > 0)),
      rowSums(out2$cmat_grp[, 3:14] * (out2$cmat_grp[, 3:14] > 0))
    )
    
    neg <- c(
      rowSums(out1$cmat_grp[, 3:14] * (out1$cmat_grp[, 3:14] < 0)),
      rowSums(out2$cmat_grp[, 3:14] * (out2$cmat_grp[, 3:14] < 0))
    )
    
    if (max(pos, na.rm = TRUE) > input$y_range[2] ||
        min(neg, na.rm = TRUE) < input$y_range[1]) 
      {
      div(style = "color: #b94a48; font-size: 0.75rem;",
          "Warning:", br(),
          "Some values are outside the plot range. Adjust y-axis limits.")}})
  
  # Matrix tabs
  output$matrix_tabs <- renderUI({
    
    out1 <- result()$out1
    out2 <- result()$out2
    
    bslib::navset_tab(bslib::nav_panel(out1$lbl, tableOutput("cmat1")),
                      bslib::nav_panel(out2$lbl, tableOutput("cmat2")))})
  
  # Tables
  make_matrix <- function(x) {
    colnames(x) <- c("x", "∆x", paste0("∆x<sub>", 1:12, "</sub>"))
    x <- rbind(x, c(NA, colSums(x[, -1, drop = FALSE])))
    x <- as.data.frame(x)
    x$x <- as.character(x$x)
    x[nrow(x), 1] <- "Sum"
    x
  }
  
  output$cmat1 <- renderTable({
    make_matrix(result()$out1$cmat_grp)}, digits = 4, 
    sanitize.text.function = identity)
  
  output$cmat2 <- renderTable({
    make_matrix(result()$out2$cmat_grp)}, digits = 4, 
    sanitize.text.function = identity)
  
}

# ------------------------------------------------------------------------------
# Run app

shinyApp(ui, server)

