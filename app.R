library(shiny)
library(ggplot2)
library(qrcode)
library(officer)

# some data
set.seed(123)
patient_data <- data.frame(
  id = paste0("P", sprintf("%02d", 1:10)),
  sex = sample(c("Male", "Female"), 10, replace = TRUE),
  age_group = sample(c("18-30", "31-50", "51-70", "70+"), 10, replace = TRUE),
  biomarker = sample(c("Positive", "Negative", "Borderline"), 10, replace = TRUE),
  stringsAsFactors = FALSE
)

# ui
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "X-axis Variable:", 
                  choices = c("sex", "age_group", "biomarker"), 
                  selected = "sex"),
      selectInput("fill_var", "Fill Variable:", 
                  choices = c("sex", "age_group", "biomarker"), 
                  selected = "biomarker"),
      downloadButton("download_plot", "Download Plot")
    ),
    mainPanel(
      plotOutput("stacked_plot", height = "500px")
    )
  )
)

# server
server <- function(input, output, session) {
  
  create_plot <- reactive({
    if (input$x_var == input$fill_var) return(NULL)
    
    plot_data <- aggregate(id ~ get(input$x_var) + get(input$fill_var), 
                           data = patient_data, 
                           FUN = length)
    names(plot_data) <- c("x_var", "fill_var", "n")
    
    ggplot(plot_data, aes(x = x_var, y = n, fill = fill_var)) +
      geom_col(position = "dodge", alpha = 0.8, color = "white", size = 0.5) +
      geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5, 
                fontface = "bold", size = 4) +
      labs(title = paste("Bar Plot:", input$x_var, "by", input$fill_var),
           x = tools::toTitleCase(gsub("_", " ", input$x_var)),
           y = "Count",
           fill = tools::toTitleCase(gsub("_", " ", input$fill_var))) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            legend.position = "bottom") +
      scale_fill_viridis_d(option = "plasma", begin = 0.2, end = 0.8)
  })
  
  output$stacked_plot <- renderPlot({
    plot_obj <- create_plot()
    if (is.null(plot_obj)) {
      ggplot() + annotate("text", x = 0.5, y = 0.5, 
                          label = "Please select different variables", 
                          size = 6, color = "gray50") + theme_void() + xlim(0, 1) + ylim(0, 1)
    } else plot_obj
  })
  
  output$download_plot <- downloadHandler(
    filename = function() paste0("result_with_qr_", Sys.Date(), ".pptx"),
    content = function(file) {
      plot_obj <- create_plot()
      if (is.null(plot_obj)) stop("No plot to save")
      
      png_f <- tempfile(fileext = ".png")
      ggsave(png_f, plot_obj, width = 12, height = 8, dpi = 300, bg = "white")
      
      qr_data <- paste("User matt\nDate", Sys.Date(), "\nFilters", input$x_var, input$fill_var)
      qr_png_f <- tempfile(fileext = ".png")
      png(qr_png_f, width = 200, height = 200)
      plot(qrcode::qr_code(qr_data))
      dev.off()
      
      officer::read_pptx() %>%
        officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
        officer::ph_with(value = officer::external_img(png_f, width = 10, height = 6.67),
                         location = officer::ph_location_type(type = "body")) %>%
        officer::ph_with(value = officer::external_img(qr_png_f, width = 0.5, height = 0.5),
                         location = officer::ph_location(left = 8.5, top = 0.5, width = 0.75, height = 0.75)) %>%
        print(target = file)
    }
  )
}

shinyApp(ui = ui, server = server)