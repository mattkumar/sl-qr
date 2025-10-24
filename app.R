library(shiny)
library(qrcode)
library(officer)

set.seed(123)
patient_data <- data.frame(
  id = paste0("P", sprintf("%02d", 1:10)),
  weight = runif(10, min = 110, max = 210),
  height = runif(10, min = 150, max = 184),
  age = rnorm(10, mean = 30, sd = 10),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  titlePanel("Demo: Plot with QR Code"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "X-axis Variable:", 
                  choices = c("weight", "height", "age"), 
                  selected = "weight"),
      selectInput("y_var", "Y-axis Variable:", 
                  choices = c("weight", "height", "age"), 
                  selected = "height"),
      downloadButton("download_plot", "Download Plot")
    ),
    mainPanel(
      plotOutput("scatter_plot", height = "500px")
    )
  )
)

server <- function(input, output, session) {
  
  create_plot <- reactive({
    function() {
      plot(patient_data[[input$x_var]], patient_data[[input$y_var]],
           xlab = input$x_var,
           ylab = input$y_var,
           main = paste("Plot:", input$x_var, "vs", input$y_var),
           pch = 16, col = "blue", cex = 1.2)
    }
  })
  
  output$scatter_plot <- renderPlot({
    plot_func <- create_plot()
    plot_func()
  })
  
  output$download_plot <- downloadHandler(
    filename = function() paste0("result_with_qr_", Sys.Date(), ".pptx"),
    content = function(file) {
      plot_func <- create_plot()
      
      png_f <- tempfile(fileext = ".png")
      png(png_f, width = 1200, height = 800, res = 300, bg = "white")
      plot_func()
      dev.off()
      
      qr_data <- paste("User matt\nDate", Sys.Date(), "\nFilters", input$x_var, input$y_var)
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

