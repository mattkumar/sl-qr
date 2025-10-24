library(shiny)
library(qrcode)
library(png)

set.seed(123)
patient_data <- data.frame(
  id = paste0("P", sprintf("%03d", 1:100)),
  weight = runif(100, min = 110, max = 210),
  height = runif(100, min = 150, max = 184),
  age = rnorm(100, mean = 45, sd = 15),
  income = rnorm(100, mean = 65000, sd = 25000),
  blood_pressure = rnorm(100, mean = 125, sd = 20),
  cholesterol = rnorm(100, mean = 180, sd = 35),
  bmi = runif(100, min = 18, max = 35),
  heart_rate = rnorm(100, mean = 72, sd = 12),
  glucose = rnorm(100, mean = 100, sd = 15),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  fluidRow(
    column(3),
    column(3,
           selectInput("x_var", "Choose X-Variable:", 
                       choices = c("weight", "height", "age", "income", "blood_pressure", "cholesterol", "bmi", "heart_rate", "glucose"), 
                       selected = "weight")
    ),
    column(3,
           selectInput("y_var", "Choose Y-Variable:", 
                       choices = c("weight", "height", "age", "income", "blood_pressure", "cholesterol", "bmi", "heart_rate", "glucose"), 
                       selected = "height")
    ),
    column(3)
  ),
  plotOutput("scatter_plot", height = "500px")
)

server <- function(input, output, session) {
  
  create_plot <- reactive({
    function() {
      # Create vibrant color gradient based on data density
      colors <- colorRampPalette(c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FFEAA7"))(100)
      point_colors <- colors[sample(1:100, 100, replace = TRUE)]
      
      # Main scatter plot with enhanced styling
      par(bg = "#f8f9fa", family = "sans")
      plot(patient_data[[input$x_var]], patient_data[[input$y_var]],
           xlab = toupper(gsub("_", " ", input$x_var)),
           ylab = toupper(gsub("_", " ", input$y_var)),
           main = paste(toupper(gsub("_", " ", input$x_var)), "vs", toupper(gsub("_", " ", input$y_var))),
           pch = 19, col = point_colors, cex = 1.5,
           col.main = "#2c3e50", col.lab = "#34495e", col.axis = "#7f8c8d",
           cex.main = 1.4, cex.lab = 1.2, cex.axis = 1.1,
           panel.first = {
             grid(col = "#ecf0f1", lty = 1, lwd = 0.8)
           })
      
      # Add subtle trend line
      lm_fit <- lm(patient_data[[input$y_var]] ~ patient_data[[input$x_var]])
      abline(lm_fit, col = "#e74c3c", lwd = 3, lty = 2)
      
      # Create QR code and add it as an inset
      qr_data <- paste("User matt\nDate", Sys.Date(), "\nFilters", input$x_var, input$y_var)
      qr_code_obj <- qrcode::qr_code(qr_data)
      
      # Get plot dimensions and device info for pixel calculations
      usr <- par("usr")
      plt <- par("plt")
      
      # QR code dimensions - fixed at 150x150 pixels
      qr_pixels <- 150
      
      # Get device dimensions in inches
      dev_width_in <- par("din")[1]
      dev_height_in <- par("din")[2]
      
      # Convert to plot coordinates based on device size
      plot_width <- usr[2] - usr[1]
      plot_height <- usr[4] - usr[3]
      
      # Calculate QR size in plot units (approximation)
      qr_width <- plot_width * (qr_pixels / (dev_width_in * 72)) * (plt[2] - plt[1])
      qr_height <- plot_height * (qr_pixels / (dev_height_in * 72)) * (plt[4] - plt[3])
      
      # Position with reduced padding from left (closer to Y axis)
      padding_factor <- 0.01
      qr_xleft <- usr[1] + plot_width * padding_factor
      qr_xright <- qr_xleft + qr_width
      qr_ybottom <- usr[3] + plot_height * 0.02
      qr_ytop <- qr_ybottom + qr_height
      
      # Convert QR code to raster and plot (no background/border)
      par(fig = c(
        (qr_xleft - usr[1]) / plot_width * (plt[2] - plt[1]) + plt[1],
        (qr_xright - usr[1]) / plot_width * (plt[2] - plt[1]) + plt[1],
        (qr_ybottom - usr[3]) / plot_height * (plt[4] - plt[3]) + plt[3],
        (qr_ytop - usr[3]) / plot_height * (plt[4] - plt[3]) + plt[3]
      ), new = TRUE, mar = c(0, 0, 0, 0))
      
      plot(qr_code_obj, axes = FALSE, xlab = "", ylab = "", mar = c(0, 0, 0, 0))
    }
  })
  
  output$scatter_plot <- renderPlot({
    plot_func <- create_plot()
    plot_func()
  })
}

shinyApp(ui = ui, server = server)
