library(shiny)
library(qrcode)
library(officer)

set.seed(123)
patient_data <- data.frame(
  id = paste0("P", sprintf("%02d", 1:10)),
  sex = sample(c("Male", "Female"), 10, replace = TRUE),
  age_group = sample(c("18-30", "31-50", "51-70", "70+"), 10, replace = TRUE),
  biomarker = sample(c("Positive", "Negative", "Borderline"), 10, replace = TRUE),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  titlePanel("Demo: Plot with QR Code"),
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

server <- function(input, output, session) {
  
  create_plot <- reactive({
    if (input$x_var == input$fill_var) return(NULL)
    
    # Create count data
    plot_data <- aggregate(id ~ get(input$x_var) + get(input$fill_var), 
                           data = patient_data, 
                           FUN = length)
    names(plot_data) <- c("x_var", "fill_var", "n")
    
    # Get unique categories
    x_cats <- unique(plot_data$x_var)
    fill_cats <- unique(plot_data$fill_var)
    
    # Create color palette
    colors <- rainbow(length(fill_cats), alpha = 0.8)
    
    # Create the plot using base R graphics
    function() {
      # Set up the plot area
      par(mar = c(5, 4, 4, 2) + 0.1)
      
      # Calculate bar positions
      n_x <- length(x_cats)
      n_fill <- length(fill_cats)
      bar_width <- 0.8 / n_fill
      
      # Find max count for y-axis
      max_count <- max(plot_data$n)
      
      # Create empty plot
      plot(1, type = "n", xlim = c(0.5, n_x + 0.5), ylim = c(0, max_count * 1.1),
           xlab = tools::toTitleCase(gsub("_", " ", input$x_var)),
           ylab = "Count",
           main = paste("Bar Plot:", input$x_var, "by", input$fill_var),
           xaxt = "n", cex.main = 1.2)
      
      # Add x-axis labels
      axis(1, at = 1:n_x, labels = x_cats)
      
      # Draw bars
      for (i in 1:nrow(plot_data)) {
        x_pos <- which(x_cats == plot_data$x_var[i])
        fill_pos <- which(fill_cats == plot_data$fill_var[i])
        
        bar_x <- x_pos - 0.4 + (fill_pos - 1) * bar_width + bar_width/2
        
        rect(bar_x - bar_width/2, 0, bar_x + bar_width/2, plot_data$n[i],
             col = colors[fill_pos], border = "white", lwd = 1)
        
        # Add text labels
        text(bar_x, plot_data$n[i] + max_count * 0.02, 
             plot_data$n[i], pos = 3, font = 2, cex = 0.8)
      }
      
      # Add legend
      legend("topright", legend = fill_cats, fill = colors,
             title = tools::toTitleCase(gsub("_", " ", input$fill_var)),
             bg = "white", box.lwd = 1)
    }
  })
  
  output$stacked_plot <- renderPlot({
    plot_func <- create_plot()
    if (is.null(plot_func)) {
      plot(1, type = "n", xlab = "", ylab = "", main = "", axes = FALSE)
      text(1, 1, "Please select different variables", cex = 1.5, col = "gray50")
    } else {
      plot_func()
    }
  })
  
  output$download_plot <- downloadHandler(
    filename = function() paste0("result_with_qr_", Sys.Date(), ".pptx"),
    content = function(file) {
      plot_func <- create_plot()
      if (is.null(plot_func)) stop("No plot to save")
      
      png_f <- tempfile(fileext = ".png")
      png(png_f, width = 1200, height = 800, res = 300, bg = "white")
      plot_func()
      dev.off()
      
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
