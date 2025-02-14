# app.R
library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Stochastic Gradient Descent Demonstration"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("learningRate", "Learning Rate:",
                  min = 0.001, max = 0.2, value = 0.05, step = 0.001),
      sliderInput("noise", "Noise Level:",
                  min = 0, max = 1, value = 0.1, step = 0.01),
      sliderInput("iterations", "Number of Iterations:",
                  min = 10, max = 200, value = 100, step = 10),
      numericInput("initX", "Initial x:", value = 8),
      numericInput("initY", "Initial y:", value = -5),
      actionButton("run", "Run SGD"),
      br(), br(),
      sliderInput("showIter", "Show Iteration:",
                  min = 1, max = 101, value = 1, step = 1)
    ),
    mainPanel(
      plotOutput("plot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value to store the SGD trajectory
  trajectory <- reactiveVal(NULL)
  
  # When the "Run SGD" button is clicked, simulate SGD updates
  observeEvent(input$run, {
    iters <- input$iterations
    lr <- input$learningRate
    noiseLevel <- input$noise
    
    # Initialize a data frame to store the trajectory
    traj <- data.frame(iter = 0:iters, x = NA, y = NA)
    traj$x[1] <- input$initX
    traj$y[1] <- input$initY
    
    # Cost function: f(x, y) = (x-2)^2 + (y-3)^2
    # Gradient: (2*(x-2), 2*(y-3))
    for (i in 1:iters) {
      x_curr <- traj$x[i]
      y_curr <- traj$y[i]
      
      # Compute the gradient at the current point
      grad_x <- 2 * (x_curr - 2)
      grad_y <- 2 * (y_curr - 3)
      
      # Add stochastic noise to the gradient
      noise_x <- rnorm(1, mean = 0, sd = noiseLevel)
      noise_y <- rnorm(1, mean = 0, sd = noiseLevel)
      
      # SGD update rule
      new_x <- x_curr - lr * (grad_x + noise_x)
      new_y <- y_curr - lr * (grad_y + noise_y)
      
      traj$x[i + 1] <- new_x
      traj$y[i + 1] <- new_y
    }
    
    trajectory(traj)
    
    # Update the "Show Iteration" slider maximum based on iterations run
    updateSliderInput(session, "showIter", max = iters + 1, value = 1)
  })
  
  # Render the plot: show a contour plot of the cost function and overlay the SGD path
  output$plot <- renderPlot({
    traj <- trajectory()
    if (is.null(traj)) return(NULL)
    
    # Use the slider to control how many iterations are displayed
    showIter <- input$showIter
    traj_sub <- traj[1:showIter, ]
    
    # Create a grid for the contour plot
    x_range <- seq(min(traj$x, na.rm = TRUE) - 1, max(traj$x, na.rm = TRUE) + 1, length.out = 100)
    y_range <- seq(min(traj$y, na.rm = TRUE) - 1, max(traj$y, na.rm = TRUE) + 1, length.out = 100)
    grid <- expand.grid(x = x_range, y = y_range)
    grid$z <- (grid$x - 2)^2 + (grid$y - 3)^2
    
    ggplot() +
      geom_contour(data = grid, aes(x = x, y = y, z = z), color = "grey80") +
      geom_path(data = traj_sub, aes(x = x, y = y), color = "blue", size = 1) +
      geom_point(data = traj_sub, aes(x = x, y = y), color = "red", size = 2) +
      # Mark the true minimum with a green star
      geom_point(aes(x = 2, y = 3), color = "green", size = 4, shape = 8) +
      labs(title = "Stochastic Gradient Descent Path",
           x = "x", y = "y",
           caption = "Green star: True Minimum (2, 3)") +
      theme_minimal()
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)