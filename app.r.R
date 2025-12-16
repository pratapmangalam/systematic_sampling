library(shiny)

ui <- fluidPage(
  titlePanel("Sampling-2: Systematic Sampling Design (Population Mean)"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("G", "Number of Subgroups", 4, min = 1),
      
      textInput("Ng", "Population sizes (comma separated)",
                "120,100,80,60"),
      
      textInput("var", "Subgroup variances (comma separated)",
                "25,16,36,20"),
      
      textInput("cost", "Cost per unit (comma separated)",
                "50,40,60,55"),
      
      textInput("time", "Time per unit (comma separated)",
                "0.5,0.4,0.6,0.5"),
      
      numericInput("d", "Allowable Error (d)", 2),
      numericInput("b", "Allowable Bias (b)", 0.3),
      numericInput("z", "Z value", 1.96),
      
      actionButton("run", "Run Sampling Design")
    ),
    
    mainPanel(
      h4("Required Sample Size"),
      verbatimTextOutput("n"),
      
      h4("Subgroup Allocation"),
      tableOutput("allocation"),
      
      h4("Systematic Sampling Design"),
      plotOutput("designPlot", height = "400px")
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$run, {
    
    N_g <- as.numeric(strsplit(input$Ng, ",")[[1]])
    sigma2_g <- as.numeric(strsplit(input$var, ",")[[1]])
    C_g <- as.numeric(strsplit(input$cost, ",")[[1]])
    t_g <- as.numeric(strsplit(input$time, ",")[[1]])
    
    N <- sum(N_g)
    W_g <- N_g / N
    
    # Sample size formula
    n <- ceiling((input$z^2 * sum(W_g^2 * sigma2_g)) /
                   (input$d^2 - input$b^2))
    
    sigma_g <- sqrt(sigma2_g)
    
    # Optimal allocation
    weights <- (N_g * sigma_g) / sqrt(C_g + t_g)
    n_g <- round(n * weights / sum(weights))
    n_g[1] <- n - sum(n_g[-1])
    
    output$n <- renderText(n)
    
    output$allocation <- renderTable({
      data.frame(
        Subgroup = 1:length(N_g),
        Population = N_g,
        Sample_Size = n_g,
        Cost = C_g,
        Time = t_g
      )
    })
    
    output$designPlot <- renderPlot({
      par(mfrow = c(2, 2))
      
      for (g in 1:length(N_g)) {
        k <- floor(N_g[g] / n_g[g])
        r <- sample(1:k, 1)
        sample_units <- seq(r, by = k, length.out = n_g[g])
        
        plot(1:N_g[g], rep(1, N_g[g]),
             pch = 16, col = "grey",
             main = paste("Subgroup", g),
             xlab = "Unit Index", ylab = "",
             yaxt = "n")
        
        points(sample_units, rep(1, length(sample_units)),
               col = "red", pch = 16, cex = 1.3)
        
        legend("topright",
               legend = c("Population", "Sample"),
               col = c("grey", "red"),
               pch = 16)
      }
    })
  })
}

shinyApp(ui = ui, server = server)
