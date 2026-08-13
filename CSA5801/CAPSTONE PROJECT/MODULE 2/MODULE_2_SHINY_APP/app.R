# ============================================================
# GATEKEEPERX - MODULE 2 SHINY APP
# QUEUEING & INTELLIGENT SECURITY-GATE MODELING
# ============================================================

library(shiny)

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  titlePanel(
    "GatekeeperX - Module 2: Queueing & Intelligent Modeling"
  ),
  
  sidebarLayout(
    
    # ========================================================
    # SIDEBAR
    # ========================================================
    
    sidebarPanel(
      
      h4("Input Dataset"),
      
      fileInput(
        inputId = "file",
        label = "Upload Module 1 CSV:",
        accept = ".csv"
      ),
      
      br(),
      
      actionButton(
        inputId = "load_data",
        label = "Load & Model Dataset",
        class = "btn-primary"
      ),
      
      br(),
      br(),
      
      h4("Queueing Model"),
      
      helpText(
        "The app calculates arrival rate, service rate, ",
        "utilization and GatekeeperX priority."
      ),
      
      hr(),
      
      h4("GatekeeperX Priority Weights"),
      
      numericInput(
        inputId = "w_severity",
        label = "Severity Weight:",
        value = 0.45,
        min = 0,
        max = 1,
        step = 0.05
      ),
      
      numericInput(
        inputId = "w_risk",
        label = "Risk Weight:",
        value = 0.25,
        min = 0,
        max = 1,
        step = 0.05
      ),
      
      numericInput(
        inputId = "w_vulnerability",
        label = "Vulnerability Weight:",
        value = 0.20,
        min = 0,
        max = 1,
        step = 0.05
      ),
      
      numericInput(
        inputId = "w_waiting",
        label = "Waiting Weight:",
        value = 0.10,
        min = 0,
        max = 1,
        step = 0.05
      ),
      
      br(),
      
      actionButton(
        inputId = "calculate",
        label = "Calculate Model",
        class = "btn-success"
      ),
      
      hr(),
      
      downloadButton(
        outputId = "download_model",
        label = "Download Modeled CSV"
      )
    ),
    
    # ========================================================
    # MAIN PANEL
    # ========================================================
    
    mainPanel(
      
      tabsetPanel(
        
        # ====================================================
        # TAB 1 - DASHBOARD
        # ====================================================
        
        tabPanel(
          "Dashboard",
          
          br(),
          
          fluidRow(
            
            column(
              3,
              wellPanel(
                h4("Jobs"),
                h3(textOutput("total_jobs"))
              )
            ),
            
            column(
              3,
              wellPanel(
                h4("Arrival Rate λ"),
                h3(textOutput("lambda"))
              )
            ),
            
            column(
              3,
              wellPanel(
                h4("Service Rate μ"),
                h3(textOutput("mu"))
              )
            ),
            
            column(
              3,
              wellPanel(
                h4("Utilization ρ"),
                h3(textOutput("rho"))
              )
            )
          ),
          
          br(),
          
          fluidRow(
            
            column(
              6,
              wellPanel(
                h4("Queue Status"),
                h3(textOutput("queue_status"))
              )
            ),
            
            column(
              6,
              wellPanel(
                h4("Average Priority Score"),
                h3(textOutput("avg_priority"))
              )
            )
          ),
          
          br(),
          
          h3("Queueing Metrics"),
          
          tableOutput("queue_metrics"),
          
          br(),
          
          h3("GatekeeperX Formula"),
          
          p(
            "GPS = Severity × 0.45 + Risk × 0.25 + ",
            "Vulnerability × 0.20 + Waiting × 0.10"
          )
        ),
        
        
        # ====================================================
        # TAB 2 - MODELED DATA
        # ====================================================
        
        tabPanel(
          "Modeled Dataset",
          
          br(),
          
          h3(
            "GatekeeperX Modeled Security Jobs"
          ),
          
          tableOutput(
            "modeled_data"
          )
        ),
        
        
        # ====================================================
        # TAB 3 - PRIORITY SCORE
        # ====================================================
        
        tabPanel(
          "Priority Score",
          
          br(),
          
          h3(
            "GatekeeperX Priority Score Distribution"
          ),
          
          plotOutput(
            "priority_plot",
            height = "600px"
          )
        ),
        
        
        # ====================================================
        # TAB 4 - PRIORITY CLASS
        # ====================================================
        
        tabPanel(
          "Priority Classes",
          
          br(),
          
          h3(
            "Security Job Priority Classes"
          ),
          
          plotOutput(
            "priority_class_plot",
            height = "600px"
          )
        ),
        
        
        # ====================================================
        # TAB 5 - RISK VS PRIORITY
        # ====================================================
        
        tabPanel(
          "Risk vs Priority",
          
          br(),
          
          h3(
            "Risk Score vs GatekeeperX Priority Score"
          ),
          
          plotOutput(
            "risk_priority_plot",
            height = "600px"
          )
        ),
        
        
        # ====================================================
        # TAB 6 - SECURITY DECISION
        # ====================================================
        
        tabPanel(
          "Security Decisions",
          
          br(),
          
          h3(
            "GatekeeperX Security Gate Decisions"
          ),
          
          plotOutput(
            "decision_plot",
            height = "600px"
          ),
          
          br(),
          
          tableOutput(
            "decision_table"
          )
        ),
        
        
        # ====================================================
        # TAB 7 - QUEUEING MODEL
        # ====================================================
        
        tabPanel(
          "Queueing Model",
          
          br(),
          
          h2(
            "M/M/1 Queueing Model"
          ),
          
          p(
            "The basic queueing model uses the arrival rate ",
            "λ and service rate μ."
          ),
          
          h4("Model Parameters"),
          
          tableOutput(
            "model_parameters"
          ),
          
          br(),
          
          h4("Queueing Equations"),
          
          tags$ul(
            
            tags$li(
              HTML(
                "<b>Arrival Rate:</b> λ = Number of Jobs / Total Time"
              )
            ),
            
            tags$li(
              HTML(
                "<b>Service Rate:</b> μ = 1 / Average Service Time"
              )
            ),
            
            tags$li(
              HTML(
                "<b>Utilization:</b> ρ = λ / μ"
              )
            ),
            
            tags$li(
              HTML(
                "<b>Average Jobs in System:</b> L = ρ / (1 - ρ)"
              )
            ),
            
            tags$li(
              HTML(
                "<b>Average Queue Length:</b> Lq = ρ² / (1 - ρ)"
              )
            ),
            
            tags$li(
              HTML(
                "<b>Average Time in System:</b> W = 1 / (μ - λ)"
              )
            ),
            
            tags$li(
              HTML(
                "<b>Average Waiting Time:</b> Wq = λ / [μ(μ - λ)]"
              )
            )
          )
        ),
        
        
        # ====================================================
        # TAB 8 - ARCHITECTURE
        # ====================================================
        
        tabPanel(
          "Architecture",
          
          br(),
          
          h2(
            "GatekeeperX Module 2 Architecture"
          ),
          
          plotOutput(
            "architecture",
            height = "700px"
          )
        )
        
      )
    )
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # LOAD DATA
  # ==========================================================
  
  raw_data <- eventReactive(
    input$load_data,
    {
      
      req(input$file)
      
      read.csv(
        input$file$datapath,
        stringsAsFactors = FALSE
      )
      
    }
  )
  
  
  # ==========================================================
  # MODEL DATA
  # ==========================================================
  
  modeled_data <- eventReactive(
    input$calculate,
    {
      
      data <- raw_data()
      
      # ------------------------------------------------------
      # CHECK REQUIRED COLUMNS
      # ------------------------------------------------------
      
      required_columns <- c(
        
        "Job_ID",
        "Pipeline_ID",
        "Scan_Type",
        "Severity",
        "Vulnerability_Count",
        "Arrival_Time_Min",
        "Service_Time_Min",
        "Risk_Score",
        "Developer_Experience",
        "Code_Change_Size"
        
      )
      
      
      missing_columns <- setdiff(
        required_columns,
        names(data)
      )
      
      
      if (length(missing_columns) > 0) {
        
        stop(
          paste(
            "Missing columns:",
            paste(
              missing_columns,
              collapse = ", "
            )
          )
        )
        
      }
      
      
      # ------------------------------------------------------
      # SEVERITY SCORE
      # ------------------------------------------------------
      
      data$Severity_Score <- ifelse(
        
        data$Severity == "Critical",
        100,
        
        ifelse(
          data$Severity == "High",
          75,
          
          ifelse(
            data$Severity == "Medium",
            50,
            25
          )
        )
      )
      
      
      # ------------------------------------------------------
      # VULNERABILITY NORMALIZATION
      # ------------------------------------------------------
      
      max_vulnerability <- max(
        data$Vulnerability_Count
      )
      
      
      data$Vulnerability_Normalized <- (
        
        data$Vulnerability_Count /
          max_vulnerability
        
      ) * 100
      
      
      # ------------------------------------------------------
      # RISK NORMALIZATION
      # ------------------------------------------------------
      
      data$Risk_Normalized <-
        data$Risk_Score
      
      
      # ------------------------------------------------------
      # WAITING FACTOR
      # ------------------------------------------------------
      
      max_arrival <- max(
        data$Arrival_Time_Min
      )
      
      
      data$Waiting_Factor <- (
        
        data$Arrival_Time_Min /
          max_arrival
        
      ) * 100
      
      
      # ------------------------------------------------------
      # WEIGHT VALIDATION
      # ------------------------------------------------------
      
      total_weight <-
        
        input$w_severity +
        input$w_risk +
        input$w_vulnerability +
        input$w_waiting
      
      
      if (
        abs(total_weight - 1) > 0.001
      ) {
        
        showNotification(
          
          "Weights must add up to 1.00",
          
          type = "error"
        )
        
        return(NULL)
        
      }
      
      
      # ------------------------------------------------------
      # GATEKEEPERX PRIORITY SCORE
      # ------------------------------------------------------
      
      data$GatekeeperX_Priority_Score <-
        
        (
          
          input$w_severity *
            data$Severity_Score
          
        ) +
        
        (
          
          input$w_risk *
            data$Risk_Normalized
          
        ) +
        
        (
          
          input$w_vulnerability *
            data$Vulnerability_Normalized
          
        ) +
        
        (
          
          input$w_waiting *
            data$Waiting_Factor
          
        )
      
      
      data$GatekeeperX_Priority_Score <-
        
        round(
          data$GatekeeperX_Priority_Score,
          2
        )
      
      
      # ------------------------------------------------------
      # PRIORITY CLASS
      # ------------------------------------------------------
      
      data$Priority_Class <- ifelse(
        
        data$GatekeeperX_Priority_Score >= 80,
        
        "P1 - Critical",
        
        ifelse(
          
          data$GatekeeperX_Priority_Score >= 60,
          
          "P2 - High",
          
          ifelse(
            
            data$GatekeeperX_Priority_Score >= 40,
            
            "P3 - Medium",
            
            "P4 - Low"
            
          )
        )
      )
      
      
      # ------------------------------------------------------
      # SECURITY DECISION
      # ------------------------------------------------------
      
      data$Security_Decision <- ifelse(
        
        data$Severity == "Critical" &
          data$Risk_Score >= 70,
        
        "BLOCK",
        
        ifelse(
          
          data$Risk_Score >= 70,
          
          "WAIT / PRIORITIZE",
          
          ifelse(
            
            data$Risk_Score >= 40,
            
            "PASS WITH MONITORING",
            
            "PASS"
            
          )
        )
      )
      
      
      # ------------------------------------------------------
      # QUEUE POSITION
      # ------------------------------------------------------
      
      data$Queue_Position <- rank(
        
        -data$GatekeeperX_Priority_Score,
        
        ties.method = "first"
      )
      
      
      data
      
    }
  )
  
  
  # ==========================================================
  # DASHBOARD
  # ==========================================================
  
  output$total_jobs <- renderText({
    
    req(modeled_data())
    
    nrow(
      modeled_data()
    )
    
  })
  
  
  output$lambda <- renderText({
    
    data <- modeled_data()
    
    total_time <- max(
      data$Arrival_Time_Min
    )
    
    lambda <- nrow(data) /
      total_time
    
    round(
      lambda,
      4
    )
    
  })
  
  
  output$mu <- renderText({
    
    data <- modeled_data()
    
    average_service <- mean(
      data$Service_Time_Min
    )
    
    mu <- 1 /
      average_service
    
    round(
      mu,
      4
    )
    
  })
  
  
  output$rho <- renderText({
    
    data <- modeled_data()
    
    total_time <- max(
      data$Arrival_Time_Min
    )
    
    lambda <- nrow(data) /
      total_time
    
    average_service <- mean(
      data$Service_Time_Min
    )
    
    mu <- 1 /
      average_service
    
    rho <- lambda /
      mu
    
    round(
      rho,
      4
    )
    
  })
  
  
  output$queue_status <- renderText({
    
    data <- modeled_data()
    
    total_time <- max(
      data$Arrival_Time_Min
    )
    
    lambda <- nrow(data) /
      total_time
    
    mu <- 1 /
      mean(
        data$Service_Time_Min
      )
    
    rho <- lambda /
      mu
    
    if (
      rho < 1
    ) {
      
      "STABLE"
      
    } else {
      
      "CONGESTED"
      
    }
    
  })
  
  
  output$avg_priority <- renderText({
    
    round(
      
      mean(
        modeled_data()$GatekeeperX_Priority_Score
      ),
      
      2
      
    )
    
  })
  
  
  # ==========================================================
  # QUEUEING METRICS
  # ==========================================================
  
  output$queue_metrics <- renderTable({
    
    data <- modeled_data()
    
    total_time <- max(
      data$Arrival_Time_Min
    )
    
    lambda <- nrow(data) /
      total_time
    
    average_service <- mean(
      data$Service_Time_Min
    )
    
    mu <- 1 /
      average_service
    
    rho <- lambda /
      mu
    
    
    if (
      rho < 1
    ) {
      
      L <- rho /
        (1 - rho)
      
      Lq <- rho^2 /
        (1 - rho)
      
      W <- 1 /
        (mu - lambda)
      
      Wq <- lambda /
        (
          mu *
            (mu - lambda)
        )
      
    } else {
      
      L <- Inf
      Lq <- Inf
      W <- Inf
      Wq <- Inf
      
    }
    
    
    data.frame(
      
      Metric = c(
        
        "Number of Jobs",
        
        "Arrival Rate (λ)",
        
        "Average Service Time",
        
        "Service Rate (μ)",
        
        "Utilization (ρ)",
        
        "Average Jobs in System (L)",
        
        "Average Queue Length (Lq)",
        
        "Average Time in System (W)",
        
        "Average Waiting Time (Wq)"
        
      ),
      
      Value = c(
        
        nrow(data),
        
        round(lambda, 4),
        
        round(average_service, 4),
        
        round(mu, 4),
        
        round(rho, 4),
        
        round(L, 4),
        
        round(Lq, 4),
        
        round(W, 4),
        
        round(Wq, 4)
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # MODELED DATA
  # ==========================================================
  
  output$modeled_data <- renderTable({
    
    head(
      modeled_data(),
      50
    )
    
  })
  
  
  # ==========================================================
  # PLOT 1
  # ==========================================================
  
  output$priority_plot <- renderPlot({
    
    data <- modeled_data()
    
    hist(
      
      data$GatekeeperX_Priority_Score,
      
      breaks = 20,
      
      col = "steelblue",
      
      border = "white",
      
      main =
        "GatekeeperX Priority Score Distribution",
      
      xlab =
        "Priority Score",
      
      ylab =
        "Number of Jobs"
    )
    
    grid()
    
  })
  
  
  # ==========================================================
  # PLOT 2
  # ==========================================================
  
  output$priority_class_plot <- renderPlot({
    
    data <- modeled_data()
    
    counts <- table(
      data$Priority_Class
    )
    
    
    barplot(
      
      counts,
      
      col = "orange",
      
      border = "white",
      
      main =
        "GatekeeperX Priority Class Distribution",
      
      xlab =
        "Priority Class",
      
      ylab =
        "Number of Jobs"
    )
    
    
    text(
      
      seq_along(counts),
      
      counts,
      
      labels = counts,
      
      pos = 3
    )
    
  })
  
  
  # ==========================================================
  # PLOT 3
  # ==========================================================
  
  output$risk_priority_plot <- renderPlot({
    
    data <- modeled_data()
    
    
    plot(
      
      data$Risk_Score,
      
      data$GatekeeperX_Priority_Score,
      
      pch = 19,
      
      col = "purple",
      
      main =
        "Risk Score vs GatekeeperX Priority Score",
      
      xlab =
        "Risk Score",
      
      ylab =
        "GatekeeperX Priority Score"
    )
    
    
    abline(
      
      lm(
        GatekeeperX_Priority_Score ~
          Risk_Score,
        
        data = data
      ),
      
      lwd = 2
    )
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # PLOT 4
  # ==========================================================
  
  output$decision_plot <- renderPlot({
    
    data <- modeled_data()
    
    counts <- table(
      data$Security_Decision
    )
    
    
    barplot(
      
      counts,
      
      col = "darkgreen",
      
      border = "white",
      
      main =
        "GatekeeperX Security Gate Decisions",
      
      xlab =
        "Security Decision",
      
      ylab =
        "Number of Jobs",
      
      las = 2
    )
    
    
    text(
      
      seq_along(counts),
      
      counts,
      
      labels = counts,
      
      pos = 3
      
    )
    
  })
  
  
  # ==========================================================
  # SECURITY DECISION TABLE
  # ==========================================================
  
  output$decision_table <- renderTable({
    
    table(
      modeled_data()$Security_Decision
    )
    
  })
  
  
  # ==========================================================
  # MODEL PARAMETERS
  # ==========================================================
  
  output$model_parameters <- renderTable({
    
    data <- modeled_data()
    
    total_time <- max(
      data$Arrival_Time_Min
    )
    
    lambda <- nrow(data) /
      total_time
    
    average_service <- mean(
      data$Service_Time_Min
    )
    
    mu <- 1 /
      average_service
    
    rho <- lambda /
      mu
    
    
    data.frame(
      
      Parameter = c(
        
        "Number of Jobs",
        
        "Total Simulation Time",
        
        "Arrival Rate λ",
        
        "Average Service Time",
        
        "Service Rate μ",
        
        "Utilization ρ",
        
        "Queue Status"
        
      ),
      
      Value = c(
        
        nrow(data),
        
        round(
          total_time,
          2
        ),
        
        round(
          lambda,
          4
        ),
        
        round(
          average_service,
          4
        ),
        
        round(
          mu,
          4
        ),
        
        round(
          rho,
          4
        ),
        
        ifelse(
          rho < 1,
          "STABLE",
          "CONGESTED"
        )
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # ARCHITECTURE DIAGRAM
  # ==========================================================
  
  output$architecture <- renderPlot({
    
    plot.new()
    
    plot.window(
      xlim = c(0, 12),
      ylim = c(0, 10)
    )
    
    
    draw_box <- function(
    x,
    y,
    label
    ) {
      
      rect(
        
        x - 1,
        y - 0.5,
        
        x + 1,
        y + 0.5,
        
        col = "lightblue",
        
        border = "navy",
        
        lwd = 2
        
      )
      
      
      text(
        
        x,
        y,
        
        label,
        
        cex = 1,
        
        font = 2
        
      )
      
    }
    
    
    draw_arrow <- function(
    x1,
    y1,
    x2,
    y2
    ) {
      
      arrows(
        
        x1,
        y1,
        
        x2,
        y2,
        
        length = 0.12,
        
        lwd = 2
        
      )
      
    }
    
    
    # TOP ROW
    
    draw_box(
      1.5,
      8,
      "Module 1\nCSV"
    )
    
    draw_box(
      3.5,
      8,
      "Arrival Rate\nλ"
    )
    
    draw_box(
      5.5,
      8,
      "Service Rate\nμ"
    )
    
    draw_box(
      7.5,
      8,
      "Utilization\nρ"
    )
    
    draw_box(
      9.5,
      8,
      "Queueing\nModel"
    )
    
    
    # BOTTOM ROW
    
    draw_box(
      9.5,
      5,
      "Severity"
    )
    
    draw_box(
      7.5,
      5,
      "Risk Score"
    )
    
    draw_box(
      5.5,
      5,
      "Vulnerability\nCount"
    )
    
    draw_box(
      3.5,
      5,
      "Priority\nScore"
    )
    
    draw_box(
      1.5,
      5,
      "Security\nDecision"
    )
    
    
    # TOP ARROWS
    
    draw_arrow(
      2.5,
      8,
      3,
      8
    )
    
    draw_arrow(
      4.5,
      8,
      5,
      8
    )
    
    draw_arrow(
      6.5,
      8,
      7,
      8
    )
    
    draw_arrow(
      8.5,
      8,
      9,
      8
    )
    
    
    # DOWN
    
    draw_arrow(
      9.5,
      7.5,
      9.5,
      5.5
    )
    
    
    # BOTTOM ARROWS
    
    draw_arrow(
      8.5,
      5,
      8,
      5
    )
    
    draw_arrow(
      7,
      5,
      6,
      5
    )
    
    draw_arrow(
      5,
      5,
      4.5,
      5
    )
    
    draw_arrow(
      3,
      5,
      2.5,
      5
    )
    
    
    title(
      "GatekeeperX - Module 2 Modeling Architecture",
      cex.main = 1.5
    )
    
  })
  
  
  # ==========================================================
  # DOWNLOAD MODELED DATASET
  # ==========================================================
  
  output$download_model <- downloadHandler(
    
    filename = function() {
      
      "GatekeeperX_Module2_Modeled_Data.csv"
      
    },
    
    content = function(file) {
      
      write.csv(
        
        modeled_data(),
        
        file,
        
        row.names = FALSE
        
      )
      
    }
    
  )
  
}


# ============================================================
# RUN SHINY APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)