# ============================================================
# GATEKEEPERX
# MODULE 4 - SHINY APP
# RESULTS & EVALUATION
# ============================================================

library(shiny)

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  titlePanel(
    "GatekeeperX - Module 4: Results & Evaluation"
  ),
  
  sidebarLayout(
    
    # ========================================================
    # SIDEBAR
    # ========================================================
    
    sidebarPanel(
      
      h4("Module 3 Input"),
      
      fileInput(
        inputId = "file",
        label = "Upload Module 3 Simulation CSV:",
        accept = ".csv"
      ),
      
      br(),
      
      numericInput(
        inputId = "waiting_threshold",
        label = "Waiting Time Threshold (minutes):",
        value = 10,
        min = 1,
        max = 100,
        step = 1
      ),
      
      numericInput(
        inputId = "turnaround_threshold",
        label = "Turnaround Threshold (minutes):",
        value = 20,
        min = 1,
        max = 200,
        step = 1
      ),
      
      numericInput(
        inputId = "correlation_threshold",
        label = "Risk-Priority Correlation Threshold:",
        value = 0.70,
        min = 0,
        max = 1,
        step = 0.05
      ),
      
      br(),
      
      actionButton(
        inputId = "evaluate",
        label = "Evaluate GatekeeperX",
        class = "btn-primary"
      ),
      
      br(),
      br(),
      
      downloadButton(
        outputId = "download_metrics",
        label = "Download Evaluation Metrics"
      ),
      
      br(),
      br(),
      
      downloadButton(
        outputId = "download_report",
        label = "Download Evaluation Report"
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
                h4("Total Jobs"),
                h3(textOutput("total_jobs"))
              )
            ),
            
            column(
              3,
              wellPanel(
                h4("Avg Waiting"),
                h3(textOutput("avg_waiting"))
              )
            ),
            
            column(
              3,
              wellPanel(
                h4("Avg Turnaround"),
                h3(textOutput("avg_turnaround"))
              )
            ),
            
            column(
              3,
              wellPanel(
                h4("Throughput"),
                h3(textOutput("throughput"))
              )
            )
            
          ),
          
          br(),
          
          fluidRow(
            
            column(
              4,
              wellPanel(
                h4("Queue Length"),
                h3(textOutput("queue_length"))
              )
            ),
            
            column(
              4,
              wellPanel(
                h4("Utilization"),
                h3(textOutput("utilization"))
              )
            ),
            
            column(
              4,
              wellPanel(
                h4("Evaluation Score"),
                h3(textOutput("evaluation_score"))
              )
            )
            
          ),
          
          br(),
          
          wellPanel(
            
            h3("Overall Result"),
            
            h2(
              textOutput(
                "overall_result"
              )
            )
            
          ),
          
          br(),
          
          h3("Evaluation Summary"),
          
          tableOutput(
            "evaluation_table"
          )
          
        ),
        
        
        # ====================================================
        # TAB 2 - METRICS
        # ====================================================
        
        tabPanel(
          "Performance Metrics",
          
          br(),
          
          h3(
            "GatekeeperX Performance Metrics"
          ),
          
          tableOutput(
            "metrics_table"
          )
          
        ),
        
        
        # ====================================================
        # TAB 3 - WAITING TIME
        # ====================================================
        
        tabPanel(
          "Waiting Time",
          
          br(),
          
          h3(
            "Waiting Time Distribution"
          ),
          
          plotOutput(
            "waiting_plot",
            height = "600px"
          ),
          
          br(),
          
          h4("Waiting Time Statistics"),
          
          tableOutput(
            "waiting_statistics"
          )
          
        ),
        
        
        # ====================================================
        # TAB 4 - SEVERITY ANALYSIS
        # ====================================================
        
        tabPanel(
          "Severity Analysis",
          
          br(),
          
          h3(
            "Waiting Time by Security Severity"
          ),
          
          plotOutput(
            "severity_plot",
            height = "600px"
          ),
          
          br(),
          
          tableOutput(
            "severity_table"
          )
          
        ),
        
        
        # ====================================================
        # TAB 5 - RISK PRIORITY
        # ====================================================
        
        tabPanel(
          "Risk vs Priority",
          
          br(),
          
          h3(
            "Risk Score vs GatekeeperX Priority"
          ),
          
          plotOutput(
            "risk_priority_plot",
            height = "600px"
          ),
          
          br(),
          
          h4(
            "Correlation"
          ),
          
          h3(
            textOutput(
              "correlation"
            )
          )
          
        ),
        
        
        # ====================================================
        # TAB 6 - TURNAROUND
        # ====================================================
        
        tabPanel(
          "Turnaround Time",
          
          br(),
          
          h3(
            "Turnaround Time Distribution"
          ),
          
          plotOutput(
            "turnaround_plot",
            height = "600px"
          )
          
        ),
        
        
        # ====================================================
        # TAB 7 - SECURITY DECISIONS
        # ====================================================
        
        tabPanel(
          "Security Decisions",
          
          br(),
          
          h3(
            "GatekeeperX Security Decisions"
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
        # TAB 8 - PRIORITY CLASS
        # ====================================================
        
        tabPanel(
          "Priority Classes",
          
          br(),
          
          h3(
            "GatekeeperX Priority Classes"
          ),
          
          plotOutput(
            "priority_plot",
            height = "600px"
          )
          
        ),
        
        
        # ====================================================
        # TAB 9 - EVALUATION
        # ====================================================
        
        tabPanel(
          "Evaluation",
          
          br(),
          
          h2(
            "GatekeeperX Evaluation"
          ),
          
          tableOutput(
            "evaluation_details"
          ),
          
          br(),
          
          h3(
            "Overall Evaluation Score"
          ),
          
          plotOutput(
            "score_plot",
            height = "400px"
          )
          
        ),
        
        
        # ====================================================
        # TAB 10 - ARCHITECTURE
        # ====================================================
        
        tabPanel(
          "Architecture",
          
          br(),
          
          h2(
            "Module 4 Results & Evaluation Architecture"
          ),
          
          plotOutput(
            "architecture_plot",
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
    
    input$evaluate,
    
    {
      
      req(input$file)
      
      read.csv(
        
        input$file$datapath,
        
        stringsAsFactors = FALSE
        
      )
      
    }
    
  )
  
  
  # ==========================================================
  # EVALUATION FUNCTION
  # ==========================================================
  
  evaluation <- eventReactive(
    
    input$evaluate,
    
    {
      
      data <- raw_data()
      
      
      # ======================================================
      # REQUIRED COLUMNS
      # ======================================================
      
      required_columns <- c(
        
        "Job_ID",
        
        "Severity",
        
        "Risk_Score",
        
        "GatekeeperX_Priority_Score",
        
        "Priority_Class",
        
        "Security_Decision",
        
        "Service_Time_Min",
        
        "Arrival_Time_Min",
        
        "Service_Start_Time",
        
        "Completion_Time",
        
        "Actual_Waiting_Time",
        
        "Turnaround_Time",
        
        "Dynamic_Priority",
        
        "Simulation_Status"
        
      )
      
      
      missing_columns <- setdiff(
        
        required_columns,
        
        names(data)
        
      )
      
      
      if (
        length(missing_columns) > 0
      ) {
        
        showNotification(
          
          paste(
            "Missing columns:",
            paste(
              missing_columns,
              collapse = ", "
            )
          ),
          
          type = "error"
          
        )
        
        return(NULL)
        
      }
      
      
      # ======================================================
      # BASIC METRICS
      # ======================================================
      
      total_jobs <-
        nrow(data)
      
      
      total_simulation_time <-
        
        max(
          data$Completion_Time
        )
      
      
      # ======================================================
      # WAITING TIME
      # ======================================================
      
      average_waiting_time <-
        
        mean(
          data$Actual_Waiting_Time
        )
      
      
      minimum_waiting_time <-
        
        min(
          data$Actual_Waiting_Time
        )
      
      
      maximum_waiting_time <-
        
        max(
          data$Actual_Waiting_Time
        )
      
      
      median_waiting_time <-
        
        median(
          data$Actual_Waiting_Time
        )
      
      
      # ======================================================
      # TURNAROUND
      # ======================================================
      
      average_turnaround_time <-
        
        mean(
          data$Turnaround_Time
        )
      
      
      minimum_turnaround_time <-
        
        min(
          data$Turnaround_Time
        )
      
      
      maximum_turnaround_time <-
        
        max(
          data$Turnaround_Time
        )
      
      
      median_turnaround_time <-
        
        median(
          data$Turnaround_Time
        )
      
      
      # ======================================================
      # SERVICE
      # ======================================================
      
      average_service_time <-
        
        mean(
          data$Service_Time_Min
        )
      
      
      # ======================================================
      # UTILIZATION
      # ======================================================
      
      total_service_time <-
        
        sum(
          data$Service_Time_Min
        )
      
      
      server_utilization <-
        
        total_service_time /
        total_simulation_time
      
      
      # ======================================================
      # THROUGHPUT
      # ======================================================
      
      throughput <-
        
        total_jobs /
        total_simulation_time *
        60
      
      
      # ======================================================
      # QUEUE LENGTH
      # ======================================================
      
      queue_lengths <- numeric(
        total_jobs
      )
      
      
      for (
        i in 1:total_jobs
      ) {
        
        queue_lengths[i] <-
          
          sum(
            data$Arrival_Time_Min <=
              data$Service_Start_Time[i]
          ) -
          
          sum(
            data$Completion_Time <=
              data$Service_Start_Time[i]
          )
        
      }
      
      
      average_queue_length <-
        
        mean(
          queue_lengths
        )
      
      
      maximum_queue_length <-
        
        max(
          queue_lengths
        )
      
      
      # ======================================================
      # SECURITY DECISIONS
      # ======================================================
      
      decision_counts <-
        
        table(
          data$Security_Decision
        )
      
      
      blocked_jobs <-
        
        sum(
          data$Security_Decision ==
            "BLOCK"
        )
      
      
      prioritized_jobs <-
        
        sum(
          data$Security_Decision ==
            "WAIT / PRIORITIZE"
        )
      
      
      passed_jobs <-
        
        sum(
          data$Security_Decision ==
            "PASS"
        )
      
      
      monitored_jobs <-
        
        sum(
          data$Security_Decision ==
            "PASS WITH MONITORING"
        )
      
      
      # ======================================================
      # PRIORITY
      # ======================================================
      
      average_priority <-
        
        mean(
          data$GatekeeperX_Priority_Score
        )
      
      
      maximum_priority <-
        
        max(
          data$GatekeeperX_Priority_Score
        )
      
      
      minimum_priority <-
        
        min(
          data$GatekeeperX_Priority_Score
        )
      
      
      # ======================================================
      # RISK-PRIORITY CORRELATION
      # ======================================================
      
      risk_priority_correlation <-
        
        cor(
          
          data$Risk_Score,
          
          data$GatekeeperX_Priority_Score,
          
          method = "pearson"
          
        )
      
      
      # ======================================================
      # SEVERITY NUMERIC
      # ======================================================
      
      severity_numeric <-
        
        ifelse(
          
          data$Severity == "Critical",
          4,
          
          ifelse(
            
            data$Severity == "High",
            3,
            
            ifelse(
              
              data$Severity == "Medium",
              2,
              1
              
            )
            
          )
          
        )
      
      
      severity_priority_correlation <-
        
        cor(
          
          severity_numeric,
          
          data$GatekeeperX_Priority_Score,
          
          method = "pearson"
          
        )
      
      
      # ======================================================
      # CRITICAL / HIGH PRIORITY
      # ======================================================
      
      critical_jobs <-
        
        data[
          data$Severity == "Critical",
        ]
      
      
      high_jobs <-
        
        data[
          data$Severity == "High",
        ]
      
      
      if (
        nrow(critical_jobs) > 0
      ) {
        
        critical_average_priority <-
          
          mean(
            critical_jobs$GatekeeperX_Priority_Score
          )
        
      } else {
        
        critical_average_priority <- 0
        
      }
      
      
      if (
        nrow(high_jobs) > 0
      ) {
        
        high_average_priority <-
          
          mean(
            high_jobs$GatekeeperX_Priority_Score
          )
        
      } else {
        
        high_average_priority <- 0
        
      }
      
      
      # ======================================================
      # EVALUATION CRITERIA
      # ======================================================
      
      if (
        server_utilization < 1
      ) {
        
        queue_stability <- "PASS"
        
      } else {
        
        queue_stability <- "FAIL"
        
      }
      
      
      if (
        average_waiting_time <=
        input$waiting_threshold
      ) {
        
        waiting_evaluation <- "PASS"
        
      } else {
        
        waiting_evaluation <-
          "NEEDS IMPROVEMENT"
        
      }
      
      
      if (
        average_turnaround_time <=
        input$turnaround_threshold
      ) {
        
        turnaround_evaluation <- "PASS"
        
      } else {
        
        turnaround_evaluation <-
          "NEEDS IMPROVEMENT"
        
      }
      
      
      if (
        critical_average_priority >=
        high_average_priority
      ) {
        
        security_prioritization <-
          "PASS"
        
      } else {
        
        security_prioritization <-
          "FAIL"
        
      }
      
      
      if (
        risk_priority_correlation >=
        input$correlation_threshold
      ) {
        
        risk_alignment <- "PASS"
        
      } else {
        
        risk_alignment <-
          "NEEDS IMPROVEMENT"
        
      }
      
      
      # ======================================================
      # EVALUATION TABLE
      # ======================================================
      
      evaluation_results <- data.frame(
        
        Evaluation = c(
          
          "Queue Stability",
          
          "Average Waiting Time",
          
          "Average Turnaround Time",
          
          "Critical Vulnerability Prioritization",
          
          "Risk-Priority Alignment"
          
        ),
        
        Result = c(
          
          queue_stability,
          
          waiting_evaluation,
          
          turnaround_evaluation,
          
          security_prioritization,
          
          risk_alignment
          
        )
        
      )
      
      
      # ======================================================
      # SCORE
      # ======================================================
      
      pass_count <-
        
        sum(
          evaluation_results$Result ==
            "PASS"
        )
      
      
      total_evaluations <-
        
        nrow(
          evaluation_results
        )
      
      
      evaluation_score <-
        
        pass_count /
        total_evaluations *
        100
      
      
      if (
        evaluation_score >= 80
      ) {
        
        overall_result <-
          "GOOD PERFORMANCE"
        
      } else if (
        evaluation_score >= 60
      ) {
        
        overall_result <-
          "MODERATE PERFORMANCE"
        
      } else {
        
        overall_result <-
          "NEEDS IMPROVEMENT"
        
      }
      
      
      # ======================================================
      # SEVERITY ANALYSIS
      # ======================================================
      
      severity_order <- c(
        
        "Critical",
        "High",
        "Medium",
        "Low"
        
      )
      
      
      severity_values <-
        
        sapply(
          
          severity_order,
          
          function(x) {
            
            if (
              sum(
                data$Severity == x
              ) == 0
            ) {
              
              return(0)
              
            }
            
            
            mean(
              
              data$Actual_Waiting_Time[
                data$Severity == x
              ]
              
            )
            
          }
          
        )
      
      
      severity_table <-
        
        data.frame(
          
          Severity =
            severity_order,
          
          Average_Waiting_Time =
            round(
              severity_values,
              2
            )
          
        )
      
      
      # ======================================================
      # RETURN
      # ======================================================
      
      list(
        
        data = data,
        
        total_jobs =
          total_jobs,
        
        total_simulation_time =
          total_simulation_time,
        
        average_waiting_time =
          average_waiting_time,
        
        minimum_waiting_time =
          minimum_waiting_time,
        
        maximum_waiting_time =
          maximum_waiting_time,
        
        median_waiting_time =
          median_waiting_time,
        
        average_turnaround_time =
          average_turnaround_time,
        
        minimum_turnaround_time =
          minimum_turnaround_time,
        
        maximum_turnaround_time =
          maximum_turnaround_time,
        
        median_turnaround_time =
          median_turnaround_time,
        
        average_service_time =
          average_service_time,
        
        server_utilization =
          server_utilization,
        
        throughput =
          throughput,
        
        average_queue_length =
          average_queue_length,
        
        maximum_queue_length =
          maximum_queue_length,
        
        decision_counts =
          decision_counts,
        
        blocked_jobs =
          blocked_jobs,
        
        prioritized_jobs =
          prioritized_jobs,
        
        passed_jobs =
          passed_jobs,
        
        monitored_jobs =
          monitored_jobs,
        
        average_priority =
          average_priority,
        
        maximum_priority =
          maximum_priority,
        
        minimum_priority =
          minimum_priority,
        
        risk_priority_correlation =
          risk_priority_correlation,
        
        severity_priority_correlation =
          severity_priority_correlation,
        
        critical_average_priority =
          critical_average_priority,
        
        high_average_priority =
          high_average_priority,
        
        severity_table =
          severity_table,
        
        evaluation_results =
          evaluation_results,
        
        evaluation_score =
          evaluation_score,
        
        overall_result =
          overall_result
        
      )
      
    }
    
  )
  
  
  # ==========================================================
  # DASHBOARD
  # ==========================================================
  
  output$total_jobs <- renderText({
    
    req(evaluation())
    
    evaluation()$total_jobs
    
  })
  
  
  output$avg_waiting <- renderText({
    
    req(evaluation())
    
    paste(
      
      round(
        evaluation()$average_waiting_time,
        2
      ),
      
      "min"
      
    )
    
  })
  
  
  output$avg_turnaround <- renderText({
    
    req(evaluation())
    
    paste(
      
      round(
        evaluation()$average_turnaround_time,
        2
      ),
      
      "min"
      
    )
    
  })
  
  
  output$throughput <- renderText({
    
    req(evaluation())
    
    paste(
      
      round(
        evaluation()$throughput,
        2
      ),
      
      "jobs/hr"
      
    )
    
  })
  
  
  output$queue_length <- renderText({
    
    req(evaluation())
    
    round(
      
      evaluation()$average_queue_length,
      
      2
      
    )
    
  })
  
  
  output$utilization <- renderText({
    
    req(evaluation())
    
    paste(
      
      round(
        
        evaluation()$server_utilization *
          100,
        
        2
        
      ),
      
      "%"
      
    )
    
  })
  
  
  output$evaluation_score <- renderText({
    
    req(evaluation())
    
    paste(
      
      round(
        evaluation()$evaluation_score,
        1
      ),
      
      "%"
      
    )
    
  })
  
  
  output$overall_result <- renderText({
    
    req(evaluation())
    
    evaluation()$overall_result
    
  })
  
  
  # ==========================================================
  # EVALUATION SUMMARY TABLE
  # ==========================================================
  
  output$evaluation_table <- renderTable({
    
    req(evaluation())
    
    evaluation()$evaluation_results
    
  })
  
  
  # ==========================================================
  # PERFORMANCE METRICS TABLE
  # ==========================================================
  
  output$metrics_table <- renderTable({
    
    result <- evaluation()
    
    
    data.frame(
      
      Metric = c(
        
        "Total Jobs",
        
        "Simulation Time",
        
        "Average Waiting Time",
        
        "Minimum Waiting Time",
        
        "Maximum Waiting Time",
        
        "Median Waiting Time",
        
        "Average Turnaround Time",
        
        "Minimum Turnaround Time",
        
        "Maximum Turnaround Time",
        
        "Median Turnaround Time",
        
        "Average Service Time",
        
        "Average Queue Length",
        
        "Maximum Queue Length",
        
        "Server Utilization",
        
        "Throughput",
        
        "Average Priority",
        
        "Risk-Priority Correlation"
        
      ),
      
      Value = c(
        
        result$total_jobs,
        
        paste(
          round(
            result$total_simulation_time,
            2
          ),
          "min"
        ),
        
        paste(
          round(
            result$average_waiting_time,
            2
          ),
          "min"
        ),
        
        paste(
          round(
            result$minimum_waiting_time,
            2
          ),
          "min"
        ),
        
        paste(
          round(
            result$maximum_waiting_time,
            2
          ),
          "min"
        ),
        
        paste(
          round(
            result$median_waiting_time,
            2
          ),
          "min"
        ),
        
        paste(
          round(
            result$average_turnaround_time,
            2
          ),
          "min"
        ),
        
        paste(
          round(
            result$minimum_turnaround_time,
            2
          ),
          "min"
        ),
        
        paste(
          round(
            result$maximum_turnaround_time,
            2
          ),
          "min"
        ),
        
        paste(
          round(
            result$median_turnaround_time,
            2
          ),
          "min"
        ),
        
        paste(
          round(
            result$average_service_time,
            2
          ),
          "min"
        ),
        
        round(
          result$average_queue_length,
          2
        ),
        
        result$maximum_queue_length,
        
        paste(
          round(
            result$server_utilization *
              100,
            2
          ),
          "%"
        ),
        
        paste(
          round(
            result$throughput,
            2
          ),
          "jobs/hr"
        ),
        
        round(
          result$average_priority,
          2
        ),
        
        round(
          result$risk_priority_correlation,
          3
        )
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # WAITING TIME PLOT
  # ==========================================================
  
  output$waiting_plot <- renderPlot({
    
    result <- evaluation()
    
    hist(
      
      result$data$Actual_Waiting_Time,
      
      breaks = 25,
      
      col = "steelblue",
      
      border = "white",
      
      main =
        "GatekeeperX - Waiting Time Distribution",
      
      xlab =
        "Waiting Time (minutes)",
      
      ylab =
        "Number of Jobs"
      
    )
    
    
    abline(
      
      v =
        result$average_waiting_time,
      
      col = "red",
      
      lwd = 3,
      
      lty = 2
      
    )
    
    
    abline(
      
      v =
        input$waiting_threshold,
      
      col = "black",
      
      lwd = 2,
      
      lty = 3
      
    )
    
    
    legend(
      
      "topright",
      
      legend = c(
        
        "Average",
        
        "Evaluation Threshold"
        
      ),
      
      col = c(
        "red",
        "black"
      ),
      
      lwd = 3,
      
      lty = c(
        2,
        3
      )
      
    )
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # WAITING STATISTICS
  # ==========================================================
  
  output$waiting_statistics <- renderTable({
    
    result <- evaluation()
    
    
    data.frame(
      
      Statistic = c(
        
        "Minimum",
        
        "Average",
        
        "Median",
        
        "Maximum",
        
        "Threshold"
        
      ),
      
      Minutes = c(
        
        round(
          result$minimum_waiting_time,
          2
        ),
        
        round(
          result$average_waiting_time,
          2
        ),
        
        round(
          result$median_waiting_time,
          2
        ),
        
        round(
          result$maximum_waiting_time,
          2
        ),
        
        input$waiting_threshold
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # SEVERITY PLOT
  # ==========================================================
  
  output$severity_plot <- renderPlot({
    
    result <- evaluation()
    
    
    values <-
      
      result$severity_table$Average_Waiting_Time
    
    
    barplot(
      
      values,
      
      names.arg =
        result$severity_table$Severity,
      
      col = "orange",
      
      border = "white",
      
      main =
        "Average Waiting Time by Severity",
      
      xlab =
        "Severity",
      
      ylab =
        "Average Waiting Time (minutes)"
      
    )
    
    
    text(
      
      seq_along(values),
      
      values,
      
      labels = values,
      
      pos = 3
      
    )
    
    
    grid()
    
  })
  
  
  output$severity_table <- renderTable({
    
    evaluation()$severity_table
    
  })
  
  
  # ==========================================================
  # RISK VS PRIORITY PLOT
  # ==========================================================
  
  output$risk_priority_plot <- renderPlot({
    
    result <- evaluation()
    
    
    plot(
      
      result$data$Risk_Score,
      
      result$data$GatekeeperX_Priority_Score,
      
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
        
        data =
          result$data
        
      ),
      
      lwd = 3
      
    )
    
    
    grid()
    
  })
  
  
  output$correlation <- renderText({
    
    paste(
      
      "Pearson Correlation =",
      
      round(
        
        evaluation()$risk_priority_correlation,
        
        3
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # TURNAROUND PLOT
  # ==========================================================
  
  output$turnaround_plot <- renderPlot({
    
    result <- evaluation()
    
    
    hist(
      
      result$data$Turnaround_Time,
      
      breaks = 25,
      
      col = "darkgreen",
      
      border = "white",
      
      main =
        "GatekeeperX - Turnaround Time Distribution",
      
      xlab =
        "Turnaround Time (minutes)",
      
      ylab =
        "Number of Jobs"
      
    )
    
    
    abline(
      
      v =
        result$average_turnaround_time,
      
      col = "red",
      
      lwd = 3,
      
      lty = 2
      
    )
    
    
    abline(
      
      v =
        input$turnaround_threshold,
      
      col = "black",
      
      lwd = 2,
      
      lty = 3
      
    )
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # SECURITY DECISION PLOT
  # ==========================================================
  
  output$decision_plot <- renderPlot({
    
    result <- evaluation()
    
    
    counts <-
      result$decision_counts
    
    
    barplot(
      
      counts,
      
      col = "steelblue",
      
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
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # DECISION TABLE
  # ==========================================================
  
  output$decision_table <- renderTable({
    
    result <- evaluation()
    
    
    data.frame(
      
      Security_Decision =
        names(
          result$decision_counts
        ),
      
      Number_of_Jobs =
        as.numeric(
          result$decision_counts
        )
      
    )
    
  })
  
  
  # ==========================================================
  # PRIORITY PLOT
  # ==========================================================
  
  output$priority_plot <- renderPlot({
    
    result <- evaluation()
    
    
    counts <-
      
      table(
        result$data$Priority_Class
      )
    
    
    barplot(
      
      counts,
      
      col = "purple",
      
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
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # EVALUATION DETAILS
  # ==========================================================
  
  output$evaluation_details <- renderTable({
    
    result <- evaluation()
    
    
    result$evaluation_results
    
  })
  
  
  # ==========================================================
  # EVALUATION SCORE PLOT
  # ==========================================================
  
  output$score_plot <- renderPlot({
    
    result <- evaluation()
    
    
    score <- result$evaluation_score
    
    
    plot(
      
      c(
        0,
        100
      ),
      
      c(
        0,
        1
      ),
      
      type = "n",
      
      axes = FALSE,
      
      xlab =
        "Evaluation Score (%)",
      
      ylab = "",
      
      main =
        "GatekeeperX Overall Evaluation"
      
    )
    
    
    rect(
      
      0,
      0.25,
      
      100,
      0.75,
      
      col = "lightgray",
      
      border = "black"
      
    )
    
    
    rect(
      
      0,
      0.25,
      
      score,
      0.75,
      
      col = "steelblue",
      
      border = "black"
      
    )
    
    
    axis(
      
      1,
      
      at =
        seq(
          0,
          100,
          10
        )
      
    )
    
    
    text(
      
      score,
      
      0.5,
      
      labels =
        paste(
          round(
            score,
            1
          ),
          "%"
        ),
      
      cex = 1.5,
      
      font = 2
      
    )
    
  })
  
  
  # ==========================================================
  # ARCHITECTURE
  # ==========================================================
  
  output$architecture_plot <- renderPlot({
    
    plot.new()
    
    plot.window(
      
      xlim = c(
        0,
        12
      ),
      
      ylim = c(
        0,
        10
      )
      
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
    
    
    # --------------------------------------------------------
    # TOP
    # --------------------------------------------------------
    
    draw_box(
      
      1.5,
      8,
      
      "Module 3\nSimulation"
      
    )
    
    
    draw_box(
      
      3.5,
      8,
      
      "Waiting\nTime"
      
    )
    
    
    draw_box(
      
      5.5,
      8,
      
      "Turnaround\nTime"
      
    )
    
    
    draw_box(
      
      7.5,
      8,
      
      "Queue\nMetrics"
      
    )
    
    
    draw_box(
      
      9.5,
      8,
      
      "Throughput &\nUtilization"
      
    )
    
    
    # --------------------------------------------------------
    # BOTTOM
    # --------------------------------------------------------
    
    draw_box(
      
      9.5,
      5,
      
      "Security\nDecisions"
      
    )
    
    
    draw_box(
      
      7.5,
      5,
      
      "Risk-Priority\nAlignment"
      
    )
    
    
    draw_box(
      
      5.5,
      5,
      
      "Evaluation\nCriteria"
      
    )
    
    
    draw_box(
      
      3.5,
      5,
      
      "Evaluation\nScore"
      
    )
    
    
    draw_box(
      
      1.5,
      5,
      
      "Final\nConclusion"
      
    )
    
    
    # --------------------------------------------------------
    # ARROWS
    # --------------------------------------------------------
    
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
    
    
    draw_arrow(
      9.5,
      7.5,
      9.5,
      5.5
    )
    
    
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
      
      "GatekeeperX - Module 4 Results & Evaluation",
      
      cex.main = 1.5
      
    )
    
  })
  
  
  # ==========================================================
  # DOWNLOAD METRICS
  # ==========================================================
  
  output$download_metrics <- downloadHandler(
    
    filename = function() {
      
      "GatekeeperX_Module4_Evaluation_Metrics.csv"
      
    },
    
    content = function(file) {
      
      result <- evaluation()
      
      
      metrics <- data.frame(
        
        Metric = c(
          
          "Total Jobs",
          
          "Simulation Time",
          
          "Average Waiting Time",
          
          "Maximum Waiting Time",
          
          "Average Turnaround Time",
          
          "Maximum Turnaround Time",
          
          "Average Queue Length",
          
          "Maximum Queue Length",
          
          "Server Utilization",
          
          "Throughput",
          
          "Average Priority",
          
          "Risk-Priority Correlation",
          
          "Evaluation Score",
          
          "Overall Result"
          
        ),
        
        Value = c(
          
          result$total_jobs,
          
          result$total_simulation_time,
          
          result$average_waiting_time,
          
          result$maximum_waiting_time,
          
          result$average_turnaround_time,
          
          result$maximum_turnaround_time,
          
          result$average_queue_length,
          
          result$maximum_queue_length,
          
          result$server_utilization * 100,
          
          result$throughput,
          
          result$average_priority,
          
          result$risk_priority_correlation,
          
          result$evaluation_score,
          
          result$overall_result
          
        )
        
      )
      
      
      write.csv(
        
        metrics,
        
        file,
        
        row.names = FALSE
        
      )
      
    }
    
  )
  
  
  # ==========================================================
  # DOWNLOAD REPORT
  # ==========================================================
  
  output$download_report <- downloadHandler(
    
    filename = function() {
      
      "GatekeeperX_Module4_Evaluation_Report.txt"
      
    },
    
    content = function(file) {
      
      result <- evaluation()
      
      
      sink(file)
      
      
      cat(
        "====================================================\n"
      )
      
      cat(
        "GATEKEEPERX MODULE 4\n"
      )
      
      cat(
        "RESULTS & EVALUATION REPORT\n"
      )
      
      cat(
        "====================================================\n\n"
      )
      
      
      cat(
        "Total Jobs:",
        result$total_jobs,
        "\n"
      )
      
      
      cat(
        "Simulation Time:",
        round(
          result$total_simulation_time,
          2
        ),
        "minutes\n"
      )
      
      
      cat(
        "Average Waiting Time:",
        round(
          result$average_waiting_time,
          2
        ),
        "minutes\n"
      )
      
      
      cat(
        "Maximum Waiting Time:",
        round(
          result$maximum_waiting_time,
          2
        ),
        "minutes\n"
      )
      
      
      cat(
        "Average Turnaround Time:",
        round(
          result$average_turnaround_time,
          2
        ),
        "minutes\n"
      )
      
      
      cat(
        "Average Queue Length:",
        round(
          result$average_queue_length,
          2
        ),
        "\n"
      )
      
      
      cat(
        "Maximum Queue Length:",
        result$maximum_queue_length,
        "\n"
      )
      
      
      cat(
        "Server Utilization:",
        round(
          result$server_utilization * 100,
          2
        ),
        "%\n"
      )
      
      
      cat(
        "Throughput:",
        round(
          result$throughput,
          2
        ),
        "jobs/hour\n"
      )
      
      
      cat(
        "Average Priority:",
        round(
          result$average_priority,
          2
        ),
        "\n"
      )
      
      
      cat(
        "Risk-Priority Correlation:",
        round(
          result$risk_priority_correlation,
          3
        ),
        "\n\n"
      )
      
      
      cat(
        "Evaluation Results:\n\n"
      )
      
      
      print(
        result$evaluation_results
      )
      
      
      cat(
        "\nEvaluation Score:",
        round(
          result$evaluation_score,
          2
        ),
        "%\n"
      )
      
      
      cat(
        "Overall Result:",
        result$overall_result,
        "\n"
      )
      
      
      cat(
        "\n====================================================\n"
      )
      
      
      sink()
      
    }
    
  )
  
}


# ============================================================
# RUN APPLICATION
# ============================================================

shinyApp(
  
  ui = ui,
  
  server = server
  
)