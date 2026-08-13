# ============================================================
# GATEKEEPERX
# MODULE 3 - SHINY APP
# DISCRETE EVENT QUEUE SIMULATION
# ============================================================

library(shiny)

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  titlePanel(
    "GatekeeperX - Module 3: Intelligent Queue Simulation"
  ),
  
  sidebarLayout(
    
    # ========================================================
    # SIDEBAR
    # ========================================================
    
    sidebarPanel(
      
      h4("Input Dataset"),
      
      fileInput(
        "file",
        "Upload Module 2 Modeled CSV:",
        accept = ".csv"
      ),
      
      br(),
      
      h4("Simulation Parameters"),
      
      numericInput(
        "aging_factor",
        "Aging Factor:",
        value = 0.50,
        min = 0,
        max = 2,
        step = 0.05
      ),
      
      helpText(
        "Aging increases a job's priority while it waits."
      ),
      
      br(),
      
      actionButton(
        "run_simulation",
        "Run GatekeeperX Simulation",
        class = "btn-primary"
      ),
      
      br(),
      br(),
      
      downloadButton(
        "download_results",
        "Download Simulation Results"
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
                h3(textOutput("avg_wait"))
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
                h4("Avg Queue Length"),
                h3(textOutput("avg_queue"))
              )
            ),
            
            column(
              4,
              wellPanel(
                h4("Max Queue Length"),
                h3(textOutput("max_queue"))
              )
            ),
            
            column(
              4,
              wellPanel(
                h4("Server Utilization"),
                h3(textOutput("utilization"))
              )
            )
          ),
          
          br(),
          
          h3("Simulation Metrics"),
          
          tableOutput(
            "metrics_table"
          )
        ),
        
        
        # ====================================================
        # TAB 2 - RESULTS
        # ====================================================
        
        tabPanel(
          "Simulation Results",
          
          br(),
          
          h3(
            "Simulated Security Jobs"
          ),
          
          tableOutput(
            "results_table"
          )
        ),
        
        
        # ====================================================
        # TAB 3 - WAITING TIME
        # ====================================================
        
        tabPanel(
          "Waiting Time",
          
          br(),
          
          h3(
            "Actual Waiting Time Distribution"
          ),
          
          plotOutput(
            "waiting_plot",
            height = "600px"
          )
        ),
        
        
        # ====================================================
        # TAB 4 - ARRIVAL VS COMPLETION
        # ====================================================
        
        tabPanel(
          "Arrival vs Completion",
          
          br(),
          
          h3(
            "Job Arrival and Completion Timeline"
          ),
          
          plotOutput(
            "timeline_plot",
            height = "600px"
          )
        ),
        
        
        # ====================================================
        # TAB 5 - QUEUE LENGTH
        # ====================================================
        
        tabPanel(
          "Queue Length",
          
          br(),
          
          h3(
            "Queue Length During Simulation"
          ),
          
          plotOutput(
            "queue_plot",
            height = "600px"
          )
        ),
        
        
        # ====================================================
        # TAB 6 - DYNAMIC PRIORITY
        # ====================================================
        
        tabPanel(
          "Dynamic Priority",
          
          br(),
          
          h3(
            "Dynamic Priority at Service Selection"
          ),
          
          plotOutput(
            "priority_plot",
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
            "Security Gate Decisions"
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
        # TAB 8 - ARCHITECTURE
        # ====================================================
        
        tabPanel(
          "Architecture",
          
          br(),
          
          h2(
            "GatekeeperX Module 3 Simulation Architecture"
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
  # LOAD DATASET
  # ==========================================================
  
  raw_data <- eventReactive(
    
    input$run_simulation,
    
    {
      
      req(input$file)
      
      read.csv(
        input$file$datapath,
        stringsAsFactors = FALSE
      )
      
    }
    
  )
  
  
  # ==========================================================
  # RUN SIMULATION
  # ==========================================================
  
  simulation <- eventReactive(
    
    input$run_simulation,
    
    {
      
      data <- raw_data()
      
      
      # ======================================================
      # VALIDATE COLUMNS
      # ======================================================
      
      required_columns <- c(
        
        "Job_ID",
        "Pipeline_ID",
        "Scan_Type",
        "Severity",
        "Vulnerability_Count",
        "Arrival_Time_Min",
        "Service_Time_Min",
        "Risk_Score",
        "GatekeeperX_Priority_Score",
        "Priority_Class",
        "Security_Decision"
        
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
      # SORT BY ARRIVAL
      # ======================================================
      
      data <- data[
        order(
          data$Arrival_Time_Min
        ),
      ]
      
      row.names(data) <- NULL
      
      
      # ======================================================
      # NUMBER OF JOBS
      # ======================================================
      
      n <- nrow(data)
      
      
      # ======================================================
      # CREATE RESULT COLUMNS
      # ======================================================
      
      data$Service_Start_Time <- NA
      
      data$Completion_Time <- NA
      
      data$Actual_Waiting_Time <- NA
      
      data$Turnaround_Time <- NA
      
      data$Dynamic_Priority <- NA
      
      data$Simulation_Queue_Position <- NA
      
      data$Simulation_Status <- "Waiting"
      
      
      # ======================================================
      # SIMULATION VARIABLES
      # ======================================================
      
      current_time <- 0
      
      next_arrival_index <- 1
      
      waiting_queue <- integer(0)
      
      completed_jobs <- integer(0)
      
      queue_history_time <- c()
      
      queue_history_length <- c()
      
      event_number <- 0
      
      
      # ======================================================
      # MAIN SIMULATION LOOP
      # ======================================================
      
      while (
        length(completed_jobs) < n
      ) {
        
        
        # ----------------------------------------------------
        # ADD ARRIVING JOBS
        # ----------------------------------------------------
        
        if (
          
          next_arrival_index <= n &&
          
          data$Arrival_Time_Min[
            next_arrival_index
          ] <= current_time
          
        ) {
          
          while (
            
            next_arrival_index <= n &&
            
            data$Arrival_Time_Min[
              next_arrival_index
            ] <= current_time
            
          ) {
            
            waiting_queue <- c(
              waiting_queue,
              next_arrival_index
            )
            
            data$Simulation_Status[
              next_arrival_index
            ] <- "Queued"
            
            next_arrival_index <-
              next_arrival_index + 1
            
          }
          
        }
        
        
        # ----------------------------------------------------
        # EMPTY QUEUE
        # ----------------------------------------------------
        
        if (
          length(waiting_queue) == 0
        ) {
          
          if (
            next_arrival_index <= n
          ) {
            
            current_time <-
              data$Arrival_Time_Min[
                next_arrival_index
              ]
            
            next
            
          }
          
        }
        
        
        # ----------------------------------------------------
        # DYNAMIC PRIORITY
        # ----------------------------------------------------
        
        if (
          length(waiting_queue) > 0
        ) {
          
          dynamic_scores <- numeric(
            length(waiting_queue)
          )
          
          
          for (
            j in seq_along(waiting_queue)
          ) {
            
            index <- waiting_queue[j]
            
            
            # Actual waiting time
            
            current_wait <- max(
              
              0,
              
              current_time -
                data$Arrival_Time_Min[
                  index
                ]
              
            )
            
            
            # Aging bonus
            
            aging_bonus <-
              
              current_wait *
              input$aging_factor
            
            
            # Dynamic priority
            
            dynamic_scores[j] <-
              
              min(
                
                100,
                
                data$GatekeeperX_Priority_Score[
                  index
                ] +
                  aging_bonus
                
              )
            
          }
          
          
          # --------------------------------------------------
          # SELECT HIGHEST PRIORITY
          # --------------------------------------------------
          
          selected_position <-
            
            which.max(
              dynamic_scores
            )
          
          
          selected_job <-
            
            waiting_queue[
              selected_position
            ]
          
          
          selected_priority <-
            
            dynamic_scores[
              selected_position
            ]
          
          
          # --------------------------------------------------
          # REMOVE SELECTED JOB
          # --------------------------------------------------
          
          waiting_queue <-
            
            waiting_queue[
              -selected_position
            ]
          
          
          # --------------------------------------------------
          # QUEUE POSITION
          # --------------------------------------------------
          
          event_number <-
            event_number + 1
          
          
          data$Simulation_Queue_Position[
            selected_job
          ] <- event_number
          
          
          # --------------------------------------------------
          # SERVICE START
          # --------------------------------------------------
          
          service_start <- max(
            
            current_time,
            
            data$Arrival_Time_Min[
              selected_job
            ]
            
          )
          
          
          data$Service_Start_Time[
            selected_job
          ] <- service_start
          
          
          # --------------------------------------------------
          # WAITING TIME
          # --------------------------------------------------
          
          actual_wait <-
            
            service_start -
            data$Arrival_Time_Min[
              selected_job
            ]
          
          
          data$Actual_Waiting_Time[
            selected_job
          ] <- actual_wait
          
          
          # --------------------------------------------------
          # DYNAMIC PRIORITY
          # --------------------------------------------------
          
          data$Dynamic_Priority[
            selected_job
          ] <- round(
            
            selected_priority,
            
            2
            
          )
          
          
          # --------------------------------------------------
          # SERVICE TIME
          # --------------------------------------------------
          
          service_time <-
            
            data$Service_Time_Min[
              selected_job
            ]
          
          
          # --------------------------------------------------
          # COMPLETION
          # --------------------------------------------------
          
          completion_time <-
            
            service_start +
            service_time
          
          
          data$Completion_Time[
            selected_job
          ] <- completion_time
          
          
          # --------------------------------------------------
          # TURNAROUND
          # --------------------------------------------------
          
          data$Turnaround_Time[
            selected_job
          ] <-
            
            completion_time -
            data$Arrival_Time_Min[
              selected_job
            ]
          
          
          # --------------------------------------------------
          # STATUS
          # --------------------------------------------------
          
          data$Simulation_Status[
            selected_job
          ] <- "Completed"
          
          
          # --------------------------------------------------
          # COMPLETED JOB
          # --------------------------------------------------
          
          completed_jobs <-
            
            c(
              completed_jobs,
              selected_job
            )
          
          
          # --------------------------------------------------
          # MOVE CLOCK
          # --------------------------------------------------
          
          current_time <-
            completion_time
          
          
          # --------------------------------------------------
          # QUEUE HISTORY
          # --------------------------------------------------
          
          queue_history_time <-
            
            c(
              queue_history_time,
              current_time
            )
          
          
          queue_history_length <-
            
            c(
              queue_history_length,
              length(waiting_queue)
            )
          
        }
        
      }
      
      
      # ======================================================
      # ROUND VALUES
      # ======================================================
      
      data$Service_Start_Time <-
        
        round(
          data$Service_Start_Time,
          2
        )
      
      
      data$Completion_Time <-
        
        round(
          data$Completion_Time,
          2
        )
      
      
      data$Actual_Waiting_Time <-
        
        round(
          data$Actual_Waiting_Time,
          2
        )
      
      
      data$Turnaround_Time <-
        
        round(
          data$Turnaround_Time,
          2
        )
      
      
      data$Dynamic_Priority <-
        
        round(
          data$Dynamic_Priority,
          2
        )
      
      
      # ======================================================
      # METRICS
      # ======================================================
      
      total_simulation_time <-
        
        max(
          data$Completion_Time
        )
      
      
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
      
      
      average_turnaround_time <-
        
        mean(
          data$Turnaround_Time
        )
      
      
      maximum_turnaround_time <-
        
        max(
          data$Turnaround_Time
        )
      
      
      average_service_time <-
        
        mean(
          data$Service_Time_Min
        )
      
      
      total_service_time <-
        
        sum(
          data$Service_Time_Min
        )
      
      
      server_utilization <-
        
        total_service_time /
        total_simulation_time
      
      
      throughput <-
        
        n /
        total_simulation_time
      
      
      throughput_per_hour <-
        
        throughput * 60
      
      
      average_queue_length <-
        
        mean(
          queue_history_length
        )
      
      
      maximum_queue_length <-
        
        max(
          queue_history_length
        )
      
      
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
      # RETURN ALL RESULTS
      # ======================================================
      
      list(
        
        data = data,
        
        queue_time =
          queue_history_time,
        
        queue_length =
          queue_history_length,
        
        total_simulation_time =
          total_simulation_time,
        
        average_waiting_time =
          average_waiting_time,
        
        minimum_waiting_time =
          minimum_waiting_time,
        
        maximum_waiting_time =
          maximum_waiting_time,
        
        average_turnaround_time =
          average_turnaround_time,
        
        maximum_turnaround_time =
          maximum_turnaround_time,
        
        average_service_time =
          average_service_time,
        
        server_utilization =
          server_utilization,
        
        throughput_per_hour =
          throughput_per_hour,
        
        average_queue_length =
          average_queue_length,
        
        maximum_queue_length =
          maximum_queue_length,
        
        blocked_jobs =
          blocked_jobs,
        
        prioritized_jobs =
          prioritized_jobs,
        
        passed_jobs =
          passed_jobs,
        
        monitored_jobs =
          monitored_jobs,
        
        decision_counts =
          decision_counts
        
      )
      
    }
    
  )
  
  
  # ==========================================================
  # DASHBOARD OUTPUTS
  # ==========================================================
  
  output$total_jobs <- renderText({
    
    req(simulation())
    
    nrow(
      simulation()$data
    )
    
  })
  
  
  output$avg_wait <- renderText({
    
    req(simulation())
    
    paste(
      round(
        simulation()$average_waiting_time,
        2
      ),
      "min"
    )
    
  })
  
  
  output$avg_turnaround <- renderText({
    
    req(simulation())
    
    paste(
      round(
        simulation()$average_turnaround_time,
        2
      ),
      "min"
    )
    
  })
  
  
  output$throughput <- renderText({
    
    req(simulation())
    
    paste(
      round(
        simulation()$throughput_per_hour,
        2
      ),
      "jobs/hr"
    )
    
  })
  
  
  output$avg_queue <- renderText({
    
    req(simulation())
    
    round(
      simulation()$average_queue_length,
      2
    )
    
  })
  
  
  output$max_queue <- renderText({
    
    req(simulation())
    
    simulation()$maximum_queue_length
    
  })
  
  
  output$utilization <- renderText({
    
    req(simulation())
    
    paste(
      round(
        simulation()$server_utilization * 100,
        2
      ),
      "%"
    )
    
  })
  
  
  # ==========================================================
  # METRICS TABLE
  # ==========================================================
  
  output$metrics_table <- renderTable({
    
    result <- simulation()
    
    
    data.frame(
      
      Metric = c(
        
        "Total Jobs",
        
        "Total Simulation Time",
        
        "Average Waiting Time",
        
        "Minimum Waiting Time",
        
        "Maximum Waiting Time",
        
        "Average Turnaround Time",
        
        "Maximum Turnaround Time",
        
        "Average Service Time",
        
        "Average Queue Length",
        
        "Maximum Queue Length",
        
        "Server Utilization",
        
        "Throughput"
        
      ),
      
      Value = c(
        
        nrow(result$data),
        
        paste(
          round(
            result$total_simulation_time,
            2
          ),
          "minutes"
        ),
        
        paste(
          round(
            result$average_waiting_time,
            2
          ),
          "minutes"
        ),
        
        paste(
          round(
            result$minimum_waiting_time,
            2
          ),
          "minutes"
        ),
        
        paste(
          round(
            result$maximum_waiting_time,
            2
          ),
          "minutes"
        ),
        
        paste(
          round(
            result$average_turnaround_time,
            2
          ),
          "minutes"
        ),
        
        paste(
          round(
            result$maximum_turnaround_time,
            2
          ),
          "minutes"
        ),
        
        paste(
          round(
            result$average_service_time,
            2
          ),
          "minutes"
        ),
        
        round(
          result$average_queue_length,
          2
        ),
        
        result$maximum_queue_length,
        
        paste(
          round(
            result$server_utilization * 100,
            2
          ),
          "%"
        ),
        
        paste(
          round(
            result$throughput_per_hour,
            2
          ),
          "jobs/hr"
        )
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # SIMULATION RESULTS TABLE
  # ==========================================================
  
  output$results_table <- renderTable({
    
    result <- simulation()
    
    head(
      result$data,
      50
    )
    
  })
  
  
  # ==========================================================
  # PLOT 1 - WAITING TIME
  # ==========================================================
  
  output$waiting_plot <- renderPlot({
    
    result <- simulation()
    
    hist(
      
      result$data$Actual_Waiting_Time,
      
      breaks = 25,
      
      col = "steelblue",
      
      border = "white",
      
      main =
        "GatekeeperX - Actual Waiting Time Distribution",
      
      xlab =
        "Waiting Time (minutes)",
      
      ylab =
        "Number of Jobs"
      
    )
    
    
    abline(
      
      v =
        result$average_waiting_time,
      
      lwd = 3,
      
      lty = 2
      
    )
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # PLOT 2 - ARRIVAL VS COMPLETION
  # ==========================================================
  
  output$timeline_plot <- renderPlot({
    
    result <- simulation()
    
    data <- result$data
    
    
    plot(
      
      data$Job_ID,
      
      data$Arrival_Time_Min,
      
      type = "l",
      
      col = "darkgreen",
      
      lwd = 2,
      
      main =
        "GatekeeperX - Arrival vs Completion Time",
      
      xlab =
        "Job ID",
      
      ylab =
        "Time (minutes)"
      
    )
    
    
    lines(
      
      data$Job_ID,
      
      data$Completion_Time,
      
      col = "red",
      
      lwd = 2
      
    )
    
    
    legend(
      
      "topleft",
      
      legend = c(
        "Arrival Time",
        "Completion Time"
      ),
      
      col = c(
        "darkgreen",
        "red"
      ),
      
      lwd = 2
      
    )
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # PLOT 3 - QUEUE LENGTH
  # ==========================================================
  
  output$queue_plot <- renderPlot({
    
    result <- simulation()
    
    
    plot(
      
      result$queue_time,
      
      result$queue_length,
      
      type = "s",
      
      lwd = 2,
      
      col = "purple",
      
      main =
        "GatekeeperX - Queue Length During Simulation",
      
      xlab =
        "Simulation Time (minutes)",
      
      ylab =
        "Number of Jobs in Queue"
      
    )
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # PLOT 4 - DYNAMIC PRIORITY
  # ==========================================================
  
  output$priority_plot <- renderPlot({
    
    result <- simulation()
    
    data <- result$data
    
    
    plot(
      
      data$Job_ID,
      
      data$Dynamic_Priority,
      
      pch = 19,
      
      col = "orange",
      
      main =
        "GatekeeperX - Dynamic Priority",
      
      xlab =
        "Job ID",
      
      ylab =
        "Dynamic Priority Score"
      
    )
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # SECURITY DECISION PLOT
  # ==========================================================
  
  output$decision_plot <- renderPlot({
    
    result <- simulation()
    
    
    counts <-
      
      result$decision_counts
    
    
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
    
    result <- simulation()
    
    
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
  # ARCHITECTURE DIAGRAM
  # ==========================================================
  
  output$architecture_plot <- renderPlot({
    
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
    
    
    # --------------------------------------------------------
    # TOP
    # --------------------------------------------------------
    
    draw_box(
      
      1.5,
      8,
      
      "Module 2\nModeled Data"
      
    )
    
    
    draw_box(
      
      3.5,
      8,
      
      "Job\nArrivals"
      
    )
    
    
    draw_box(
      
      5.5,
      8,
      
      "Security\nQueue"
      
    )
    
    
    draw_box(
      
      7.5,
      8,
      
      "Dynamic\nPriority"
      
    )
    
    
    draw_box(
      
      9.5,
      8,
      
      "Job\nSelection"
      
    )
    
    
    # --------------------------------------------------------
    # BOTTOM
    # --------------------------------------------------------
    
    draw_box(
      
      9.5,
      5,
      
      "Security\nScanner"
      
    )
    
    
    draw_box(
      
      7.5,
      5,
      
      "Service\nCompletion"
      
    )
    
    
    draw_box(
      
      5.5,
      5,
      
      "Waiting &\nTurnaround"
      
    )
    
    
    draw_box(
      
      3.5,
      5,
      
      "Queue\nMetrics"
      
    )
    
    
    draw_box(
      
      1.5,
      5,
      
      "Simulation\nResults"
      
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
      
      "GatekeeperX - Module 3 Simulation Architecture",
      
      cex.main = 1.5
      
    )
    
  })
  
  
  # ==========================================================
  # DOWNLOAD RESULTS
  # ==========================================================
  
  output$download_results <- downloadHandler(
    
    filename = function() {
      
      "GatekeeperX_Module3_Simulation_Results.csv"
      
    },
    
    content = function(file) {
      
      write.csv(
        
        simulation()$data,
        
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