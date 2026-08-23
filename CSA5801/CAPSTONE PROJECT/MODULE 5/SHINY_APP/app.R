# ============================================================
# GATEKEEPERX
# MODULE 5 - SHINY APP
# COMPARISON WITH EXISTING APPROACHES
# ============================================================

library(shiny)


# ============================================================
# QUEUE SIMULATION FUNCTION
# ============================================================

simulate_queue <- function(input_data,
                           method,
                           aging_factor = 0.50) {
  
  data <- input_data
  
  data <- data[
    order(data$Arrival_Time_Min),
  ]
  
  row.names(data) <- NULL
  
  n <- nrow(data)
  
  data$Service_Start_Time <- NA
  data$Completion_Time <- NA
  data$Waiting_Time <- NA
  data$Turnaround_Time <- NA
  data$Queue_Position <- NA
  data$Selection_Priority <- NA
  
  current_time <- 0
  
  next_arrival <- 1
  
  waiting_queue <- integer(0)
  
  completed <- integer(0)
  
  queue_event <- 0
  
  
  # ==========================================================
  # MAIN SIMULATION
  # ==========================================================
  
  while (length(completed) < n) {
    
    # --------------------------------------------------------
    # ADD ARRIVING JOBS
    # --------------------------------------------------------
    
    if (
      next_arrival <= n &&
      data$Arrival_Time_Min[next_arrival] <= current_time
    ) {
      
      while (
        next_arrival <= n &&
        data$Arrival_Time_Min[next_arrival] <= current_time
      ) {
        
        waiting_queue <- c(
          waiting_queue,
          next_arrival
        )
        
        next_arrival <- next_arrival + 1
      }
    }
    
    
    # --------------------------------------------------------
    # IF QUEUE EMPTY, MOVE CLOCK
    # --------------------------------------------------------
    
    if (length(waiting_queue) == 0) {
      
      if (next_arrival <= n) {
        
        current_time <-
          data$Arrival_Time_Min[next_arrival]
        
        next
        
      }
    }
    
    
    # --------------------------------------------------------
    # SELECT JOB
    # --------------------------------------------------------
    
    if (length(waiting_queue) > 0) {
      
      # ======================================================
      # FCFS
      # ======================================================
      
      if (method == "FCFS") {
        
        selected_position <- 1
        
        selected_priority <-
          data$GatekeeperX_Priority_Score[
            waiting_queue[selected_position]
          ]
        
      }
      
      
      # ======================================================
      # STATIC PRIORITY
      # ======================================================
      
      else if (method == "Static Priority") {
        
        priorities <-
          data$GatekeeperX_Priority_Score[
            waiting_queue
          ]
        
        selected_position <-
          which.max(priorities)
        
        selected_priority <-
          priorities[selected_position]
        
      }
      
      
      # ======================================================
      # GATEKEEPERX
      # ======================================================
      
      else if (method == "GatekeeperX") {
        
        dynamic_scores <- numeric(
          length(waiting_queue)
        )
        
        for (j in seq_along(waiting_queue)) {
          
          index <- waiting_queue[j]
          
          waiting_time <- max(
            0,
            current_time -
              data$Arrival_Time_Min[index]
          )
          
          aging_bonus <-
            waiting_time * aging_factor
          
          dynamic_scores[j] <-
            min(
              100,
              data$GatekeeperX_Priority_Score[index] +
                aging_bonus
            )
        }
        
        selected_position <-
          which.max(dynamic_scores)
        
        selected_priority <-
          dynamic_scores[selected_position]
      }
      
      
      # ------------------------------------------------------
      # SELECTED JOB
      # ------------------------------------------------------
      
      selected_job <-
        waiting_queue[selected_position]
      
      
      # ------------------------------------------------------
      # REMOVE FROM QUEUE
      # ------------------------------------------------------
      
      waiting_queue <-
        waiting_queue[-selected_position]
      
      
      # ------------------------------------------------------
      # QUEUE POSITION
      # ------------------------------------------------------
      
      queue_event <- queue_event + 1
      
      data$Queue_Position[selected_job] <-
        queue_event
      
      
      # ------------------------------------------------------
      # SERVICE START
      # ------------------------------------------------------
      
      service_start <- max(
        current_time,
        data$Arrival_Time_Min[selected_job]
      )
      
      data$Service_Start_Time[selected_job] <-
        service_start
      
      
      # ------------------------------------------------------
      # WAITING TIME
      # ------------------------------------------------------
      
      data$Waiting_Time[selected_job] <-
        
        service_start -
        data$Arrival_Time_Min[selected_job]
      
      
      # ------------------------------------------------------
      # PRIORITY USED
      # ------------------------------------------------------
      
      data$Selection_Priority[selected_job] <-
        selected_priority
      
      
      # ------------------------------------------------------
      # SERVICE
      # ------------------------------------------------------
      
      service_time <-
        data$Service_Time_Min[selected_job]
      
      
      # ------------------------------------------------------
      # COMPLETION
      # ------------------------------------------------------
      
      completion <-
        service_start + service_time
      
      data$Completion_Time[selected_job] <-
        completion
      
      
      # ------------------------------------------------------
      # TURNAROUND
      # ------------------------------------------------------
      
      data$Turnaround_Time[selected_job] <-
        
        completion -
        data$Arrival_Time_Min[selected_job]
      
      
      # ------------------------------------------------------
      # COMPLETED
      # ------------------------------------------------------
      
      completed <- c(
        completed,
        selected_job
      )
      
      
      # ------------------------------------------------------
      # MOVE CLOCK
      # ------------------------------------------------------
      
      current_time <- completion
    }
  }
  
  
  # ==========================================================
  # METRICS
  # ==========================================================
  
  total_time <-
    max(data$Completion_Time)
  
  
  total_service <-
    sum(data$Service_Time_Min)
  
  
  average_waiting <-
    mean(data$Waiting_Time)
  
  
  maximum_waiting <-
    max(data$Waiting_Time)
  
  
  average_turnaround <-
    mean(data$Turnaround_Time)
  
  
  maximum_turnaround <-
    max(data$Turnaround_Time)
  
  
  utilization <-
    total_service / total_time
  
  
  throughput <-
    n / total_time * 60
  
  
  # ==========================================================
  # QUEUE LENGTH
  # ==========================================================
  
  queue_lengths <- numeric(n)
  
  for (i in 1:n) {
    
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
  
  
  average_queue <-
    mean(queue_lengths)
  
  
  maximum_queue <-
    max(queue_lengths)
  
  
  # ==========================================================
  # CRITICAL JOB WAITING
  # ==========================================================
  
  critical_data <-
    data[
      data$Severity == "Critical",
    ]
  
  
  if (nrow(critical_data) > 0) {
    
    critical_waiting <-
      mean(critical_data$Waiting_Time)
    
  } else {
    
    critical_waiting <- 0
    
  }
  
  
  # ==========================================================
  # HIGH JOB WAITING
  # ==========================================================
  
  high_data <-
    data[
      data$Severity == "High",
    ]
  
  
  if (nrow(high_data) > 0) {
    
    high_waiting <-
      mean(high_data$Waiting_Time)
    
  } else {
    
    high_waiting <- 0
    
  }
  
  
  # ==========================================================
  # RETURN
  # ==========================================================
  
  list(
    
    data = data,
    
    method = method,
    
    average_waiting = average_waiting,
    
    maximum_waiting = maximum_waiting,
    
    average_turnaround = average_turnaround,
    
    maximum_turnaround = maximum_turnaround,
    
    average_queue = average_queue,
    
    maximum_queue = maximum_queue,
    
    throughput = throughput,
    
    utilization = utilization,
    
    critical_waiting = critical_waiting,
    
    high_waiting = high_waiting
    
  )
}


# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  titlePanel(
    "GatekeeperX - Module 5: Comparative Evaluation"
  ),
  
  sidebarLayout(
    
    # ========================================================
    # SIDEBAR
    # ========================================================
    
    sidebarPanel(
      
      h4("Module 3 Dataset"),
      
      fileInput(
        "file",
        "Upload Module 3 CSV:",
        accept = ".csv"
      ),
      
      br(),
      
      numericInput(
        "aging_factor",
        "GatekeeperX Aging Factor:",
        value = 0.50,
        min = 0,
        max = 2,
        step = 0.05
      ),
      
      helpText(
        "Higher values increase priority faster while a job waits."
      ),
      
      br(),
      
      actionButton(
        "run",
        "Run Comparison",
        class = "btn-primary"
      ),
      
      br(),
      br(),
      
      downloadButton(
        "download_comparison",
        "Download Comparison CSV"
      ),
      
      br(),
      br(),
      
      downloadButton(
        "download_report",
        "Download Report"
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
                h4("Best Approach"),
                h3(textOutput("winner"))
              )
            ),
            
            column(
              3,
              wellPanel(
                h4("GatekeeperX Score"),
                h3(textOutput("gatekeeper_score"))
              )
            ),
            
            column(
              3,
              wellPanel(
                h4("vs FCFS"),
                h3(textOutput("improvement_fcfs"))
              )
            ),
            
            column(
              3,
              wellPanel(
                h4("vs Static"),
                h3(textOutput("improvement_static"))
              )
            )
            
          ),
          
          br(),
          
          h3(
            "Performance Comparison"
          ),
          
          tableOutput(
            "comparison_table"
          )
          
        ),
        
        
        # ====================================================
        # TAB 2 - WAITING TIME
        # ====================================================
        
        tabPanel(
          
          "Waiting Time",
          
          br(),
          
          h3(
            "Average Waiting Time Comparison"
          ),
          
          plotOutput(
            "waiting_plot",
            height = "600px"
          ),
          
          br(),
          
          h3(
            "Maximum Waiting Time"
          ),
          
          plotOutput(
            "max_waiting_plot",
            height = "500px"
          )
          
        ),
        
        
        # ====================================================
        # TAB 3 - TURNAROUND
        # ====================================================
        
        tabPanel(
          
          "Turnaround Time",
          
          br(),
          
          h3(
            "Average Turnaround Time"
          ),
          
          plotOutput(
            "turnaround_plot",
            height = "600px"
          )
          
        ),
        
        
        # ====================================================
        # TAB 4 - QUEUE
        # ====================================================
        
        tabPanel(
          
          "Queue Length",
          
          br(),
          
          h3(
            "Average Queue Length"
          ),
          
          plotOutput(
            "queue_plot",
            height = "600px"
          )
          
        ),
        
        
        # ====================================================
        # TAB 5 - THROUGHPUT
        # ====================================================
        
        tabPanel(
          
          "Throughput",
          
          br(),
          
          h3(
            "Throughput Comparison"
          ),
          
          plotOutput(
            "throughput_plot",
            height = "600px"
          )
          
        ),
        
        
        # ====================================================
        # TAB 6 - SECURITY
        # ====================================================
        
        tabPanel(
          
          "Security Performance",
          
          br(),
          
          h3(
            "Critical Job Waiting Time"
          ),
          
          plotOutput(
            "critical_plot",
            height = "600px"
          ),
          
          br(),
          
          h3(
            "Security Performance Table"
          ),
          
          tableOutput(
            "security_table"
          )
          
        ),
        
        
        # ====================================================
        # TAB 7 - MULTI METRIC
        # ====================================================
        
        tabPanel(
          
          "Overall Comparison",
          
          br(),
          
          h3(
            "Normalized Multi-Metric Comparison"
          ),
          
          plotOutput(
            "normalized_plot",
            height = "650px"
          ),
          
          br(),
          
          h3(
            "Improvement Analysis"
          ),
          
          tableOutput(
            "improvement_table"
          )
          
        ),
        
        
        # ====================================================
        # TAB 8 - ARCHITECTURE
        # ====================================================
        
        tabPanel(
          
          "Architecture",
          
          br(),
          
          h2(
            "Module 5 Comparison Architecture"
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
  
  input_data <- eventReactive(
    
    input$run,
    
    {
      
      req(input$file)
      
      data <- read.csv(
        
        input$file$datapath,
        
        stringsAsFactors = FALSE
        
      )
      
      
      required_columns <- c(
        
        "Job_ID",
        "Severity",
        "GatekeeperX_Priority_Score",
        "Service_Time_Min",
        "Arrival_Time_Min"
        
      )
      
      
      missing_columns <-
        
        setdiff(
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
      
      
      data
      
    }
    
  )
  
  
  # ==========================================================
  # RUN ALL THREE APPROACHES
  # ==========================================================
  
  comparison_results <- eventReactive(
    
    input$run,
    
    {
      
      data <- input_data()
      
      req(data)
      
      
      withProgress(
        
        message =
          "Running queue simulations...",
        
        value = 0,
        
        {
          
          incProgress(
            0.20,
            detail = "Running FCFS"
          )
          
          
          fcfs <-
            
            simulate_queue(
              
              data,
              
              "FCFS",
              
              input$aging_factor
              
            )
          
          
          incProgress(
            0.30,
            detail = "Running Static Priority"
          )
          
          
          static <-
            
            simulate_queue(
              
              data,
              
              "Static Priority",
              
              input$aging_factor
              
            )
          
          
          incProgress(
            0.50,
            detail = "Running GatekeeperX"
          )
          
          
          gatekeeper <-
            
            simulate_queue(
              
              data,
              
              "GatekeeperX",
              
              input$aging_factor
              
            )
          
          
          incProgress(
            1,
            detail = "Comparison completed"
          )
          
          
          # --------------------------------------------------
          # COMPARISON TABLE
          # --------------------------------------------------
          
          comparison <- data.frame(
            
            Approach = c(
              "FCFS",
              "Static Priority",
              "GatekeeperX"
            ),
            
            Average_Waiting_Time = c(
              
              fcfs$average_waiting,
              
              static$average_waiting,
              
              gatekeeper$average_waiting
              
            ),
            
            Maximum_Waiting_Time = c(
              
              fcfs$maximum_waiting,
              
              static$maximum_waiting,
              
              gatekeeper$maximum_waiting
              
            ),
            
            Average_Turnaround_Time = c(
              
              fcfs$average_turnaround,
              
              static$average_turnaround,
              
              gatekeeper$average_turnaround
              
            ),
            
            Maximum_Turnaround_Time = c(
              
              fcfs$maximum_turnaround,
              
              static$maximum_turnaround,
              
              gatekeeper$maximum_turnaround
              
            ),
            
            Average_Queue_Length = c(
              
              fcfs$average_queue,
              
              static$average_queue,
              
              gatekeeper$average_queue
              
            ),
            
            Maximum_Queue_Length = c(
              
              fcfs$maximum_queue,
              
              static$maximum_queue,
              
              gatekeeper$maximum_queue
              
            ),
            
            Throughput_Jobs_Per_Hour = c(
              
              fcfs$throughput,
              
              static$throughput,
              
              gatekeeper$throughput
              
            ),
            
            Server_Utilization_Percent = c(
              
              fcfs$utilization * 100,
              
              static$utilization * 100,
              
              gatekeeper$utilization * 100
              
            ),
            
            Critical_Average_Waiting = c(
              
              fcfs$critical_waiting,
              
              static$critical_waiting,
              
              gatekeeper$critical_waiting
              
            ),
            
            High_Average_Waiting = c(
              
              fcfs$high_waiting,
              
              static$high_waiting,
              
              gatekeeper$high_waiting
              
            )
            
          )
          
          
          # --------------------------------------------------
          # ROUND VALUES
          # --------------------------------------------------
          
          comparison[, -1] <-
            
            round(
              comparison[, -1],
              2
            )
          
          
          # --------------------------------------------------
          # WAITING IMPROVEMENT
          # --------------------------------------------------
          
          fcfs_wait <-
            fcfs$average_waiting
          
          static_wait <-
            static$average_waiting
          
          gatekeeper_wait <-
            gatekeeper$average_waiting
          
          
          if (
            fcfs_wait != 0
          ) {
            
            improvement_fcfs <-
              
              (
                fcfs_wait -
                  gatekeeper_wait
              ) /
              
              fcfs_wait *
              
              100
            
          } else {
            
            improvement_fcfs <- 0
            
          }
          
          
          if (
            static_wait != 0
          ) {
            
            improvement_static <-
              
              (
                static_wait -
                  gatekeeper_wait
              ) /
              
              static_wait *
              
              100
            
          } else {
            
            improvement_static <- 0
            
          }
          
          
          # --------------------------------------------------
          # CRITICAL IMPROVEMENT
          # --------------------------------------------------
          
          critical_fcfs <-
            fcfs$critical_waiting
          
          critical_static <-
            static$critical_waiting
          
          critical_gatekeeper <-
            gatekeeper$critical_waiting
          
          
          if (
            critical_fcfs != 0
          ) {
            
            critical_improvement_fcfs <-
              
              (
                critical_fcfs -
                  critical_gatekeeper
              ) /
              
              critical_fcfs *
              
              100
            
          } else {
            
            critical_improvement_fcfs <- 0
            
          }
          
          
          if (
            critical_static != 0
          ) {
            
            critical_improvement_static <-
              
              (
                critical_static -
                  critical_gatekeeper
              ) /
              
              critical_static *
              
              100
            
          } else {
            
            critical_improvement_static <- 0
            
          }
          
          
          # --------------------------------------------------
          # WINNER
          # --------------------------------------------------
          
          waiting_rank <-
            
            rank(
              comparison$Average_Waiting_Time
            )
          
          
          turnaround_rank <-
            
            rank(
              comparison$Average_Turnaround_Time
            )
          
          
          queue_rank <-
            
            rank(
              comparison$Average_Queue_Length
            )
          
          
          throughput_rank <-
            
            rank(
              -comparison$Throughput_Jobs_Per_Hour
            )
          
          
          comparison$Overall_Rank <-
            
            waiting_rank +
            turnaround_rank +
            queue_rank +
            throughput_rank
          
          
          winner_index <-
            
            which.min(
              comparison$Overall_Rank
            )
          
          
          winner <-
            
            comparison$Approach[
              winner_index
            ]
          
          
          # --------------------------------------------------
          # GATEKEEPERX SCORE
          # --------------------------------------------------
          
          score_waiting <-
            
            gatekeeper$average_waiting <=
            
            min(
              fcfs$average_waiting,
              static$average_waiting
            )
          
          
          score_turnaround <-
            
            gatekeeper$average_turnaround <=
            
            min(
              fcfs$average_turnaround,
              static$average_turnaround
            )
          
          
          score_queue <-
            
            gatekeeper$average_queue <=
            
            min(
              fcfs$average_queue,
              static$average_queue
            )
          
          
          score_throughput <-
            
            gatekeeper$throughput >=
            
            max(
              fcfs$throughput,
              static$throughput
            )
          
          
          score_critical <-
            
            gatekeeper$critical_waiting <=
            
            min(
              fcfs$critical_waiting,
              static$critical_waiting
            )
          
          
          score_vector <- c(
            
            score_waiting,
            
            score_turnaround,
            
            score_queue,
            
            score_throughput,
            
            score_critical
            
          )
          
          
          gatekeeper_score <-
            
            mean(score_vector) * 100
          
          
          # --------------------------------------------------
          # NORMALIZED DATA
          # --------------------------------------------------
          
          normalized <- comparison
          
          
          normalized$Waiting <-
            
            normalized$Average_Waiting_Time /
            max(
              normalized$Average_Waiting_Time
            )
          
          
          normalized$Turnaround <-
            
            normalized$Average_Turnaround_Time /
            max(
              normalized$Average_Turnaround_Time
            )
          
          
          normalized$Queue <-
            
            normalized$Average_Queue_Length /
            max(
              normalized$Average_Queue_Length
            )
          
          
          normalized$Throughput <-
            
            normalized$Throughput_Jobs_Per_Hour /
            max(
              normalized$Throughput_Jobs_Per_Hour
            )
          
          
          # --------------------------------------------------
          # RETURN
          # --------------------------------------------------
          
          list(
            
            fcfs = fcfs,
            
            static = static,
            
            gatekeeper = gatekeeper,
            
            comparison = comparison,
            
            normalized = normalized,
            
            winner = winner,
            
            gatekeeper_score =
              gatekeeper_score,
            
            improvement_fcfs =
              improvement_fcfs,
            
            improvement_static =
              improvement_static,
            
            critical_improvement_fcfs =
              critical_improvement_fcfs,
            
            critical_improvement_static =
              critical_improvement_static
            
          )
          
        }
        
      )
      
    }
    
  )
  
  
  # ==========================================================
  # DASHBOARD
  # ==========================================================
  
  output$winner <- renderText({
    
    req(comparison_results())
    
    comparison_results()$winner
    
  })
  
  
  output$gatekeeper_score <- renderText({
    
    req(comparison_results())
    
    paste0(
      
      round(
        comparison_results()$gatekeeper_score,
        1
      ),
      
      "%"
      
    )
    
  })
  
  
  output$improvement_fcfs <- renderText({
    
    req(comparison_results())
    
    paste0(
      
      round(
        comparison_results()$improvement_fcfs,
        2
      ),
      
      "%"
      
    )
    
  })
  
  
  output$improvement_static <- renderText({
    
    req(comparison_results())
    
    paste0(
      
      round(
        comparison_results()$improvement_static,
        2
      ),
      
      "%"
      
    )
    
  })
  
  
  # ==========================================================
  # COMPARISON TABLE
  # ==========================================================
  
  output$comparison_table <- renderTable({
    
    result <- comparison_results()
    
    result$comparison
    
  })
  
  
  # ==========================================================
  # WAITING TIME PLOT
  # ==========================================================
  
  output$waiting_plot <- renderPlot({
    
    result <- comparison_results()
    
    values <-
      
      result$comparison$
      Average_Waiting_Time
    
    
    barplot(
      
      values,
      
      names.arg =
        result$comparison$Approach,
      
      col = c(
        "gray",
        "orange",
        "steelblue"
      ),
      
      border = "white",
      
      main =
        "Average Waiting Time Comparison",
      
      xlab =
        "Approach",
      
      ylab =
        "Waiting Time (minutes)"
      
    )
    
    
    text(
      
      seq_along(values),
      
      values,
      
      labels =
        round(
          values,
          2
        ),
      
      pos = 3
      
    )
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # MAX WAITING
  # ==========================================================
  
  output$max_waiting_plot <- renderPlot({
    
    result <- comparison_results()
    
    values <-
      
      result$comparison$
      Maximum_Waiting_Time
    
    
    barplot(
      
      values,
      
      names.arg =
        result$comparison$Approach,
      
      col = c(
        "gray",
        "orange",
        "steelblue"
      ),
      
      border = "white",
      
      main =
        "Maximum Waiting Time Comparison",
      
      xlab =
        "Approach",
      
      ylab =
        "Maximum Waiting Time (minutes)"
      
    )
    
    
    text(
      
      seq_along(values),
      
      values,
      
      labels =
        round(
          values,
          2
        ),
      
      pos = 3
      
    )
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # TURNAROUND
  # ==========================================================
  
  output$turnaround_plot <- renderPlot({
    
    result <- comparison_results()
    
    values <-
      
      result$comparison$
      Average_Turnaround_Time
    
    
    barplot(
      
      values,
      
      names.arg =
        result$comparison$Approach,
      
      col = c(
        "gray",
        "orange",
        "steelblue"
      ),
      
      border = "white",
      
      main =
        "Average Turnaround Time Comparison",
      
      xlab =
        "Approach",
      
      ylab =
        "Turnaround Time (minutes)"
      
    )
    
    
    text(
      
      seq_along(values),
      
      values,
      
      labels =
        round(
          values,
          2
        ),
      
      pos = 3
      
    )
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # QUEUE
  # ==========================================================
  
  output$queue_plot <- renderPlot({
    
    result <- comparison_results()
    
    values <-
      
      result$comparison$
      Average_Queue_Length
    
    
    barplot(
      
      values,
      
      names.arg =
        result$comparison$Approach,
      
      col = c(
        "gray",
        "orange",
        "steelblue"
      ),
      
      border = "white",
      
      main =
        "Average Queue Length Comparison",
      
      xlab =
        "Approach",
      
      ylab =
        "Average Queue Length"
      
    )
    
    
    text(
      
      seq_along(values),
      
      values,
      
      labels =
        round(
          values,
          2
        ),
      
      pos = 3
      
    )
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # THROUGHPUT
  # ==========================================================
  
  output$throughput_plot <- renderPlot({
    
    result <- comparison_results()
    
    values <-
      
      result$comparison$
      Throughput_Jobs_Per_Hour
    
    
    barplot(
      
      values,
      
      names.arg =
        result$comparison$Approach,
      
      col = c(
        "gray",
        "orange",
        "steelblue"
      ),
      
      border = "white",
      
      main =
        "Throughput Comparison",
      
      xlab =
        "Approach",
      
      ylab =
        "Jobs per Hour"
      
    )
    
    
    text(
      
      seq_along(values),
      
      values,
      
      labels =
        round(
          values,
          2
        ),
      
      pos = 3
      
    )
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # CRITICAL JOB PLOT
  # ==========================================================
  
  output$critical_plot <- renderPlot({
    
    result <- comparison_results()
    
    values <-
      
      result$comparison$
      Critical_Average_Waiting
    
    
    barplot(
      
      values,
      
      names.arg =
        result$comparison$Approach,
      
      col = c(
        "gray",
        "orange",
        "steelblue"
      ),
      
      border = "white",
      
      main =
        "Critical Security Job Waiting Time",
      
      xlab =
        "Approach",
      
      ylab =
        "Critical Waiting Time (minutes)"
      
    )
    
    
    text(
      
      seq_along(values),
      
      values,
      
      labels =
        round(
          values,
          2
        ),
      
      pos = 3
      
    )
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # SECURITY TABLE
  # ==========================================================
  
  output$security_table <- renderTable({
    
    result <- comparison_results()
    
    
    data.frame(
      
      Approach =
        result$comparison$Approach,
      
      Critical_Waiting =
        result$comparison$
        Critical_Average_Waiting,
      
      High_Waiting =
        result$comparison$
        High_Average_Waiting
      
    )
    
  })
  
  
  # ==========================================================
  # NORMALIZED MULTI-METRIC PLOT
  # ==========================================================
  
  output$normalized_plot <- renderPlot({
    
    result <- comparison_results()
    
    
    normalized <- result$normalized
    
    
    comparison_matrix <- t(
      
      normalized[
        ,
        c(
          "Waiting",
          "Turnaround",
          "Queue",
          "Throughput"
        )
      ]
      
    )
    
    
    barplot(
      
      comparison_matrix,
      
      beside = TRUE,
      
      col = c(
        "gray",
        "orange",
        "steelblue",
        "darkgreen"
      ),
      
      names.arg = c(
        "FCFS",
        "Static Priority",
        "GatekeeperX"
      ),
      
      main =
        "Normalized Multi-Metric Comparison",
      
      xlab =
        "Approach",
      
      ylab =
        "Normalized Value",
      
      ylim = c(
        0,
        1.2
      )
      
    )
    
    
    legend(
      
      "topright",
      
      legend = c(
        "Waiting Time",
        "Turnaround Time",
        "Queue Length",
        "Throughput"
      ),
      
      fill = c(
        "gray",
        "orange",
        "steelblue",
        "darkgreen"
      )
      
    )
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # IMPROVEMENT TABLE
  # ==========================================================
  
  output$improvement_table <- renderTable({
    
    result <- comparison_results()
    
    
    data.frame(
      
      Comparison = c(
        
        "GatekeeperX vs FCFS",
        
        "GatekeeperX vs Static Priority",
        
        "Critical Jobs vs FCFS",
        
        "Critical Jobs vs Static Priority"
        
      ),
      
      Improvement_Percent = round(
        
        c(
          
          result$improvement_fcfs,
          
          result$improvement_static,
          
          result$critical_improvement_fcfs,
          
          result$critical_improvement_static
          
        ),
        
        2
        
      )
      
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
        
        cex = 0.9,
        
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
    # INPUT
    # --------------------------------------------------------
    
    draw_box(
      2,
      8,
      "Module 3\nDataset"
    )
    
    
    # --------------------------------------------------------
    # THREE APPROACHES
    # --------------------------------------------------------
    
    draw_box(
      5,
      9,
      "FCFS"
    )
    
    
    draw_box(
      5,
      7,
      "Static\nPriority"
    )
    
    
    draw_box(
      5,
      5,
      "GatekeeperX\nDynamic Priority"
    )
    
    
    # --------------------------------------------------------
    # METRICS
    # --------------------------------------------------------
    
    draw_box(
      8,
      8,
      "Waiting\nTime"
    )
    
    
    draw_box(
      8,
      6,
      "Turnaround\nTime"
    )
    
    
    draw_box(
      8,
      4,
      "Queue +\nThroughput"
    )
    
    
    # --------------------------------------------------------
    # FINAL
    # --------------------------------------------------------
    
    draw_box(
      10.5,
      6,
      "Final\nComparison"
    )
    
    
    # --------------------------------------------------------
    # ARROWS
    # --------------------------------------------------------
    
    draw_arrow(
      3,
      8,
      4,
      9
    )
    
    
    draw_arrow(
      3,
      8,
      4,
      7
    )
    
    
    draw_arrow(
      3,
      8,
      4,
      5
    )
    
    
    draw_arrow(
      6,
      9,
      7,
      8
    )
    
    
    draw_arrow(
      6,
      7,
      7,
      6
    )
    
    
    draw_arrow(
      6,
      5,
      7,
      4
    )
    
    
    draw_arrow(
      9,
      8,
      9.5,
      6.5
    )
    
    
    draw_arrow(
      9,
      6,
      9.5,
      6
    )
    
    
    draw_arrow(
      9,
      4,
      9.5,
      5.5
    )
    
    
    title(
      
      "GatekeeperX Module 5 - Comparison Architecture",
      
      cex.main = 1.5
      
    )
    
  })
  
  
  # ==========================================================
  # DOWNLOAD COMPARISON CSV
  # ==========================================================
  
  output$download_comparison <-
    
    downloadHandler(
      
      filename = function() {
        
        "GatekeeperX_Module5_Comparison.csv"
        
      },
      
      content = function(file) {
        
        result <- comparison_results()
        
        
        write.csv(
          
          result$comparison,
          
          file,
          
          row.names = FALSE
          
        )
        
      }
      
    )
  
  
  # ==========================================================
  # DOWNLOAD REPORT
  # ==========================================================
  
  output$download_report <-
    
    downloadHandler(
      
      filename = function() {
        
        "GatekeeperX_Module5_Report.txt"
        
      },
      
      content = function(file) {
        
        result <- comparison_results()
        
        
        sink(file)
        
        
        cat(
          "====================================================\n"
        )
        
        cat(
          "GATEKEEPERX MODULE 5\n"
        )
        
        cat(
          "COMPARISON REPORT\n"
        )
        
        cat(
          "====================================================\n\n"
        )
        
        
        cat(
          "Approaches Compared:\n"
        )
        
        cat(
          "1. FCFS\n"
        )
        
        cat(
          "2. Static Priority\n"
        )
        
        cat(
          "3. GatekeeperX\n\n"
        )
        
        
        cat(
          "Performance Comparison:\n\n"
        )
        
        
        print(
          result$comparison
        )
        
        
        cat(
          "\n\nGatekeeperX vs FCFS:",
          round(
            result$improvement_fcfs,
            2
          ),
          "%\n"
        )
        
        
        cat(
          "GatekeeperX vs Static Priority:",
          round(
            result$improvement_static,
            2
          ),
          "%\n"
        )
        
        
        cat(
          "Critical Jobs vs FCFS:",
          round(
            result$critical_improvement_fcfs,
            2
          ),
          "%\n"
        )
        
        
        cat(
          "Critical Jobs vs Static Priority:",
          round(
            result$critical_improvement_static,
            2
          ),
          "%\n"
        )
        
        
        cat(
          "\nGatekeeperX Performance Score:",
          round(
            result$gatekeeper_score,
            2
          ),
          "%\n"
        )
        
        
        cat(
          "Overall Winner:",
          result$winner,
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