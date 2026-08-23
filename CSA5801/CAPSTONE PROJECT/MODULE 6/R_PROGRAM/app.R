# ============================================================
# GATEKEEPERX
# QUEUEING SIMULATION FOR INTELLIGENT DEVSECOPS SECURITY GATES
#
# COMPLETE SHINY APPLICATION
#
# MODULE 1 - DATASET GENERATION
# MODULE 2 - MODELING
# MODULE 3 - SIMULATION
# MODULE 4 - RESULTS & EVALUATION
# MODULE 5 - COMPARISON
# MODULE 6 - ARCHITECTURE
# ============================================================


library(shiny)


# ============================================================
# MODULE 1
# DATASET GENERATION
# ============================================================

generate_dataset <- function(n = 1000) {
  
  set.seed(42)
  
  severity <- sample(
    c("Critical", "High", "Medium", "Low"),
    n,
    replace = TRUE,
    prob = c(0.10, 0.25, 0.35, 0.30)
  )
  
  data <- data.frame(
    
    Job_ID = 1:n,
    
    Severity = severity,
    
    Vulnerability_Count = sample(
      0:20,
      n,
      replace = TRUE
    ),
    
    Risk_Score = round(
      runif(n, 10, 100),
      2
    ),
    
    Code_Complexity = round(
      runif(n, 1, 10),
      2
    ),
    
    Security_Scan_Time = round(
      runif(n, 1, 10),
      2
    ),
    
    Dependency_Risk = round(
      runif(n, 0, 100),
      2
    ),
    
    Business_Impact = round(
      runif(n, 0, 100),
      2
    ),
    
    Developer_Priority = sample(
      1:10,
      n,
      replace = TRUE
    ),
    
    Service_Time_Min = round(
      runif(n, 1, 8),
      2
    ),
    
    Arrival_Interval_Min = round(
      rexp(n, rate = 1 / 2),
      2
    )
    
  )
  
  data$Arrival_Time_Min <- cumsum(
    data$Arrival_Interval_Min
  )
  
  return(data)
}


# ============================================================
# MODULE 2
# SECURITY PRIORITY MODEL
# ============================================================

model_dataset <- function(data) {
  
  severity_score <- ifelse(
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
  
  vulnerability_score <- pmin(
    data$Vulnerability_Count * 5,
    100
  )
  
  data$GatekeeperX_Priority_Score <- round(
    
    severity_score * 0.30 +
      
      data$Risk_Score * 0.25 +
      
      vulnerability_score * 0.15 +
      
      data$Dependency_Risk * 0.10 +
      
      data$Business_Impact * 0.10 +
      
      data$Developer_Priority * 10 * 0.05 +
      
      data$Code_Complexity * 10 * 0.05,
    
    2
    
  )
  
  data$GatekeeperX_Priority_Score <- pmin(
    pmax(
      data$GatekeeperX_Priority_Score,
      0
    ),
    100
  )
  
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
  
  data$Security_Decision <- ifelse(
    
    data$GatekeeperX_Priority_Score >= 80,
    
    "BLOCK",
    
    ifelse(
      
      data$GatekeeperX_Priority_Score >= 60,
      
      "WAIT / PRIORITIZE",
      
      ifelse(
        
        data$GatekeeperX_Priority_Score >= 40,
        
        "PASS WITH MONITORING",
        
        "PASS"
        
      )
      
    )
    
  )
  
  return(data)
}


# ============================================================
# MODULE 3
# QUEUE SIMULATION
# ============================================================

simulate_queue <- function(
    input_data,
    method = "GatekeeperX",
    aging_factor = 0.50
) {
  
  data <- input_data
  
  data <- data[
    order(data$Arrival_Time_Min),
  ]
  
  row.names(data) <- NULL
  
  n <- nrow(data)
  
  data$Service_Start_Time <- NA_real_
  
  data$Completion_Time <- NA_real_
  
  data$Actual_Waiting_Time <- NA_real_
  
  data$Turnaround_Time <- NA_real_
  
  data$Selection_Priority <- NA_real_
  
  data$Queue_Position <- NA_integer_
  
  
  current_time <- 0
  
  next_arrival <- 1
  
  waiting_queue <- integer(0)
  
  completed <- integer(0)
  
  queue_position <- 0
  
  
  while (
    length(completed) < n
  ) {
    
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
    # IF QUEUE IS EMPTY
    # --------------------------------------------------------
    
    if (
      length(waiting_queue) == 0
    ) {
      
      if (
        next_arrival <= n
      ) {
        
        current_time <-
          data$Arrival_Time_Min[next_arrival]
        
        next
      }
    }
    
    
    # --------------------------------------------------------
    # SELECT JOB
    # --------------------------------------------------------
    
    if (
      length(waiting_queue) > 0
    ) {
      
      # ======================================================
      # FCFS
      # ======================================================
      
      if (
        method == "FCFS"
      ) {
        
        selected_position <- 1
        
        selected_priority <-
          
          data$GatekeeperX_Priority_Score[
            waiting_queue[selected_position]
          ]
        
      }
      
      
      # ======================================================
      # STATIC PRIORITY
      # ======================================================
      
      else if (
        method == "Static Priority"
      ) {
        
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
      
      else {
        
        dynamic_scores <- numeric(
          length(waiting_queue)
        )
        
        for (
          j in seq_along(waiting_queue)
        ) {
          
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
      
      waiting_queue <-
        waiting_queue[-selected_position]
      
      queue_position <-
        queue_position + 1
      
      data$Queue_Position[selected_job] <-
        queue_position
      
      
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
      
      data$Actual_Waiting_Time[selected_job] <-
        
        service_start -
        data$Arrival_Time_Min[selected_job]
      
      
      # ------------------------------------------------------
      # PRIORITY USED
      # ------------------------------------------------------
      
      data$Selection_Priority[selected_job] <-
        selected_priority
      
      
      # ------------------------------------------------------
      # COMPLETION
      # ------------------------------------------------------
      
      completion <-
        
        service_start +
        data$Service_Time_Min[selected_job]
      
      data$Completion_Time[selected_job] <-
        completion
      
      
      # ------------------------------------------------------
      # TURNAROUND TIME
      # ------------------------------------------------------
      
      data$Turnaround_Time[selected_job] <-
        
        completion -
        data$Arrival_Time_Min[selected_job]
      
      
      completed <- c(
        completed,
        selected_job
      )
      
      current_time <- completion
    }
  }
  
  return(data)
}


# ============================================================
# METRICS
# ============================================================

calculate_metrics <- function(data) {
  
  total_jobs <- nrow(data)
  
  total_time <-
    max(data$Completion_Time)
  
  average_waiting <-
    mean(data$Actual_Waiting_Time)
  
  maximum_waiting <-
    max(data$Actual_Waiting_Time)
  
  average_turnaround <-
    mean(data$Turnaround_Time)
  
  maximum_turnaround <-
    max(data$Turnaround_Time)
  
  total_service <-
    sum(data$Service_Time_Min)
  
  utilization <-
    total_service / total_time
  
  throughput <-
    total_jobs / total_time * 60
  
  
  # ----------------------------------------------------------
  # QUEUE LENGTH
  # ----------------------------------------------------------
  
  queue_lengths <- numeric(total_jobs)
  
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
  
  average_queue <-
    mean(queue_lengths)
  
  maximum_queue <-
    max(queue_lengths)
  
  
  # ----------------------------------------------------------
  # CRITICAL WAITING
  # ----------------------------------------------------------
  
  critical_data <-
    data[
      data$Severity == "Critical",
    ]
  
  if (
    nrow(critical_data) > 0
  ) {
    
    critical_waiting <-
      mean(
        critical_data$Actual_Waiting_Time
      )
    
  } else {
    
    critical_waiting <- 0
    
  }
  
  
  # ----------------------------------------------------------
  # HIGH WAITING
  # ----------------------------------------------------------
  
  high_data <-
    data[
      data$Severity == "High",
    ]
  
  if (
    nrow(high_data) > 0
  ) {
    
    high_waiting <-
      mean(
        high_data$Actual_Waiting_Time
      )
    
  } else {
    
    high_waiting <- 0
    
  }
  
  
  list(
    
    total_jobs = total_jobs,
    
    total_time = total_time,
    
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
# SIMPLE STAT CARD
# ============================================================

stat_card <- function(
    value,
    title,
    color = "#e8f1f8"
) {
  
  div(
    
    style = paste0(
      
      "background:", color, ";",
      
      "border:1px solid #cccccc;",
      
      "border-radius:10px;",
      
      "padding:15px;",
      
      "margin:5px;",
      
      "text-align:center;"
      
    ),
    
    h3(
      value,
      style = "margin-bottom:5px;"
    ),
    
    p(
      title,
      style = "font-weight:bold;"
    )
    
  )
}


# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  titlePanel(
    "GatekeeperX: Intelligent DevSecOps Security Gate"
  ),
  
  
  sidebarLayout(
    
    # ========================================================
    # SIDEBAR
    # ========================================================
    
    sidebarPanel(
      
      h3("Project Controls"),
      
      numericInput(
        
        "records",
        
        "Number of Records",
        
        value = 1000,
        
        min = 100,
        
        max = 10000,
        
        step = 100
        
      ),
      
      numericInput(
        
        "aging",
        
        "GatekeeperX Aging Factor",
        
        value = 0.50,
        
        min = 0,
        
        max = 2,
        
        step = 0.05
        
      ),
      
      hr(),
      
      actionButton(
        
        "generate",
        
        "1. Generate Dataset",
        
        class = "btn-primary",
        
        width = "100%"
        
      ),
      
      br(),
      br(),
      
      actionButton(
        
        "run_model",
        
        "2. Run Modeling",
        
        class = "btn-success",
        
        width = "100%"
        
      ),
      
      br(),
      br(),
      
      actionButton(
        
        "run_simulation",
        
        "3. Run Simulation",
        
        class = "btn-warning",
        
        width = "100%"
        
      ),
      
      hr(),
      
      downloadButton(
        
        "download_dataset",
        
        "Download Dataset CSV",
        
        width = "100%"
        
      ),
      
      br(),
      br(),
      
      downloadButton(
        
        "download_results",
        
        "Download Results CSV",
        
        width = "100%"
        
      ),
      
      hr(),
      
      helpText(
        
        "Run the modules in order: ",
        
        "Dataset → Modeling → Simulation → Evaluation → Comparison."
        
      )
      
    ),
    
    
    # ========================================================
    # MAIN PANEL
    # ========================================================
    
    mainPanel(
      
      tabsetPanel(
        
        
        # ====================================================
        # HOME
        # ====================================================
        
        tabPanel(
          
          "Home",
          
          br(),
          
          h2(
            "GatekeeperX"
          ),
          
          h4(
            "Queueing Simulation for Intelligent DevSecOps Security Gates"
          ),
          
          br(),
          
          p(
            "GatekeeperX is an intelligent security queue "
            ,
            "that prioritizes DevSecOps security jobs using "
            ,
            "risk-based priority and dynamic aging."
          ),
          
          br(),
          
          h3(
            "Six-Module Project Flow"
          ),
          
          plotOutput(
            "project_flow",
            height = "500px"
          )
          
        ),
        
        
        # ====================================================
        # MODULE 1
        # ====================================================
        
        tabPanel(
          
          "Module 1 - Dataset",
          
          br(),
          
          h2(
            "Module 1: Dataset Generation"
          ),
          
          p(
            "Generate the workload of DevSecOps security jobs."
          ),
          
          fluidRow(
            
            column(
              4,
              uiOutput("record_card")
            ),
            
            column(
              4,
              uiOutput("critical_card")
            ),
            
            column(
              4,
              uiOutput("high_card")
            )
            
          ),
          
          br(),
          
          h3(
            "Dataset Preview"
          ),
          
          tableOutput(
            "dataset_preview"
          ),
          
          br(),
          
          h3(
            "Severity Distribution"
          ),
          
          plotOutput(
            "severity_distribution",
            height = "450px"
          )
          
        ),
        
        
        # ====================================================
        # MODULE 2
        # ====================================================
        
        tabPanel(
          
          "Module 2 - Modeling",
          
          br(),
          
          h2(
            "Module 2: Risk & Priority Modeling"
          ),
          
          p(
            "GatekeeperX calculates a priority score for every "
            ,
            "security job."
          ),
          
          br(),
          
          h3(
            "Priority Score Distribution"
          ),
          
          plotOutput(
            "priority_distribution",
            height = "500px"
          ),
          
          br(),
          
          h3(
            "Priority Classes"
          ),
          
          tableOutput(
            "priority_table"
          ),
          
          br(),
          
          h3(
            "Security Decisions"
          ),
          
          plotOutput(
            "decision_distribution",
            height = "500px"
          )
          
        ),
        
        
        # ====================================================
        # MODULE 3
        # ====================================================
        
        tabPanel(
          
          "Module 3 - Simulation",
          
          br(),
          
          h2(
            "Module 3: Queue Simulation"
          ),
          
          p(
            "GatekeeperX dynamically changes job priority "
            ,
            "when a security job waits in the queue."
          ),
          
          fluidRow(
            
            column(
              4,
              uiOutput("simulation_jobs_card")
            ),
            
            column(
              4,
              uiOutput("simulation_wait_card")
            ),
            
            column(
              4,
              uiOutput("simulation_throughput_card")
            )
            
          ),
          
          br(),
          
          h3(
            "Waiting Time Distribution"
          ),
          
          plotOutput(
            "simulation_waiting_plot",
            height = "500px"
          ),
          
          br(),
          
          h3(
            "Simulation Metrics"
          ),
          
          tableOutput(
            "simulation_table"
          )
          
        ),
        
        
        # ====================================================
        # MODULE 4
        # ====================================================
        
        tabPanel(
          
          "Module 4 - Evaluation",
          
          br(),
          
          h2(
            "Module 4: Results & Evaluation"
          ),
          
          fluidRow(
            
            column(
              4,
              uiOutput("evaluation_score_card")
            ),
            
            column(
              4,
              uiOutput("evaluation_result_card")
            ),
            
            column(
              4,
              uiOutput("utilization_card")
            )
            
          ),
          
          br(),
          
          h3(
            "Performance Metrics"
          ),
          
          tableOutput(
            "evaluation_metrics"
          ),
          
          br(),
          
          h3(
            "Risk Score vs Priority Score"
          ),
          
          plotOutput(
            "risk_priority_plot",
            height = "500px"
          )
          
        ),
        
        
        # ====================================================
        # MODULE 5
        # ====================================================
        
        tabPanel(
          
          "Module 5 - Comparison",
          
          br(),
          
          h2(
            "Module 5: Comparison with Existing Approaches"
          ),
          
          p(
            "The same workload is processed using FCFS, "
            ,
            "Static Priority and GatekeeperX."
          ),
          
          br(),
          
          h3(
            "Comparison Table"
          ),
          
          tableOutput(
            "comparison_table"
          ),
          
          br(),
          
          h3(
            "Average Waiting Time"
          ),
          
          plotOutput(
            "comparison_waiting",
            height = "500px"
          ),
          
          br(),
          
          h3(
            "Average Turnaround Time"
          ),
          
          plotOutput(
            "comparison_turnaround",
            height = "500px"
          ),
          
          br(),
          
          h3(
            "Throughput"
          ),
          
          plotOutput(
            "comparison_throughput",
            height = "500px"
          ),
          
          br(),
          
          h3(
            "Critical Job Waiting Time"
          ),
          
          plotOutput(
            "comparison_critical",
            height = "500px"
          ),
          
          br(),
          
          h3(
            "Normalized Multi-Metric Comparison"
          ),
          
          plotOutput(
            "normalized_comparison",
            height = "600px"
          ),
          
          br(),
          
          h3(
            "GatekeeperX Improvement"
          ),
          
          tableOutput(
            "improvement_table"
          )
          
        ),
        
        
        # ====================================================
        # MODULE 6
        # ====================================================
        
        tabPanel(
          
          "Module 6 - Architecture",
          
          br(),
          
          h2(
            "Module 6: GatekeeperX Architecture"
          ),
          
          p(
            "GatekeeperX acts as an intelligent security gate "
            ,
            "inside the DevSecOps CI/CD pipeline."
          ),
          
          br(),
          
          plotOutput(
            "architecture",
            height = "850px"
          ),
          
          br(),
          
          h3(
            "Architecture Components"
          ),
          
          tableOutput(
            "architecture_table"
          )
          
        )
        
      )
      
    )
    
  )
  
)


# ============================================================
# SERVER
# ============================================================

server <- function(
    
  input,
  output,
  session
  
) {
  
  
  # ==========================================================
  # MODULE 1
  # DATASET
  # ==========================================================
  
  dataset <- eventReactive(
    
    input$generate,
    
    {
      
      generate_dataset(
        input$records
      )
      
    },
    
    ignoreNULL = TRUE
    
  )
  
  
  # ==========================================================
  # MODULE 2
  # MODELING
  # ==========================================================
  
  modeled_data <- eventReactive(
    
    input$run_model,
    
    {
      
      req(
        dataset()
      )
      
      model_dataset(
        dataset()
      )
      
    },
    
    ignoreNULL = TRUE
    
  )
  
  
  # ==========================================================
  # MODULE 3
  # SIMULATION
  # ==========================================================
  
  simulation_data <- eventReactive(
    
    input$run_simulation,
    
    {
      
      req(
        modeled_data()
      )
      
      withProgress(
        
        message =
          "Running GatekeeperX simulation...",
        
        value = 0,
        
        {
          
          incProgress(
            0.2,
            detail = "Initializing queue"
          )
          
          result <-
            
            simulate_queue(
              
              modeled_data(),
              
              "GatekeeperX",
              
              input$aging
              
            )
          
          incProgress(
            1,
            detail = "Simulation completed"
          )
          
          result
          
        }
        
      )
      
    },
    
    ignoreNULL = TRUE
    
  )
  
  
  # ==========================================================
  # MODULE 1 CARDS
  # ==========================================================
  
  output$record_card <- renderUI({
    
    req(
      dataset()
    )
    
    stat_card(
      
      nrow(
        dataset()
      ),
      
      "Total Records"
      
    )
    
  })
  
  
  output$critical_card <- renderUI({
    
    req(
      dataset()
    )
    
    stat_card(
      
      sum(
        dataset()$Severity ==
          "Critical"
      ),
      
      "Critical Jobs",
      
      "#fde9e9"
      
    )
    
  })
  
  
  output$high_card <- renderUI({
    
    req(
      dataset()
    )
    
    stat_card(
      
      sum(
        dataset()$Severity ==
          "High"
      ),
      
      "High Jobs",
      
      "#fff0d9"
      
    )
    
  })
  
  
  # ==========================================================
  # MODULE 1 TABLE
  # ==========================================================
  
  output$dataset_preview <- renderTable({
    
    req(
      dataset()
    )
    
    head(
      dataset(),
      10
    )
    
  })
  
  
  # ==========================================================
  # MODULE 1 PLOT
  # ==========================================================
  
  output$severity_distribution <- renderPlot({
    
    req(
      dataset()
    )
    
    counts <-
      
      table(
        dataset()$Severity
      )
    
    
    barplot(
      
      counts,
      
      col = c(
        "red",
        "orange",
        "yellow",
        "lightgreen"
      ),
      
      border = "white",
      
      main =
        "Security Severity Distribution",
      
      xlab =
        "Severity",
      
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
  # MODULE 2
  # PRIORITY DISTRIBUTION
  # ==========================================================
  
  output$priority_distribution <- renderPlot({
    
    req(
      modeled_data()
    )
    
    hist(
      
      modeled_data()$
        GatekeeperX_Priority_Score,
      
      breaks = 25,
      
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
  # MODULE 2 TABLE
  # ==========================================================
  
  output$priority_table <- renderTable({
    
    req(
      modeled_data()
    )
    
    counts <-
      
      table(
        modeled_data()$
          Priority_Class
      )
    
    
    data.frame(
      
      Priority_Class =
        names(counts),
      
      Number_of_Jobs =
        as.numeric(counts)
      
    )
    
  })
  
  
  # ==========================================================
  # MODULE 2 DECISION PLOT
  # ==========================================================
  
  output$decision_distribution <- renderPlot({
    
    req(
      modeled_data()
    )
    
    counts <-
      
      table(
        modeled_data()$
          Security_Decision
      )
    
    
    barplot(
      
      counts,
      
      col = c(
        "red",
        "orange",
        "lightgreen",
        "green"
      ),
      
      border = "white",
      
      main =
        "GatekeeperX Security Decisions",
      
      xlab =
        "Decision",
      
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
  # MODULE 3 CARDS
  # ==========================================================
  
  output$simulation_jobs_card <- renderUI({
    
    req(
      simulation_data()
    )
    
    stat_card(
      
      nrow(
        simulation_data()
      ),
      
      "Simulated Jobs"
      
    )
    
  })
  
  
  output$simulation_wait_card <- renderUI({
    
    req(
      simulation_data()
    )
    
    value <-
      
      mean(
        simulation_data()$
          Actual_Waiting_Time
      )
    
    
    stat_card(
      
      paste0(
        round(value, 2),
        " min"
      ),
      
      "Average Waiting Time"
      
    )
    
  })
  
  
  output$simulation_throughput_card <- renderUI({
    
    req(
      simulation_data()
    )
    
    metrics <-
      
      calculate_metrics(
        simulation_data()
      )
    
    
    stat_card(
      
      paste0(
        round(
          metrics$throughput,
          2
        ),
        " jobs/hr"
      ),
      
      "Throughput"
      
    )
    
  })
  
  
  # ==========================================================
  # MODULE 3 WAITING TIME
  # ==========================================================
  
  output$simulation_waiting_plot <- renderPlot({
    
    req(
      simulation_data()
    )
    
    waiting <-
      
      simulation_data()$
      Actual_Waiting_Time
    
    
    hist(
      
      waiting,
      
      breaks = 25,
      
      col = "steelblue",
      
      border = "white",
      
      main =
        "GatekeeperX Waiting Time Distribution",
      
      xlab =
        "Waiting Time (minutes)",
      
      ylab =
        "Number of Jobs"
      
    )
    
    
    abline(
      
      v = mean(waiting),
      
      col = "red",
      
      lwd = 3,
      
      lty = 2
      
    )
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # MODULE 3 TABLE
  # ==========================================================
  
  output$simulation_table <- renderTable({
    
    req(
      simulation_data()
    )
    
    m <-
      
      calculate_metrics(
        simulation_data()
      )
    
    
    data.frame(
      
      Metric = c(
        
        "Total Jobs",
        
        "Average Waiting Time",
        
        "Maximum Waiting Time",
        
        "Average Turnaround Time",
        
        "Maximum Turnaround Time",
        
        "Average Queue Length",
        
        "Maximum Queue Length",
        
        "Throughput",
        
        "Server Utilization",
        
        "Critical Job Waiting",
        
        "High Job Waiting"
        
      ),
      
      Value = c(
        
        m$total_jobs,
        
        round(
          m$average_waiting,
          2
        ),
        
        round(
          m$maximum_waiting,
          2
        ),
        
        round(
          m$average_turnaround,
          2
        ),
        
        round(
          m$maximum_turnaround,
          2
        ),
        
        round(
          m$average_queue,
          2
        ),
        
        round(
          m$maximum_queue,
          2
        ),
        
        round(
          m$throughput,
          2
        ),
        
        paste0(
          round(
            m$utilization * 100,
            2
          ),
          "%"
        ),
        
        round(
          m$critical_waiting,
          2
        ),
        
        round(
          m$high_waiting,
          2
        )
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # MODULE 4 CARDS
  # ==========================================================
  
  output$evaluation_score_card <- renderUI({
    
    req(
      simulation_data()
    )
    
    m <-
      
      calculate_metrics(
        simulation_data()
      )
    
    
    score <- 0
    
    
    if (
      m$average_waiting <= 10
    ) {
      
      score <- score + 25
      
    }
    
    
    if (
      m$average_turnaround <= 20
    ) {
      
      score <- score + 25
      
    }
    
    
    if (
      m$utilization < 1
    ) {
      
      score <- score + 25
      
    }
    
    
    if (
      m$critical_waiting <=
      m$average_waiting
    ) {
      
      score <- score + 25
      
    }
    
    
    stat_card(
      
      paste0(
        score,
        "%"
      ),
      
      "Evaluation Score"
      
    )
    
  })
  
  
  output$evaluation_result_card <- renderUI({
    
    req(
      simulation_data()
    )
    
    m <-
      
      calculate_metrics(
        simulation_data()
      )
    
    
    score <- 0
    
    
    if (
      m$average_waiting <= 10
    ) {
      
      score <- score + 25
      
    }
    
    
    if (
      m$average_turnaround <= 20
    ) {
      
      score <- score + 25
      
    }
    
    
    if (
      m$utilization < 1
    ) {
      
      score <- score + 25
      
    }
    
    
    if (
      m$critical_waiting <=
      m$average_waiting
    ) {
      
      score <- score + 25
      
    }
    
    
    result <-
      
      if (
        score >= 80
      ) {
        
        "GOOD"
        
      } else if (
        score >= 60
      ) {
        
        "MODERATE"
        
      } else {
        
        "NEEDS IMPROVEMENT"
        
      }
    
    
    stat_card(
      
      result,
      
      "Overall Evaluation",
      
      "#eaf7ea"
      
    )
    
  })
  
  
  output$utilization_card <- renderUI({
    
    req(
      simulation_data()
    )
    
    m <-
      
      calculate_metrics(
        simulation_data()
      )
    
    
    stat_card(
      
      paste0(
        
        round(
          m$utilization * 100,
          2
        ),
        
        "%"
        
      ),
      
      "Server Utilization"
      
    )
    
  })
  
  
  # ==========================================================
  # MODULE 4 TABLE
  # ==========================================================
  
  output$evaluation_metrics <- renderTable({
    
    req(
      simulation_data()
    )
    
    m <-
      
      calculate_metrics(
        simulation_data()
      )
    
    
    data.frame(
      
      Metric = c(
        
        "Average Waiting Time",
        
        "Maximum Waiting Time",
        
        "Average Turnaround Time",
        
        "Maximum Turnaround Time",
        
        "Average Queue Length",
        
        "Maximum Queue Length",
        
        "Throughput",
        
        "Server Utilization",
        
        "Critical Job Waiting"
        
      ),
      
      Value = c(
        
        round(
          m$average_waiting,
          2
        ),
        
        round(
          m$maximum_waiting,
          2
        ),
        
        round(
          m$average_turnaround,
          2
        ),
        
        round(
          m$maximum_turnaround,
          2
        ),
        
        round(
          m$average_queue,
          2
        ),
        
        round(
          m$maximum_queue,
          2
        ),
        
        round(
          m$throughput,
          2
        ),
        
        paste0(
          round(
            m$utilization * 100,
            2
          ),
          "%"
        ),
        
        round(
          m$critical_waiting,
          2
        )
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # MODULE 4 RISK VS PRIORITY
  # ==========================================================
  
  output$risk_priority_plot <- renderPlot({
    
    req(
      modeled_data()
    )
    
    
    plot(
      
      modeled_data()$Risk_Score,
      
      modeled_data()$
        GatekeeperX_Priority_Score,
      
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
          modeled_data()
        
      ),
      
      lwd = 3
      
    )
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # MODULE 5
  # COMPARISON
  # ==========================================================
  
  comparison_results <- eventReactive(
    
    input$run_simulation,
    
    {
      
      req(
        modeled_data()
      )
      
      
      fcfs_data <-
        
        simulate_queue(
          
          modeled_data(),
          
          "FCFS",
          
          input$aging
          
        )
      
      
      static_data <-
        
        simulate_queue(
          
          modeled_data(),
          
          "Static Priority",
          
          input$aging
          
        )
      
      
      gatekeeper_data <-
        
        simulation_data()
      
      
      fcfs <-
        
        calculate_metrics(
          fcfs_data
        )
      
      
      static <-
        
        calculate_metrics(
          static_data
        )
      
      
      gatekeeper <-
        
        calculate_metrics(
          gatekeeper_data
        )
      
      
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
        
        Average_Turnaround_Time = c(
          
          fcfs$average_turnaround,
          
          static$average_turnaround,
          
          gatekeeper$average_turnaround
          
        ),
        
        Average_Queue_Length = c(
          
          fcfs$average_queue,
          
          static$average_queue,
          
          gatekeeper$average_queue
          
        ),
        
        Throughput = c(
          
          fcfs$throughput,
          
          static$throughput,
          
          gatekeeper$throughput
          
        ),
        
        Utilization = c(
          
          fcfs$utilization * 100,
          
          static$utilization * 100,
          
          gatekeeper$utilization * 100
          
        ),
        
        Critical_Waiting = c(
          
          fcfs$critical_waiting,
          
          static$critical_waiting,
          
          gatekeeper$critical_waiting
          
        )
        
      )
      
      
      comparison[, -1] <-
        
        round(
          comparison[, -1],
          2
        )
      
      
      # ------------------------------------------------------
      # IMPROVEMENT
      # ------------------------------------------------------
      
      improvement_fcfs <-
        
        if (
          fcfs$average_waiting != 0
        ) {
          
          (
            
            fcfs$average_waiting -
              gatekeeper$average_waiting
            
          ) /
            
            fcfs$average_waiting *
            100
          
        } else {
          
          0
          
        }
      
      
      improvement_static <-
        
        if (
          static$average_waiting != 0
        ) {
          
          (
            
            static$average_waiting -
              gatekeeper$average_waiting
            
          ) /
            
            static$average_waiting *
            100
          
        } else {
          
          0
          
        }
      
      
      # ------------------------------------------------------
      # WINNER
      # ------------------------------------------------------
      
      waiting_values <- c(
        
        fcfs$average_waiting,
        
        static$average_waiting,
        
        gatekeeper$average_waiting
        
      )
      
      
      winner <-
        
        comparison$Approach[
          which.min(
            waiting_values
          )
        ]
      
      
      list(
        
        comparison =
          comparison,
        
        fcfs =
          fcfs,
        
        static =
          static,
        
        gatekeeper =
          gatekeeper,
        
        improvement_fcfs =
          improvement_fcfs,
        
        improvement_static =
          improvement_static,
        
        winner =
          winner
        
      )
      
    }
    
  )
  
  
  # ==========================================================
  # COMPARISON TABLE
  # ==========================================================
  
  output$comparison_table <- renderTable({
    
    req(
      comparison_results()
    )
    
    comparison_results()$comparison
    
  })
  
  
  # ==========================================================
  # WAITING TIME COMPARISON
  # ==========================================================
  
  output$comparison_waiting <- renderPlot({
    
    req(
      comparison_results()
    )
    
    
    x <-
      comparison_results()$comparison
    
    
    values <-
      x$Average_Waiting_Time
    
    
    barplot(
      
      values,
      
      names.arg =
        x$Approach,
      
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
        "Minutes"
      
    )
    
    
    text(
      
      seq_along(values),
      
      values,
      
      labels =
        values,
      
      pos = 3
      
    )
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # TURNAROUND COMPARISON
  # ==========================================================
  
  output$comparison_turnaround <- renderPlot({
    
    req(
      comparison_results()
    )
    
    
    x <-
      comparison_results()$comparison
    
    
    values <-
      x$Average_Turnaround_Time
    
    
    barplot(
      
      values,
      
      names.arg =
        x$Approach,
      
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
        "Minutes"
      
    )
    
    
    text(
      
      seq_along(values),
      
      values,
      
      labels =
        values,
      
      pos = 3
      
    )
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # THROUGHPUT
  # ==========================================================
  
  output$comparison_throughput <- renderPlot({
    
    req(
      comparison_results()
    )
    
    
    x <-
      comparison_results()$comparison
    
    
    values <-
      x$Throughput
    
    
    barplot(
      
      values,
      
      names.arg =
        x$Approach,
      
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
        values,
      
      pos = 3
      
    )
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # CRITICAL WAITING
  # ==========================================================
  
  output$comparison_critical <- renderPlot({
    
    req(
      comparison_results()
    )
    
    
    x <-
      comparison_results()$comparison
    
    
    values <-
      x$Critical_Waiting
    
    
    barplot(
      
      values,
      
      names.arg =
        x$Approach,
      
      col = c(
        "gray",
        "orange",
        "steelblue"
      ),
      
      border = "white",
      
      main =
        "Critical Job Waiting Time",
      
      xlab =
        "Approach",
      
      ylab =
        "Minutes"
      
    )
    
    
    text(
      
      seq_along(values),
      
      values,
      
      labels =
        values,
      
      pos = 3
      
    )
    
    
    grid()
    
  })
  
  
  # ==========================================================
  # NORMALIZED COMPARISON
  # ==========================================================
  
  output$normalized_comparison <- renderPlot({
    
    req(
      comparison_results()
    )
    
    
    x <-
      comparison_results()$comparison
    
    
    normalized <- data.frame(
      
      Waiting =
        
        x$Average_Waiting_Time /
        max(
          x$Average_Waiting_Time
        ),
      
      Turnaround =
        
        x$Average_Turnaround_Time /
        max(
          x$Average_Turnaround_Time
        ),
      
      Queue =
        
        x$Average_Queue_Length /
        max(
          x$Average_Queue_Length
        ),
      
      Throughput =
        
        x$Throughput /
        max(
          x$Throughput
        )
      
    )
    
    
    comparison_matrix <-
      
      t(
        normalized
      )
    
    
    # IMPORTANT:
    # 4 metrics = 4 colors
    # 3 approaches = 3 names
    
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
    
    req(
      comparison_results()
    )
    
    
    result <-
      comparison_results()
    
    
    data.frame(
      
      Comparison = c(
        
        "GatekeeperX vs FCFS",
        
        "GatekeeperX vs Static Priority"
        
      ),
      
      Waiting_Time_Improvement_Percent = round(
        
        c(
          
          result$improvement_fcfs,
          
          result$improvement_static
          
        ),
        
        2
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # MODULE 6
  # ARCHITECTURE
  # ==========================================================
  
  output$architecture <- renderPlot({
    
    plot.new()
    
    
    plot.window(
      
      xlim = c(
        0,
        16
      ),
      
      ylim = c(
        0,
        12
      )
      
    )
    
    
    draw_box <- function(
    
      x,
      y,
      label,
      fill = "lightblue"
      
    ) {
      
      rect(
        
        x - 1.2,
        y - 0.4,
        
        x + 1.2,
        y + 0.4,
        
        col = fill,
        
        border = "navy",
        
        lwd = 2
        
      )
      
      
      text(
        
        x,
        y,
        
        label,
        
        font = 2,
        
        cex = 0.8
        
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
        
        length = 0.1,
        
        lwd = 2
        
      )
      
    }
    
    
    # --------------------------------------------------------
    # DEVSECOPS PIPELINE
    # --------------------------------------------------------
    
    draw_box(
      
      2,
      10,
      
      "Developer\nCommit",
      
      "lightgray"
      
    )
    
    
    draw_box(
      
      5,
      10,
      
      "CI / Build",
      
      "lightgray"
      
    )
    
    
    draw_box(
      
      8,
      10,
      
      "Security\nScanning",
      
      "lightyellow"
      
    )
    
    
    draw_box(
      
      11,
      10,
      
      "GatekeeperX\nSecurity Gate",
      
      "lightblue"
      
    )
    
    
    draw_box(
      
      14,
      10,
      
      "Deployment",
      
      "lightgreen"
      
    )
    
    
    draw_arrow(
      3.2,
      10,
      3.8,
      10
    )
    
    
    draw_arrow(
      6.2,
      10,
      6.8,
      10
    )
    
    
    draw_arrow(
      9.2,
      10,
      9.8,
      10
    )
    
    
    draw_arrow(
      12.2,
      10,
      12.8,
      10
    )
    
    
    # --------------------------------------------------------
    # GATEKEEPERX ENGINE
    # --------------------------------------------------------
    
    draw_box(
      
      5,
      7,
      
      "Risk\nAssessment",
      
      "white"
      
    )
    
    
    draw_box(
      
      8,
      7,
      
      "Priority\nCalculation",
      
      "white"
      
    )
    
    
    draw_box(
      
      11,
      7,
      
      "Intelligent\nQueue",
      
      "white"
      
    )
    
    
    draw_box(
      
      14,
      7,
      
      "Dynamic\nAging",
      
      "white"
      
    )
    
    
    draw_arrow(
      6.2,
      7,
      6.8,
      7
    )
    
    
    draw_arrow(
      9.2,
      7,
      9.8,
      7
    )
    
    
    draw_arrow(
      12.2,
      7,
      12.8,
      7
    )
    
    
    draw_arrow(
      8,
      9.6,
      8,
      7.5
    )
    
    
    # --------------------------------------------------------
    # DECISION
    # --------------------------------------------------------
    
    draw_box(
      
      8,
      4.5,
      
      "Security\nDecision",
      
      "lightyellow"
      
    )
    
    
    draw_arrow(
      
      14,
      6.5,
      8,
      5
      
    )
    
    
    # --------------------------------------------------------
    # OUTPUTS
    # --------------------------------------------------------
    
    draw_box(
      
      4,
      2,
      
      "PASS",
      
      "lightgreen"
      
    )
    
    
    draw_box(
      
      8,
      2,
      
      "WAIT /\nPRIORITIZE",
      
      "orange"
      
    )
    
    
    draw_box(
      
      12,
      2,
      
      "BLOCK",
      
      "lightcoral"
      
    )
    
    
    draw_arrow(
      7,
      4.2,
      4.5,
      2.5
    )
    
    
    draw_arrow(
      8,
      4,
      8,
      2.5
    )
    
    
    draw_arrow(
      9,
      4.2,
      11.5,
      2.5
    )
    
    
    title(
      
      "GatekeeperX DevSecOps Architecture",
      
      cex.main = 1.5
      
    )
    
  })
  
  
  # ==========================================================
  # ARCHITECTURE TABLE
  # ==========================================================
  
  output$architecture_table <- renderTable({
    
    data.frame(
      
      Component = c(
        
        "Developer",
        
        "CI / Build",
        
        "Security Scanning",
        
        "Risk Assessment",
        
        "Priority Calculation",
        
        "Intelligent Queue",
        
        "Dynamic Aging",
        
        "Security Decision",
        
        "Deployment Gate"
        
      ),
      
      Function = c(
        
        "Creates or modifies application code",
        
        "Builds and prepares the application",
        
        "Performs security checks",
        
        "Determines security risk",
        
        "Calculates the GatekeeperX priority score",
        
        "Organizes security jobs",
        
        "Increases priority of waiting jobs",
        
        "Produces PASS, WAIT/PRIORITIZE or BLOCK",
        
        "Controls deployment"
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # PROJECT FLOW
  # ==========================================================
  
  output$project_flow <- renderPlot({
    
    plot.new()
    
    
    plot.window(
      
      xlim = c(
        0,
        12
      ),
      
      ylim = c(
        0,
        8
      )
      
    )
    
    
    draw_box <- function(
    
      x,
      y,
      label
      
    ) {
      
      rect(
        
        x - 0.9,
        y - 0.45,
        
        x + 0.9,
        y + 0.45,
        
        col = "lightblue",
        
        border = "navy",
        
        lwd = 2
        
      )
      
      
      text(
        
        x,
        y,
        
        label,
        
        font = 2,
        
        cex = 0.7
        
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
    
    
    draw_box(
      1,
      6,
      "MODULE 1\nDATASET"
    )
    
    
    draw_box(
      3,
      6,
      "MODULE 2\nMODELING"
    )
    
    
    draw_box(
      5,
      6,
      "MODULE 3\nSIMULATION"
    )
    
    
    draw_box(
      7,
      6,
      "MODULE 4\nEVALUATION"
    )
    
    
    draw_box(
      9,
      6,
      "MODULE 5\nCOMPARISON"
    )
    
    
    draw_arrow(
      1.9,
      6,
      2.1,
      6
    )
    
    
    draw_arrow(
      3.9,
      6,
      4.1,
      6
    )
    
    
    draw_arrow(
      5.9,
      6,
      6.1,
      6
    )
    
    
    draw_arrow(
      7.9,
      6,
      8.1,
      6
    )
    
    
    draw_box(
      
      5,
      3,
      
      "MODULE 6\nARCHITECTURE"
      
    )
    
    
    draw_arrow(
      
      9,
      5.5,
      5,
      3.5
      
    )
    
    
    text(
      
      5,
      
      1.3,
      
      "GATEKEEPERX\nINTELLIGENT DEVSECOPS SECURITY GATE",
      
      cex = 1.2,
      
      font = 2
      
    )
    
    
    title(
      
      "GatekeeperX Six-Module Project Flow",
      
      cex.main = 1.5
      
    )
    
  })
  
  
  # ==========================================================
  # DOWNLOAD DATASET
  # ==========================================================
  
  output$download_dataset <-
    
    downloadHandler(
      
      filename = function() {
        
        "GatekeeperX_Dataset.csv"
        
      },
      
      content = function(file) {
        
        req(
          dataset()
        )
        
        
        write.csv(
          
          dataset(),
          
          file,
          
          row.names = FALSE
          
        )
        
      }
      
    )
  
  
  # ==========================================================
  # DOWNLOAD RESULTS
  # ==========================================================
  
  output$download_results <-
    
    downloadHandler(
      
      filename = function() {
        
        "GatekeeperX_Simulation_Results.csv"
        
      },
      
      content = function(file) {
        
        req(
          simulation_data()
        )
        
        
        write.csv(
          
          simulation_data(),
          
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