# ============================================================
# GATEKEEPERX
# MODULE 3 - DISCRETE EVENT QUEUE SIMULATION
#
# Topic:
# Queueing Simulation for Intelligent DevSecOps Security Gates
# ============================================================


# ============================================================
# 1. SETUP
# ============================================================

rm(list = ls())

set.seed(42)

cat("\n")
cat("====================================================\n")
cat(" GATEKEEPERX - MODULE 3: SIMULATION\n")
cat("====================================================\n")


# ------------------------------------------------------------
# OUTPUT DIRECTORY
# ------------------------------------------------------------

output_dir <- "GatekeeperX_Module3_Output"

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


# ============================================================
# 2. LOAD MODULE 2 DATASET
# ============================================================

csv_file <- file.path(
  "GatekeeperX_Module2_Output",
  "GatekeeperX_Module2_Modeled_Data.csv"
)


if (!file.exists(csv_file)) {
  
  stop(
    "\nModule 2 modeled CSV was not found.\n",
    "Please run Module 2 first.\n"
  )
  
}


data <- read.csv(
  csv_file,
  stringsAsFactors = FALSE
)


cat(
  "\nModule 2 dataset loaded successfully.\n"
)

cat(
  "Number of jobs:",
  nrow(data),
  "\n"
)


# ============================================================
# 3. VALIDATE REQUIRED COLUMNS
# ============================================================

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


# ============================================================
# 4. SORT JOBS BY ARRIVAL TIME
# ============================================================

data <- data[
  order(
    data$Arrival_Time_Min
  ),
]


row.names(data) <- NULL


# ============================================================
# 5. SIMULATION PARAMETERS
# ============================================================

# Aging factor:
#
# A waiting job gains priority as it waits.
#
# This helps prevent starvation of low-priority jobs.

aging_factor <- 0.50


# Maximum priority score

max_priority <- 100


# ============================================================
# 6. INITIALIZE SIMULATION VARIABLES
# ============================================================

n <- nrow(data)


# Actual service start time

data$Service_Start_Time <- NA


# Actual completion time

data$Completion_Time <- NA


# Actual waiting time

data$Actual_Waiting_Time <- NA


# Turnaround time

data$Turnaround_Time <- NA


# Priority at the moment job is selected

data$Dynamic_Priority <- NA


# Queue position

data$Simulation_Queue_Position <- NA


# Service status

data$Simulation_Status <- "Waiting"


# ============================================================
# 7. QUEUE SIMULATION
# ============================================================

cat("\n")
cat("Starting discrete-event simulation...\n")


# Current simulation clock

current_time <- 0


# Index of next job that has not yet entered queue

next_arrival_index <- 1


# Jobs currently waiting

waiting_queue <- integer(0)


# Completed jobs

completed_jobs <- integer(0)


# Queue history

queue_history_time <- c()

queue_history_length <- c()


# Event counter

event_number <- 0


# ------------------------------------------------------------
# MAIN SIMULATION LOOP
# ------------------------------------------------------------

while (
  
  length(completed_jobs) < n
  
) {
  
  
  # ==========================================================
  # ADD ARRIVING JOBS TO QUEUE
  # ==========================================================
  
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
  
  
  # ==========================================================
  # IF QUEUE IS EMPTY
  # ==========================================================
  
  if (
    
    length(waiting_queue) == 0
    
  ) {
    
    
    # Move clock to next arrival
    
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
  
  
  # ==========================================================
  # CALCULATE DYNAMIC PRIORITY
  # ==========================================================
  
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
      
      
      # ------------------------------------------------------
      # Waiting time at current moment
      # ------------------------------------------------------
      
      current_wait <- max(
        
        0,
        
        current_time -
          data$Arrival_Time_Min[index]
        
      )
      
      
      # ------------------------------------------------------
      # Aging bonus
      # ------------------------------------------------------
      
      aging_bonus <-
        
        current_wait *
        aging_factor
      
      
      # ------------------------------------------------------
      # Dynamic priority
      # ------------------------------------------------------
      
      dynamic_scores[j] <-
        
        min(
          
          max_priority,
          
          data$GatekeeperX_Priority_Score[index] +
            aging_bonus
          
        )
      
    }
    
    
    # ========================================================
    # SELECT HIGHEST PRIORITY JOB
    # ========================================================
    
    selected_position <- which.max(
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
    
    
    # ========================================================
    # REMOVE JOB FROM QUEUE
    # ========================================================
    
    waiting_queue <-
      waiting_queue[
        -selected_position
      ]
    
    
    # ========================================================
    # RECORD QUEUE POSITION
    # ========================================================
    
    event_number <-
      event_number + 1
    
    
    data$Simulation_Queue_Position[
      selected_job
    ] <- event_number
    
    
    # ========================================================
    # SERVICE START TIME
    # ========================================================
    
    service_start <- max(
      
      current_time,
      
      data$Arrival_Time_Min[
        selected_job
      ]
      
    )
    
    
    data$Service_Start_Time[
      selected_job
    ] <- service_start
    
    
    # ========================================================
    # WAITING TIME
    # ========================================================
    
    actual_wait <-
      
      service_start -
      data$Arrival_Time_Min[
        selected_job
      ]
    
    
    data$Actual_Waiting_Time[
      selected_job
    ] <- actual_wait
    
    
    # ========================================================
    # DYNAMIC PRIORITY
    # ========================================================
    
    data$Dynamic_Priority[
      selected_job
    ] <- round(
      
      selected_priority,
      
      2
      
    )
    
    
    # ========================================================
    # SERVICE TIME
    # ========================================================
    
    service_time <-
      
      data$Service_Time_Min[
        selected_job
      ]
    
    
    # ========================================================
    # COMPLETION TIME
    # ========================================================
    
    completion_time <-
      
      service_start +
      service_time
    
    
    data$Completion_Time[
      selected_job
    ] <- completion_time
    
    
    # ========================================================
    # TURNAROUND TIME
    # ========================================================
    
    data$Turnaround_Time[
      selected_job
    ] <-
      
      completion_time -
      data$Arrival_Time_Min[
        selected_job
      ]
    
    
    # ========================================================
    # STATUS
    # ========================================================
    
    data$Simulation_Status[
      selected_job
    ] <- "Completed"
    
    
    # ========================================================
    # ADD TO COMPLETED JOBS
    # ========================================================
    
    completed_jobs <-
      c(
        completed_jobs,
        selected_job
      )
    
    
    # ========================================================
    # MOVE SIMULATION CLOCK
    # ========================================================
    
    current_time <-
      completion_time
    
    
    # ========================================================
    # RECORD QUEUE LENGTH
    # ========================================================
    
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


# ============================================================
# 8. ROUND RESULTS
# ============================================================

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


# ============================================================
# 9. SIMULATION METRICS
# ============================================================

total_simulation_time <-
  
  max(
    data$Completion_Time
  )


average_waiting_time <-
  
  mean(
    data$Actual_Waiting_Time
  )


maximum_waiting_time <-
  
  max(
    data$Actual_Waiting_Time
  )


minimum_waiting_time <-
  
  min(
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


# ============================================================
# 10. SERVER UTILIZATION
# ============================================================

total_service_time <-
  
  sum(
    data$Service_Time_Min
  )


server_utilization <-
  
  total_service_time /
  total_simulation_time


# ============================================================
# 11. THROUGHPUT
# ============================================================

throughput <-
  
  n /
  total_simulation_time


# Jobs per hour

throughput_per_hour <-
  
  throughput * 60


# ============================================================
# 12. QUEUE METRICS
# ============================================================

average_queue_length <-
  
  mean(
    queue_history_length
  )


maximum_queue_length <-
  
  max(
    queue_history_length
  )


# ============================================================
# 13. SECURITY DECISION COUNTS
# ============================================================

decision_counts <- table(
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


# ============================================================
# 14. SIMULATION SUMMARY
# ============================================================

cat("\n")
cat("====================================================\n")
cat(" SIMULATION RESULTS\n")
cat("====================================================\n")


cat(
  "Total Jobs:",
  n,
  "\n"
)


cat(
  "Simulation Time:",
  round(
    total_simulation_time,
    2
  ),
  "minutes\n"
)


cat(
  "Average Waiting Time:",
  round(
    average_waiting_time,
    2
  ),
  "minutes\n"
)


cat(
  "Maximum Waiting Time:",
  round(
    maximum_waiting_time,
    2
  ),
  "minutes\n"
)


cat(
  "Average Turnaround Time:",
  round(
    average_turnaround_time,
    2
  ),
  "minutes\n"
)


cat(
  "Maximum Queue Length:",
  maximum_queue_length,
  "\n"
)


cat(
  "Average Queue Length:",
  round(
    average_queue_length,
    2
  ),
  "\n"
)


cat(
  "Server Utilization:",
  round(
    server_utilization * 100,
    2
  ),
  "%\n"
)


cat(
  "Throughput:",
  round(
    throughput_per_hour,
    2
  ),
  "jobs/hour\n"
)


# ============================================================
# 15. SECURITY DECISION RESULTS
# ============================================================

cat("\n")
cat("---- SECURITY GATE RESULTS ----\n")


cat(
  "Blocked Jobs:",
  blocked_jobs,
  "\n"
)


cat(
  "Prioritized Jobs:",
  prioritized_jobs,
  "\n"
)


cat(
  "Pass Jobs:",
  passed_jobs,
  "\n"
)


cat(
  "Pass with Monitoring:",
  monitored_jobs,
  "\n"
)


# ============================================================
# 16. PLOT 1
# WAITING TIME DISTRIBUTION
# ============================================================

png(
  
  file.path(
    output_dir,
    "Module3_Waiting_Time_Distribution.png"
  ),
  
  width = 1000,
  
  height = 700
)


hist(
  
  data$Actual_Waiting_Time,
  
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
  
  v = average_waiting_time,
  
  lwd = 3,
  
  lty = 2
  
)


grid()


dev.off()


# ============================================================
# 17. PLOT 2
# ARRIVAL VS COMPLETION
# ============================================================

png(
  
  file.path(
    output_dir,
    "Module3_Arrival_vs_Completion.png"
  ),
  
  width = 1000,
  
  height = 700
)


plot(
  
  data$Job_ID,
  
  data$Arrival_Time_Min,
  
  type = "l",
  
  col = "darkgreen",
  
  lwd = 2,
  
  main =
    "GatekeeperX - Job Arrival and Completion Timeline",
  
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


dev.off()


# ============================================================
# 18. PLOT 3
# QUEUE LENGTH OVER TIME
# ============================================================

png(
  
  file.path(
    output_dir,
    "Module3_Queue_Length.png"
  ),
  
  width = 1000,
  
  height = 700
)


plot(
  
  queue_history_time,
  
  queue_history_length,
  
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


dev.off()


# ============================================================
# 19. PLOT 4
# DYNAMIC PRIORITY
# ============================================================

png(
  
  file.path(
    output_dir,
    "Module3_Dynamic_Priority.png"
  ),
  
  width = 1000,
  
  height = 700
)


plot(
  
  data$Job_ID,
  
  data$Dynamic_Priority,
  
  pch = 19,
  
  col = "orange",
  
  main =
    "GatekeeperX - Dynamic Priority at Service Selection",
  
  xlab =
    "Job ID",
  
  ylab =
    "Dynamic Priority Score"
)


grid()


dev.off()


# ============================================================
# 20. SIMULATION ARCHITECTURE DIAGRAM
# ============================================================

png(
  
  file.path(
    output_dir,
    "Module3_Simulation_Architecture.png"
  ),
  
  width = 1500,
  
  height = 900
)


plot.new()


plot.window(
  
  xlim = c(0, 12),
  
  ylim = c(0, 10)
  
)


draw_box <- function(
    
  x,
  y,
  label,
  width = 2
  
) {
  
  rect(
    
    x - width / 2,
    
    y - 0.5,
    
    x + width / 2,
    
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


# ------------------------------------------------------------
# TOP ROW
# ------------------------------------------------------------

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


# ------------------------------------------------------------
# MIDDLE ROW
# ------------------------------------------------------------

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


# ------------------------------------------------------------
# TOP ARROWS
# ------------------------------------------------------------

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


# ------------------------------------------------------------
# DOWN
# ------------------------------------------------------------

draw_arrow(
  
  9.5,
  7.5,
  
  9.5,
  5.5
  
)


# ------------------------------------------------------------
# BOTTOM
# ------------------------------------------------------------

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
  
  "GatekeeperX - Module 3 Discrete Event Simulation Architecture",
  
  cex.main = 1.5
  
)


dev.off()


# ============================================================
# 21. SAVE SIMULATED DATASET
# ============================================================

simulation_csv <- file.path(
  
  output_dir,
  
  "GatekeeperX_Module3_Simulation_Results.csv"
  
)


write.csv(
  
  data,
  
  simulation_csv,
  
  row.names = FALSE
  
)


# ============================================================
# 22. SAVE METRICS
# ============================================================

metrics <- data.frame(
  
  Metric = c(
    
    "Total Jobs",
    
    "Total Simulation Time (minutes)",
    
    "Average Waiting Time (minutes)",
    
    "Minimum Waiting Time (minutes)",
    
    "Maximum Waiting Time (minutes)",
    
    "Average Turnaround Time (minutes)",
    
    "Maximum Turnaround Time (minutes)",
    
    "Average Service Time (minutes)",
    
    "Average Queue Length",
    
    "Maximum Queue Length",
    
    "Server Utilization (%)",
    
    "Throughput (jobs/hour)",
    
    "Blocked Jobs",
    
    "Prioritized Jobs",
    
    "Pass Jobs",
    
    "Pass With Monitoring Jobs"
    
  ),
  
  
  Value = c(
    
    n,
    
    round(
      total_simulation_time,
      2
    ),
    
    round(
      average_waiting_time,
      2
    ),
    
    round(
      minimum_waiting_time,
      2
    ),
    
    round(
      maximum_waiting_time,
      2
    ),
    
    round(
      average_turnaround_time,
      2
    ),
    
    round(
      maximum_turnaround_time,
      2
    ),
    
    round(
      average_service_time,
      2
    ),
    
    round(
      average_queue_length,
      2
    ),
    
    maximum_queue_length,
    
    round(
      server_utilization * 100,
      2
    ),
    
    round(
      throughput_per_hour,
      2
    ),
    
    blocked_jobs,
    
    prioritized_jobs,
    
    passed_jobs,
    
    monitored_jobs
    
  )
  
)


metrics_csv <- file.path(
  
  output_dir,
  
  "GatekeeperX_Module3_Metrics.csv"
  
)


write.csv(
  
  metrics,
  
  metrics_csv,
  
  row.names = FALSE
  
)


# ============================================================
# 23. SAVE SECURITY DECISION SUMMARY
# ============================================================

decision_summary <- data.frame(
  
  Security_Decision =
    names(decision_counts),
  
  Number_of_Jobs =
    as.numeric(decision_counts)
  
)


decision_csv <- file.path(
  
  output_dir,
  
  "GatekeeperX_Module3_Decision_Summary.csv"
  
)


write.csv(
  
  decision_summary,
  
  decision_csv,
  
  row.names = FALSE
  
)


# ============================================================
# 24. SAVE TEXT REPORT
# ============================================================

report_file <- file.path(
  
  output_dir,
  
  "GatekeeperX_Module3_Report.txt"
  
)


sink(report_file)


cat(
  "====================================================\n"
)

cat(
  "GATEKEEPERX - MODULE 3 SIMULATION REPORT\n"
)

cat(
  "====================================================\n\n"
)


cat(
  "Simulation Type:\n"
)

cat(
  "Single-server priority queue with dynamic aging\n\n"
)


cat(
  "Total Jobs:",
  n,
  "\n"
)


cat(
  "Total Simulation Time:",
  round(
    total_simulation_time,
    2
  ),
  "minutes\n"
)


cat(
  "Average Waiting Time:",
  round(
    average_waiting_time,
    2
  ),
  "minutes\n"
)


cat(
  "Maximum Waiting Time:",
  round(
    maximum_waiting_time,
    2
  ),
  "minutes\n"
)


cat(
  "Average Turnaround Time:",
  round(
    average_turnaround_time,
    2
  ),
  "minutes\n"
)


cat(
  "Average Queue Length:",
  round(
    average_queue_length,
    2
  ),
  "\n"
)


cat(
  "Maximum Queue Length:",
  maximum_queue_length,
  "\n"
)


cat(
  "Server Utilization:",
  round(
    server_utilization * 100,
    2
  ),
  "%\n"
)


cat(
  "Throughput:",
  round(
    throughput_per_hour,
    2
  ),
  "jobs/hour\n\n"
)


cat(
  "Security Gate Results:\n"
)

print(
  decision_summary
)


cat(
  "\nDynamic Priority Aging Factor:",
  aging_factor,
  "\n"
)


cat(
  "\nDynamic Priority Formula:\n"
)

cat(
  "Dynamic Priority = Base GatekeeperX Priority + ",
  "Waiting Time × Aging Factor\n"
)


sink()


# ============================================================
# 25. FINAL OUTPUT
# ============================================================

cat("\n")
cat("====================================================\n")
cat(" MODULE 3 COMPLETED SUCCESSFULLY\n")
cat("====================================================\n")


cat(
  "Total Jobs:",
  n,
  "\n"
)


cat(
  "Average Waiting Time:",
  round(
    average_waiting_time,
    2
  ),
  "minutes\n"
)


cat(
  "Average Turnaround Time:",
  round(
    average_turnaround_time,
    2
  ),
  "minutes\n"
)


cat(
  "Average Queue Length:",
  round(
    average_queue_length,
    2
  ),
  "\n"
)


cat(
  "Maximum Queue Length:",
  maximum_queue_length,
  "\n"
)


cat(
  "Server Utilization:",
  round(
    server_utilization * 100,
    2
  ),
  "%\n"
)


cat(
  "Throughput:",
  round(
    throughput_per_hour,
    2
  ),
  "jobs/hour\n"
)


cat("\n")
cat(
  "Output Folder:",
  output_dir,
  "\n"
)


cat("\n")
cat(
  "Generated:\n"
)

cat(
  "1. Simulation Results CSV\n"
)

cat(
  "2. Metrics CSV\n"
)

cat(
  "3. Decision Summary CSV\n"
)

cat(
  "4. Waiting Time Plot\n"
)

cat(
  "5. Arrival vs Completion Plot\n"
)

cat(
  "6. Queue Length Plot\n"
)

cat(
  "7. Dynamic Priority Plot\n"
)

cat(
  "8. Simulation Architecture Diagram\n"
)

cat(
  "9. Simulation Report\n"
)


cat("\n")
cat("====================================================\n")

cat(
  "Next: Module 4 - Results & Evaluation\n"
)

cat("====================================================\n")