# ============================================================
# GATEKEEPERX
# MODULE 5 - COMPARISON WITH EXISTING APPROACHES
#
# Proposed:
#   GatekeeperX Dynamic Priority Queue
#
# Baseline 1:
#   FCFS - First Come First Served
#
# Baseline 2:
#   Static Priority Queue
#
# ============================================================


# ============================================================
# 1. SETUP
# ============================================================

rm(list = ls())

set.seed(42)

cat("\n")
cat("====================================================\n")
cat(" GATEKEEPERX - MODULE 5\n")
cat(" COMPARISON WITH EXISTING APPROACHES\n")
cat("====================================================\n")


# ============================================================
# 2. OUTPUT DIRECTORY
# ============================================================

output_dir <- "GatekeeperX_Module5_Output"

if (!dir.exists(output_dir)) {
  
  dir.create(output_dir)
  
}


# ============================================================
# 3. LOAD MODULE 3 DATA
# ============================================================

csv_file <- file.path(
  
  "GatekeeperX_Module3_Output",
  
  "GatekeeperX_Module3_Simulation_Results.csv"
  
)


if (!file.exists(csv_file)) {
  
  stop(
    "\nModule 3 results were not found.\n",
    "Run Module 3 before Module 5.\n"
  )
  
}


data <- read.csv(
  
  csv_file,
  
  stringsAsFactors = FALSE
  
)


cat(
  "\nDataset loaded successfully.\n"
)

cat(
  "Total jobs:",
  nrow(data),
  "\n"
)


# ============================================================
# 4. PARAMETERS
# ============================================================

aging_factor <- 0.50


# ============================================================
# 5. GENERIC QUEUE SIMULATION FUNCTION
# ============================================================

simulate_queue <- function(
    
  input_data,
  
  method,
  
  aging_factor = 0.50
  
) {
  
  
  data <- input_data
  
  
  # ----------------------------------------------------------
  # SORT ARRIVALS
  # ----------------------------------------------------------
  
  data <- data[
    order(
      data$Arrival_Time_Min
    ),
  ]
  
  
  row.names(data) <- NULL
  
  
  n <- nrow(data)
  
  
  # ----------------------------------------------------------
  # RESULT COLUMNS
  # ----------------------------------------------------------
  
  data$Service_Start_Time <- NA
  
  data$Completion_Time <- NA
  
  data$Waiting_Time <- NA
  
  data$Turnaround_Time <- NA
  
  data$Queue_Position <- NA
  
  data$Selection_Priority <- NA
  
  
  # ----------------------------------------------------------
  # SIMULATION VARIABLES
  # ----------------------------------------------------------
  
  current_time <- 0
  
  next_arrival <- 1
  
  waiting_queue <- integer(0)
  
  completed <- integer(0)
  
  queue_event <- 0
  
  
  # ----------------------------------------------------------
  # MAIN LOOP
  # ----------------------------------------------------------
  
  while (
    
    length(completed) < n
    
  ) {
    
    
    # ========================================================
    # ADD ARRIVING JOBS
    # ========================================================
    
    if (
      
      next_arrival <= n &&
      
      data$Arrival_Time_Min[
        next_arrival
      ] <= current_time
      
    ) {
      
      while (
        
        next_arrival <= n &&
        
        data$Arrival_Time_Min[
          next_arrival
        ] <= current_time
        
      ) {
        
        waiting_queue <- c(
          
          waiting_queue,
          
          next_arrival
          
        )
        
        next_arrival <-
          
          next_arrival + 1
        
      }
      
    }
    
    
    # ========================================================
    # EMPTY QUEUE
    # ========================================================
    
    if (
      
      length(waiting_queue) == 0
      
    ) {
      
      if (
        
        next_arrival <= n
        
      ) {
        
        current_time <-
          
          data$Arrival_Time_Min[
            next_arrival
          ]
        
        next
        
      }
      
    }
    
    
    # ========================================================
    # SELECT NEXT JOB
    # ========================================================
    
    if (
      
      length(waiting_queue) > 0
      
    ) {
      
      
      # ------------------------------------------------------
      # FCFS
      # ------------------------------------------------------
      
      if (
        
        method == "FCFS"
        
      ) {
        
        
        # First job that entered queue
        
        selected_position <- 1
        
        
        selected_priority <-
          
          data$GatekeeperX_Priority_Score[
            waiting_queue[
              selected_position
            ]
          ]
        
        
      }
      
      
      # ------------------------------------------------------
      # STATIC PRIORITY
      # ------------------------------------------------------
      
      else if (
        
        method == "Static Priority"
        
      ) {
        
        
        priorities <-
          
          data$GatekeeperX_Priority_Score[
            waiting_queue
          ]
        
        
        selected_position <-
          
          which.max(
            priorities
          )
        
        
        selected_priority <-
          
          priorities[
            selected_position
          ]
        
      }
      
      
      # ------------------------------------------------------
      # GATEKEEPERX
      # ------------------------------------------------------
      
      else if (
        
        method == "GatekeeperX"
        
      ) {
        
        
        dynamic_scores <- numeric(
          
          length(
            waiting_queue
          )
          
        )
        
        
        for (
          
          j in seq_along(
            waiting_queue
          )
          
        ) {
          
          
          index <-
            
            waiting_queue[j]
          
          
          # Current waiting time
          
          waiting_time <-
            
            max(
              
              0,
              
              current_time -
                data$Arrival_Time_Min[
                  index
                ]
              
            )
          
          
          # Aging bonus
          
          aging_bonus <-
            
            waiting_time *
            aging_factor
          
          
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
        
        
        selected_position <-
          
          which.max(
            dynamic_scores
          )
        
        
        selected_priority <-
          
          dynamic_scores[
            selected_position
          ]
        
      }
      
      
      # ======================================================
      # SELECT JOB
      # ======================================================
      
      selected_job <-
        
        waiting_queue[
          selected_position
        ]
      
      
      # ======================================================
      # REMOVE FROM QUEUE
      # ======================================================
      
      waiting_queue <-
        
        waiting_queue[
          -selected_position
        ]
      
      
      # ======================================================
      # QUEUE POSITION
      # ======================================================
      
      queue_event <-
        
        queue_event + 1
      
      
      data$Queue_Position[
        selected_job
      ] <- queue_event
      
      
      # ======================================================
      # SERVICE START
      # ======================================================
      
      service_start <-
        
        max(
          
          current_time,
          
          data$Arrival_Time_Min[
            selected_job
          ]
          
        )
      
      
      data$Service_Start_Time[
        selected_job
      ] <- service_start
      
      
      # ======================================================
      # WAITING TIME
      # ======================================================
      
      data$Waiting_Time[
        selected_job
      ] <-
        
        service_start -
        
        data$Arrival_Time_Min[
          selected_job
        ]
      
      
      # ======================================================
      # PRIORITY
      # ======================================================
      
      data$Selection_Priority[
        selected_job
      ] <-
        
        selected_priority
      
      
      # ======================================================
      # SERVICE
      # ======================================================
      
      service_time <-
        
        data$Service_Time_Min[
          selected_job
        ]
      
      
      # ======================================================
      # COMPLETION
      # ======================================================
      
      completion <-
        
        service_start +
        service_time
      
      
      data$Completion_Time[
        selected_job
      ] <- completion
      
      
      # ======================================================
      # TURNAROUND
      # ======================================================
      
      data$Turnaround_Time[
        selected_job
      ] <-
        
        completion -
        
        data$Arrival_Time_Min[
          selected_job
        ]
      
      
      # ======================================================
      # ADD COMPLETED JOB
      # ======================================================
      
      completed <-
        
        c(
          
          completed,
          
          selected_job
          
        )
      
      
      # ======================================================
      # UPDATE CLOCK
      # ======================================================
      
      current_time <-
        completion
      
    }
    
  }
  
  
  # ==========================================================
  # CALCULATE METRICS
  # ==========================================================
  
  total_time <-
    
    max(
      data$Completion_Time
    )
  
  
  total_service <-
    
    sum(
      data$Service_Time_Min
    )
  
  
  average_waiting <-
    
    mean(
      data$Waiting_Time
    )
  
  
  maximum_waiting <-
    
    max(
      data$Waiting_Time
    )
  
  
  average_turnaround <-
    
    mean(
      data$Turnaround_Time
    )
  
  
  maximum_turnaround <-
    
    max(
      data$Turnaround_Time
    )
  
  
  utilization <-
    
    total_service /
    total_time
  
  
  throughput <-
    
    n /
    total_time *
    60
  
  
  # ----------------------------------------------------------
  # Queue Length
  # ----------------------------------------------------------
  
  queue_lengths <- numeric(n)
  
  
  for (
    
    i in 1:n
    
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
    
    mean(
      queue_lengths
    )
  
  
  maximum_queue <-
    
    max(
      queue_lengths
    )
  
  
  # ==========================================================
  # CRITICAL JOB PERFORMANCE
  # ==========================================================
  
  critical_data <-
    
    data[
      data$Severity == "Critical",
    ]
  
  
  if (
    
    nrow(critical_data) > 0
    
  ) {
    
    critical_waiting <-
      
      mean(
        critical_data$Waiting_Time
      )
    
  } else {
    
    critical_waiting <- 0
    
  }
  
  
  # ==========================================================
  # HIGH JOB PERFORMANCE
  # ==========================================================
  
  high_data <-
    
    data[
      data$Severity == "High",
    ]
  
  
  if (
    
    nrow(high_data) > 0
    
  ) {
    
    high_waiting <-
      
      mean(
        high_data$Waiting_Time
      )
    
  } else {
    
    high_waiting <- 0
    
  }
  
  
  # ==========================================================
  # RETURN
  # ==========================================================
  
  list(
    
    data = data,
    
    method = method,
    
    average_waiting =
      average_waiting,
    
    maximum_waiting =
      maximum_waiting,
    
    average_turnaround =
      average_turnaround,
    
    maximum_turnaround =
      maximum_turnaround,
    
    average_queue =
      average_queue,
    
    maximum_queue =
      maximum_queue,
    
    throughput =
      throughput,
    
    utilization =
      utilization,
    
    total_time =
      total_time,
    
    critical_waiting =
      critical_waiting,
    
    high_waiting =
      high_waiting
    
  )
  
}


# ============================================================
# 6. RUN THREE APPROACHES
# ============================================================

cat("\n")
cat("Running FCFS...\n")


fcfs_result <-
  
  simulate_queue(
    
    data,
    
    "FCFS",
    
    aging_factor
    
  )


cat("Running Static Priority...\n")


static_result <-
  
  simulate_queue(
    
    data,
    
    "Static Priority",
    
    aging_factor
    
  )


cat("Running GatekeeperX...\n")


gatekeeper_result <-
  
  simulate_queue(
    
    data,
    
    "GatekeeperX",
    
    aging_factor
    
  )


# ============================================================
# 7. CREATE COMPARISON TABLE
# ============================================================

comparison <- data.frame(
  
  Approach = c(
    
    "FCFS",
    
    "Static Priority",
    
    "GatekeeperX"
    
  ),
  
  Average_Waiting_Time = c(
    
    fcfs_result$average_waiting,
    
    static_result$average_waiting,
    
    gatekeeper_result$average_waiting
    
  ),
  
  Maximum_Waiting_Time = c(
    
    fcfs_result$maximum_waiting,
    
    static_result$maximum_waiting,
    
    gatekeeper_result$maximum_waiting
    
  ),
  
  Average_Turnaround_Time = c(
    
    fcfs_result$average_turnaround,
    
    static_result$average_turnaround,
    
    gatekeeper_result$average_turnaround
    
  ),
  
  Maximum_Turnaround_Time = c(
    
    fcfs_result$maximum_turnaround,
    
    static_result$maximum_turnaround,
    
    gatekeeper_result$maximum_turnaround
    
  ),
  
  Average_Queue_Length = c(
    
    fcfs_result$average_queue,
    
    static_result$average_queue,
    
    gatekeeper_result$average_queue
    
  ),
  
  Maximum_Queue_Length = c(
    
    fcfs_result$maximum_queue,
    
    static_result$maximum_queue,
    
    gatekeeper_result$maximum_queue
    
  ),
  
  Throughput_Jobs_Per_Hour = c(
    
    fcfs_result$throughput,
    
    static_result$throughput,
    
    gatekeeper_result$throughput
    
  ),
  
  Server_Utilization_Percent = c(
    
    fcfs_result$utilization * 100,
    
    static_result$utilization * 100,
    
    gatekeeper_result$utilization * 100
    
  ),
  
  Critical_Average_Waiting = c(
    
    fcfs_result$critical_waiting,
    
    static_result$critical_waiting,
    
    gatekeeper_result$critical_waiting
    
  ),
  
  High_Average_Waiting = c(
    
    fcfs_result$high_waiting,
    
    static_result$high_waiting,
    
    gatekeeper_result$high_waiting
    
  )
  
)


# ============================================================
# 8. ROUND NUMBERS
# ============================================================

comparison[, -1] <-
  
  round(
    
    comparison[, -1],
    
    2
    
  )


# ============================================================
# 9. IMPROVEMENT CALCULATION
# ============================================================

fcfs_waiting <-
  
  fcfs_result$average_waiting


static_waiting <-
  
  static_result$average_waiting


gatekeeper_waiting <-
  
  gatekeeper_result$average_waiting


# ------------------------------------------------------------
# Improvement vs FCFS
# ------------------------------------------------------------

improvement_vs_fcfs <-
  
  (
    
    fcfs_waiting -
      gatekeeper_waiting
    
  ) /
  
  fcfs_waiting *
  
  100


# ------------------------------------------------------------
# Improvement vs Static Priority
# ------------------------------------------------------------

improvement_vs_static <-
  
  (
    
    static_waiting -
      gatekeeper_waiting
    
  ) /
  
  static_waiting *
  
  100


# ============================================================
# 10. CRITICAL JOB IMPROVEMENT
# ============================================================

critical_fcfs <-
  
  fcfs_result$critical_waiting


critical_static <-
  
  static_result$critical_waiting


critical_gatekeeper <-
  
  gatekeeper_result$critical_waiting


critical_improvement_vs_fcfs <-
  
  (
    
    critical_fcfs -
      critical_gatekeeper
    
  ) /
  
  critical_fcfs *
  
  100


critical_improvement_vs_static <-
  
  (
    
    critical_static -
      critical_gatekeeper
    
  ) /
  
  critical_static *
  
  100


# ============================================================
# 11. OVERALL COMPARISON SCORE
# ============================================================

# Lower waiting time = better
# Lower turnaround = better
# Lower queue = better
# Higher throughput = better
# Security-aware critical processing = better


score_waiting <-
  
  gatekeeper_result$average_waiting <=
  
  min(
    
    fcfs_result$average_waiting,
    
    static_result$average_waiting
    
  )


score_turnaround <-
  
  gatekeeper_result$average_turnaround <=
  
  min(
    
    fcfs_result$average_turnaround,
    
    static_result$average_turnaround
    
  )


score_queue <-
  
  gatekeeper_result$average_queue <=
  
  min(
    
    fcfs_result$average_queue,
    
    static_result$average_queue
    
  )


score_throughput <-
  
  gatekeeper_result$throughput >=
  
  max(
    
    fcfs_result$throughput,
    
    static_result$throughput
    
  )


score_critical <-
  
  gatekeeper_result$critical_waiting <=
  
  min(
    
    fcfs_result$critical_waiting,
    
    static_result$critical_waiting
    
  )


scores <- c(
  
  score_waiting,
  
  score_turnaround,
  
  score_queue,
  
  score_throughput,
  
  score_critical
  
)


gatekeeper_score <-
  
  mean(
    scores
  ) * 100


# ============================================================
# 12. DETERMINE WINNER
# ============================================================

# Normalize metrics for ranking


waiting_rank <-
  
  rank(
    
    comparison$Average_Waiting_Time,
    
    ties.method = "average"
    
  )


turnaround_rank <-
  
  rank(
    
    comparison$Average_Turnaround_Time,
    
    ties.method = "average"
    
  )


queue_rank <-
  
  rank(
    
    comparison$Average_Queue_Length,
    
    ties.method = "average"
    
  )


throughput_rank <-
  
  rank(
    
    -comparison$Throughput_Jobs_Per_Hour,
    
    ties.method = "average"
    
  )


comparison$Overall_Rank_Score <-
  
  waiting_rank +
  
  turnaround_rank +
  
  queue_rank +
  
  throughput_rank


winner_index <-
  
  which.min(
    
    comparison$Overall_Rank_Score
    
  )


winner <-
  
  comparison$Approach[
    winner_index
  ]


# ============================================================
# 13. PRINT RESULTS
# ============================================================

cat("\n")
cat("====================================================\n")
cat(" COMPARISON RESULTS\n")
cat("====================================================\n")


print(
  comparison
)


cat("\n")


cat(
  "GatekeeperX Improvement vs FCFS:",
  round(
    improvement_vs_fcfs,
    2
  ),
  "%\n"
)


cat(
  "GatekeeperX Improvement vs Static Priority:",
  round(
    improvement_vs_static,
    2
  ),
  "%\n"
)


cat(
  "Critical Job Improvement vs FCFS:",
  round(
    critical_improvement_vs_fcfs,
    2
  ),
  "%\n"
)


cat(
  "Critical Job Improvement vs Static Priority:",
  round(
    critical_improvement_vs_static,
    2
  ),
  "%\n"
)


cat(
  "GatekeeperX Performance Score:",
  round(
    gatekeeper_score,
    2
  ),
  "%\n"
)


cat(
  "Overall Winner:",
  winner,
  "\n"
)


# ============================================================
# 14. SAVE COMPARISON CSV
# ============================================================

write.csv(
  
  comparison,
  
  file.path(
    
    output_dir,
    
    "GatekeeperX_Module5_Comparison.csv"
    
  ),
  
  row.names = FALSE
  
)


# ============================================================
# 15. SAVE IMPROVEMENT DATA
# ============================================================

improvement <- data.frame(
  
  Comparison = c(
    
    "GatekeeperX vs FCFS",
    
    "GatekeeperX vs Static Priority",
    
    "Critical Jobs vs FCFS",
    
    "Critical Jobs vs Static Priority"
    
  ),
  
  Improvement_Percent = c(
    
    improvement_vs_fcfs,
    
    improvement_vs_static,
    
    critical_improvement_vs_fcfs,
    
    critical_improvement_vs_static
    
  )
  
)


improvement$Improvement_Percent <-
  
  round(
    
    improvement$Improvement_Percent,
    
    2
    
  )


write.csv(
  
  improvement,
  
  file.path(
    
    output_dir,
    
    "GatekeeperX_Module5_Improvements.csv"
    
  ),
  
  row.names = FALSE
  
)


# ============================================================
# 16. PLOT 1
# AVERAGE WAITING TIME
# ============================================================

png(
  
  file.path(
    
    output_dir,
    
    "Module5_Average_Waiting_Comparison.png"
    
  ),
  
  width = 1000,
  
  height = 700
  
)


values <-
  
  comparison$Average_Waiting_Time


barplot(
  
  values,
  
  names.arg =
    comparison$Approach,
  
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
    "Average Waiting Time (minutes)"
  
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


dev.off()


# ============================================================
# 17. PLOT 2
# TURNAROUND TIME
# ============================================================

png(
  
  file.path(
    
    output_dir,
    
    "Module5_Turnaround_Comparison.png"
    
  ),
  
  width = 1000,
  
  height = 700
  
)


values <-
  
  comparison$Average_Turnaround_Time


barplot(
  
  values,
  
  names.arg =
    comparison$Approach,
  
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
    "Average Turnaround Time (minutes)"
  
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


dev.off()


# ============================================================
# 18. PLOT 3
# QUEUE LENGTH
# ============================================================

png(
  
  file.path(
    
    output_dir,
    
    "Module5_Queue_Comparison.png"
    
  ),
  
  width = 1000,
  
  height = 700
  
)


values <-
  
  comparison$Average_Queue_Length


barplot(
  
  values,
  
  names.arg =
    comparison$Approach,
  
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


dev.off()


# ============================================================
# 19. PLOT 4
# THROUGHPUT
# ============================================================

png(
  
  file.path(
    
    output_dir,
    
    "Module5_Throughput_Comparison.png"
    
  ),
  
  width = 1000,
  
  height = 700
  
)


values <-
  
  comparison$Throughput_Jobs_Per_Hour


barplot(
  
  values,
  
  names.arg =
    comparison$Approach,
  
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


dev.off()


# ============================================================
# 20. PLOT 5
# CRITICAL JOB WAITING
# ============================================================

png(
  
  file.path(
    
    output_dir,
    
    "Module5_Critical_Job_Comparison.png"
    
  ),
  
  width = 1000,
  
  height = 700
  
)


values <-
  
  comparison$Critical_Average_Waiting


barplot(
  
  values,
  
  names.arg =
    comparison$Approach,
  
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
    "Critical Job Waiting Time (minutes)"
  
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


dev.off()

# ============================================================
# 21. PLOT 6
# NORMALIZED MULTI-METRIC COMPARISON
# ============================================================

normalized <- comparison

normalized$Waiting <-
  normalized$Average_Waiting_Time /
  max(normalized$Average_Waiting_Time)

normalized$Turnaround <-
  normalized$Average_Turnaround_Time /
  max(normalized$Average_Turnaround_Time)

normalized$Queue <-
  normalized$Average_Queue_Length /
  max(normalized$Average_Queue_Length)

normalized$Throughput <-
  normalized$Throughput_Jobs_Per_Hour /
  max(normalized$Throughput_Jobs_Per_Hour)


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


png(
  
  file.path(
    output_dir,
    "Module5_Normalized_Performance.png"
  ),
  
  width = 1200,
  
  height = 800
  
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
    "Normalized Performance Comparison",
  
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


dev.off()
# ============================================================
# 22. SAVE FINAL REPORT
# ============================================================

report_file <- file.path(
  
  output_dir,
  
  "GatekeeperX_Module5_Report.txt"
  
)


sink(report_file)


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
  "Approaches Compared:\n\n"
)


cat(
  "1. FCFS - First Come First Served\n"
)

cat(
  "2. Static Priority Queue\n"
)

cat(
  "3. GatekeeperX Dynamic Priority Queue\n\n"
)


cat(
  "----------------------------------------------------\n"
)

cat(
  "PERFORMANCE COMPARISON\n"
)

cat(
  "----------------------------------------------------\n\n"
)


print(
  comparison
)


cat(
  "\n----------------------------------------------------\n"
)


cat(
  "IMPROVEMENTS\n"
)


cat(
  "----------------------------------------------------\n\n"
)


cat(
  "GatekeeperX vs FCFS:",
  round(
    improvement_vs_fcfs,
    2
  ),
  "% waiting-time improvement\n"
)


cat(
  "GatekeeperX vs Static Priority:",
  round(
    improvement_vs_static,
    2
  ),
  "% waiting-time improvement\n"
)


cat(
  "Critical jobs vs FCFS:",
  round(
    critical_improvement_vs_fcfs,
    2
  ),
  "% improvement\n"
)


cat(
  "Critical jobs vs Static Priority:",
  round(
    critical_improvement_vs_static,
    2
  ),
  "% improvement\n\n"
)


cat(
  "GatekeeperX Score:",
  round(
    gatekeeper_score,
    2
  ),
  "%\n"
)


cat(
  "Overall Winner:",
  winner,
  "\n\n"
)


cat(
  "Conclusion:\n"
)


if (
  
  winner == "GatekeeperX"
  
) {
  
  cat(
    
    paste(
      
      "GatekeeperX demonstrated the best overall",
      
      "performance among the three evaluated",
      
      "queueing approaches under the simulated",
      
      "workload. Its dynamic priority mechanism",
      
      "combines security risk with waiting-time",
      
      "aging, allowing security-critical jobs to",
      
      "receive appropriate priority while reducing",
      
      "the possibility of starvation."
      
    )
    
  )
  
} else {
  
  cat(
    
    paste(
      
      "The comparison shows that the selected",
      
      "baseline approach performed better on the",
      
      "combined metrics for this particular",
      
      "workload. GatekeeperX should therefore",
      
      "be further tuned using different aging",
      
      "factors and workload conditions."
      
    )
    
  )
  
}


cat(
  "\n\n====================================================\n"
)


sink()


# ============================================================
# 23. FINAL MESSAGE
# ============================================================

cat("\n")
cat("====================================================\n")
cat(" MODULE 5 COMPLETED SUCCESSFULLY\n")
cat("====================================================\n")


cat(
  "Overall Winner:",
  winner,
  "\n"
)


cat(
  "GatekeeperX Score:",
  round(
    gatekeeper_score,
    2
  ),
  "%\n"
)


cat(
  "Improvement vs FCFS:",
  round(
    improvement_vs_fcfs,
    2
  ),
  "%\n"
)


cat(
  "Improvement vs Static Priority:",
  round(
    improvement_vs_static,
    2
  ),
  "%\n"
)


cat("\n")
cat(
  "Output Folder:",
  output_dir,
  "\n"
)


cat("\nGenerated files:\n")

cat(
  "1. Comparison CSV\n"
)

cat(
  "2. Improvement CSV\n"
)

cat(
  "3. Average Waiting Comparison Plot\n"
)

cat(
  "4. Turnaround Comparison Plot\n"
)

cat(
  "5. Queue Comparison Plot\n"
)

cat(
  "6. Throughput Comparison Plot\n"
)

cat(
  "7. Critical Job Comparison Plot\n"
)

cat(
  "8. Normalized Performance Plot\n"
)

cat(
  "9. Module 5 Report\n"
)


cat("\n")
cat("====================================================\n")