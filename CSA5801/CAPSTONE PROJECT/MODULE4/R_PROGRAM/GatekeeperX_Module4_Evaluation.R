# ============================================================
# GATEKEEPERX
# MODULE 4 - RESULTS & EVALUATION
#
# Queueing Simulation for Intelligent DevSecOps Security Gates
# ============================================================


# ============================================================
# 1. SETUP
# ============================================================

rm(list = ls())

cat("\n")
cat("====================================================\n")
cat(" GATEKEEPERX - MODULE 4\n")
cat(" RESULTS & EVALUATION\n")
cat("====================================================\n")


# ============================================================
# 2. OUTPUT DIRECTORY
# ============================================================

output_dir <- "GatekeeperX_Module4_Output"

if (!dir.exists(output_dir)) {
  
  dir.create(output_dir)
  
}


# ============================================================
# 3. LOAD MODULE 3 RESULTS
# ============================================================

csv_file <- file.path(
  
  "GatekeeperX_Module3_Output",
  
  "GatekeeperX_Module3_Simulation_Results.csv"
  
)


if (!file.exists(csv_file)) {
  
  stop(
    "\nModule 3 simulation results were not found.\n",
    "Please run Module 3 first.\n"
  )
  
}


data <- read.csv(
  
  csv_file,
  
  stringsAsFactors = FALSE
  
)


cat(
  "\nModule 3 simulation data loaded.\n"
)


cat(
  "Number of Jobs:",
  nrow(data),
  "\n"
)


# ============================================================
# 4. VALIDATE REQUIRED COLUMNS
# ============================================================

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
# 5. BASIC METRICS
# ============================================================

total_jobs <- nrow(data)


total_simulation_time <-
  
  max(
    data$Completion_Time
  )


# ------------------------------------------------------------
# WAITING TIME
# ------------------------------------------------------------

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


# ------------------------------------------------------------
# TURNAROUND TIME
# ------------------------------------------------------------

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


# ------------------------------------------------------------
# SERVICE TIME
# ------------------------------------------------------------

average_service_time <-
  
  mean(
    data$Service_Time_Min
  )


# ============================================================
# 6. SERVER UTILIZATION
# ============================================================

total_service_time <-
  
  sum(
    data$Service_Time_Min
  )


server_utilization <-
  
  total_service_time /
  total_simulation_time


server_utilization_percentage <-
  
  server_utilization * 100


# ============================================================
# 7. THROUGHPUT
# ============================================================

throughput_jobs_per_minute <-
  
  total_jobs /
  total_simulation_time


throughput_jobs_per_hour <-
  
  throughput_jobs_per_minute *
  60


# ============================================================
# 8. QUEUE LENGTH
# ============================================================

# Estimate queue length at service completion events

queue_length <- numeric(
  total_jobs
)


for (
  
  i in 1:total_jobs
  
) {
  
  queue_length[i] <-
    
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
    queue_length
  )


maximum_queue_length <-
  
  max(
    queue_length
  )


# ============================================================
# 9. SECURITY DECISION ANALYSIS
# ============================================================

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


# ============================================================
# 10. DECISION PERCENTAGES
# ============================================================

blocked_percentage <-
  
  blocked_jobs /
  total_jobs *
  100


prioritized_percentage <-
  
  prioritized_jobs /
  total_jobs *
  100


passed_percentage <-
  
  passed_jobs /
  total_jobs *
  100


monitored_percentage <-
  
  monitored_jobs /
  total_jobs *
  100


# ============================================================
# 11. PRIORITY ANALYSIS
# ============================================================

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


median_priority <-
  
  median(
    
    data$GatekeeperX_Priority_Score
    
  )


# ============================================================
# 12. DYNAMIC PRIORITY ANALYSIS
# ============================================================

average_dynamic_priority <-
  
  mean(
    
    data$Dynamic_Priority
    
  )


priority_increase <-
  
  data$Dynamic_Priority -
  
  data$GatekeeperX_Priority_Score


average_priority_increase <-
  
  mean(
    priority_increase
  )


maximum_priority_increase <-
  
  max(
    priority_increase
  )


# ============================================================
# 13. SEVERITY-WISE WAITING TIME
# ============================================================

severity_waiting <- aggregate(
  
  Actual_Waiting_Time ~
    
    Severity,
  
  data = data,
  
  FUN = mean
  
)


names(
  severity_waiting
)[2] <-
  
  "Average_Waiting_Time"


severity_waiting$Average_Waiting_Time <-
  
  round(
    
    severity_waiting$Average_Waiting_Time,
    
    2
    
  )


# ============================================================
# 14. SEVERITY-WISE TURNAROUND
# ============================================================

severity_turnaround <- aggregate(
  
  Turnaround_Time ~
    
    Severity,
  
  data = data,
  
  FUN = mean
  
)


names(
  severity_turnaround
)[2] <-
  
  "Average_Turnaround_Time"


severity_turnaround$Average_Turnaround_Time <-
  
  round(
    
    severity_turnaround$Average_Turnaround_Time,
    
    2
    
  )


# ============================================================
# 15. PRIORITY-CLASS WAITING TIME
# ============================================================

priority_waiting <- aggregate(
  
  Actual_Waiting_Time ~
    
    Priority_Class,
  
  data = data,
  
  FUN = mean
  
)


names(
  priority_waiting
)[2] <-
  
  "Average_Waiting_Time"


priority_waiting$Average_Waiting_Time <-
  
  round(
    
    priority_waiting$Average_Waiting_Time,
    
    2
    
  )


# ============================================================
# 16. RISK-PRIORITY CORRELATION
# ============================================================

risk_priority_correlation <-
  
  cor(
    
    data$Risk_Score,
    
    data$GatekeeperX_Priority_Score,
    
    method = "pearson"
    
  )


# ============================================================
# 17. PRIORITY-SEVERITY CORRELATION
# ============================================================

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


# ============================================================
# 18. SECURITY PRIORITIZATION CHECK
# ============================================================

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


# ============================================================
# 19. EVALUATION CRITERIA
# ============================================================

# These are project evaluation thresholds,
# not industry-standard thresholds.


if (
  
  server_utilization < 1
  
) {
  
  queue_stability <-
    "PASS"
  
} else {
  
  queue_stability <-
    "FAIL"
  
}


if (
  
  average_waiting_time <= 10
  
) {
  
  waiting_evaluation <-
    "PASS"
  
} else {
  
  waiting_evaluation <-
    "NEEDS IMPROVEMENT"
  
}


if (
  
  average_turnaround_time <= 20
  
) {
  
  turnaround_evaluation <-
    "PASS"
  
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
  
  risk_priority_correlation >= 0.70
  
) {
  
  risk_alignment <-
    "PASS"
  
} else {
  
  risk_alignment <-
    "NEEDS IMPROVEMENT"
  
}


# ============================================================
# 20. OVERALL EVALUATION
# ============================================================

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


# ============================================================
# 21. PRINT RESULTS
# ============================================================

cat("\n")
cat("====================================================\n")
cat(" MODULE 4 - RESULTS\n")
cat("====================================================\n")


cat(
  "\nTotal Jobs:",
  total_jobs,
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
    server_utilization_percentage,
    2
  ),
  "%\n"
)


cat(
  "Throughput:",
  round(
    throughput_jobs_per_hour,
    2
  ),
  "jobs/hour\n"
)


cat(
  "\nAverage GatekeeperX Priority:",
  round(
    average_priority,
    2
  ),
  "\n"
)


cat(
  "Risk-Priority Correlation:",
  round(
    risk_priority_correlation,
    3
  ),
  "\n"
)


cat(
  "\nOverall Evaluation:",
  overall_result,
  "\n"
)


cat(
  "Evaluation Score:",
  round(
    evaluation_score,
    2
  ),
  "%\n"
)


# ============================================================
# 22. PLOT 1
# WAITING TIME
# ============================================================

png(
  
  file.path(
    
    output_dir,
    
    "Module4_Waiting_Time_Evaluation.png"
    
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
    "GatekeeperX - Waiting Time Evaluation",
  
  xlab =
    "Waiting Time (minutes)",
  
  ylab =
    "Number of Jobs"
  
)


abline(
  
  v =
    average_waiting_time,
  
  col = "red",
  
  lwd = 3,
  
  lty = 2
  
)


legend(
  
  "topright",
  
  legend =
    paste(
      "Average =",
      round(
        average_waiting_time,
        2
      ),
      "min"
    ),
  
  col = "red",
  
  lwd = 3
  
)


grid()


dev.off()


# ============================================================
# 23. PLOT 2
# SEVERITY VS WAITING
# ============================================================

png(
  
  file.path(
    
    output_dir,
    
    "Module4_Severity_vs_Waiting.png"
    
  ),
  
  width = 1000,
  
  height = 700
  
)


severity_order <-
  
  c(
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


barplot(
  
  severity_values,
  
  names.arg =
    severity_order,
  
  col = "orange",
  
  border = "white",
  
  main =
    "Average Waiting Time by Severity",
  
  xlab =
    "Security Severity",
  
  ylab =
    "Average Waiting Time (minutes)"
  
)


text(
  
  seq_along(
    severity_values
  ),
  
  severity_values,
  
  labels =
    round(
      severity_values,
      2
    ),
  
  pos = 3
  
)


grid()


dev.off()


# ============================================================
# 24. PLOT 3
# RISK VS PRIORITY
# ============================================================

png(
  
  file.path(
    
    output_dir,
    
    "Module4_Risk_Priority_Evaluation.png"
    
  ),
  
  width = 1000,
  
  height = 700
  
)


plot(
  
  data$Risk_Score,
  
  data$GatekeeperX_Priority_Score,
  
  pch = 19,
  
  col = "purple",
  
  main =
    "Risk Score vs GatekeeperX Priority",
  
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
  
  lwd = 3
  
)


grid()


dev.off()


# ============================================================
# 25. PLOT 4
# TURNAROUND TIME
# ============================================================

png(
  
  file.path(
    
    output_dir,
    
    "Module4_Turnaround_Time.png"
    
  ),
  
  width = 1000,
  
  height = 700
  
)


hist(
  
  data$Turnaround_Time,
  
  breaks = 25,
  
  col = "darkgreen",
  
  border = "white",
  
  main =
    "GatekeeperX - Turnaround Time",
  
  xlab =
    "Turnaround Time (minutes)",
  
  ylab =
    "Number of Jobs"
  
)


abline(
  
  v =
    average_turnaround_time,
  
  col = "red",
  
  lwd = 3,
  
  lty = 2
  
)


grid()


dev.off()


# ============================================================
# 26. PLOT 5
# SECURITY DECISIONS
# ============================================================

png(
  
  file.path(
    
    output_dir,
    
    "Module4_Security_Decisions.png"
    
  ),
  
  width = 1000,
  
  height = 700
  
)


barplot(
  
  decision_counts,
  
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
  
  seq_along(
    decision_counts
  ),
  
  decision_counts,
  
  labels =
    decision_counts,
  
  pos = 3
  
)


grid()


dev.off()


# ============================================================
# 27. PLOT 6
# PRIORITY CLASS
# ============================================================

png(
  
  file.path(
    
    output_dir,
    
    "Module4_Priority_Class.png"
    
  ),
  
  width = 1000,
  
  height = 700
  
)


priority_counts <-
  
  table(
    data$Priority_Class
  )


barplot(
  
  priority_counts,
  
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
  
  seq_along(
    priority_counts
  ),
  
  priority_counts,
  
  labels =
    priority_counts,
  
  pos = 3
  
)


grid()


dev.off()


# ============================================================
# 28. EVALUATION SCORE PLOT
# ============================================================

png(
  
  file.path(
    
    output_dir,
    
    "Module4_Evaluation_Score.png"
    
  ),
  
  width = 1000,
  
  height = 700
  
)


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
  
  xlab = "Evaluation Score (%)",
  
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
  
  evaluation_score,
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
  
  evaluation_score,
  
  0.5,
  
  labels =
    paste(
      round(
        evaluation_score,
        1
      ),
      "%"
    ),
  
  cex = 1.5,
  
  font = 2
  
)


dev.off()


# ============================================================
# 29. SAVE EVALUATION METRICS
# ============================================================

metrics <- data.frame(
  
  Metric = c(
    
    "Total Jobs",
    
    "Total Simulation Time",
    
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
    
    "Throughput Jobs Per Hour",
    
    "Blocked Jobs",
    
    "Prioritized Jobs",
    
    "Passed Jobs",
    
    "Monitored Jobs",
    
    "Average Priority Score",
    
    "Maximum Priority Score",
    
    "Minimum Priority Score",
    
    "Average Dynamic Priority",
    
    "Average Priority Increase",
    
    "Risk-Priority Correlation",
    
    "Severity-Priority Correlation",
    
    "Evaluation Score"
    
  ),
  
  Value = c(
    
    total_jobs,
    
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
      median_waiting_time,
      2
    ),
    
    round(
      average_turnaround_time,
      2
    ),
    
    round(
      minimum_turnaround_time,
      2
    ),
    
    round(
      maximum_turnaround_time,
      2
    ),
    
    round(
      median_turnaround_time,
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
    
    paste(
      round(
        server_utilization_percentage,
        2
      ),
      "%"
    ),
    
    round(
      throughput_jobs_per_hour,
      2
    ),
    
    blocked_jobs,
    
    prioritized_jobs,
    
    passed_jobs,
    
    monitored_jobs,
    
    round(
      average_priority,
      2
    ),
    
    round(
      maximum_priority,
      2
    ),
    
    round(
      minimum_priority,
      2
    ),
    
    round(
      average_dynamic_priority,
      2
    ),
    
    round(
      average_priority_increase,
      2
    ),
    
    round(
      risk_priority_correlation,
      3
    ),
    
    round(
      severity_priority_correlation,
      3
    ),
    
    round(
      evaluation_score,
      2
    )
    
  )
  
)


write.csv(
  
  metrics,
  
  file.path(
    
    output_dir,
    
    "GatekeeperX_Module4_Metrics.csv"
    
  ),
  
  row.names = FALSE
  
)


# ============================================================
# 30. SAVE SEVERITY ANALYSIS
# ============================================================

write.csv(
  
  severity_waiting,
  
  file.path(
    
    output_dir,
    
    "GatekeeperX_Severity_Waiting_Analysis.csv"
    
  ),
  
  row.names = FALSE
  
)


write.csv(
  
  severity_turnaround,
  
  file.path(
    
    output_dir,
    
    "GatekeeperX_Severity_Turnaround_Analysis.csv"
    
  ),
  
  row.names = FALSE
  
)


write.csv(
  
  priority_waiting,
  
  file.path(
    
    output_dir,
    
    "GatekeeperX_Priority_Waiting_Analysis.csv"
    
  ),
  
  row.names = FALSE
  
)


# ============================================================
# 31. SAVE EVALUATION RESULTS
# ============================================================

write.csv(
  
  evaluation_results,
  
  file.path(
    
    output_dir,
    
    "GatekeeperX_Evaluation_Results.csv"
    
  ),
  
  row.names = FALSE
  
)


# ============================================================
# 32. SAVE TEXT REPORT
# ============================================================

report_file <- file.path(
  
  output_dir,
  
  "GatekeeperX_Module4_Report.txt"
  
)


sink(report_file)


cat(
  "====================================================\n"
)

cat(
  "GATEKEEPERX - MODULE 4\n"
)

cat(
  "RESULTS & EVALUATION REPORT\n"
)

cat(
  "====================================================\n\n"
)


cat(
  "1. SIMULATION PERFORMANCE\n\n"
)


cat(
  "Total Jobs:",
  total_jobs,
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
    server_utilization_percentage,
    2
  ),
  "%\n"
)


cat(
  "Throughput:",
  round(
    throughput_jobs_per_hour,
    2
  ),
  "jobs/hour\n\n"
)


cat(
  "2. SECURITY PERFORMANCE\n\n"
)


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
  "Passed Jobs:",
  passed_jobs,
  "\n"
)


cat(
  "Pass With Monitoring:",
  monitored_jobs,
  "\n\n"
)


cat(
  "3. INTELLIGENT PRIORITY ANALYSIS\n\n"
)


cat(
  "Average Priority:",
  round(
    average_priority,
    2
  ),
  "\n"
)


cat(
  "Average Dynamic Priority:",
  round(
    average_dynamic_priority,
    2
  ),
  "\n"
)


cat(
  "Average Priority Increase:",
  round(
    average_priority_increase,
    2
  ),
  "\n"
)


cat(
  "Risk-Priority Correlation:",
  round(
    risk_priority_correlation,
    3
  ),
  "\n"
)


cat(
  "Severity-Priority Correlation:",
  round(
    severity_priority_correlation,
    3
  ),
  "\n\n"
)


cat(
  "4. EVALUATION\n\n"
)


print(
  evaluation_results
)


cat(
  "\nOverall Evaluation:",
  overall_result,
  "\n"
)


cat(
  "Evaluation Score:",
  round(
    evaluation_score,
    2
  ),
  "%\n\n"
)


cat(
  "====================================================\n"
)

cat(
  "END OF REPORT\n"
)

cat(
  "====================================================\n"
)


sink()


# ============================================================
# 33. FINAL OUTPUT
# ============================================================

cat("\n")
cat("====================================================\n")
cat(" MODULE 4 COMPLETED SUCCESSFULLY\n")
cat("====================================================\n")


cat(
  "Evaluation Score:",
  round(
    evaluation_score,
    2
  ),
  "%\n"
)


cat(
  "Overall Result:",
  overall_result,
  "\n"
)


cat(
  "\nOutput Folder:",
  output_dir,
  "\n"
)


cat("\nGenerated:\n")

cat(
  "1. Module 4 Metrics CSV\n"
)

cat(
  "2. Severity Waiting Analysis\n"
)

cat(
  "3. Severity Turnaround Analysis\n"
)

cat(
  "4. Priority Waiting Analysis\n"
)

cat(
  "5. Evaluation Results CSV\n"
)

cat(
  "6. Waiting Time Plot\n"
)

cat(
  "7. Severity vs Waiting Plot\n"
)

cat(
  "8. Risk vs Priority Plot\n"
)

cat(
  "9. Turnaround Time Plot\n"
)

cat(
  "10. Security Decision Plot\n"
)

cat(
  "11. Priority Class Plot\n"
)

cat(
  "12. Evaluation Score Plot\n"
)

cat(
  "13. Module 4 Report\n"
)


cat("\n")
cat("====================================================\n")

cat(
  "Next: Module 5 - Comparison with Existing Approaches\n"
)

cat("====================================================\n")