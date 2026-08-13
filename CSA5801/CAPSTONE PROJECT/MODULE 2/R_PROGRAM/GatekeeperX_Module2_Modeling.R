# ============================================================
# GATEKEEPERX
# MODULE 2 - QUEUEING & INTELLIGENT SECURITY-GATE MODELING
# ============================================================

# ============================================================
# 1. SETUP
# ============================================================

rm(list = ls())

cat("\n")
cat("====================================================\n")
cat(" GATEKEEPERX - MODULE 2: MODELING\n")
cat("====================================================\n")


# ------------------------------------------------------------
# OUTPUT DIRECTORY
# ------------------------------------------------------------

output_dir <- "GatekeeperX_Module2_Output"

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


# ============================================================
# 2. LOAD MODULE 1 DATASET
# ============================================================

csv_file <- file.path(
  "GatekeeperX_Output",
  "GatekeeperX_Module1_1000_Records.csv"
)


# Check whether CSV exists

if (!file.exists(csv_file)) {
  
  stop(
    "Module 1 CSV file not found.\n",
    "Please run Module 1 first."
  )
  
}


# Load dataset

data <- read.csv(
  csv_file
)


cat(
  "\nDataset successfully loaded.\n"
)

cat(
  "Records:",
  nrow(data),
  "\n"
)

cat(
  "Parameters:",
  ncol(data),
  "\n"
)


# ============================================================
# 3. DATA VALIDATION
# ============================================================

cat("\n")
cat("---- DATA VALIDATION ----\n")


if (nrow(data) != 1000) {
  
  warning(
    "Dataset does not contain exactly 1000 records."
  )
  
}


if (ncol(data) != 10) {
  
  warning(
    "Dataset does not contain exactly 10 parameters."
  )
  
}


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
    "Missing columns: ",
    paste(
      missing_columns,
      collapse = ", "
    )
  )
  
}


cat(
  "Validation completed successfully.\n"
)


# ============================================================
# 4. BASIC QUEUEING MODEL
# ============================================================

cat("\n")
cat("---- QUEUEING MODEL ----\n")


# ------------------------------------------------------------
# TOTAL SIMULATION TIME
# ------------------------------------------------------------

total_time <- max(
  data$Arrival_Time_Min
)


# ------------------------------------------------------------
# NUMBER OF JOBS
# ------------------------------------------------------------

number_of_jobs <- nrow(
  data
)


# ------------------------------------------------------------
# ARRIVAL RATE λ
# ------------------------------------------------------------

lambda <- number_of_jobs / total_time


# ------------------------------------------------------------
# AVERAGE SERVICE TIME
# ------------------------------------------------------------

average_service_time <- mean(
  data$Service_Time_Min
)


# ------------------------------------------------------------
# SERVICE RATE μ
# ------------------------------------------------------------

mu <- 1 / average_service_time


# ------------------------------------------------------------
# UTILIZATION ρ
# ------------------------------------------------------------

rho <- lambda / mu


cat(
  "Total Simulation Time:",
  round(total_time, 2),
  "minutes\n"
)

cat(
  "Number of Jobs:",
  number_of_jobs,
  "\n"
)

cat(
  "Arrival Rate (lambda):",
  round(lambda, 4),
  "jobs/minute\n"
)

cat(
  "Average Service Time:",
  round(average_service_time, 4),
  "minutes\n"
)

cat(
  "Service Rate (mu):",
  round(mu, 4),
  "jobs/minute\n"
)

cat(
  "Utilization (rho):",
  round(rho, 4),
  "\n"
)


# ============================================================
# 5. UTILIZATION STATUS
# ============================================================

if (rho < 1) {
  
  utilization_status <- "STABLE"
  
} else {
  
  utilization_status <- "CONGESTED"
  
}


cat(
  "Queue Status:",
  utilization_status,
  "\n"
)


# ============================================================
# 6. SEVERITY SCORE
# ============================================================

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


# ============================================================
# 7. NORMALIZE VULNERABILITY COUNT
# ============================================================

max_vulnerability <- max(
  data$Vulnerability_Count
)


data$Vulnerability_Normalized <-
  
  (
    data$Vulnerability_Count /
      max_vulnerability
  ) * 100


# ============================================================
# 8. NORMALIZE RISK SCORE
# ============================================================

data$Risk_Normalized <-
  
  data$Risk_Score


# ============================================================
# 9. WAITING/ARRIVAL FACTOR
# ============================================================

max_arrival <- max(
  data$Arrival_Time_Min
)


data$Waiting_Factor <-
  
  (
    data$Arrival_Time_Min /
      max_arrival
  ) * 100


# ============================================================
# 10. GATEKEEPERX PRIORITY SCORE
# ============================================================

data$GatekeeperX_Priority_Score <-
  
  (
    0.45 *
      data$Severity_Score
  ) +
  
  (
    0.25 *
      data$Risk_Normalized
  ) +
  
  (
    0.20 *
      data$Vulnerability_Normalized
  ) +
  
  (
    0.10 *
      data$Waiting_Factor
  )


# Round score

data$GatekeeperX_Priority_Score <-
  
  round(
    data$GatekeeperX_Priority_Score,
    2
  )


# ============================================================
# 11. PRIORITY CLASS
# ============================================================

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


# ============================================================
# 12. SECURITY GATE DECISION
# ============================================================

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


# ============================================================
# 13. QUEUE POSITION
# ============================================================

data$Queue_Position <- rank(
  
  -data$GatekeeperX_Priority_Score,
  
  ties.method = "first"
)


# ============================================================
# 14. DISPLAY MODEL RESULTS
# ============================================================

cat("\n")
cat("---- GATEKEEPERX MODEL RESULTS ----\n")

cat(
  "Average Priority Score:",
  round(
    mean(
      data$GatekeeperX_Priority_Score
    ),
    2
  ),
  "\n"
)


cat(
  "Maximum Priority Score:",
  max(
    data$GatekeeperX_Priority_Score
  ),
  "\n"
)


cat(
  "Minimum Priority Score:",
  min(
    data$GatekeeperX_Priority_Score
  ),
  "\n"
)


# ============================================================
# 15. PRIORITY DISTRIBUTION
# ============================================================

cat("\n")
cat("Priority Distribution:\n")

print(
  table(
    data$Priority_Class
  )
)


# ============================================================
# 16. SECURITY DECISION DISTRIBUTION
# ============================================================

cat("\n")
cat("Security Decision Distribution:\n")

print(
  table(
    data$Security_Decision
  )
)


# ============================================================
# 17. QUEUEING FORMULAS
# ============================================================

# M/M/1 theoretical model

if (rho < 1) {
  
  # Average number of jobs in system
  L <- rho / (1 - rho)
  
  # Average number waiting in queue
  Lq <- rho^2 / (1 - rho)
  
  # Average time in system
  W <- 1 / (mu - lambda)
  
  # Average waiting time
  Wq <- lambda /
    (mu * (mu - lambda))
  
} else {
  
  L <- Inf
  Lq <- Inf
  W <- Inf
  Wq <- Inf
  
}


cat("\n")
cat("---- THEORETICAL M/M/1 METRICS ----\n")

cat(
  "L  - Average jobs in system:",
  round(L, 4),
  "\n"
)

cat(
  "Lq - Average jobs in queue:",
  round(Lq, 4),
  "\n"
)

cat(
  "W  - Average time in system:",
  round(W, 4),
  "minutes\n"
)

cat(
  "Wq - Average waiting time:",
  round(Wq, 4),
  "minutes\n"
)


# ============================================================
# 18. SAVE MODELED DATASET
# ============================================================

model_csv <- file.path(
  
  output_dir,
  
  "GatekeeperX_Module2_Modeled_Data.csv"
)


write.csv(
  
  data,
  
  model_csv,
  
  row.names = FALSE
)


# ============================================================
# 19. SAVE MODEL PARAMETERS
# ============================================================

model_parameters <- data.frame(
  
  Parameter = c(
    
    "Number of Jobs",
    
    "Total Simulation Time",
    
    "Arrival Rate Lambda",
    
    "Average Service Time",
    
    "Service Rate Mu",
    
    "Utilization Rho",
    
    "Queue Status",
    
    "Average Jobs in System L",
    
    "Average Queue Length Lq",
    
    "Average Time in System W",
    
    "Average Waiting Time Wq"
    
  ),
  
  Value = c(
    
    number_of_jobs,
    
    total_time,
    
    lambda,
    
    average_service_time,
    
    mu,
    
    rho,
    
    utilization_status,
    
    L,
    
    Lq,
    
    W,
    
    Wq
    
  )
  
)


parameters_csv <- file.path(
  
  output_dir,
  
  "GatekeeperX_Module2_Parameters.csv"
)


write.csv(
  
  model_parameters,
  
  parameters_csv,
  
  row.names = FALSE
)


# ============================================================
# 20. PLOT 1 - PRIORITY SCORE DISTRIBUTION
# ============================================================

png(
  
  file.path(
    output_dir,
    "Module2_Priority_Score_Distribution.png"
  ),
  
  width = 1000,
  
  height = 700
)


hist(
  
  data$GatekeeperX_Priority_Score,
  
  breaks = 20,
  
  col = "steelblue",
  
  border = "white",
  
  main =
    "GatekeeperX Priority Score Distribution",
  
  xlab =
    "GatekeeperX Priority Score",
  
  ylab =
    "Number of Security Jobs"
)


grid()


dev.off()


# ============================================================
# 21. PLOT 2 - PRIORITY CLASS
# ============================================================

png(
  
  file.path(
    output_dir,
    "Module2_Priority_Class.png"
  ),
  
  width = 1000,
  
  height = 700
)


priority_counts <- table(
  data$Priority_Class
)


barplot(
  
  priority_counts,
  
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
  
  seq_along(priority_counts),
  
  priority_counts,
  
  labels = priority_counts,
  
  pos = 3
)


dev.off()


# ============================================================
# 22. PLOT 3 - RISK VS PRIORITY
# ============================================================

png(
  
  file.path(
    output_dir,
    "Module2_Risk_vs_Priority.png"
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


dev.off()


# ============================================================
# 23. PLOT 4 - SECURITY DECISIONS
# ============================================================

png(
  
  file.path(
    output_dir,
    "Module2_Security_Decisions.png"
  ),
  
  width = 1000,
  
  height = 700
)


decision_counts <- table(
  data$Security_Decision
)


barplot(
  
  decision_counts,
  
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
  
  seq_along(decision_counts),
  
  decision_counts,
  
  labels = decision_counts,
  
  pos = 3
)


dev.off()


# ============================================================
# 24. MODEL ARCHITECTURE DIAGRAM
# ============================================================

png(
  
  file.path(
    output_dir,
    "Module2_Model_Architecture.png"
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
  "GatekeeperX - Module 2 Modeling Architecture",
  cex.main = 1.5
)


dev.off()


# ============================================================
# 25. SAVE TEXT REPORT
# ============================================================

report_file <- file.path(
  
  output_dir,
  
  "GatekeeperX_Module2_Report.txt"
)


sink(report_file)


cat(
  "====================================================\n"
)

cat(
  "GATEKEEPERX - MODULE 2 MODELING REPORT\n"
)

cat(
  "====================================================\n\n"
)


cat(
  "Number of Jobs:",
  number_of_jobs,
  "\n"
)

cat(
  "Total Simulation Time:",
  round(total_time, 2),
  "minutes\n"
)

cat(
  "Arrival Rate (lambda):",
  round(lambda, 4),
  "\n"
)

cat(
  "Average Service Time:",
  round(average_service_time, 4),
  "\n"
)

cat(
  "Service Rate (mu):",
  round(mu, 4),
  "\n"
)

cat(
  "Utilization (rho):",
  round(rho, 4),
  "\n"
)

cat(
  "Queue Status:",
  utilization_status,
  "\n\n"
)


cat(
  "GatekeeperX Priority Formula:\n"
)

cat(
  "GPS = 0.45(Severity) + 0.25(Risk) + ",
  "0.20(Vulnerability) + 0.10(Waiting)\n\n"
)


cat(
  "Priority Distribution:\n"
)

print(
  table(
    data$Priority_Class
  )
)


cat(
  "\nSecurity Decision Distribution:\n"
)

print(
  table(
    data$Security_Decision
  )
)


cat(
  "\nTheoretical M/M/1 Metrics:\n"
)

cat(
  "L  =",
  round(L, 4),
  "\n"
)

cat(
  "Lq =",
  round(Lq, 4),
  "\n"
)

cat(
  "W  =",
  round(W, 4),
  "minutes\n"
)

cat(
  "Wq =",
  round(Wq, 4),
  "minutes\n"
)


sink()


# ============================================================
# FINAL OUTPUT
# ============================================================

cat("\n")
cat("====================================================\n")
cat(" MODULE 2 COMPLETED SUCCESSFULLY\n")
cat("====================================================\n")

cat(
  "Modeled Dataset :",
  model_csv,
  "\n"
)

cat(
  "Parameters File :",
  parameters_csv,
  "\n"
)

cat(
  "Plots Generated : 4\n"
)

cat(
  "Architecture Diagram : 1\n"
)

cat(
  "Report Generated : 1\n"
)

cat("====================================================\n")

cat(
  "Next: Module 3 - Simulation\n"
)

cat("====================================================\n")