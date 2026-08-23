# ============================================================
# GATEKEEPERX
# MODULE 6 - ARCHITECTURE
#
# Queueing Simulation for Intelligent DevSecOps Security Gates
# ============================================================


# ============================================================
# 1. SETUP
# ============================================================

rm(list = ls())

cat("\n")
cat("====================================================\n")
cat(" GATEKEEPERX - MODULE 6\n")
cat(" SYSTEM ARCHITECTURE\n")
cat("====================================================\n")


# ============================================================
# 2. OUTPUT DIRECTORY
# ============================================================

output_dir <- "GatekeeperX_Module6_Output"

if (!dir.exists(output_dir)) {
  
  dir.create(output_dir)
  
}


# ============================================================
# 3. LOAD REQUIRED LIBRARY
# ============================================================

if (!requireNamespace(
  "grid",
  quietly = TRUE
)) {
  
  stop(
    "The grid package is required."
  )
  
}

library(grid)


# ============================================================
# 4. CREATE ARCHITECTURE IMAGE
# ============================================================

png(
  
  file.path(
    
    output_dir,
    
    "GatekeeperX_Complete_Architecture.png"
    
  ),
  
  width = 1600,
  
  height = 1200,
  
  res = 150
  
)


# ============================================================
# 5. CANVAS
# ============================================================

plot.new()

plot.window(
  
  xlim = c(
    0,
    16
  ),
  
  ylim = c(
    0,
    14
  )
  
)


# ============================================================
# 6. BOX FUNCTION
# ============================================================

draw_box <- function(
    
  x,
  
  y,
  
  width,
  
  height,
  
  label,
  
  fill = "lightblue",
  
  border = "navy",
  
  cex = 0.9
  
) {
  
  
  rect(
    
    x - width / 2,
    
    y - height / 2,
    
    x + width / 2,
    
    y + height / 2,
    
    col = fill,
    
    border = border,
    
    lwd = 2
    
  )
  
  
  text(
    
    x,
    
    y,
    
    label,
    
    cex = cex,
    
    font = 2
    
  )
  
}


# ============================================================
# 7. ARROW FUNCTION
# ============================================================

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


# ============================================================
# 8. TITLE
# ============================================================

text(
  
  8,
  
  13.6,
  
  "GatekeeperX",
  
  cex = 2,
  
  font = 2
  
)


text(
  
  8,
  
  13.1,
  
  "Intelligent DevSecOps Security Gate Architecture",
  
  cex = 1.2,
  
  font = 2
  
)


# ============================================================
# 9. DEVSECOPS PIPELINE
# ============================================================


draw_box(
  
  2,
  
  11.5,
  
  2.5,
  
  0.8,
  
  "Developer\nCode Commit",
  
  fill = "lightgray"
  
)


draw_box(
  
  5,
  
  11.5,
  
  2.5,
  
  0.8,
  
  "CI / Build",
  
  fill = "lightgray"
  
)


draw_box(
  
  8,
  
  11.5,
  
  2.5,
  
  0.8,
  
  "Security\nScanning",
  
  fill = "lightyellow"
  
)


draw_box(
  
  11,
  
  11.5,
  
  2.5,
  
  0.8,
  
  "GatekeeperX\nSecurity Gate",
  
  fill = "lightblue"
  
)


draw_box(
  
  14,
  
  11.5,
  
  2.5,
  
  0.8,
  
  "CD /\nDeployment",
  
  fill = "lightgreen"
  
)


# ============================================================
# 10. PIPELINE ARROWS
# ============================================================

draw_arrow(
  3.25,
  11.5,
  3.75,
  11.5
)


draw_arrow(
  6.25,
  11.5,
  6.75,
  11.5
)


draw_arrow(
  9.25,
  11.5,
  9.75,
  11.5
)


draw_arrow(
  12.25,
  11.5,
  12.75,
  11.5
)


# ============================================================
# 11. SECURITY SCANNING COMPONENTS
# ============================================================

draw_box(
  
  6,
  
  9.8,
  
  2.2,
  
  0.7,
  
  "SAST",
  
  fill = "white"
  
)


draw_box(
  
  8,
  
  9.8,
  
  2.2,
  
  0.7,
  
  "DAST",
  
  fill = "white"
  
)


draw_box(
  
  10,
  
  9.8,
  
  2.2,
  
  0.7,
  
  "SCA / Secrets",
  
  fill = "white"
  
)


draw_arrow(
  
  8,
  
  11.1,
  
  8,
  
  10.2
  
)


# ============================================================
# 12. GATEKEEPERX ENGINE
# ============================================================

draw_box(
  
  8,
  
  7.5,
  
  8,
  
  2.8,
  
  "",
  
  fill = "aliceblue"
  
)


text(
  
  8,
  
  8.65,
  
  "GATEKEEPERX INTELLIGENT SECURITY ENGINE",
  
  cex = 1.2,
  
  font = 2
  
)


# ============================================================
# 13. ENGINE COMPONENTS
# ============================================================

draw_box(
  
  4.5,
  
  7.3,
  
  2.5,
  
  0.8,
  
  "Risk\nAssessment",
  
  fill = "white"
  
)


draw_box(
  
  7.5,
  
  7.3,
  
  2.5,
  
  0.8,
  
  "Priority\nCalculation",
  
  fill = "white"
  
)


draw_box(
  
  10.5,
  
  7.3,
  
  2.5,
  
  0.8,
  
  "Intelligent\nQueue",
  
  fill = "white"
  
)


draw_box(
  
  13.5,
  
  7.3,
  
  2.5,
  
  0.8,
  
  "Dynamic\nAging",
  
  fill = "white"
  
)


# ============================================================
# 14. ENGINE FLOW
# ============================================================

draw_arrow(
  
  5.75,
  
  7.3,
  
  6.25,
  
  7.3
  
)


draw_arrow(
  
  8.75,
  
  7.3,
  
  9.25,
  
  7.3
  
)


draw_arrow(
  
  11.75,
  
  7.3,
  
  12.25,
  
  7.3
  
)


# ============================================================
# 15. SECURITY DECISION
# ============================================================

draw_box(
  
  8,
  
  5.2,
  
  3.5,
  
  0.9,
  
  "Security Decision\nEngine",
  
  fill = "lightyellow"
  
)


draw_arrow(
  
  8,
  
  6.0,
  
  8,
  
  5.7
  
)


# ============================================================
# 16. DECISION OUTPUTS
# ============================================================

draw_box(
  
  4,
  
  3.3,
  
  2.5,
  
  0.9,
  
  "PASS",
  
  fill = "lightgreen"
  
)


draw_box(
  
  8,
  
  3.3,
  
  2.5,
  
  0.9,
  
  "WAIT /\nPRIORITIZE",
  
  fill = "orange"
  
)


draw_box(
  
  12,
  
  3.3,
  
  2.5,
  
  0.9,
  
  "BLOCK",
  
  fill = "lightcoral"
  
)


draw_arrow(
  
  6.5,
  
  5.2,
  
  5,
  
  3.8
  
)


draw_arrow(
  
  8,
  
  4.7,
  
  8,
  
  3.8
  
)


draw_arrow(
  
  9.5,
  
  5.2,
  
  11,
  
  3.8
  
)


# ============================================================
# 17. DEPLOYMENT DECISION
# ============================================================

draw_box(
  
  8,
  
  1.6,
  
  5,
  
  0.8,
  
  "Deployment Gate",
  
  fill = "lightblue"
  
)


draw_arrow(
  
  4,
  
  2.85,
  
  6,
  
  1.9
  
)


draw_arrow(
  
  8,
  
  2.85,
  
  8,
  
  2.0
  
)


draw_arrow(
  
  12,
  
  2.85,
  
  10,
  
  1.9
  
)


# ============================================================
# 18. MODULE PIPELINE
# ============================================================

draw_box(
  
  1.5,
  
  6.5,
  
  2.3,
  
  0.7,
  
  "Module 1\nDataset",
  
  fill = "lightgray"
  
)


draw_box(
  
  1.5,
  
  5.2,
  
  2.3,
  
  0.7,
  
  "Module 2\nModeling",
  
  fill = "lightgray"
  
)


draw_box(
  
  1.5,
  
  3.9,
  
  2.3,
  
  0.7,
  
  "Module 3\nSimulation",
  
  fill = "lightgray"
  
)


draw_box(
  
  1.5,
  
  2.6,
  
  2.3,
  
  0.7,
  
  "Module 4\nEvaluation",
  
  fill = "lightgray"
  
)


draw_box(
  
  1.5,
  
  1.3,
  
  2.3,
  
  0.7,
  
  "Module 5\nComparison",
  
  fill = "lightgray"
  
)


draw_arrow(
  
  1.5,
  
  6.1,
  
  1.5,
  
  5.6
  
)


draw_arrow(
  
  1.5,
  
  4.8,
  
  1.5,
  
  4.3
  
)


draw_arrow(
  
  1.5,
  
  3.5,
  
  1.5,
  
  3.0
  
)


draw_arrow(
  
  1.5,
  
  2.2,
  
  1.5,
  
  1.7
  
)


# ============================================================
# 19. DATA CONNECTION TO GATEKEEPERX
# ============================================================

draw_arrow(
  
  2.7,
  
  6.5,
  
  4.2,
  
  7.3
  
)


draw_arrow(
  
  2.7,
  
  5.2,
  
  4.2,
  
  7.0
  
)


draw_arrow(
  
  2.7,
  
  3.9,
  
  4.2,
  
  6.7
  
)


# ============================================================
# 20. EVALUATION CONNECTION
# ============================================================

draw_arrow(
  
  2.7,
  
  2.6,
  
  4.2,
  
  2.6
  
)


draw_arrow(
  
  2.7,
  
  1.3,
  
  4.2,
  
  2.0
  
)


# ============================================================
# 21. CLOSE PLOT
# ============================================================

dev.off()


# ============================================================
# 22. CREATE SIMPLE MODULE FLOW DIAGRAM
# ============================================================

png(
  
  file.path(
    
    output_dir,
    
    "GatekeeperX_Module_Flow.png"
    
  ),
  
  width = 1400,
  
  height = 900,
  
  res = 150
  
)


plot.new()

plot.window(
  
  xlim = c(
    0,
    10
  ),
  
  ylim = c(
    0,
    10
  )
  
)


# ============================================================
# MODULE BOXES
# ============================================================

modules <- list(
  
  list(
    x = 1,
    y = 8.5,
    text = "MODULE 1\nDATASET GENERATION"
  ),
  
  list(
    x = 3,
    y = 8.5,
    text = "MODULE 2\nMODELING"
  ),
  
  list(
    x = 5,
    y = 8.5,
    text = "MODULE 3\nSIMULATION"
  ),
  
  list(
    x = 7,
    y = 8.5,
    text = "MODULE 4\nEVALUATION"
  ),
  
  list(
    x = 9,
    y = 8.5,
    text = "MODULE 5\nCOMPARISON"
  )
  
)


for (
  
  m in modules
  
) {
  
  draw_box(
    
    m$x,
    
    m$y,
    
    1.6,
    
    1,
    
    m$text,
    
    fill = "lightblue",
    
    cex = 0.65
    
  )
  
}


# ============================================================
# ARROWS
# ============================================================

draw_arrow(
  
  1.8,
  8.5,
  
  2.2,
  8.5
  
)


draw_arrow(
  
  3.8,
  8.5,
  
  4.2,
  8.5
  
)


draw_arrow(
  
  5.8,
  8.5,
  
  6.2,
  8.5
  
)


draw_arrow(
  
  7.8,
  8.5,
  
  8.2,
  8.5
  
)


# ============================================================
# GATEKEEPERX
# ============================================================

draw_box(
  
  5,
  
  5.5,
  
  4,
  
  1.3,
  
  "GATEKEEPERX\nINTELLIGENT SECURITY GATE",
  
  fill = "lightyellow",
  
  cex = 1
  
)


# ============================================================
# CONNECTION
# ============================================================

draw_arrow(
  
  5,
  
  8,
  
  5,
  
  6.2
  
)


# ============================================================
# OUTPUTS
# ============================================================

draw_box(
  
  2.5,
  
  2.5,
  
  2.2,
  
  1,
  
  "PASS",
  
  fill = "lightgreen"
  
)


draw_box(
  
  5,
  
  2.5,
  
  2.2,
  
  1,
  
  "PRIORITIZE",
  
  fill = "orange"
  
)


draw_box(
  
  7.5,
  
  2.5,
  
  2.2,
  
  1,
  
  "BLOCK",
  
  fill = "lightcoral"
  
)


draw_arrow(
  
  4,
  
  4.9,
  
  3,
  
  3.1
  
)


draw_arrow(
  
  5,
  
  4.8,
  
  5,
  
  3.1
  
)


draw_arrow(
  
  6,
  
  4.9,
  
  7,
  
  3.1
  
)


# ============================================================
# TITLE
# ============================================================

title(
  
  "GatekeeperX - Research Module Flow",
  
  cex.main = 1.5
  
)


dev.off()


# ============================================================
# 23. CREATE ARCHITECTURE DESCRIPTION
# ============================================================

architecture_text <- c(
  
  "====================================================",
  
  "GATEKEEPERX ARCHITECTURE",
  
  "====================================================",
  
  "",
  
  "1. DEVOPS PIPELINE",
  
  "Developer Code Commit",
  
  "        ↓",
  
  "CI / Build",
  
  "        ↓",
  
  "Security Scanning",
  
  "        ↓",
  
  "GatekeeperX Security Gate",
  
  "        ↓",
  
  "Deployment Decision",
  
  "",
  
  "2. SECURITY SCANNING",
  
  "SAST - Static Application Security Testing",
  
  "DAST - Dynamic Application Security Testing",
  
  "SCA - Software Composition Analysis",
  
  "Secret Detection",
  
  "",
  
  "3. GATEKEEPERX ENGINE",
  
  "Risk Assessment",
  
  "        ↓",
  
  "Priority Calculation",
  
  "        ↓",
  
  "Intelligent Queue",
  
  "        ↓",
  
  "Dynamic Aging",
  
  "        ↓",
  
  "Security Decision",
  
  "",
  
  "4. SECURITY DECISIONS",
  
  "PASS",
  
  "WAIT / PRIORITIZE",
  
  "BLOCK",
  
  "",
  
  "5. RESEARCH MODULES",
  
  "Module 1 - Dataset Generation",
  
  "Module 2 - Modeling",
  
  "Module 3 - Simulation",
  
  "Module 4 - Results & Evaluation",
  
  "Module 5 - Comparison",
  
  "Module 6 - Architecture",
  
  "",
  
  "====================================================",
  
  "END OF ARCHITECTURE",
  
  "===================================================="
  
)


writeLines(
  
  architecture_text,
  
  file.path(
    
    output_dir,
    
    "GatekeeperX_Architecture_Description.txt"
    
  )
  
)


# ============================================================
# 24. FINAL OUTPUT
# ============================================================

cat("\n")

cat(
  "====================================================\n"
)

cat(
  " MODULE 6 COMPLETED SUCCESSFULLY\n"
)

cat(
  "====================================================\n\n"
)


cat(
  "Generated files:\n\n"
)


cat(
  "1. GatekeeperX_Complete_Architecture.png\n"
)


cat(
  "2. GatekeeperX_Module_Flow.png\n"
)


cat(
  "3. GatekeeperX_Architecture_Description.txt\n"
)


cat("\n")

cat(
  "Architecture represents the complete\n"
)

cat(
  "GatekeeperX DevSecOps security-gate system.\n"
)


cat(
  "\n====================================================\n"
)