# 🔥 Install packages (run only once)
install.packages(c("MASS", "ggplot2", "dplyr", "corrplot"))

# 🔥 Load libraries
library(MASS)
library(ggplot2)
library(dplyr)
library(corrplot)

set.seed(123)

# 🔹 Number of records
n <- 1000

# 🔹 Generate synthetic dataset (GMM-like clusters)
data1 <- mvrnorm(n = n/2,
                 mu = c(50, 8, 3.5, 2.8, 10.8, 0.7, 0.6, 75, 0.4, 1),
                 Sigma = diag(10))

data2 <- mvrnorm(n = n/2,
                 mu = c(70, 12, 5.0, 3.5, 15.2, 0.9, 0.8, 85, 0.6, 2),
                 Sigma = diag(10))

# 🔹 Combine clusters
dataset <- as.data.frame(rbind(data1, data2))

# 🔹 Column names (10 parameters)
colnames(dataset) <- c(
  "Build_Time",
  "Queue_Length",
  "Waiting_Time",
  "Service_Time",
  "Throughput",
  "CPU_Utilization",
  "Memory_Usage",
  "Security_Risk_Score",
  "Vulnerability_Density",
  "Priority_Level"
)

# 🔹 Convert values to positive
dataset <- abs(dataset)

# 🔹 Save dataset
write.csv(dataset, "devsecops_dataset.csv", row.names = FALSE)

# 🔹 Preview
head(dataset)

# =========================================================
# 📊 DATA SUMMARY
# =========================================================
cat("\n===== DATA SUMMARY =====\n")
print(summary(dataset))

# =========================================================
# 🚨 OUTLIER DETECTION (IQR METHOD)
# =========================================================
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  which(x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR))
}

outlier_counts <- sapply(dataset, function(col) length(detect_outliers(col)))

cat("\n===== OUTLIER COUNT PER VARIABLE =====\n")
print(outlier_counts)

# =========================================================
# 📈 VISUALIZATION + SAVE GRAPHS
# =========================================================

# 🔹 Histogram
p1 <- ggplot(dataset, aes(Build_Time)) +
  geom_histogram(fill = "skyblue", bins = 25) +
  ggtitle("Histogram of Build Time")

ggsave("histogram_build_time.png", plot = p1, width = 6, height = 4)

# 🔹 Scatter Plot
p2 <- ggplot(dataset, aes(Build_Time, Queue_Length)) +
  geom_point(color = "blue") +
  ggtitle("Build Time vs Queue Length")

ggsave("scatter_build_vs_queue.png", plot = p2, width = 6, height = 4)

# 🔹 Boxplot
p3 <- ggplot(dataset, aes(y = Build_Time)) +
  geom_boxplot(fill = "orange") +
  ggtitle("Boxplot of Build Time")

ggsave("boxplot_build_time.png", plot = p3, width = 6, height = 4)

# =========================================================
# 📊 CORRELATION MATRIX
# =========================================================
cor_matrix <- cor(dataset)

png("correlation_matrix.png", width = 700, height = 700)
corrplot(cor_matrix, method = "color", tl.col = "black", tl.srt = 45)
dev.off()

cat("\n✅ All graphs saved successfully!\n")
# 🔹 Data Summary
summary_result <- summary(dataset)
print(summary_result)