numeric_vars <- sapply(main_data, is.numeric)
numeric_data <- main_data[, numeric_vars]

summary_stats <- sapply(numeric_data, function(x) {
  c(
    Mean = mean(x),
    SD = sd(x),
    Min = min(x),
    Q1 = quantile(x, 0.25),
    Median = median(x),
    Q3 = quantile(x, 0.75),
    Max = max(x)
  )
})

t(as.data.frame(summary_stats))

table(main_data$Dedicated)
table(main_data$Shader)

main_data <- subset(main_data, !(Shader %in% c(1, 1.4)))

library(ggplot2)
ggplot(main_data, aes(x = Memory_Bandwidth)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 15) +
  labs(title = "Histogram of Memory_Bandwidth", x = "Memory Bandwidth (GB/s)", y = "Count") + theme_minimal()

# main_data$Memory_Bandwidth<-log(main_data$Memory_Bandwidth)
# main_data$Memory_Speed<-log(main_data$Memory_Speed)
# main_data$L2_Cache<-log(main_data$L2_Cache)
# main_data$Memory_Bus<-log(main_data$Memory_Bus)

# ggplot(main_data, aes(x = Memory_Bandwidth)) +
#   geom_histogram(fill = "lightblue", color = "black", bins = 15) +
#   labs(title = "Histogram of log (Memory Bandwidth)", x = "log(Memory Bandwidth)", y = "Count") +
#   theme_minimal()

ggplot(main_data, aes(x = Dedicated, y = Memory_Bandwidth)) +
  geom_boxplot(fill = "pink", color = "black") +
  labs(title = "Boxplot of Memory_Bandwidth for Dedicated GPU",
       x = "Dedicated GPU", y = "Memory_Bandwidth (GB/sec)") +
  theme_minimal()

ggplot(main_data, aes(x = Shader, y = Memory_Bandwidth)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of Memory_Bandwidth for Shader",
       x = "Shader", y = "Memory_Bandwidth") +
  theme_minimal()

# Pie chart
ma = table(main_data$Manufacturer)
pie(ma, main = "Pie chart of Manufacturer ", radius = 1.3)

ggplot(main_data, aes(x = Memory_Speed, y = Memory_Bandwidth)) +
  geom_point(color = "red") +
  labs(title = "Scatter Plot of Memory_Bandwidth and Memory_Speed",
       x = "Memory_Speed (MHz)", y = "Memory_Bandwidth (GB/sec)") +
  theme_minimal()

ggplot(main_data, aes(x = L2_Cache, y = Memory_Bandwidth)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of Memory_Bandwidth and L2_Cache",
       x = "L2_Cache (KB)", y = "Memory_Bandwidth (GB/sec)") +
  theme_minimal()

ggplot(main_data, aes(x = Memory_Bus, y = Memory_Bandwidth)) +
  geom_point(color = "purple") +
  labs(title = "Scatter Plot of Memory_Bandwidth and Memory_Bus",
       x = "Memory_Bus (Bit)", y = "Memory_Bandwidth (GB/sec)") +
  theme_minimal()

library(corrplot)
corrplot(cor(numeric_data), method = "number")