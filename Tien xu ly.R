# Doc du lieu
All_GPUs <- read.csv("All_GPUs.csv")
head(All_GPUs)
str(All_GPUs)

All_GPUs[All_GPUs == ""] <- NA
All_GPUs[] <- lapply(All_GPUs, function(x) gsub ("^\\n- $", NA, x))
All_GPUs[] <- lapply(All_GPUs, function(x) gsub ("^\\ n$", NA, x))

# library(questionr)
# freq.na(All_GPUs)

# NA_summary <- data.frame(freq.na(All_GPUs))
# colnames(NA_summary) <- c("Missing", "Percent")
# selected_columns <- rownames(NA_summary[NA_summary$Percent < 10, ])
# new_GPU_data <- All_GPUs[, selected_columns]
# new_GPU_data <- na.omit(new_GPU_data)

NA_summary <- data.frame(
  Column = names(All_GPUs),
  NA_Count = colSums(is.na(All_GPUs)),
  NA_Percentage = colMeans(is.na(All_GPUs)) * 100
)

print(NA_summary)

# str(new_GPU_data)

library(ggplot2)

ggplot(NA_summary, aes(x = Column, y = NA_Percentage)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = paste0(round(NA_Percentage, 1), "%")),
            vjust = -0.5, size = 2) +
  labs(title = "NA Percentage of Factors",
       x = "Factor",
       y = "NA Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1))

selected_columns <- NA_summary$Column[NA_summary$NA_Percentage < 10]
new_GPU_data <- All_GPUs[, selected_columns]
new_GPU_data <- na.omit(new_GPU_data)

columns_to_clean <- c("L2_Cache","Memory_Bandwidth","Memory_Bus","Memory_Speed")

remove_units <- function(column) {
  cleaned_column <- gsub("[^0-9.]", "", column)
  cleaned_column <- as.numeric(cleaned_column)
  return(cleaned_column)
}

new_GPU_data[columns_to_clean] <- lapply(new_GPU_data[columns_to_clean], remove_units)

main_data <-new_GPU_data[c("Memory_Bandwidth","Memory_Speed","L2_Cache","Memory_Bus", "Shader","Dedicated", "Manufacturer")]
head(main_data, 10)
