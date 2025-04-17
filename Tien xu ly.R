# Doc du lieu
All_GPUs <- read.csv("~/HCMUT/242 XSTK/btl/archive/All_GPUs.csv")
head(All_GPUs)

All_GPUs[All_GPUs == ""] <- NA
All_GPUs[] <- lapply(All_GPUs, function(x) gsub ("^\\n- $", NA, x))
All_GPUs[] <- lapply(All_GPUs, function(x) gsub ("^\\ n$", NA, x))

library(questionr)
freq.na(All_GPUs)

NA_summary <- data.frame(freq.na(All_GPUs))
colnames(NA_summary) <- c("Missing", "Percent")
selected_columns <- rownames(NA_summary[NA_summary$Percent < 10, ])
new_GPU_data <- All_GPUs[, selected_columns]
new_GPU_data <- na.omit(new_GPU_data)

str(new_GPU_data)

columns_to_clean <- c("L2_Cache","Memory_Bandwidth","Memory_Bus","Memory_Speed")

remove_units <- function(column) {
  cleaned_column <- gsub("[^0-9.]", "", column)
  cleaned_column <- as.numeric(cleaned_column)
  return(cleaned_column)
}

new_GPU_data[columns_to_clean] <- lapply(new_GPU_data[columns_to_clean], remove_units)

main_data <-new_GPU_data[c("Memory_Bandwidth","Memory_Speed","L2_Cache","Memory_Bus", "Shader","Dedicated")]
head(main_data)