GPUs_list <- read.csv("D:/CS_Major/Đại Cương/XSTK_BTL/All_GPUs.csv", na.strings = c("", "N/A"))

GPUs_list[GPUs_list == ""] <- NA
GPUs_list[] <- lapply(GPUs_list, function(x) gsub("^\\n- $", NA, x))
GPUs_list[] <- lapply(GPUs_list, function(x) gsub("^\\n$", NA, x))

NA_summary <- data.frame(
  Column = names(GPUs_list),
  NA_Count = colSums(is.na(GPUs_list)),
  NA_Percentage = colMeans(is.na(GPUs_list)) * 100
)

print(NA_summary)

missing_percent <- sapply(GPUs_list, function(x) sum(is.na(x)) / length(x) * 100)
missing_percent <- sort(missing_percent)

selected_cols <- names(missing_percent[missing_percent < 10])

important_cols <- c("Memory_Speed", "L2_Cache", "Dedicated", "Memory_Bus", "Shader", "Memory_Bandwidth", "Manufacturer")
df_gpu <- GPUs_list[, important_cols]

clean_numeric <- function(x) {
  as.numeric(gsub("[^0-9\\.]", "", x))
}

convert_bandwidth <- function(x) {
  x <- trimws(x)
  if (grepl("GB", x)) {
    as.numeric(gsub("[^0-9\\.]", "", x))
  } else if (grepl("MB", x)) {
    as.numeric(gsub("[^0-9\\.]", "", x)) / 1024
  } else {
    NA
  }
}

names(df_gpu)[names(df_gpu) == "Memory_Speed"] <- "Memory_Speed (MHz)"
names(df_gpu)[names(df_gpu) == "Memory_Bus"] <- "Memory_Bus (bit)"
names(df_gpu)[names(df_gpu) == "Memory_Bandwidth"] <- "Memory_Bandwidth (GB/s)"
names(df_gpu)[names(df_gpu) == "L2_Cache"] <- "L2_Cache (KB)"


df_gpu[["Memory_Speed (MHz)"]] <- clean_numeric(df_gpu[["Memory_Speed (MHz)"]])
df_gpu[["L2_Cache (KB)"]] <- clean_numeric(df_gpu[["L2_Cache (KB)"]])
df_gpu[["Memory_Bus (bit)"]] <- clean_numeric(df_gpu[["Memory_Bus (bit)"]])
df_gpu[["Shader"]] <- clean_numeric(df_gpu[["Shader"]])
df_gpu[["Memory_Bandwidth (GB/s)"]] <- sapply(df_gpu[["Memory_Bandwidth (GB/s)"]], convert_bandwidth)

GPUs_final <- df_gpu[!is.na(df_gpu[["Memory_Bandwidth (GB/s)"]]), ]
GPUs_final <- na.omit(GPUs_final)