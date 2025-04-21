
GPUs_list <- read.csv("D:/CS_Major/Đại Cương/XSTK_BTL/All_GPUs.csv", na.strings = c("", "N/A"))

GPUs_list[GPUs_list == ""] <- NA
GPUs_list[] <- lapply(GPUs_list, function(x) gsub("^\\n- $", NA, x))
GPUs_list[] <- lapply(GPUs_list, function(x) gsub("^\\n$", NA, x))

missing_percent <- sapply(GPUs_list, function(x) sum(is.na(x)) / length(x) * 100)
missing_percent <- sort(missing_percent)

selected_cols <- names(missing_percent[missing_percent < 10])

important_cols <- c("Memory_Speed", "L2_Cache", "Dedicated", "Memory_Bus", "Shader", "Memory_Bandwidth")
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

df_gpu <- df_gpu %>%
  rename(
    `Memory_Speed (MHz)` = Memory_Speed,
    `Memory_Bus (bit)` = Memory_Bus,
    `Memory_Bandwidth (GB/s)` = Memory_Bandwidth,
    `L2_Cache (KB)` = L2_Cache
  )


df_gpu <- df_gpu %>%
  mutate(
    `Memory_Speed (MHz)` = clean_numeric(`Memory_Speed (MHz)`),
    `L2_Cache (KB)` = clean_numeric(`L2_Cache (KB)`),
    `Memory_Bus (bit)` = clean_numeric(`Memory_Bus (bit)`),
    `Shader` = clean_numeric(`Shader`),
    `Memory_Bandwidth (GB/s)` = sapply(`Memory_Bandwidth (GB/s)`, convert_bandwidth)
  )

GPUs_final <- df_gpu %>% 
  filter(!is.na(`Memory_Bandwidth (GB/s)`))

