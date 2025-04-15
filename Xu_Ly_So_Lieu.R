library(dplyr)
GPUs_list <- read.csv("D:/CS_Major/Đại Cương/XSTK_BTL/All_GPUs.csv", na.strings = c("", "N/A"))

missing_percent <- sapply(GPUs_list, function(x) sum(is.na(x)) / length(x) * 100)
missing_percent <- sort(missing_percent)

selected_cols <- names(missing_percent[missing_percent < 10])[1:10]

df_gpu <- GPUs_list[, selected_cols]

clean_numeric <- function(x){
  as.numeric(gsub("[^0-9\\.]", "", x))
}

numeric_like_patterns <- c("Shader", "Speed", "Bus", "Bandwidth", "Open_GL", "Direct_X")
numeric_cols <- names(df_gpu)[sapply(numeric_like_patterns, function(p) grepl(paste(p, collapse="|"), names(df_gpu))) %>% apply(2, any)]

df_gpu[numeric_cols] <- lapply(df_gpu[numeric_cols], clean_numeric)

GPUs_final <- df_gpu