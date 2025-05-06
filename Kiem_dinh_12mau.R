# import các thư viện cần thiết
  library(stringr)
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  library(reshape2)
  library(plotly)
  library("BSDA")

  # ----------------------------------Kiểm tra phân phối chuẩn--------------------------
  ND_Check <- function(data0){
    qqnorm(data0)
    qqline(data0, col="red", lwd=2) # Thêm chuẩn màu đỏ
    p=shapiro.test(data0)
    print(p)
  }
  Z_TEST1 <- function(data0,num){
    z_qs=((mean(data0)-num)/sd(data0)*sqrt(length(data0)))
    gtth=qnorm(p=0.05,lower.tail = FALSE)
    print(paste("z_qs = ", z_qs))
    print(paste("gtth = ", gtth))
    if (z_qs>gtth) print("Có cơ sở để bác bỏ H0, chấp nhận H1")
    else  print("Có cơ sở để chấp nhận H0")
  }
  Z_TEST2 <- function(data1,data2){
    z_qs=(mean(data1)-mean(data2))/sqrt(sd(data1)^2/length(data1) + sd(data2)^2/length(data2))
    gtth=qnorm(1-0.05)
    print(paste("z_qs = ", z_qs))
    print(paste("gtth = ", gtth))
    if (z_qs>gtth) print("Có cơ sở để bác bỏ H0, chấp nhận H1")
    else print("Có cơ sở để chấp nhận H0")
  }
  #----------------------------Đọc và xử lý số liệu--------------------------------------
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
  Nvidia_MB=MB[main_data$Manufacturer=='Nvidia',]
  AMD_MB=MB[main_data$Manufacturer=='AMD',]
#------------------------------------Kiểm tra phân phối chuẩn--------------------------------------
  ND_Check(Nvidia_MB$Memory_Bandwidth)
  ND_Check(AMD_MB$Memory_Bandwidth)
#---------------------------------------Kiểm định giả thuyết---------------------------------------
#Kiểm định giả thuyết: 
  #1 mẫu
    z_test <- z.test(Nvidia_MB$Memory_Bandwidth, mu = 200, sigma.x = sd(Nvidia_MB$Memory_Bandwidth), conf.level = 0.05, alternative = "less")
    print(z_test)
    Z_TEST1(Nvidia_MB$Memory_Bandwidth,200)
  #2 mẫu  
    
    z2 <- z.test(Nvidia_MB$Memory_Bandwidth, AMD_MB$Memory_Bandwidth,
                 alternative = "greater", conf.level = 0.95,
                 sigma.x = sd(Nvidia_MB$Memory_Bandwidth), sigma.y = sd(AMD_MB$Memory_Bandwidth))
    print(z2)
    Z_TEST2(Nvidia_MB$Memory_Bandwidth,AMD_MB$Memory_Bandwidth)
   
