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
    gtth=qnorm(p=0.05,lower.tail = FALSE)
    print(paste("z_qs = ", z_qs))
    print(paste("gtth = ", gtth))
    if (z_qs>gtth) print("Có cơ sở để bác bỏ H0, chấp nhận H1")
    else print("Có cơ sở để chấp nhận H0")
  }
  #----------------------------Đọc và xử lý số liệu--------------------------------------
  RAW <- read.csv("C:/Users/Tuan/Desktop/xstk/All_GPUs.csv", stringsAsFactors = FALSE)
  RAW[RAW == "" | RAW=="\n- "] <- NA
  MB=RAW[,c("Manufacturer","Memory_Bandwidth")]
  MB = MB[!is.na(MB$Memory_Bandwidth), ]
  MB <- MB %>%
    mutate(
      memory_value = as.numeric(str_extract(Memory_Bandwidth, "[0-9.]+")),
      memory_unit = str_extract(Memory_Bandwidth, "(GB|MB)/sec"),
      Memory_Bandwidth = ifelse(memory_unit == "MB/sec", memory_value / 1024, memory_value)
    ) %>%
    select(Manufacturer,Memory_Bandwidth)
  Nvidia_MB=MB[MB$Manufacturer=='Nvidia',]
  AMD_MB=MB[MB$Manufacturer=='AMD',]
#------------------------------------Kiểm tra phân phối chuẩn--------------------------------------
  ND_Check(Nvidia_MB$Memory_Bandwidth)
  ND_Check(AMD_MB$Memory_Bandwidth)
#---------------------------------------Kiểm định giả thuyết---------------------------------------
#Kiểm định giả thuyết: 
  #1 mẫu
    z_test <- z.test(Nvidia_MB$Memory_Bandwidth, mu = 200, sigma.x = sd(Nvidia_MB$Memory_Bandwidth), conf.level = 1- 0.05, alternative = "less")
    print(z_test)
  #2 mẫu  
    Z_TEST2(Nvidia_MB$Memory_Bandwidth,AMD_MB$Memory_Bandwidth)
   
