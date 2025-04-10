# import các thư viện cần thiết
  library(stringr)
  library(tidyr)
  library(dplyr)
  library(zoo)
  library(Metrics)
  library(caret)
  library(MASS)
  library(ggplot2)
  library(reshape2)
  library(mltools)
  library(DescTools)
  library(plotly)
  library("BSDA")
# ----------------------------------Kiểm tra phân phối chuẩn--------------------------
  ND_Check <- function(data0){
    qqnorm(data0)
    qqline(data0, col="red", lwd=2) # Thêm chuẩn màu đỏ
    p=shapiro.test(data0)
  }
  Z_TEST1 <- function(data0,num){
    z_qs=((mean(data0)-num)/sd(data0)*sqrt(length(data0)))
    gtth=qnorm(p=0.05,lower.tail = FALSE)
    if (z_qs>gtth) print("Có cơ sở để bác bỏ H0, chấp nhận H1")
    else  print("Có cơ sở để chấp nhận H0")
  }
  Z_TEST2 <- function(data1,data2){
    z_qs=(mean(data1)-mean(data2))/sqrt(sd(data1)^2/length(data1) + sd(data2)^2/length(data2))
    gtth=qnorm(p=0.05,lower.tail = FALSE)
    if (z_qs>gtth) print("Có cơ sở để bác bỏ H0, chấp nhận H1")
    else print("Có cơ sở để chấp nhận H0")
  }
#-----------------------------------------Đọc dữ liệu-------------------------------------------
  All_GPUs=read.csv("C:/Users/Tuan/Desktop/xstk/All_GPUs.csv", header = TRUE, na.strings = c("", "N/A"))
#-----------------------------------------Lọc dữ liệu------------------------------------------
  data=All_GPUs[,c("Manufacturer","Release_Price")]
  data = data[!is.na(data$Release_Price), ]
  data$Release_Price <- as.numeric(gsub("[\\$,]", "", data$Release_Price))
  Nvidia_Prices=data[data$Manufacturer=='Nvidia',]
  AMD_Prices=data[data$Manufacturer=='AMD',]
#------------------------------------Kiểm tra phân phối chuẩn--------------------------------------
  ND_Check(Nvidia_Prices$Release_Price)
  #Từ hình và p-value<0.05 => Không theo phân phối chuẩn
  ND_Check(AMD_Prices$Release_Price)
  #Từ hình và p-value<0.05 => Không theo phân phối chuẩn
  #-> Khonong theo phân phối chuẩn, mẫu lớn-> thực hiện z_test
#---------------------------------------Kiểm định giả thuyết---------------------------------------
#Kiểm định giả thuyết: Giá trung bình của GPU từ
    #H0: u=348
    #H1: u>384
    Z_TEST1(Nvidia_Prices$Release_Price,348)
    #Ý nghĩa thu được:
        #*cho thấy công nghệ tiên tiến như GPU có thể không dễ tiếp cận với người lao động trung bình tại Việt Nam, 
        #*có thể tạo ra rào cản trong việc tham gia vào các ngành công nghiệp công nghệ cao 
        #*như chơi game, AI và phát triển phần mềm.
#Kiểm định giả thuyết:Giá trung bình của GPU từ hãng Nvidia(u0)>hãng AMD(u1)
    #H0: u0=u1
    #H1: u0>u1
    Z_TEST2(Nvidia_Prices$Release_Price,AMD_Prices$Release_Price)
    #Ý nghĩa thu được
        #* -về thị trường, chiến lược:AMD có thể đang nhắm đến phân khúc người dùng tầm trung, phổ thông; trong khi Nvidia mở rộng sang
        #* những khách hàng có khả năng chi trả cao hơn
        #* -Về sản phẩm: Các sản phẩm của Nvidia được định giá cao hơn so với AMD
        #* -về phía người dùng: người dùng có vốn thấp có thể tham khảo sang GPU của AMD

