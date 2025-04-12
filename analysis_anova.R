library(car)       # để dùng leveneTest
library(dplyr)     # để xử lý dữ liệu

# 1. Chuẩn bị dữ liệu
GPU <- read.csv("D:/242_XSTK/XSTK242-Assignment/All_GPUs.csv", stringsAsFactors = FALSE)
GPU[GPU == ""] <- NA
GPU$Memory_Bandwidth <- as.numeric(gsub(" GB/s", "", GPU$Memory_Bandwidth))

GPU <- GPU[!is.na(GPU$Memory_Bandwidth), ]

# 2. Phân nhóm hãng
AMD <- subset(GPU, Manufacturer == "AMD")
Nvidia <- subset(GPU, Manufacturer == "Nvidia")

# 3. Kiểm tra phân phối chuẩn
qnorm(AMD$Memory_Bandwidth)
qqline(AMD$Memory_Bandwidth, col="red")
shapiro.test(AMD$Memory_Bandwidth)     # -> p < 0.05

qqnorm(Nvidia$Memory_Bandwidth)
qqline(Nvidia$Memory_Bandwidth, col="red")
shapiro.test(Nvidia$Memory_Bandwidth)  # -> p < 0.05

# 4. Kiểm định đồng nhất phương sai
leveneTest(Memory_Bandwidth ~ as.factor(Manufacturer), data = GPU)

# 5. Phân tích phương sai (ANOVA)
anova_model <- aov(Memory_Bandwidth ~ Manufacturer, data = GPU)
summary(anova_model)