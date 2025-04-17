library(car)       # để dùng leveneTest
library(dplyr)     # để xử lý dữ liệu

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

# 2. Phân nhóm hãng
AMD <- subset(new_GPU_data, Manufacturer == "AMD")
Nvidia <- subset(new_GPU_data, Manufacturer == "Nvidia")

# 3. Kiểm tra phân phối chuẩn
qnorm(AMD$Memory_Bandwidth)
qqline(AMD$Memory_Bandwidth, col="red")
shapiro.test(AMD$Memory_Bandwidth)     # -> p < 0.05

qqnorm(Nvidia$Memory_Bandwidth)
qqline(Nvidia$Memory_Bandwidth, col="red")
shapiro.test(Nvidia$Memory_Bandwidth)  # -> p < 0.05

# 4. Kiểm định đồng nhất phương sai
leveneTest(new_GPU_data$Memory_Bandwidth ~ as.factor(new_GPU_data$Manufacturer), data = new_GPU_data)
# -> p < 0.05, không đồng nhất phương sai

# 5. Phân tích phương sai (ANOVA)
anova_model <- aov(Memory_Bandwidth ~ Manufacturer, data = new_GPU_data)
summary(anova_model)