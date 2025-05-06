# library(car)       # để dùng leveneTest
# library(dplyr)     # để xử lý dữ liệu

# # 2. Phân nhóm hãng
# AMD <- subset(new_GPU_data, Manufacturer == "AMD")
# Nvidia <- subset(new_GPU_data, Manufacturer == "Nvidia")
# 
# # 3. Kiểm tra phân phối chuẩn
# qnorm(AMD$Memory_Bandwidth)
# qqline(AMD$Memory_Bandwidth, col="red")
# shapiro.test(AMD$Memory_Bandwidth)     # -> p < 0.05
# 
# qqnorm(Nvidia$Memory_Bandwidth)
# qqline(Nvidia$Memory_Bandwidth, col="red")
# shapiro.test(Nvidia$Memory_Bandwidth)  # -> p < 0.05
# 
# # 4. Kiểm định đồng nhất phương sai
# leveneTest(new_GPU_data$Memory_Bandwidth ~ as.factor(new_GPU_data$Manufacturer), data = new_GPU_data)
# # -> p < 0.05, không đồng nhất phương sai
# 
# # 5. Phân tích phương sai (ANOVA)
# anova_model <- aov(Memory_Bandwidth ~ Manufacturer, data = new_GPU_data)
# summary(anova_model)

Mem_Anova = c("Memory_Bandwidth", "Manufacturer")
Memory_Anova <- new_GPU_data[, Mem_Anova]
Memory_Anova <- Memory_Anova[Memory_Anova$Manufacturer %in% c("ATI", "Intel", "Nvidia", "AMD"),]
summary(Memory_Anova)

MBandwidth_ATI <- subset(Memory_Anova, Memory_Anova$Manufacturer == "ATI")
MBandwidth_Intel <- subset(Memory_Anova, Memory_Anova$Manufacturer == "Intel")
MBandwidth_Nvidia <- subset(Memory_Anova, Memory_Anova$Manufacturer == "Nvidia")
MBandwidth_AMD <- subset(Memory_Anova, Memory_Anova$Manufacturer == "AMD")

par(mfrow = c(2,2))

qqnorm(MBandwidth_Nvidia$Memory_Bandwidth, main = "Nvidia")
qqline(MBandwidth_Nvidia$Memory_Bandwidth)

qqnorm(MBandwidth_AMD$Memory_Bandwidth, main = "AMD")
qqline(MBandwidth_AMD$Memory_Bandwidth)

qqnorm(MBandwidth_Intel$Memory_Bandwidth, main = "Intel")
qqline(MBandwidth_Intel$Memory_Bandwidth)

qqnorm(MBandwidth_ATI$Memory_Bandwidth, main = "ATI")
qqline(MBandwidth_ATI$Memory_Bandwidth)

par(mfrow = c(1,1))

shapiro.test(MBandwidth_Nvidia$Memory_Bandwidth)
shapiro.test(MBandwidth_AMD$Memory_Bandwidth)
shapiro.test(MBandwidth_Intel$Memory_Bandwidth)
shapiro.test(MBandwidth_ATI$Memory_Bandwidth)

library(car)
leveneTest(Memory_Bandwidth ~ Manufacturer, data = Memory_Anova)

Anova_model <- aov(Memory_Bandwidth ~ Manufacturer, data = Memory_Anova)
summary(Anova_model)

TukeyHSD(Anova_model)

plot(TukeyHSD(Anova_model))
