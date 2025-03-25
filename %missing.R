Intel_CPUs <- read.csv("~/HCMUT/242 XSTK/btl/archive/Intel_CPUs.csv")
head(Intel_CPUs)

Intel_CPUs[Intel_CPUs == ""] <- NA
Intel_CPUs[] <- lapply(Intel_CPUs, function(x) gsub("^\\n$", NA, x))
Intel_CPUs[] <- lapply(Intel_CPUs, function(x) gsub("^\\n- $", NA , x))

library(questionr)
freq.na(Intel_CPUs)
