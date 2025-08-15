#每次开始前运行（重置环境）
Sys.setenv(LANGUAGE='en')#设置报错为英文，便于debug
options(stringsAsFactors = F)
rm(list =ls())

#设置路径（修改成本次paper路径）
setwd("C:/Users/Documents")

## 读取数据---------------
library(haven)
library(dplyr)
library(readr)
library(purrr)
library(remotes)
# 从本地文件夹安装
# remotes::install_local("C:/Users/Documents") 
library(dietaryindex)


# 设置文件路径
path <- "C:/Users/Documents"
load("NHANES_20052006.rda")## NHANES 2005-2006
load("NHANES_20072008.rda")## NHANES 2007-2008
load("NHANES_20092010.rda")## NHANES 2009-2010
load("NHANES_20112012.rda")## NHANES 2011-2012
load("NHANES_20132014.rda")## NHANES 2013-2014
load("NHANES_20152016.rda")## NHANES 2015-2016
data("NHANES_20172018")## NHANES 2017-2018 内置在dietaryindex包中
load("NHANES_20172020.rda")## NHANES 2017-2020


##计算饮食模型指数（HEI2015:2015-2020,HEI2020:2020-2025）
# 定义所有周期（与已加载的数据框名称对应）
cycles <- c("20052006", 
            "20072008", "20092010", "20112012", "20132014", 
            "20152016", "20172018", "20172020")

# 初始化列表存储各周期HEI结果
hei2015_results <- list()

# 循环计算每个周期的HEI2015
for (cycle in cycles) {
  # 通过周期名获取已加载的数据框（如NHANES_20172018）
  nhanes_data <- get(paste0("NHANES_", cycle))
  
  # 计算HEI2015（与2017-2018的格式完全一致）
  hei_result <- HEI2015_NHANES_FPED(
    FPED_PATH = nhanes_data$FPED, 
    NUTRIENT_PATH = nhanes_data$NUTRIENT, 
    DEMO_PATH = nhanes_data$DEMO,
    FPED_PATH2 = nhanes_data$FPED2,
    NUTRIENT_PATH2 = nhanes_data$NUTRIENT2
  )
  
  # 添加周期标识列，方便后续分析
  hei_result$cycle <- cycle
  
  # 存入结果列表
  hei2015_results[[cycle]] <- hei_result
  
  # 打印进度
  cat("已完成", cycle, "周期的HEI2015计算，样本量：", nrow(hei_result), "\n")
}

# 合并所有周期结果
hei2015_0520 <- do.call(rbind, hei2015_results)
write.csv(hei2015_0520, "hei2015_0520.csv", row.names = FALSE)

