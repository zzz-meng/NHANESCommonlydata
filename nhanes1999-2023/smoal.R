#每次开始前运行（重置环境）
Sys.setenv(LANGUAGE='en')#设置报错为英文，便于debug
options(stringsAsFactors = F)
rm(list =ls())

#设置路径（修改成本次paper路径）
setwd("C:/Users/zzz/Documents/BaiduSyncdisk/research/")


# 下载 smoking,alcohol 数据1999-2023------------
# 加载必要的包
library(rvest)
library(httr)
library(xml2)

# 设置下载文件夹路径
download_folder <- "C:/Users/zzz/Documents/BaiduSyncdisk/research/NHANES1999-2023/question/smoal1999-2023"

# 确保下载文件夹存在
if (!dir.exists(download_folder)) {
  dir.create(download_folder)
}

# 定义年份范围，从1999年到2018年，每两年一个周期
years <- seq(1999, 2018, by = 2)

# 定义特殊的周期
special_cycles <- c("2017-2020", "2021-2023")

# 设置基础网址
base_url <- "https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component="

# 设置要抓取的组件
components <- c( "Questionnaire")

# 创建一个空列表，用来存储所有的URL
all_urls <- list()

# 动态生成所有需要抓取的 URL (按年份)
for (year in years) {
  for (component in components) {
    # 生成每个年份和组件的 URL
    url <- paste0(base_url, component, "&CycleBeginYear=", year)
    all_urls[[paste0(component, "_", year)]] <- url
  }
}

# 动态生成所有需要抓取的 URL (特殊周期)
for (cycle in special_cycles) {
  for (component in components) {
    # 生成每个周期的 URL
    url <- paste0(base_url, component, "&Cycle=", cycle)
    all_urls[[paste0(component, "_", cycle)]] <- url
  }
}

# 需要下载的 XPT 文件名列表
health_files <- c("SMQ.xpt","SMQ_B.xpt","SMQ_C.xpt","SMQ_D.xpt","SMQ_E.xpt","SMQ_F.xpt",
                  "SMQ_G.xpt","SMQ_H.xpt","SMQ_I.xpt","SMQ_J.xpt","P_SMQ.xpt","SMQ_L.xpt",
                  "ALQ.xpt","ALQ_B.xpt","ALQ_C.xpt","ALQ_D.xpt","ALQ_E.xpt","ALQ_F.xpt",
                  "ALQ_G.xpt","ALQ_H.xpt","ALQ_I.xpt","ALQ_J.xpt","P_ALQ.xpt","ALQ_L.xpt")

# 循环抓取每个 URL
for (url_name in names(all_urls)) {
  url <- all_urls[[url_name]]
  
  # 访问目标网页
  webpage <- read_html(url)
  
  # 查找所有下载链接，假设文件链接包含 .xpt 后缀
  download_links <- webpage %>%
    html_nodes("a") %>%  # 查找所有链接
    html_attr("href") %>% # 获取链接的 URL
    grep("\\.xpt$", ., value = TRUE) # 过滤只包含 .xpt 文件的链接
  
  # 打印当前抓取的网页和文件链接
  print(paste("正在访问:", url_name, "链接:", url))
  print(download_links)
  
  # 下载符合条件的文件（在 health_files 列表中的文件）
  for (file_link in download_links) {
    file_name <- basename(file_link)  # 获取文件名
    if (file_name %in% health_files) {  # 只下载指定的文件
      full_url <- paste0("https://wwwn.cdc.gov", file_link)  # 构建完整的文件下载链接
      print(paste("准备下载文件:", file_name, "从", full_url))
      
      # 使用 httr 包进行下载
      response <- httr::GET(full_url, httr::write_disk(file.path(download_folder, file_name), overwrite = TRUE))
      
      if (response$status_code == 200) {
        print(paste("文件下载完成:", file_name))
      } else {
        print(paste("下载失败:", file_name, "响应代码:", response$status_code))
      }
    }
  }
}


## 读取 smoking & alcohol 数据---------------
library(haven)
library(dplyr)
library(purrr)
# 设置文件路径
path <- "C:/Users/zzz/Documents/BaiduSyncdisk/research/NHANES1999-2023/question/smoal1999-2023"
# 需要下载的 XPT 文件名列表
smoking_alcohol_files <- c("SMQ.xpt","SMQ_B.xpt","SMQ_C.xpt","SMQ_D.xpt","SMQ_E.xpt","SMQ_F.xpt",
                           "SMQ_G.xpt","SMQ_H.xpt","SMQ_I.xpt","SMQ_J.xpt","P_SMQ.xpt","SMQ_L.xpt",
                           "ALQ.xpt","ALQ_B.xpt","ALQ_C.xpt","ALQ_D.xpt","ALQ_E.xpt","ALQ_F.xpt",
                           "ALQ_G.xpt","ALQ_H.xpt","ALQ_I.xpt","ALQ_J.xpt","P_ALQ.xpt","ALQ_L.xpt")

# 构建文件路径并逐一读取数据
smoking_alcohol_list <- lapply(smoking_alcohol_files, function(file) {
  file_path <- file.path(path, file) 
  read_xpt(file_path) 
})



## 提取存在的列------------
selected_columns_sa <- c('SEQN', 'SMQ020',  'SMQ040', 'ALQ100',  'ALD100','ALQ101','ALQ101','ALQ121')
sa_list_selected <- lapply(smoking_alcohol_list, function(x) {
  x %>% select(any_of(selected_columns_sa))
})

## 添加列检查和防错机制，同时重命名列-----------------
sa_list_selected <- lapply(sa_list_selected, function(df) {
  # 检查列是否存在，如果不存在就跳过
  if ("SMQ040" %in% colnames(df)) {
    df <- df %>% mutate(
      SMQ040 = case_when(
        SMQ040 %in% c(7, 9) ~ NA_real_,
        TRUE ~ SMQ040
      )
    )
  }
  
  if ("ALQ100" %in% colnames(df)) {
    df <- df %>% mutate(
      ALQ100 = case_when(
        ALQ100 %in% c(7, 9) ~ NA_real_,
        TRUE ~ ALQ100
      )
    )%>%
      rename(alcohol = ALQ100)  # 重命名 ALQ100 为 alcohol
  }
  
  if ("ALD100" %in% colnames(df)) {
    df <- df %>% mutate(
      ALD100 = case_when(
        ALD100 %in% c(7, 9) ~ NA_real_,
        TRUE ~ ALD100
      )
    )%>%
      rename(alcohol = ALD100)  # 重命名 ALD100 为 alcohol
  }
  
  if ("ALQ101" %in% colnames(df)) {
    df <- df %>% mutate(
      ALQ101 = case_when(
        ALQ101 %in% c(7, 9) ~ NA_real_,
        TRUE ~ ALQ101
      )
    )%>%
      rename(alcohol = ALQ101)  # 重命名 ALQ101 为 alcohol
  }
  
  if ("ALQ121" %in% colnames(df)) {
    df <- df %>% mutate(
      ALQ121 = case_when(
        ALQ121 %in% c(1, 2, 3, 4, 5, 6) ~ 1,
        ALQ121 %in% c(0, 7, 8, 9, 10) ~ 2,
        ALQ121 %in% c(77, 99) ~ NA_real_,
        TRUE ~ ALQ121
      )
    )%>%
      rename(alcohol = ALQ121)  # 重命名 ALQ121 为 alcohol
  }
  return(df)
})

# 创建一个函数来合并小列表到DataFrame
merge_sublists_to_df <- function(sublist_indices, main_list) {
  # 使用lapply和bind_rows将索引指定的小列表合并成一个数据框
  do.call(bind_rows, lapply(sublist_indices, function(i) as.data.frame(main_list[[i]])))
}

# 定义索引范围
smoking_indices <- 1:12
alcohol_indices <- 13:24


# 合并小列表为数据框
smoking_noclear <- merge_sublists_to_df(smoking_indices, sa_list_selected)
alcohol_noclear <- merge_sublists_to_df(alcohol_indices, sa_list_selected)

# 导出为CSV文件
write.csv(alcohol_noclear, "alcohol_noclear.csv", row.names = FALSE)

#2.添加哑变量--------------
#2.1 分类变量
alcohol_noclear <- alcohol_noclear %>%
  mutate( alcoholnew = case_when(
    is.na(alcohol) ~ 3,       # 将 NA 替换为 4
    TRUE ~ alcohol            # 保留其他值
  )
  )  


## 整理数据----------------------------------------------------
#第一步：清理 SMQ020 和 SMQ040 中的无效值
smoking_noclear <- smoking_noclear %>%
  mutate(
    # 清理 SMQ020：将拒绝回答(7)、不知道(9) 设为 NA
    SMQ020 = case_when(
      SMQ020 %in% c(7, 9) ~ NA_real_,
      SMQ020 %in% c(1, 2) ~ SMQ020,
      TRUE ~ NA_real_
    ),
    
    # 清理 SMQ040：保留 1=偶尔, 2=每天, 3=不吸；无效值设为 NA
    SMQ040 = case_when(
      SMQ040 %in% c(7, 9) ~ NA_real_,           # 拒绝回答、不知道 → NA
      SMQ040 %in% c(1, 2, 3) ~ SMQ040,          # 有效值保留
      TRUE ~ NA_real_                           # 其他非法值 → NA
    )
  )

# 第二步：判断吸烟状态
##判断吸烟的标准,满足条件之一：-----------------------------
#(1)抽烟超过100,SMQ020=='1'
#(2)抽烟不超过100,但目前在抽烟,每天或者偶尔抽。
smoking_noclear <- smoking_noclear %>%
  mutate(
    smoke = case_when(
      # 条件1：SMQ020 缺失 → NA
      is.na(SMQ020) ~ NA_real_,
      
      # 条件2：抽过 ≥100 支烟（SMQ020 == 1）→ 吸烟者（无论现在是否吸）
      SMQ020 == 1 ~ 1L,
      
      # 条件3：没抽过 100 支（SMQ020 == 2），但现在抽烟（每天或偶尔）→ 吸烟者
      SMQ020 == 2 & SMQ040 %in% c(1, 2) ~ 1L,
      
      # 条件4：没抽过 100 支，且现在不抽烟（SMQ040 == 3）→ 从不吸烟
      SMQ020 == 2 & SMQ040 == 3 ~ 2L,
      
      # 条件5：没抽过 100 支，SMQ040 缺失 → 推断为不吸烟（保守）
      SMQ020 == 2 & is.na(SMQ040) ~ 2L,
    )
  )

# 导出为CSV文件
write.csv(smoking_noclear, "smoking_noclear.csv", row.names = FALSE)



