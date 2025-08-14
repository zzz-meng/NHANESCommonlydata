#每次开始前运行（重置环境）
Sys.setenv(LANGUAGE='en')#设置报错为英文，便于debug
options(stringsAsFactors = F)
rm(list =ls())

#设置路径（修改成本次paper路径）
setwd("C:/Users/zzz/Documents/BaiduSyncdisk/research/")


# 下载 blood pressure,diabetes1999-2023数据------------
# 加载必要的包
library(rvest)
library(httr)
library(xml2)

# 设置下载文件夹路径
download_folder <- "C:/Users/zzz/Documents/BaiduSyncdisk/research/NHANES1999-2023/question/bpdi1999-2023"

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
health_files <- c("BPQ.xpt","BPQ_B.xpt","BPQ_C.xpt","BPQ_D.xpt","BPQ_E.xpt","BPQ_F.xpt",
                  "BPQ_G.xpt","BPQ_H.xpt","BPQ_I.xpt","BPQ_J.xpt","P_BPQ.xpt","BPQ_L.xpt",
                  "DIQ.xpt","DIQ_B.xpt","DIQ_C.xpt","DIQ_D.xpt","DIQ_E.xpt","DIQ_F.xpt",
                  "DIQ_G.xpt","DIQ_H.xpt","DIQ_I.xpt","DIQ_J.xpt","P_DIQ.xpt","DIQ_L.xpt")

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


## 读取数据---------------
library(haven)
library(dplyr)
library(purrr)
# 设置文件路径
path <- "C:/Users/zzz/Documents/BaiduSyncdisk/research/NHANES1999-2023/question/bpdi1999-2023"
# 需要下载的 XPT 文件名列表
health_files <- c("BPQ.xpt","BPQ_B.xpt","BPQ_C.xpt","BPQ_D.xpt","BPQ_E.xpt","BPQ_F.xpt",
                  "BPQ_G.xpt","BPQ_H.xpt","BPQ_I.xpt","BPQ_J.xpt","P_BPQ.xpt","BPQ_L.xpt",
                  "DIQ.xpt","DIQ_B.xpt","DIQ_C.xpt","DIQ_D.xpt","DIQ_E.xpt","DIQ_F.xpt",
                  "DIQ_G.xpt","DIQ_H.xpt","DIQ_I.xpt","DIQ_J.xpt","P_DIQ.xpt","DIQ_L.xpt")

# 构建文件路径并逐一读取数据
health_list <- lapply(health_files, function(file) {
  file_path <- file.path(path, file) 
  read_xpt(file_path) 
})



## 提取存在的列(BP, DI)----------------
selected_columns_health <- c('SEQN', 'BPQ020',  'DIQ010')
health_list_selected <- lapply(health_list, function(x) {
  x %>% select(any_of(selected_columns_health))
})

# 创建一个函数来合并小列表到DataFrame
merge_sublists_to_df <- function(sublist_indices, main_list) {
  # 使用lapply和bind_rows将索引指定的小列表合并成一个数据框
  do.call(bind_rows, lapply(sublist_indices, function(i) as.data.frame(main_list[[i]])))
}

# 定义索引范围
BP_indices <- 1:12
DI_indices <- 13:24


# 合并小列表为数据框
BP_noclear <- merge_sublists_to_df(BP_indices, health_list_selected)
DI_noclear <- merge_sublists_to_df(DI_indices, health_list_selected)
BP_noclear <- BP_noclear %>%rename(BP = BPQ020)
DI_noclear <- DI_noclear %>%rename(DI = DIQ010)


## 转换缺失值-------------
BP_noclear <- BP_noclear %>%
  mutate(
    # 首先确保 BP 列是数值型，以正确处理数值比较
    BP = as.numeric(as.character(BP)),  # 如果已经是数值型可省略此步骤
    # 使用 case_when 来设置新的 BP 值
    BP = case_when(
      BP %in% c(7, 9) ~ NA_real_,       # 将 '7' 和 '9' 替换为 NA，NA_real_ 用于数值型 NA
      TRUE ~ BP                         # 保留其他值
    ))
DI_noclear <- DI_noclear %>%
  mutate(
    # 首先确保 DI 列是数值型，以正确处理数值比较
    DI = as.numeric(as.character(DI)),  # 如果已经是数值型可省略此步骤
    # 使用 case_when 来设置新的 DI 值
    DI = case_when(
      DI %in% c(7, 9) ~ NA_real_,       # 将 '7' 和 '9' 替换为 NA，NA_real_ 用于数值型 NA
      TRUE ~ DI                         # 保留其他值
    ))


# 导出为CSV文件
write.csv(BP_noclear, "BP_noclear.csv", row.names = FALSE)
write.csv(DI_noclear, "DI_noclear.csv", row.names = FALSE)
