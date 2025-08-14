#每次开始前运行（重置环境）
Sys.setenv(LANGUAGE='en')#设置报错为英文，便于debug
options(stringsAsFactors = F)
rm(list =ls())

#设置路径（修改成本次paper路径）
setwd("C:/Users/zzz/Documents/BaiduSyncdisk/research/NHANES1999-2023/question/meduse1999-2023")



# 1.下载 meduse 数据(99-02,03-12,13-20类似，其中2021-2023年只有2组数据故排除）------------------
# 加载必要的包
library(rvest)
library(httr)
library(xml2)
# 设置下载文件夹路径
download_folder <- "C:/Users/zzz/Documents/BaiduSyncdisk/research/NHANES1999-2023/question/meduse1999-2023"

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
meduse_files <- c("RXQ_RX.xpt","RXQ_RX_B.xpt","RXQ_RX_C.xpt","RXQ_RX_D.xpt","RXQ_RX_E.xpt","RXQ_RX_F.xpt",
                  "RXQ_RX_G.xpt","RXQ_RX_H.xpt","RXQ_RX_I.xpt","RXQ_RX_J.xpt","P_RXQ_RX.xpt","RXQ_RX_L.xpt")

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
  
  # 下载符合条件的文件（在 meduse_files 列表中的文件）
  for (file_link in download_links) {
    file_name <- basename(file_link)  # 获取文件名
    if (file_name %in% meduse_files) {  # 只下载指定的文件
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
path <- "C:/Users/zzz/Documents/BaiduSyncdisk/research/NHANES1999-2023/question/meduse1999-2023"

# 需要下载的 XPT 文件名列表
meduse_files <- c("RXQ_RX.xpt","RXQ_RX_B.xpt","RXQ_RX_C.xpt","RXQ_RX_D.xpt","RXQ_RX_E.xpt","RXQ_RX_F.xpt",
                  "RXQ_RX_G.xpt","RXQ_RX_H.xpt","RXQ_RX_I.xpt","RXQ_RX_J.xpt","P_RXQ_RX.xpt")

# 构建文件路径并逐一读取数据
meduse_list <- lapply(meduse_files, function(file) {
  file_path <- file.path(path, file) 
  read_xpt(file_path) 
})

# 提取存在的列
selected_columns_meduse <- c("SEQN","RXDDRGID")
meduse_list_selected <- lapply(meduse_list, function(x) {
  x %>% select(any_of(selected_columns_meduse))
})
# 将列表中的各个数据框合并为一个数据框(223038)
meduse_noclean <- do.call(rbind, meduse_list_selected)
write.csv(meduse_noclean, "meduse_noclean.csv", row.names = FALSE)



## 读medtype文件-------------------
medtype <- read_xpt("RXQ_DRUG.xpt")
write.csv(medtype, "medtype.csv", row.names = FALSE)



## 提取质子泵抑制剂(暂时无用，根据文章需求提取相应种类的药物）-------------------------
# 根据 RXDDRGID 匹配样本medtype
target_ids <- c('d00325', 'd03828', 'd04272', 'd04448', 'd04514', 
                'd04749', 'd04913', 'd05399', 'd05770', 'd07395', 'd07631')

meduse_noclean1 <- meduse_noclean %>%
  mutate(
    PPI = case_when(
      # 判断字符型“缺失”：包括空字符串和 "NA"（不区分大小写可加 tolower）
      RXDDRGID == "" | RXDDRGID == "NA" | RXDDRGID == "na" ~ NA_real_,
      
      # 匹配目标药物，,若在目标列表中，PPI = 1
      RXDDRGID %in% target_ids ~ 1,
      
      # 其他情况
      TRUE ~ 0
    )
  )
## meduse去重-------------------------
meduse_noclean2 <- meduse_noclean1 %>%
  arrange(SEQN, desc(PPI)) %>%    # 按 SEQN 分组，PPI 降序排列（PPI=1 排在前面）
  group_by(SEQN) %>%
  slice_head(n = 1) %>%           # 每组只保留第一行（即 PPI=1 的优先）
  ungroup()

write.csv(meduse_noclean2, "meduse9920.csv", row.names = FALSE)

