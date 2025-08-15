#每次开始前运行（重置环境）
Sys.setenv(LANGUAGE='en')#设置报错为英文，便于debug
options(stringsAsFactors = F)
rm(list =ls())

#设置路径（修改成本次paper路径）
setwd("C:/Users/Documents")


# 下载Question数据(1999-2023)------------------
# 加载必要的包
library(rvest)
library(httr)
library(xml2)

# 设置下载文件夹路径
download_folder <- "C:/Users/Documents"

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
GI_files <- c("MCQ.xpt","MCQ_B.xpt","MCQ_C.xpt","MCQ_D.xpt","MCQ_E.xpt","MCQ_F.xpt",
              "MCQ_G.xpt","MCQ_H.xpt","MCQ_I.xpt","MCQ_J.xpt","P_MCQ.xpt","MCQ_L.xpt")

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
  
  # 下载符合条件的文件（在 GI_files 列表中的文件）
  for (file_link in download_links) {
    file_name <- basename(file_link)  # 获取文件名
    if (file_name %in% GI_files) {  # 只下载指定的文件
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
path <- "C:/Users/Documents"

# 需要下载的 XPT 文件名列表
med_files <- c("MCQ.xpt","MCQ_B.xpt","MCQ_C.xpt","MCQ_D.xpt","MCQ_E.xpt","MCQ_F.xpt",
              "MCQ_G.xpt","MCQ_H.xpt","MCQ_I.xpt","MCQ_J.xpt","P_MCQ.xpt","MCQ_L.xpt")

# 构建文件路径并逐一读取数据
med_list <- lapply(med_files, function(file) {
  file_path <- file.path(path, file) 
  read_xpt(file_path) 
})




## 提取想要的列（暂时无用，需要根据具体所需要的数据查询代码）这里的220&230A指的是癌症----------------
selected_columns_med <- c("SEQN","MCQ220", "MCQ230A")
med_list_selected <- lapply(med_files, function(x) {
  x %>% select(any_of(selected_columns_med))
})

# 将列表中的各个数据框合并为一个数据框()
med_noclean <- do.call(rbind, GI_list_selected)





## 删除缺失值（暂时无用）-----------
# 在数据框all_GI中新建一列GI，将"MCQ230A"列中为17,35,22,29,16,31的人标记为1，其余为0；
all_GI <- all_GI_noclean %>%
  dplyr::filter(!is.na(SEQN), 
                !is.na(MCQ230A), 
                MCQ230A != "77", 
                MCQ230A != "99") %>%
  select(SEQN,MCQ230A)  # 选择需要的列

all_GI <- all_GI %>%
  mutate(GI = ifelse(MCQ230A %in% c(17, 35, 22, 29, 16, 31), 1, 0))%>%
  select(SEQN,GI)  # 选择需要的列


# 使用 dplyr::filter 进行筛选
# 在数据框all_GI中新建一列GI，将"MCQ220"列中为2的人标记为0，其余为1；
all_cancer <- all_GI_noclean %>%
  dplyr::filter(!is.na(SEQN),
                !is.na(MCQ220), 
                MCQ220 != "1",
                MCQ220 != "7", 
                MCQ220 != "9") %>%
  select(SEQN, MCQ220)  # 选择需要的列
all_cancer <- all_cancer %>%
  mutate(GI = ifelse(MCQ220 %in% c(2), 0, 1))%>%
  select(SEQN,GI)  # 选择需要的列

# 使用dplyr::bind_rows直接按行合并(64188)
all_GI_merged <- dplyr::bind_rows(all_GI, all_cancer)

