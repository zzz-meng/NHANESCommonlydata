#每次开始前运行（重置环境）
Sys.setenv(LANGUAGE='en')#设置报错为英文，便于debug
options(stringsAsFactors = F)
rm(list =ls())

#设置路径（修改成本次paper路径）
setwd("C:/Users/Documents")


# 下载Question数据(1999-2023)------------
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
sitting_files <- c("PAQ.xpt","PAQ_B.xpt","PAQ_C.xpt","PAQ_D.xpt","PAQ_E.xpt","PAQ_F.xpt",
                   "PAQ_G.xpt","PAQ_H.xpt","PAQ_I.xpt","PAQ_J.xpt","P_PAQ.xpt","PAQ_L.xpt")


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
  
  # 下载符合条件的文件（在 sitting_files 列表中的文件）
  for (file_link in download_links) {
    file_name <- basename(file_link)  # 获取文件名
    if (file_name %in% sitting_files) {  # 只下载指定的文件
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


## 读取sitting数据(2007-2023)------------
library(haven)
library(dplyr)
library(purrr)
# 设置文件路径
path <- "C:/Users/Documents"

# 需要下载的 XPT 文件名列表
sitting_files <- c("PAQ_E.xpt","PAQ_F.xpt",
                   "PAQ_G.xpt","PAQ_H.xpt","PAQ_I.xpt","PAQ_J.xpt","P_PAQ.xpt","PAQ_L.xpt")


# 构建文件路径并逐一读取数据
sitting_list <- lapply(sitting_files, function(file) {
  file_path <- file.path(path, file) 
  read_xpt(file_path) 
})

# 提取存在的列
selected_columns_sitting <- c('SEQN', 'PAD680')
sitting_list_selected <- lapply(sitting_list, function(x) {
  x %>% select(any_of(selected_columns_sitting))
})
sitting_list_selected <- lapply(sitting_list_selected, function(x) {
  x %>% rename(
    sitting= PAD680,
  )
})

# 将列表中的各个数据框合并为一个数据框
sitting_noclean<- do.call(rbind, sitting_list_selected)

# 导出为CSV文件
write.csv(sitting_noclean, "sitting_noclean0723.csv", row.names = FALSE)


## 删除缺失值(暂时不用）----------
sitting <- all_sitting_noclean %>%
  dplyr::filter(!is.na(SEQN), 
                !is.na(sitting), 
                sitting != "7777", 
                sitting != "9999") 




## 读取PA数据(2007-2020)--------------------------
library(haven)
library(dplyr)
library(purrr)
# 设置文件路径
path <- "C:/Users/Documents"
# 需要下载的 XPT 文件名列表
PA_files <- c("PAQ_E.xpt","PAQ_F.xpt",
              "PAQ_G.xpt","PAQ_H.xpt","PAQ_I.xpt","PAQ_J.xpt","P_PAQ.xpt")

# 构建文件路径并逐一读取数据
PA_list <- lapply(PA_files, function(file) {
  file_path <- file.path(path, file) 
  read_xpt(file_path) 
})

# 提取存在的列
selected_columns_PA <- c('SEQN', 'PAQ620','PAQ635','PAQ665','PAQ625','PAQ640','PAQ670','PAD630','PAD645','PAD675')
PA_list_selected <- lapply(PA_list, function(x) {
  x %>% select(any_of(selected_columns_PA))
})

# 将列表中的各个数据框合并为一个数据框
all_PA_noclean<- do.call(rbind, PA_list_selected)
# 将 'PAQ625', 'PAQ640', 'PAQ670' ,'PAD630','PAD645','PAD675'列中值为 77 或 99 或 NA 的数赋值为 0
all_PA_noclean <- all_PA_noclean %>%
  mutate(across(c(PAQ625, PAQ640, PAQ670), ~ case_when(
    is.na(.) | . %in% c(77, 99) ~ 0,
    TRUE ~ .
  )))
all_PA_noclean <- all_PA_noclean %>%
  mutate(across(c(PAD630,PAD645,PAD675), ~ case_when(
    is.na(.) | . %in% c(7777, 9999) ~ 0,
    TRUE ~ .
  )))


## 2.3计算中等强度运动时间，生成新列 'moderate-time/w'--------------
all_PA_noclean <- all_PA_noclean %>%
  mutate(`moderate-time/w` = (PAQ625 * PAD630) + 
           (PAQ640 * PAD645) + 
           (PAQ670 * PAD675))

# 删除 'moderate-time/w' 列中有缺失值或等于0的行，并选择特定列
time <- all_PA_noclean %>%
  filter(!is.na(`moderate-time/w`), `moderate-time/w` != 0) %>%
  select(SEQN, `moderate-time/w`)


# 筛选出 'PAQ620', 'PAQ635', 'PAQ665' 列中值同时为2的行
filtered_data_YorN <- all_PA_noclean %>%
  filter(PAQ620 == 2 & 
           PAQ635 == 2 & 
           PAQ665 == 2)
# 创建或更新 'moderate-act/w' 列：当 'PAQ620', 'PAQ635', 'PAQ665' 列同时为2时，设置为0
filtered_data_YorN <- filtered_data_YorN %>%
  mutate(`moderate-time/w` = if_else(PAQ620 == 2 & PAQ635 == 2 & PAQ665 == 2, 0, NA_real_)) %>% 
  select(SEQN,`moderate-time/w`)

# 按行合并两个数据框并选择特定列
all_PA <- rbind(time, filtered_data_YorN)

# 导出为CSV文件
write.csv(all_PA, "PA_noclean0720.csv", row.names = FALSE)

