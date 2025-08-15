#每次开始前运行（重置环境）
Sys.setenv(LANGUAGE='en')#设置报错为英文，便于debug
options(stringsAsFactors = F)
rm(list =ls())

#设置路径（修改成本次paper路径）
setwd("C:/Users/Documents")


# 下载 demo 数据(1999-2023)------------
# 加载必要的包
library(rvest)
library(httr)
library(xml2)
install.packages("stringi")

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
components <- c( "Demographics")

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
demo_files <- c("DEMO.xpt","DEMO_B.xpt","DEMO_C.xpt","DEMO_D.xpt","DEMO_E.xpt","DEMO_F.xpt",
                "DEMO_G.xpt","DEMO_H.xpt","DEMO_I.xpt","DEMO_J.xpt","P_DEMO.xpt","DEMO_L.xpt")

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
  
  # 下载符合条件的文件（在 demo_files 列表中的文件）
  for (file_link in download_links) {
    file_name <- basename(file_link)  # 获取文件名
    if (file_name %in% demo_files) {  # 只下载指定的文件
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


## 读取数据-------------------------------------------
library(haven)
library(dplyr)
library(purrr)
# 设置文件路径
path <- "C:/Users/Documents"
# 需要下载的 XPT 文件名列表
demo_files <- c("DEMO.xpt","DEMO_B.xpt","DEMO_C.xpt","DEMO_D.xpt","DEMO_E.xpt","DEMO_F.xpt",
                "DEMO_G.xpt","DEMO_H.xpt","DEMO_I.xpt","DEMO_J.xpt","P_DEMO.xpt","DEMO_L.xpt")

# 构建文件路径并逐一读取数据
demo_list <- lapply(demo_files, function(file) {
  file_path <- file.path(path, file) 
  read_xpt(file_path) 
})

## 提取想要的列（age, gender, race, edu, pir）----------------
selected_columns_demo <- c('SEQN', 'RIDAGEYR', 'RIAGENDR', 'RIDRETH1',  'DMDEDUC2', 'INDFMPIR','SDMVPSU','SDMVSTRA')
demo_list_selected <- lapply(demo_list, function(x) {
  x %>% select(any_of(selected_columns_demo))
})
# 使用 lapply 遍历 demo_list_selected，重命名指定的列
demo_list_selected <- lapply(demo_list_selected, function(x) {
  x %>% rename(
    age = RIDAGEYR,
    gender = RIAGENDR,
    race = RIDRETH1,
    edu = DMDEDUC2,
    pir = INDFMPIR
  )
})

# 将列表中的各个数据框合并为一个数据框
demo_noclean <- do.call(rbind, demo_list_selected)

## 将高中及以下学历合并-------------
demo_noclean <- demo_noclean %>%
  mutate(
    # 确保 edu 是数值型（如果原来是因子或字符）
    edu = as.numeric(as.character(edu)),
    
    # 重新编码教育程度
    edu = case_when(
      edu %in% c(1, 2, 3) ~ 1L,           # 高中及以下 → 1
      edu == 4 ~ 2L,                      # 高中毕业 → 2
      edu == 5 ~ 3L,                      # 大学及以上 → 3
      edu %in% c(7, 9) ~ NA_real_,       # 拒绝回答、不知道 → NA
      TRUE ~ NA_real_                     # 其他非法值也设为 NA（保险起见）
    )
  )

# 导出为CSV文件
write.csv(demo_noclean, "demo_noclean.csv", row.names = FALSE)



#2.添加哑变量--------------
#2.1 分类变量
all_date_GNRI_del <- all_date_GNRI_del %>%
  mutate(
    pirnew = case_when(
      pir <= 1 ~ 1,
      pir > 1 & pir <= 3 ~ 2,
      pir > 3 ~ 3
    ),
    
    pirnew = case_when(
      is.na(pirnew) ~ 4,       # 将 NA 替换为 0
      TRUE ~ pirnew            # 保留其他值
    )
    )

