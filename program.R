### cleaning tools
rm(list = ls()); gc()

### ESG fund

# Package loading ----
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(lubridate)

# Data loading ----
# data_00850 <- read_xlsx('/Users/larry.chen.int/Fund_20180101_20231012/00850ESGFUND_20170101_20231012.xlsx')
# data_TESG <- read_xlsx('/Users/larry.chen.int/Fund_20180101_20231012/TESG_20170101_20231012.xlsx')
# data_fundBasic <- read_xlsx('/Users/larry.chen.int/Fund_20180101_20231012/FundBasic_20170101_20231012.xlsx')
# data_fundHolding <- read_xlsx('/Users/larry.chen.int/Fund_20180101_20231012/FundHolding_20180101_20231012.xlsx')

data_00850 <- read_xlsx('~/NSYSU/RA_Alin/Fund_20180101_20231012/00850ESGFUND_20170101_20231012.xlsx')
data_TESG <- read_xlsx('~/NSYSU/RA_Alin/Fund_20180101_20231012/TESG_20170101_20231012.xlsx')
data_fundBasic <- read_xlsx('~/NSYSU/RA_Alin/Fund_20180101_20231012/FundBasic_20170101_20231012.xlsx')
data_fundNV <- read_xlsx('~/NSYSU/RA_Alin/Fund_20180101_20231012/FundNV_20170101_20231012.xlsx')
data_fundHolding <- read_xlsx('~/NSYSU/RA_Alin/Fund_20180101_20231012/FundHolding_20180101_20231012.xlsx')

## Data brief ----
## 結論：有些基金可能沒有持股資料
length(unique(data_fundHolding$證券代碼)) # 827
length(unique(data_fundBasic$證券代碼)) # 4906
length(unique(data_TESG$證券代碼)) # 2561


# Data manipulating ----

## Fund basic ----
df_fundBasic <- data_fundBasic %>% 
  separate(`證券代碼`, into = c('證券代碼', '證券名稱'), sep = ' ') %>% 
  select(-`基金全稱`, -`主基金代碼`, -`淨值`)

## Fund 00850 ----
df_00850 <- mutate(data_00850, `是否為00850成分股` = 1) %>% 
  rename(`00850持股比例` = `持股數/流通在外股數%`)

df_00850Components <- select(df_00850, c(`年月`, `標的碼`, `標的名稱`, `00850持股比例`, `是否為00850成分股`))


## TESG ----
df_TESG <- data_TESG %>% 
  separate(`證券代碼`, into = c('證券代碼', '證券名稱'), sep = ' ') %>% 
  mutate(`年月` = as.numeric(`年月`),
         `TESG分數` = as.numeric(`TESG分數`)) %>% 
  mutate(`成分股TESG等級分數` = case_when(
    `TESG等級` == 'A+' ~ 7,
    `TESG等級` == 'A' ~ 6,
    `TESG等級` == 'B+' ~ 5,
    `TESG等級` == 'B' ~ 4,
    `TESG等級` == 'B-' ~ 3,
    `TESG等級` == 'C' ~ 2,
    `TESG等級` == 'C-' ~ 1
  )) %>% 
  mutate(`採用永續報告書年度` = ifelse(is.na(`採用永續報告書年度`),  year(ym(`年月`))-1, `採用永續報告書年度`))

df_scoreTESG <- select(df_TESG, c(`證券代碼`, `證券名稱`, `年月`, `成分股TESG等級分數`)) %>% 
  rename(`標的碼` = `證券代碼`,
         `標的名稱` = `證券名稱`) %>% 
  mutate(`年月` = ifelse(`年月` == 201712, 201801, `年月`))



## Fund Holding ----
df_fundHolding <- data_fundHolding %>% 
  separate(`證券代碼`, into = c('證券代碼', '證券名稱'), sep = ' ') %>% 
  select(-`基金全稱`, -`主基金代碼`) %>% 
  mutate(`年月` = as.numeric(`年月`)) %>% 
  filter(!grepl('M|T', `標的碼`)) %>% 
  left_join(df_00850Components) %>% 
  mutate(`是否為00850成分股` = ifelse(is.na(`是否為00850成分股`), 0, `是否為00850成分股`),
         `00850持股比例` = ifelse(is.na(`00850持股比例`), 0, `00850持股比例`),)


## Fund NV ----
df_fundNV <- data_fundNV %>% 
  separate(`證券代碼`, into = c('證券代碼', '證券名稱'), sep = ' ') %>% 
  select(-`基金全稱`, -`主基金代碼`) %>% 
  mutate(`年月` = floor(`年月日`/100) ) %>% 
  group_by(`證券代碼`) %>% 
  arrange(`證券代碼`, `年月`) %>% 
  mutate(`近一個月變動率%` = `近一個月變動率%` * 0.01,
         `近一年變動率%` = as.numeric(`近一年變動率%`) * 0.01,
         `近二年變動率%` = as.numeric(`近二年變動率%`) * 0.01) %>% 
  mutate(`前一個月變動率%` = lag(`近一個月變動率%`),
         `前一年變動率%` = lag(`近一年變動率%`),
         `前二年變動率%` = `近二年變動率%` - `近一年變動率%`)

df_fundReturn <- select(df_fundNV, c(`證券代碼`, `證券名稱`, `年月`, `近一個月變動率%`, `前一個月變動率%`, `前一年變動率%`, `前二年變動率%`))  



# Combine data frame ----
## Calculate FLOW ----
df_Basic_NV <- df_fundBasic %>% 
  left_join(df_fundReturn) %>% 
  group_by(`證券代碼`) %>% 
  arrange(`證券代碼`, `年月`) %>% 
  mutate(`基金流量` = `基金淨資產`/( lag(`基金淨資產`)*(1+`近一個月變動率%`) ) - 1)

## 去掉成分股沒有TESG分數
df_Holding_TESG <- df_fundHolding %>% 
  left_join(df_scoreTESG) %>% 
  group_by(`標的碼`) %>%
  arrange(`標的碼`, `年月`) %>% 
  fill(`成分股TESG等級分數`, .direction = "up") %>% 
  group_by(`證券代碼`) %>% 
  arrange(`證券代碼`, `年月`) %>% 
  filter(!is.na(`成分股TESG等級分數`)) %>% 
  mutate(`當期ESG3` = 1/3 * sum(`投資比率％`*0.01*`成分股TESG等級分數`))

  
