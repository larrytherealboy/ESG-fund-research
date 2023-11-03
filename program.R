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
data_00850 <- read_xlsx('/Users/larry.chen.int/Fund_20170101_20231012/00850ESGFUND_20170101_20231012.xlsx')
data_TESG <- read_xlsx('/Users/larry.chen.int/Fund_20170101_20231012/TESG_20170101_20231012.xlsx')
data_fundBasic <- read_xlsx('/Users/larry.chen.int/Fund_20170101_20231012/FundBasic_20170101_20231012.xlsx')
data_fundNV <- read_xlsx('/Users/larry.chen.int/Fund_20170101_20231012/FundNV_20170101_20231012.xlsx')
data_fundHolding <- read_xlsx('/Users/larry.chen.int/Fund_20170101_20231012/FundHolding_20170101_20231231.xlsx')

# data_00850 <- read_xlsx('~/NSYSU/RA_Alin/Fund_20180101_20231012/00850ESGFUND_20170101_20231012.xlsx')
# data_TESG <- read_xlsx('~/NSYSU/RA_Alin/Fund_20180101_20231012/TESG_20170101_20231012.xlsx')
# data_fundBasic <- read_xlsx('~/NSYSU/RA_Alin/Fund_20180101_20231012/FundBasic_20170101_20231012.xlsx')
# data_fundNV <- read_xlsx('~/NSYSU/RA_Alin/Fund_20180101_20231012/FundNV_20170101_20231012.xlsx')
# data_fundHolding <- read_xlsx('~/NSYSU/RA_Alin/Fund_20180101_20231012/FundHolding_20180101_20231012.xlsx')

## Data brief ----
length(unique(data_fundHolding$證券代碼)) # 842
length(unique(data_fundBasic$證券代碼)) # 4906
length(unique(data_TESG$證券代碼)) # 2606
### FYI: 
### 1. 有些基金可能沒有持股資料


# Data manipulating ----

## Fund basic ----
df_fundBasic <- data_fundBasic %>% 
  separate(`證券代碼`, into = c('證券代碼', '證券名稱'), sep = ' ') %>% 
  select(-`基金全稱`, -`主基金代碼`, -`淨值`)

## Fund 00850 ----
df_00850 <- mutate(data_00850, `是否為00850成分股` = 1) %>% 
  rename(`00850持股比例` = `持股數/流通在外股數%`)

df_00850_components <- select(df_00850, c(`年月`, `標的碼`, `標的名稱`, `00850持股比例`, `是否為00850成分股`))

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
  ))

df_TESG_score <- select(df_TESG, c(`證券代碼`, `證券名稱`, `年月`, `成分股TESG等級分數`)) %>% 
  rename(`標的碼` = `證券代碼`,
         `標的名稱` = `證券名稱`) %>% 
  mutate(`年月` = ifelse(`年月` == 201712, 201801, `年月`))

## Fund Holding ----
df_fundHolding <- data_fundHolding %>% 
  separate(`證券代碼`, into = c('證券代碼', '證券名稱'), sep = ' ') %>% 
  select(-`基金全稱`, -`主基金代碼`) %>% 
  mutate(`年月` = as.numeric(`年月`)) %>% 
  filter(!grepl('M|T', `標的碼`)) %>% 
  left_join(df_00850_components) %>% 
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

df_fundNV_return <- select(df_fundNV, c(`證券代碼`, `證券名稱`, `年月`, `近一個月變動率%`, `前一個月變動率%`, `前一年變動率%`, `前二年變動率%`))  


# Combine data frame ----

## Calculate FLOW ----
df_Basic_NV <- df_fundBasic %>% 
  left_join(df_fundNV_return) %>% 
  group_by(`證券代碼`) %>% 
  arrange(`證券代碼`, `年月`) %>% 
  mutate(`基金流量` = `基金淨資產`/( lag(`基金淨資產`)*(1+`近一個月變動率%`) ) - 1,
         `基金流量` = round(`基金流量`, 6))

## Put 0 in TESG score NA value ----
df_Holding_TESG <- df_fundHolding %>% 
  left_join(df_TESG_score) %>% 
  group_by(`標的碼`) %>%
  arrange(`標的碼`, `年月`) %>% 
  fill(`成分股TESG等級分數`, .direction = "up") %>% 
  group_by(`證券代碼`, `年月`) %>% 
  arrange(`證券代碼`, `年月`) %>% 
  filter(!sum(is.na(`成分股TESG等級分數`)) == n()) %>% 
  mutate(`成分股TESG等級分數` = ifelse(is.na(`成分股TESG等級分數`), 0, `成分股TESG等級分數`)) %>% 
  group_by()
### FYI: 
### 1. 271281筆資料，該基金在該月所有成分股沒有TESG分數，予以剔除，保留430287筆
### 2. 430287筆資料中，132552筆沒有TESG分數，補0

## Calculate ESG score ----
df_ESG_score <- df_Holding_TESG %>% 
  group_by(`證券代碼`, `證券名稱`, `年月`) %>% 
  arrange(`證券代碼`, `證券名稱`, `年月`) %>% 
  summarise(`當期ESG1` = (1/3) * sum(`投資比率％`*0.01*`是否為00850成分股`),
            `當期ESG2` = (1/3) * sum(`投資比率％`*0.01*`00850持股比例`),
            `當期ESG3` = (1/3) * sum(`投資比率％`*0.01*`成分股TESG等級分數`)) %>% 
  mutate(`累積ESG1` = lag(`當期ESG1`) + lag(`當期ESG1`, 2) + lag(`當期ESG1`, 3),
         `累積ESG2` = lag(`當期ESG2`) + lag(`當期ESG2`, 2) + lag(`當期ESG2`, 3),
         `累積ESG3` = lag(`當期ESG3`) + lag(`當期ESG3`, 2) + lag(`當期ESG3`, 3)) %>% 
  group_by(`年月`) %>% 
  mutate(`HIGH_ESG1` = ifelse(`累積ESG1` >= median(`累積ESG1`, na.rm = TRUE), 1, 0), 
         `HIGH_ESG2` = ifelse(`累積ESG2` >= median(`累積ESG2`, na.rm = TRUE), 1, 0), 
         `HIGH_ESG3` = ifelse(`累積ESG3` >= median(`累積ESG3`, na.rm = TRUE), 1, 0))
### FYI: 
### 1. 當期ESG1, ESG2因為00850上述較晚，所以2019/09以前為0




code_ESGfund <- c('00932', 'T0496', '00850', 'T0848', '00692', '00920',
                  'T1284', 'T1282', 'T1511', '00923', 'T2069Y', 'T2117',
                  'T2132', 'T2133', 'T2135', 'T2137', 'T2578', '00888',
                  'T2589', '00930', '00883B', '00891', 'T2622', 'T2623',
                  '00928', 'T3349', 'T3527', 'T3528', 'T3615', '00878',
                  'T3788', 'T3812', 'T3813', 'T4136', 'T4138', '00890B',
                  'T4142', 'T4767', 'T4769', 'T4819', 'T4820', 'T4822',
                  'T4911', 'T5006',  )

  
  
  