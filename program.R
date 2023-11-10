### cleaning tools
rm(list = ls()); gc()
options(scipen=999)

### ESG fund
# Setting working directory ----
setwd("~/ESG-fund-research")

# Package loading ----
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stargazer)

# Data Import ------------------------------------------------------------------
data_00850 <- read_xlsx('/Users/larry.chen.int/Fund_20170101_20231012/00850ESGFUND_20170101_20231012.xlsx')
data_TESG <- read_xlsx('/Users/larry.chen.int/Fund_20170101_20231012/TESG_20170101_20231012.xlsx')
data_fundBasic <- read_xlsx('/Users/larry.chen.int/Fund_20170101_20231012/FundBasic_20170101_20231012.xlsx')
data_fundNV <- read_xlsx('/Users/larry.chen.int/Fund_20170101_20231012/FundNV_20170101_20231012.xlsx')
data_fundHolding <- read_xlsx('/Users/larry.chen.int/Fund_20170101_20231012/FundHolding_20170101_20231231.xlsx')

# data_00850 <- read_xlsx('~/NSYSU/RA_Alin/Fund_20170101_20231012/00850ESGFUND_20170101_20231012.xlsx')
# data_TESG <- read_xlsx('~/NSYSU/RA_Alin/Fund_20170101_20231012/TESG_20170101_20231012.xlsx')
# data_fundBasic <- read_xlsx('~/NSYSU/RA_Alin/Fund_20170101_20231012/FundBasic_20170101_20231012.xlsx')
# data_fundNV <- read_xlsx('~/NSYSU/RA_Alin/Fund_20170101_20231012/FundNV_20170101_20231012.xlsx')
# data_fundHolding <- read_xlsx('~/NSYSU/RA_Alin/Fund_20170101_20231012/FundHolding_20170101_20231231.xlsx')

# Variables define -------------------------------------------------------------
code_ESGfund <- c('00932', 'T0496', '00850', 'T0848', '00692', '00920',
                  'T1284', 'T1282', 'T1511', '00923', 'T2069Y', 'T2117',
                  'T2132', 'T2133', 'T2135', 'T2137', 'T2578', '00888',
                  'T2589', '00930', '00883B', '00891', 'T2622', 'T2623',
                  '00928', 'T3349', 'T3527', 'T3528', 'T3615', '00878',
                  'T3788', 'T3812', 'T3813', 'T4136', 'T4138', '00890B',
                  'T4142', 'T4767', 'T4769', 'T4819', 'T4820', 'T4822',
                  'T4911', 'T5006')

# Data brief -------------------------------------------------------------------
length(unique(data_fundHolding$證券代碼)) # 842
length(unique(data_fundBasic$證券代碼)) # 4906
length(unique(data_TESG$證券代碼)) # 2606
### FYI: 
### 1. 有些基金可能沒有持股資料


# Data manipulating ------------------------------------------------------------
## *Naming rules* --------
#* TNA = 基金淨資產; Expense ratio = 成本費用率; AGE = 基金年紀; FLOW = 基金流量
#* w% = 投資比率％; I = 是否為00850成分股; Iw = 00850持股比例; TESG = 根據TESG等級計算的TESG分數
#* Return(t0) = 月報酬; Return(t0,t-11) = 年報酬; Return(t0,t-23) = 二年報酬
#* Return(t-1) = 前一月報酬; Return(t-1,t-12) = 截至前一月年報酬; Return(t-13,t-24) = 前年報酬

## Fund basic --------
df_fundBasic <- data_fundBasic %>% 
  separate(`證券代碼`, into = c('證券代碼', '證券名稱'), sep = ' ') %>% 
  rename(`TNA` = `基金淨資產`,
         `Expense ratio` = `成本費用率`) %>% 
  mutate(`AGE` = ceiling(time_length(interval(ymd(`成立日`),  ym(`年月`)), "years"))) %>% 
  select(-`基金全稱`, -`主基金代碼`, -`淨值`)

## Fund 00850 --------
df_00850 <- mutate(data_00850, `I` = 1) %>% 
  rename(`Iw` = `持股數/流通在外股數%`)

df_00850_components <- select(df_00850, c(`年月`, `標的碼`, `標的名稱`, `I`, `Iw`))

## TESG --------
df_TESG <- data_TESG %>% 
  separate(`證券代碼`, into = c('證券代碼', '證券名稱'), sep = ' ') %>% 
  mutate(`年月` = as.numeric(`年月`),
         `TESG分數` = as.numeric(`TESG分數`)) %>% 
  mutate(`TESG` = case_when(
    `TESG等級` == 'A+' ~ 7,
    `TESG等級` == 'A' ~ 6,
    `TESG等級` == 'B+' ~ 5,
    `TESG等級` == 'B' ~ 4,
    `TESG等級` == 'B-' ~ 3,
    `TESG等級` == 'C' ~ 2,
    `TESG等級` == 'C-' ~ 1
  ))

df_TESG_score <- select(df_TESG, c(`證券代碼`, `證券名稱`, `年月`, `TESG`)) %>% 
  rename(`標的碼` = `證券代碼`,
         `標的名稱` = `證券名稱`) %>% 
  mutate(`年月` = ifelse(`年月` == 201712, 201801, `年月`))

## Fund Holding --------
df_fundHolding <- data_fundHolding %>% 
  separate(`證券代碼`, into = c('證券代碼', '證券名稱'), sep = ' ') %>% 
  rename(`w%` = `投資比率％`) %>% 
  select(-`基金全稱`, -`主基金代碼`) %>% 
  mutate(`年月` = as.numeric(`年月`)) %>% 
  filter(!grepl('M|T', `標的碼`)) %>% 
  left_join(df_00850_components) %>% 
  mutate(`I` = ifelse(is.na(`I`), 0, `I`),
         `Iw` = ifelse(is.na(`Iw`), 0, `Iw`),)

## Fund NV --------
df_fundNV <- data_fundNV %>% 
  separate(`證券代碼`, into = c('證券代碼', '證券名稱'), sep = ' ') %>% 
  rename(`Return(t0)` = `近一個月變動率%`,
         `Return(t0,t-11)` = `近一年變動率%`,
         `Return(t0,t-23)` = `近二年變動率%`) %>% 
  select(-`基金全稱`, -`主基金代碼`) %>% 
  mutate(`年月` = floor(`年月日`/100) ) %>% 
  group_by(`證券代碼`) %>% 
  arrange(`證券代碼`, `年月`) %>% 
  mutate(`Return(t0)` = `Return(t0)` * 0.01,
         `Return(t0,t-11)` = as.numeric(`Return(t0,t-11)`) * 0.01,
         `Return(t0,t-23)` = as.numeric(`Return(t0,t-23)`) * 0.01) %>% 
  mutate(`Return(t-1)` = lag(`Return(t0)`),
         `Return(t-1,t-12)` = lag(`Return(t0,t-11)`),
         `Return(t-13,t-24)` = lag(`Return(t0,t-23)`) - `Return(t-1,t-12)`)
  
df_fundNV_return <- select(df_fundNV, c(`證券代碼`, `證券名稱`, `年月`, `Return(t0)`, `Return(t-1)`, `Return(t-1,t-12)`, `Return(t-13,t-24)`))  

# Combine data frame -----------------------------------------------------------

## Calculate FLOW --------
df_Basic_NV <- df_fundBasic %>% 
  left_join(df_fundNV_return) %>% 
  group_by(`證券代碼`) %>% 
  arrange(`證券代碼`, `年月`) %>% 
  mutate(`FLOW` = `TNA`/( lag(`TNA`)*(1+`Return(t0)`) ) - 1,
         `FLOW` = round(`FLOW`, 6))

## Put 0 in TESG score NA value --------
#* FYI: 
#* 1. 271281筆資料，該基金在該月所有成分股沒有TESG分數，予以剔除
#*    -> 總共除去315檔基金，保留430287筆
#* 2. 430287筆資料中，132552筆沒有TESG分數，補0
df_Holding_TESG <- df_fundHolding %>% 
  left_join(df_TESG_score) %>% 
  group_by(`標的碼`) %>%
  arrange(`標的碼`, `年月`) %>% 
  fill(`TESG`, .direction = "up") %>% 
  group_by(`證券代碼`, `年月`) %>% 
  arrange(`證券代碼`, `年月`) %>% 
  filter(!sum(is.na(`TESG`)) == n()) %>% 
  mutate(`TESG` = ifelse(is.na(`TESG`), 0, `TESG`)) %>% 
  group_by()

## Calculate ESG score --------
#* FYI: 
#* 1. 當期ESG1, ESG2因為00850上市較晚，所以2019/09以前為0
df_ESG_score <- df_Holding_TESG %>% 
  group_by(`證券代碼`, `證券名稱`, `年月`) %>% 
  arrange(`證券代碼`, `證券名稱`, `年月`) %>% 
  summarise(`當期ESG1` = (1/3) * sum(`w%`*0.01*`I`),
            `當期ESG2` = (1/3) * sum(`w%`*0.01*`Iw`),
            `當期ESG3` = (1/3) * sum(`w%`*0.01*`TESG`)) %>% 
  mutate(`ESG1` = lag(`當期ESG1`) + lag(`當期ESG1`, 2) + lag(`當期ESG1`, 3),
         `ESG2` = lag(`當期ESG2`) + lag(`當期ESG2`, 2) + lag(`當期ESG2`, 3),
         `ESG3` = lag(`當期ESG3`) + lag(`當期ESG3`, 2) + lag(`當期ESG3`, 3)) %>% 
  group_by(`年月`) %>% 
  mutate(`HIGH_ESG1` = ifelse(`ESG1` >= median(`ESG1`, na.rm = TRUE), 1, 0), 
         `HIGH_ESG2` = ifelse(`ESG2` >= median(`ESG2`, na.rm = TRUE), 1, 0), 
         `HIGH_ESG3` = ifelse(`ESG3` >= median(`ESG3`, na.rm = TRUE), 1, 0))

## Combine Basic & ESG score data --------
df_Basic_ESG <- df_Basic_NV %>% 
  left_join(df_ESG_score) %>% 
  group_by()

# Description Statistic --------------------------------------------------------
df_descriptionStat <- select(df_Basic_ESG, c(`HIGH_ESG1`, `HIGH_ESG2`, `HIGH_ESG3`, `ESG1`, `ESG2`, `ESG3`,
                                             `FLOW`, `TNA`, `AGE`, `Expense ratio`,
                                             `Return(t0)`, `Return(t-1)`, `Return(t-1,t-12)`, `Return(t-13,t-24)`)) %>% 
  na.omit()

descriptionStat_HIGH_ESG <- tapply(select(df_Basic_ESG, c(`ESG1`, `ESG2`, `ESG3`,
                                                          `FLOW`, `TNA`, `AGE`, `Expense ratio`,
                                                          `Return(t0)`, `Return(t-1)`, `Return(t-1,t-12)`, `Return(t-13,t-24)`)), 
                                   df_Basic_ESG$HIGH_ESG1, 
                                   summary)

# Data Export --------
# write_xlsx(df_Basic_ESG, '基金基本資料_20170101_20231231.xlsx')
# write_xlsx(df_Holding_TESG, '基金持股資料_20170101_20231231.xlsx')

#* FYI: 
#* 1. 當期ESG1, ESG2因為00850上市較晚，所以2019/09以前為0
#* 2. 有些基金在某月所有成分股沒有TESG分數，予以剔除
#* 3. 有些基金在某月某些成分股沒有TESG分數，以0補值
#* 4. 多數基金可能沒有持股資料，所以基金檔數 -> 基金基本資料 > 基金持股資料


