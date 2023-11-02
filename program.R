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
data_00850 <- read_xlsx('/Users/larry.chen.int/Fund_20180101_20231012/00850ESGFUND_20170101_20231012.xlsx')
data_TESG <- read_xlsx('/Users/larry.chen.int/Fund_20180101_20231012/TESG_20170101_20231012.xlsx')
data_fundBasic <- read_xlsx('/Users/larry.chen.int/Fund_20180101_20231012/FundBasic_20170101_20231012.xlsx')
data_fundNV <- read_xlsx('/Users/larry.chen.int/Fund_20180101_20231012/FundNV_20170101_20231012.xlsx')
data_fundHolding <- read_xlsx('/Users/larry.chen.int/Fund_20180101_20231012/FundHolding_20180101_20231012.xlsx')

# data_00850 <- read_xlsx('~/NSYSU/RA_Alin/Fund_20180101_20231012/00850ESGFUND_20170101_20231012.xlsx')
# data_TESG <- read_xlsx('~/NSYSU/RA_Alin/Fund_20180101_20231012/TESG_20170101_20231012.xlsx')
# data_fundBasic <- read_xlsx('~/NSYSU/RA_Alin/Fund_20180101_20231012/FundBasic_20170101_20231012.xlsx')
# data_fundNV <- read_xlsx('~/NSYSU/RA_Alin/Fund_20180101_20231012/FundNV_20170101_20231012.xlsx')
# data_fundHolding <- read_xlsx('~/NSYSU/RA_Alin/Fund_20180101_20231012/FundHolding_20180101_20231012.xlsx')

## Data brief ----
## 結論：有些基金可能沒有持股資料
length(unique(data_fundHolding$證券代碼)) # 827
length(unique(data_fundBasic$證券代碼)) # 4906
length(unique(data_TESG$證券代碼)) # 2606


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
## FYI: 
## 1. 271281筆資料，該基金在該月所有成分股沒有TESG分數，予以剔除，保留430287筆
## 2. 430287筆資料中，132552筆沒有TESG分數，補0

## Calculate ESG score ----
df_ESG_score <- df_Holding_TESG %>% 
  group_by(`證券代碼`, `證券名稱`, `年月`) %>% 
  arrange(`證券代碼`, `證券名稱`, `年月`) %>% 
  summarise(`當期ESG1` = (1/3) * sum(`投資比率％`*0.01*`是否為00850成分股`),
            `當期ESG2` = (1/3) * sum(`投資比率％`*0.01*`00850持股比例`),
            `當期ESG3` = (1/3) * sum(`投資比率％`*0.01*`成分股TESG等級分數`)) %>% 
  mutate(`累積ESG1` = lag(`當期ESG1`) + lag(`當期ESG1`, 2) + lag(`當期ESG1`, 3),
         `累積ESG2` = lag(`當期ESG2`) + lag(`當期ESG2`, 2) + lag(`當期ESG2`, 3),
         `累積ESG3` = lag(`當期ESG3`) + lag(`當期ESG3`, 2) + lag(`當期ESG3`, 3))
## FYI: 
## 1. 當期ESG1, ESG2因為00850上述較晚，所以2019/09以前為0




code_ESGfund <- c('00932', 'T0496A', 'T0496B', 'T0496C', 'T0496D', 'T0496E', 
                  'T0496F', 'T0496G', 'T0496H', 'T0496J', 'T0496K', 'T0496L', 
                  'T0496M', '00850', 'T0848A', 'T0848B', 'T0848C', 'T0848D',
                  'T0848E', 'T0848F', 'T0848G', 'T0848H', 'T0848J', 'T0848K',
                  'T0848L', 'T0848M', '00692', '00920', 'T1284A', 'T1284B',
                  'T1282A', 'T1282B', 'T1511A', 'T1511B', 'T1511C', 'T1511D',
                  'T1511E', 'T1511F', 'T1511G', 'T1511H', 'T1511J', 'T1511K',
                  'T1511L', 'T1511M', '00923', 'T2069Y', 'T2117A', 'T2117B',
                  'T2117C', 'T2117D', 'T2117E', 'T2117F', 'T2117G', 'T2117H',
                  'T2117J', 'T2117K', 'T2117L', 'T2117M', 'T2117N', 'T2117P',
                  'T2117Q', 'T2117R', 'T2117S', 'T2132A', 'T2132B', 'T2132C',
                  'T2132D', 'T2132E', 'T2132F', 'T2132G', 'T2132H', 'T2132J',
                  'T2132K', 'T2132L', 'T2132M', 'T2132N', 'T2132P', 'T2132Q',
                  'T2132R', 'T2132S', 'T2132T', 'T2132U', 'T2132V', 'T2132W',
                  'T2132X')

  
  
  