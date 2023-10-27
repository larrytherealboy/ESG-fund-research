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
data_fundBasic <- read_xlsx('/Users/larry.chen.int/Fund_20180101_20231012/FundBasic_20170101_20231012.xlsx')
data_fundHolding <- read_xlsx('/Users/larry.chen.int/Fund_20180101_20231012/FundHolding_20180101_20231012.xlsx')
data_TESG <- read_xlsx('/Users/larry.chen.int/Fund_20180101_20231012/TESG_20170101_20231012.xlsx')

## Data brief ----
## 結論：有些基金可能沒有持股資料
length(unique(data_fundHolding$證券代碼)) # 1854
length(unique(data_fundBasic$證券代碼)) # 4906
length(unique(data_TESG$證券代碼)) # 2561


# Data manipulating ----

## Fund basic ----
df_fundBasic <- data_fundBasic %>% 
  group_by(`證券代碼`) %>% 
  arrange(`證券代碼`, `年月`) %>% 
  mutate(`基金月報酬率` = `淨值`/lag(`淨值`, 1) - 1,
         `基金前一月報酬率` = lag(`基金月報酬率`),
         `基金去年報酬率` = lag(`淨值`, 1)/lag(`淨值`, 12) - 1,
         `基金前年報酬率` = lag(`淨值`, 13)/lag(`淨值`, 24) - 1,
         `基金前二年報酬率` = lag(`淨值`, 25)/lag(`淨值`, 36) - 1) %>% 
  mutate(`基金流量` = `基金淨資產`/( lag(`基金淨資產`)*(1+`基金月報酬率`) ) - 1)

  
## Fund 00850 ----
df_00850 <- mutate(data_00850, `是否為00850成分股` = 1) %>% 
  rename(`持股數/流通在外股數%_00850` = `持股數/流通在外股數%`)

df_00850Components <- select(df_00850, c(`年月`, `標的碼`, `標的名稱`, `持股數/流通在外股數%_00850`, `是否為00850成分股`))


## TESG ----
df_TESG <- data_TESG %>% 
  separate(`證券代碼`, into = c('證券代碼', '證券名稱'), sep = ' ') %>% 
  mutate(`有效年份` = year(ym(`年月`))) %>% 
  mutate(`TESG等級分數` = case_when(
    `TESG等級` == 'A+' ~ 7,
    `TESG等級` == 'A' ~ 6,
    `TESG等級` == 'B+' ~ 5,
    `TESG等級` == 'B' ~ 4,
    `TESG等級` == 'B-' ~ 3,
    `TESG等級` == 'C' ~ 2,
    `TESG等級` == 'C-' ~ 1
  ))

df_scoreTESG <- select(df_TESG, c(`證券代碼`, `證券名稱`, `年月`, `有效年份`, `TESG等級分數`))



## Fund Holding ----
df_fundHolding <- data_fundHolding %>% 
  filter(`年月` >= 201909) %>% 
  left_join(df_00850Components, by = c('年月', '標的碼', '標的名稱')) %>% 
  mutate(`是否為00850成分股` = ifelse(is.na(`是否為00850成分股`), 0, `是否為00850成分股`))





  
