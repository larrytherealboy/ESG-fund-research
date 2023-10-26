### cleaning tools
rm(list = ls()); gc()

### ESG fund
# install.packages("readxl")

# loading package
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)

# loading data in local
data00850 <- read_xlsx('/Users/larry.chen.int/Fund_20180101_20231012/00850ESGFUND_20170101_20231012.xlsx')
dataFundBasic <- read_xlsx('/Users/larry.chen.int/Fund_20180101_20231012/FundBasic_20170101_20231012.xlsx')
dataFundHolding <- read_xlsx('/Users/larry.chen.int/Fund_20180101_20231012/FundHolding_20180101_20231012.xlsx')
dataTESG <- read_xlsx('/Users/larry.chen.int/Fund_20180101_20231012/TESG_20180101_20231012.xlsx')

TESG <- dataTESG %>% 
  separate(`證券代碼`, into = c('證券代碼', '證券名稱'), sep = ' ') %>% 
  mutate(`TESG等級分數` = case_when(
    `TESG等級` == 'A+' ~ 7,
    `TESG等級` == 'A' ~ 6,
    `TESG等級` == 'B+' ~ 5,
    `TESG等級` == 'B' ~ 4,
    `TESG等級` == 'B-' ~ 3,
    `TESG等級` == 'C' ~ 2,
    `TESG等級` == 'C-' ~ 1
  ))
  