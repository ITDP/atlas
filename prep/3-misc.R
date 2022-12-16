#get list of indicators

library(googlesheets4)
library(readr)
library(data.table)

list_indicators <- read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/13LZoiy0RcQ_ivc8SOoiU9ctHq5GQY48dpNbCpU9GzKk/edit#gid=0",
  sheet = "Indicators"
) 


# salvar
write_rds(list_indicators, "data/sample5/list_indicators.rds")


# block density list
list_block <- read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1c3KL909dthuTFMq_snm55fURK4v5yCWJ-30kRqtnaqw/edit?usp=sharing",
  sheet = "Sheet1"
) %>% as.data.frame()


# salvar
write_rds(list_block, "data/sample5/list_block_density.rds")
