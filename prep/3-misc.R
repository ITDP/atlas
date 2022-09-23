#get list of indicators

library(googlesheets4)
library(readr)
library(data.table)

list_indicators <- read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/13LZoiy0RcQ_ivc8SOoiU9ctHq5GQY48dpNbCpU9GzKk/edit#gid=0",
  sheet = "Indicators"
) %>% setDT()


# salvar
write_rds(list_indicators, "data/sample3/list_indicators.rds")
