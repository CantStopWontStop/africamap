library(rjson)
library(jsonlite)

json_file <- fromJSON("https://gist.githubusercontent.com/DimsumPanda/88aa1d70cd0c394feeae/raw/192d7eed828268ea91b24c100063992042f86835/africa.topojson", flatten = FALSE)
africa_properties <- json_file$objects$collection$geometries$properties

africa <- africaData %>% 
  left_join(africa_properties, by = c('Code.x'='sov_a3'))

write_csv(africa_properties, 'map/africaProperties.csv')
