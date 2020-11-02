library(dplyr)
library(magrittr)
library(readr)
library(rvest)
library(stringr)

# Bring in data

## Diaspora
diasporaUrl <- read_html('https://en.wikipedia.org/wiki/African_diaspora')

diasporaPop <- diasporaUrl %>% 
  html_nodes(xpath = '/html/body/div[3]/div[3]/div[5]/div[1]/table[2]/tbody/tr') 

#define the empty table

ncol <-  diasporaPop %>%
  .[[1]] %>%
  html_children()%>%
  length()
nrow <- length(diasporaPop)
diaspora <- as.data.frame(matrix(nrow = nrow,ncol = ncol))


# fill the table
for(i in 1:nrow){
  # get content of the line
  linecontent <- diasporaPop[[i]]%>%
    html_children()%>%
    html_text()%>%
    gsub("\n","",.)
  
  # attribute the content to free columns
  colselect <- is.na(diaspora[i,])
  diaspora[i,colselect] <- linecontent
  
  # get the line repetition of each columns
  repetition <- diasporaPop[[i]]%>%
    html_children()%>%
    html_attr("rowspan")%>%
    ifelse(is.na(.),1,.) %>% # if no rowspan, then it is a normal row, not a multiple one
    as.numeric
  
  # repeat the cells of the multiple rows down
  for(j in 1:length(repetition)){
    span <- repetition[j]
    if(span > 1){
      diaspora[(i+1):(i+span-1),colselect][,j] <- rep(linecontent[j],span-1)
    }
  }
}

diaspora <- data_frame(diaspora)
names(diaspora)<- c('Country', 'Country Population', 'African Percentage', 'African Population')
  
diasporaPopulation <- diaspora[-1,]
rm(diaspora, diasporaPop, diasporaUrl, i, j, linecontent, ncol,nrow, repetition, span, colselect)

diasporaPopulation <- diasporaPopulation %>% 
  filter(!Country %in% c("Caribbean", "South America","North America","Central America","Europe"))


## African Population

populationUrl <- read_html("https://www.worldometers.info/geography/how-many-countries-in-africa/") 
population <-  populationUrl %>% 
  html_nodes('#example2 td , #example2 .sorting , .sorting_desc') %>% 
  html_text() %>% 
  matrix(nrow = 54 , ncol = 4, byrow = TRUE)

population <-  as.data.frame(population)
population <- data_frame(population)

names(population) <- c("num", "Country", "Population", "Region")
population <- population[,-1]


## Africa GDP and PPP
gdpPPPFull <- read_csv("datasets/worldgdpPPP.csv", skip = 4, col_names = TRUE)

gdpPPPFull <- gdpPPPFull %>% 
  select(`Country Name`, `Country Code`, `2015`, `2016`, `2017`, `2018`, `2019`) %>%
  mutate(`Country Name`= replace(`Country Name`, `Country Name`=="Egypt, Arab Rep.","Egypt")) %>%
  mutate(`Country Name`= replace(`Country Name`, `Country Name`=="Congo, Dem. Rep.", "DR Congo")) %>% 
  mutate(`Country Name`= replace(`Country Name`, `Country Name`=="Cote d'Ivoire","Côte d'Ivoire")) %>%
  mutate(`Country Name`= replace(`Country Name`, `Country Name`=="Congo, Rep.","Congo")) %>% 
  mutate(`Country Name`= replace(`Country Name`, `Country Name`=="Gambia, The", "Gambia")) %>% 
  mutate(`Country Name`= replace(`Country Name`, `Country Name`=="Sao Tome and Principe","Sao Tome & Principe"))

names(gdpPPPFull) <- c("Country", "Code","gdppp-2015","gdppp-2016","gdppp-2017","gdppp-2018","gdppp-2019")


## GDP per capita
gdpPerCapFull<- read_csv("datasets/worldgdpPer.csv", skip = 4, col_names = TRUE)

gdpPerCapFull <- gdpPerCapFull %>% 
  select(`Country Name`, `Country Code`, `2015`, `2016`, `2017`, `2018`, `2019`) %>%
  mutate(`Country Name`= replace(`Country Name`, `Country Name`=="Egypt, Arab Rep.","Egypt")) %>%
  mutate(`Country Name`= replace(`Country Name`, `Country Name`=="Congo, Dem. Rep.", "DR Congo")) %>% 
  mutate(`Country Name`= replace(`Country Name`, `Country Name`=="Cote d'Ivoire","Côte d'Ivoire")) %>%
  mutate(`Country Name`= replace(`Country Name`, `Country Name`=="Congo, Rep.","Congo")) %>% 
  mutate(`Country Name`= replace(`Country Name`, `Country Name`=="Gambia, The", "Gambia")) %>% 
  mutate(`Country Name`= replace(`Country Name`, `Country Name`=="Sao Tome and Principe","Sao Tome & Principe"))

names(gdpPerCapFull) <- c("Country", "Code","gdppc-2015","gdppc-2016","gdppc-2017","gdppc-2018","gdppc-2019")


## Capital Cities
capitals <- read_csv('https://raw.githubusercontent.com/icyrockcom/country-capitals/master/data/country-list.csv', col_names = TRUE)

capitals <- capitals[,-3]

names(capitals) <- c("Country", "Capital City")

capitals <- capitals %>% 
  mutate(Country = replace(Country, Country == "Democratic Republic of the Congo", "DR Congo")) %>% 
  mutate(Country = replace(Country, Country == "Cape Verde", "Cabo Verde")) %>% 
  mutate(Country = replace(Country, Country == "São Tomé and Príncipe", "Sao Tome & Principe")) %>% 
  mutate(Country = replace(Country, Country == "Swaziland", "Eswatini")) %>% 
  mutate(Country = replace(Country, Country == "Republic of the Congo", "Congo"))


# Join data
africaData <- population %>%
  left_join(gdpPerCapFull, by="Country") %>% 
  left_join(gdpPPPFull, by = "Country") %>% 
  left_join(capitals, by = "Country")

#Preview Data - dim(), str(), summary(), colnames(),head(), view()

write_csv(africaData,'datasets/africaData.csv')

rm(list=setdiff(ls(), "africaData"))
