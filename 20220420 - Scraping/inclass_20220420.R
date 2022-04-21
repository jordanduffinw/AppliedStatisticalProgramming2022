library(tidyverse)
library(rvest)

arkansas_url <- "https://ballotpedia.org/United_States_Senate_elections_in_Arkansas,_2014"
arkansas_page <- read_html(arkansas_url)

arkansas_page %>% html_nodes("tbody") %>% 
  .[2] %>% 
  html_text()


maintext <- arkansas_page %>% 
  html_nodes('p') %>% 
  html_text()
maintext[83:91]

table1 <- arkansas_page %>% html_nodes('center table') %>% 
  html_table(header=TRUE)
table1[[1]][,2:5]

table2 <- arkansas_page %>% 
  html_nodes('.wikitable th , .wikitable center') %>% 
  html_text()
table2[[1]]

primaryDate <- arkansas_page %>% 
  html_nodes('td:nth-child(2) center') %>% 
  html_text()
primaryDate

cooksScore <- arkansas_page %>% 
  html_nodes('.infobox br~ b') %>% 
  html_text()
cooksScore

stateLinks <- arkansas_page %>% 
  html_nodes('small center a') %>% 
  html_attr('href')

stateLinks[3] <- "/United_States_Senate_elections_in_Arkansas,_2014"
stateLinks

x<-stateLinks[3]
thisUrl<-paste0("https://ballotpedia.org", x)
thisUrl

this_page<-read_html(thisUrl)

scrapeResults<-function(x, stateLinks){
  thisUrl<-paste0("https://ballotpedia.org", stateLinks[x])
  table.out<-read_html(thisUrl) %>%
    html_nodes('center table') %>%
    html_table(header=TRUE)
  table.out
}
results1<-scrapeResults(3, stateLinks=stateLinks)
str(results1)

mainText<-arkansas_page %>%
  html_nodes('p') %>%
  html_text()

scrapeText <- function(x, stateLinks){
  thisURL <- paste0("https://ballotpedia.org", stateLinks[x])
  text.out <- read_html(thisURL) %>% 
    html_nodes('p') %>% 
    html_text()
  text.out
}

results2 <- scrapeText(x = X, stateLinks = stateLinks)
results2[20:21]


allResults <- NULL
for (i in 1:33) {
  print(i)
  allResults[i] <- scrapeText(x = i, stateLinks = stateLinks)
  Sys.sleep(3)
}

gs_url<-"https://scholar.google.com/scholar?hl=en&as_sdt=7%2C26&q=political+parties&btnG="
pg1<-gs_url%>%
  read_html()%>%
  html_nodes(".gs_fl , .gs_rs , .gs_a , .gs_rt") %>%
  html_text()
pg1
