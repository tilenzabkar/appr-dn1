library(tidyverse)
library(rvest)
library(readxl)
load(file="tabele.RData")

stran_glavna_mesta = 
  "https://kt.ijs.si/~ljupco/lectures/appr-2425-dn1-podatki/List%20of%20capitals%20in%20the%20United%20States%20-%20Wikipedia.html" %>% 
  read_html()
stran_predsedniki =
  "https://kt.ijs.si/~ljupco/lectures/appr-2425-dn1-podatki/List%20of%20presidents%20of%20the%20United%20States%20-%20Wikipedia.html" %>% 
  read_html()
stran_predsedniki_leta = 
  "https://kt.ijs.si/~ljupco/lectures/appr-2425-dn1-podatki/List%20of%20presidents%20of%20the%20United%20States%20by%20date%20of%20death%20-%20Wikipedia.html" %>% 
  read_html()
stran_predsedniki_rojstva = 
  "https://kt.ijs.si/~ljupco/lectures/appr-2425-dn1-podatki/List%20of%20presidents%20of%20the%20United%20States%20by%20home%20state%20-%20Wikipedia.html" %>% 
  read_html()
stran_podpredsedniki = 
  "https://kt.ijs.si/~ljupco/lectures/appr-2425-dn1-podatki/List%20of%20vice%20presidents%20of%20the%20United%20States%20-%20Wikipedia.html" %>% 
  read_html()
stran_podpredsedniki_starost = 
  "https://kt.ijs.si/~ljupco/lectures/appr-2425-dn1-podatki/List%20of%20vice%20presidents%20of%20the%20United%20States%20by%20age%20-%20Wikipedia.html" %>% 
  read_html()

tabela_glavna_mesta = stran_glavna_mesta %>%
  html_nodes(xpath="//table[@class='wikitable plainrowheaders sortable jquery-tablesorter']") %>%
  .[[1]] %>%
  html_table() 
tabela_predsedniki_leta = stran_predsedniki_leta %>%
  html_nodes(xpath="//table[@class='wikitable sortable jquery-tablesorter']") %>%
  .[[1]] %>%
  html_table() 
tabela_predsedniki_rojstva = stran_predsedniki_rojstva %>%
  html_nodes(xpath="//table[@class='wikitable sortable jquery-tablesorter']") %>%
  .[[1]] %>%
  html_table() 
tabela_podpredsedniki = stran_podpredsedniki %>%
  html_nodes(xpath="//table[@class='wikitable sortable sticky-header jquery-tablesorter']") %>%
  .[[1]] %>%
  html_table() 
tabela_podpredsedniki_starost = stran_podpredsedniki_starost %>%
  html_nodes(xpath="//table[@class='sortable wikitable jquery-tablesorter']") %>%
  .[[1]] %>%
  html_table() 

setwd("~/Desktop/faks/appr")
d = dir()
populacija_mest = str_subset(d, "Data.csv")
mesta_populacije_vsa_leta = lapply(populacija_mest, read_csv)

populacija_drzave = str_subset(d, "states.+.xlsx")
populacija_drzave_vsa_leta = lapply(populacija_drzave, read_excel)


tabela_predsedniki = stran_predsedniki %>%
  html_nodes(xpath="//table[@class='wikitable sortable sticky-header jquery-tablesorter']") %>%
  .[[1]] %>%
  html_table()


colnames(tabela_predsedniki) = str_replace_all(colnames(tabela_predsedniki), "\\[.+\\]|\\(.+\\)", "")
tabela_predsedniki = tabela_predsedniki %>% 
  .[,c(-1, -2, -5, -8)] %>% 
  lapply(str_replace_all, pattern="\\[.+?\\]", replacement="") %>%  # odstranimo opombe
  as.tibble() %>% 
  rename(predsednik = Name, mandat = Term, stranka = Party, volitve = Election) %>% 
  separate(mandat, into=c("zacetek_mandata", "konec_mandata"), sep="–") %>% 
  mutate(zacetek_mandata = as.Date(strptime(zacetek_mandata, "%B %d, %Y"))) %>% 
  mutate(konec_mandata = as.Date(strptime(konec_mandata, "%B %d, %Y"))) %>% 
  mutate(predsednik = lapply(predsednik, str_replace_all, pattern="\\(.+?\\)", replacement="")) %>%
  mutate(volitve = volitve %>% 
           str_replace("1788–1789", "1788") %>% 
           str_replace("18001804", "1800 1804")) %>% 
  separate_rows(volitve, sep=" ") %>%   # vsako leto volitev nova vrstica
  mutate(volitve = parse_number(volitve, na= c("–", "–"))) %>% 
  mutate(stranka = case_when(
    str_detect(stranka, "Democratic-RepublicanNational Republican") ~ "Democratic-Republican_National Republican",
    str_detect(stranka, "WhigUnaffiliated") ~ "Whig_Unaffiliated",
    str_detect(stranka, "Republican National Union") ~ "Republican_National Union",
    str_detect(stranka, "National UnionDemocratic") ~ "National Union_Democratic",
    TRUE ~ stranka
  )) %>% 
  separate_rows(stranka, sep="_")


# stranke = unique(tabela_predsedniki$stranka)[c(-4, -7, -8, -9)] %>% 
#   append(c("National Republican", "National Union")) 
# tabela_stranke = lapply(lapply(tabela_predsedniki$stranka, str_equal, stranke), function(x) stranke[x]) %>% 
#   unlist() %>%
#   as.tibble()
# colnames(tabela_stranke) = c("stranka")
# # dobimo seznam vektorjev, ki imajo natanko en TRUE, s funkcijo jih poberemo ven
# 
# tabela_predsedniki = tabela_predsedniki %>% 
#   left_join(tabela_stranke, by="stranka")
         