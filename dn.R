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
  html_table() %>% 
  select(c(-2, -5, -8)) %>% 
  rename_with(~ str_replace_all(., "\\[.+?\\]|\\(.+?\\)", "")) %>% # z anonimno funkcijo preimenujemo imena stolpcev
  lapply(str_replace_all, pattern="\\[.+?\\]", replacement="") %>%  # odstranimo opombe
  as_tibble() %>% 
  rename(zap_stevilka = No.,predsednik = Name, mandat = Term, stranka = Party, leto_volitev = Election) %>% 
  separate(mandat, into=c("zacetek_mandata", "konec_mandata"), sep="–") %>% 
  mutate(zacetek_mandata = as.Date(strptime(zacetek_mandata, "%B %d, %Y"))) %>% 
  mutate(konec_mandata = as.Date(strptime(konec_mandata, "%B %d, %Y"))) %>% 
  mutate(predsednik = lapply(predsednik, str_replace_all, 
                             pattern="\\(.+?\\)", 
                             replacement="") %>% unlist()) %>%
  mutate(leto_volitev = leto_volitev %>% 
           str_replace("1788–1789", "1788") %>% 
           str_replace("18001804", "1800 1804")) %>% 
  separate_rows(leto_volitev, sep=" ") %>%   # vsako leto volitev nova vrstica
  mutate(leto_volitev = leto_volitev %>% parse_number(na= c("–", "–"))) %>% 
  mutate(zap_stevilka = zap_stevilka %>% parse_number) %>% 
  mutate(stranka = case_when(
    str_detect(stranka, "Democratic-RepublicanNational Republican") ~ "Democratic-Republican_National Republican",
    str_detect(stranka, "WhigUnaffiliated") ~ "Whig_Unaffiliated",
    str_detect(stranka, "Republican National Union") ~ "Republican_National Union",
    str_detect(stranka, "National UnionDemocratic") ~ "National Union_Democratic",
    TRUE ~ stranka
  )) %>% 
  separate_rows(stranka, sep="_") %>% 
  left_join(tabela_predsedniki_rojstva, by=c("predsednik"="President")) %>% 
  rename(datum_rojstva = "Date of birth", kraj_rojstva = Birthplace, 
         zvezna_drzava_rojstva = "State† of birth") %>% 
  select(-"In office") %>% 
  mutate(datum_rojstva = as.Date(strptime(datum_rojstva, "%B %d, %Y"))) %>% 
  mutate(zvezna_drzava_rojstva = zvezna_drzava_rojstva %>% str_replace_all("†", "")) %>% 
  left_join(tabela_predsedniki_leta %>% 
    mutate(
      President = President %>% str_replace_all("\\[.+?\\]", "")
    ),
    by=c("predsednik"="President")
    ) %>% # popravimo predsednike, da se ujamejo
  rename(datum_smrti = "Date[d]", kraj_smrti = Place) %>% 
  mutate(datum_smrti = datum_smrti %>% str_replace_all("\\(|\\)|\\[.+?\\]", "")) %>% 
  mutate(datum_smrti = as.Date(strptime(datum_smrti, "%B %d %Y"))) %>% 
  select(-Age, -Cause, -Order, -contains("Presidency")) %>%  # Presidency (order) dates ni zaznavalo
  separate(kraj_smrti, into=c("kraj_smrti", "zvezna_drzava_smrti"), sep=", ") %>% 
  select(colnames(predsedniki)) %>% # da imamo enak vrstni red stolpcev
  arrange(zap_stevilka)




         