library(tidyverse)
library(rvest)
library(readxl)

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

tabela_predsedniki_leta = stran_predsedniki_leta %>%
  html_nodes(xpath = "//table[@class='wikitable sortable jquery-tablesorter']") %>%
  .[[1]] %>%
  html_table()
tabela_predsedniki_rojstva = stran_predsedniki_rojstva %>%
  html_nodes(xpath = "//table[@class='wikitable sortable jquery-tablesorter']") %>%
  .[[1]] %>%
  html_table()
tabela_podpredsedniki_starost = stran_podpredsedniki_starost %>%
  html_nodes(xpath = "//table[@class='sortable wikitable jquery-tablesorter']") %>%
  .[[1]] %>%
  html_table()

d = dir()
populacija_mest = str_subset(d, "Data.csv")
mesta_populacije_vsa_leta = lapply(populacija_mest, read_csv)

populacija_drzave = str_subset(d, "states.+.xlsx")
populacija_drzave_vsa_leta = lapply(populacija_drzave, read_excel)



predsedniki = stran_predsedniki %>%
  html_nodes(xpath = "//table[@class='wikitable sortable sticky-header jquery-tablesorter']") %>%
  .[[1]] %>%
  html_table() %>%
  select(c(-2, -5, -8)) %>%
  rename_with( ~ str_replace_all(., "\\[.+?\\]|\\(.+?\\)", "")) %>% # z anonimno funkcijo preimenujemo imena stolpcev
  lapply(str_replace_all, pattern = "\\[.+?\\]", replacement = "") %>%  # odstranimo opombe
  as_tibble() %>%
  rename(
    zap_stevilka = No.,
    predsednik = Name,
    mandat = Term,
    stranka = Party,
    leto_volitev = Election
  ) %>%
  separate(mandat,
           into = c("zacetek_mandata", "konec_mandata"),
           sep = "–") %>%
  mutate(zacetek_mandata = as.Date(strptime(zacetek_mandata, "%B %d, %Y"))) %>%
  mutate(konec_mandata = as.Date(strptime(konec_mandata, "%B %d, %Y"))) %>%
  mutate(
    predsednik = lapply(
      predsednik,
      str_replace_all,
      pattern = "\\(.+?\\)",
      replacement = ""
    ) %>% unlist()
  ) %>%
  mutate(
    leto_volitev = leto_volitev %>%
      str_replace("1788–1789", "1788") %>%
      str_replace("18001804", "1800 1804")
  ) %>%
  separate_rows(leto_volitev, sep = " ") %>%   # vsako leto volitev nova vrstica
  mutate(leto_volitev = leto_volitev %>% parse_number(na = c("–", "–"))) %>%
  mutate(leto_volitev = leto_volitev %>% as.integer) %>%
  mutate(zap_stevilka = zap_stevilka %>% parse_number) %>%
  mutate(
    stranka = case_when(
      str_detect(stranka, "Democratic-RepublicanNational Republican") ~ "Democratic-Republican_National Republican",
      str_detect(stranka, "WhigUnaffiliated") ~ "Whig_Unaffiliated",
      str_detect(stranka, "Republican National Union") ~ "Republican_National Union",
      str_detect(stranka, "National UnionDemocratic") ~ "National Union_Democratic",
      TRUE ~ stranka
    )
  ) %>%
  separate_rows(stranka, sep = "_") %>%
  left_join(tabela_predsedniki_rojstva, by = c("predsednik" = "President")) %>%
  rename(
    datum_rojstva = "Date of birth",
    kraj_rojstva = Birthplace,
    zvezna_drzava_rojstva = "State† of birth"
  ) %>%
  select(-"In office") %>%
  mutate(datum_rojstva = as.Date(strptime(datum_rojstva, "%B %d, %Y"))) %>%
  mutate(zvezna_drzava_rojstva = zvezna_drzava_rojstva %>% str_replace_all("†", "")) %>%
  left_join(
    tabela_predsedniki_leta %>%
      mutate(President = President %>% str_replace_all("\\[.+?\\]", "")),
    by = c("predsednik" = "President")
  ) %>% # popravimo predsednike, da se ujamejo
  rename(datum_smrti = "Date[d]", kraj_smrti = Place) %>%
  mutate(datum_smrti = datum_smrti %>% str_replace_all("\\(|\\)|\\[.+?\\]", "")) %>%
  mutate(datum_smrti = as.Date(strptime(datum_smrti, "%B %d %Y"))) %>%
  select(-Age, -Cause, -Order, -contains("Presidency")) %>%  # Presidency (order) dates ni zaznavalo
  separate(kraj_smrti,
           into = c("kraj_smrti", "zvezna_drzava_smrti"),
           sep = ", ") %>%
  select(
    c(
      "zap_stevilka",
      "predsednik",
      "datum_rojstva",
      "kraj_rojstva",
      "zvezna_drzava_rojstva",
      "datum_smrti",
      "kraj_smrti",
      "zvezna_drzava_smrti",
      "stranka",
      "leto_volitev",
      "zacetek_mandata",
      "konec_mandata"
    )
  ) %>% # da imamo enak vrstni red stolpcev
  arrange(zap_stevilka)



podpredsedniki = stran_podpredsedniki %>%
  html_nodes(xpath = "//table[@class = 'wikitable sortable sticky-header jquery-tablesorter']") %>%
  .[[1]] %>%
  html_table() %>%
  select(c(-2, -5)) %>%
  rename_with( ~ str_replace_all(., "\\[.+?\\]|\\(.+?\\)", "")) %>%
  lapply(str_replace_all, pattern = "\\[.+?\\]", replacement = "") %>%  # odstranimo opombe
  as_tibble() %>%
  rename(
    zap_stevilka = No.,
    podpredsednik = Name,
    mandat = Term,
    stranka = Party,
    leto_volitev = Election,
    predsednik = President
  ) %>%
  mutate(across(c(-mandat, -predsednik), ~ ifelse(zap_stevilka == "—", NA, .))) %>%  # za Office vacant damo povsod NA
  mutate(
    podpredsednik = podpredsednik %>%
      str_replace_all("\\(.+?\\)", "") %>%
      str_replace("Richard Mentor Johnson", "Richard M. Johnson")
  ) %>%
  separate(mandat,
           into = c("zacetek_mandata", "konec_mandata"),
           sep = "–| – ") %>%
  mutate(
    zacetek_mandata = zacetek_mandata %>%
      str_replace_all("Office vacant ", "") %>%
      str_replace("^October 10$", "October 10, 1973") %>%
      str_replace("^August 9$", "August 9, 1974")
  ) %>% # popravimo obliko datumov, ki nimajo letnic
  mutate(zacetek_mandata = as.Date(strptime(zacetek_mandata, "%B %d, %Y"))) %>%
  mutate(konec_mandata = as.Date(strptime(konec_mandata, "%B %d, %Y"))) %>%
  mutate(
    konec_mandata = if_else(
      !is.na(podpredsednik) & podpredsednik == "Chester A. Arthur",
      as.Date("1881-09-19"),
      konec_mandata
    ) # ročno popravimo edini datum, ki ga ne zazna
  ) %>%
  mutate(leto_volitev = leto_volitev %>%
           str_replace("1788–89", "1788")) %>%
  separate_rows(leto_volitev, sep = "\\s+") %>% # " " ni deloval
  mutate(leto_volitev = leto_volitev %>% parse_number(na = c("–", "–"))) %>%
  mutate(leto_votitev = leto_volitev %>% as.integer) %>%
  mutate(zap_stevilka = zap_stevilka %>% parse_number) %>%
  mutate(podpredsednik = str_trim(podpredsednik)) %>% # nekateri predsedniki imajo presledek po imenu
  left_join(tabela_podpredsedniki_starost[-c(1, 4, 5, 6, 8)],
            by = c("podpredsednik" = "Vice president")) %>%
  rename(datum_rojstva = Born, datum_smrti = Lifespan) %>%
  mutate(datum_smrti = datum_smrti %>%
           str_replace_all("July 5, 1826Jul 4, 1826", "Jul 4, 1826")) %>%
  mutate(datum_rojstva = as.Date(strptime(datum_rojstva, "%B %d, %Y"))) %>%
  mutate(datum_smrti = as.Date(strptime(datum_smrti, "%B %d, %Y"))) %>%
  select(
    c(
      "zap_stevilka",
      "podpredsednik",
      "datum_rojstva",
      "datum_smrti",
      "stranka",
      "leto_volitev",
      "zacetek_mandata",
      "konec_mandata",
      "predsednik"
    )
  ) %>%
  arrange(zap_stevilka)


glavna_mesta = stran_glavna_mesta %>%
  html_nodes(xpath = "//table[@class='wikitable plainrowheaders sortable jquery-tablesorter']") %>%
  .[[1]] %>%
  html_table() %>%
  select(c(1:4, 8)) %>%
  slice(2:(n() - 1)) %>%
  rename(
    drzava = State,
    glavno_mesto = Capital,
    leto_razglasitve = Since,
    povrsina_km2 = Area,
    rang_v_drzavi = "City rank in state"
  ) %>%
  mutate(povrsina_km2 = povrsina_km2 %>%
           str_extract("\\(.+?\\)") %>%
           parse_number) %>%
  mutate(glavno_mesto = glavno_mesto %>% str_trim()) %>%
  mutate(mesto_drzava = paste(glavno_mesto, drzava, sep = "_"))

# ni šlo v eni cevi, ker se je za mesta_list potrebno sklicati na glavna_mesta

mesta_list = lapply(mesta_populacije_vsa_leta, function(df) {
  df %>%
    separate(NAME, into = c("mesto", "drzava"), sep = " ?, ?") %>%
    rename(populacija = B01003_001E) %>%
    select(mesto, drzava, populacija) %>%
    slice(-1) %>%
    mutate(
      mesto = mesto %>% str_replace_all(" city| CDP| town| ?\\(.+?\\)", "") %>%
        str_replace("Juneau and borough", "Juneau") %>%
        str_replace("Urban Honolulu", "Honolulu") %>%
        str_replace("Boise City", "Boise") %>%
        str_replace("St. Paul", "Saint Paul") %>%
        str_replace("Nashville-Davidson metropolitan government", "Nashville")
    ) %>%
    mutate(mesto_drzava = paste(mesto, drzava, sep = "_")) %>%
    filter(mesto_drzava %in% glavna_mesta$mesto_drzava)
})

glavna_mesta = glavna_mesta %>%
  left_join(
    bind_cols(mesto_drzava = mesta_list[[1]]$mesto_drzava, # nepotrebno, da shranjujemo od vseh, saj so enake
              lapply(mesta_list, function(df) {
                df$populacija
              }))
    ,
    by = c("mesto_drzava" = "mesto_drzava")
  ) %>%
  rename_with(~ as.character(as.numeric(str_replace_all(., "\\.\\.\\.", "")) + 2008), # odstranimo pike in seštejemo, da dobimo letnico
              starts_with("...")) %>%
  pivot_longer(-c(1:6), names_to = "leto", values_to = "populacija") %>%
  mutate(populacija = populacija %>% parse_number) %>%
  mutate(leto = leto %>% parse_number) %>%
  mutate(leto_razglasitve = leto_razglasitve %>% parse_number) %>%
  mutate(rang_v_drzavi = rang_v_drzavi %>% parse_number) %>%
  select(-mesto_drzava) %>%
  select(
    c(
      "glavno_mesto",
      "drzava",
      "leto",
      "populacija",
      "leto_razglasitve",
      "povrsina_km2",
      "rang_v_drzavi"
    )
  ) %>%
  arrange(leto)


drzave_populacija = left_join(
  populacija_drzave_vsa_leta[[1]],
  populacija_drzave_vsa_leta[[2]],
  by = "table with row headers in column A and column headers in rows 3 through 4. (leading dots indicate sub-parts)",
  relationship = "many-to-many"
) %>%
  slice(10:60) %>%
  select(-c(3, 4, 15)) %>%
  mutate(`...2.x` = `...2.x` %>% parse_number,
         `...2.y` = `...2.y` %>% parse_number) %>%
  rename_with(
    ~ case_when(
      endsWith(., "2.y") ~ "...14",
      # moramo ročno popraviti, sicer se ne izidejo letnice
      endsWith(., "y") ~ paste("...", as.character(as.numeric(
        str_extract(., "[0-9]+")
      ) + 11), sep = ""),
      endsWith(., "2.x") ~ "...4",
      endsWith(., "x") ~ str_replace(., "\\.x", ""),
      TRUE ~ .
    )
  ) %>%
  rename_with(~ ifelse(
    startsWith(., "..."),
    as.character(as.numeric(str_replace_all(., "\\.\\.\\.", "")) + 2006),
    "drzava"
  )) %>% # enak trik kot prej
  pivot_longer(-1, names_to = "leto", values_to = "populacija") %>%
  mutate(drzava = drzava %>% str_replace("\\.", ""),
         leto = leto %>% parse_number)



# tabela_predsedniki = predsedniki
# tabela_podpredsedniki = podpredsedniki
# tabela_glavna_mesta = glavna_mesta
# tabela_drzave_populacija = drzave_populacija
# 
# load(file = "tabele.RData")
# test1 = all_equal(tabela_predsedniki, predsedniki, na_equal = TRUE)
# test2 = all_equal(tabela_podpredsedniki, podpredsedniki, na_equal = TRUE)
# test3 = all_equal(tabela_drzave_populacija, drzave_populacija, na_equal = TRUE)
# test4 = all_equal(tabela_glavna_mesta, glavna_mesta, na_equal = TRUE)
# all(test1, test2, test3, test4)
