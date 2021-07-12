
# set up ------------------------------------------------------------------
rm(list = ls())

## load libraries ---------------------------------------------------------
library(tidyverse)


# covid data --------------------------------------------------------------
download.file("https://dl.dropboxusercontent.com/s/6mztoeb6xf78g5w/COVID-19.csv", "./data/COVID-19.csv")
covid_19 <- read.csv("./data/COVID-19.csv")
infected <- as.Date(covid_19$確定日, "%m/%d/%y") 
hist(infected, breaks="days", freq=TRUE)


# visualize covid-19 ------------------------------------------------------
## histogram --------------------------------------------------------------
covid_19 %>%
  dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) %>%
  ggplot2::ggplot(aes(x=days)) + 
  geom_histogram(binwidth = 1,color = "black", fill = "lightgray") +
  theme_classic()

## histgram of female -----------------------------------------------------
covid_19 %>%  dplyr::filter(性別=="女性") %>% 
  dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) %>% 
  ggplot2::ggplot(aes(x=days)) +   geom_histogram(binwidth = 1, color="black", fill="lightgray") +  theme_classic()


## histgram by sex --------------------------------------------------------
covid_19 %>%
  dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) %>%
  ggplot2::ggplot(aes(x=days, fill = 性別)) + 
  geom_histogram(binwidth = 1) +
  theme_classic()


## fix sex ----------------------------------------------------------------
covid_19 %>% dplyr::group_by(性別) %>%  summarize()

covid_19 <- 
covid_19 %>%  
  dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) %>% 
  dplyr::mutate(
    sex = dplyr::case_when(
      stringr::str_detect(性別, '男') ~ "♂",
      stringr::str_detect(性別, '女') ~ "♀",
      TRUE ~ "Unknown"
      )
    )


## scatter plot -----------------------------------------------------------
covid_19 %>%
  dplyr::mutate(week = format(days, "%W")) %>%
  dplyr::group_by(年代,week) %>%
  dplyr::mutate(case = n()) %>%
  ggplot2::ggplot(aes(x = week, y = 年代, size = case)) + 
  geom_point() +
  theme_classic()


## fix ages ---------------------------------------------------------------
agelist <- covid_19 %>% dplyr::group_by(年代) %>%  summarize()

covid_19 <- 
  covid_19 %>%  
  dplyr::mutate(
    ages = dplyr::case_when(
      stringr::str_detect(年代, '10') ~ "000 - 010",
      stringr::str_detect(年代, '20') ~ "010 - 020",
      stringr::str_detect(年代, '30') ~ "020 - 030",
      stringr::str_detect(年代, '40') ~ "030 - 040",
      stringr::str_detect(年代, '50') ~ "040 - 050",
      stringr::str_detect(年代, '60') ~ "050 - 060",
      stringr::str_detect(年代, '70') ~ "060 - 070",
      stringr::str_detect(年代, '80') ~ "070 - 080",
      stringr::str_detect(年代, '90') ~ "080 - 090",
      TRUE ~ "Unknown"
    ),
    ages = ifelse(str_detect(年代, '100'), "100 - ", ages),
    week = format(days, "%W")
  )
## scatter plot -----------------------------------------------------------
covid_19 %>%
  dplyr::group_by(ages, week) %>%
  dplyr::mutate(case = n()) %>%
  ggplot2::ggplot(aes(x = week, y = ages, size = case)) + 
  geom_point() +
  theme_classic()


## SNS appealing figure ---------------------------------------------------
covid_19 %>% 
  dplyr::group_by(ages,week) %>% 
  dplyr::mutate(case = n()) %>% 
  ggplot2::ggplot(aes(x=week, y=ages, size = case, color = case)) +
  geom_point() +
  theme_classic() +
  scale_colour_gradient(low = "green", high = "red", na.value = NA)



## SNS appealing figure by prefecture -------------------------------------
covid_19 %>% 
  dplyr::rename(pref = "Residential.Pref") %>%
  dplyr::group_by(ages,week) %>% 
  dplyr::mutate(case = n()) %>% 
  ggplot2::ggplot(aes(x=week, y=ages, size = case, color = case)) +
  geom_point() +
  theme_classic() +
  scale_colour_gradient(low = "green", high = "red", na.value = NA) +
  facet_wrap(~ pref, nrow = 5)


# table -------------------------------------------------------------------

## basic table ------------------------------------------------------------
covid_19 %>%
  tidyr::drop_na(days) %>%
  dplyr::mutate(month = format(days, "%m")) %>%
  dplyr::mutate(ages = as.numeric(年代)) %>%
  dplyr::group_by(month) %>%
  dplyr::summarize(mean = mean(ages),
                   median = median(ages, na.rm = TRUE))


## table ranking ----------------------------------------------------------
covid_19 %>%
  dplyr::rename(pref = "Residential.Pref") %>%
  dplyr::group_by(pref) %>%
  dplyr::summarize(case = n()) %>%
  dplyr::mutate(proportion = case/sum(case)) %>%
  dplyr::arrange(desc(proportion)) %>%
  dplyr::mutate(rank = row_number()) %>%
  dplyr::select(rank, proportion, pref)


## table ranking object ---------------------------------------------------
ranking_table <- 
  covid_19 %>%
  dplyr::rename(pref = "Residential.Pref") %>%
  dplyr::group_by(pref) %>%
  dplyr::summarize(case = n()) %>%
  dplyr::mutate(proportion = case/sum(case)) %>%
  dplyr::arrange(desc(proportion)) %>%
  dplyr::mutate(rank = row_number()) %>%
  dplyr::select(rank, proportion, pref)


## view object ------------------------------------------------------------
View(ranking_table)


## format -----------------------------------------------------------------
ranking_table$proportion <- round(ranking_table$proportion, 3)
View(ranking_table)


## export table -----------------------------------------------------------
readr::write_csv(ranking_table, file = "figuretable/covid_19.csv")

ranking_huxtable <-
  ranking_table %>%
  huxtable::as_hux() %>%
  huxtable::set_outer_padding(0) %>% 
  huxtable::set_bold(row = 1, col = huxtable::everywhere) %>% 
  huxtable::set_bottom_border(row = 1, col = huxtable::everywhere) %>% 
  huxtable::set_caption("国内新型コロナウイルス感染者数")

ranking_huxtable %>%
  huxtable::quick_docx(file = "figuretable/covid_19.docx")
ranking_huxtable %>%
  huxtable::quick_xlsx(file = "figuretable/covid_19.xlsx")
