
# set up ------------------------------------------------------------------
rm(list = ls())

## load libraries ---------------------------------------------------------
library(magrittr)
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
  ggplot2::ggplot(ggplot2::aes(x=days)) + 
  ggplot2::geom_histogram(binwidth = 1, color = "black", fill = "lightgray") +
  ggplot2::theme_classic()

## histgram of female -----------------------------------------------------
covid_19 %>% dplyr::filter(性別=="女性") %>% 
  dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) %>% 
  ggplot2::ggplot(ggplot2::aes(x=days)) + 
  ggplot2::geom_histogram(binwidth = 1, color="black", fill="lightgray") + 
  ggplot2::theme_classic()

## histgram by sex --------------------------------------------------------
covid_19 %>%
  dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) %>%
  ggplot2::ggplot(ggplot2::aes(x=days, fill = 性別)) + 
  ggplot2::geom_histogram(binwidth = 1) +
  ggplot2::theme_classic()


## fix sex ----------------------------------------------------------------
covid_19 %>% 
  dplyr::group_by(性別) %>% 
  dplyr::summarize()

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

covid_19 %>%
  dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) %>%
  ggplot2::ggplot(ggplot2::aes(x=days, fill = sex)) + 
  ggplot2::geom_histogram(binwidth = 1) +
  ggplot2::theme_classic()

## scatter plot -----------------------------------------------------------
covid_19 %>%
  dplyr::mutate(week = format(days, "%W")) %>%
  dplyr::group_by(年代,week) %>%
  dplyr::mutate(case = dplyr::n()) %>%
  ggplot2::ggplot(ggplot2::aes(x = week, y = 年代, size = case)) + 
  ggplot2::geom_point() +
  ggplot2::theme_classic()


## fix ages ---------------------------------------------------------------
agelist <- covid_19 %>% dplyr::group_by(年代) %>%  dplyr::summarize()

covid_19 <- 
  covid_19 %>%  
  dplyr::mutate(
    ages = dplyr::case_when(
      stringr::str_detect(年代, '10') ~ "0 - 10",
      stringr::str_detect(年代, '20') ~ "10 - 20",
      stringr::str_detect(年代, '30') ~ "20 - 30",
      stringr::str_detect(年代, '40') ~ "30 - 40",
      stringr::str_detect(年代, '50') ~ "40 - 50",
      stringr::str_detect(年代, '60') ~ "50 - 60",
      stringr::str_detect(年代, '70') ~ "60 - 70",
      stringr::str_detect(年代, '80') ~ "70 - 80",
      stringr::str_detect(年代, '90') ~ "80 - 90",
      TRUE ~ "Unknown"
    ),
    ages = ifelse(
      stringr::str_detect(年代, '100'),
      "100 - ",
      ages
      ),
    # factor は level を付けることで並び順を変える事ができる。
    ages = factor(
      ages,
      levels = c(
        "0 - 10",
        "10 - 20",
        "20 - 30",
        "30 - 40",
        "40 - 50",
        "50 - 60",
        "60 - 70",
        "70 - 80",
        "80 - 90",
        "100 - ",
        "Unknown"
        )
      ),
    week = format(days, "%W")
    )

## scatter plot -----------------------------------------------------------
covid_19 %>%
  dplyr::group_by(ages, week) %>%
  dplyr::mutate(case = dplyr::n()) %>%
  ggplot2::ggplot(ggplot2::aes(x = week, y = ages, size = case)) + 
  ggplot2::geom_point() +
  ggplot2::theme_classic()

## SNS appealing figure ---------------------------------------------------
covid_19 %>% 
  dplyr::group_by(ages,week) %>% 
  dplyr::mutate(case = dplyr::n()) %>% 
  ggplot2::ggplot(ggplot2::aes(x=week, y=ages, size = case, color = case)) +
  ggplot2::geom_point() +
  ggplot2::theme_classic() +
  ggplot2::scale_colour_gradient(low = "green", high = "red", na.value = NA)

## SNS appealing figure by prefecture -------------------------------------
covid_19 %>% 
  dplyr::rename(pref = "Residential.Pref") %>%
  dplyr::group_by(ages,week) %>% 
  dplyr::mutate(case = dplyr::n()) %>% 
  ggplot2::ggplot(ggplot2::aes(x=week, y=ages, size = case, color = case)) +
  ggplot2::geom_point() +
  ggplot2::theme_classic() +
  ggplot2::scale_colour_gradient(low = "green", high = "red", na.value = NA) +
  ggplot2::facet_wrap(~ pref, nrow = 5)

# table -------------------------------------------------------------------

## basic table ------------------------------------------------------------
covid_19 %>%
  tidyr::drop_na(days) %>%
  dplyr::mutate(month = format(days, "%m")) %>%
  dplyr::mutate(ages = as.numeric(年代)) %>%
  dplyr::group_by(month) %>%
  dplyr::summarize(mean = mean(ages, na.rm = TRUE),
                   median = median(ages, na.rm = TRUE))


## table ranking ----------------------------------------------------------
covid_19 %>%
  dplyr::rename(pref = "Residential.Pref") %>%
  dplyr::group_by(pref) %>%
  dplyr::summarize(case = dplyr::n()) %>%
  dplyr::mutate(proportion = case/sum(case)) %>%
  dplyr::arrange(desc(proportion)) %>%
  dplyr::mutate(rank = dplyr::row_number()) %>%
  dplyr::select(rank, proportion, pref)


## table ranking object ---------------------------------------------------
ranking_table <- 
  covid_19 %>%
  dplyr::rename(pref = "Residential.Pref") %>%
  dplyr::group_by(pref) %>%
  dplyr::summarize(case = dplyr::n()) %>%
  dplyr::mutate(proportion = case/sum(case)) %>%
  dplyr::arrange(desc(proportion)) %>%
  dplyr::mutate(rank = dplyr::row_number()) %>%
  dplyr::select(rank, proportion, pref)


## view object ------------------------------------------------------------
View(ranking_table)


## format -----------------------------------------------------------------
ranking_table$proportion <- round(ranking_table$proportion, 3)
View(ranking_table)


## export table -----------------------------------------------------------
readr::write_csv(ranking_table, file = "figuretable/covid_19.csv")


# export table by kableExtra ----------------------------------------------
ranking_kableExtra <-
  ranking_table %>%
  dplyr::rename(
    "順位" = rank,
    "感染者率" = proportion,
    "都道府県" = pref
  ) %>%
  kableExtra::kbl() %>%
  kableExtra::add_header_above(
    c("国内新型コロナウイルス感染者数" = 3)
  ) %>%
  kableExtra::kable_classic()


## export -----------------------------------------------------------------
ranking_kableExtra %>%
  kableExtra::save_kable(
    file = "figuretable/table_covid_19.png"
  )

ranking_kableExtra %>%
  kableExtra::save_kable(
    file = "figuretable/table_covid_19.html"
  )

webshot::webshot(
  "figuretable/table_covid_19.html",
  "figuretable/table_covid_19.png"
)

