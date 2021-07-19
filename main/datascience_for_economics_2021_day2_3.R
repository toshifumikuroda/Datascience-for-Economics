# setup -------------------------------------------------------------------


## clear memory -----------------------------------------------------------
rm(list=ls())


## load library -----------------------------------------------------------
library(tidyverse)


## load data --------------------------------------------------------------
giving <- foreign::read.dta(file = "./data/AERtables1-5.dta")


# table 1 -----------------------------------------------------------------
## all --------------------------------------------------------------------
Table_1_all <- giving %>%
  dplyr::select(MRM2, HPA, freq, years, dormant, female, couple) %>%
  gtsummary::tbl_summary(
    type = list(everything() ~ "continuous"),
    statistic = list(gtsummary::all_continuous() ~ "{mean} ({sd})"),
    digits = list(gtsummary::all_continuous() ~ c(3,3)),
    missing = "no",
    label = list(
      MRM2 ~ "最後の寄付からの期間",
      HPA ~ "過去の最高寄附額",
      freq ~ "過去の寄付回数",
      years ~ "初めて寄付してからの年数",
      dormant ~ "2005年に既に寄付をしていたか",
      female ~ "女性ダミー",
      couple ~ "既婚ダミー"
      )
  )

Table_1_all %>%
  gtsummary::as_flex_table() %>%
  flextable::save_as_docx(
    path = "figuretable/demand_table1.docx"
  )

Table_1_all %>%
  gtsummary::as_gt() %>%
  gt::gtsave(
    file = "figuretable/table1.png"
  )

## Table_1_ttest --------------------------------------------------------------
Table_1_ttest <- giving %>%
  dplyr::select(MRM2, HPA, freq, years, dormant, female, couple, treatment) %>%
  gtsummary::tbl_summary(
    by = treatment,
    type = list(everything() ~ "continuous"),
    statistic = list(gtsummary::all_continuous() ~ "{mean} ({sd})"),
    digits = list(gtsummary::all_continuous() ~ c(3,3)),
    missing = "no",
    label = list(
      MRM2 ~ "最後の寄付からの期間",
      HPA ~ "過去の最高寄附額",
      freq ~ "過去の寄付回数",
      years ~ "初めて寄付してからの年数",
      dormant ~ "2005年に既に寄付をしていたか",
      female ~ "女性ダミー",
      couple ~ "既婚ダミー")
    ) %>% 
  gtsummary::add_overall() %>%
  gtsummary::add_p(test = everything() ~ "t.test")

Table_1_ttest %>%
  gtsummary::as_gt() %>%
  gt::gtsave(
    file = "figuretable/demand_table_1_ttest.png"
  )

# Table 2 -----------------------------------------------------------------
## Treatment and Control --------------------------------------------------
Table_2_treatment_control <- giving %>%
  dplyr::select(gave, amount, treatment) %>%
  dplyr::mutate(doller_cond_giving = ifelse(gave==1, amount, NA)) %>%
  gtsummary::tbl_summary(
    by = treatment,
    type = list(everything() ~ "continuous"),
    statistic = list(gtsummary::all_continuous() ~ "{mean} ({sd})"),
    digits = list(gtsummary::all_continuous() ~ c(3,3)),
    missing = "no",
    label = list(
      gave ~ "寄付をした率",
      amount ~ "寄附額",
      doller_cond_giving ~ "寄付をした人の平均額"
    )
  ) %>%
  gtsummary::add_p(test = everything() ~ "t.test")

## ratio -------------------------------------------------------------------
Table_2_ratio <- giving %>%
  dplyr::filter(treatment == 1) %>%
  dplyr::select(ratio, gave, amount) %>%
  dplyr::mutate(doller_cond_giving = ifelse(gave==1, amount, NA)) %>%
  gtsummary::tbl_summary(
    by = ratio,
    type = list(everything() ~ "continuous"),
    statistic = list(gtsummary::all_continuous() ~ "{mean} ({sd})"),
    digits = list(gtsummary::all_continuous() ~ c(3,3)),
    missing = "no",
    label = list(
      gave ~ "寄付をした率",
      amount ~ "寄附額",
      doller_cond_giving ~ "寄付をした人の平均額"
    )
  )


## merge table ------------------------------------------------------------
Table_2 <-
  gtsummary::tbl_merge(
    tbls = list(Table_2_treatment_control, Table_2_ratio),
    tab_spanner = c("", "**マッチ比率**")
  )


ttest_2 <- giving %>%
  dplyr::select(gave, amount, treatment) %>%
  dplyr::mutate(doller_cond_giving = ifelse(gave==1, amount, NA)) %>%
  dplyr::summarise_each(funs(t.test(.[treatment == 1], .[treatment == 0])$statistic), vars = gave, amount,doller_cond_giving) %>%
  tidyr::gather(key, value) %>%
  dplyr::mutate("T statistics" = value) %>%
  dplyr::select("T statistics")
ttest_2


# Table3 ------------------------------------------------------------------
giving %>%
  lm(formula = amount ~ treatment)

Table3_1 <- giving %>%
  lm(formula = amount ~ treatment)
summary(Table3_1)

Table3_2 <- giving %>%
  lm(formula = amount ~ 
       treatment + treatment : ratio2 + treatment : ratio3 +
       treatment : size25 + treatment : size50 + treatment : size50 + treatment : size100 +
       treatment : askd2 + treatment : askd3)
summary(Table3_2)

Table3_3 <- giving %>%
  dplyr::filter(dormant == 1) %>%
  lm(formula = amount ~ treatment)
summary(Table3_3)

Table3_4 <- giving %>%
  dplyr::filter(dormant == 1) %>%
  lm(formula = amount ~ 
       treatment + treatment : ratio2 + treatment : ratio3 +
       treatment : size25 + treatment : size50 + treatment : size50 + treatment : size100 +
       treatment : askd2 + treatment : askd3)
summary(Table3_4)

Table3_5 <- giving %>%
  dplyr::filter(dormant == 0) %>%
  lm(formula = amount ~ treatment)
summary(Table3_5)

Table3_6 <- giving %>%
  dplyr::filter(dormant == 0) %>%
  lm(formula = amount ~ 
       treatment + treatment : ratio2 + treatment : ratio3 +
       treatment : size25 + treatment : size50 + treatment : size50 + treatment : size100 +
       treatment : askd2 + treatment : askd3)
summary(Table3_6)

Table3_7 <- giving %>% 
  lm(formula = amountchange ~ treatment)
summary(Table3_7)

Table3_8 <- giving %>%
  lm(formula = amountchange ~ 
       treatment + treatment : ratio2 + treatment : ratio3 +
       treatment : size25 + treatment : size50 + treatment : size50 + treatment : size100 +
       treatment : askd2 + treatment : askd3)
summary(Table3_8)

Table3 <- huxtable::huxreg(Table3_1, Table3_2, Table3_3, Table3_4, Table3_5, Table3_6, Table3_7, Table3_8,
       statistics = c(N = "nobs", R2 = "r.squared"))

Table3 %>%
  huxtable::quick_xlsx(file = "figuretable/demand_table3.xlsx")



library(jtools)
export_summs(Table3_1, Table3_2, scale = TRUE)

# 付加価値
## 価格の作成
linear_1 <- giving %>% 
  dplyr::mutate(price = 1/(1+ratio)) %>%
  lm(formula = amountchange ~ price)
summary(linear_1)

## 両側対数モデル
log_1 <- giving %>% 
  dplyr::mutate(price = 1/(1+ratio)) %>%
  dplyr::mutate(logprice = log(price)) %>%
  dplyr::mutate(logamount = log(1+amount)) %>%
  lm(formula = logamount ~ logprice)
summary(log_1)

## プロット
giving %>%
  dplyr::mutate(price = 1/(1+ratio)) %>%
  ggplot2::ggplot(aes(x = amount, y = price)) +
  geom_point(color = "black", fill = "lightgray") + 
  stat_smooth(colour = "red", size = 1) + 
  theme_classic()

## プロット2
giving %>%
  dplyr::mutate(price = 1/(1+ratio)) %>%
  dplyr::mutate(logprice = log(price)) %>%
  dplyr::mutate(logamount = log(1+amount)) %>%
  ggplot2::ggplot(aes(x = logamount, y = logprice)) +
  geom_point(color = "black", fill = "lightgray") + 
  stat_smooth(colour = "red", size = 1) + 
  theme_classic()


test <- giving %>%
  dplyr::mutate(price = 1/(1+ratio)) %>%
  lm(formula = amount ~ price + price * red0 + price * redcty + price * nonlit + 
       price * cases + price * MRM2 + price * HPA + price * freq + 
       price * years + price * dormant + price * female + price * couple)
summary(test)

giving_edited <- giving %>%
  dplyr::mutate(price = 1/(1+ratio)) %>%
  tidyr::drop_na() %>%
  dplyr::group_by(., red0, redcty, nonlit, cases) %>%
  dplyr::mutate(region = cur_group_id())
summary(giving_edited$region)

fixed_effect <- giving_edited %>%
  lm_robust(formula = amount ~ price,
            clusters = region)
summary(fixed_effect)   

giving_edited_aggregated <- giving_edited %>%
  dplyr::group_by(., red0, redcty, nonlit, cases) %>%
  dplyr::summarise(aggregate_amount = sum(amount), aggregate_price = mean(price)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(region = row_number())

aggregate <- giving_edited_aggregated %>%
  lm(formula = aggregate_amount ~ aggregate_price)
summary(aggregate)   


fatalities_mod_hc <- lm_robust(fatal_rate ~ beertax + state + year, data = Fatalities)
