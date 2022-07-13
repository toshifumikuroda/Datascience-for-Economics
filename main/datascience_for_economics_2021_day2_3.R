# Reproduct Karlan and List(2007) to understand demand estimation  --------
# setup -------------------------------------------------------------------


## clear memory -----------------------------------------------------------
rm(list=ls())


## load library -----------------------------------------------------------
library(tidyverse)


## load data --------------------------------------------------------------
giving <- foreign::read.dta(file = "./data/06_ols/AERtables1-5.dta")

# summary tools -----------------------------------------------------------
# psych
giving %>%
  dplyr::select(MRM2, HPA, freq, years, dormant, female, couple) %>%
  psych::describe()

# Hmisc
giving %>%
  dplyr::select(MRM2, HPA, freq, years, dormant, female, couple) %>%
  Hmisc::describe()

# gtsummary
giving %>%
  dplyr::select(MRM2, HPA, freq, years, dormant, female, couple) %>%
  gtsummary::tbl_summary()

# modelsummary
giving %>%
  dplyr::select(MRM2, HPA, freq, years, dormant, female, couple) %>%
  modelsummary::datasummary_skim()

# skimr
giving %>%
  dplyr::select(MRM2, HPA, freq, years, dormant, female, couple) %>%
  skimr::skim()

# table 1 -----------------------------------------------------------------
# summarise one variables
giving %>%
  dplyr::summarise(
    MRM2_mean = mean(MRM2, na.rm = TRUE)
    )

# summarise two variables
giving %>%
  dplyr::summarise(
    MRM2_mean = mean(MRM2, na.rm = TRUE),
    HPA_mean = mean(HPA, na.rm = TRUE)
  )

# summarise two statistics
giving %>%
  dplyr::summarise(
    MRM2_mean = mean(MRM2, na.rm = TRUE),
    MRM2_sd = sd(MRM2, na.rm = TRUE),
    HPA_mean = mean(HPA, na.rm = TRUE),
    HPA_sd = sd(MRM2, na.rm = TRUE)
  )

## all --------------------------------------------------------------------
# set variable list
controls <- c("MRM2", "HPA", "freq", "years", "dormant", "female", "couple")

### summarise mean --------------------------------------------------------
Table_1_all_mean <- giving %>%
  dplyr::summarise(
    dplyr::across(
      controls,
      mean, na.rm = TRUE
      )
  )


### summarise sd ----------------------------------------------------------
Table_1_all_sd <- giving %>%
  dplyr::summarise(
    dplyr::across(
      controls,
      sd, na.rm = TRUE
    )
  )


### bind two tables -------------------------------------------------------
Table_1_all <-
  rbind(
    Table_1_all_mean,
    Table_1_all_sd
  ) %>%
  # rename
  dplyr::rename(
    "最後の寄付からの期間" = MRM2,
    "過去の最高寄附額" = HPA,
    "過去の寄付回数" = freq,
    "初めて寄付してからの年数" = years,
    "2005年に既に寄付をしていたか" = dormant,
    "女性ダミー" = female,
    "既婚ダミー" = couple
  ) %>%
  t()

colnames(Table_1_all) <- c("Mean", "SD")
Table_1_all <- Table_1_all %>%
  as.data.frame() %>%
  dplyr::mutate(across(everything(), num, digits = 3))

## group_by(treatment)
### summarise mean --------------------------------------------------------
Table_1_group_mean <- giving %>%
  dplyr::group_by(treatment) %>%
  dplyr::summarise(
    dplyr::across(
      controls,
      mean, na.rm = TRUE
    )
  )


### summarise sd ----------------------------------------------------------
Table_1_group_sd <- giving %>%
  dplyr::group_by(treatment) %>%
  dplyr::summarise(
    dplyr::across(
      controls,
      sd, na.rm = TRUE
    )
  )


### bind two tables -------------------------------------------------------
Table_1_group <-
  rbind(
    Table_1_group_mean,
    Table_1_group_sd
  ) %>%
  # rename
  dplyr::rename(
    "最後の寄付からの期間" = MRM2,
    "過去の最高寄附額" = HPA,
    "過去の寄付回数" = freq,
    "初めて寄付してからの年数" = years,
    "2005年に既に寄付をしていたか" = dormant,
    "女性ダミー" = female,
    "既婚ダミー" = couple
  ) %>%
  dplyr::select(
    -treatment
  ) %>%
  t()

Table_1_group <- Table_1_group[, c(2,5,1,4)] %>%
  as.data.frame() %>%
  dplyr::mutate(across(everything(), num, digits = 3))
colnames(Table_1_group) <- c("Mean", "SD", "Mean", "SD")


## bind all and group -----------------------------------------------------
Table_1 <-
  cbind(
    Table_1_all,
    Table_1_group
  )


### kable_styling ---------------------------------------------------------
Table_1 %>%
  kableExtra::kbl() %>%
  kableExtra::kable_styling()
Table_1

Table_1 %>%
  kableExtra::kbl() %>%
  kableExtra::add_header_above(
    c(" " = 1, "All" = 2, "Treatment" = 2, "Control" = 2)
    ) %>%
  kableExtra::kable_classic() %>%
  kableExtra::save_kable(
    "figuretable/demand_table1.png"
  )


# Table 2 -----------------------------------------------------------------
## Treatment and Control --------------------------------------------------
### mean ------------------------------------------------------------------
Table_2_mean_treatment_control <- giving %>%
  dplyr::group_by(treatment) %>%
  dplyr::mutate(
    doller_cond_giving = ifelse(gave==1, amount, NA),
    doller_per_letter_wt_match = amount,
    doller_per_letter_wo_match = (1 + ratio) * amount
  ) %>%
  dplyr::summarise(
    dplyr::across(
      c("gave", "amount", "doller_cond_giving", "doller_per_letter_wt_match", "doller_per_letter_wo_match"),
      mean, na.rm = TRUE
    )
  ) %>%
  dplyr::select(-treatment)

### se --------------------------------------------------------------------
se <- function(x) {
  x <- na.omit(x)
  sqrt(var(x) / length(x))
}
  
Table_2_se_treatment_control <- 
  giving %>%
  dplyr::group_by(treatment) %>%
  dplyr::mutate(
    doller_cond_giving = ifelse(gave==1, amount, NA),
    doller_per_letter_wt_match = amount,
    doller_per_letter_wo_match = (1 + ratio) * amount
  ) %>%
  dplyr::summarise(
    dplyr::across(
      c("gave", 
        "amount", 
        "doller_cond_giving", 
        "doller_per_letter_wt_match", 
        "doller_per_letter_wo_match"
      ),
      se
    )
  ) %>%
  dplyr::select(-treatment)

## ratio -------------------------------------------------------------------
### mean ------------------------------------------------------------------
Table_2_mean_ratio <- 
  giving %>%
  dplyr::group_by(ratio) %>%
  dplyr::mutate(
    doller_cond_giving = ifelse(gave==1, amount, NA),
    doller_per_letter_wt_match = amount,
    doller_per_letter_wo_match = (1 + ratio) * amount
  ) %>%
  dplyr::summarise(
    dplyr::across(
      c("gave", 
        "amount", 
        "doller_cond_giving", 
        "doller_per_letter_wt_match", 
        "doller_per_letter_wo_match"
      ),
      mean,
      na.rm = TRUE
    )
  ) %>%
  dplyr::filter(ratio != 0) %>%
  dplyr::select(-ratio)

### se --------------------------------------------------------------------
Table_2_se_ratio <- 
  giving %>%
  dplyr::group_by(ratio) %>%
  dplyr::mutate(
    doller_cond_giving = ifelse(gave==1, amount, NA),
    doller_per_letter_wt_match = amount,
    doller_per_letter_wo_match = (1 + ratio) * amount
  ) %>%
  dplyr::summarise(
    dplyr::across(
      c("gave", 
        "amount", 
        "doller_cond_giving", 
        "doller_per_letter_wt_match", 
        "doller_per_letter_wo_match"
      ),
      se
    )
  ) %>%
  dplyr::filter(ratio != 0) %>%
  dplyr::select(-ratio)

## bind tables ------------------------------------------------------------
Table_2 <-
  rbind(
    Table_2_mean_treatment_control,
    Table_2_se_treatment_control,
    Table_2_mean_ratio,
    Table_2_se_ratio
  ) %>%
  dplyr::rename(
    "寄付をした率" = gave,
    "寄附額" = amount,
    "寄付をした人の平均額" = doller_cond_giving,
    "手紙一通あたりの寄付額" = doller_per_letter_wt_match,
    "手紙一通あたりの寄付額 + マッチ額" = doller_per_letter_wo_match
  ) %>%
  t()

Table_2 <- Table_2[, c(1,4,2,5,7, 10, 8, 11, 9, 12 )] %>%
  as.data.frame() %>%
  dplyr::mutate(across(everything(), num, digits = 3))

colnames(Table_2) <- c(
  "Mean", "SE", "Mean", "SE", "Mean", "SE", "Mean", "SE", "Mean", "SE"
  )


## output table ------------------------------------------------------------
Table_2 %>%
  kableExtra::kbl() %>%
  kableExtra::add_header_above(
    c("寄附価格" = 1, "1" = 2, "0.36" = 2, "0.5" = 2, "0.33" = 2, "0.25" = 2)
  ) %>%
  kableExtra::add_header_above(
    c(" " = 1, "Control" = 2, "Treatment" = 2, "1:1" = 2, "1:2" = 2, "1:3" = 2)
  ) %>%
  kableExtra::kable_classic() %>%
  kableExtra::save_kable(
    "figuretable/demand_table2.png"
  )
## When got error in local environment, following code may work.
Table_2 %>%
  kableExtra::kbl() %>%
  kableExtra::add_header_above(
    c("寄附価格" = 1, "1" = 2, "0.36" = 2, "0.5" = 2, "0.33" = 2, "0.25" = 2)
  ) %>%
  kableExtra::add_header_above(
    c(" " = 1, "Control" = 2, "Treatment" = 2, "1:1" = 2, "1:2" = 2, "1:3" = 2)
  ) %>%
  kableExtra::kable_classic() %>%
  kableExtra::save_kable(
    "figuretable/demand_table2.html"
  )

webshot::webshot(
  "figuretable/demand_table2.html",
  "figuretable/demand_table2.png"
  )


### ttest -----------------------------------------------------------------
ttest_2_gave <- giving %>% 
  dplyr::filter(
     is.na(treatment) != TRUE &
       is.na(gave) != TRUE
  ) %>%
  infer::t_test(
    formula = gave ~ treatment,
  )

ttest_2_amount <- giving %>% 
  dplyr::filter(
    is.na(treatment) != TRUE &
      is.na(amount) != TRUE
  ) %>%
  infer::t_test(
    formula = amount ~ treatment,
  )

  dplyr::summarise(
    across(
      c("gave","amount"),
      t.test(.[treatment == 1], .[treatment == 0])$statistic)
    ) %>% 
  tidyr::gather(key, value) %>% 
  dplyr::mutate("T statistics" = value) %>% 
  dplyr::select("T statistics")


# Table3 ------------------------------------------------------------------

## regression -------------------------------------------------------------
giving %>%
  lm(formula = amount ~ treatment)


### column 1 --------------------------------------------------------------
Table3_1 <- giving %>%
  lm(formula = amount ~ treatment)
summary(Table3_1)

### column 2 --------------------------------------------------------------
Table3_2 <- giving %>%
  lm(formula = amount ~ 
       treatment + treatment : ratio2 + treatment : ratio3 +
       treatment : size25 + treatment : size50 + treatment : size50 + treatment : size100 +
       treatment : askd2 + treatment : askd3)
summary(Table3_2)

### column 3 --------------------------------------------------------------
Table3_3 <- giving %>%
  dplyr::filter(dormant == 1) %>%
  lm(formula = amount ~ treatment)
summary(Table3_3)

### column 4 --------------------------------------------------------------
Table3_4 <- giving %>%
  dplyr::filter(dormant == 1) %>%
  lm(formula = amount ~ 
       treatment + treatment : ratio2 + treatment : ratio3 +
       treatment : size25 + treatment : size50 + treatment : size50 + treatment : size100 +
       treatment : askd2 + treatment : askd3)
summary(Table3_4)

### column 5 --------------------------------------------------------------
Table3_5 <- giving %>%
  dplyr::filter(dormant == 0) %>%
  lm(formula = amount ~ treatment)
summary(Table3_5)

### column 6 --------------------------------------------------------------
Table3_6 <- giving %>%
  dplyr::filter(dormant == 0) %>%
  lm(formula = amount ~ 
       treatment + treatment : ratio2 + treatment : ratio3 +
       treatment : size25 + treatment : size50 + treatment : size50 + treatment : size100 +
       treatment : askd2 + treatment : askd3)
summary(Table3_6)

### column 7 --------------------------------------------------------------
Table3_7 <- giving %>% 
  lm(formula = amountchange ~ treatment)
summary(Table3_7)

### column 8 --------------------------------------------------------------
Table3_8 <- giving %>%
  lm(formula = amountchange ~ 
       treatment + treatment : ratio2 + treatment : ratio3 +
       treatment : size25 + treatment : size50 + treatment : size50 + treatment : size100 +
       treatment : askd2 + treatment : askd3)
summary(Table3_8)


## aggregate tables --------------------------------------------------------
modelsummary::modelsummary(
  title = "寄附価格と寄付行動の関係",
  list(Table3_1, Table3_2, Table3_3, Table3_4, Table3_5, Table3_6, Table3_7, Table3_8),
  gof_map = c("nobs", "r.squared"),
  statistic = "std.error"
)

modelsummary::modelsummary(
  title = "寄附価格と寄付行動の関係",
  list(Table3_1, Table3_2, Table3_3, Table3_4, Table3_5, Table3_6, Table3_7, Table3_8),
  gof_map = c("nobs", "r.squared"),
  statistic = "std.error",
  output = "huxtable"
  ) %>%
  huxtable::quick_xlsx(
    file = "figuretable/demand_table3.xlsx"
  )

Table3 <- 
  modelsummary::modelsummary(
    title = "寄附価格と寄付行動の関係",
    list(
      "(1)" = Table3_1, 
      "(2)" = Table3_2, 
      "(3)" = Table3_3, 
      "(4)" = Table3_4, 
      "(5)" = Table3_5, 
      "(6)" = Table3_6, 
      "(7)" = Table3_7, 
      "(8)" = Table3_8
      ),
    gof_map = c("nobs", "r.squared"),
    statistic = "std.error",
    output = "kableExtra"
    ) %>%
  kableExtra::add_header_above(
    c(" " = 1, 
      "All" = 2, 
      "2005年に寄付済み" = 2, 
      "2005年に寄付していない" = 2, 
      "All" = 2
      )
  ) %>%
  kableExtra::add_header_above(
    c("被説明変数 " = 1, 
      "寄付額" = 6, 
      "寄付額の増加" = 2
    )
  ) %>%
  kableExtra::kable_classic()

### export ----------------------------------------------------------------
Table3 %>%
  kableExtra::save_kable(
    file = "figuretable/demand_table3.png"
  )

# when webshot does not work in local environment.
Table3 %>%
  kableExtra::save_kable(
    file = "figuretable/demand_table3.html"
  )

webshot::webshot(
  "figuretable/demand_table3.html",
  "figuretable/demand_table3.png"
)

### export table ----------------------------------------------------------



library(jtools)
jtools::export_summs(Table3_1, Table3_2, scale = TRUE)

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
