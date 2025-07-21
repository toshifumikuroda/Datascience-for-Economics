# データの読み込み ----------------------------------------------------------------
giving <- 
  foreign::read.dta(
    file = "./input/day2/AERtables1-5.dta"
  )

# dplyr::summarize の使い方 ----------------------------------------------
giving |>
  dplyr::summarise(
    MRM2_mean = mean(MRM2, na.rm = TRUE)
  )

# dplyr::summarize の使い方2 --------------------------------------------------
giving |>
  dplyr::summarise(
    MRM2_mean = mean(MRM2, na.rm = TRUE),
    HPA_mean = mean(HPA, na.rm = TRUE)
  )

# dplyr::summarize の使い方3 --------------------------------------------------
giving |>
  dplyr::summarise(
    MRM2_mean = mean(MRM2, na.rm = TRUE),
    MRM2_sd = sd(MRM2, na.rm = TRUE),
    HPA_mean = mean(HPA, na.rm = TRUE),
    HPA_sd = sd(HPA, na.rm = TRUE)
  )

# dplyr::across() を利用した集計 -------------------------------------------------
controls <- c("MRM2", "HPA", "freq", "years", "dormant", "female", "couple")

table_1_mean <-
  giving |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(controls), 
      \(a_control) mean(a_control, na.rm = TRUE)
    )
  )

table_1_mean

# dplyr::across() を利用した標準偏差の集計 ------------------------
table_1_sd <-
  giving |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(controls), 
      \(a_control) sd(a_control, na.rm = TRUE)
    )
  )

table_1_sd

# 表の整形 ------------------------------------------------------
table_1_all <-
  rbind(table_1_mean, table_1_sd ) |>
  dplyr::rename(
    "最後の寄付からの期間" = MRM2,
    "過去の最高寄附額" = HPA,
    "過去の寄付回数" = freq,
    "初めて寄付してからの年数" = years,
    "2005年に既に寄付をしていたか" = dormant,
    "女性ダミー" = female,
    "既婚ダミー" = couple
  ) |>
  t()

colnames(table_1_all) <- c("Mean", "SD")

table_1_all <-
  table_1_all |>
  as.data.frame() |>  
  dplyr::mutate(
    dplyr::across(
      everything(), 
      \(x) tibble::num(x, digits = 3)
    )
  )

table_1_all

# グループ集計 -----------------------------------------------------
table_1_group_mean <-
  giving |>
  dplyr::group_by(treatment) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(controls),
      \(a_control) mean(a_control, na.rm = TRUE)
    )
  )

table_1_group_mean

# グループ標準偏差 --------------------------------------------------
table_1_group_sd <- 
  giving |>
  dplyr::group_by(treatment) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(controls),
      \(a_control) sd(a_control, na.rm = TRUE)
    )
  )

table_1_group_sd

# 表の整形 --------------------------------------------------------------------
table_1_group <-
  rbind(
    table_1_group_mean,   
    table_1_group_sd  
  ) |>
  dplyr::rename(
    "最後の寄付からの期間" = MRM2,
    "過去の最高寄附額" = HPA,
    "過去の寄付回数" = freq,
    "初めて寄付してからの年数" = years,
    "2005年に既に寄付をしていたか" = dormant,
    "女性ダミー" = female,
    "既婚ダミー" = couple
  ) |>
  dplyr::select(    -treatment  ) |>  
  t()

table_1_group

## 不要な列の削除と並び替え --------------------------------------
table_1_group <- 
  table_1_group[, c(2,5,1,4)] |>
  as.data.frame() |>
  dplyr::mutate(
    dplyr::across(
      everything(), 
      \(x) tibble::num(x, digits = 3)
    )
  )

colnames(table_1_group) <- c("Mean", "SD", "Mean", "SD")

table_1_group


# 表を横に束ねる方法 ------------------------------------------
table_1 <-
  cbind(
    table_1_all,
    table_1_group
  )

table_1


## 表の整形 --------------------------------------------------------------------


table_1 |>
  kableExtra::kbl() |>
  kableExtra::kable_styling()


## 集計対象行の追加 ------------------------------------------------
table_1 |>
  kableExtra::kbl() |>
  kableExtra::add_header_above(
    c(" " = 1, "All" = 2, "Treatment" = 2, "Control" = 2)
  ) |>
  kableExtra::kable_styling()

table_1 |>
  kableExtra::kbl() |>
  kableExtra::add_header_above(
    c(" " = 1, "All" = 2, "Treatment" = 2, "Control" = 2)
  ) |>
  kableExtra::kable_classic() |>
  kableExtra::save_kable(
    "figuretable/demand_table1.png"
  )


# 表2の作成 -------------------------------------------------------------------


## 行の値を作成 ------------------------------------------------------------------

giving_table_2 <- 
  giving |>
  dplyr::filter(
    !is.na(treatment)
  ) |>
  dplyr::mutate(
    doller_cond_giving = ifelse(gave==1, amount, NA),
    doller_per_letter_wt_match = amount,
    doller_per_letter_wo_match = (1 + ratio) * amount,
  )


## 変数名セットの作成 ---------------------------------------------------------------

x_table2 <-
  c(
    "gave",
    "amount",
    "doller_cond_giving",
    "doller_per_letter_wt_match",
    "doller_per_letter_wo_match"
  )


## Control列とTreatment列の作成 --------------------------------------------------

table_2_mean_treatment_control <-
  giving_table_2 |>
  dplyr::group_by(treatment) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(x_table2),
      \(x) mean(x, na.rm = TRUE)
    )
  ) |> 
  dplyr::select(-treatment)

table_2_mean_treatment_control


## Control列とTreatment列の標準誤差の作成 --------------------------------------------------
table_2_se_treatment_control <-
  giving_table_2 |>
  dplyr::group_by(treatment) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(x_table2),
      \(x) {
        x <- na.omit(x)
        sqrt(var(x) / length(x))
      }
    ) 
  ) |>  
  dplyr::select(-treatment)

table_2_se_treatment_control


## 3，4，5列目の作成 --------------------------------------------------------------
table_2_mean_ratio <- 
  giving_table_2 |>
  dplyr::group_by(ratio) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(x_table2),
      \(x) mean(x, na.rm = TRUE)
    )
  ) |>
  dplyr::filter(ratio != 0) |> 
  dplyr::select(-ratio)

table_2_mean_ratio

## 3，4，5列目のseの作成 --------------------------------------------------------------

table_2_se_ratio <- 
  giving_table_2 |>
  dplyr::group_by(ratio) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(x_table2),
      \(x) {
        x <- na.omit(x)
        sqrt(var(x) / length(x))
      }
    )
  ) |>
  dplyr::filter(ratio != 0) |> 
  dplyr::select(-ratio)

table_2_se_ratio


# 二つの表を結合し、列名を修正し、転置し、必要な列を必要な順に並べ直す -------------------------------------- 
table_2 <-  
  rbind(
    table_2_mean_treatment_control, 
    table_2_se_treatment_control, 
    table_2_mean_ratio, 
    table_2_se_ratio
  ) |>
  dplyr::rename(
    "寄付をした率" = gave,
    "寄附額" = amount,
    "寄付をした人の平均額" = doller_cond_giving,
    "手紙一通あたりの寄付額" = doller_per_letter_wt_match,
    "手紙一通あたりの寄付額 + マッチ額" = doller_per_letter_wo_match
  ) |>
  t()

table_2 <- 
  table_2[, c(1, 3, 2, 4, 5, 8, 6, 9, 7, 10)] |>
  as.data.frame() |>
  dplyr::mutate(
    dplyr::across(
      everything(), 
      \(x) tibble::num(x, digits = 3)
      )
  )

colnames(table_2) <- 
  c(
    rep(
      c("Mean","SE"),5
    )
  )

table_2

## 表の様式を整えて画像として出力 ------------------------------------------------------
table_2_edited <-
  table_2 |>
  kableExtra::kbl() |>
  kableExtra::add_header_above(
    c("寄附価格" = 1, "1" = 2, "0.36" = 2, "0.5" = 2, "0.33" = 2, "0.25" = 2)
  ) |>
  kableExtra::add_header_above(
    c(" " = 1, "Control" = 2, "Treatment" = 2, "1:1" = 2, "1:2" = 2, "1:3" = 2)
  ) |>
  kableExtra::kable_classic()

table_2_edited

table_2_edited |>
  kableExtra::save_kable(
    "figuretable/demand_table2.png"
  )

# ｔ検定してみた -------------------------------------------------------
ttest_2_gave <- 
  giving |> 
  dplyr::filter(
    !is.na(treatment) & !is.na(gave)
  ) |>
  infer::t_test(
    formula = gave ~ treatment
  ) 

ttest_2_gave


# 表4 ----------------------------------------------------------------------

giving |> 
  lm(formula = amount ~ treatment) 



## 1列目 ---------------------------------------------------------------------


table4_1 <- 
  giving |> 
  lm(formula = amount ~ treatment)

summary(table4_1)

## 重回帰分析による表4-2の作成 ---------------------------------------------------------

table4_2 <-
  giving |> 
  lm(formula = amount ~ 
       treatment + treatment : ratio2 + treatment : ratio3 + 
       treatment : size25 + treatment : size50 + treatment : size100 +  
       treatment : askd2 + treatment : askd3)

summary(table4_2)

## 表4-3 --------------------------------------------------------------------
table4_3 <- giving |> 
  dplyr::filter(dormant == 1) |> 
  lm(formula = amount ~ treatment)

summary(table4_3)


## 表4-4 --------------------------------------------------------------------
table4_4 <- giving |> 
  dplyr::filter(dormant == 1) |> 
  lm(formula = amount ~ 
       treatment + treatment : ratio2 + treatment : ratio3 + 
       treatment : size25 + treatment : size50 + treatment : size100 +  
       treatment : askd2 + treatment : askd3)

summary(table4_4)

## 表4-5 --------------------------------------------------------------------
table4_5 <-
  giving |> 
  dplyr::filter(dormant == 0) |> 
  lm(formula = amount ~ treatment)

summary(table4_5)


## 表4-6 --------------------------------------------------------------------
table4_6 <-
  giving |> 
  dplyr::filter(dormant == 0) |> 
  lm(formula = amount ~ 
       treatment + treatment : ratio2 + treatment : ratio3 + 
       treatment : size25 + treatment : size50 + treatment : size100 +  
       treatment : askd2 + treatment : askd3)

summary(table4_6)


## 表4-7 -------------------------------------------------------------
table4_7 <-
  giving |> 
  lm(formula = amountchange ~ treatment)

summary(table4_7)

## 表4-8 --------------------------------------------------------------------
table4_8 <-
  giving |> 
  lm(formula = amountchange ~ 
       treatment + treatment : ratio2 + treatment : ratio3 + 
       treatment : size25 + treatment : size50 + treatment : size100 +  
       treatment : askd2 + treatment : askd3)

summary(table4_8)

## 結果をまとめた表を作成 ---------------------------------------------------------
table4 <- 
  modelsummary::modelsummary(
    list(
      table4_1, 
      table4_2, 
      table4_3, 
      table4_4, 
      table4_5, 
      table4_6, 
      table4_7, 
      table4_8
    ),
    gof_map = c("r.squared", "nobs"),
    statistic = "std.error"
  ) 

table4

## 表のファイルへの出力 --------------------------------------------------------------
modelsummary::modelsummary(
  title = "寄附価格と寄付行動の関係",
  list(
    table4_1,
    table4_2, 
    table4_3, 
    table4_4, 
    table4_5, 
    table4_6, 
    table4_7, 
    table4_8
  ),
  gof_map = c("nobs", "r.squared"),
  statistic = "std.error",
  output = "./figuretable/demand_table4.html"
) 

## 表のpng保存 ------------------------------------------------------


modelsummary::modelsummary(
  title = "寄附価格と寄付行動の関係",
  list(
    table4_1,
    table4_2, 
    table4_3, 
    table4_4, 
    table4_5, 
    table4_6, 
    table4_7, 
    table4_8
  ),
  gof_map = c("nobs", "r.squared"),
  statistic = "std.error",
  output = "kableExtra") |>
  kableExtra::kable_classic() |>
  kableExtra::save_kable(
    "figuretable/demand_table4.png"
  )


# 表のエクセルシートとしての出力 ---------------------------------------------------------
modelsummary::modelsummary(
  title = "寄附価格と寄付行動の関係",
  list(table4_1, table4_2, table4_3, table4_4, table4_5, table4_6, table4_7, table4_8),
  gof_map = c("nobs", "r.squared"),   statistic = "std.error",
  output = "huxtable"
) |>
  huxtable::quick_xlsx(file = "figuretable/demand_table4.xlsx")
