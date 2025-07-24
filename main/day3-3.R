# 初期化 ------------------------------------------------------------
rm(list = ls())

# データの取得と読み込み ------------------------------------------------
download.file("https://stacks.stanford.edu/file/druid:xh580yd6172/realdata.csv", "./input/realdata.csv")
realdata <- readr::read_csv("./input/realdata.csv")

# 図1ヒストグラムの作成 -----------------------------------------------------------------
realdata |>
  ggplot2::ggplot(ggplot2::aes(x=amanagement))+
  ggplot2::geom_histogram(ggplot2::aes(y = ..density..), bins = 50)+
  ggplot2::facet_wrap(cty ~ ., nrow = 2)+
  ggplot2::scale_y_continuous(breaks=seq(0,1.2,0.2)) +
  ggplot2::labs(title="Distribution of Management Scores by Country",x="Management Score", y = "Density")+
  ggplot2::theme_classic()

# 経営スコアの記述統計 --------------------------------------------------------------
realdata |>
  dplyr::select(management, cty) |>
  dplyr::group_by(cty) |>
  dplyr::summarise(
    mean = mean(management, na.rm = TRUE),
    sd = sd(management, na.rm = TRUE),
    min = min(management, na.rm = TRUE),
    median = median(management, na.rm = TRUE),
    max = max(management, na.rm = TRUE)
  )

# 表1労働・資本・中間投入物を生産要素にした生産関数等 ----------------------------------------------
cyy <- paste("cyy", 1:44, sep = "")
cyy

## 一列目 ---------------------------------------------------------------------
### 変数の定義 ------------------------------------------------------------------
col1 <- c("zmanagement", "le", "le_fr", "le_gr", "le_uncons", "le_us", "uncons")


### formulaの作成 ------------------------------------------------------------------- 
xvars_col1 <- paste(c(col1, cyy), collapse = " + ")
model_col1 <- paste("ls", xvars_col1, sep = " ~ ")
model_col1 <- as.formula(model_col1)

### 回帰 ------------------------------------------------------------------
table1_1 <- 
  realdata |> 
  tidyr::drop_na(lm, lp) |>
  estimatr::lm_robust(formula = model_col1, clusters = code)

table1_1

### STATAと同じクラスター誤差に頑健な標準誤差 -----------------------
table1_1 <-
  realdata |>
  tidyr::drop_na(lm, lp) |>
  estimatr::lm_robust(formula = model_col1, clusters = code, se_type = "stata")

table1_1

## 二列目 ---------------------------------------------------------------------
### 右辺変数の指定 -----------------------------------------------------------------
model_base <- c("zmanagement", "le", "lp", "lm", 
                "le_fr", "le_gr", "le_uncons", "le_us", 
                "lp_fr", "lp_gr", "lp_uncons", "lp_us", 
                "lm_fr", "lm_gr", "lm_un", "lm_us", "uncons")

### formulaの作成 ----------------------------------------------------------
xvars_col2 <- paste(c(model_base, cyy), collapse = " + ")
model_col2 <- paste("ls", xvars_col2, sep = " ~ ")
model_col2 <- as.formula(model_col2)

### 推定 ------------------------------------------------------------------
table1_2 <-
  realdata |>
  tidyr::drop_na(lm, lp) |>
  estimatr::lm_robust(formula = model_col2, clusters = code , se_type = "stata")

table1_2


## 3列目からの変数群の定義 ------------------------------------------------------------

controls <- c("lfirmage", "public", "ldegree", "ldegreemiss", "mba", "mbamiss", "lhrs", "factor(sic3)") 

### formulaの作成 ----------------------------------------------------------
xvars_col3 <- paste(c(model_base, controls, cyy), collapse = " + ")
model_col3 <- paste("ls", xvars_col3, sep = " ~ ")
model_col3 <- as.formula(model_col3)

### 推定 ------------------------------------------------------------------
table1_3 <-
  realdata |>
  tidyr::drop_na(lm, lp) |>
  estimatr::lm_robust(formula = model_col3, clusters = code , se_type = "stata")

table1_3

## 4列目からの変数群の定義 ------------------------------------------------------------

noise <- c("gender", "sen1", "tenurepost", "countries", 
           "day2", "day3", "day4", "day5", "timelocal", "duration", "reli",  
           "aa1", "aa2", "aa3", "aa4", "aa5", "aa6", "aa7", "aa8", "aa9", 
           "aa10", "aa11", "aa12", "aa13", "aa14", "aa15", "aa16")


### formulaの作成 ----------------------------------------------------------
xvars_col4 <- paste(c(model_base, controls, cyy, noise), collapse = " + ")
model_col4 <- paste("ls", xvars_col4, sep = " ~ ")
model_col4 <- as.formula(model_col4)

### 推定 ------------------------------------------------------------------
table1_4 <- 
  realdata |>
  tidyr::drop_na(lm, lp) |>
  estimatr::lm_robust(formula = model_col3, clusters = code , se_type = "stata")

table1_4

## 6列目 ------------------------------------------------------------------
### 右辺変数の指定 -------------------------------------------------------
xvars_col6 <- paste(c(model_base, noise, cyy, controls), collapse = " + ")

### formulaの作成 ----------------------------------------------------------
model_col6 <- paste("roce", xvars_col6, sep = " ~ ")
model_col6 <- as.formula(model_col6)

### 推定 ------------------------------------------------------------------
table1_6 <- 
  realdata |> 
  tidyr::drop_na(lm, lp) |> 
  estimatr::lm_robust(formula = model_col6, clusters = code, se_type = "stata") 

table1_6

## 7列目 ------------------------------------------------------------------
### 右辺変数の指定 -------------------------------------------------------
xvars_col7 <- paste(c(model_base, noise, cyy, controls), collapse = " + ")

### formulaの作成 ----------------------------------------------------------
model_col7 <- paste("lq", xvars_col7, sep = " ~ ")
model_col7 <- as.formula(model_col7)

### 推定 ------------------------------------------------------------------
table1_7 <-
  realdata |> 
  tidyr::drop_na(lm, lp) |> 
  estimatr::lm_robust(formula = model_col7, clusters = code, se_type = "stata") 

table1_7

## 8列目 ------------------------------------------------------------------
### 右辺変数の指定 -------------------------------------------------------
xvars_col9 <- paste(c(model_base, noise, cyy, controls), collapse = " + ")

### formulaの作成 ----------------------------------------------------------
model_col9 <- paste("dsales", xvars_col9, sep = " ~ ")
model_col9 <- as.formula(model_col9)

### 推定 ------------------------------------------------------------------
table1_9 <-
  realdata |> 
  tidyr::drop_na(lm, lp) |> 
  estimatr::lm_robust(formula = model_col9, clusters = code, se_type = "stata") 

table1_9

# 表1の纏め -------------------------------------------------------------------

### 変数名の定義 ---------------------------------------------------------------

table1_coef_map <- c(
  "zmanagement" = "Management z-score",
  "le" = "Ln(Labor)",             
  "lp" = "Ln(Capital)", 
  "lm" = "Ln(Materials)"
)

### 追加行の定義 ------------------------------------------------------------------
table1_rows <-
  data.frame(rbind(
    c("Estimation method ", "OLS", "OLS", "OLS", "OLS","OLS", "OLS", "OLS"),
    c("Firms", "All", "All", "All", "All", "All", "Quoted", "All"),
    c("Dependent variable ", "Sales", "Sales", "Sales", "Sales", "Profitability", "Tobin’s av. Q", "Sales growth"),
    c("Country, time, and industry dummies", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"), 
    c("General controls", "No", "No", "Yes", "Yes", "Yes", "Yes", "Yes"),
    c("Noise controls", "No", "No", "No", "Yes", "Yes", "Yes", "Yes")
  ))
attr(table1_rows, 'position') <- c(1,2,3,12,13,14) 

### 表の作成 --------------------------------------------------------------------
table1 <- 
  modelsummary::modelsummary(
    list(
      "(1)"=table1_1, 
      "(2)"=table1_2, 
      "(3)"=table1_3,
      "(4)"=table1_4, 
      "(6)"=table1_6,
      "(7)"=table1_7,
      "(9)"=table1_9
    ),
    coef_map = table1_coef_map,
    statistics = c('# observations' = 'nobs'),
    add_rows = table1_rows,
    gof_map = c("nobs", "r.squared")
  ) 

table1

### 表1のpngファイルを出力 -----------------------------------------------------------

modelsummary::modelsummary(
  list(
    "(1)"=table1_1, 
    "(2)"=table1_2, 
    "(3)"=table1_3,
    "(4)"=table1_4, 
    "(6)"=table1_6,
    "(7)"=table1_7,
    "(9)"=table1_9
  ),
  coef_map = table1_coef_map,
  statistics = c('# observations' = 'nobs'),
  add_rows = table1_rows,
  gof_map = c("nobs", "r.squared"),
  output = "kableExtra"
) |> 
  kableExtra::kable_classic() |>
  kableExtra::save_kable(
    "figuretable/firm_table1.png"
  )


### 表1のhtmlファイルを出力 -----------------------------------------------------------

modelsummary::modelsummary(
  list(
    "(1)"=table1_1, 
    "(2)"=table1_2, 
    "(3)"=table1_3,
    "(4)"=table1_4, 
    "(6)"=table1_6,
    "(7)"=table1_7,
    "(9)"=table1_9
  ),
  coef_map = table1_coef_map,
  statistics = c('# observations' = 'nobs'),
  add_rows = table1_rows,
  gof_map = c("nobs", "r.squared"),
  output = "kableExtra"
) |> 
  kableExtra::kable_classic() |>
  kableExtra::save_kable(
    "figuretable/firm_table1.html"
  )


# 表2 ----------------------------------------------------------------------


## クロスセクションデータの作成 ----------------------------------------------------------


table2_data <- 
  realdata |> 
  dplyr::rowwise() |>
  dplyr::mutate(zcapital = mean(c(zlean1, zlean2, zperf2), na.rm = TRUE)) |>
  dplyr::mutate(zhuman = mean(c(ztalent1, ztalent6, ztalent7), na.rm = TRUE)) |>
  dplyr::mutate(zhuman_zcapital = zhuman-zcapital) |>
  dplyr::ungroup() |>
  dplyr::group_by(code) |>
  dplyr::filter(year == max(year))


## モデルの回帰 ------------------------------------------------------------------
table2_1 <-
  table2_data |>
  estimatr::lm_robust(
    formula = zhuman ~ ldegree + ldegreemiss + cceurope + ccgermany + ccus,
    se_type = "stata")

table2_2 <-
  table2_data |>
  estimatr::lm_robust(
    formula = zcapital ~ ldegree + ldegreemiss + cceurope + ccgermany + ccus, 
    se_type = "stata")

table2_3 <-
  table2_data |>
  estimatr::lm_robust(
    formula = zhuman_zcapital ~ ldegree + ldegreemiss + cceurope + ccgermany + ccus, 
    se_type = "stata")

table2_4 <-
  table2_data |>
  estimatr::lm_robust(
    formula = zhuman_zcapital ~ ldegree + ldegreemiss + lempm + lfirmage + public + cceurope + ccgermany + ccus + factor(sic3),
    se_type = "stata")

table2_5 <-
  table2_data |>
  estimatr::lm_robust(
    formula = zhuman_zcapital ~ law + lempm + public + cceurope + ccgermany + ccus + factor(sic3), se_type = "stata")

## 回帰のまとめ表の作成 --------------------------------------------------------------
### 追加行の定義 ------------------------------------------------------------------
table2_rows <-
  data.frame(rbind(
    c("Dependent variable", 
      "Human capital management", 
      "Fixed capital management",
      "Human capital - fixed capital management", 
      "Human capital - fixed capital management", 
      "Human capital - fixed capital management"    ),
    c("General controls", "No", "No", "No", "Yes", "Yes"),
    c("Industry controls", "No", "No", "No", "Yes", "Yes")  )
  )
attr(table2_rows, 'position') <- c(1,6,7)

### 変数名の定義 ---------------------------------------------------------------

table2_coef_map <- c(
  "ldegree" = "Ln(proportion of employees with college derees)",
  "law" = "Ln(firm averae wages)"
)

### 表の作成 --------------------------------------------------------------------
table2 <- 
  modelsummary::modelsummary(
    list(
      "(1)"=table2_1, 
      "(2)"=table2_2, 
      "(3)"=table2_3, 
      "(4)"=table2_4, 
      "(5)"=table2_5
    ),
    coef_map = table2_coef_map,
    statistics = "std.error",
    add_rows = table2_rows,
    gof_map = c("nobs", "r.squared")
  )

table2


# 表3 -----------------------------------------------------------------

## コントロール変数の定義 ------------------------------------------

table3_control <- 
  c(
    "lempm", 
    "lfirmage",
    "public",
    "ldegree",
    "ldegreemiss",
    "mba",
    "mbamiss",
    "uncons",
    "ccfrance",
    "ccgermany",
    "ccuk",
    noise
  ) 



## 1列目 ---------------------------------------------------------------------


### 右辺変数の指定 -----------------------------------------------------------------


xvars_table3_col1 <- paste(c("lindiopen9599", "factor(cty)"), collapse = " + ")


### formulaの作成 --------------------------------------------------------------


model_table3_col1 <- paste("zmanagement", xvars_table3_col1, sep = " ~ ")
model_table3_col1 <- as.formula(model_table3_col1)


### 回帰 ----------------------------------------------------------------------


table3_1 <- 
  realdata |>  
  dplyr::group_by(code) |>
  dplyr::filter(year == max(year)) |>
  estimatr::lm_robust(
    formula = model_table3_col1, 
    clusters = oecdind_cty, 
    se_type = "stata"
  )


## 二列目 ---------------------------------------------------------------------


### 右辺変数の定義 -----------------------------------------------------------------


xvars_table3_col2 <- 
  paste(
    c(
      "lindiopen9599", 
      "factor(sic3)", 
      table3_control), 
    collapse = " + "
  )


### formulaの作成 --------------------------------------------------------------


model_table3_col2 <- 
  paste("zmanagement", xvars_table3_col2, sep = " ~ ")
model_table3_col2 <- as.formula(model_table3_col2)


### 回帰 ----------------------------------------------------------------------

table3_2 <-
  realdata |>
  dplyr::group_by(code) |>
  dplyr::filter(year == max(year)) |>
  estimatr::lm_robust(
    formula = model_table3_col2, 
    clusters = oecdind_cty, 
    se_type = "stata"
  )


## 3列目 ---------------------------------------------------------------------


### 右辺変数の定義 -----------------------------------------------------------------


xvars_table3_col3 <- paste(c("lerner", "factor(cty)"), collapse = " + ")


### formulaの作成 --------------------------------------------------------------

model_table3_col3 <- paste("zmanagement", xvars_table3_col3, sep = " ~ ")
model_table3_col3 <- as.formula(model_table3_col3)



### 回帰 ----------------------------------------------------------------------


table3_3 <- 
  realdata |>
  dplyr::group_by(code) |>
  dplyr::filter(year == max(year)) |>
  estimatr::lm_robust(
    formula = model_table3_col3, 
    clusters = csic3, 
    se_type = "stata"
  )



## 4列目 ---------------------------------------------------------------------

xvars_table3_col4 <- paste(c("lerner", "factor(sic3)", table3_control), collapse = " + ")
model_table3_col4 <- paste("zmanagement", xvars_table3_col4, sep = " ~ ")
model_table3_col4 <- as.formula(model_table3_col4)


### 回帰 ----------------------------------------------------------------------

table3_4 <- 
  realdata |> 
  dplyr::group_by(code) |>
  dplyr::filter(year == max(year)) |>
  estimatr::lm_robust(
    formula = model_table3_col4, 
    clusters = csic3, 
    se_type = "stata"
  )

## 5列目 ---------------------------------------------------------------------


### 右辺変数の定義 -----------------------------------------------------------------


xvars_table3_col5 <- paste(c("competition", "competitionmiss"), collapse = " + ")


### formulaの作成 --------------------------------------------------------------

model_table3_col5 <- paste("zmanagement", xvars_table3_col5, sep = " ~ ")
model_table3_col5 <- as.formula(model_table3_col5)



### 回帰 ----------------------------------------------------------------------


table3_5 <-
  realdata |> 
  dplyr::group_by(code) |>
  dplyr::filter(year == max(year)) |>
  estimatr::lm_robust(
    formula = model_table3_col5, 
    clusters = csic3, 
    se_type = "stata"
  )



## 6列目 ---------------------------------------------------------------------

xvars_table3_col6 <- paste(c("competition", "competitionmiss", table3_control), collapse = " + ")
model_table3_col6 <- paste("zmanagement", xvars_table3_col6, sep = " ~ ")
model_table3_col6 <- as.formula(model_table3_col6)


### 回帰 ----------------------------------------------------------------------

table3_6 <-
  realdata |> 
  dplyr::group_by(code) |>
  dplyr::filter(year == max(year)) |>
  estimatr::lm_robust(
    formula = model_table3_col6,
    clusters = csic3,
    se_type = "stata"
  )


## 7列目 ---------------------------------------------------------------------


### 右辺変数の定義 -----------------------------------------------------------------


xvars_table3_col7 <- paste(
  c("competition", "competitionmiss",
    "lindiopen9599", "factor(sic3)",
    "lerner", "factor(sic3)"),
  collapse = " + ")


### formulaの作成 --------------------------------------------------------------

model_table3_col7 <- paste("zmanagement", xvars_table3_col7, sep = " ~ ")
model_table3_col7 <- as.formula(model_table3_col7)



### 回帰 ----------------------------------------------------------------------


table3_7 <-
  realdata |>
  dplyr::group_by(code) |>
  dplyr::filter(year == max(year)) |>
  estimatr::lm_robust(
    formula = model_table3_col7,
    clusters = csic3, 
    se_type = "stata"
  )



## 8列目 ---------------------------------------------------------------------

xvars_table3_col8 <- 
  paste(
    c(
      c("competition", "competitionmiss",
        "lindiopen9599", "factor(sic3)",
        "lerner", "factor(sic3)"),
      table3_control
    ),
    collapse = " + "
  )
model_table3_col8 <- paste("zmanagement", xvars_table3_col6, sep = " ~ ")
model_table3_col8 <- as.formula(model_table3_col8)


### 回帰 ----------------------------------------------------------------------

table3_8 <- 
  realdata |>
  dplyr::group_by(code) |>
  dplyr::filter(year == max(year)) |>
  estimatr::lm_robust(
    formula = model_table3_col8, 
    clusters = csic3,
    se_type = "stata"
  )


## 表3のまとめ ------------------------------------------------------------------



### 変数名の定義 ------------------------------------------------------------------


table3_coef_map <- c(
  "lindiopen9599" = "Import penetration (5-years lagged)",
  "lerner" = "Lerner index (5-years lagged)",
  "competition" = "Number of competitors"
)


### 追加行の定義 ------------------------------------------------------------------


table3_rows <-  data.frame(rbind(
  c("Estimation method ", "OLS", "OLS", "OLS", "OLS", "OLS", "OLS", "OLS", "OLS"),
  c("Dependent variable", 
    "Management z-score", "Management z-score", "Management z-score", "Management z-score", 
    "Management z-score", "Management z-score", "Management z-score", "Management z-score"),
  c("General controls", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes")  )  )
attr(table3_rows, 'position') <- c(1,2,10)


### 表の作成 --------------------------------------------------------------------

table3 <- 
  modelsummary::modelsummary(
    list(
      "(1)"=table3_1,
      "(2)"=table3_2,
      "(3)"=table3_3, 
      "(4)"=table3_4, 
      "(5)"=table3_5,
      "(6)"=table3_6,
      "(7)"=table3_7,
      "(8)"=table3_8
    ),
    coef_map = table3_coef_map,
    statistics = "std.error",
    add_rows = table3_rows,
    gof_map = c("nobs", "r.squared")
  )

table3

