# 初期化 ------------------------------------------------------------
rm(list = ls())


# zipファイルの解凍 --------------------------------------------------------------

for (i in 1:5) {
  zipfilename <- paste("./input/day4/maketable", toString(i), ".zip", sep="")
  unzip(zipfile = zipfilename, exdir = "./input/day4/")
}


# 図1の再現 -------------------------------------------------------------------

table1_df <- foreign::read.dta("./input/day4/maketable1.dta") 
figure1 <- table1_df |>
  dplyr::filter(baseco==1) |> 
  ggplot2::ggplot(ggplot2:: aes(x = logem4, y = logpgp95, label = shortnam)) + 
  ggplot2::geom_text() + 
  ggplot2::stat_smooth(formula = "y ~ x", method = "lm", se = FALSE, colour = "black") + 
  ggplot2::xlab("Log European settler mortality") + 
  ggplot2::ylab("log GDP per capita, PPP, in 1995") + 
  ggplot2::theme_classic()

figure1


# 表1の再現 -------------------------------------------------------------------

colvars <- c("logpgp95", "loghjypl", "avexpr", "cons00a", "cons1", "democ00a", "euro1900", "logem4")


## 1列目の作成 -----------------------------------------------------------------
table1_col1_mean <- table1_df |>
  dplyr::summarise( dplyr::across(dplyr::all_of(colvars), mean, na.rm = TRUE)  )

table1_col1_sd <- table1_df |>
  dplyr::summarise(    dplyr::across(dplyr::all_of(colvars), sd, na.rm = TRUE)  )

table1_col1_n <- table1_df |>
  dplyr::summarise(    dplyr::across(dplyr::all_of(colvars), ~ sum(!is.na(.)))  )

## 2列目の作成 ----------------------------------------------------------------
table1_col2_mean <- table1_df |>
  dplyr::filter(baseco == 1) |>
  dplyr::summarise(    dplyr::across(dplyr::all_of(colvars), mean, na.rm = TRUE)  )

table1_col2_sd <- table1_df |>
  dplyr::filter(baseco == 1) |>
  dplyr::summarise(    dplyr::across(dplyr::all_of(colvars), sd, na.rm = TRUE)  ) 

table1_col2_n <- table1_df |>
  dplyr::filter(baseco == 1) |>
  dplyr::summarise(    dplyr::across(dplyr::all_of(colvars), ~ sum(!is.na(.)))  )

## 3列目の作成 ----------------------------------------------------------------
table1_col3_mean <- table1_df |>
  dplyr::filter(!is.na(logem4)) |>
  dplyr::filter(baseco==1 & quantile(logem4, 0.25, na.rm = TRUE) > logem4  ) |>
  dplyr::summarise(    dplyr::across(dplyr::all_of(colvars), mean, na.rm = TRUE)  )

table1_col3_sd <- table1_df |>
  dplyr::filter(!is.na(logem4)) |>
  dplyr::filter(baseco==1 & quantile(logem4, 0.25, na.rm = TRUE) > logem4  ) |>
  dplyr::summarise(    dplyr::across(dplyr::all_of(colvars), sd, na.rm = TRUE)  )

table1_col3_n <- table1_df |>
  dplyr::filter(!is.na(logem4)) |>
  dplyr::filter(baseco==1 & quantile(logem4, 0.25, na.rm = TRUE) > logem4
  ) |>  dplyr::summarise(    dplyr::across(dplyr::all_of(colvars), ~ sum(!is.na(.)))  )


## 4列目の作成 ---------------------------------------------------------------- 
table1_col4_mean <- table1_df |>
  dplyr::filter(!is.na(logem4)) |>
  dplyr::filter(baseco==1 & quantile(logem4, 0.25, na.rm = TRUE) <= logem4 &quantile(logem4, 0.50, na.rm = TRUE) > logem4) |>
  dplyr::summarise(    dplyr::across(dplyr::all_of(colvars), mean, na.rm = TRUE)  )

table1_col4_n <- table1_df |>
  dplyr::filter(!is.na(logem4)) |>
  dplyr::filter(baseco==1 & quantile(logem4, 0.25, na.rm = TRUE) <= logem4 &quantile(logem4, 0.50, na.rm = TRUE) > logem4) |>
  dplyr::summarise(    dplyr::across(dplyr::all_of(colvars), ~ sum(!is.na(.)))
  )

table1_col4_sd <- table1_df |>
  dplyr::filter(!is.na(logem4)) |>
  dplyr::filter(baseco==1 & quantile(logem4, 0.25, na.rm = TRUE) <= logem4 &quantile(logem4, 0.50, na.rm = TRUE) > logem4) |>
  dplyr::summarise(    dplyr::across(dplyr::all_of(colvars), sd, na.rm = TRUE)  )

## 5列目の作成 ---------------------------------------------------------------- 
table1_col5_mean <- table1_df |>
  dplyr::filter(!is.na(logem4)) |>
  dplyr::filter(baseco==1 & quantile(logem4, 0.5, na.rm = TRUE) <= logem4 &quantile(logem4, 0.75, na.rm = TRUE) > logem4) |>
  dplyr::summarise(    dplyr::across(dplyr::all_of(colvars), mean, na.rm = TRUE)  )

table1_col5_n <- table1_df |>
  dplyr::filter(!is.na(logem4)) |>
  dplyr::filter(baseco==1 & quantile(logem4, 0.5, na.rm = TRUE) <= logem4 &quantile(logem4, 0.75, na.rm = TRUE) > logem4) |>
  dplyr::summarise(    dplyr::across(dplyr::all_of(colvars), ~ sum(!is.na(.)))  )

table1_col5_sd <- table1_df |>
  dplyr::filter(!is.na(logem4)) |>
  dplyr::filter(baseco==1 & quantile(logem4, 0.5, na.rm = TRUE) <= logem4 &quantile(logem4, 0.75, na.rm = TRUE) > logem4) |>
  dplyr::summarise(    dplyr::across(dplyr::all_of(colvars), sd, na.rm = TRUE)  )

## 6列目の作成 ---------------------------------------------------------------- 
table1_col6_mean <- table1_df |>
  dplyr::filter(!is.na(logem4)) |>
  dplyr::filter(baseco==1 & quantile(logem4, 0.75, na.rm = TRUE) <= logem4 ) |>
  dplyr::summarise(    dplyr::across(dplyr::all_of(colvars), mean, na.rm = TRUE)  )

table1_col6_n <- table1_df |>
  dplyr::filter(!is.na(logem4)) |>
  dplyr::filter(baseco==1 & quantile(logem4, 0.75, na.rm = TRUE) <= logem4 ) |>
  dplyr::summarise(    dplyr::across(dplyr::all_of(colvars), ~ sum(!is.na(.)))  )

table1_col6_sd <- table1_df |>
  dplyr::filter(!is.na(logem4)) |>
  dplyr::filter(baseco==1 & quantile(logem4, 0.75, na.rm = TRUE) <= logem4 ) |>
  dplyr::summarise(    dplyr::across(dplyr::all_of(colvars), sd, na.rm = TRUE)  )


## 表1の列を纏める ----------------------------------------------------------------

table1 <- rbind(  table1_col1_mean,  table1_col1_sd, table1_col1_n,
                  table1_col2_mean,  table1_col2_sd, table1_col2_n, 
                  table1_col3_mean,  table1_col3_sd, table1_col3_n,
                  table1_col4_mean,  table1_col4_sd, table1_col4_n,
                  table1_col5_mean,  table1_col5_sd, table1_col5_n,
                  table1_col6_mean,  table1_col6_sd, table1_col6_n) |>
  dplyr::rename(
    "log GDP per capita, PPP, in 1995" = logpgp95,
    "log output per worker in 1988" = loghjypl, 
    "Average protection against expropriation risk, 1985-1995" = avexpr, 
    "Constraint on executive, 1900" = cons00a, 
    "Constraint on executive in first year of independence" = cons1, 
    "Democracy in 1900" = democ00a, 
    "European settlement in 1900" = euro1900,
    "Log European settler mortality" = logem4) |>
  t() 



## 列名の付与と表の見栄え整理 -----------------------------------------------------------

colnames(table1) <- rep(c("Mean", "SD", "N"),6)

table1 <-
  table1 |>
  kableExtra::kbl(
    digits = 3
  ) |>
  kableExtra::add_header_above(
    c(" " = 1, "Whole world" = 3, "Base sample" = 3, "(1)" = 3, "(2)" = 3, "(3)" = 3, "(4)" = 3)
  ) |>
  kableExtra::add_header_above(
    c(" " = 7, "By quantiles of mortality" = 12)
  ) |>
  kableExtra::kable_classic()

table1

## 表の保存 --------------------------------------------------------------------

table1 |>
  kableExtra::save_kable(
    "figuretable/gdp_table1.png"
  )


# 表2の再現 -------------------------------------------------------------------


## データ読み込み -----------------------------------------------------------------

table2_df <- foreign::read.dta("./input/day4/maketable2.dta")


## 変数セットの定義 -------------------------------------------------------------------

table2_xvars <- c("avexpr", "lat_abst", "africa", "asia", "other")
table2_yvars <- c("logpgp95", "loghjypl")


## 1列目 ---------------------------------------------------------------------


### モデル作成 -------------------------------------------------------------------

xvars_col1 <- paste(table2_xvars[1], collapse = " + ")
model_col1 <- paste(table2_yvars[1], xvars_col1, sep = " ~ ")
model_col1 <- as.formula(model_col1)


### 回帰 ----------------------------------------------------------------------

table2_col1 <- 
  table2_df |>
  estimatr::lm_robust(formula = model_col1, se_type = "stata")


## 2列目 ---------------------------------------------------------------------

### 回帰 ----------------------------------------------------------------------
table2_col2 <- 
  table2_df |>
  dplyr::filter(baseco==1) |>
  estimatr::lm_robust(formula = model_col1, se_type = "stata")

## 3列目 ---------------------------------------------------------------------

### モデル作成 -------------------------------------------------------------------

xvars_col3 <- paste(table2_xvars[1:2], collapse = " + ")
model_col3 <- paste(table2_yvars[1], xvars_col3, sep = " ~ ")
model_col3 <- as.formula(model_col3)

### 回帰 ----------------------------------------------------------------------

table2_col3 <- 
  table2_df |>
  estimatr::lm_robust(formula = model_col3, se_type = "stata")

## 4列目 ---------------------------------------------------------------------

### モデル作成 -------------------------------------------------------------------

xvars_col4 <- paste(table2_xvars, collapse = " + ")
model_col4 <- paste(table2_yvars[1], xvars_col4, sep = " ~ ")
model_col4 <- as.formula(model_col4)

### 回帰 ----------------------------------------------------------------------

table2_col4 <- 
  table2_df |>
  estimatr::lm_robust(formula = model_col4, se_type = "stata")

## 5列目 ---------------------------------------------------------------------

### 回帰 ----------------------------------------------------------------------

table2_col5 <- 
  table2_df |>
  dplyr::filter(baseco==1) |>
  estimatr::lm_robust(formula = model_col3, se_type = "stata")

## 6列目 ---------------------------------------------------------------------

### 回帰 ----------------------------------------------------------------------

table2_col6 <- 
  table2_df |>
  dplyr::filter(baseco==1) |>
  estimatr::lm_robust(formula = model_col4, se_type = "stata")


## 7列目 --------------------------------------------------------------


### モデル作成 -------------------------------------------------------------------
model_col7 <- paste(table2_yvars[2], xvars_col1, sep = " ~ ")
model_col7 <- as.formula(model_col7)


### 回帰 ----------------------------------------------------------------------
table2_col7 <- 
  table2_df |>
  estimatr::lm_robust(formula = model_col7, se_type = "stata")

## 8列目 --------------------------------------------------------------

### 回帰 ----------------------------------------------------------------------

table2_col8 <- 
  table2_df |>
  dplyr::filter(baseco==1) |>
  estimatr::lm_robust(formula = model_col7, se_type = "stata")


## 結果のまとめ -------------------------------------------------------------------


### 変数名の作成 ------------------------------------------------------------------

table2_coef_map <-
  c(
    "avexpr" = "Average protection \n against expropriation risk,\n 1985 ~ 1995",
    "lat_abst" = "Latitude",
    "asia" = "Asia dummy",
    "africa" = "Africa dummy",
    "other" = "Other continent dummy"
  )


### 挿入行の作成 ------------------------------------------------------------------

table2_rows <- data.frame(
  "", 
  "Dependent variable is log GDP per capita in 1995", 
  "", "", "", "", "", 
  "Dependent variable is log output per worker in 1988",
  ""
)
attr(table2_rows, 'position') <- c(1)



### 表作成 --------------------------------------------------------------------
table2 <- modelsummary::modelsummary(
  list(
    "Whole world (1)" = table2_col1,     "Base sample (2)" = table2_col2,     "Whole world (3)" = table2_col3,     "Whole world (4)" = table2_col4,
    "Base sample (5)" = table2_col5,    "Base sample (6)" = table2_col6,    "Whole world (7)" = table2_col7,    "Base sample (8)" = table2_col8
  ),
  coef_map = table2_coef_map,
  gof_map = c(    "r.squared",    "nobs"  ),
  add_rows = table2_rows,
  title = paste0("OLS Regressions"),
  fmt = 2
)
table2


### 表保存 -------------------------------------------------------------------

table2 <- modelsummary::modelsummary(
  list(
    "Whole world (1)" = table2_col1, 
    "Base sample (2)" = table2_col2, 
    "Whole world (3)" = table2_col3, 
    "Whole world (4)" = table2_col4,
    "Base sample (5)" = table2_col5,
    "Base sample (6)" = table2_col6,
    "Whole world (7)" = table2_col7,
    "Base sample (8)" = table2_col8
  ),
  coef_map = table2_coef_map,
  gof_map = c(
    "r.squared",
    "nobs"
  ),
  add_rows = table2_rows,
  title = paste0("OLS Regressions"),
  output = "kableExtra"
) |> 
  kableExtra::kable_classic() |>
  kableExtra::save_kable(
    "figuretable/gdp_table2.png"
  )



# 図2 ----------------------------------------------------------------------

figure2 <- table2_df |> 
  dplyr::filter(baseco==1) |>  
  ggplot2::ggplot(ggplot2::aes(x = avexpr , y = logpgp95, label = shortnam)) + 
  ggplot2::geom_text() + 
  ggplot2::stat_smooth(formula = "y ~ x", method = "lm", se = FALSE, colour = "black") + 
  ggplot2::xlab("Average Expropritation Risk 1985 ~ 95") +  
  ggplot2::ylab("log GDP per capita, PPP, in 1995") + 
  ggplot2::ylim(4, 10) +
  ggplot2::theme_classic()

figure2

figure2 |>
  ggplot2::ggsave(
    file = "./figuretable/gdp_figure2.png",
    width = 8,
    height = 6,
    unit = "in",
    dpi = "retina"
    )

# 図3 ----------------------------------------------------------------------


## データ読み込み ----------------------------------------------------------------

table3_df <- foreign::read.dta("./input/day4/maketable3.dta") |> 
  dplyr::filter(excolony==1&!is.na(extmort4)) |> 
  dplyr::mutate(euro1900 = euro1900 / 100)


## 作図 ----------------------------------------------------------------------

figure3 <- table3_df |> 
  ggplot2::ggplot(ggplot2::aes(x = logem4 , y = avexpr)) + 
  ggplot2::geom_point() +
  ggplot2::stat_smooth(formula = "y ~ x", method = "lm", se = FALSE, colour = "black") + 
  ggplot2::xlab("Log European settler mortality") + 
  ggplot2::ylab("Average Expropritation Risk 1985 ~ 95") +
  ggplot2::theme_classic()

figure3

figure3 |>
  ggplot2::ggsave(
    file = "./figuretable/gdp_figure3.png",
    width = 8,
    height = 6,
    unit = "in",
    dpi = "retina"
  )


# 表3の再現 -------------------------------------------------------------------


## パネルA --------------------------------------------------------------------

table3_col_odd <- list()
table3_col_even <-list()

### 4変数のループ ---------------------------------------------------------------

for (xvar in c("cons00a", "democ00a", "indtime + cons1", "euro1900")) { 
  xvars_col_odd <- paste(xvar, collapse = " + ") 
  model_col_odd <- paste("avexpr", xvars_col_odd, sep = " ~ ") 
  model_col_odd <- as.formula(model_col_odd) 
  table3_col_odd[[xvar]] <- lm(data = table3_df, formula = model_col_odd) 
  xvars_col_even <- paste(c(xvar, "lat_abst"), collapse = " + ") 
  model_col_even <- paste("avexpr", xvars_col_even, sep = " ~ ") 
  model_col_even <- as.formula(model_col_even) 
  table3_col_even[[xvar]] <- lm(data = table3_df, formula = model_col_even)  
  }


### 5変数目 --------------------------------------------------------------------

xvars_col_odd <- paste("logem4", collapse = " + ")
model_col_odd <- paste("avexpr", xvars_col_odd, sep = " ~ ")
model_col_odd <- as.formula(model_col_odd)
table3_col_odd[[5]] <- 
  table3_df |>
  dplyr::filter(!is.na(logpgp95)) |>
  lm(formula = model_col_odd)

xvars_col_even <- paste(c("logem4", "lat_abst"), collapse = " + ")
model_col_even <- paste("avexpr", xvars_col_even, sep = " ~ ")
model_col_even <- as.formula(model_col_even)
table3_col_even[[5]] <- 
  table3_df |>
  dplyr::filter(!is.na(logpgp95)) |>
  lm(formula = model_col_even)



### 変数名の作成 ------------------------------------------------------------------
table3_coef_map = c(
  "cons00a" = "Constraint on executive in\n 1900",
  "democ00a" = "Democracy in 1900", 
  "cons1" = "Constraint on executive in first\n year of independence", 
  "euro1900" = "European settlements in 1900",
  "logem4" = "Log European settler mortality",
  "lat_abst" = "Latitude"
)

### 表作成 --------------------------------------------------------------------

table3_a <- modelsummary::modelsummary(
  list(
    table3_col_odd[[1]], table3_col_even[[1]],
    table3_col_odd[[2]], table3_col_even[[2]],
    table3_col_odd[[3]], table3_col_even[[3]],
    table3_col_odd[[4]], table3_col_even[[4]],
    table3_col_odd[[5]], table3_col_even[[5]]
  ),
  coef_map = table3_coef_map,
  gof_map = c("r.squared", "nobs"),
  title = paste0(
    "Determinents of Institutions /n",
    "Panel A: Dependent Variable Is Average Protection Against Expropriation Risk in 1985 to 1995"
  )
)

table3_a

### 表保存 -------------------------------------------------------------------
table3_a <- modelsummary::modelsummary(
  list(
    table3_col_odd[[1]], table3_col_even[[1]],
    table3_col_odd[[2]], table3_col_even[[2]],
    table3_col_odd[[3]], table3_col_even[[3]],
    table3_col_odd[[4]], table3_col_even[[4]],
    table3_col_odd[[5]], table3_col_even[[5]]
  ),
  coef_map = table3_coef_map,
  gof_map = c("r.squared", "nobs"),
  title = paste0(
    "Determinents of Institutions /n",
    "Panel A: Dependent Variable Is Average Protection Against Expropriation Risk in 1985 to 1995"
  ),
  output = "kableExtra"
) |> 
  kableExtra::kable_classic() |>
  kableExtra::save_kable(
    "figuretable/gdp_table3a.png"
  )


## 表3b ----------------------------------------------------------------------


### モデルの定義 ------------------------------------------------------------------

model_col <- c(
  "cons00a ~ euro1900",
  "cons00a ~ euro1900 + lat_abst",
  "cons00a ~ logem4",
  "cons00a ~ logem4 + lat_abst",
  "democ00a ~ euro1900",
  "democ00a ~ euro1900 + lat_abst",
  "democ00a ~ logem4",
  "democ00a ~ logem4 + lat_abst",
  "euro1900 ~ logem4",
  "euro1900 ~ logem4 + lat_abst"
)



### 回帰 ----------------------------------------------------------------------

#### 1,2列目 -------------------------------------------------------------------

table3_panel_b <- list()
for (i in 1:2) {
  table3_panel_b[[i]] <- 
    estimatr::lm_robust(
      data = table3_df |> 
        dplyr::filter(
          !is.na(logpgp95)
        ), 
      formula = as.formula(
        model_col[i]
      ),
      se_type = "stata"
    )
}

#### 3,4列目 -------------------------------------------------------------------

for (i in 3:4) {
  table3_panel_b[[i]] <- 
    estimatr::lm_robust(
      data = table3_df, 
      formula = as.formula(
        model_col[i]
      ), 
      se_type = "stata"
    )
}

#### 5-10列目 ------------------------------------------------------------------

for (i in 5:10) {
  table3_panel_b[[i]] <- 
    estimatr::lm_robust(
      data = table3_df |> 
        dplyr::filter(
          !is.na(logpgp95)
        ),
      formula = as.formula(
        model_col[i]
      ),
      se_type = "stata"
    )
}

### 変数名の作成 ------------------------------------------------------------------

table3b_coef_map <-c(
  "euro1900" = "European settlements in 1900",
  "logem4" = "Log European settler mortality", 
  "lat_abst" = "Latitude"
)

### 挿入行の作成 ------------------------------------------------------------------

table3b_rows <- data.frame(
  "Panel B", 
  "Dependent Variable Is Constraint\n on Executive in 1900",
  "", "", "",
  "Dependent Variable Is\n Democracy in 1900",
  "", "", "", 
  "Dependent Variable Is\n European Settlements\n in 1900",
  ""
)

attr(table3b_rows, 'position') <- c(1)


### 表作成 --------------------------------------------------------------------
table3_b <- modelsummary::modelsummary(
  table3_panel_b,
  coef_map = table3b_coef_map,
  add_rows = table3b_rows,
  gof_map = c("r.squared", "nobs"),
  title = "Determinents of Institutions"
)

table3_b

### 表保存 -------------------------------------------------------------------

modelsummary::modelsummary(
  table3_panel_b,
  coef_map = table3b_coef_map,
  add_rows = table3b_rows,
  gof_map = c("r.squared", "nobs"),
  title = "Determinents of Institutions",
  output = "kableExtra"
) |> 
  kableExtra::kable_classic() |>
  kableExtra::save_kable(
    "figuretable/gdp_table3b.png"
  )



# 表4 ----------------------------------------------------------------------


## データ読み込み -----------------------------------------------------------------

table4_df <- foreign::read.dta("./input/day4/maketable4.dta") |>
  dplyr::filter(baseco==1) |>
  dplyr::mutate(
    other_cont = ifelse(
      shortnam=="AUS" | shortnam=="MLT" | shortnam=="NZL",
      1, 
      0
    )
  )



## パネルa --------------------------------------------------------------------

### 回帰 ---------------------------------------------------------------
table4_a_col1 <- 
  AER::ivreg(
    formula = "logpgp95 ~ avexpr | logem4",
    data = table4_df
  )

table4_a_col2 <- 
  AER::ivreg(
    formula = "logpgp95 ~ avexpr + lat_abst| logem4 + lat_abst", 
    data = table4_df
  )

table4_a_col3 <- 
  AER::ivreg(
    formula = "logpgp95 ~ avexpr | logem4", 
    data = table4_df |> 
      dplyr::filter(rich4 != 1)
  )

table4_a_col4 <- 
  AER::ivreg(
    formula = "logpgp95 ~ avexpr + lat_abst| logem4 + lat_abst", 
    data = table4_df |> 
      dplyr::filter(rich4!= 1)
  )

table4_a_col5 <- 
  AER::ivreg(
    formula = "logpgp95 ~ avexpr | logem4", 
    data = table4_df |> 
      dplyr::filter(africa != 1)
  )

table4_a_col6 <- 
  AER::ivreg(
    formula = "logpgp95 ~ avexpr + lat_abst| logem4 + lat_abst", 
    data = table4_df |> 
      dplyr::filter(africa != 1)
  )

table4_a_col7 <- 
  AER::ivreg(
    formula = "logpgp95 ~ avexpr + asia + africa + other_cont | logem4 + africa + asia + other_cont",
    data = table4_df)

table4_a_col8 <- 
  AER::ivreg(
    formula = "logpgp95 ~ avexpr + lat_abst + asia + africa + other_cont | logem4 + lat_abst + africa + asia + other_cont", 
    data = table4_df
  )

table4_a_col9 <- 
  AER::ivreg(
    formula = "loghjypl ~ avexpr | logem4", 
    data = table4_df
  )


### 変数名の作成 ------------------------------------------------------------------

table4_a_coef_map <-
  c(
    "avexpr" = "Average protection against\n expropriation risk 1985–1995",
    "lat_abst" = "Latitude",
    "asia" = "Asia dummy",
    "africa" = "Africa dummy",
    "other_cont" = "Other continent dummy"
  )


### 表作成 --------------------------------------------------------------------

table4_a <- modelsummary::modelsummary(
  list(
    "Base sample" = table4_a_col1, 
    table4_a_col2, 
    "Base sample\n without Neo-Europes" = table4_a_col3, 
    table4_a_col4, 
    "Base sample\n without Africa" = table4_a_col5, 
    table4_a_col6, 
    "Base sample\n with continent dummies" = table4_a_col7, 
    table4_a_col8, 
    "Base sample of \n log output per worker" = table4_a_col9
  ), 
  coef_map = table4_a_coef_map,
  gof_map = c("r.squared", "nobs"),
  title = "IV REGRESSIONS OF LOG GDP PER CAPITA"
)
table4_a

### 表保存 -------------------------------------------------------------------
modelsummary::modelsummary(
  list(
    "Base sample" = table4_a_col1, 
    table4_a_col2, 
    "Base sample\n without Neo-Europes" = table4_a_col3, 
    table4_a_col4, 
    "Base sample\n without Africa" = table4_a_col5, 
    table4_a_col6, 
    "Base sample\n with continent dummies" = table4_a_col7, 
    table4_a_col8, 
    "Base sample of \n log output per worker" = table4_a_col9
  ), 
  coef_map = table4_a_coef_map,
  gof_map = c("r.squared", "nobs"),
  title = "IV REGRESSIONS OF LOG GDP PER CAPITA",
  output = "kableExtra"
) |> 
  kableExtra::kable_classic() |>
  kableExtra::save_kable(
    "figuretable/gdp_table4a.png"
  )

## パネルb ----------------------------------------------------------------

### 回帰 -------------------------------------------------------
table4_b_col1 <- 
  lm(
    formula = "avexpr ~ logem4", 
    data = table4_df
  )

table4_b_col2 <- 
  lm(
    formula = "avexpr ~ logem4 + lat_abst",
    data = table4_df
  )

table4_b_col3 <- 
  lm(
    formula = "avexpr ~ logem4", 
    data = table4_df |> 
      dplyr::filter(rich4 != 1)
  )

table4_b_col4 <- 
  lm(
    formula = "avexpr ~ logem4 + lat_abst", 
    data = table4_df |> 
      dplyr::filter(rich4 != 1)
  )

table4_b_col5 <- 
  lm(
    formula = "avexpr ~ logem4", 
    data = table4_df |> 
      dplyr::filter(africa != 1)
  )

table4_b_col6 <- 
  lm(
    formula = "avexpr ~ logem4 + lat_abst", 
    data = table4_df |> 
      dplyr::filter(africa != 1)
  )

table4_b_col7 <- 
  lm(
    formula = "avexpr ~ logem4 + asia + africa + other_cont", 
    data = table4_df
  )

table4_b_col8 <- 
  lm(
    formula = "avexpr ~ logem4 + lat_abst + asia + africa + other_cont", 
    data = table4_df
  )

table4_b_col9 <- 
  lm(
    formula = "avexpr ~ logem4", 
    data = table4_df
  )

### 変数名の作成 ------------------------------------------------------------------
table4_b_coef_map = c(
  "logem4" = "Log European settler mortality",
  "lat_abst" = "Latitude",
  "asia" = "Asia dummy",
  "africa" = "Africa dummy",
  "other_cont" = "Other continent dummy"
)


### 表作成 --------------------------------------------------------------------
table4_b <- modelsummary::modelsummary(
  list(
    "Base sample" = table4_b_col1, 
    table4_b_col2, 
    "Base sample\n without Neo-Europes" = table4_b_col3, 
    table4_b_col4, 
    "Base sample\n without Africa" = table4_b_col5, 
    table4_b_col6, 
    "Base sample\n with continent dummies" = table4_b_col7, 
    table4_b_col8, 
    "Base sample of \n log output per worker" = table4_b_col9
  ), 
  coef_map = table4_b_coef_map,
  gof_map = c("r.squared", "F"),
  title = "Panel B: First Stage for Average Protection Against Expropriation Risk in 1985 ~ 1995"
)

table4_b

### 表保存 -------------------------------------------------------------------
table4_b <- modelsummary::modelsummary(
  list(
    "Base sample" = table4_b_col1, 
    table4_b_col2, 
    "Base sample\n without Neo-Europes" = table4_b_col3, 
    table4_b_col4, 
    "Base sample\n without Africa" = table4_b_col5, 
    table4_b_col6, 
    "Base sample\n with continent dummies" = table4_b_col7, 
    table4_b_col8, 
    "Base sample of \n log output per worker" = table4_b_col9
  ), 
  coef_map = table4_b_coef_map,
  gof_map = c("r.squared", "F"),
  title = "Panel B: First Stage for Average Protection Against Expropriation Risk in 1985 ~ 1995",
  output = "kableExtra"
) |> 
  kableExtra::kable_classic() |>
  kableExtra::save_kable(
    "figuretable/gdp_table4b.png"
  )


## パネルc ----------------------------------------------------------------

### 回帰 -------------------------------------------------------
table4_c_col1 <- 
  lm(
    formula = "logpgp95 ~ avexpr", 
    data = table4_df
  )

table4_c_col2 <- 
  lm(
    formula = "logpgp95 ~ avexpr + lat_abst", 
    data = table4_df
  )

table4_c_col3 <- 
  lm(
    formula = "logpgp95 ~ avexpr", 
    data = table4_df |> 
      dplyr::filter(rich4 != 1)
  )

table4_c_col4 <- 
  lm(
    formula = "logpgp95 ~ avexpr + lat_abst", 
    data = table4_df |> 
      dplyr::filter(rich4 != 1)
  )

table4_c_col5 <- 
  lm(
    formula = "logpgp95 ~ avexpr", 
    data = table4_df |> 
      dplyr::filter(africa != 1)
  )

table4_c_col6 <- 
  lm(
    formula = "logpgp95 ~ avexpr + lat_abst", 
    data = table4_df |> 
      dplyr::filter(africa != 1)
  )

table4_c_col7 <- 
  lm(
    formula = "logpgp95 ~ avexpr + asia + africa + other_cont", 
    data = table4_df
  )

table4_c_col8 <- 
  lm(
    formula = "logpgp95 ~ avexpr + lat_abst + asia + africa + other_cont", 
    data = table4_df
  )

table4_c_col9 <- 
  lm(
    formula = "loghjypl ~ avexpr", 
    data = table4_df
  )

### 変数名の作成 ------------------------------------------------------------------

table4_b_coef_map <- c(
  "avexpr" = "Average protection against\n expropriation risk 1985 ~ 1995"
)

### 表作成 --------------------------------------------------------------------

table4_c <- modelsummary::modelsummary(
  list(
    "Base sample" = table4_c_col1, 
    table4_c_col2, 
    "Base sample\n without Neo-Europes" = table4_c_col3, 
    table4_c_col4, 
    "Base sample\n without Africa" = table4_c_col5, 
    table4_c_col6, 
    "Base sample\n with continent dummies" = table4_c_col7, 
    table4_c_col8, 
    "Base sample of \n log output per worker" = table4_c_col9
  ), 
  coef_map = table4_b_coef_map,
  gof_map = c("nobs")
)

table4_c

### 表保存 -------------------------------------------------------------------
table4_c <- modelsummary::modelsummary(
  list(
    "Base sample" = table4_c_col1, 
    table4_c_col2, 
    "Base sample\n without Neo-Europes" = table4_c_col3, 
    table4_c_col4, 
    "Base sample\n without Africa" = table4_c_col5, 
    table4_c_col6, 
    "Base sample\n with continent dummies" = table4_c_col7, 
    table4_c_col8, 
    "Base sample of \n log output per worker" = table4_c_col9
  ), 
  coef_map = table4_b_coef_map,
  gof_map = c("nobs"),
  output = "kableExtra"
) |> 
  kableExtra::kable_classic() |>
  kableExtra::save_kable(
    "figuretable/gdp_table4c.png"
  )







