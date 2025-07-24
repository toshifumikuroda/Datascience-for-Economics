# ビール税と交通事故死亡者数 -----------------------------------------------------------
## 初期化とデータの読み込み ------------------------------------------------------------
rm(list = ls())

library(AER)
data(Fatalities)

## subset 1982,1988 ------------------------------------------------------
Fatalities$fatal_rate <- 
  Fatalities$fatal / Fatalities$pop * 10000

Fatalities1982 <- 
  Fatalities |>
  dplyr::filter(year == "1982")

Fatalities1988 <- 
  Fatalities |>
  dplyr::filter(year == "1988") 

## クロスセクションols -----------------------------------------------------------
fatal1982_mod <- 
  lm(fatal_rate ~ beertax, data = Fatalities1982)
coeftest(fatal1982_mod, vcov. = vcovHC, type = "HC1")

fatal1988_mod <- 
  lm(fatal_rate ~ beertax, data = Fatalities1988)
coeftest(fatal1988_mod, vcov. = vcovHC, type = "HC1")

## ビール税と交通事故死の図 ----------------------------------------------
### 1982 -------------------------------------------------------------------
plot(x = Fatalities1982$beertax, 
     y = Fatalities1982$fatal_rate, 
     xlab = "Beer tax (in 1982 dollars)",
     ylab = "Fatality rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1982",
     ylim = c(0, 4.5),
     pch = 20, 
     col = "steelblue")
abline(fatal1982_mod, lwd = 1.5)

### 1988 -------------------------------------------------------------------
plot(x = Fatalities1988$beertax, 
     y = Fatalities1988$fatal_rate, 
     xlab = "Beer tax (in 1988 dollars)",
     ylab = "Fatality rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1988",
     ylim = c(0, 4.5),
     pch = 20, 
     col = "steelblue")
abline(fatal1988_mod, lwd = 1.5)

## 差分ols -----------------------------------------------------------------
diff_fatal_rate <- 
  Fatalities1988$fatal_rate - Fatalities1982$fatal_rate

diff_beertax <- 
  Fatalities1988$beertax - Fatalities1982$beertax

fatal_diff_mod <- 
  lm(diff_fatal_rate ~ diff_beertax)

coeftest(fatal_diff_mod, vcov = vcovHC, type = "HC1")


## 差分プロット ------------------------------------------------------------------
plot(x = diff_beertax, 
     y = diff_fatal_rate, 
     xlab = "Change in beer tax (in 1988 dollars)",
     ylab = "Change in fatality rate (fatalities per 10000)",
     main = "Changes in Traffic Fatality Rates and Beer Taxes in 1982-1988",
     xlim = c(-0.6, 0.6),
     ylim = c(-1.5, 1),
     pch = 20, 
     col = "steelblue")
abline(fatal_diff_mod, lwd = 1.5)


# 教育と賃金 ---------------------------------------------------------

## データの読み込みと並び替え -------------------------------------------------
data("CPSSWEducation")

CPSSWEducation <-
  CPSSWEducation |>
  dplyr::arrange(
    education
  )

## 教育年数と所得の関係をプロット ---------------------------------------
labor_model <- 
  lm(earnings ~ education, data = CPSSWEducation)

plot(CPSSWEducation$education, CPSSWEducation$earnings,  ylim = c(0, 150))

abline(labor_model, col = "steelblue", lwd = 2)

summary(labor_model)


## 箱ひげ図の追加 ----------------------------------------------------------------- 
boxplot(
  formula = earnings ~ education, 
  at = c(intersect(
    CPSSWEducation$education,
    CPSSWEducation$education)
  ),
  add = TRUE, 
  border = "black", 
  data = CPSSWEducation
)

## 不均一分散に頑健な標準誤差 ------------------------------------------------------------
coeftest(labor_model)

vcov_HC0 <- vcovHC(labor_model, type = "HC0")

coeftest(labor_model, vcov. = vcov_HC0)

vcov_HC1 <- vcovHC(labor_model, type = "HC1")

coeftest(labor_model, vcov. = vcov_HC1) 



## ダミー変数の作成 ----------------------------------------------------------------
Fatalities$drinkagec <- cut(Fatalities$drinkage, breaks = 18:22, include.lowest = TRUE, right = FALSE)
Fatalities$drinkagec <- relevel(Fatalities$drinkagec, "[21,22]")
Fatalities$punish <- with(Fatalities, factor(jail == "yes" | service == "yes", labels = c("no", "yes")))


# 誤差の比較 -------------------------------------------------------------------


fatalities_mod <- lm(fatal_rate ~ beertax, data = Fatalities)
fatalities_mod_i <- lm(fatal_rate ~ beertax + state, data = Fatalities)
fatalities_mod_it <- lm(fatal_rate ~ beertax + state + year, data = Fatalities)

fatalities_mod_hc <- estimatr::lm_robust(fatal_rate ~ beertax + state + year, data = Fatalities)
fatalities_mod_hac <- estimatr::lm_robust(fatal_rate ~ beertax + state + year, data = Fatalities, clusters = state)

fatalities_mod_hc2 <- estimatr::lm_robust(fatal_rate ~ beertax + state + year + drinkagec + punish + miles, data = Fatalities)
fatalities_mod_hac2 <- estimatr::lm_robust(fatal_rate ~ beertax + state + year + drinkagec + punish + miles, data = Fatalities, clusters = state)


## まとめ表 --------------------------------------------------------------------


modelsummary::modelsummary(
  title = "ビール税と死亡率",
  list(
    fatalities_mod, 
    fatalities_mod_i, 
    fatalities_mod_it, 
    fatalities_mod_hc, 
    fatalities_mod_hac, 
    fatalities_mod_hc2, 
    fatalities_mod_hac2
    ),
  gof_map = c("nobs", "r.squared"),
  statistic = "std.error"
)

# 回帰分析のまとめ表の整形 ------------------------------------------------------------ 
state_control <- c("State controls", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
year_control <- c("Year controls", "No", "No", "Yes", "Yes", "Yes", "Yes", "Yes")
se_table <- c("Robust Standard Error", "No", "No", "NO", "HC", "Cluster", "HC", "Cluster")
table_rows <- data.frame(rbind(state_control, year_control, se_table))
attr(table_rows, 'position') <- c(15,16,17)

modelsummary::modelsummary(
  title = "ビール税と死亡率",
  list(
    fatalities_mod, 
    fatalities_mod_i, 
    fatalities_mod_it, 
    fatalities_mod_hc, 
    fatalities_mod_hac, 
    fatalities_mod_hc2, 
    fatalities_mod_hac2
  ),
  gof_map = c("nobs", "r.squared"),
  statistic = "std.error",
  coef_omit = "state*|year*",
  add_rows = table_rows
)
