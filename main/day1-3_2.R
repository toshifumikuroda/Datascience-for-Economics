
# ファイルの読み込み ---------------------------------------------------------------

timeseries <-  readr::read_csv(    file = "input/03_timeseries.csv"  )

# GDPのプロット ----------------------------------------------------------------
timeseries |>
  dplyr::mutate(GDP_real = GDP_real/1000) |>
  dplyr::mutate(GDP_nominal = GDP_nominal/1000) |>
  ggplot2::ggplot(ggplot2::aes(x = year)) +
  ggplot2::geom_line(ggplot2::aes(y = GDP_nominal, colour="blue"))+
  ggplot2::geom_line(ggplot2::aes(y = GDP_real, colour="red")) +
  ggplot2::xlab("Year") +  ggplot2::ylab("GDP (JPY billion)") +
  ggplot2::scale_color_discrete(name = "GDP", labels = c("Nominal", "Real")) +
  ggplot2::theme_classic()

# ロング型への変換 -------------------------------------------------------------
timeseries |>  
  tidyr::pivot_longer(cols = c(GDP_nominal, GDP_real), names_to = "Type", values_to = "GDP")

timeseries_long <- 
  timeseries |>  
  dplyr::mutate(GDP_real = GDP_real/1000) |> 
  dplyr::mutate(GDP_nominal = GDP_nominal/1000) |> 
  tidyr::pivot_longer(cols = c(GDP_nominal, GDP_real), names_to = "Type", values_to = "GDP")


## ロング型のプロット ---------------------------------------------------------------

figure_GDP_nominal_2 <-  
  timeseries_long |> 
  ggplot2::ggplot(ggplot2::aes(x = year, y = GDP, colour = Type)) + 
  ggplot2::geom_line() + 
  ggplot2::xlab("Year") + 
  ggplot2::ylab("GDP (JPY billion)") +  
  ggplot2::theme_classic()

figure_GDP_nominal_2

# GDPデフレータを算出する ----------------------------------------
timeseries <- timeseries |> 
  dplyr::mutate(gdp_deflator = GDP_nominal / GDP_real * 100)

# 物価のプロット ----------------------------------------------------------------
timeseries |>
  ggplot2::ggplot(ggplot2::aes(x = year)) +
  ggplot2::geom_line(ggplot2::aes(y = gdp_deflator, colour="blue"))+
  ggplot2::geom_line(ggplot2::aes(y = CPI, colour="red")) +
  ggplot2::xlab("Year") +  ggplot2::ylab("Price") +
  ggplot2::scale_color_discrete(name = "物価指数", labels = c("GDPデフレータ", "消費者物価指数")) +
  ggplot2::theme_classic()


# 物価のプロット ----------------------------------------------------------------
timeseries |>
  ggplot2::ggplot(ggplot2::aes(x = year)) +
  ggplot2::geom_line(ggplot2::aes(y = Unemployment_rate, colour="red")) +
  ggplot2::xlab("Year") +  ggplot2::ylab("失業率") +
  ggplot2::scale_color_discrete(name = "失業率", labels = c("失業率")) +
  ggplot2::theme_classic()

# インフレ率の作成 -------------------------------------------------------------
timeseries <-
  timeseries |> 
  dplyr::mutate(inflation_rate_gdpdef = (gdp_deflator/ dplyr::lag(gdp_deflator)-1)) |> 
  dplyr::mutate(inflation_rate_cpi = (CPI/ dplyr::lag(CPI)-1))

# インフレ率のプロット ----------------------------------------------------------
timeseries |> 
  ggplot2::ggplot(ggplot2::aes(x = GDP_gap,  y = inflation_rate_gdpdef)) + 
  ggplot2::geom_point() + 
  ggplot2::xlab("GDP Gap") + 
  ggplot2::ylab("Inflation(GDP Deflator)") + 
  ggplot2::theme_classic()

# geom_pathによる推移のプロット ----------------------------------------------

## GDPギャップとGDPデフレータ --------------------------------------------------------


timeseries |> 
  ggplot2::ggplot(ggplot2::aes(x = GDP_gap,  y = inflation_rate_gdpdef)) + 
  ggplot2::geom_path() + 
  ggplot2::xlab("GDP Gap") + 
  ggplot2::ylab("Inflation(GDP Deflator)") + 
  ggplot2::theme_classic()



## GDPデフレータとCPI ------------------------------------------------------------


timeseries |> 
  ggplot2::ggplot(ggplot2::aes(x = GDP_gap,  y = inflation_rate_cpi)) + 
  ggplot2::geom_path() + 
  ggplot2::xlab("GDP Gap") + 
  ggplot2::ylab("Inflation(CPI)") + 
  ggplot2::theme_classic()

## 失業率とGDPデフレータ --------------------------------------------------------


timeseries |> 
  ggplot2::ggplot(ggplot2::aes(x = Unemployment_rate,  y = inflation_rate_gdpdef)) + 
  ggplot2::geom_path() + 
  ggplot2::xlab("失業率") + 
  ggplot2::ylab("Inflation(GDP Deflator)") + 
  ggplot2::theme_classic()



## 失業率とCPI ------------------------------------------------------------


timeseries |> 
  ggplot2::ggplot(ggplot2::aes(x = Unemployment_rate,  y = inflation_rate_cpi)) + 
  ggplot2::geom_path() + 
  ggplot2::xlab("失業率") + 
  ggplot2::ylab("Inflation(CPI)") + 
  ggplot2::theme_classic()

# 東経大データサイエンス ------------------------------------------------
timeseries |>
  dplyr::mutate(ages = format(as.integer(year/10)*10,3)) |>
  ggplot2::ggplot(ggplot2::aes(x = Unemployment_rate,  y = inflation_rate_cpi, colour = ages, shape= ages)) +
  ggplot2::geom_path() +  ggplot2::geom_point() +
  ggplot2::xlab("Unemployment Rate") + ggplot2::ylab("Inflation(CPI)") +
  ggplot2::theme_classic() +
  ggplot2::scale_color_manual(    breaks = c("1980", "1990", "2000", "2010"),    values=c("red", "blue", "green", "purple")    )

# フィリップス曲線の回帰 -------------------------------------------------------------

## GDPギャップとGDPデフレータ -----------------------------------------------------------------

phillips_ols_1 <- lm(inflation_rate_gdpdef ~ GDP_gap, data=timeseries)
phillips_ols_1$coefficients
phillips_ols_1$fitted.values
summary(phillips_ols_1)[["coefficients"]][, "t value"]


## GDPギャップとCPI -----------------------------------------------------------------

phillips_ols_2 <- lm(inflation_rate_cpi ~ GDP_gap, data=timeseries)
phillips_ols_2$coefficients
phillips_ols_2$fitted.values
summary(phillips_ols_2)[["coefficients"]][, "t value"]


## 失業率とGDPデフレータ -----------------------------------------------------------------

phillips_ols_3 <- lm(inflation_rate_gdpdef ~ Unemployment_rate, data=timeseries)
phillips_ols_3$coefficients
phillips_ols_3$fitted.values
summary(phillips_ols_3)[["coefficients"]][, "t value"]


## 失業率とCPI -----------------------------------------------------------------

phillips_ols_4 <- lm(inflation_rate_cpi ~ Unemployment_rate, data=timeseries)
phillips_ols_4$coefficients
phillips_ols_4$fitted.values
summary(phillips_ols_4)[["coefficients"]][, "t value"]

# フィリップス曲線のプロット ---------------------------------------------------
timeseries |>
  ggplot2::ggplot(ggplot2::aes(x = Unemployment_rate,  y = inflation_rate_cpi)) + 
  ggplot2::geom_point() + 
  ggplot2::stat_smooth(method = "lm", se = FALSE, colour = "red", size = 1) + 
  ggplot2::xlab("Unemployment Rate") + ggplot2::ylab("Inflation(CPI)") +  ggplot2::theme_classic()


# 系列相関 --------------------------------------------------------------------
timeseries$phillips_ols_1_residual <- NA
timeseries$phillips_ols_1_residual[which(!is.na(timeseries$inflation_rate_cpi))] <- phillips_ols_1$resid

timeseries |> 
  ggplot2::ggplot(ggplot2::aes(x = year,  y = phillips_ols_1_residual)) + 
  ggplot2::geom_path() + 
  ggplot2::xlab("Year") + ggplot2::ylab("OLS Residuals") +  ggplot2::theme_classic()

# 誤差の系列相関の検定 --------------------------------------------------
acf(na.omit(timeseries$phillips_ols_1_residual), lag.max = 10, plot = T)

phillips_nw_ols_1 <- sandwich::NeweyWest(phillips_ols_1, lag = 1, prewhite = F, adjust = T)
summary(phillips_nw_ols_1)
lmtest::coeftest(phillips_ols_1, vcov = phillips_nw_ols_1)
summary(phillips_ols_1)
ttest_1 <- lmtest::coeftest(phillips_ols_1, vcov = phillips_nw_ols_1)

phillips_nw_ols_2 <- sandwich::NeweyWest(phillips_ols_2, lag = 1, prewhite = F, adjust = T)
ttest_2 <- lmtest::coeftest(phillips_ols_2, vcov = phillips_nw_ols_2)

phillips_nw_ols_3 <- sandwich::NeweyWest(phillips_ols_3, lag = 1, prewhite = F, adjust = T)
ttest_3 <- lmtest::coeftest(phillips_ols_3, vcov = phillips_nw_ols_3)

phillips_nw_ols_4 <- sandwich::NeweyWest(phillips_ols_4, lag = 1, prewhite = F, adjust = T)
ttest_4 <- lmtest::coeftest(phillips_ols_4, vcov = phillips_nw_ols_4)


# 結果のまとめ表 -----------------------------------------------
results <- as.data.frame(rbind(ttest_1, ttest_2, ttest_3, ttest_4)) |>
  dplyr::filter(dplyr::row_number() %% 2 == 0)
row.names(results) <- c(
  "GDPギャップとGDPデフレータ", 
  "GDPギャップとCPI",
  "失業率とGDPデフレータ", 
  "失業率とCPI"
  )
results



