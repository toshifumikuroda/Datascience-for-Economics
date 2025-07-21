
# データの読み込み ----------------------------------------------------------------

timeseries <-
  readr::read_csv(
    file = "input/03_timeseries.csv"
  )

# GDPのプロット ----------------------------------------------------------------

timeseries |>  
  dplyr::mutate(gdp_real = gdp_real/1000) |> 
  dplyr::mutate(gdp_nominal = gdp_nominal/1000) |>
  ggplot2::ggplot() + 
  ggplot2::aes(x = year) +
  ggplot2::geom_line(ggplot2::aes(y = gdp_nominal, colour="blue"))+ 
  ggplot2::geom_line(ggplot2::aes(y = gdp_real, colour="red")) +
  ggplot2::xlab("年") +
  ggplot2::ylab("GDP (兆円)") + 
  ggplot2:: scale_color_discrete(name = "GDP", labels = c("名目", "実質")) +
  ggplot2:: theme_classic()


# ロング型への変換 ----------------------------------------------------------------

timeseries_long <- 
  timeseries |>  
  dplyr::mutate(gdp_real = gdp_real/1000) |> 
  dplyr::mutate(gdp_nominal = gdp_nominal/1000) |> 
  tidyr::pivot_longer(cols = c(gdp_nominal, gdp_real), names_to = "Type", values_to = "GDP")


# ロングデータのプロット -------------------------------------------------------------


figure_gdp_nominal_2 <-  
  timeseries_long |> 
  ggplot2::ggplot() + 
  ggplot2::aes(x = year, y = GDP, colour = Type) +
  ggplot2::geom_line() +  ggplot2::xlab("年") +
  ggplot2::ylab("GDP (兆円)") +
  ggplot2::theme_classic()

figure_gdp_nominal_2

## グラフ内の変数名を書き換える* ------------------------------------------------------------

figure_gdp_nominal_2 <-  
  timeseries_long |> 
  ggplot2::ggplot() + 
  ggplot2::aes(x = year, y = GDP, colour = Type) +
  ggplot2::geom_line() +
  ggplot2::xlab("年") +
  ggplot2::ylab("GDP (兆円)") +
  ggplot2::scale_colour_manual(
    name = NULL,
    values = c("gdp_real" = "blue", "gdp_nominal" = "red"),
    labels = c("gdp_real" = "実質", "gdp_nominal" = "名目")
  ) +
  ggplot2::theme_classic()

figure_gdp_nominal_2




# GDPデフレータを算出する -----------------------------------------------------------
timeseries <- timeseries |> 
  dplyr::mutate(gdp_deflator = gdp_nominal / gdp_real * 100)


# GDPデフレータとCPIのプロット --------------------------------------------------

timeseries_gdp_cpi <- 
  timeseries |>  
  tidyr::pivot_longer(cols = c(gdp_deflator, cpi), names_to = "type", values_to = "price") |>
  ggplot2::ggplot() + 
  ggplot2::aes(x = year, y = price, colour = type) +
  ggplot2::geom_line() +  ggplot2::xlab("年") +  
  ggplot2::ylab("物価") +  
  ggplot2::scale_colour_manual(
    name = NULL,
    values = c("gdp_deflator" = "blue", "cpi" = "red"),
    labels = c("gdp_deflator" = "GDPデーフレータ", "cpi" = "消費者物価指数")
  ) +
  ggplot2::theme_classic()

timeseries_gdp_cpi

# 失業率のプロット ------------------------------------------------------------

timeseries_unemployment <- 
  timeseries |>  
  ggplot2::ggplot() + 
  ggplot2::aes(x = year, y = unemployment_rate) +
  ggplot2::geom_line() +
  ggplot2::xlab("年") +  
  ggplot2::ylab("完全失業率") +  
  ggplot2::theme_classic()

timeseries_unemployment


# インフレ率の作成 ----------------------------------------------------------------

timeseries <- 
  timeseries |> 
  dplyr::mutate(inflation_rate_gdpdef = (gdp_deflator/ dplyr::lag(gdp_deflator)-1)) |> 
  dplyr::mutate(inflation_rate_cpi = (cpi/ dplyr::lag(cpi)-1))


# インフレ率のプロット --------------------------------------------------------------

timeseries |> 
  ggplot2::ggplot(ggplot2::aes(x = gdp_gap,  y = inflation_rate_gdpdef)) + 
  ggplot2::geom_point() + 
  ggplot2::xlab("GDPギャップ") + 
  ggplot2::ylab("インフレ率(GDPデフレータ)") + 
  ggplot2::theme_classic()


# geom_pathによる推移のプロット -----------------------------------------------------

timeseries |> 
  ggplot2::ggplot(ggplot2::aes(x = gdp_gap,  y = inflation_rate_gdpdef)) + 
  ggplot2::geom_path() + 
  ggplot2::xlab("GDPギャップ") + 
  ggplot2::ylab("インフレ率(GDPデフレータ)") + 
  ggplot2::theme_classic()



# 東経大データサイエンス -------------------------------------------------------------

timeseries |>
  dplyr::mutate(ages = format(as.integer(year/10)*10,3)) |> 
  ggplot2::ggplot(ggplot2::aes(x = unemployment_rate,  y = inflation_rate_cpi, colour = ages)) + 
  ggplot2::geom_path() +
  ggplot2::xlab("失業率") + 
  ggplot2::ylab("インフレ率(CPI)") + 
  ggplot2::theme_classic() + 
  ggplot2::scale_color_manual(
    name = "年代",
    breaks = c("1980", "1990", "2000", "2010"),
    values=c("red", "blue", "green", "purple")
  )

# フィリップス曲線の回帰 -------------------------------------------------------------

phillips_ols_1 <- lm(inflation_rate_cpi ~ unemployment_rate, data=timeseries)
phillips_ols_1
phillips_ols_1$coefficients
phillips_ols_1$fitted.values
summary(phillips_ols_1)[["coefficients"]][, "t value"]


## 変数を入れ換えたバージョンの作成 -------------------------------------------------------

phillips_ols_2 <- lm(inflation_rate_gdpdef ~ unemployment_rate, data=timeseries)
phillips_ols_3 <- lm(inflation_rate_cpi ~ gdp_gap, data=timeseries)
phillips_ols_4 <- lm(inflation_rate_gdpdef ~ gdp_gap, data=timeseries)


# フィリップス曲線のプロット -----------------------------------------------------------

timeseries |>
  ggplot2::ggplot(ggplot2::aes(x = unemployment_rate,  y = inflation_rate_cpi)) + 
  ggplot2::geom_point() + 
  ggplot2::stat_smooth(method = "lm", se = FALSE, colour = "red", size = 1) + 
  ggplot2::xlab("失業率") + ggplot2::ylab("インフレ率(CPI)") +  ggplot2::theme_classic()



# 系列相関 --------------------------------------------------------------------
timeseries$phillips_ols_1_residual <- NA
timeseries$phillips_ols_1_residual[which(!is.na(timeseries$inflation_rate_cpi))] <- phillips_ols_1$resid
timeseries |> 
  ggplot2::ggplot(ggplot2::aes(x = year,  y = phillips_ols_1_residual)) + 
  ggplot2::geom_path() + 
  ggplot2::stat_smooth(method = "lm", se = FALSE, colour = "red", size = 1) + 
  ggplot2::xlab("年") + ggplot2::ylab("OLS残差") +  ggplot2::theme_classic()




# ホワイトノイズを作成 --------------------------------------------------------------

timeseries |>  
  dplyr::mutate(whitenoize=rnorm(dplyr::n(), mean = 0, sd = summary(phillips_ols_1)$sigma)) |> 
  ggplot2::ggplot(ggplot2::aes(x = year,  y = whitenoize))+ 
  ggplot2::geom_path(linetype="dashed") + 
  ggplot2::geom_path(ggplot2::aes(x = year,  y = phillips_ols_1_residual), colour = "red") + 
  ggplot2::geom_line(ggplot2::aes(x = year, y = 0)) + 
  ggplot2::xlab("年") + 
  ggplot2::ylab("OLS残差") +  
  ggplot2::theme_classic()


# 誤差の系列相関の検定 --------------------------------------------------------------
acf(na.omit(timeseries$phillips_ols_1_residual), lag.max = 10, plot = T)



# ライブラリの追加 ----------------------------------------------------------------


install.packages(c("sandwich","lmtest"))

# HAC標準誤差の推定 --------------------------------------------------------------

phillips_nw_ols_1 <- sandwich::NeweyWest(phillips_ols_1, lag = 1, prewhite = F, adjust = T)
lmtest::coeftest(phillips_ols_1, vcov = phillips_nw_ols_1)
ttest_1 <- lmtest::coeftest(phillips_ols_1, vcov = phillips_nw_ols_1)

phillips_nw_ols_2 <- sandwich::NeweyWest(phillips_ols_2, lag = 1, prewhite = F, adjust = T)
ttest_2 <- lmtest::coeftest(phillips_ols_2, vcov = phillips_nw_ols_2)

phillips_nw_ols_3 <- sandwich::NeweyWest(phillips_ols_3, lag = 1, prewhite = F, adjust = T)
ttest_3<- lmtest::coeftest(phillips_ols_3, vcov = phillips_nw_ols_3)


phillips_nw_ols_4 <- sandwich::NeweyWest(phillips_ols_4, lag = 1, prewhite = F, adjust = T)
ttest_4 <- lmtest::coeftest(phillips_ols_4, vcov = phillips_nw_ols_4)


# 結果のまとめ表 -----------------------------------------------------------------


results <- as.data.frame(rbind(ttest_1, ttest_2, ttest_3, ttest_4)) |> 
  dplyr::filter(dplyr::row_number() %% 2 == 0)
row.names(results) <- c("CPIを失業に", "GDPデフレータを失業に", "CPIをGDPギャップに", "GDPデフレータをGDPギャップに")
results
