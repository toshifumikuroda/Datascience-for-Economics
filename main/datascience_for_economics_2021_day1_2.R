# Figure Out Modern Japanese Philips Curb ---------------------------------
# set up ------------------------------------------------------------------
rm(list = ls())
library(tidyverse)


# read csv ----------------------------------------------------------------
timeseries <- readr::read_csv("./data/03_timeseries.csv")

# part 1 ------------------------------------------------------------------
## plot GDP_nominal and GDP_real ------------------------------------------
figure_GDP_nominal_1 <- timeseries %>% 
  dplyr::mutate(GDP_real = GDP_real/1000) %>%
  dplyr::mutate(GDP_nominal = GDP_nominal/1000) %>%
  ggplot2::ggplot(ggplot2::aes(x = year)) +
  ggplot2::geom_line(ggplot2::aes(y = GDP_nominal, colour="blue"))+
  ggplot2::geom_line(ggplot2::aes(y = GDP_real, colour="red")) +
  ggplot2::xlab("Year") +
  ggplot2::ylab("GDP (JPY Trillion)") +
  ggplot2::scale_color_discrete(name = "GDP", labels = c("Nominal", "Real")) +
  ggplot2::theme_classic()
figure_GDP_nominal_1



## plot GDP_nominal and GDP_real using gather -----------------------------
timeseries_gather <-
  timeseries %>% 
  dplyr::mutate(GDP_real = GDP_real/1000) %>%
  dplyr::mutate(GDP_nominal = GDP_nominal/1000) %>%
  tidyr::gather(key = "Type", value = "GDP", GDP_nominal, GDP_real)

figure_GDP_nominal_2 <- 
  timeseries_gather %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = GDP, colour = Type)) +
  ggplot2::geom_line() +
  ggplot2::xlab("Year") +
  ggplot2::ylab("GDP (JPY billion)") +
  ggplot2::theme_classic()
figure_GDP_nominal_2



## generate GDP_deflator = GDP_nominal / GDP_real -------------------------
timeseries <- timeseries %>%
  dplyr::mutate(gdp_deflator = GDP_nominal / GDP_real * 100)

## plot GDP_defrator & CPI -----------------------------------------------
figure_price <- timeseries %>% 
  tidyr::gather(key = "Type", value = "Price", CPI, gdp_deflator) %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = Price, colour = Type)) +
  ggplot2::geom_line() +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Price") +
  ggplot2::theme_classic() +
  ggplot2::scale_color_discrete(name = "Price", labels = c("CPI", "GDP Deflator"))
figure_price


## plot Unemployment ------------------------------------------------------
figure_unemployment <- timeseries %>% 
  ggplot2::ggplot(ggplot2::aes(x = year, y = Unemployment_rate)) +
  ggplot2::geom_line() +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Unemployment Rate") +
  ggplot2::theme_classic()
figure_unemployment


# part2 : philips curve ---------------------------------------------------
## gen inflation rate of GDP def % CPI ------------------------------------
timeseries <- timeseries %>%
  dplyr::mutate(
    inflation_rate_gdpdef = (gdp_deflator/dplyr::lag(gdp_deflator)-1),
    inflation_rate_cpi = (CPI/dplyr::lag(CPI)-1)
    )

## plot scatter of inflation on GDPgap ------------------------------------
timeseries %>% ggplot2::ggplot(ggplot2::aes(x = GDP_gap,  y = inflation_rate_gdpdef)) +
  ggplot2::geom_point() +
  ggplot2::xlab("GDP Gap") +
  ggplot2::ylab("Inflation(GDP Deflator)") +
  ggplot2::theme_classic()


### geom_path(1) : GDP_def GDP_gap ----------------------------------------
timeseries %>% ggplot2::ggplot(ggplot2::aes(x = GDP_gap,  y = inflation_rate_gdpdef)) +
  ggplot2::geom_path() +
  ggplot2::xlab("GDP Gap") +
  ggplot2::ylab("Inflation(GDP Deflator)") +
  ggplot2::theme_classic()


### geom_path(2) : CPI GDP_gap ----------------------------------------------
timeseries %>% ggplot2::ggplot(ggplot2::aes(x = GDP_gap,  y = inflation_rate_cpi)) +
  ggplot2::geom_path() +
  ggplot2::xlab("GDP Gap") +
  ggplot2::ylab("Inflation(CPI)") +
  ggplot2::theme_classic()


### geom_path(3) : GDP_def Unemplotment -----------------------------------
timeseries %>% ggplot2::ggplot(ggplot2::aes(x = Unemployment_rate,  y = inflation_rate_gdpdef)) +
  ggplot2::geom_path() +
  ggplot2::xlab("Unemployment Rate") +
  ggplot2::ylab("Inflation(GDP Deflator)") +
  ggplot2::theme_classic()


### geom_path(4) : CPI Unemplotment ---------------------------------------
timeseries %>% ggplot2::ggplot(ggplot2::aes(x = Unemployment_rate,  y = inflation_rate_cpi)) +
  ggplot2::geom_path() +
  ggplot2::xlab("Unemployment Rate") +
  ggplot2::ylab("Inflation(CPI)") +
  ggplot2::theme_classic()


### geom_path(5) : for SNS --------------------------------------------------
timeseries %>% dplyr::mutate(ages = format(as.integer(year/10),3)) %>%
  ggplot2::ggplot(ggplot2::aes(x = Unemployment_rate,  y = inflation_rate_cpi, colour = ages, shape= ages)) +
  ggplot2::geom_path() +
  ggplot2::geom_point() +
  ggplot2::xlab("Unemployment Rate") +
  ggplot2::ylab("Inflation(CPI)") +
  ggplot2::theme_classic() + 
  ggplot2::scale_color_manual(breaks = c("198", "199", "200", "201"),
                     values=c("red", "blue", "green", "purple"))


## regress inflation on GDPgap ---------------------------------------------
phillips_ols_1 <- lm(inflation_rate_cpi ~ Unemployment_rate, data=timeseries)
phillips_ols_1$coefficients
phillips_ols_1$fitted.values
summary(phillips_ols_1)[["coefficients"]][, "t value"]

## geom_point with lm
timeseries %>% ggplot2::ggplot(ggplot2::aes(x = Unemployment_rate,  y = inflation_rate_cpi)) +
  ggplot2::geom_point() +
  ggplot2::stat_smooth(method = "lm", se = FALSE, colour = "red", size = 1) +
  ggplot2::xlab("Unemployment Rate") +
  ggplot2::ylab("Inflation(CPI)") +
  ggplot2::theme_classic()


## geom_point with lm for SNS ---------------------------------------------
timeseries %>% dplyr::mutate(ages = format(as.integer(year/10),3)) %>%
  ggplot2::ggplot(ggplot2::aes(x = Unemployment_rate,  y = inflation_rate_cpi, colour = ages, shape= ages)) +
  ggplot2::geom_point() +
  ggplot2::stat_smooth(method = "lm", se = FALSE, size = 0.5, linetype="dashed") +
  ggplot2::xlab("Unemployment Rate") +
  ggplot2::ylab("Inflation(CPI)") +
  ggplot2::theme_classic() + 
  ggplot2::scale_color_manual(breaks = c("198", "199", "200", "201"),
                     values=c("red", "blue", "green", "purple"))


# part3 is data autocorrelated? -------------------------------------------

## plot residuals ---------------------------------------------------------
timeseries$phillips_ols_1_residual <- NA
timeseries$phillips_ols_1_residual[which(!is.na(timeseries$inflation_rate_cpi))] <- phillips_ols_1$resid
timeseries %>% 
  ggplot2::ggplot(ggplot2::aes(x = year,  y = phillips_ols_1_residual)) +
  ggplot2::geom_path() +
  ggplot2::stat_smooth(method = "lm", se = FALSE, colour = "red", size = 1) +
  ggplot2::xlab("Year") +  ggplot2::ylab("OLS Residuals") +
  ggplot2::theme_classic()


## Is redisuals are white noize ? -----------------------------------------
timeseries %>% 
  dplyr::mutate(
    whitenoize = rnorm(
      dplyr::n(), mean = 0, sd = summary(phillips_ols_1)$sigma)
    ) %>%
  ggplot2::ggplot(ggplot2::aes(x = year,  y = whitenoize))+
  ggplot2::geom_path(linetype="dashed") +
  ggplot2::geom_path(ggplot2::aes(x = year,  y = phillips_ols_1_residual), colour = "red") +
  ggplot2::geom_line(ggplot2::aes(x = year, y = 0)) +
  ggplot2::xlab("Year") +  ggplot2::ylab("OLS Residuals") +
  ggplot2::theme_classic()


## autocorration test ------------------------------------------------------
acf(na.omit(timeseries$phillips_ols_1_residual), lag.max = 10, plot = T)


# Part4 HAC standard error ------------------------------------------------
# install.packages("sandwich") 
# install.packages("lmtest")
library(sandwich)
library(lmtest)

phillips_nw_ols_1 <- sandwich::NeweyWest(phillips_ols_1, lag = 1, prewhite = F, adjust = T)
ttest_1 <- lmtest::coeftest(phillips_ols_1, vcov = phillips_nw_ols_1)

phillips_ols_2 <- lm(inflation_rate_gdpdef ~ Unemployment_rate, data=timeseries)
phillips_nw_ols_2 <- sandwich::NeweyWest(phillips_ols_2, lag = 1, prewhite = F, adjust = T)
ttest_2 <- lmtest::coeftest(phillips_ols_2, vcov = phillips_nw_ols_2)

phillips_ols_3 <- lm(inflation_rate_cpi ~ GDP_gap, data=timeseries)
phillips_nw_ols_3 <- sandwich::NeweyWest(phillips_ols_3, lag = 1, prewhite = F, adjust = T)
ttest_3<- lmtest::coeftest(phillips_ols_3, vcov = phillips_nw_ols_3)


phillips_ols_4 <- lm(inflation_rate_gdpdef ~ GDP_gap, data=timeseries)
phillips_nw_ols_4 <- sandwich::NeweyWest(phillips_ols_4, lag = 1, prewhite = F, adjust = T)
ttest_4 <- lmtest::coeftest(phillips_ols_4, vcov = phillips_nw_ols_4)

results <- as.data.frame(rbind(ttest_1, ttest_2, ttest_3, ttest_4)) %>%
  dplyr::filter(dplyr::row_number() %% 2 == 0)
row.names(results) <- c("CPI on Unemployment", "GDP Deflator on Unemployment",
                        "CPI on GDP Gap", "GDP Deflator on GDP Gap")
results
