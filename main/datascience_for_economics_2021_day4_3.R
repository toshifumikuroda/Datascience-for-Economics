# setup -------------------------------------------------------------------
rm(list = ls())

## ライブラリ読み込み --------------------------------------------------------------
# library(tidyverse)
library(magrittr)

# library(ggplot2)

## unzip -------------------------------------------------------------------
# ファイルは以下のURLからDLし、メニューからupload済みとする
# https://economics.mit.edu/faculty/acemoglu/data/ajr2001

for (i in 1:5) {
  zipfilename <- paste("./data/AJR/maketable", toString(i), ".zip", sep="")
  unzip(zipfile = zipfilename, exdir = "./data/AJR/")
}


# Figure 1 ----------------------------------------------------------------
table1_df <- foreign::read.dta("./data/AJR/maketable1.dta")
colnames(table1_df)
figure1 <- table1_df %>% 
  dplyr::filter(baseco==1) %>%
  ggplot2::ggplot(ggplot2::aes(x = logem4, y = logpgp95, label = shortnam)) +
  ggplot2::geom_text() +
  ggplot2::stat_smooth(formula = "y ~ x", method = "lm", se = FALSE, colour = "black", size = 1) +
  ggplot2::xlab("Log European settler mortality") + 
  ggplot2::ylab("log GDP per capita, PPP, in 1995") +
  ggplot2::theme_classic()

ggplot2::ggsave(file = "./figuretable/AJR_figure1.png", plot = figure1, dpi = 100, width = 9.6, height = 5.4)

# Table 1 -----------------------------------------------------------------
colvars <- c("logpgp95", "loghjypl", "avexpr", "cons00a", "cons1", "democ00a", "euro1900", "logem4")

count_n <- function(x) {
  x <- na.omit(x)
  return(length(x))
}

## col1 -------------------------------------------------------------------
table1_col1_mean <- table1_df %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(colvars), mean, na.rm = TRUE)
  )

table1_col1_sd <- table1_df %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(colvars), sd, na.rm = TRUE)
  )

table1_col1_mean_n <- table1_df %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(colvars), length)
  )

table1_col1_mean_n <- table1_df %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(colvars), count_n)
  )


## col2 -------------------------------------------------------------------
table1_col2_mean <- table1_df %>%
  dplyr::filter(baseco == 1) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(colvars), mean, na.rm = TRUE)
  )

table1_col2_sd <- table1_df %>%
  dplyr::filter(baseco == 1) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(colvars), sd, na.rm = TRUE)
  )

table1_col2_mean_n <- table1_df %>%
  dplyr::filter(baseco == 1) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(colvars), length)
  )

## col3 -------------------------------------------------------------------
table1_col3_mean <- table1_df %>%
  dplyr::filter(!is.na(logem4)) %>%
  dplyr::filter(baseco==1 & 
                  quantile(logem4, 0.25, na.rm = TRUE) > logem4
  ) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(colvars), mean, na.rm = TRUE)
  )

table1_col3_sd <- table1_df %>%
  dplyr::filter(!is.na(logem4)) %>%
  dplyr::filter(baseco==1 & 
                  quantile(logem4, 0.25, na.rm = TRUE) > logem4
  ) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(colvars), sd, na.rm = TRUE)
  )

table1_col3_mean_n <- table1_df %>%
  dplyr::filter(!is.na(logem4)) %>%
  dplyr::filter(baseco==1 & 
                  quantile(logem4, 0.25, na.rm = TRUE) > logem4
  ) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(colvars), length)
  )

## col4 -------------------------------------------------------------------
table1_col4_mean <- table1_df %>%
  dplyr::filter(!is.na(logem4)) %>%
  dplyr::filter(baseco==1 & 
                  quantile(logem4, 0.25, na.rm = TRUE) <= logem4 &
                  quantile(logem4, 0.50, na.rm = TRUE) > logem4) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(colvars), mean, na.rm = TRUE)
  )

table1_col4_mean_n <- table1_df %>%
  dplyr::filter(!is.na(logem4)) %>%
  dplyr::filter(baseco==1 & 
                  quantile(logem4, 0.25, na.rm = TRUE) <= logem4 &
                  quantile(logem4, 0.50, na.rm = TRUE) > logem4) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(colvars), length)
  )

table1_col4_sd <- table1_df %>%
  dplyr::filter(!is.na(logem4)) %>%
  dplyr::filter(baseco==1 & 
                  quantile(logem4, 0.25, na.rm = TRUE) <= logem4 &
                  quantile(logem4, 0.50, na.rm = TRUE) > logem4) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(colvars), sd, na.rm = TRUE)
  )

## col5 -------------------------------------------------------------------
table1_col5_mean <- table1_df %>%
  dplyr::filter(!is.na(logem4)) %>%
  dplyr::filter(baseco==1 & 
                  quantile(logem4, 0.50, na.rm = TRUE) <= logem4 &
                  quantile(logem4, 0.75, na.rm = TRUE) > logem4) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(colvars), mean, na.rm = TRUE)
  )

table1_col5_mean_n <- table1_df %>%
  dplyr::filter(!is.na(logem4)) %>%
  dplyr::filter(baseco==1 & 
                  quantile(logem4, 0.50, na.rm = TRUE) <= logem4 &
                  quantile(logem4, 0.75, na.rm = TRUE) > logem4) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(colvars), length)
  )

table1_col5_sd <- table1_df %>%
  dplyr::filter(!is.na(logem4)) %>%
  dplyr::filter(baseco==1 & 
                  quantile(logem4, 0.50, na.rm = TRUE) <= logem4 &
                  quantile(logem4, 0.75, na.rm = TRUE) > logem4) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(colvars), sd, na.rm = TRUE)
  )

## col6 -------------------------------------------------------------------
table1_col6_mean <- table1_df %>%
  dplyr::filter(!is.na(logem4)) %>%
  dplyr::filter(!is.na(logem4)) %>%
  dplyr::filter(baseco==1 & 
                  quantile(logem4, 0.75, na.rm = TRUE) <= logem4 &
                  quantile(logem4, 1, na.rm = TRUE) > logem4) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(colvars), mean, na.rm = TRUE)
  )

table1_col6_mean_n <- table1_df %>%
  dplyr::filter(!is.na(logem4)) %>%
  dplyr::filter(baseco==1 & 
                  quantile(logem4, 0.75, na.rm = TRUE) <= logem4 &
                  quantile(logem4, 1, na.rm = TRUE) > logem4) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(colvars), length)
  )

table1_col6_sd <- table1_df %>%
  dplyr::filter(!is.na(logem4)) %>%
  dplyr::filter(!is.na(logem4)) %>%
  dplyr::filter(baseco==1 & 
                  quantile(logem4, 0.75, na.rm = TRUE) <= logem4 &
                  quantile(logem4, 1, na.rm = TRUE) > logem4) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(colvars), sd, na.rm = TRUE)
  )

# join columns ------------------------------------------------------------
table1 <- rbind(
  table1_col1_mean,
  table1_col1_sd,
  table1_col2_mean,
  table1_col2_sd,
  table1_col3_mean,
  table1_col3_sd,
  table1_col4_mean,
  table1_col4_sd,
  table1_col5_mean,
  table1_col5_sd,
  table1_col6_mean,
  table1_col6_sd
) %>%
  # rename
  dplyr::rename(
    "log GDP per capita, PPP, in 1995" = logpgp95,
    "log output per worker in 1988" = loghjypl, 
    "Average protection against expropriation risk, 1985-1995" = avexpr, 
    "Constraint on executive, 1900" = cons00a, 
    "Constraint on executive in first year of independence" = cons1, 
    "Democracy in 1900" = democ00a, 
    "European settlement in 1900" = euro1900,
    "Log European settler mortality" = logem4) %>%
  t()

n <- c(max(table1_col1_mean_n),
              max(table1_col1_mean_n),
              max(table1_col2_mean_n),
              max(table1_col2_mean_n),
              max(table1_col3_mean_n),
              max(table1_col3_mean_n),
              max(table1_col4_mean_n),
              max(table1_col4_mean_n),
              max(table1_col5_mean_n),
              max(table1_col5_mean_n),
              max(table1_col6_mean_n),
              max(table1_col6_mean_n)
)

table1 <- rbind(table1, n)

colnames(table1) <- c("Mean", "SD", "Mean", "SD", "Mean", "SD", "Mean", "SD", "Mean", "SD", "Mean", "SD")

table1 %>%
  kableExtra::kbl(
    digits = 3
  ) %>%
  kableExtra::add_header_above(
    c(" " = 1, "Whole world" = 2, "Base sample" = 2, "(1)" = 2, "(2)" = 2, "(3)" = 2, "(4)" = 2)
  ) %>%
  kableExtra::add_header_above(
    c(" " = 5, "By quantiles of mortality" = 8)
  ) %>%
  kableExtra::kable_classic()

## Col 3 - 5 の閾値
exp(quantile(table1_df$logem4, 0.25, na.rm = TRUE))
exp(quantile(table1_df$logem4, 0.50, na.rm = TRUE))
exp(quantile(table1_df$logem4, 0.75, na.rm = TRUE))


# Table 2 -----------------------------------------------------------------
table2_df <- foreign::read.dta("./data/AJR/maketable2.dta")


## table 2 formula objects ------------------------------------------------
table2_xvars <- c("avexpr", "lat_abst", "africa + asia + other")
table2_yvars <- c("logpgp95", "loghjypl")


## make columns -----------------------------------------------------------

### Column 1 --------------------------------------------------------------
xvars_col1 <- paste(table2_xvars[1], collapse = " + ")
model_col1 <- paste(table2_yvars[1], xvars_col1, sep = " ~ ")
model_col1 <- as.formula(model_col1)
table2_col1 <- estimatr::lm_robust(data = table2_df, formula = model_col1, se_type = "stata")

### Column 2 --------------------------------------------------------------
table2_col2 <- estimatr::lm_robust(data = table2_df %>% dplyr::filter(baseco==1), formula = model_col1, se_type = "stata")

### Column 3 --------------------------------------------------------------
xvars_col3 <- paste(table2_xvars[1:2], collapse = " + ")
model_col3 <- paste("logpgp95", xvars_col3, sep = " ~ ")
model_col3 <- as.formula(model_col3)
table2_col3 <- estimatr::lm_robust(data = table2_df, formula = model_col3, se_type = "stata")

### Column 4 --------------------------------------------------------------
xvars_col4 <- paste(table2_xvars, collapse = " + ")
model_col4 <- paste("logpgp95", xvars_col4, sep = " ~ ")
model_col4 <- as.formula(model_col4)
table2_col4 <- estimatr::lm_robust(data = table2_df, formula = model_col4, se_type = "stata")

### Column 5 --------------------------------------------------------------
table2_col5 <- estimatr::lm_robust(data = table2_df %>% dplyr::filter(baseco==1), formula = model_col3, se_type = "stata")

### Column 6 --------------------------------------------------------------
table2_col6 <- estimatr::lm_robust(data = table2_df %>% dplyr::filter(baseco==1), formula = model_col4, se_type = "stata")

### Column 7 --------------------------------------------------------------
model_col7 <- paste("loghjypl", xvars_col1, sep = " ~ ")
model_col7 <- as.formula(model_col7)
table2_col7 <- estimatr::lm_robust(data = table2_df, formula = model_col7, se_type = "stata")

### Column 8 --------------------------------------------------------------
table2_col8 <- estimatr::lm_robust(data = table2_df %>% dplyr::filter(baseco==1), formula = model_col7, se_type = "stata")


## make table2 objects ----------------------------------------------------

### coef_map --------------------------------------------------------------
table2_coef_map <-
  c(
    "avexpr" = "Average protection \n against expropriation risk,\n 1985 ~ 1995",
    "lat_abst" = "Latitude",
    "asia" = "Asia dummy",
    "africa" = "Africa dummy",
    "other" = "Other continent dummy"
    )

### add_rows --------------------------------------------------------------
table2_rows <- data.frame(
  "", 
  "Dependent variable is log GDP per capita in 1995", 
  "", "", "", "", "", 
  "Dependent variable is log output per worker in 1988",
  ""
  )
attr(table2_rows, 'position') <- c(1)


## make table2 ------------------------------------------------------------
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
  title = paste0("OLS Regressions")
)

## plot table2 ------------------------------------------------------------
table2

## save table2 ------------------------------------------------------------
modelsummary::modelsummary(
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
  output = "huxtable"
  ) %>%
  huxtable::quick_xlsx(file = "figuretable/AJR_table2.xlsx")


# make figure 2 -----------------------------------------------------------
figure2 <- 
  table2_df %>% 
  dplyr::filter(baseco==1) %>% 
  ggplot2::ggplot(ggplot2::aes(x = avexpr , y = logpgp95, label = shortnam)) +
  ggplot2::geom_text() +
  ggplot2::stat_smooth(
    formula = "y ~ x", 
    method = "lm", 
    se = FALSE, 
    colour = "black", 
    size = 1
    ) +
  ggplot2::xlab("Average Expropritation Risk 1985 ~ 95") + 
  ggplot2::ylab("log GDP per capita, PPP, in 1995") +
  ggplot2::theme_classic() +
  ggplot2::ylim(4, 10)
ggplot2::ggsave(file = "figuretable/AJR_figure2.png", plot = figure2, dpi = 100, width = 9.6, height = 6.4)


# make figure 3 -----------------------------------------------------------
table3_df <- 
  foreign::read.dta("./data/AJR/maketable3.dta") %>%
  dplyr::filter(
    excolony==1 & !is.na(extmort4)
    ) %>%
  dplyr::mutate(
    euro1900 = euro1900 / 100
    )

figure3 <- 
  table3_df %>% 
  dplyr::filter(!is.na(logpgp95)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = logem4 , y = avexpr)) +
  ggplot2::geom_point() +
  ggplot2::stat_smooth(
    formula = "y ~ x", 
    method = "lm", 
    se = FALSE, 
    colour = "black", 
    size = 1
    ) +
  ggplot2::xlab("Log European settler mortality") + 
  ggplot2::ylab("Average Expropritation Risk 1985 ~ 95") +
  ggplot2::theme_classic()
ggplot2::ggsave(file = "./figuretable/AJR_figure3.png", plot = figure3, dpi = 100, width = 9.6, height = 5.4)


# make table 3 ------------------------------------------------------------

## panel A ----------------------------------------------------------------
i <- 0
table3_col_odd <- list()
table3_col_even <-list()
for (xvar in c("cons00a", "democ00a", "indtime + cons1", "euro1900", "logem4")) {
  i <- i + 1
  ## Column odd
  xvars_col_odd <- paste(xvar, collapse = " + ")
  model_col_odd <- paste("avexpr", xvars_col_odd, sep = " ~ ")
  model_col_odd <- as.formula(model_col_odd)
  table3_col_odd[[i]] <- lm(data = table3_df, formula = model_col_odd)
  #table3_col_odd[[i]] <- estimatr::lm_robust(data = table3_df, formula = model_col_odd, se_type = "stata")
  ## Columns odd
  xvars_col_even <- paste(c(xvar, "lat_abst"), collapse = " + ")
  model_col_even <- paste("avexpr", xvars_col_even, sep = " ~ ")
  model_col_even <- as.formula(model_col_even)
  table3_col_even[[i]] <- lm(data = table3_df, formula = model_col_even)
  #table3_col_even[[i]] <- estimatr::lm_robust(data = table3_df, formula = model_col_even, se_type = "stata")
}


## fix column of logem4 ---------------------------------------------------

### column odd ------------------------------------------------------------
xvars_col_odd <- paste("logem4", collapse = " + ")
model_col_odd <- paste("avexpr", xvars_col_odd, sep = " ~ ")
model_col_odd <- as.formula(model_col_odd)
table3_col_odd[[5]] <- lm(data = table3_df %>% dplyr::filter(!is.na(logpgp95)), formula = model_col_odd)

### column even -----------------------------------------------------------
xvars_col_even <- paste(c("logem4", "lat_abst"), collapse = " + ")
model_col_even <- paste("avexpr", xvars_col_even, sep = " ~ ")
model_col_even <- as.formula(model_col_even)
table3_col_even[[5]] <- lm(data = table3_df %>% dplyr::filter(!is.na(logpgp95)), formula = model_col_even)


# make table 3 ------------------------------------------------------------


## panel a -----------------------------------------------------------------


### make coef_map ---------------------------------------------------------
table3_coef_map = c(
  "cons00a" = "Constraint on executive in\n 1900",
  "democ00a" = "Democracy in 1900", 
  "cons1" = "Constraint on executive in first\n year of independence", 
  "euro1900" = "European settlements in 1900",
  "logem4" = "Log European settler mortality",
  "lat_abst" = "Latitude"
  )

### make table 3 -----------------------------------------------------------
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

### save table3a ------------------------------------------------------------
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
  output = "huxtable"
  ) %>%
  huxtable::quick_xlsx(file = "figuretable/AJR_table3_a.xlsx")


## panel b -----------------------------------------------------------------

### define models ---------------------------------------------------------
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


### regress models ----------------------------------------------------------
table3_panel_b <- list()
for (i in 1:2) {
  table3_panel_b[[i]] <- 
    estimatr::lm_robust(
      data = table3_df %>% 
        dplyr::filter(
          !is.na(logpgp95)
          ), 
      formula = as.formula(
        model_col[i]
        ),
      se_type = "stata"
    )
  }

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

for (i in 5:10) {
  table3_panel_b[[i]] <- 
    estimatr::lm_robust(
      data = table3_df %>% 
        dplyr::filter(
          !is.na(logpgp95)
          ),
      formula = as.formula(
        model_col[i]
        ),
      se_type = "stata"
    )
  }

### make table3 objects -----------------------------------------------------
table3b_coef_map <-c(
  "euro1900" = "European settlements in 1900",
  "logem4" = "Log European settler mortality", 
  "lat_abst" = "Latitude"
  )

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

  
### make table3b ----------------------------------------------------------
table3_b <- modelsummary::modelsummary(
  table3_panel_b,
  coef_map = table3b_coef_map,
  add_rows = table3b_rows,
  gof_map = c("r.squared", "nobs"),
  title = "Determinents of Institutions"
  )

table3_b

# save table 3b -----------------------------------------------------------
table3_b <- modelsummary::modelsummary(
  table3_panel_b,
  coef_map = table3b_coef_map,
  add_rows = table3b_rows,
  gof_map = c("r.squared", "nobs"),
  title = "Determinents of Institutions",
  output = "huxtable"
  ) %>%
  huxtable::quick_xlsx(file = "figuretable/AJR_table3_b.xlsx")

# Table 4 -----------------------------------------------------------------

## prepare data -----------------------------------------------------------
table4_df <- foreign::read.dta("./data/AJR/maketable4.dta") %>%
  dplyr::filter(baseco==1) %>%
  dplyr::mutate(
    other_cont = ifelse(
      shortnam=="AUS" | shortnam=="MLT" | shortnam=="NZL",
      1, 
      0
      )
    )



## panel a ----------------------------------------------------------------

### regress ---------------------------------------------------------------
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
    data = table4_df %>% 
      dplyr::filter(rich4 != 1)
    )

table4_a_col4 <- 
  AER::ivreg(
    formula = "logpgp95 ~ avexpr + lat_abst| logem4 + lat_abst", 
    data = table4_df %>% 
      dplyr::filter(rich4!= 1)
    )

table4_a_col5 <- 
  AER::ivreg(
    formula = "logpgp95 ~ avexpr | logem4", 
    data = table4_df %>% 
      dplyr::filter(africa != 1)
    )

table4_a_col6 <- 
  AER::ivreg(
    formula = "logpgp95 ~ avexpr + lat_abst| logem4 + lat_abst", 
    data = table4_df %>% 
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


### make table objects ----------------------------------------------------
table4_a_coef_map <-
  c(
    "avexpr" = "Average protection against\n expropriation risk 1985–1995",
    "lat_abst" = "Latitude",
    "asia" = "Asia dummy",
    "africa" = "Africa dummy",
    "other_cont" = "Other continent dummy"
    )

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


### save table 4a ---------------------------------------------------------
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
  title = "IV REGRESSIONS OF LOG GDP PER CAPITA",
  
  ) %>%
  huxtable::quick_xlsx(file = "figuretable/AJR_table4_a.xlsx")

## panel b ----------------------------------------------------------------

### regress panel b -------------------------------------------------------
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
    data = table4_df %>% 
      dplyr::filter(rich4 != 1)
    )

table4_b_col4 <- 
  lm(
    formula = "avexpr ~ logem4 + lat_abst", 
    data = table4_df %>% 
      dplyr::filter(rich4 != 1)
    )

table4_b_col5 <- 
  lm(
    formula = "avexpr ~ logem4", 
    data = table4_df %>% 
      dplyr::filter(africa != 1)
    )

table4_b_col6 <- 
  lm(
    formula = "avexpr ~ logem4 + lat_abst", 
    data = table4_df %>% 
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

### make panel b objects --------------------------------------------------
table4_b_coef_map = c(
  "logem4" = "Log European settler mortality",
  "lat_abst" = "Latitude",
  "asia" = "Asia dummy",
  "africa" = "Africa dummy",
  "other_cont" = "Other continent dummy"
  )


### make table 4 b --------------------------------------------------------
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

### save table4b ----------------------------------------------------------
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
  output = "huxtable"
  ) %>%
  huxtable::quick_xlsx(file = "figuretable/AJR_table4_b.xlsx")


## panel c ----------------------------------------------------------------


### regress panel c -------------------------------------------------------
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
    data = table4_df %>% 
      dplyr::filter(rich4 != 1)
    )

table4_c_col4 <- 
  lm(
    formula = "logpgp95 ~ avexpr + lat_abst", 
    data = table4_df %>% 
      dplyr::filter(rich4 != 1)
    )

table4_c_col5 <- 
  lm(
    formula = "logpgp95 ~ avexpr", 
    data = table4_df %>% 
      dplyr::filter(africa != 1)
    )

table4_c_col6 <- 
  lm(
    formula = "logpgp95 ~ avexpr + lat_abst", 
    data = table4_df %>% 
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


# make panel c object -----------------------------------------------------

table4_b_coef_map <- c(
  "avexpr" = "Average protection against\n expropriation risk 1985 ~ 1995"
  )

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


### save table 4c ---------------------------------------------------------
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
  ) %>%
  huxtable::quick_xlsx(file = "figuretable/AJR_table4_c.xlsx")

