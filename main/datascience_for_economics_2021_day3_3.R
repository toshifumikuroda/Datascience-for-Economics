# Reproduct Bloom and Reenen (2007) ---------------------------------------
# to understand estimate production function 


# setup -------------------------------------------------------------------
# Clear memory ------------------------------------------------------------
rm(list=ls())

# load library ------------------------------------------------------------
library(tidyverse)
library(estimatr)
library(huxtable)


# file downloads  ---------------------------------------------------------
# download.file("https://stacks.stanford.edu/file/druid:xh580yd6172/realdata.csv",
#                "./data/realdata.csv")

# load csv ----------------------------------------------------------------
realdata <- readr::read_csv("./data/realdata.csv")


# Figure 1 ----------------------------------------------------------------
realdata %>% ggplot2::ggplot(aes(x=amanagement))+
  geom_histogram(aes(y = ..density..), bins = 50)+
  facet_wrap(cty ~ ., nrow = 2)+
  scale_y_continuous(breaks=seq(0,1.2,0.2)) +
  labs(title="Distribution of Management Scores by Country",x="Management Score", y = "Density")+
  theme_classic()


# Management table --------------------------------------------------------
realdata %>%
  dplyr::select(management, cty) %>%
  dplyr::group_by(cty) %>%
  dplyr::summarise(
    mean = mean(management, na.rm = TRUE),
    sd = sd(management, na.rm = TRUE),
    min = min(management, na.rm = TRUE),
    median = median(management, na.rm = TRUE),
    max = max(management, na.rm = TRUE)
  )


# Table 1  ----------------------------------------------------------------

## Col1 --------------------------------------------------------------------
realdata %>%
  tidyr::drop_na(lm, lp) %>%
  estimatr::lm_robust(formula = ls ~ zmanagement + le + le_fr + le_gr + le_uncons + le_us + uncons + cty + factor(year), clusters = code)

# can't replicate by using cty and factor(year)

### define cyy* as set variavles -------------------------------------------
cyy <- paste("cyy", 1:44, sep = "")


### define xvars -----------------------------------------------------------
col1 <- c("zmanagement", "le", "le_fr", "le_gr", "le_uncons", "le_us", "uncons")


### paste model 1 formula ----------------------------------------------
xvars_col1 <- paste(c(col1, cyy), collapse = " + ")
model_col1 <- paste("ls", xvars_col1, sep = " ~ ")
model_col1 <- as.formula(model_col1)


### regress ----------------------------------------------------------------
table1_1 <- 
  realdata %>%
  tidyr::drop_na(lm, lp) %>%
  estimatr::lm_robust(formula = model_col1, clusters = code)


### regress again to use stata's cluster robust option ---------------------
table1_1 <- 
  realdata %>%
  tidyr::drop_na(lm, lp) %>%
  estimatr::lm_robust(formula = model_col1, clusters = code, se_type = "stata")



## Col2 -------------------------------------------------------------------

### define xvars ----------------------------------------------------------
model_base <- c("zmanagement", "le", "lp", "lm", 
                "le_fr", "le_gr", "le_uncons", "le_us", 
                "lp_fr", "lp_gr", "lp_uncons", "lp_us", 
                "lm_fr", "lm_gr", "lm_un", "lm_us", 
                "uncons")


### paste model 2 formula ----------------------------------------------
xvars_col2 <- paste(c(model_base, cyy), collapse = " + ")
model_col2 <- paste("ls", xvars_col2, sep = " ~ ")
model_col2 <- as.formula(model_col2)


### regress ---------------------------------------------------------------
table1_2 <- realdata %>%
  tidyr::drop_na(lm, lp) %>%
  estimatr::lm_robust(formula = model_col2, clusters = code, se_type = "stata")


## Col3 -------------------------------------------------------------------

### define set variables --------------------------------------------------
# controls <- c("lfirmage", "public", "ldegree", "ldegreemiss", "mba", "mbamiss", "lhrs", "factor(sic3)") # this is the paper's model
controls <- c("lfirmage", "public", "ldegree", "ldegreemiss", "mba", "mbamiss", "lhrs", "factor(sic3)") # this is a model in a class


### paste model 3 formula ----------------------------------------------
xvars_col3 <- paste(c(model_base, cyy, controls), collapse = " + ")
model_col3 <- paste("ls", xvars_col3, sep = " ~ ")
model_col3 <- as.formula(model_col3)

### regress col3
table1_3 <- realdata %>%
 tidyr::drop_na(lm, lp) %>%
 estimatr::lm_robust(formula = model_col3, clusters = code, se_type = "stata")


## Col4 -------------------------------------------------------------------
###redefine controls to avoid memory overflow
#controls <- c("lfirmage", "public", "ldegree", "ldegreemiss", "mba", "mbamiss", "lhrs")
### define noise variables
noise <- c("gender", "sen1", "tenurepost", "countries", 
           "day2", "day3", "day4", "day5", "timelocal", "duration", "reli", 
           "aa1", "aa2", "aa3", "aa4", "aa5", "aa6", "aa7", "aa8", "aa9", 
           "aa10", "aa11", "aa12", "aa13", "aa14", "aa15", "aa16")


### paste model 4 formula ----------------------------------------------
xvars_col4 <- paste(c(model_base, noise, cyy, controls), collapse = " + ")
model_col4 <- paste("ls", xvars_col4, sep = " ~ ")
model_col4 <- as.formula(model_col4)


### regress col4 ----------------------------------------------------------
table1_4 <- realdata %>%
 tidyr::drop_na(lm, lp) %>%
 estimatr::lm_robust(formula = model_col4, clusters = code, se_type = "stata")

## Col6 -------------------------------------------------------------------
### paste model 6 formula ----------------------------------------------
xvars_col6 <- paste(c(model_base, noise, cyy, controls), collapse = " + ")
model_col6 <- paste("roce", xvars_col6, sep = " ~ ")
model_col6 <- as.formula(model_col6)

table1_6 <- realdata %>%
  tidyr::drop_na(lm, lp) %>%
  estimatr::lm_robust(formula = model_col6, clusters = code, se_type = "stata")

## Col7 -------------------------------------------------------------------
### paste model 7 formula ----------------------------------------------
xvars_col7 <- paste(c(model_base, noise, cyy, controls), collapse = " + ")
model_col7 <- paste("lq", xvars_col7, sep = " ~ ")
model_col7 <- as.formula(model_col7)

table1_7 <- realdata %>%
  tidyr::drop_na(lm, lp) %>%
  estimatr::lm_robust(formula = model_col7, clusters = code, se_type = "stata")


## Col9 -------------------------------------------------------------------
### paste model 9 formula ----------------------------------------------
xvars_col9 <- paste(c(model_base, noise, cyy, controls), collapse = " + ")
model_col9 <- paste("dsales", xvars_col9, sep = " ~ ")
model_col9 <- as.formula(model_col9)

table1_9 <- realdata %>%
  tidyr::drop_na(lm, lp) %>%
  estimatr::lm_robust(formula = model_col9, clusters = code,se_type = "stata")




### table 1 ---------------------------------------------------------------
table1_coef_map <- c(
  "zmanagement" = "Management z-score",
  "le" = "Ln(Labor)",             
  "lp" = "Ln(Capital)", 
  "lm" = "Ln(Materials)"
  )

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

table1 <- 
  modelsummary::modelsummary(
    list("(1)"=table1_1, "(2)"=table1_2, "(3)"=table1_3, "(4)"=table1_4, 
         "(6)"=table1_6, "(7)"=table1_7, "(9)"=table1_9
         ),
    coef_map = table1_coef_map,
    statistics = "std.error",
    add_rows = table1_rows,
    gof_map = c("nobs", "r.squared")
    )


## export table ------------------------------------------------------------
modelsummary::modelsummary(
  list("(1)"=table1_1, "(2)"=table1_2, "(3)"=table1_3, "(4)"=table1_4, 
       "(6)"=table1_6, "(7)"=table1_7, "(9)"=table1_9
  ),
  coef_map = table1_coef_map,
  statistics = c('# observations' = 'nobs'),
  add_rows = table1_rows,
  gof_map = c("nobs", "r.squared"),
  output = "huxtable"
) %>%
  huxtable::quick_xlsx(
    file = "figuretable/firm_table1.xlsx"
  )


# Table 2 -----------------------------------------------------------------
table2_data <- realdata %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(zcapital = mean(c(zlean1, zlean2, zperf2), na.rm = TRUE)) %>%
  dplyr::mutate(zhuman = mean(c(ztalent1, ztalent6, ztalent7), na.rm = TRUE)) %>%
  dplyr::mutate(zhuman_zcapital = zhuman-zcapital) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(code) %>%
  dplyr::filter(year == max(year))

table2_1 <- table2_data %>%
  lm_robust(formula = zhuman ~ ldegree + ldegreemiss + cceurope + ccgermany + ccus, 
            se_type = "stata")

table2_2 <- table2_data %>%
  lm_robust(formula = zcapital ~ ldegree + ldegreemiss + cceurope + ccgermany + ccus, 
            se_type = "stata")

table2_3 <- table2_data %>%
  lm_robust(formula = zhuman_zcapital ~ ldegree + ldegreemiss + cceurope + ccgermany + ccus, 
            se_type = "stata")

table2_4 <- table2_data %>%
  lm_robust(formula = zhuman_zcapital ~ ldegree + ldegreemiss + lempm + lfirmage + public + cceurope + ccgermany + ccus + factor(sic3), 
            se_type = "stata")

table2_5 <- table2_data %>%
  lm_robust(formula = zhuman_zcapital ~ law + lempm + public + cceurope + ccgermany + ccus + factor(sic3), 
            se_type = "stata")


## table2 -----------------------------------------------------------------
table2_coef_map <- c(
  "ldegree" = "Ln(proportion of employees with college degree)",
  "law" = "Ln(firm average wages)"
)

table2_rows <-
  data.frame(rbind(
    c("Dependent variable", 
      "Human capital management", 
      "Fixed capital management",
      "Human capital - fixed capital management", 
      "Human capital - fixed capital management", 
      "Human capital - fixed capital management"
    ),
    c("General controls", "No", "No", "No", "Yes", "Yes"),
    c("Industry controls", "No", "No", "No", "Yes", "Yes")
  )
  )

attr(table2_rows, 'position') <- c(1,6,7)

table2 <- 
  modelsummary::modelsummary(
    list("(1)"=table2_1, "(2)"=table2_2, "(3)"=table2_3, "(4)"=table2_4, "(5)"=table2_5),
    coef_map = table2_coef_map,
    statistics = "std.error",
    add_rows = table2_rows,
    gof_map = c("nobs", "r.squared")
    )


### export table2 ---------------------------------------------------------
modelsummary::modelsummary(
  list("(1)"=table2_1, "(2)"=table2_2, "(3)"=table2_3, "(4)"=table2_4, "(5)"=table2_5),
  coef_map = table2_coef_map,
  statistics = "std.error",
  add_rows = table2_rows,
  gof_map = c("nobs"),
  output = "huxtable"
) %>%
  huxtable::quick_xlsx(
    file = "figuretable/firm_table2.xlsx"
  )

# Table 3 -----------------------------------------------------------------
table3_control <- c("lempm", "lfirmage", "public", "ldegree", "ldegreemiss", "mba", "mbamiss", "uncons", "ccfrance", "ccgermany", "ccuk", noise) 


## col1 -------------------------------------------------------------------
### paste model 1 formula ----------------------------------------------
xvars_table3_col1 <- paste(c("lindiopen9599", "factor(cty)"), collapse = " + ")
model_table3_col1 <- paste("zmanagement", xvars_table3_col1, sep = " ~ ")
model_table3_col1 <- as.formula(model_table3_col1)


### regress  --------------------------------------------------------------
table3_1 <- realdata %>%
  dplyr::group_by(code) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::ungroup() %>%
  estimatr::lm_robust(formula = model_table3_col1, clusters = oecdind_cty, se_type = "stata")


## col2 -------------------------------------------------------------------
### paste model 2 formula ----------------------------------------------
xvars_table3_col2 <- paste(c("lindiopen9599", "factor(sic3)", table3_control), collapse = " + ")
model_table3_col2 <- paste("zmanagement", xvars_table3_col2, sep = " ~ ")
model_table3_col2 <- as.formula(model_table3_col2)


### regress  --------------------------------------------------------------
table3_2 <- realdata %>%
  dplyr::group_by(code) %>%
  dplyr::filter(year == max(year)) %>%
  estimatr::lm_robust(formula = model_table3_col2, clusters = oecdind_cty, se_type = "stata")


## col3 -------------------------------------------------------------------
### paste model 3 formula ----------------------------------------------
xvars_table3_col3 <- paste(c("lerner", "factor(cty)"), collapse = " + ")
model_table3_col3 <- paste("zmanagement", xvars_table3_col3, sep = " ~ ")
model_table3_col3 <- as.formula(model_table3_col3)

### regress  --------------------------------------------------------------
table3_3 <- realdata %>%
  dplyr::group_by(code) %>%
  dplyr::filter(year == max(year)) %>%
  estimatr::lm_robust(formula = model_table3_col3, clusters = csic3, se_type = "stata")

## col4 -------------------------------------------------------------------
### paste model 4 formula ----------------------------------------------
xvars_table3_col4 <- paste(c("lerner", "factor(sic3)", table3_control), collapse = " + ")
model_table3_col4 <- paste("zmanagement", xvars_table3_col4, sep = " ~ ")
model_table3_col4 <- as.formula(model_table3_col4)

### regress  --------------------------------------------------------------
table3_4 <- realdata %>%
  dplyr::group_by(code) %>%
  dplyr::filter(year == max(year)) %>%
  estimatr::lm_robust(formula = model_table3_col4, clusters = csic3, se_type = "stata")

## col5 -------------------------------------------------------------------
### paste model 5 formula ----------------------------------------------
xvars_table3_col5 <- paste(c("competition", "competitionmiss", "factor(cty)"), collapse = " + ")
model_table3_col5 <- paste("zmanagement", xvars_table3_col5, sep = " ~ ")
model_table3_col5 <- as.formula(model_table3_col5)

### regress  --------------------------------------------------------------
table3_5 <- realdata %>%
  dplyr::group_by(code) %>%
  dplyr::filter(year == max(year)) %>%
  estimatr::lm_robust(formula = model_table3_col5, clusters = csic3, se_type = "stata")

## col6 -------------------------------------------------------------------
### paste model 6 formula ----------------------------------------------
xvars_table3_col6 <- paste(c("competition", "competitionmiss", "factor(sic3)", table3_control), collapse = " + ")
model_table3_col6 <- paste("zmanagement", xvars_table3_col6, sep = " ~ ")
model_table3_col6 <- as.formula(model_table3_col6)

### regress  --------------------------------------------------------------
table3_6 <- realdata %>%
  dplyr::group_by(code) %>%
  dplyr::filter(year == max(year)) %>%
  estimatr::lm_robust(formula = model_table3_col6, clusters = csic3, se_type = "stata")

## col7 -------------------------------------------------------------------
### paste model 7 formula ----------------------------------------------
xvars_table3_col7 <- paste(c("lindiopen9599", "lerner", "competition", "competitionmiss", "factor(cty)"), collapse = " + ")
model_table3_col7 <- paste("zmanagement", xvars_table3_col7, sep = " ~ ")
model_table3_col7 <- as.formula(model_table3_col7)

### regress  --------------------------------------------------------------
table3_7 <- realdata %>%
  dplyr::group_by(code) %>%
  dplyr::filter(year == max(year)) %>%
  estimatr::lm_robust(formula = model_table3_col7, clusters = oecdind_cty, se_type = "stata")

## col8 -------------------------------------------------------------------
### paste model 8 formula ----------------------------------------------
xvars_table3_col8 <- paste(c("lindiopen9599", "lerner", "competition", "competitionmiss", "factor(sic3)", table3_control), collapse = " + ")
model_table3_col8 <- paste("zmanagement", xvars_table3_col8, sep = " ~ ")
model_table3_col8 <- as.formula(model_table3_col8)

### regress  --------------------------------------------------------------
table3_8 <- realdata %>%
  dplyr::group_by(code) %>%
  dplyr::filter(year == max(year)) %>%
  estimatr::lm_robust(formula = model_table3_col8, clusters = csic3, se_type = "stata")

### table3 ------------------------------------------------------------------
table3_coef_map <- c(
  "lindiopen9599" = "Import penetration (5-years lagged)",
  "lerner" = "Lerner index (5-years lagged)",
  "competition" = "Number of competitors"
)

table3_rows <-
  data.frame(rbind(
    c("Estimation method ", "OLS", "OLS", "OLS", "OLS", "OLS", "OLS", "OLS", "OLS"),
    c("Dependent variable", 
      "Management z-score", "Management z-score", "Management z-score", "Management z-score", 
      "Management z-score", "Management z-score", "Management z-score", "Management z-score"),
    c("General controls", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes")
  )
  )

attr(table3_rows, 'position') <- c(1,2,10)

table3 <- 
  modelsummary::modelsummary(
    list("(1)"=table3_1, "(2)"=table3_2, "(3)"=table3_3, "(4)"=table3_4, 
         "(5)"=table3_5, "(6)"=table3_6, "(7)"=table3_7, "(8)"=table3_8),
    coef_map = table3_coef_map,
    statistics = "std.error",
    add_rows = table3_rows,
    gof_map = c("nobs", "r.squared")
  )


### export table3 ---------------------------------------------------------
modelsummary::modelsummary(
  list("(1)"=table3_1, "(2)"=table3_2, "(3)"=table3_3, "(4)"=table3_4, 
       "(5)"=table3_5, "(6)"=table3_6, "(7)"=table3_7, "(8)"=table3_8),
  coef_map = table3_coef_map,
  statistics = "std.error",
  add_rows = table3_rows,
  gof_map = c("nobs", "r.squared"),
  output = "huxtable"
) %>%
  huxtable::quick_xlsx(
    file = "figuretable/firm_table3.xlsx"
  )
