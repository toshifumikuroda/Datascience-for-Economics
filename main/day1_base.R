
# 4行でできるコロナ感染者数 ---------------------------------------
download.file("https://dl.dropboxusercontent.com/s/6mztoeb6xf78g5w/COVID-19.csv", "./input/COVID-19.csv")
covid_19 <- read.csv("./input/COVID-19.csv")
infected <- as.Date(covid_19$確定日, "%m/%d/%y") 
hist(infected, breaks="days", freq=FALSE)

# 性別プロット ----------------------------------------

covid_19_femail <- subset(covid_19, 性別=="女性")
infected_femails <- as.Date(covid_19_femail$確定日, "%m/%d/%y")
hist(infected_femails , breaks="days", freq=TRUE)
