# tidyverseを利用したコード -------------------------------------------

# データの読み込み -------------------------------------------
covid_19 <- readr::read_csv("./input/COVID-19.csv")


# tidyverseを利用したヒストグラム -------------------------------------------
covid_19 |> dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) |> 
  ggplot2::ggplot() +
  ggplot2::aes(x=days) +
  ggplot2::geom_histogram(bandwidth = 1, color="black", fill="lightgray") + 
  ggplot2::theme_classic()


# 条件付きプロット ----------------------------------------------------------------

covid_19 |>  dplyr::filter(性別=="女性") |> 
  dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) |> 
  ggplot2::ggplot() + 
  ggplot2::aes(x=days) + 
  ggplot2::geom_histogram(binwidth = 1, color="black", fill="lightgray")+ 
  ggplot2::theme_classic()


# 性別で塗り分けるケース ------------------------------------------

covid_19 |> 
  dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) |> 
  ggplot2::ggplot() + 
  ggplot2::aes(x=days, fill = 性別) +
  ggplot2::geom_histogram(bins =100) + 
  ggplot2::theme_classic()


## 性別の確認 ---------------------------------------------------------


covid_19 |> dplyr::group_by(性別) |> dplyr::summarize()


## 性別のクリーニング ---------------------------------------------------------------


covid_19_edited <-
  covid_19 |>  
  dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) |>
  dplyr::mutate(
    sex = dplyr::case_when(
      stringr::str_detect(性別, "男") ~ "男性", 
      stringr::str_detect(性別, "女") ~ "女性", 
      stringr::str_detect(性別, "⼥") ~ "女性", 
      TRUE ~ "Unknown")
  )

covid_19_edited |> dplyr::group_by(性別, sex) |> dplyr::summarize()

## 性別を修正した性別プロット -------------------------------------------------------
covid_19_edited |> 
  dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) |> 
  ggplot2::ggplot() + 
  ggplot2::aes(x=days, fill = sex) +
  ggplot2::geom_histogram(binwidth =1) + 
  ggplot2::theme_classic()


# 週毎の年齢別感染者数の作成と散布図プロット -------------------------

covid_19_edited |> 
  dplyr::mutate(week = format(days, "%W")) |> 
  dplyr::mutate(ages = dplyr::recode(年代, "0-10" = "0", "不明" = "Unknown", "100" = "99+", .missing = "NA")) |> 
  dplyr::group_by(ages,week) |> 
  dplyr::mutate(case = dplyr::n()) |> 
  ggplot2::ggplot(ggplot2::aes(x=week, y=ages, size = case)) +   ggplot2::geom_point(color = "black", fill = "lightgray") +  ggplot2::theme_classic()



## 年代の書き換え -----------------------------------------------------------------


covid_19_edited <-
  covid_19_edited |>    
  dplyr::mutate(
    ages = dplyr::case_when(
      stringr::str_detect(年代, "10") ~ "000 - 010",
      stringr::str_detect(年代, "20") ~ "010 - 020",
      stringr::str_detect(年代, "30") ~ "020 - 030",
      stringr::str_detect(年代, "40") ~ "030 - 040",
      stringr::str_detect(年代, "50") ~ "040 - 050",
      stringr::str_detect(年代, "60") ~ "050 - 060",
      stringr::str_detect(年代, "70") ~ "060 - 070",
      stringr::str_detect(年代, "80") ~ "070 - 080",
      stringr::str_detect(年代, "90") ~ "080 - 090", 
      TRUE ~ "Unknown"
    ),
    ages = ifelse(stringr::str_detect(年代, "100"), " 100 - ", ages),
    week = format(days, "%W")
  ) 

## 感染者数に応じて色を塗り分け --------------------------------------
covid_19_edited |> 
  dplyr::group_by(ages,week) |> 
  dplyr::mutate(case = dplyr::n()) |>
  ggplot2::ggplot() +
  ggplot2::aes(x=week, y=ages, size = case, color = case) +
  ggplot2::geom_point() +
  ggplot2::theme_classic() +
  ggplot2::scale_colour_gradient(low = "green", high = "red", na.value = NA)


## 都道府県毎プロット ---------------------------------------------------------------


covid_19_edited |> 
  dplyr::rename(pref = "Residential Pref") |>
  dplyr::group_by(ages,week) |> 
  dplyr::mutate(case = dplyr::n()) |>
  ggplot2::ggplot() +
  ggplot2::aes(x=week, y=ages, size = case, color = case) +
  ggplot2::geom_point() +
  ggplot2::theme_classic() +
  ggplot2::scale_colour_gradient(low = "green", high = "red", na.value = NA) +
  ggplot2::facet_wrap(~ pref, nrow = 5)

# 月ごとの感染者の平均年代を作成するコード -----------------------
covid_19 |> 
  dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) |> 
  tidyr::drop_na(days) |> 
  dplyr::mutate(month = format(days, "%m")) |> 
  dplyr::mutate(ages = as.numeric(年代)) |> 
  dplyr::group_by(month) |> 
  dplyr::summarize(
    mean = mean(ages, na.rm = TRUE), 
    median = median(ages, na.rm = TRUE)
  ) 


# 居住地域毎の感染者比率のランキング -------------------------------------------------------

covid_19 |> 
  dplyr::rename(pref = "Residential Pref") |> 
  dplyr::group_by(pref) |> 
  dplyr::summarize(case = dplyr::n()) |> 
  dplyr::mutate(proportion = case/sum(case)) |> 
  dplyr::arrange(desc(proportion)) |> 
  dplyr::mutate(rank = dplyr::row_number()) |> 
  dplyr::select(rank, proportion, pref)


## ランキングの丸め ----------------------------------------------------------------

covid_19 |> 
  dplyr::rename(pref = "Residential Pref") |> 
  dplyr::group_by(pref) |> 
  dplyr::summarize(case = dplyr::n()) |> 
  dplyr::mutate(proportion = round(case/sum(case), 3)) |>
  dplyr::arrange(desc(proportion)) |> 
  dplyr::mutate(rank = dplyr::row_number()) |> 
  dplyr::select(rank, proportion, pref)


## ランキングのオブジェクトとしての保存 ------------------------------------------------------


ranking_table <-
  covid_19 |> 
  dplyr::rename(pref = "Residential Pref") |> 
  dplyr::group_by(pref) |> 
  dplyr::summarize(case = dplyr::n()) |> 
  dplyr::mutate(proportion = case/sum(case)) |> 
  dplyr::arrange(desc(proportion)) |> 
  dplyr::mutate(rank = dplyr::row_number()) |> 
  dplyr::select(rank, proportion, pref)


## 表の出力 --------------------------------------------------------------------

readr::write_csv(ranking_table, file = "figuretable/covid_19.csv")


# kableExtraのインストール -------------------------------------------------------

install.packages("kableExtra")

# kableExtraによる表の出力 -----------------------------------------------------
ranking_kableExtra <- ranking_table |>
  dplyr::rename(
    "順位" = rank, "感染者率" = proportion, "都道府県" = pref
  ) |>
  kableExtra::kbl() |>
  kableExtra::add_header_above(c("国内新型コロナウイルス感染者数" = 3)) |>
  kableExtra::kable_classic()

ranking_kableExtra


# ライブラリのインストール ------------------------------------------------------------

install.packages(c("magick", "webshot2"))

# kableExtraによるpngファイルの出力 ------------------------------
ranking_kableExtra |>
  kableExtra::save_kable(
    file = "figuretable/table_covid_19.png"
  )





















