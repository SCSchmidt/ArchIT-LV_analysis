 ## extract infos from old webpage


### SoSe 2019
url <- "https://web.archive.org/web/20190815131304/https://www.ianus-fdz.de/it-empfehlungen/lehrangebote"

library(rvest)

df <- url |>
  read_html()  |>
  html_nodes("table")  |>
  html_table(fill = T) 


sose_2019 <- df[[1]]

sose_2019$Semester <- "SoSe 2019"

library(stringr)
library(dplyr)

sose_2019 <- sose_2019 |>
  mutate(Veranstaltungstyp = case_when(
    str_detect(`Veranstaltungstyp/ Zielgruppe`, "Seminar") ~ "Seminar",
    str_detect(`Veranstaltungstyp/ Zielgruppe`, "Übung") ~ "Übung",
    str_detect(`Veranstaltungstyp/ Zielgruppe`, "Vorles") ~ "Vorlesung",
    str_detect(`Veranstaltungstyp/ Zielgruppe`, "Prakt") ~ "Praktikum",
    str_detect(`Veranstaltungstyp/ Zielgruppe`, "Worksh") ~ "Workshop",
    str_detect(`Veranstaltungstyp/ Zielgruppe`, "Tutor") ~ "Tutorium" ) )

sose_2019 <- sose_2019 |>
  mutate(Zielgruppe = case_when(
    str_detect(`Veranstaltungstyp/ Zielgruppe`, "BA, MA, Doktoranden") ~ "BA, MA, PHD",
    str_detect(`Veranstaltungstyp/ Zielgruppe`, "MA, Doktoranden, Sonstige") ~ "MA, PHD, Sonstige",
    str_detect(`Veranstaltungstyp/ Zielgruppe`, "MA, Doktoranden") ~ "MA, PHD",
    str_detect(`Veranstaltungstyp/ Zielgruppe`, "BA, MA") ~ "BA, MA",
    str_detect(`Veranstaltungstyp/ Zielgruppe`, "BA") ~ "BA",
    str_detect(`Veranstaltungstyp/ Zielgruppe`, "MA") ~ "MA"
  ))

write_ods(sose_2019, "IANUS_LV_SoSe2019.ods")

### WiSe 2021/22

url <- "https://web.archive.org/web/20211129000602/https://www.ianus-fdz.de/it-empfehlungen/lehrangebote"


df <- url |>
  read_html()  |>
  html_nodes("table")  |>
  html_table(fill = T) 


wise_2021 <- df[[2]]


wise_2021 <- wise_2021 |>
  mutate(Veranstaltungstyp = case_when(
    str_detect(`LV-typ/ Zielgruppe`, "Seminar") ~ "Seminar",
    str_detect(`LV-typ/ Zielgruppe`, "Übung") ~ "Übung",
    str_detect(`LV-typ/ Zielgruppe`, "Vorles") ~ "Vorlesung",
    str_detect(`LV-typ/ Zielgruppe`, "Prakt") ~ "Praktikum",
    str_detect(`LV-typ/ Zielgruppe`, "Worksh") ~ "Workshop",
    str_detect(`LV-typ/ Zielgruppe`, "Kollo") ~ "Kolloquium",
    str_detect(`LV-typ/ Zielgruppe`, "Tutor") ~ "Tutorium" ) )

wise_2021 <- wise_2021 |>
  mutate(Zielgruppe =  `LV-typ/ Zielgruppe`)


wise_2021$Zielgruppe <-  gsub(x = wise_2021$Zielgruppe , pattern = "Seminar", replacement = "")
wise_2021$Zielgruppe <-  gsub(x = wise_2021$Zielgruppe , pattern = "Übung", replacement = "")
wise_2021$Zielgruppe <-  gsub(x = wise_2021$Zielgruppe , pattern = "Vorlesung", replacement = "")
wise_2021$Zielgruppe <-  gsub(x = wise_2021$Zielgruppe , pattern = "Praktikum", replacement = "")
wise_2021$Zielgruppe <-  gsub(x = wise_2021$Zielgruppe , pattern = "Workshop", replacement = "")
wise_2021$Zielgruppe <-  gsub(x = wise_2021$Zielgruppe , pattern = "Kolloquium", replacement = "")
wise_2021$Zielgruppe <-  gsub(x = wise_2021$Zielgruppe , pattern = "Tutorium", replacement = "")

wise_2021$Zielgruppe <-  gsub(x = wise_2021$Zielgruppe , pattern = "Doktoranden/innen", replacement = "PHD")

wise_2021$Semester <- "WiSe 2021/22"

write_ods(wise_2021, "IANUS_LV_WiSe_2021-22.ods")


### WiSe 2022/23


url <- "https://web.archive.org/web/20221110133403/https://ianus-fdz.de/it-empfehlungen/lehrangebote"


df <- url |>
  read_html()  |>
  html_nodes("table")  |>
  html_table(fill = T) 


wise_2022 <- df[[2]]


wise_2022 <- wise_2022 |>
  mutate(Veranstaltungstyp = case_when(
    str_detect(`LV-typ/ Zielgruppe`, "Seminar|seminar") ~ "Seminar",
    str_detect(`LV-typ/ Zielgruppe`, "Übung") ~ "Übung",
    str_detect(`LV-typ/ Zielgruppe`, "Vorles") ~ "Vorlesung",
    str_detect(`LV-typ/ Zielgruppe`, "Prakt") ~ "Praktikum",
    str_detect(`LV-typ/ Zielgruppe`, "Worksh") ~ "Workshop",
    str_detect(`LV-typ/ Zielgruppe`, "Kollo") ~ "Kolloquium",
    str_detect(`LV-typ/ Zielgruppe`, "Tutor") ~ "Tutorium" ) )

wise_2022 <- wise_2022 |>
  mutate(Zielgruppe =  `LV-typ/ Zielgruppe`)

wise_2022$Zielgruppe <-  gsub(x = wise_2022$Zielgruppe , pattern = "Seminar", replacement = "")
wise_2022$Zielgruppe <-  gsub(x = wise_2022$Zielgruppe , pattern = "Hauptseminar", replacement = "")
wise_2022$Zielgruppe <-  gsub(x = wise_2022$Zielgruppe , pattern = "Übung", replacement = "")
wise_2022$Zielgruppe <-  gsub(x = wise_2022$Zielgruppe , pattern = "Vorlesung", replacement = "")
wise_2022$Zielgruppe <-  gsub(x = wise_2022$Zielgruppe , pattern = "Praktikum", replacement = "")
wise_2022$Zielgruppe <-  gsub(x = wise_2022$Zielgruppe , pattern = "Workshop", replacement = "")
wise_2022$Zielgruppe <-  gsub(x = wise_2022$Zielgruppe , pattern = "Kolloquium", replacement = "")
wise_2022$Zielgruppe <-  gsub(x = wise_2022$Zielgruppe , pattern = "Tutorium", replacement = "")

wise_2022$Zielgruppe <-  gsub(x = wise_2022$Zielgruppe , pattern = "Doktoranden/innen", replacement = "PHD")

wise_2022$Semester <- "WiSe 2022/23"

write_ods(wise_2022, "IANUS_LV_WiSe_2022-23.ods")

