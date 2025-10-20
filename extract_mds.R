
## extract mds into dataframes

## package yaml always threw an error I didn't get... so here we are.

## Problem: unterschiedliche file encodings, nicht alle sind utf-8
# im 1. run die fails rausgezogen und die encodings einzeln nachgeschlagen



#path <- "./IANUS-LV-MDs/WiSe23-24/" -> erfolgreich durchgelaufen

path <-  "./IANUS-LV-MDs/SoSe24/"

fn <- list.files(path = path)

wise23 <- data.frame("einrichtung" = as.character(),
                        "inhalte" = as.character(),
                        "lehrende" = as.character(),
                        "link" = as.character(),
                        "lv.typ" = as.character(),
                        "stadt"  = as.character(),
                        "studiengang"  = as.character(),
                        "title"  = as.character(),
                        "zielgruppe"  = as.character(),
                        "sem" = as.character(),
                     "dat" = as.character())

#enc <- readr::read_delim("./IANUS-LV-MDs/wise_23-24_failed_mds_encodings.txt", delim = " - ", skip_empty_rows = TRUE)

enc <- readr::read_delim("./IANUS-LV-MDs/sose24_failed_mds_encodings.txt", delim = " - ", skip_empty_rows = TRUE)

for (j in fn) {

if (j %in% enc$dat) {
  
d <- read.delim(paste0(path, j), sep = "\n", fileEncoding = enc$encoding[enc$dat == j])

} else {
  d <- read.delim(paste0(path, j), sep = "\n")
}

colnames(d) <- "i"
 
d <- d |> dplyr::filter(i != "---")

library(dplyr)
library(stringr)

# tabellenspaltenkopf vor die inhalte schreiben, um sie später teilen zu können. unschön epezifisch, aber klappt insgesamt

d <- d |>
  dplyr::mutate(i = case_when(
    i == "  - BA" ~ "zielgruppe: BA",
    i == "  - MA" ~ "zielgruppe: MA",
    i == "  - PHD" ~ "zielgruppe: PHD",
    TRUE ~ i
  )) 

d <- d |>
  mutate(v2 = gsub(pattern = "  -", x = i, replacement = "inhalte:")) |> 
  filter(i != "inhalte:") |>       # leere zeilen raus
  filter(i != "zielgruppe:") |>
  filter(!str_detect(v2, "beschreibung:")) |>
  filter(str_detect(v2, pattern = ":")) # beschreibung raus -> ist zu komplex, mit unterschiedlichen Zeilenangaben...

d2 <- data.frame(str_split_fixed(d$v2, pattern = ": ", n = 2) )

d3 <- d2 |>
  group_by(X1) |>
  summarise(inh = paste(X2, collapse = ", ") )

library(tidyr) # für pivot

d4 <- d3 |>
  pivot_wider(names_from = X1,
              values_from = inh)

d4$sem <- "SoSe 2024"

if("lv-typ" %in% colnames(d4)) {
  
  d4 <- d4 |>
  dplyr::rename(lv.typ = `lv-typ`)
  
}

d4$dat <- paste(j)

wise23 <- plyr::rbind.fill(wise23, d4)

}

#readODS::write_ods(wise23, "./IANUS-LV-MDs/wise23-24.ods")

readODS::write_ods(wise23, "./IANUS-LV-MDs/sose2024.ods")


## check for those that didn't work

fn_fail <- fn[!(fn %in% wise23$dat)] 
## saarbrücken hat vmtl iso 8859-15

write(fn_fail, file = paste0("./IANUS-LV-MDs/failed_mds.txt") )

      
