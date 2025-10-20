
## extract mds into dataframes

## package yaml always threw an error I didn't get... so here we are.

path <- "./IANUS-LV-MDs/WiSe23-24/"

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
                        "sem" = as.character() )

for (j in fn) {

d <-readr::read_delim(paste0(path, j), delim = "\n")

colnames(d) <- "i"

d <- d[d$i != "---", ]

library(dplyr)
library(stringr)

# tabellenspaltenkopf vor die inhalte schreiben, um sie später teilen zu können. unschön epezifisch, aber klappt insgesamt

d <- d |>
  mutate(i = case_when(
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

d4$sem <- "WiSe 2023/24"

if (exists("d4$'lv-typ'") ) {

d4 <- d4 |>
  rename(lv.typ = `lv-typ`)
  
}

wise23 <- plyr::rbind.fill(wise23, d4)

}
