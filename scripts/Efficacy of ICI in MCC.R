library(tidyverse)
library(here)
library(gt)
library(gtsummary)
library(webshot2)
source("/Users/davidmiller/Dropbox (Partners HealthCare)/The Miller Lab Website/slides/MCC_Symposium_05-03-2024/scripts/gt plus biblio.R")

biblio <- bibtex::read.bib("/Users/davidmiller/Dropbox (Partners HealthCare)/The Miller Lab Website/slides/MCC_Symposium_05-03-2024/references.bib")

# Avelumab

javelin_2nd_line <- tribble(
  ~Therapy, ~Study, ~Target, ~`Tx Line`, ~N, ~`Objective Response`, ~`Median PFS`, ~`Median OS`,
  "Avelumab", "Javelin", "PD-L1", "≥2", "88", "33", "3", "12.6"
)


javelin_1st_line <- tribble(
  ~Therapy, ~Study, ~Target, ~`Tx Line`, ~N, ~`Objective Response`, ~`Median PFS`, ~`Median OS`,
  "Avelumab", "Javelin", "PD-L1", "1", "116", "39.7", "4.1", "20.3"
)

# Pembrolizumab
pembro_1st_line <- tribble(
  ~Therapy, ~Study, ~Target, ~`Tx Line`, ~N, ~`Objective Response`, ~`Median PFS`, ~`Median OS`,
  "Pembrolizumab", "CITN-09", "PD-1", "1", "50",  "58", "16.8", "NR"
)


# Nivolumab
nivoluamb_1st_line <- tribble(
  ~Therapy, ~Study, ~Target, ~`Tx Line`, ~N, ~`Objective Response`, ~`Median PFS`, ~`Median OS`,
  "Nivolumab", "CheckMate-358", "PD-1", "1", "15","73", "24.8", "NR"
)

nivoluamb_2nd_line <- tribble(
  ~Therapy, ~Study, ~Target, ~`Tx Line`, ~N, ~`Objective Response`, ~`Median PFS`, ~`Median OS`,
  "Nivolumab", "CheckMate-358", "PD-1", "≥2", "10","50", "21.3", "NR"
)

# Ipi/Nivo
ipi_nivo_1st_line <-  tribble(
  ~Therapy, ~Study, ~Target, ~`Tx Line`, ~N, ~`Objective Response`, ~`Median PFS`, ~`Median OS`,
  "Nivolumab + Ipilimumab", "CheckMate-358", "PD-1 + CTLA4", "1", "33","63.6", "15.4", "35.58"
)

ipi_nivo_1st_line_moffitt <-  tribble(
  ~Therapy, ~Study, ~Target, ~`Tx Line`, ~N, ~`Objective Response`, ~`Median PFS`, ~`Median OS`,
  "Nivolumab + Ipilimumab", "Moffitt IST", "PD-1 + CTLA4", "1", "13","100", "NR", "NR"
)

ipi_nivo_2nd_line <-  tribble(
  ~Therapy, ~Study, ~Target, ~`Tx Line`, ~N, ~`Objective Response`, ~`Median PFS`, ~`Median OS`,
  "Nivolumab + Ipilimumab", "CheckMate-358", "PD-1 + CTLA4", "≥2", "10","40", "2.74", "8.56"
)

ipi_nivo_2nd_line_moffitt <-  tribble(
  ~Therapy, ~Study, ~Target, ~`Tx Line`, ~N, ~`Objective Response`, ~`Median PFS`, ~`Median OS`,
  "Nivolumab + Ipilimumab", "Moffitt IST", "PD-1 + CTLA4", "≥2", "12","42", "4.2", "14.9"
)

ipi_nivo_2nd_line_mgb <-  tribble(
  ~Therapy, ~Study, ~Target, ~`Tx Line`, ~N, ~`Objective Response`, ~`Median PFS`, ~`Median OS`,
  "Nivolumab + Ipilimumab", "MGB\nRetrospective", "PD-1 + CTLA4", "≥2", "13","0", "1.3", "4.7"
)

# Retifanlimab

retifanlimab_1st_line <-  tribble(
  ~Therapy, ~Study, ~Target, ~`Tx Line`, ~N, ~`Objective Response`, ~`Median PFS`, ~`Median OS`,
  "Retifanlimab", "POD1UM-201", "PD-1", "1", "65","52", "NA", "NA"
)

# Combine Rows
mcc_systemic_therapy <- 
  javelin_1st_line |> 
  add_row(javelin_2nd_line) |> 
  add_row(pembro_1st_line) |> 
  add_row(nivoluamb_1st_line) |> 
  add_row(nivoluamb_2nd_line) |> 
  add_row(ipi_nivo_1st_line) |> 
  add_row(ipi_nivo_1st_line_moffitt) |> 
  add_row(ipi_nivo_2nd_line) |> 
  add_row(ipi_nivo_2nd_line_moffitt) |> 
  # add_row(ipi_nivo_2nd_line_mgb) |> 
  add_row(retifanlimab_1st_line) 

mcc_systemic_therapy$`Objective Response` <- 
  round(as.double(mcc_systemic_therapy$`Objective Response`),0)

mcc_systemic_therapy <- mcc_systemic_therapy |> 
  select(-Target)

mcc_systemic_therapy_gt <- 
  create_mcc_systemic_therapy_table(
    mcc_systemic_therapy, biblio = biblio
  )
mcc_systemic_therapy_gt


# First line only
mcc_systemic_1L <- 
  mcc_systemic_therapy |> 
  dplyr::filter(mcc_systemic_therapy$`Tx Line` == "1")
mcc_systemic_1L_gt <- 
  create_mcc_systemic_therapy_table_no_biblio(
    mcc_systemic_1L
  )
mcc_systemic_1L_gt
# sort by N
mcc_systemic_1L_n <- mcc_systemic_1L |> mutate(N = as.integer(N)) |> dplyr::arrange((N))
mcc_systemic_1L_n
mcc_systemic_1L_n_gt <- 
  create_mcc_systemic_therapy_table_no_biblio(
    mcc_systemic_1L_n
  )
mcc_systemic_1L_n_gt
# Add row of weighted average response rate
responders <- 
  mcc_systemic_1L |> 
  mutate(
    N = round(as.double(N),0),
    `Objective Response` = round(as.double(`Objective Response`),0),
    responders = (N * `Objective Response`/100),
    responders_round = round(responders,0),
    responders_total = sum(responders_round),
    n_total = sum(N),
    rr_avg = round((responders_total/n_total)*100,0)
  )
orr_avg <- tribble(
  ~Therapy, ~Study, ~Target, ~`Tx Line`, ~N, ~`Objective Response`, ~`Median PFS`, ~`Median OS`,
  "Aggregate", "Aggregate", "PD-1 +/- CTLA4", "1", responders$n_total[1], responders$rr_avg[1], "NA", "NA" 
)

mcc_systemic_1L_avg <- 
  mcc_systemic_1L |> mutate(N = as.double(N),`Objective Response` = as.double(`Objective Response`)) |> 
  add_row(orr_avg |> select(-Target))

mcc_systemic_1L_avg_gt <- 
  create_mcc_systemic_therapy_table_no_biblio(
    mcc_systemic_1L_avg
  )

mcc_systemic_1L_avg_gt  <- 
  mcc_systemic_1L_avg_gt |>   
  tab_style(
    style = list(
      cell_fill(color = "#C24641"),
      cell_text(color = "white")),
    locations = cells_body(rows = `Therapy` == "Aggregate")
  )
mcc_systemic_1L_avg_gt 

##### No ipi/nivo
responders_no_ipi <- 
  mcc_systemic_1L |> 
  dplyr::filter(Therapy != "Nivolumab + Ipilimumab") %>% 
  mutate(
    N = round(as.double(N),0),
    `Objective Response` = round(as.double(`Objective Response`),0),
    responders = (N * `Objective Response`/100),
    responders_round = round(responders,0),
    responders_total = sum(responders_round),
    n_total = sum(N),
    rr_avg = round((responders_total/n_total)*100,0)
  )
orr_avg_no_ipi <- tribble(
  ~Therapy, ~Study, ~Target, ~`Tx Line`, ~N, ~`Objective Response`, ~`Median PFS`, ~`Median OS`,
  "Aggregate", "Aggregate", "PD-1 +/- CTLA4", "1", responders_no_ipi$n_total[1], responders_no_ipi$rr_avg[1], "NA", "NA" 
)

mcc_systemic_1L_no_ipi <- 
  mcc_systemic_1L |> 
  dplyr::filter(Therapy != "Nivolumab + Ipilimumab")

mcc_systemic_1L <- 
  mcc_systemic_therapy |> 
  dplyr::filter(mcc_systemic_therapy$`Tx Line` == "1")

mcc_systemic_1L_no_ipi_gt <- 
  create_mcc_systemic_therapy_table_no_biblio(
    mcc_systemic_1L_no_ipi
  )
mcc_systemic_1L_no_ipi_gt


mcc_systemic_1L_avg_no_ipi <- 
  mcc_systemic_1L_no_ipi |> 
  mutate(N = as.double(N),`Objective Response` = as.double(`Objective Response`)) |> 
  add_row(orr_avg_no_ipi |> select(-Target))

mcc_systemic_1L_avg_gt_no_ipi <- 
  create_mcc_systemic_therapy_table_no_biblio(
    mcc_systemic_1L_avg_no_ipi
  )

mcc_systemic_1L_avg_gt_no_ipi  <- 
  mcc_systemic_1L_avg_gt_no_ipi |>   
  tab_style(
    style = list(
      cell_fill(color = "#C24641"),
      cell_text(color = "white")),
    locations = cells_body(rows = `Therapy` == "Aggregate")
  )
mcc_systemic_1L_avg_gt_no_ipi

