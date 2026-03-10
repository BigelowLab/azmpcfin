suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

path = "/mnt/s1/projects/ecocast/coredata/dfompo/AZMP_data"
fileA = "CalanusAbundance_m2_CAN_data_1999_2024.txt"
fileB = "CalanusAbundance_m2_CAN_data.txt"

a = readr::read_tsv(file.path(path, fileA), show_col_types = FALSE) |>
  mutate(year = sprintf("%0.4i", as.numeric(year)),
         month = sprintf("%0.2i", as.numeric(month)),
         day = sprintf("%0.2i", as.numeric(day)),
         .date = as.Date(paste(year, month, day, sep = "-")))
b = readr::read_tsv(file.path(path, fileB), show_col_types = FALSE) |>
  mutate(year = sprintf("%0.4i", as.numeric(year)),
         month = sprintf("%0.2i", as.numeric(month)),
         day = sprintf("%0.2i", as.numeric(day)),
         .date = as.Date(paste(year, month, day, sep = "-")))

cat("A adds", nrow(a) - nrow(b), "records\n")

range(a$.date)
range(b$.date)