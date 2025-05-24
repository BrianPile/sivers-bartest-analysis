# ALMAE requested some typical LIV curves from each wafer. So we will:
# 1. sample some devices that represent median performance for output power
# 2. save the data to individual csv files

library(tidyverse)
library(here)
source(here("./config_info.R"))

# calibration constants
# TODO: put calibration constants inside config file?
CAL_FACTOR_Pf3 = 0.7
CAL_OFFSET_Lp3 = -3.578

output_path = "/Users/brianpile/POET Technologies Dropbox/Brian Pile/Brian Pile/Lasers/Almae/C-Band/Phase 3/20250507 - phase3 cband data package for Almae/LIV files"

df_summary = read_csv(file = here(str_glue("./data/EEVEE-{config_info$lotID}-{config_info$waferID}-summary-binned-data.csv")))
df_liv = data.table::fread(here(str_glue("./data/{config_info$lotID}_combined_LIV.csv"))) |> as_tibble()

# get some medians
med_Pf3_cal = median(df_summary$Pf3_cal, na.rm = TRUE)
med_Ith1d = median(df_summary$Ith1d, na.rm = TRUE)
 
# get SN of curves that represent median results
SN_typical = df_summary |> 
  filter(
    Pf3_cal |> between(med_Pf3_cal-2, med_Pf3_cal+2),
    Ith1d |> between(med_Ith1d-1, med_Ith1d+1)) |> 
  pull(SN) |> 
  sample(5)

# plot LI data for device SN with the median results
df_liv |> 
  filter(SN %in% SN_typical) |> 
  ggplot(aes(x = current/1e-3, y = power/1e-3, group = SN)) +
  geom_path()

# save csv files
df_liv |> 
  filter(SN %in% SN_typical) |> 
  group_by(SN) |> 
  group_split() |> 
  walk(\(dat) {
    # create file name
    SN = dat$SN |> unique()
    fname = str_glue("{SN}-50C-LIV.csv")
    
    # add calibrated power column
    dat = dat |> mutate(power_cal = power * CAL_FACTOR_Pf3, .after = power)
    
    # write the LIV data to csv file
    write_csv(dat, file = file.path(output_path, fname))
  })

