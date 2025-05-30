# Sivers LOT Data Summarization

# NOTE: A chunk here is a single device

library(tidyverse)
library(here)
library(devparext)
library(readxl)
library(writexl)
rm(list = ls())

# source the config file
source(here("config_info.R"))

#### LIV DATA SUMMARIZATION ####
df_liv = data.table::fread(here("data/processed", paste0(config_info$lotID, "_combined_LIV.csv")))

df_liv = df_liv |> 
  group_by(filename, lotID, SN, tempC) |> 
  mutate(group_id = cur_group_id()) |> 
  ungroup()

If_vec = c(50, 200, 400)*1e-3
Ix_vec = c(20)*1e-3
Pop_vec = c(80, 100)*1e-3 / config_info$CAL_FACTOR_Pf3 # 0.7 here is the power calibration factor determined by CoC test correlation

chunk_len = 500
idx1 = 1
idx2 = idx1 + chunk_len - 1
max_group_id_liv = max(unique(df_liv$group_id))
append_mode = FALSE
while (TRUE) {
  
  if (idx2 > max_group_id_liv) {
    idx2 = max_group_id_liv
  }
  
  # write message updates to console
  message(paste0("idx1 = ", idx1))
  message(paste0("idx2 = ", idx2))
  
  this_df_liv = df_liv |> 
    filter(group_id %in% idx1:idx2)
  
  df_summary_liv = this_df_liv %>%
    group_by(filename, lotID, SN, tempC) |> 
    summarize_raw_liv_data(If_vec, Ix_vec, Pop_vec, n1_smooth = 5, n2_smooth = 5, n3_smooth = 5, Ik1 = 250, Ik2 = 450) %>%
    ungroup()
  
  df_summary_liv = df_summary_liv %>%
    tidyfast::dt_separate(SN, into = c("waferID", "cellID", "barID", "dieID"), sep = "-", remove = FALSE) %>%
    relocate(waferID:dieID, .after = SN)
  
  message("saving liv summary chunk data to file...")
  data.table::fwrite(
    x = df_summary_liv,
    file = here("data/output", paste0(config_info$lotID, "_summary_liv.csv")),
    append = append_mode
  )
  append_mode = TRUE
  
  idx1 = idx2 + 1
  idx2 = idx1 + chunk_len - 1
  if (idx1 > max_group_id_liv) {
    break
  }
}

rm(df_liv, this_df_liv, df_summary_liv)


#### OSA DATA SUMMARIZATION ####

# from csv
df_osa = data.table::fread(here("data/processed", paste0(config_info$lotID, "_combined_OSA.csv"))) |>
  rename(If_osa = If)

df_osa = df_osa |> 
  group_by(lotID, SN, tempC) |> 
  mutate(group_id = cur_group_id()) |> 
  ungroup()

# chunk_len = 1000
chunk_len = 300
idx1 = 1
idx2 = idx1 + chunk_len - 1
max_group_id_osa = max(unique(df_osa$group_id))
append_mode = FALSE
while (TRUE) {
  
  if (idx2 > max_group_id_osa) {
    idx2 = max_group_id_osa
  }
  
  # write message updates to console
  message(paste0("idx1 = ", idx1))
  message(paste0("idx2 = ", idx2))
  
  this_df_osa = df_osa |> 
    filter(group_id %in% idx1:idx2)
  
  df_summary_osa = this_df_osa %>%
    # rename(If_osa = If) |>
    group_by(lotID, SN, tempC, If_osa) |>
    summarize(Lp = extract_peak_wav(wavelength, power),
              Pp = extract_peak_power(power),
              SMSR = extract_smsr(wavelength, power),
              BW20dB = extract_bw20dB(wavelength, power),
              .groups = "drop") %>%
    ungroup() |>
    mutate(If_osa = If_osa / 1e-3)
  
  df_summary_osa_wide = df_summary_osa %>%
    group_by(SN, tempC) %>%
    mutate(If_index = seq_along(If_osa)) %>%
    ungroup() %>%
    pivot_wider(names_from = If_index,
                values_from = c(If_osa, Lp, Pp, SMSR, BW20dB),
                names_sep = "")
  
  
  
  # debug duplicate SN rows
  dup_rows = df_summary_osa_wide |> 
    group_by(SN) |> 
    filter(n() > 1) |> 
    nrow()
  
  
  message("saving osa summary chunk data to file...\n")
  data.table::fwrite(
    x = df_summary_osa_wide,
    file = here("data/output", paste0(config_info$lotID, "_summary_osa.csv")),
    append = append_mode
  )
  append_mode = TRUE
  
  idx1 = idx2 + 1
  idx2 = idx1 + chunk_len - 1
  if (idx1 > max_group_id_osa) {
    break
  }
  
}

rm(df_osa, this_df_osa, df_summary_osa_wide)

#### COMBINE SUMMARIES ####
df_summary_liv = data.table::fread(here("data/output", paste0(config_info$lotID, "_summary_liv.csv")))
df_summary_osa = data.table::fread(here("data/output", paste0(config_info$lotID, "_summary_osa.csv")))
df_summary = full_join(df_summary_liv, df_summary_osa)
df_summary = df_summary |> mutate(across(where(is.numeric), \(x) round(x, 4)))
data.table::fwrite(df_summary, here("data/output", paste0(config_info$lotID, "_summary.csv")))
