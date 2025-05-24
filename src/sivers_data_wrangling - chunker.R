# Combine Sivers bar-tester raw data into single csv files for further
# processing Note: A .Renviron file has been created to set R_MAX_VSIZE = 20GB
# so we don't reach the memory limit. This was set by experimenting on my 16GB
# M4 Macbook Pro. Ref: https://www.carleton.edu/its/blog/dealing-with-a-vector-memory-exhausted-error-in-r/

library(tidyverse)
library(readxl)
rm(list = ls())

# source the EEVEE mask decoder and helper functions
source("/Users/brianpile/POET Technologies Dropbox/Brian Pile/Brian Pile/R_scripts/POET LD maskset decoders/decode_eevee.R")
source("./config_info.R")

# input_data_path = "/Users/brianpile/POET Technologies Dropbox/Brian Pile/1) Test/1.4) Outsource (dropbox)/Sivers/P10515"
file_list = list.files(path = config_info$input_data_path,
                       full.names = TRUE,
                       pattern = "_GP[0-9][0-9]_dat\\.xls$",
                       # pattern = "P10515-7_755&756_82A_GP04_dat.xls",
                       recursive = TRUE)

# remove problematic file(s)
problem_files = config_info$problem_files
file_list = setdiff(file_list, problem_files)

chunk_len = 10
idx1 = 1
idx2 = idx1 + chunk_len - 1
append_mode = FALSE
while (TRUE) {
  
  # check if idx2 is greater than file_list length, if it is set it to file_list
  # length
  if (idx2 > length(file_list)) {
    idx2 = length(file_list)
  }
  
  this_file_list = file_list[idx1:idx2]
  
  # write message updates to console
  print(paste0("idx1 = ", idx1))
  print(paste0("idx2 = ", idx2))
  
  #### import L-I data ####
  df_pow_wide = this_file_list |>
    set_names() |>
    map(\(x) read_xls(x, sheet = "Pow", trim_ws = FALSE)) |> 
    list_rbind(names_to = "pathname")
  
  df_pow = df_pow_wide |>
    pivot_longer(
      cols = !c(pathname, If),
      names_to = "dieID",
      values_to = "power"
    ) |> 
    mutate(dieID = sprintf("%02s", dieID))
  
  df_pow = df_pow |>
    select(pathname, dieID, current = If, power) |>
    mutate(
      current = current *1e-3,
      power = power *1e-3
    ) |>
    arrange(pathname, dieID, current) |>
    filter(is.na(power) == FALSE)
  
  df_pow = df_pow |>
    mutate(filename = basename(pathname), .after = pathname) |>
    tidyfast::dt_separate(
      col = filename,
      into = c("lotID", "dummy2", "barID", "dummy3", "dummy4"),
      sep = "_",
      remove = FALSE) |>
    select(-starts_with("dummy")) |> 
    mutate(barID = str_sub(barID, start = 1, end = 2))
  
  #### import V-I data ####
  df_vf_wide = this_file_list |>
    set_names() |>
    map(\(x) read_xls(x, sheet = "Vf", trim_ws = FALSE)) |> 
    list_rbind(names_to = "pathname")
  
  df_vf = df_vf_wide |>
    pivot_longer(
      cols = !c(pathname, If),
      names_to = "dieID",
      values_to = "voltage"
    ) |> 
    mutate(dieID = sprintf("%02s", dieID))
  
  df_vf = df_vf |>
    select(pathname, dieID, current = If, voltage) |>
    mutate(
      current = current * 1e-3
    ) |>
    arrange(pathname, dieID, current) |>
    filter(is.na(voltage) == FALSE)
  
  df_vf = df_vf |>
    mutate(filename = basename(pathname), .after = pathname) |>
    tidyfast::dt_separate(
      col = filename,
      into = c("lotID", "dummy2", "barID", "dummy3", "dummy4"),
      sep = "_",
      remove = FALSE
    ) |>
    select(-starts_with("dummy")) |> 
    mutate(barID = str_sub(barID, start = 1, end = 2))
  
  #### join L-I and V-I data together ####
  df_liv = left_join(df_pow, df_vf) |>
    mutate(
      waferID = config_info$waferID,
      cellID = "NA",
      tempC = 50
    ) |>
    correct_eevee_cellID() |> 
    unite("SN", waferID, cellID, barID, dieID, sep = "-", remove = TRUE) |>
    select(filename, lotID, SN, tempC, current, power, voltage) |>
    as_tibble()
  
  # save to csv file
  print("saving liv chunk data to file...")
  data.table::fwrite(
    x = df_liv,
    file = paste0("./data/", config_info$lotID, "_combined_LIV.csv"),
    append = append_mode
  )
  append_mode = TRUE # on subsequent iteration append to file
  
  # update indices and check if we should break out
  idx1 = idx2 + 1
  idx2 = idx1 + chunk_len - 1
  if (idx1 > length(file_list)) {
    break
  }
  
}

# remove duplicate data
df_liv = data.table::fread(paste0("./data/", config_info$lotID, "_combined_LIV.csv"))
df_liv |> 
  distinct(SN, tempC, current, .keep_all = TRUE) |> 
  data.table::fwrite(file = paste0("./data/", config_info$lotID, "_combined_LIV.csv"))

# clean up
rm(df_liv, df_pow, df_pow_wide, df_vf, df_vf_wide)


##### OSA SECTION ####

chunk_len = 10
idx1 = 1
idx2 = idx1 + chunk_len - 1
append_mode = FALSE
while (TRUE) {
  
  # check if idx2 is greater than file_list length, if it is set it to file_list
  # length
  if (idx2 > length(file_list)) {
    idx2 = length(file_list)
  }
  
  this_file_list = file_list[idx1:idx2]
  
  # write message updates to console
  print(paste0("idx1 = ", idx1))
  print(paste0("idx2 = ", idx2))

  #### import OSA data ####
  df_osa1_wide = this_file_list |>
    set_names() |>
    map(\(x) read_xls(x, sheet = "OSA1", trim_ws = FALSE)) |>
    list_rbind(names_to = "pathname")
  
  df_osa1 = df_osa1_wide |>
    pivot_longer(
      cols = !c(pathname, Wave),
      names_to = "dieID",
      values_to = "power"
    ) |>
    mutate(If = 50e-3, .after = pathname) |>
    mutate(dieID = sprintf("%02s", dieID))
  
  rm(df_osa1_wide)
  
  df_osa2_wide = this_file_list |>
    set_names() |>
    map(\(x) read_xls(x, sheet = "OSA2", trim_ws = FALSE)) |>
    list_rbind(names_to = "pathname")
  
  df_osa2 = df_osa2_wide |>
    pivot_longer(
      cols = !c(pathname, Wave),
      names_to = "dieID",
      values_to = "power"
    ) |>
    mutate(If = 200e-3, .after = pathname) |>
    mutate(dieID = sprintf("%02s", dieID))
  
  rm(df_osa2_wide)
  
  df_osa3_wide = this_file_list |>
    set_names() |>
    map(\(x) read_xls(x, sheet = "OSA3", trim_ws = FALSE)) |>
    list_rbind(names_to = "pathname")
  
  df_osa3 = df_osa3_wide |>
    pivot_longer(
      cols = !c(pathname, Wave),
      names_to = "dieID",
      values_to = "power"
    ) |>
    mutate(If = 400e-3, .after = pathname) |>
    mutate(dieID = sprintf("%02s", dieID))
  
  rm(df_osa3_wide)
  
  df_osa = bind_rows(df_osa1, df_osa2, df_osa3)
  
  df_osa = df_osa |>
    filter(is.na(power) == FALSE)
  
  df_osa = df_osa |>
    rename(wavelength = Wave) |>
    mutate(filename = basename(pathname), .after = pathname) |>
    select(-pathname) |>
    tidyfast::dt_separate(
      col = filename,
      into = c("lotID", "dummy2", "barID", "dummy3", "dummy4"),
      sep = "_",
      remove = FALSE
    ) |>
    select(-starts_with("dummy")) |>
    mutate(barID = str_sub(barID, start = 1, end = 2)) |>
    # select(filename, lotID, barID, dieID, If, wavelength, power) |>
    mutate(
      waferID = config_info$waferID,
      cellID = "NA",
      tempC = 50
    ) |>
    correct_eevee_cellID() |>
    unite("SN", waferID, cellID, barID, dieID, sep = "-", remove = TRUE) |>
    # select(filename, lotID, SN, tempC, If, wavelength, power) |>
    select(lotID, SN, tempC, If, wavelength, power) |>
    # arrange(filename, lotID, SN, tempC, If, wavelength, power)
    arrange(lotID, SN, tempC, If, wavelength, power)
  
  # clean up
  rm(list = c("df_osa1", "df_osa2", "df_osa3"))
  
  # save to csv file
  print("saving osa chunk data to file...")
  data.table::fwrite(
    x = df_osa,
    file = paste0("./data/", config_info$lotID, "_combined_OSA.csv"),
    append = append_mode
  )
  append_mode = TRUE # on subsequent iteration append to file
  
  # update indices and check if we should break out
  idx1 = idx2 + 1
  idx2 = idx1 + chunk_len - 1
  if (idx1 > length(file_list)) {
    break
  }
  
}

# remove duplicate data
df_osa = data.table::fread(file = paste0("./data/", config_info$lotID, "_combined_OSA.csv"))
df_osa |> 
  distinct(SN, tempC, If, wavelength, .keep_all = TRUE) |> 
  data.table::fwrite(file = paste0("./data/", config_info$lotID, "_combined_OSA.csv"))
