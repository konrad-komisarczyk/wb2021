source("libraries.R")

process_tongji <- function(filename) {
  readxl::read_xlsx(str_c("raw_data/", filename, ".xlsx")) %>%
    rename( # rename for consistency and easier access
      id = PATIENT_ID,
      LDH = 'Lactate dehydrogenase',
      hsCRP = 'Hypersensitive c-reactive protein',
      lymphocytes = '(%)lymphocyte',
      admission = 'Admission time',
      discharge = 'Discharge time'
    ) %>%
    fill(id) %>%
    group_by(id, admission, discharge, outcome) %>%
    summarise(
      LDH_first = first(na.omit(LDH)),
      LDH_last = last(na.omit(LDH)),
      hsCRP_first = first(na.omit(hsCRP)),
      hsCRP_last = last(na.omit(hsCRP)),
      lymphocytes_first = first(na.omit(lymphocytes)),
      lymphocytes_last = last(na.omit(lymphocytes)),
    ) %>%
    write.csv(file=str_c("data/", filename, ".csv"), row.names = F)
}

process_antonius <- function(filename) {
  st_antonius_types = c("numeric", "text", "date", "date", "date",
                        "text", "text", "date", "numeric", "date",
                        "numeric", "numeric", "numeric", "numeric", "numeric")

  suppressWarnings( # expected warnings, caused by non-date strings in date columns
    readxl::read_xlsx(str_c("raw_data/", filename, ".xlsx"), col_types = st_antonius_types)
    ) %>%
    mutate(id = 1:305) %>%
    rename(
      admission = "Date of admission",
      discharge = "Date of discharge",
      outcome = "Survival/death", 
      LDH = "LD", 
      hsCRP = "CRP", 
      lymphocytes = "Percentage lymphocytes"
    ) %>%
    mutate(
      LDH_first = na.omit(LDH),
      LDH_last = na.omit(LDH),
      hsCRP_first = na.omit(hsCRP),
      hsCRP_last = na.omit(hsCRP),
      lymphocytes_first = na.omit(lymphocytes),
      lymphocytes_last = na.omit(lymphocytes),
    ) %>%
    select(id, admission, discharge, outcome, 
           LDH_first, LDH_last, hsCRP_first, hsCRP_last, lymphocytes_first, lymphocytes_last) %>%
    mutate(outcome = ifelse(outcome == "Alive", 0, 1))
    write.csv(file = str_c("data/", filename, ".csv"), row.names = F)
}
  


process_tongji("Tongji_375_CN")
process_tongji("Tongji_110_CN")
process_antonius("St_Antonius_NL")
