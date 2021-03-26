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

st_antonius_types = c("numeric", "text", "date", "date", "date",
                      "text", "text", "date", "numeric", "date",
                      "numeric", "numeric", "numeric", "numeric", "numeric")

suppressWarnings( # expected warnings, caused by disgusting non-date strings in date columns
  readxl::read_xlsx("raw_data/St_Antonius_NL.xlsx", col_types = st_antonius_types)
  )


process_tongji("Tongji_375_CN")
process_tongji("Tongji_110_CN")
