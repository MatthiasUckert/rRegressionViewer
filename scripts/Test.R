library(tidyverse)
.path_template <- "/Users/matthiasuckert/Dropbox/MyPapers/TerminologyPaper/TerminologyPaper/TableTemplatesTest.xlsx"
.path_regressions <- "/Users/matthiasuckert/RProjects/Projects/pTerminology/2_output/31-Regressions/Output/FinalRegressions.rds"

run_regression_comparison(
  .path = "/Users/matthiasuckert/RProjects/Projects/pTerminology/2_output/31-Regressions/Output/FinalRegressions.rds",
  .dir = "/Users/matthiasuckert/Downloads/TEST"
)


.path_template <- "/Users/matthiasuckert/Dropbox/MyPapers/TerminologyPaper/TerminologyPaper/TableTemplatesTest.xlsx"
# Get all sheet names
wb <- openxlsx2::wb_load(.path_template)
sheet_names <- openxlsx2::wb_get_sheet_names(wb)

tab_ <- openxlsx2::wb_to_df(
  wb,
  sheet = sheet_names[1],
  col_names = TRUE,
  start_row = 1
) %>% tibble::as_tibble(.name_repair = "unique_quiet")

cols_ <- tab_ %>% 
  dplyr::filter(ID == "C") %>% 
  dplyr::select(dplyr::starts_with("C")) %>% 
  tidyr::pivot_longer(dplyr::everything(), names_to = "ColName", values_to = "TabName") %>% 
  dplyr::left_join(
    y = tibble::tibble(
      ColNum = seq_len(ncol(tab_)),
      ColName = colnames(tab_)
    ),
    by = dplyr::join_by(ColName)
  )

rows_ <- tibble::tibble(
  RowNum = seq_len(nrow(tab_)),
  RowName = tab_$V
)

tibble::as_tibble(which(tab_ == "_", arr.ind = TRUE)) %>% 
  dplyr::rename(RowNum = row, ColNum = col) %>% 
  dplyr::left_join(cols_, by = dplyr::join_by(ColNum)) %>% 
  dplyr::left_join(rows_, dplyr::join_by(RowNum))
