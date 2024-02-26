# Geocode Practices and PCNs

library(tidyverse)
library(readxl)

fnGeoPracticesAndPCNs <- function(fil_org_map, fil_epcn, sht_pcn_details = 'PCNDetails', fil_pcd, outfile = NULL){
  df_org_map <- read.csv(fil_org_map)
  
  df_pcn_details <- read_excel(fil_epcn, sht_pcn_details) %>% 
    rename_with(.fn = ~c(
      'PCN_CODE', 'PCN_NAME', 'SICBL_CODE', 'SICBL_NAME', 'OPEN_DATE', 'CLOSE_DATE', 
      'ADDRESS_01', 'ADDRESS_02', 'ADDRESS_03', 'ADDRESS_04', 'ADDRESS_05', 'POSTCODE')) %>%
    filter(is.na(CLOSE_DATE)) %>%
    select(PCN_CODE, POSTCODE)
  
  df_pcd <- read.csv(fil_pcd) %>%
    mutate(POSTCODE = pcds, LAT = lat, LNG = long)
  
  df_org_map <- df_org_map %>% 
    select(-c('PUBLICATION', 'EXTRACT_DATE')) %>%
    left_join(df_pcn_details, by = 'PCN_CODE') %>%
    mutate(PCN_POSTCODE = POSTCODE, .after = 'PCN_NAME', .keep = 'unused') %>% 
    left_join(df_pcd %>% mutate(POSTCODE, PRACTICE_LAT = LAT, PRACTICE_LNG = LNG, .keep = 'none'), by = c('PRACTICE_POSTCODE' = 'POSTCODE')) %>%
    left_join(df_pcd %>% mutate(POSTCODE, PCN_LAT = LAT, PCN_LNG = LNG, .keep = 'none'), by = c('PCN_POSTCODE' = 'POSTCODE'))

  if(!is.null(outfile)){
    write.csv(df_org_map, outfile, row.names = FALSE)
  }
  
  return(df_org_map)  
}

# Example script
fil_org_map <- 'D:/Data/NHSD/ORGMAP/20240101/gp-reg-pat-prac-map.csv'
fil_epcn <- 'D:/Data/NHSD/EPCN/20240223/ePCN.xlsx'
sht_pcn_details <- 'PCNDetails'
fil_pcd <- 'D:/Data/OpenGeography/Lookups/PCD/20231129/ONSPD_NOV_2023_UK.csv'
df_org <- fnGeoPracticesAndPCNs(fil_org_map, fil_epcn, sht_pcn_details, fil_pcd, outfile = 'geocoded_prac_and_pcn.csv')

