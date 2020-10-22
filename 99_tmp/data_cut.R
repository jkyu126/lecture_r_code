# THIS IS NOT A PART OF THE LECTURE
# JUST SKIP THIS CODE 

shrout_in = read.csv("./01_data/list/dataguide_shrout.csv", stringsAsFactors = F) 
shrout_in = shrout_in[,1:32]
write.csv(shrout_in, "./01_data/list/dataguide_shrout.csv", row.names = F)

firm_list = colnames(shrout_in)[3:32] 
firm_list = substr(firm_list,2,7)

price_in = read.csv("./01_data/list/dataguide_price.csv", stringsAsFactors = F)
price_in = price_in[,1:32]
write.csv(price_in, "./01_data/list/dataguide_price.csv", row.names = F)

sas_in = read.csv("./01_data/summary.csv", stringsAsFactors = F) %>%
  mutate(stock2 = substr(ISU_CD, 4, 9)) %>%
  filter(stock2 %in% firm_list) %>%
  select(-stock2)
write.csv(sas_in, "./01_data/summary.csv", row.names = F)

pe_in = read.csv("./01_data/pe_all.csv", stringsAsFactors = F) %>%
  mutate(stock2 = substr(ISU_CD, 4, 9)) %>%
  filter(stock2 %in% firm_list) %>%
  select(-stock2)
write.csv(pe_in, "./01_data/pe_all.csv", row.names = F)


