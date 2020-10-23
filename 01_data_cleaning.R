
rm(list=ls())
library(tidyverse)

########### STEP 1. IMPORT ALL DATASET ########### 
########### DATASETS ARE DAILY BASIS ###########

## 1. Import shareoutstanding data (Dataguide)

shrout = read.csv("01_data/list/dataguide_shrout.csv", stringsAsFactors = F) %>%
  select(-Symbol) %>%
  pivot_longer(!date_id, names_to = "Varname", values_to = "Var") %>%
  mutate(stock2 = substr(Varname, 2,7), 
         shrout = sapply(Var, function(x) gsub(",", "", x) %>% 
                           as.numeric())) %>%
  select(date_id, stock2, shrout) %>%
  arrange(stock2, date_id) %>% 
  drop_na() %>%
  as.data.frame()

## 2. Import price data (Dataguide)

price2 = read.csv("01_data/list/dataguide_price.csv", stringsAsFactors = F) %>%
  select(-Symbol) %>%
  pivot_longer(!date_id, names_to = "Varname", values_to = "Var") %>%
  mutate(stock2 = substr(Varname, 2,7), 
         price = sapply(Var, function(x) gsub(",", "", x) %>% 
                           as.numeric())) %>%
  select(date_id, stock2, price) %>%
  arrange(stock2, date_id) %>% 
  drop_na() %>%
  as.data.frame()

## 3. Import SAS calculated files
sas = read.csv("./01_data/summary.csv", stringsAsFactors = F) %>%
  mutate(stock2 = substr(ISU_CD,4,9)) %>%
  drop_na(mm)

## 4. Import Price Efficiency data
pe = read.csv("./01_data/pe_all.csv", stringsAsFactors = F) %>%
  drop_na(ISU_CD)




############ STEP 2. MERGE IMPORTED DATASETS #############

## 1. Merge Price + SAS

price_sas = price2 %>%
  left_join(sas, by=c("stock2", "date_id")) %>%
  group_by(stock2) %>%
  mutate(ret = (price - dplyr::lag(price))/dplyr::lag(price)) %>%
  filter(date_id != 122)

## 2. Merge PE and #1

price_sas_pe = price_sas %>%
  left_join(pe, by=c("ISU_CD", "date_id"))

## 3. Merge Shrout and #2

merged = price_sas_pe %>%
  left_join(shrout, by=c("stock2", "date_id")) %>%
  group_by(stock2) %>%
  mutate(amihud_p_bil = abs(ret*100)/(dvolume/1000000000),
         mktcap_bil = price*shrout/1000000000) %>%
  select(mm, date_id, ISU_CD, stock2, price, espread:adv_selection, ret, amihud_p_bil, mktcap_bil, sig.s, sig.p, PE, dg:Ksq) %>%
  as.data.frame()

############ STEP 3. FILTER DATASETS #############

filt2= merged %>% filter(nr_trades>10) 

############ STEP 4. WINSORIZE AT 1% #############

## 1. Generate percentile values and Winsorize at 1% 
## (price, espread, qspread, volume, dvolume, nr_trades, rspread, adv_selectin, ret, amihud_p_bil, mktcap_bil, total_tradeday)

wins1 = filt2 %>%
  select(stock2, date_id, price:mktcap_bil, sig.s, sig.p, PE) %>%
  pivot_longer(!(stock2:date_id), names_to = "Varname", values_to = "Var") %>%
  group_by(stock2, Varname) %>%
  mutate(perc = ntile(Var,100), 
         perc99 = quantile(Var, 0.99, na.rm=T), 
         perc1 = quantile(Var, 0.01, na.rm=T)) %>%
  mutate(adjVar = 
           ifelse(perc==100, perc99, 
                  ifelse(perc==1, perc1, Var))) %>%
  as.data.frame()

## 2. Transformation: Long to Wide 

wins2 = wins1 %>% 
  select(stock2, date_id, Varname, adjVar)%>%
  pivot_wider(names_from = Varname, values_from = adjVar)

## 3. Merge Winsorized data and dummy variables
wins3 = 
  wins2 %>% 
  left_join(filt2 %>% 
              select(date_id, stock2, mm, ISU_CD, dg, month, pre_mm, post_mm, post_mm2, prolong, Ksq), 
            by=c("stock2", "date_id")) %>%
  select(mm, stock2, date_id, espread, qspread, rspread, adv_selection, amihud_p_bil, volume, dvolume, price, ret, 
         nr_trades, mktcap_bil, sig.s, sig.p, PE, ISU_CD, dg, month, pre_mm, post_mm, post_mm2, prolong, Ksq ) %>%
  as.data.frame()



## 4. Merge MM fixed effects and #3

DF = wins3

## 5. Write CSV File
write.csv(DF, "./01_data/DF.csv", row.names = F)


############ STEP 5. GENERATE MONTHLY DATASET #############
rm(list=ls())

## 1. Generate Monthly dataset from Daily dataset
library(dplyr)
mydat = read.csv("./01_data/DF.csv", stringsAsFactors = F)
monthdat = 
  mydat %>%
  arrange(stock2, dg, date_id) %>%
  group_by(stock2, dg) %>%
  summarise(mm = mean(mm),
            espread = mean(espread) * 100,
            qspread = mean(qspread) * 100, 
            rspread = mean(rspread) * 100,
            adv_selection = mean(adv_selection) * 100,
            amihud = mean(amihud_p_bil),
            volatility = sd(ret, na.rm=T) * 100,
            volume = mean(volume)/1000,
            dvolume = mean(dvolume)/1000000000,
            nr_trades = mean(nr_trades),
            price = mean(price)/1000,
            # autocorr = arima(ret, order=c(1,0,0))$coef["ar1"],
            ret = (exp(sum(log(1+ret)))-1)*100,
            mktcap = mean(mktcap_bil),
            sig.s = mean(sig.s, na.rm=T),
            sig.p = mean(sig.p, na.rm=T),
            PE = mean(PE, na.rm=T),
            month = mean(month),
            pre_mm = mean(pre_mm),
            post_mm = mean(post_mm), 
            post_mm2 = mean(post_mm2),
            prolong = mean(prolong),
            Ksq = mean(Ksq)
            ) %>% as.data.frame()
write.csv(monthdat, "./01_data/monthdat.csv", row.names=F)


## SUMMARY STAT
monthdat = read.csv("./01_data/monthdat.csv", stringsAsFactors = F)

t1 = monthdat %>%
  select(mm, espread:mktcap) %>% 
  pivot_longer(!mm, names_to="Varname", values_to = "Var") %>%
  group_by(mm, Varname) %>%
  summarise(Mean = mean(Var, na.rm=T), Std = sd(Var, na.rm=T), Min=min(Var, na.rm=T), Max=max(Var, na.rm=T), Median=median(Var, na.rm=T)) %>% 
  arrange(desc(mm), 
          match(Varname, c("espread", "qspread", "rspread", "adv_selection", 
                           "amihud", "volatility", "volume", "dvolume",
                                     "price", "nr_trades", "ret", "mktcap"))) %>%
  ungroup() %>%
  select(-mm) %>%
  as.data.frame()

t1$Varname = rep(c("Effective Spread (%)", "Quoted Spread(%)", "Realized Spread(%)", "Adverse Selection(%)",
                   "ILLIQ (%/KRW 1M)", "Volatility(%)", "Volume (1,000 shares)", "Volume (KRW 1B)", "Price (KRW 1,000)",
                   "NumTrades", "Return (%)", "MktCap (KRW 1B)"),2)
colnames(t1)[1]=" "
t1[,2:6] = round(t1[,2:6], 3)









