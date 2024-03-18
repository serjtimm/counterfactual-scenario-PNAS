## clean the environment
rm(list = ls())

## libraries 
# install.packages(c("patchwork", "tidyverse"))
library(tidyverse)
library (patchwork) 

## external functions
source ("C:/Users/U1136175/OneDrive - Australian National University/projects/USA_PNAS_letter/calculations/functions.R")

## add HMD data: sex-specific LT (downloaded from mortality.org on 15/03/24) 
setwd("C:/Users/U1136175/OneDrive - Australian National University/data/hmd")
hmd_names <- list.files(path = "input/lt_male/mltper_1x1")
hmd_names<-gsub(".mltper_1x1.txt","", hmd_names)

mLT<-NULL
for (i in hmd_names) {
  df <- read.table(paste("input/lt_male/mltper_1x1/", i,".mltper_1x1.txt", sep=""), skip=2, header=TRUE)
  df$hmd_name <- i
  df$sex <- 1
  mLT<-rbind(mLT,df)
}

fLT<-NULL
for (i in hmd_names) {
  df <- read.table(paste("input/lt_female/fltper_1x1/", i,".fltper_1x1.txt", sep=""), skip=2, header=TRUE)
  df$hmd_name <- i
  df$sex <- 2
  fLT<-rbind(fLT,df)
}
  
LT_HMD <- rbind (mLT, fLT) 
rm (df, fLT, mLT) 

### transform the data
setwd ("C:/Users/U1136175/OneDrive - Australian National University/projects/USA_PNAS_letter/calculations")

country_list <- read.csv("list_of_countries.csv")
LT_HMD <- left_join(LT_HMD, country_list, by = "hmd_name")
LT_HMD <- LT_HMD %>% drop_na()

LT_HMD <- LT_HMD %>% 
  mutate (Age = case_when (Age=="110+" ~ "110",
                           Age!="110+" ~ Age)) %>% 
  filter (Year == 2010  | Year == 2019) %>% 
  transmute (year = Year, country_name, age = Age, sex, mx, ex) %>%
  mutate_at(c("mx", "ex"), as.numeric) %>%
  mutate_at(c("year", "sex", "age"), as.integer) 

LT_HMD <- LT_HMD[order(LT_HMD$age),]


## calculate ROMI from 2010 to 2019 for the USA 
ROMI <- LT_HMD %>% 
  select (-ex) %>% 
  filter (country_name == "USA") %>% 
  pivot_wider(names_from = year, values_from = mx, names_prefix = "y_") %>% 
  mutate (romi = y_2019 / y_2010) %>% 
  select (age, sex, romi)

## calculate counterfactual mx for each country
df_mx <- LT_HMD %>% 
  select (-ex) %>% 
  pivot_wider(names_from = year, values_from = mx, names_prefix = "mx_") %>% 
  #drop_na() %>% 
  left_join(ROMI, by = c("age", "sex")) %>% 
  mutate (mx_romi = mx_2010* romi)

## calculate LTs 
LT_romi <- NULL
for (c in unique(df_mx$country_name)){
  for (s in c(1,2)) {
    
     mx <- df_mx$mx_romi [df_mx$country_name == c & df_mx$sex == s]

     df <- compute_LT (age=0:110, sex=s, mx)
     df$sex <- s
     df$country_name <- c
     
     LT_romi <-rbind(LT_romi, df)

  }
}
LT_romi <- LT_romi %>% 
  rename(ex_romi = ex) %>% 
  select (country_name, age, sex, ex_romi)


LT_2010 <- NULL
for (c in unique(df_mx$country_name)){
  for (s in c(1,2)) {
    
    mx <- df_mx$mx_2010 [df_mx$country_name == c & df_mx$sex == s]
    
    df <- compute_LT (age=0:110, sex=s, mx)
    df$sex <- s
    df$country_name <- c
    
    LT_2010 <-rbind(LT_2010, df)
    
  }
}
LT_2010 <- LT_2010 %>% 
  rename(ex_2010 = ex) %>% 
  select (country_name, age, sex, ex_2010)


LT_2019 <- NULL
for (c in unique(df_mx$country_name)){
  for (s in c(1,2)) {
    
    mx <- df_mx$mx_2019[df_mx$country_name == c & df_mx$sex == s]
    
    if (is.na(mx[1])) next()
    
    df <- compute_LT (age=0:110, sex=s, mx)
    df$sex <- s
    df$country_name <- c
    
    LT_2019 <-rbind(LT_2019, df)
    
  }
}
LT_2019 <- LT_2019 %>% 
  rename(ex_2019 = ex) %>% 
  select (country_name, age, sex, ex_2019)

## compare LE(0) 
LT_compare <- left_join(LT_romi, LT_2010)
LT_compare <- left_join(LT_compare, LT_2019)

LT_compare_ex <- LT_compare %>% 
  pivot_longer(cols = starts_with("ex"), names_to = "variable", values_to = "ex")

write.csv(LT_compare_ex, "LT_compare_ex.csv")

