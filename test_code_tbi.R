list.files()
library(dplyr)
library(tidyr)
options(stringsAsFactors = F)
raw_data_path <- file.path(getwd(), "Downloads/Parking_Citations.csv")
test_df_1 <- read.csv(raw_data_path)
colnames(test_df_1)
head(test_df_1)
mean(test_df_1$ViolFine)

ll <- test_df_1$ImportDate %>% unique()
ll <- sapply(ll, function(x) strsplit(x, split = " ")[[1]][1])
ll <- as.Date(ll, format ="%m/%d/%Y") 
ll[which.min(ll)]
test_df_1_former_citation <- test_df_1 %>% 
  mutate(citation_date = sapply(ImportDate, function(x) strsplit(x, split = " ")[[1]][1])) %>%
  mutate(citation_date = as.Date(citation_date, format = "%m/%d/%Y"))
test_df_1_former_citation_1 <- test_df_1_former_citation %>%
  filter(citation_date < as.Date("01/01/2019", format = "%m/%d/%Y"))


test_df_1_former_citation$citation_date %>% unique()
test_df_1_former_citation_1$citation_date[which.max(test_df_1_former_citation_1$citation_date)]

test_df_1_former_citation_2 <- test_df_1_former_citation_1 %>% 
  mutate(police_district = tolower(PoliceDistrict),
         police_district = gsub("notheastern","northeastern", police_district)) 

test_df_1_former_citation_3 <- test_df_1_former_citation_2 %>%
  filter(nzchar(police_district)) %>% group_by(police_district) %>% summarise(vf_mean = mean(ViolFine))

test_df_1_former_citation_3 %>% filter(vf_mean == vf_mean[which.max(vf_mean)]) %>% View()

test_df_1_former_citation_4 <- test_df_1_former_citation_1 %>% 
  mutate(citation_year =  gsub("\\-.*$","", citation_date))

test_df_1_former_citation_5 <- test_df_1_former_citation_4 %>% group_by(citation_year) %>%
  summarise(total_num = length(Citation)) 
attach(test_df_1_former_citation_5)
citation_year <- citation_year %>% as.numeric()
lm.test_1 <- lm(total_num ~ citation_year )
summary(lm.test_1)
plot(lm.test_1)

test_df_1_former_citation_1$OpenPenalty %>% unique()
test_df_1_former_citation_1 %>% filter(OpenPenalty == 0) %>% View()
test_df_1_former_citation_6 <- test_df_1_former_citation_1 %>%
  filter(OpenPenalty != 0) 
  
quantile(test_df_1_former_citation_6$OpenPenalty, .81)

test_df_1_former_citation_7 <- test_df_1_former_citation_4 %>% 
  filter(citation_year == "2017") %>%
  mutate(make_1 = gsub("\\ |\\(","", tolower(Make))) %>%
  mutate(cars = as.character(make_1)) %>%
  mutate(cars = replace(cars, cars %in% c("toyot","toyt"), "toyota"),
         cars = replace(cars, cars %in% c("hond"), "honda"),
         cars = replace(cars, cars %in% c("niss","nissa"), "nissan"),
         cars = replace(cars, cars %in% c("hyund","hyun"), "hyundai"),
         cars = replace(cars, cars %in% c("chrys","chry"), "chrysler"),
         cars = replace(cars, cars %in% c("chev", "chevr"), "chevrolet")) 
test_df_1_former_citation_8 <- test_df_1_former_citation_7 %>%
  mutate(cars = replace(cars, cars %in% c("mazd", "maz", "maza", "mazy"), "mazda"),
         cars = replace(cars, cars %in% c("acu","acur"), "acura"),
         cars = replace(cars, cars %in% c("lexs", "lexu", "lex", "lexi", "lexy"), "lexus"),
         cars = replace(cars, cars %in% c("dodg"), "dodge"))

makes_df_0 <- table(test_df_1_former_citation_8$cars) %>% as.data.frame() 
makes_df_1 <- makes_df_0 %>% 
  arrange(desc(Freq))

test_df_2 <- test_df_1 %>%
  mutate(make_1 = gsub("\\ |\\(","", tolower(Make))) %>%
  mutate(cars = as.character(make_1)) %>%
  mutate(cars = replace(cars, cars %in% c("toyot","toyt"), "toyota"),
         cars = replace(cars, cars %in% c("hond"), "honda"),
         cars = replace(cars, cars %in% c("niss","nissa"), "nissan"),
         cars = replace(cars, cars %in% c("hyund","hyun"), "hyundai"),
         cars = replace(cars, cars %in% c("chrys","chry"), "chrysler"),
         cars = replace(cars, cars %in% c("chev", "chevr"), "chevrolet")) %>%
  mutate(cars = replace(cars, cars %in% c("mazd", "maz", "maza", "mazy"), "mazda"),
         cars = replace(cars, cars %in% c("acu","acur"), "acura"),
         cars = replace(cars, cars %in% c("lexs", "lexu", "lex", "lexi", "lexy"), "lexus"),
         cars = replace(cars, cars %in% c("dodg"), "dodge"))
test_df_2 %>% filter(cars %in% c("honda", "touota", "nissan", "acura", "lexus")) %>% View()
878594/3374648
