library(tidyverse)

health_20201231_1 <- read.csv("E:/Lectures/Other work/R/MP/health_20201231_1.csv")

healthTib <- as_tibble(health_20201231_1)

### dropping useless columns

health_cleaned <- healthTib %>%
  select(-c("기준년도", "총.콜레스테롤", "트리글리세라이드", "HDL.콜레스테롤",
         "LDL.콜레스테롤", "데이터.공개일자"))

### changing age
health_cleaned$연령대.코드.5세단위. <- health_cleaned$연령대.코드.5세단위. * 5

summary(health_cleaned)

library(Hmisc)
hist.data.frame(health_cleaned)

strat_sampled_tib <- health_cleaned %>% 
  # partition the population into strata
  group_nest(연령대.코드.5세단위.) %>% 
  # for each stratum...
  rowwise() %>% 
  mutate(
    # calculate the required sample size
    samplesz = round(1000*nrow(data)/nrow(health_cleaned)),
    # then draw the sample
    sample = list(sample_n(data, size = samplesz))) %>% 
  select(-c(data, samplesz)) %>% 
  # et voila!
  unnest(sample)

hist.data.frame(strat_sampled_tib)
### using hist.data.frame on sampled data "cleaned" the outliers

strat_sampled_tib %>%
  select(연령대.코드.5세단위.,신장.5Cm단위.,허리둘레) %>%
  ggpairs()
### reduce both the observations and the number of variable to be able to use ggpairs


### dropping small NA's
health_no_NA <- health_cleaned %>%
  filter(!is.na("허리둘레"))

### dealing with outliers and 


### 3 blood_pressure

blood_pressure <- health_cleaned %>%
  select('수축기.혈압', '이완기.혈압') %>%
  mutate(bp_class = case_when(
    (blood_pressure$이완기.혈압<=80) & (blood_pressure$수축기.혈압<=120) ~ 0,
    (blood_pressure$이완기.혈압>=90) | (blood_pressure$수축기.혈압>=130) ~ 1,
    TRUE ~ 2
  ))

summary(blood_pressure)

blood_pressure %>%
  group_by(bp_class) %>%
  count()

