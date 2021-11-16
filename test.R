library(tidyverse)

health_20201231_1 <- read.csv("E:/Lectures/Other work/R/MP/health_20201231_1.csv")

healthTib <- as_tibble(health_20201231_1)

### dropping useless columns

health_cleaned <- healthTib %>%
  select(-c("기준년도", "총.콜레스테롤", "트리글리세라이드", "HDL.콜레스테롤",
         "LDL.콜레스테롤", "데이터.공개일자"))

### changing age
health_cleaned$연령대.코드.5세단위. <- health_cleaned$연령대.코드.5세단위. * 5

### dropping small NA's
health_no_NA <- health_cleaned %>%
  filter(!is.na("허리둘레"))

### dealing with outliers and 

summary(health_cleaned)

hist.data.frame(health_cleaned)

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

