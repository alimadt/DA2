library(readr)
library(dplyr)
library(estimatr)
library(ggplot2)
library(modelsummary)

# Downloading the data
earnings <- read_csv('https://osf.io/4ay9x/download')

# Lawyers 
earnings <- earnings %>% filter(occ2012 == 2100)

# Additional filtering
earnings <- earnings %>% filter(uhours>=20 & earnwke>0 & age>=24 & age<=64 & grade92>=44)

# Creating female and hourly wage variables
earnings <- earnings %>% mutate(female=as.numeric(sex==2)) %>%
  mutate(w=earnwke/uhours) %>%
  mutate(lnw=log(w))

# Regressions on female (with and without robust)
regr1 <- lm(lnw ~ female, data = earnings)

regr2 <- lm_robust(lnw ~ female, data = earnings, se_type = "HC1")

ht1<-huxreg(regr1,regr2,
           statistics = c(N = "nobs", R2 = "r.squared")) 
ht1

# Regression on female and level of education
earnings <- earnings %>% mutate(ed_MA=as.numeric(grade92==44),
                      ed_Profess = as.numeric(grade92==45),
                      ed_PhD = as.numeric(grade92==46)
)
regr3 <- lm_robust(lnw ~ female + ed_Profess + ed_PhD, data=earnings, se_type = "HC1")
regr4 <- lm_robust(lnw ~ female + ed_MA + ed_PhD, data=earnings, se_type = "HC1")

ht2 <- huxreg(regr2, regr3, regr4, statistics = c(N = "nobs", R2 = "r.squared"))
ht2

# Graph
earnings$grade92 <- as.factor(earnings$grade92)
earnings <- earnings %>%
  mutate(educ = case_when(grade92 ==  '44'~ "Master's degree",
                          grade92 == '45' ~ "Professional school",
                          grade92 == '46' ~ "PhD"))

graph1 <- ggplot(data = earnings, aes(x = educ, fill = educ)) + 
  geom_histogram(stat='count')+ facet_wrap(~ifelse(female, "Female", "Male")) + 
  labs(x='Level of education', y='Count', fill='Level of education') + geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")
graph1

