library(tidyverse)

vals = read.csv("validation.csv", header=T)
# assume that all lung cases after 2014 were forecasted as 10 persons.
vals$pred_method1 = 10

# to calculate MAPE
# if true LUNG is zero, then the resulting value will be infinity (ignore this for now)
MAPE_method1 = vals %>% 
  group_by(ID, NAME) %>%
  summarize(MAPE = mean(abs(LUNG - pred_method1) / LUNG) * 100)

print(MAPE_method1)