library(tidyverse)
library(caret)

decision_data <- read_csv("etots_decision.csv")

target <- factor(decision_data$HOL_REP)

decision_data <-
  mutate(decision_data, Missing_Self = is.na(Self), Missing_Gift = is.na(Gift)) %>%
  replace_na(list(Self = 0, Gift = 0))

decision_data <- decision_data %>% mutate(
  Self = factor(Self),
  Gift = factor(Gift),
  J_9711 = factor(J_9711),
  J_9712 = factor(J_9712),
  J_9801 = factor(J_9801),
  J_9802 = factor(J_9802),
  J_9803 = factor(J_9803),
  J_9804 = factor(J_9804),
  J_9805 = factor(J_9805),
  J_9806 = factor(J_9806),
  J_9807 = factor(J_9807),
  J_9808 = factor(J_9808),
  J_9809 = factor(J_9809),
  J_9810 = factor(J_9810),
  J_9811 = factor(J_9811),
  J_9812 = factor(J_9812),
  J_9901 = factor(J_9901),
  J_9902 = factor(J_9902),
  J_9903 = factor(J_9903),
  J_9904 = factor(J_9904),
  J_9905 = factor(J_9905),
  J_9906 = factor(J_9906),
  J_9907 = factor(J_9907),
  J_9908 = factor(J_9908),
)

decision_score <- predict(model_1, decision_data)

confusionMatrix(decision_score, target, positive="1")
