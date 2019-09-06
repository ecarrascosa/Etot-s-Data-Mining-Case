library(tidyverse)
library(caret)

# -------------------------------------- #
# Load the data
data <- read_csv("etots_training.csv")

decision_data <- read_csv("etots_decision.csv")

# check if there is any missing values in the data
colSums(is.na(data))
# turns out: Missing "Self" and "gift"

# Replace missing values with zero
data <-
  mutate(data, Missing_Self = is.na(Self), Missing_Gift = is.na(Gift)) %>%
  replace_na(list(Self = 0, Gift = 0))

# Remove HOL_MVAL variable

data <- data %>% select(-HOL_MVAL)


# how many customers (rows) are there in the data
n <- nrow(data)
n
# turns out: 30,000

# -------------------------------------- #
# Create target
target <- factor(data$HOL_REP)

# -------------------------------------- #
# Partition Data
# partition the data according to the following ratios
training_p <- 0.2
validation_p <- 0.35
validation_size <- validation_p * n

set.seed(909439224)
all_rows = 1:n
training <- createDataPartition(target, p = training_p, list=TRUE)$Resample1
validation <- sample(setdiff(all_rows, training), size = validation_size)
test <- setdiff(setdiff(all_rows, training), validation)

training_downsample <- downSample(training, target[training], list = FALSE)$x
training_upsample <- upSample(training, target[training], list = FALSE)$x

# -------------------------------------- #
# normalize data
data <- data %>% mutate(
  S_99HOL = scale(S_99HOL),
  M_VAL99H = scale(M_VAL99H),
  S_00OFF = scale(S_00OFF),
  M_VAL00O = scale(M_VAL00O),
  S_IDX = scale(S_IDX),
  N_S_INDX = scale(N_S_INDX),
  TENURE = scale(TENURE),
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
  J_9908 = factor(J_9908)
)
# -------------------------------------- #
# Model development stage
# at this stage you should not use "test set"
# you can try different models and decide which one is better
# eventually after trying different models, you have to settle on one

# Model 1: Classification Tree

model_1 <- train(
  factor(HOL_REP) ~.,
  data = data[training_downsample, ],
  method = 'rpart'
)

score_validation <- predict(model_1, newdata = data[validation, ])

confusionMatrix(score_validation, target[validation], positive="1")

model_2 <- train(
  factor(HOL_REP) ~ .,
  data = data[training_downsample, ],
  method = 'rf'
)

score_validation <- predict(model_2, newdata = data[validation, ])

confusionMatrix(score_validation, target[validation], positive="1")

# Model 3: Gradient Boosting
model_3 <- train(
  factor(HOL_REP) ~.,
  data = data[training_downsample, ],
  method = 'xgbTree'
)

score_validation <- predict(model_3, newdata = data[validation, ])

confusionMatrix(score_validation, target[validation], positive="1")



# -------------------------------------- #
# Evaluation stage
# use your selected model to score the test set and report model performance

score_test <- predict(model_3, newdata = data[test, ])

confusionMatrix(score_test, target[test], positive="1")

# Apply the model chosen to the decision set and print a two column
# spreadsheet with the answers

decision_score <- predict(model_3, decision_data)

decision_data <- decision_data %>% mutate(answers = decision_score)

write_csv(decision_data, path = "answers_2.csv")
