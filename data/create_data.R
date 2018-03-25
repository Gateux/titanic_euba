library(dplyr)

# Generating more data
train <- titanic::titanic_train %>% 
  select(-Name, -Cabin, -Ticket, -PassengerId) %>% 
  mutate(Ship = 0)

test <- titanic::titanic_test %>% 
  select(-Name, -Cabin, -Ticket, -PassengerId)

set.seed(5987125)

train_sample <- train %>% select(-Survived) %>% 
  sample_n(9000, replace = TRUE) %>% 
  mutate(Claim = 0, Ship = round(runif(9000, 1, 10))) 
  
train_sample[train_sample$Ship %in% c(7, 2), "Claim"] <-
  sample(train$Survived, nrow(train_sample[train_sample$Ship %in% c(7, 2), ]), replace = TRUE)

train <- rbind(train %>% 
                 mutate(Claim = abs(Survived - 1)) %>% 
                 select(-Survived), 
               train_sample)

rm(train_sample)

names(train) <- names(train) %>% tolower

write.csv(train, "data/data_titanic_adj.csv", row.names = FALSE)
