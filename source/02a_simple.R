# load libraries
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("shiny")) install.packages("shiny")
if (!require("pROC")) install.packages("pROC")

# Running the pricing app
shiny::shinyAppDir("./")

# VARIABLE DESCRIPTIONS:
#   claim        Claim Status
# (0 = Not Having a Claim; 1 = Having a Claim)
# pclass          Passenger Class
# (1 = 1st; 2 = 2nd; 3 = 3rd)
# sex             Sex
# age             Age
# sibsp           Number of Siblings/Spouses Aboard
# parch           Number of Parents/Children Aboard
# fare            Passenger Fare
# embarked        Port of Embarkation
# (C = Cherbourg; Q = Queenstown; S = Southampton)
# ship            Company ship number
# 
# SPECIAL NOTES:
#   Pclass is a proxy for socio-economic status (SES)
# 1st ~ Upper; 2nd ~ Middle; 3rd ~ Lower
# 
# Age is in Years; Fractional if Age less than One (1)
# If the Age is Estimated, it is in the form xx.5
# 
# With respect to the family relation variables (i.e. sibsp and parch)
# some relations were ignored.  The following are the definitions used
# for sibsp and parch.
# 
# Sibling:  Brother, Sister, Stepbrother, or Stepsister of Passenger Aboard Titanic
# Spouse:   Husband or Wife of Passenger Aboard Titanic (Mistresses and Fiances Ignored)
# Parent:   Mother or Father of Passenger Aboard Titanic
# Child:    Son, Daughter, Stepson, or Stepdaughter of Passenger Aboard Titanic
# 
# Other family relatives excluded from this study include cousins,
# nephews/nieces, aunts/uncles, and in-laws.  Some children travelled
# only with a nanny, therefore parch=0 for them.  As well, some
# travelled with very close friends or neighbors in a village, however,
# the definitions do not support such relations.

# `%>%` works as a pipe and forward object on the left side to function on the right side 
# e.g. x %>% f(y) is same as f(x, y)
# Why is it useful and more info about piping here:
# https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html

# Load training data
train <- read.csv("data/data_titanic_adj.csv", stringsAsFactors = FALSE)

# Here we go, lets look at the variables in the table!
# glimpse is short description of table
train %>% glimpse

#### Training vs. Validation ####
set.seed(58742) # to fix randomizer
ind <- sample(3, nrow(train), replace=TRUE, prob=c(0.70, 0.20, 0.10)) # generate random indicator to split

train <- mutate(train,
                data_status = ifelse(ind == 1, 
                                     "Training",
                                     ifelse(ind == 2, 
                                            "Validation", 
                                            "Unseen")
                )
)

# Filtering training and validation set
train_70 <- train %>% filter(data_status == "Training")
val_20 <- train %>% filter(data_status == "Validation")

train_70 %>% nrow
val_20 %>% nrow

##### Target ########
# How many people had a claim?
train_70$claim %>% table # same as table(train_70$claim)

### Exercise:
# Whats "probability of having claim"?


####### ***Features Exploration ***##############
######### Sex ###########
### Exercise:
# How many men and women was there?

### Exercise:
# Do we have complete information? Are there any missingness?
train_70$sex %>% is.na %>% table

### Exercise:
# Who has more claims, men or women?


### Visualization
train_70 %>% 
  ggplot(aes(x = sex,  y = factor(claim))) +
  geom_jitter(alpha = 0.3)

train_70 %>% 
  ggplot(aes(x = sex, fill = factor(claim))) +
  geom_bar(position = position_fill())

#### ***Modeling*** ####
# This is our basic model from Underwriters
log_model_1 <- glm(data = train_70,
                   formula = claim ~ sex + age,
                   family = binomial())

# Information about model
summary(log_model_1) # OU! Age contain missings! Almost 20% passengers excluded from modelling. This is really not good.

### Advice:
# Always check variables for missing values before modeling


# Benchmark ROC - Receiver Operating Characteristic - higher better
# more info why this:
# https://en.wikipedia.org/wiki/Receiver_operating_characteristic
roc_model <- function(model, data_set){
  
  predicted <- predict(model, data_set, type = "response") # prediction of current model
  observed <- data_set[, "claim"] # real target
  
  pROC::auc(pROC::roc(observed, predicted))
}

# benchmark on training
roc_model(log_model_1, train_70)
# benchmark on validation
roc_model(log_model_1, val_20)

######### Age ##############
### Exercise:
# Which kind of people buy ticket to the boat? 
train_70$age %>% table
train_70$age %>% summary
train_70$age %>% hist

### Exercise:
# Do you see missing values?


# Lets make a simple density plot with function density()
train_70$age[!is.na(train_70$age)] %>% density() %>% plot

### Visualization
# See trends, how old people usually had a claim?
train_70 %>% 
  ggplot(aes(x = age,  color = factor(claim))) +
  geom_density() +
  theme_classic()

# We need to solve the problem of missing values.
### Exercise:
# How could we repair missing values?


# See how it changed our density graph
train_70 %>% 
  ggplot(aes(x = age,  color = factor(claim))) +
  geom_density() +
  theme_classic()

### Advice:
# Ou, thats huge change! Keep it in your mind when you are modelling 
# if you possibbly didn't bring some trend you woudn't want to

### Exercise
# Try model with new repaired variable
log_model_2 <- glm(data = train_70,
                   formula = claim ~ sex + age,
                   family = binomial())

# Information about model
summary(log_model_2)

# benchmark on training
roc_model(log_model_2, train_70)
# benchmark on validation
roc_model(log_model_2, val_20)


### Improving model by adding new feature

########## Fare #####
### Exercise:
# Create model with Fare feature and calculate its benchmark metric
log_model_3 <- # glm()

# Information about model
summary(log_model_3)

# benchmark on training
roc_model(log_model_3, train_70)
# benchmark on validation
roc_model(log_model_3, val_20)


# What is the structure of the fare? Do you have some hyphothesis?
### Exercise:
# Do we have complete information? Are there any missing values?


### Exercise: 
# Try to plot histogram or density and make a hypothesis about fare


### Exercise: 
# What is the structure (density) of the fare for those who has claim vs. does not have claim?


### Visualization
# In one graph
train_70 %>% 
  ggplot(aes(x = fare,  color = factor(claim))) +
  geom_density() +
  theme_classic()

### Advice: Capping
# It seems we have no observations between ticket price of 300 and maximum 500.
# What do you think how many people paid 500? Probably not so much.
# Don't you think it could be only coincidence that those people had a claim or not?
# We are using statisical methods and they are based on law of big numbers, 
# So we should not rely on small group of observations. We will make assumptions 
# that those people paid 300 instead of maximum of 500. Second assumptions could be 
# we will exclude those people from our dataset. You as a modeler need to decide.
#
# How to identify if making assumptions only about maximum value is right?
# Usuallu taking down the maximum is not enough, we will take down whole 1% or 5% of highest data
#
# Same could apply to minimum values. We will not focus on them for now.

# Capping
train_70_cap <- train_70
capped_value <- quantile(train_70_cap$fare, 0.99)
train_70_cap$fare[train_70_cap$fare >= quantile(train_70_cap$fare, 0.99)] <- capped_value

# Excluding outliers
train_70_outliers_out <- train_70[train_70$fare <= quantile(train_70$fare, 0.99), ]

# Exercise: Try both options: Capping/Excluding outliers, which one is better and why?
# Model with adjusted Fare
log_model_4 <- # glm()
  
log_model_5 <- # glm()
  
# Information about model
summary(log_model_4)
summary(log_model_5)

# benchmark on training
roc_model(log_model_4, train_70)
# benchmark on validation
roc_model(log_model_4, val_20)

# benchmark on training
roc_model(log_model_5, train_70)
# benchmark on validation
roc_model(log_model_5, val_20)

### Visualization
# Capping
train_70_cap %>% 
  ggplot(aes(x = fare,  color = factor(claim))) +
  geom_density() +
  theme_classic()

# Excluding outliers
train_70_outliers_out %>% 
  ggplot(aes(x = fare,  color = factor(claim))) +
  geom_density() +
  theme_classic()

#### ***Final Evaluation*** ####
# Lets say this is our final model, we will use unseen dataseet for final evaluation
unseen <- train %>% filter(data_status=="Unseen")

# Doing same adjustments as for train_70, age missings and capping/excluding outliers
unseen[is.na(unseen$age), "age"] <- mean(train_70$age, na.rm = TRUE)
unseen <- unseen[unseen$fare <= quantile(train_70$fare, 0.99), ]

# Final evaluation
roc_model(log_model_4, unseen) # great final evaluation is so different as training and validation evaluation

# To update final model in the pricing app
saveRDS(log_model_4, "data/model_final.rds")

# Running the pricing app
shiny::shinyAppDir("./")

#### Bonus Exercises: ####
# 1)
# Try to group age feature into 5 groups (babies, youngters, adults-1, adults-2, pensioner) and 
# fit new model. Does it helps to imporve the model?

# 2)
# Underwriter are not able to use sex feature in European Union because this is a discrimination. 
# Try to figure out how to improve model which is without sex feature.






