library(ggplot2)
library(dplyr)

# VARIABLE DESCRIPTIONS:
#   survival        Survival
# (0 = No; 1 = Yes)
# pclass          Passenger Class
# (1 = 1st; 2 = 2nd; 3 = 3rd)
# sex             Sex
# age             Age
# sibsp           Number of Siblings/Spouses Aboard
# parch           Number of Parents/Children Aboard
# fare            Passenger Fare
# embarked        Port of Embarkation
# ship            Company ship number
# (C = Cherbourg; Q = Queenstown; S = Southampton)
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

train_70 <- train %>% filter(data_status == "Training")
# Here we go, lets look at the variables in the table!

# See map of missings
library(Amelia)
missmap(train_70, main = "Missing values vs observed")



##### Target ########
# How many people died?
train_70$survived %>% table

# Whats survival ratio?
train_70$survived %>% mean

# Which ship crashed?
table(train_70$ship, train_70$survived)

####### Embarked ####
train_70$embarked %>% table # is equal to table(train_70$embarked)
table(train_70$embarked, train_70$survived)

# changing blank values to most common 
# Have you noticed we changed it also for our whole dataset? Can you guess why?
train_70$embarked[train_70$embarked == ""] <- "S"
train$embarked[train$embarked == ""] <- "S"
table(train_70$embarked, train_70$survived)
# chi-sq test of independence
table(train_70$embarked, train_70$survived) %>% summary()
# rejecting null hypothesis of independence between embarked and survived

# OK, it looks like this variable is not so useful, lets visualize little bit

# Graph

# look at so differences between groups!
train_70 %>% 
  group_by(embarked) %>% 
  summarise(m = mean(survived), n_people = n()) %>% 
  ggplot(. , aes(y = m, x = factor(embarked))) +
  geom_point(aes(size = n_people), color = c("#00BFFF"), alpha = 0.5)


# or no? don't forget to thing about the scale
train_70 %>% 
  group_by(embarked) %>% 
  summarise(m = mean(survived), n_people = n()) %>% 
  ggplot(. , aes(y = m, x = factor(embarked))) +
  geom_point(aes(size = n_people), color = c("#00BFFF"), alpha = 0.5) +
  ylim(0, 1)

########## Fare #####
train_70$fare %>% hist
train_70$fare %>% density %>% plot

train_70[train_70$survived == 1, ]$fare %>% density %>% plot
train_70[train_70$survived == 0, ]$fare %>% density %>% plot

train_70[train_70$survived == 1, ]$fare %>% 
  quantile(probs = c(0, 0.01, 0.05, seq(0.1, 0.90, 0.1), 0.95, 0.99, 1)) 
train_70[train_70$survived == 0, ]$fare %>% 
  quantile(probs = c(0, 0.01, 0.05, seq(0.1, 0.90, 0.1), 0.95, 0.99, 1)) 

train_70 %>% 
  ggplot(aes(x = Fare,  color = factor(Survived))) +
  geom_density() 


######### Age ##############
train_70$age %>% summary
# we have missings in age, 
# age seems to be stong predictor we could create model for estimating age

train_70$age %>% 
  quantile(probs = c(0, 0.01, 0.05, seq(0.1, 0.90, 0.2), 0.95, 0.99, 1), na.rm = TRUE) 

train_70$age %>% density %>% plot

train_70 %>% 
  ggplot(aes(y = age,  x = factor(survived))) +
  geom_violin()

p <- train_70 %>% 
  ggplot(aes(x = age,  color = factor(survived))) +
  geom_density()
ggplotly(p)

# age + sex
# counts
p <- train_70 %>% 
  ggplot(aes(x = age,  fill = factor(survived))) +
  geom_histogram() +
  facet_grid( ~ sex)
ggplotly(p)

# density, nice trends, possible interaction?
p <- train_70 %>% 
  ggplot(aes(x = age,  fill = factor(survived))) +
  geom_histogram(alpha = 0.6) +
  facet_grid( ~ sex) +
  theme_classic()
ggplotly(p)

train_70_adj <- train_70 %>% 
  mutate(age_cat = case_when(is.na(.$age) ~ "Missing",
                             .$age <= 15 ~ "1 - babies",
                             .$age > 15 & .$age <= 22 ~ "2 - youngsters",
                             .$age > 22 & .$age <= 35 ~ "3 - adults - 1",
                             .$age > 35 & .$age <= 50 ~ "4 - adults - 2",
                             .$age > 50 ~ "5 - olders"),
         age_mutualize = ifelse(is.na(age), mean(age, na.rm = TRUE), age)
  )
val_20 <- val_20 %>% 
  mutate(age_cat = case_when(is.na(.$age) ~ "Missing",
                             .$age <= 15 ~ "1 - babies",
                             .$age > 15 & .$age <= 22 ~ "2 - youngsters",
                             .$age > 22 & .$age <= 35 ~ "3 - adults - 1",
                             .$age > 35 & .$age <= 50 ~ "4 - adults - 2",
                             .$age > 50 ~ "5 - olders"),
         age_mutualize = ifelse(is.na(age), mean(age, na.rm = TRUE), age)
  )

train_70_adj$age_cat %>% table(train_70_adj$survived)
train_70_adj$age_cat %>% table(train_70_adj$survived) %>% summary
######### Sex ###########
train_70$sex %>% table(useNA = 'ifany')
table(train_70$sex, train_70$survived)

# rejecting null hypothesis, factors are independent, very strong rejection!
table(train_70$sex, train_70$survived) %>% summary

p <- train_70 %>% 
  ggplot(aes(x = sex,  y = factor(survived))) +
  geom_jitter() +
  geom_density(alpha = 0.3) 
ggplotly(p)

########### Parch/Sibsp - parents/siblings aboard #############
train_70$parch %>% table(useNA = 'ifany')
train_70$sibsp %>% table(useNA = 'ifany')

train_70$parch %>% table(train_70$survived)
train_70$parch %>% table(train_70$survived) %>% summary

train_70 %>% filter(parch<3) %>% select(parch, survived) %>% table %>% summary

train_70_adj <- 
  train_70 %>% 
  mutate(parch_adj = ifelse(parch >2, 0, parch))

train_70$sibsp %>% table(train_70$survived)
train_70$sibsp %>% table(train_70$survived) %>% summary

train_70 %>% filter(sibsp<2) %>% select(sibsp, survived) %>% table %>% summary

train_70_adj <- 
  train_70 %>% 
  mutate(sibsp_adj = ifelse(sibsp >2, 0, sibsp))
######### PClass - socioeconomic status ##########
train_70$pclass %>% table(useNA = 'ifany')

# people with lower status died, look only 3th class is significant
table(train_70$pclass, train_70$survived)

# pclass vs. fares
# people from 1th class if they paid much more for the ticket, looks they survived
p <- train_70 %>% 
  ggplot(aes(x = fare, color = factor(survived))) +
  geom_density() +
  facet_wrap(~ pclass) +
  coord_flip()
ggplotly(p)


train %>% 
  mutate(deck = substr(cabin, 1, 1)) %T>% # odskocim si vypisat tabulku
  View %>% # a pokracujem dalej 
  select(deck, survived) %>% table()




