log_model_1 <- glm(data = train_70,
                   formula = survived ~ sex + fare,
                   family = binomial())

summary(log_model_1)


plot(log_model_1)

step(log_model_1, direction="backward", trace = TRUE)

library(tidyr)
train_70 %>% cbind(fit=log_model_1$fitted.values) %>% 
  rename(observed = survived) %>% 
  gather(status, target, -one_of(names(train_70)[-2]))
  ggplot(aes(x = embarked,)
         

train_70 %>% 
  ggplot(aes(x = embarked, y = survived)) + 
  geom_point(shape=1, position=position_jitter(width=.05,height=.05)) +
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)