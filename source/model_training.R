log_model_1 <- glm(data = train_70_adj,
                   formula = survived ~ sex  + pclass + sibsp + embarked + fare + parch,
                   family = binomial())

summary(log_model_1)

plot(log_model_1)

step(log_model_1, direction="backward", trace = TRUE)
