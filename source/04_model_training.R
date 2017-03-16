log_model_1 <- glm(data = train_70,
                   formula = Survived ~ Sex  + SibSp + Embarked + Fare + Parch,
                   family = binomial())

summary(log_model_1)

plot(log_model_1)

step(log_model_1, direction="backward", trace = TRUE)
