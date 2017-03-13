log_model_final <- glm(data = train_70_adj,
                   formula = survived ~ sex  + pclass + sibsp + embarked + fare + parch,
                   family = binomial())

