log_model_final <- glm(data = train_70,
                   formula = survived ~ sex  + pclass,
                   family = binomial())

