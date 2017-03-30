# definition of final model
log_model_final <- glm(data = train_70,
                       formula = survived ~ sex + fare,
                       family = binomial())

# to update final model
saveRDS(log_model_final, "data/model_final.rds")