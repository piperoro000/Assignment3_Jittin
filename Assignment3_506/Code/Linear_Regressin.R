# normalize ADP data
platelet_data_clean$ADP_log<-log(platelet_data_clean$ADP)

# rs4244285 vs ADP_log
liner_logA <- lm(ADP_log ~ rs4244285, data = platelet_data_clean)

qqnorm(liner_logA$residuals)
qqline(liner_logA$residuals,col="blue")

summary(liner_logA)

# rs4986893 vs ADP_log
liner_logB <- lm(ADP_log ~ rs4986893, data = platelet_data_clean)

qqnorm(liner_logB$residuals)
qqline(liner_logB$residuals,col="red")

summary(liner_logB)

# rs662 vs ADP_log
linear_logC <- lm(ADP_log ~ rs662, data = platelet_data_clean)

qqnorm(linear_logC$residuals)
qqline(linear_logC$residuals,col="green")

summary(linear_logC)

# Multiple Linear Regression
linear_logABC <- lm(ADP_log ~ rs4244285 + rs4986893 + rs662, data = platelet_data_clean)

qqnorm(linear_logABC$residuals)
qqline(linear_logABC$residuals,col="yellow")

summary(linear_logABC)

# Confounding Factor
linear_CF <- lm(ADP_log ~ rs4244285 + rs4986893 + rs662 + SEX + AGE, data = platelet_data_clean)
summary(adjusted_model)

qqnorm(linear_CF$residuals)
qqline(linear_CF$residuals,col="purple")

summary(linear_CF)