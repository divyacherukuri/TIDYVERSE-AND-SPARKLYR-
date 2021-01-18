# TIDYVERSE-AND-SPARKLYR-
USING R
fly <- flights %>%
 ml_linear_regression(dep_delay ~ month + distance + hour + minute)
fly %>%
 summary()
partitions <- flights %>%
 filter(is.na(hour)==FALSE | is.na(carrier)==FALSE | is.na(distance)==FALSE | is.na(manufacture
r)==FALSE | is.na(manf_year)==FALSE | is.na(model)==FALSE) %>%
 sdf_random_split(training=0.7,test=0.3,seed=1234)
flights_train <- partitions$training
flights_test <- partitions$test
rf_model <- flights_train %>%
 ml_random_forest(arr_delay ~ hour+carrier+distance+manufacturer,type="regression")
pred <- ml_predict(rf_model,flights_test)
pred %>%
 mutate(residual=prediction-arr_delay) %>%
summarise(r=sqrt(mean(residual^2)))
rfmodel2 <- flights_train %>%
 ml_random_forest(arr_delay ~ hour+carrier+distance+manufacturer+manf_year,type="regression")
prealternate <- ml_predict(rfmodel2,flights_test)
prealternate %>%
 mutate(residual=prediction-arr_delay) %>%
 summarise(rmse=sqrt(mean(residual^2)))
