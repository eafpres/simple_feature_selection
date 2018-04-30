#
# demo automatic feature extraction in R
# using simple p-value threshold and lm
#
  rm(list = ls())
#
  train_fn <- paste0("demo_auto_feature_with_lm_in_R.csv")
  test_fn <- paste0("demo_auto_feature_with_lm_in_R_test.csv") 
#
  train_data <- read.csv(train_fn, header = TRUE)
  test_data <- read.csv(test_fn, header = TRUE)
  par(fig = c(0, 1, 0, 1))
  plot(x = train_data$time, y = train_data$metric, 
       xlim = c(min(train_data$time, test_data$time), 
                max(train_data$time, test_data$time)),
       ylim = c(min(train_data$metric, test_data$metric), 
                max(train_data$metric, test_data$metric)),
       type = "l",
       lwd = 0.2,
       col = "blue",
       xlab = "Time",
       ylab = "Metric (measured values)")
  lines(x = test_data$time, y = test_data$metric,
        lwd = 0.2,
        col = "red")
  rect(-50, 0, 150, 30,
       density = NULL,
       border = "black")
  arrows(0, 30, 750, 35,
         length = 0.1,
         col = "black",
         lwd = 0.5)
  text(x = 775, y = 35,
       "detail",
       cex = 0.75,
       pos = 4)
  lines(x = c(7500, 10000), 
        y = c(15, 15),
        col = "blue",
        lwd = 2)
  lines(x = c(7500, 10000),
        y = c(13, 13),
        col = "red",
        lwd = "2")
  text(x = 10200, y = 15, 
       pos = 4,
       "training data",
       col = "blue",
       cex = 0.75)
  text(x = 10200, y = 13, 
       pos = 4,
       "test data", 
       col = "red",
       cex = 0.75)
#
# standard linear regression
#
  linear_model <- lm(metric ~ ., 
                     data = train_data)
  summary(linear_model)
  init_pred <- predict(linear_model, newdata = test_data)
  points(x = test_data$time, y = init_pred,
         pch = 20,
         cex = 1,
         col = "darkgrey")
#
# build a formula to fit using only sigificant factors
#
  threshold <- 0.05
  signif_form <- 
    as.formula(paste("metric ~ ",
                     paste(names(which((summary(linear_model)$coefficients[
                       2:(nrow(summary(linear_model)$coefficients)), 4] < threshold) == TRUE)), 
                       collapse = "+")))
#
# refit with only the signficant factors
#
  linear_model <- lm(signif_form, data = train_data)
  final_pred <- predict(linear_model, newdata = test_data)
  points(x = test_data$time, y = final_pred,
         pch = 1,
         cex = 2,
         lwd = 0.5,
         col = "black")
  summary(linear_model)
  points(x = 11800, y = 28,
         pch = 20, 
         cex = 1,
         col = "darkgray")
  points(x = 11800, y = 26,
         pch = 1,
         cex = 1.5,
         lwd = 0.5,
         col = "black")
  text(x = 12000, y = 28,
       "initial model",
       pos = 4, 
       cex = 0.75)
  text(x = 12000, y = 26,
       "final model",
       pos = 4,
       cex = 0.75)
#
  par(fig = c(0, 0.4, 0.6, 1), new = TRUE)
  plot(x = train_data$time[1:100],
        y = train_data$metric[1:100],
       type = "l",
       lwd = 0.5,
       col = "blue",
       xaxt = "n",
       yaxt = "n",
       ann = FALSE)

  