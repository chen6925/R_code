library(forecast)
library(tseries)
library(dplyr)
library(quantmod)

suppressWarnings({
  continue <- 1
  avg_1 <- 0
  which_stock <- readline(prompt = "請輸入股票代號:")
  data <- na.omit(getSymbols(Symbols = which_stock,auto.assign = F))
  data_train <- data[paste0(max(index(data) - 3000),"/",max(index(data) - 3)),4]
  data_compare <- data[paste0(max(index(data)-2),"/",max(index(data))),4]
  
  num_lambda <- 1
  i <- 0
  while (continue != 0)
  {
    time_data <- na.omit(ts(data_train,frequency = 20,start = c(1)))
    if(continue == 1)
    {
      lambda_data <- BoxCox(time_data,lambda = "auto")
      num_lambda <- attr(lambda_data,"lambda")
      test <- adf.test(lambda_data)  
    }
    if(test$p.value > 0.05 || continue != 1)
    {
      if(continue == 1)
      {
        num_lambda <- 1
        for(i in 1:5)
        {
          dif_data <- diff(lambda_data,difference = i)
          test <- adf.test(dif_data)
          if(test$p.value < 0.05)
          {
            break
          }
        }
      }
      if(test$p.value > 0.05 || continue != 1)
      {
        for(i in 1:5)
        {
          yo <- 1
          lambda_data <- BoxCox(time_data,lambda = "auto")
          num_lambda <- attr(lambda_data,"lambda")
          dif_data <- diff(lambda_data,difference = i)
          test <- adf.test(dif_data)
          if(test$p.value < 0.05)
          {
            break
          }
        }
      }
    }
    cat("資料轉換完成")
    cat(sprintf("\n差分次數:%d\n",i))
    cat("開始建立模型")
    if(num_lambda == 1)
    {
      parameter <- auto.arima(time_data,stepwise = F,trace = F,stationary = T,ic=("aic"))
    }else{
      parameter <- auto.arima(lambda_data,stepwise = F,trace = F,stationary = T,ic=("aic"))
    }
    cat(sprintf("建立完成參數為:%d %d %d %d %d %d\n",parameter$arma[1],i,parameter$arma[2],parameter$arma[3],i,parameter$arma[4]))
    if(num_lambda == 1)
    {
      lambda_data <- time_data
    }
    if(i != 0)
    {
      have_mean = FALSE
    }else{
      have_mean = TRUE
    }
    if(parameter$arma[3] == 0 && parameter$arma[4] == 0)
    {
      model <- arima(lambda_data,order = c(parameter$arma[1],i,parameter$arma[2]),include.mean = have_mean)
    }else{
      model <- arima(lambda_data,order = c(parameter$arma[1],i,parameter$arma[2]),seasonal = list(order = c(parameter$arma[3],i,parameter$arma[4]),period = parameter$arma[5]),include.mean = have_mean)
    }
    
    
    predict <- forecast(model,3,lambda = num_lambda)
    output <- as.data.frame(predict)
    yes <- cbind(output,data_compare)
    wrong_abs <- abs(yes[,6]-yes[,1])
    wrong <- (abs(yes[,6]-yes[,1]) / yes[,6])
    avg <- ((wrong[1] + wrong[2] + wrong[3]) / 3) * 100
    cat(sprintf("%.2f%% \n",avg))
    if(avg <= 5 || continue != 1)
    {
      if(avg_1 > avg)
      {
        model <- model_now
      }
      continue <- 0
    }else{
      model_now <- model
      avg_1 <- avg
      continue <- 2
      now_i <- i
    }
  }
  
  new <- data[paste0(max(index(data) - 368),"/",max(index(data))),4]
  time_new_data <- na.omit(ts(new, frequency = 20, start = c(1)))
  if(num_lambda != 1) {
    lambda_new <- BoxCox(time_new_data, lambda = num_lambda)
  }else{
    lambda_new <- time_new_data
  }
  if(parameter$arma[3]==0 && parameter$arma[4]==0) {
    model_2<- arima(lambda_new,order = c(parameter$arma[1],i,parameter$arma[2]),include.mean = have_mean)
  }else{
    model_2<-arima(lambda_new,order = c(parameter$arma[1],i,parameter$arma[2]),seasonal = list(order = c(parameter$arma[3],i,parameter$arma[4]), period = parameter$arma[5]), include.mean = have_mean)
  }
  q<-forecast(model_2,10,lambda = num_lambda)
  output_2<-as.data.frame(q)
  plot(q)
  cat(sprintf("今日預測股價:%f\n", output_2$`Point Forecast`))
  cat(sprintf("95%%信心區間%f-%f\n",output_2$`Lo 95`,output_2$`Hi 95`))
  cat(sprintf("80%%信心區間%f-%f",output_2$`Lo 80`,output_2$`Hi 80`))
  
  #Box.test(model$residuals,lag = 10,type = "Ljung-Box")
  #Box.test(model$residuals,lag = 40,type = "Ljung-Box")
  #qqnorm(model$residuals)
  #qqline(model$residuals, col = "red")
  
})
