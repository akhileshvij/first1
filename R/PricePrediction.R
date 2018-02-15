#' @title Predicts PriceUsing one TimeSeries using Another as explanatory
#'
#' @description
#'
#' @param
#'
#' @return
#'
#' @examples
#'
#' @export price_prediction

price_prediction=function(weekly_input,explanatory_output)
{
  library(forecast)

  input_train<-weekly_input
  colnames(input_train)<-c("week","time_series","expl_var")

  starting_year=as.numeric(substr(input_train[1,1],1,4))
  starting_week=as.numeric(substr(input_train[1,1],6,7))
  #creating time series of history
  hist_ts=ts(input_train$time_series,start = c(starting_year,starting_week),frequency = 53 )

  arima_model=auto.arima(hist_ts,xreg = input_train$expl_var)

  pred_model=predict(arima_model,newx=explanatory_output)

  write.csv(pred_model$pred,"predictions.csv")

}


