#######################################
### FORECASTING WITH (S)ARIMA Model ### 
### - DATATHON GROUP E -###############
#######################################

path <- getwd()
library(data.table)
library(forecast)
library(tseries)
library(dplyr)


data<-read.csv(paste0(path,"/DATA.csv"))
head(data)
colnames(data)[colnames(data) == 'AÃ.o'] <- 'Año'
data$Fecha<-as.Date(data$Fecha)

colnames(data)

summary(data)
str(data)

levels(data$Escenario)

forecastCol <- 'Volumen'
numericCols <- colnames(data)[sapply(data,class) == "numeric" | sapply(data,class) == "integer" ]
categoryCols <- colnames(data)[sapply(data, class) != "numeric"]

data$MKT_PROD_ID <- as.factor(paste(data$id_Mercado, data$id_Producto))
list_MKT_PROD <- list(unique(data$MKT_PROD_ID))

## This is the dataset which we want ot loop over
forecast_data<-data[,c('Fecha','MKT_PROD_ID',forecastCol,'Escenario')]
forecast_data$id <- as.factor(paste(forecast_data$Fecha, forecast_data$MKT_PROD_ID))

## I will create a list with 3 different data frames in it, corresponding to the three plausible scenarios.
forecast_DT <- as.data.table(forecast_data)

pre_forecast <- as.data.frame(forecast_DT[Escenario == "pre_confinement",])

conf_forecast <-as.data.frame(forecast_DT[Escenario == "confinement",])

post_forecast <- as.data.frame(forecast_DT[Escenario == "post_confinement",])

sub_forecasts <- list(pre = pre_forecast, on = conf_forecast, post = post_forecast)

pre_results <- data.frame(MKT_PROD_ID = factor(),
                          Point.Forecast = double(),
                          stringsAsFactors = TRUE)

conf_results <- data.frame(MKT_PROD_ID = factor(),
                           Point.Forecast = double(),
                           stringsAsFactors = TRUE)

post_results <- data.frame(MKT_PROD_ID = factor(),
                           Point.Forecast = double(),
                           stringsAsFactors = TRUE)

## LOOP OVER DATASETS
predict_volume <- function(datasets, weight_pre, weight_conf, weight_post, horizons) {
  
  for (e in names(datasets)){
    scenario <- data.frame()
    scenario <- as.data.frame(datasets[[e]])
    rownames(scenario) <- scenario$id
    
    for (i in levels(scenario$MKT_PROD_ID)){
      i
      scenario_mkt_prod <- scenario %>%
        filter(MKT_PROD_ID == i) %>%
        select(-Escenario)
      
      volume <- as.ts(scenario_mkt_prod$Volumen)
      
      # AUTOARIMAS:
      fit_nonseas <- auto.arima(volume,
                                seasonal = FALSE,
                                stepwise=FALSE, approximation=FALSE)
      
      eval_nonseas <- fit_nonseas$aic
      
      fit_seas <- auto.arima(volume, 
                             seasonal = TRUE,
                             stepwise=FALSE, approximation=FALSE)
      
      eval_seas <- fit_seas$aic
      
      
      # Choose better model (based on AIC):
      if (eval_seas >= eval_nonseas){
        best_fit <- fit_nonseas
      }
      else {
        best_fit <- fit_seas
      }
      
      # Create predictions table, ending as dataframe for technical convenience:
      predictions <- data.table(MKT_PROD_ID = factor(),
                                Point.Forecast = data.frame(forecast(best_fit, h = horizons))[,1])
      
      predictions$MKT_PROD_ID <- rep(i, length.out = horizons)
      
      predictions_df <- as.data.frame(predictions)
      
      # Add predictions to the intial dataframes:
      if (e == "on"){
        conf_results <- rbind(conf_results, predictions_df)
        
      } else if (e == "pre"){
        pre_results <- rbind(pre_results,predictions_df)
        
      } else {
        post_results <- rbind(post_results, predictions_df)
        
      }  
      
    }

  }
  
  post_results_df <- as.data.frame(post_results)
  pre_results_df <- as.data.frame(pre_results)
  conf_results_df <- as.data.frame(conf_results)
  
  
  conf_results_df["Point.Forecast"] <- 0.05 * pre_results_df$Point.Forecast + 0.90 * conf_results_df$Point.Forecast+ + 0.05 * post_results_df$Point.Forecast
  post_results_df["Point.Forecast"] <- 0.10 * conf_results_df["Point.Forecast"] + 0.90 * post_results_df["Point.Forecast"]
  Final_results <- matrix(0, ncol = 0, nrow = 10260)
  Final_results <- data.frame(Final_results)
  #Final_results <- data.frame(Pre_Pred = double(), Conf_Pred = double(), Post_Pred = double(), stringsAsFactors = FALSE)
  
  Final_results$Pre_Pred <-  0.80 * pre_results_df$Point.Forecast + 0.10 * conf_results_df$Point.Forecast +  0.10 * post_results_df$Point.Forecast
  Final_results$Conf_Pred <- 0.30 * pre_results_df$Point.Forecast + 0.60 * conf_results_df$Point.Forecast +  0.10 * post_results_df$Point.Forecast
  Final_results$Post_Pred <- 0.30 * pre_results_df$Point.Forecast + 0.10 * conf_results_df$Point.Forecast +  0.60 * post_results_df$Point.Forecast
  Final_results$Pre_Pred <- as.integer(Final_results$Pre_Pred )
  Final_results$Conf_Pred <- as.integer(Final_results$Conf_Pred )
  Final_results$Post_Pred <- as.integer(Final_results$Post_Pred )
  Final_results$MKT_PROD_ID <- pre_results$MKT_PROD_ID
  
  Final_results$Fecha <- as.Date(rep(c("2020-06-15", "2020-06-22", "2020-06-29", "2020-07-02", "2020-07-09", "2020-07-16"), 1710))

  data_final <- select(data, Fecha, Volumen, MKT_PROD_ID)
  data_final$Pre_Pred <- data_final$Volumen
  data_final$Conf_Pred <- data_final$Volumen
  names(data_final)[names(data_final) == 'Volumen'] <- 'Post_Pred'
  data_final <- data_final[c("Pre_Pred", "Conf_Pred", "Post_Pred",'MKT_PROD_ID','Fecha')]

  Final_Dataset <- rbind(data_final, Final_results)
  
  
  return(Final_Dataset)
  
}


July_August_pred<-predict_volume(sub_forecasts, 0.7,0.15,0.15,6)
July_August_pred


############################
# AND READY TO GO TO TABLEAU
############################