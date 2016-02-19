library(dplyr)
options_data <- read.csv("./cboe_options_data.csv")

# Just to convert the factors to numeric vectors.
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

# Cleans the data read and converts all the columns to numeric vectors.
clean_options_data <- function(options_data){
  oldw <- getOption("warn")
  options(warn = -1)
  colnames(options_data) <- c("strike","value")
  options_data$value <- as.numeric.factor(options_data$value)
  options_data$strike <- as.numeric.factor(options_data$strike)
  options_data[is.na(options_data)] <- 0 
  options(warn = oldw)
  options_data
}

#16-Mar is the near contract and 16-June is the far contract.
F_near <- as.numeric(as.character(options_data[273,6]))
F_far <- as.numeric(as.character(options_data[274,6]))

#Taking third week options data as it is near to these contracts. 
options_near_calls <- options_data[23873:24119,c(1,6)]
options_near_calls <- clean_options_data(options_near_calls)

options_far_calls <- options_data[24599:24836,c(1,6)]
options_far_calls <- clean_options_data(options_far_calls)

options_near_puts <- options_data[25646:25892,c(1,6)]
options_near_puts <- clean_options_data(options_near_puts)

options_far_puts <- options_data[26372:26609,c(1,6)]
options_far_puts <- clean_options_data(options_far_puts)

# Calculates the weights in the VIX formula
calc_weight <- function(date1,date2,calculation_date,ndays){
  T2 = as.numeric(date2 - calculation_date)
  T1 = as.numeric(date1 - calculation_date )
  weight = (T2-ndays)/(T2-T1)
  return(weight)
}


# calculates the delta K vector given a K vectos, K-strike levels
calc_delta_K <- function(K){
  K_upper <- K[3:length(K)]
  K_lower <- K[1:(length(K)-2)]
  first <- K[2] - K[1]
  last <- K[length(K)] - K[length(K)-1]
  deltaK <- (K_upper - K_lower)/2
  return(c(first,deltaK,last))
}

# calculates the (delta-k/Square(K))*option_value over all options and gives the total sum. 
total_options_value <- function(options_data){
  deltaK_by_K_square <- options_data$deltaK / (options_data$strike^2)
  return(sum(deltaK_by_K_square*options_data$value))
}

# calculates the forward price
calc_forward_price <- function(r,t,calls_data, puts_data, F0){
  K0 = max(calls_data$strike[calls_data$strike<=F0])
  relevant_calls_data = calls_data %>% filter(strike >= K0)
  relevant_calls_data$deltaK = calc_delta_K(relevant_calls_data$strike)
  relevant_puts_data = puts_data %>% filter(strike <= K0)
  relevant_puts_data$deltaK = calc_delta_K(relevant_puts_data$strike)
  result = (total_options_value(relevant_puts_data)+total_options_value(relevant_calls_data))*2*exp(r*t) - (((F0/K0)-1)^2)
  cat(result,"\n")
  return(result)
}

# calculates the VIX
calculate_VIX <-function(r,near_calls_data,near_puts_data,far_calls_data,far_puts_data,F0_near,F0_far,near_expiry_date,far_expiry_date,calculation_date){
  weight <- calc_weight(near_expiry_date,far_expiry_date,calculation_date,30)
  tou1 <- as.numeric(near_expiry_date - calculation_date)/365
  tou2 <- as.numeric(far_expiry_date - calculation_date)/365
  P1 = calc_forward_price(r,tou1,near_calls_data,near_puts_data,F0_near)
  P2 = calc_forward_price(r,tou2,far_calls_data,far_puts_data,F0_far)
  VIX = 100*sqrt((365/30)*(weight*P1+(1-weight)*P2))
  return(VIX)
}

near_expiry_date = as.Date("18/03/16","%d/%m/%y")
far_expiry_date = as.Date("17/06/16","%d/%m/%y")
Today = as.Date("18/02/16","%d/%m/%y")
N = 30

r = 0.0035  # took this as 2-year yield rate is 0.70 today. 
calculate_VIX(r,options_near_calls,options_near_puts,options_far_calls,options_far_puts,F_near,F_far,near_expiry_date,far_expiry_date,Today)
