optionsProfit <- function(strike, type, price, write) {

  # strike - the strike price
  # type - call or put
  # price - the price of the option
  # write - is the option long or short?
  
  option <- list(strike=strike, type=type, price=price, write=write)
  
  possible_prices <- seq(from=0, to=500, by=.01)
  
  if(type=="Call" & write==FALSE) {
    
    profit <- pmax(possible_prices - price - strike, -price)
    
  }
  
  else if(type=="Call" & write==TRUE) {
    profit <- pmin(-(possible_prices - price - strike), price)
  }
  
  else if(type == "Put" & write == FALSE) {
    
    # profit is strike - stock price - option price
    
    profit <- pmax(strike - price - possible_prices, -price)
  }
  
  else if(type=="Put" & write == TRUE) {
    profit <- pmin(-(-possible_prices - price + strike), price)
  }
  
  else if(type=="Stock" & write == FALSE) {
    profit <- possible_prices - price
  }
  
  else {
    profit <- price - possible_prices
  }
  
  #print(profit)
  data.frame(stock=possible_prices, profit=profit)
    
}
