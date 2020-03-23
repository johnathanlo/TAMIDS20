MakeLateArrHist <- function(FlightDelays05 = FlightDelays05, carrier= "all"){
  if(carrier == "all"){
    quant <-quantile(FlightDelays05$LATE_ARR, probs = c(.01, .99))
    plot<- ggplot(data = FlightDelays05[FlightDelays05$LATE_ARR<quant[2],], aes(x = LATE_ARR)) + 
      geom_histogram(bins = 25) + 
      labs(x = "Lateness (minutes)", title = "Late Arrivals, 1st to 99th percentiles")
    return(plot)
  }else{
    quant <-quantile(FlightDelays05$LATE_ARR[FlightDelays05$CARRIER==carrier], probs = c(.01, .99))
    plot<- ggplot(data = FlightDelays05[which(FlightDelays05$LATE_ARR<quant[2] & FlightDelays05$CARRIER == carrier) ,], aes(x = LATE_ARR)) + 
      geom_histogram(bins = 25) + 
      labs(x = "Lateness (minutes)", title = paste("Late Arrivals, ", carrier, sep = " "))
    return(plot)
  }
}
