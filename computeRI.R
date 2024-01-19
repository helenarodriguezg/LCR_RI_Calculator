
computeRI <- function(metab, value, age, age_unit){ 
  
  value <- as.double(value)
  age <- as.integer(age)
  
  if (age_unit == "Years"){
    age <- age*12
  }
  
  classification <- rep(NULL, length(value))
  
  params <- fromJSON(txt = "config.json")
  
  B0 <- as.double(params[[metab]]$B0)
  B1 <- as.double(params[[metab]]$B1)
  B2 <- as.double(params[[metab]]$B2)
  Ku <- as.double(params[[metab]]$Ku)
  Kl <- as.double(params[[metab]]$Kl)
  
  fitted_mean <- B0 + B1*log(age+1) + B2*log(age+1)^2
  
  top_limit <- exp(1)^(fitted_mean + Ku)
  bottom_limit <- exp(1)^(fitted_mean - Kl)
  
  classification[(value<=top_limit & value>=bottom_limit)] <- "normal"
  classification[value>top_limit] <- "high"
  classification[value<bottom_limit] <- "low"
  
  return(list(classification, round(top_limit,0), round(bottom_limit,0))) 
} 


plotPointWithinRI <- function(metab, value, age, age_unit){ #, params){
  
  value <- as.double(value)
  age <- as.integer(age)
  
  if (age_unit == "Years"){
    age <- age*12
  }
  
  params <- fromJSON(txt = "config.json")
  
  B0 <- as.double(params[[metab]]$B0)
  B1 <- as.double(params[[metab]]$B1)
  B2 <- as.double(params[[metab]]$B2)
  Ku <- as.double(params[[metab]]$Ku)
  Kl <- as.double(params[[metab]]$Kl)
  
  age_range <- 0:(16*12)
  fitted_mean <- B0 + B1*log(age_range+1) + B2*log(age_range+1)^2
  
  top_limit <- exp(1)^(fitted_mean + Ku)
  bottom_limit <- exp(1)^(fitted_mean - Kl)
  
  # Paint sample point
  plot(age/12, value,
       main= '',
       ylab = paste(metab, '(nmol/L)'), xlab = "Age (years)",
       cex = 1.2, pch= 19, col = 'coral', xlim = c(0, 16),
       ylim = c(0, max(top_limit)),
       axes = FALSE)

  # Paint curves
  lines(age_range/12, top_limit, col = 'black', lwd = 1.5)
  lines(age_range/12, bottom_limit, col = 'black', lwd = 1.5)
  
  grid(col = "gray", lty = "dotted")
  axis(1)
  axis(2)
  

}