#This program generates the scaling factors to display two y variables on a single plot.
# Scale the right axis to match the left axis.
#set the lower and upper bounds of the left axis
            #c(lower,upper)
left_axis <- c(0,500)
#set the lower and upper bounds of the right axis
            #c(lower,upper)
right_axis <- c(0,15)


#Function to determine the scale factors
DetermineScale <- function(x, y) {
  factors <- lm(y ~ x)
  m <- signif(as.numeric(factors$coefficients["x"]),5)
  b <- signif(as.numeric(factors$coefficients["(Intercept)"]),5)
  cat("Use this formula to scale orignal values of the right axis: adj = original * ", m,
      ifelse(b>=0,"+","-"),
      b, "\n")
  cat("Use this formula to scale the right axis ticks: scale = original",
      ifelse(b>=0,"-","+"), b, "/",
      m)
}


DetermineScale(right_axis, left_axis)
