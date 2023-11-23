func <- function(x){
  return (log(x)-exp(-x))
}

compute_img <- function(in_x){
  #Accept vector as input 
  #return their associated output vector
  out_y <- as.numeric()
  for(x in in_x){
    if(x <= 0){
      cat('Sorry, the function are defined as x >0 ')
      break
    }
    y <- func(x)
    out_y <- append(out_y, round(y,2))
  }
  x_y <- data.frame(x=in_x,y=out_y)
  return (x_y)
}
library(ggplot2)
behave_func <- function(input,x_opt){
  #this func return plot of our function

  x_y_cor <- compute_img(input)
  #ggplot
  plot1 <- ggplot(x_y_cor, aes(x = x, y = y,color='blue')) +  
              geom_line(color = "darkred") +  
              geom_point(color= 'red',size=3,shape=16)+
              labs(title = "Secant method")

  print(plot1)
}

secant_method <- function(x1,x2, f = NULL,epsilon = 1e-9){
  #accept two initial input to get the optimum
  #by the end function return the optimum number and 
  #graph that describe how the iterative sequences 
  #converge to the desired solution
  x1 <- x1 
  x2 <- x2
  seq_x <- c(x1)

  while(abs(x2-x1) > epsilon){
    seq_x <- append(seq_x, x2)

    x0 <- x2 
    x2<- x2 - f(x2) * ((x2-x1)/(f(x2)-f(x1)))
    x1 <- x0
    
  }


  behave_func(seq_x,x2)

  return (paste('The solution is: ',x2))
}