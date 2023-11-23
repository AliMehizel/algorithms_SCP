fixedpoint <- function(f = NULL, g, x0, eps = 1e-9, max.iter = 100){
x1 <- g(x0)
iter <- 1
while ((abs(x1-x0) > eps) && (iter < max.iter)) {
print(paste("at iteration", iter,"the value of x is :" ,x1))
x0 <- x1
x1 <- g(x0)
iter <- iter + 1
}
if (abs(x1-x0) > eps) {
print(paste("The algorithm didn't converge"))
} else {
print(paste("The algorithm has converged"))
return(x1)
}
}
