newtonraphson <- function(f, df, x0, eps = 1e-9, max.iter = 100){
f0 <- f(x0)
df0 <- df(x0)
iter <- 0
while ((abs(f0) > eps) && (iter < max.iter)) {
x1 <- x0 - f0/df0
f0 <- f(x1)
df0 <- df(x1)
iter <- iter + 1
print(paste("At iteration", iter, "value of x is:", x1))
x0 <- x1
}
if (abs(f0) > eps) {
print("Algorithm failed to converge")
return(NULL)
} else {
print("Algorithm converged")
return(x1)
}
}