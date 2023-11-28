lambda <- 0.5
mu <- 10

probNeyman <- function(x, lambda, mu) {
resultado <- 0

for (i in 0:1000) {
  aux <- resultado
  resultado <- resultado + (i^x / factorial(i)) * (mu*exp(-lambda))^i
  if(abs(aux-resultado) < 0.000000001 & i > 1) {
    break
  }
}

resultado <- resultado*(lambda^x)*(exp(-mu))*(1/factorial(x))

return(resultado)
}

probNeyman(2, .5,10)
