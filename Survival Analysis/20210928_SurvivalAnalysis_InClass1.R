library(survival)


generate.data = function(N, par){
  # true value
  alpha = par[1]
  beta = par[2]
  gamma = par[3]
  # generate one dimension covariate
  z = sample(c(0, 1), N, p = c(0.5, 0.5), replace = T)
  # prepare uniform rv to generate event time
  u = runif(N)
  # find the cdf inverse function
  cdf.inverse = function(unif, alpha, beta, gammaa){
    return ((-log(1 - u) / exp(alpha + beta * z))^(1/gamma))
  }
  # generate the event time
  e = cdf.inverse(unif, alpha, beta, gamma)
  # generate the censoring
  c = rexp(N, 0.25)
  
  # compare event time and censoring and get observations time and delta
  o = c()
  delta = c()
  for (i in 1:N){
    if (e[i] >= c[i]){
      o[i] = c[i]
      delta[i] = 0
    }else{
      o[i] = e[i]
      delta[i] = 1
    }
  }
  dt = data.frame(observation = o, delta = delta, covariate = z)
  return (dt)
}

par = c()
for (i in 1:100){
  d = generate.data(N = 300, par = c(-0.5, 0.5, 2))
  mod = survreg(Surv(observation, delta) ~ covariate, data = d, dist = "weibull")
  alpha.beta = unname(-summary(mod)$coef / summary(mod)$scale)
  gamma = 1 / summary(mod)$scale
  p = c(alpha.beta, gamma)
  par = rbind(par, p)
}
apply(par, 2, mean)
apply(par, 2, sd)
