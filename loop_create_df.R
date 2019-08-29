random_growth <- rnorm(n=10000, mean=0.11, sd=0.3560766)
fcf0 <- 3619
ffcfs = data.frame()
for (i in random_growth){
  fcf1 = fcf0 *(1+i)
  fcf2 = fcf1*(1+i)
  fcf3 = fcf2*(1+i)
  fcf4= fcf3*(1+i)
  fcf5 = fcf4*(1+i)
  fcf6 = fcf5*(1+i)
  row_i = c(fcf1,fcf2,fcf3,fcf4,fcf5,fcf6)
  ffcfs = rbind(ffcfs,row_i)
  
}

class(ffcfs)
dim(ffcfs)
