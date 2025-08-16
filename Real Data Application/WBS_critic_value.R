
SN_test <- function(ts, k){
  n <- length(ts)
  mean1 <- mean(ts[1:k])
  mean2 <- mean(ts[(k+1):n])
  inter1 <- cumsum(ts[1:k])-(1:k)*mean1
  inter2 <- cumsum(ts[n:(k+1)])-(1:(n-k))*mean2
  M1 <- sum(inter1^2)/n^2
  M2 <- sum(inter2^2)/n^2
  test_SN <- sqrt(n)*(((n-k)*k/n^2)*(mean1-mean2))/sqrt(M1+M2) 
  return(test_SN)
}
sum_squared_vals <- function(x) (sum(abs(x)^2))^(1/2)
get_D1 = function(seqi){
  b=0.15
  n = dim(seqi)[2]
  seqihead = seqi[,1:floor(n*b)]
  seqimiddle = seqi[,(floor(n*b)+1):(n-floor(n*b))]
  seqitail = seqi[,(n-floor(n*b)+1):n]
  
  #####prepare the 1 dimensional data: hat Z_t
  
  Zt = rep(0,dim(seqimiddle)[2])
  for (t in 1:dim(seqimiddle)[2]){
    headZ = seqihead -  seqimiddle[,t]
    tailZ =  seqitail -  seqimiddle[,t]
    Zt[t] = sum(apply(tailZ, 2, sum_squared_vals))-sum(apply(headZ, 2, sum_squared_vals))
  }
  
  TNpl = rep(0,dim(seqimiddle)[2])
  for (k in 1:(dim(seqimiddle)[2]-1)){
    TNpl[k] = SN_test(Zt,k)
  }
  
  return(c(max(TNpl), which.max(TNpl)+floor(n*b)))
}



set.seed(1)
n=200
M=50
get_intervals <- function(n, M){
  
  if(n<20){
    return(NULL)
  }else{
    Interval <- matrix(rep(0, 2*M), M, 2)
    
    counter = 1
    set.seed(100)
    while(counter <= M){
      s = sample(1:n,2,replace=T)
      if(s[2] - s[1] >= 20){
        Interval[counter,] = c(s[1]/n, s[2]/n)
        counter = counter + 1
      }else if(s[1] - s[2] >= 20){
        Interval[counter,] = c(s[2]/n, s[1]/n)
        counter = counter + 1
      }else{
        next
      }
    }
    return(Interval)
  }
}
intervals <- get_intervals(n, M)

result <- sapply(c(1:1000), function(k){
  set.seed(2022+k)
  print(k)
  data = t(matrix(rep(rnorm(n,0,1), 2), n, 2))
  
  Tall <- sapply(c(1:dim(intervals)[1]), function(j){
    dataj <- data[,c(floor(intervals[j,1]*n):floor(intervals[j,2]*n))]
    return(get_D1(dataj)[1])
  })
  #write.table(c(k),file="wbs_critical_value.txt", append=TRUE, col.names=FALSE, row.names=FALSE)
  return(max(Tall))
})

quantile(result, c(0.9,0.95,0.975,0.99))
#n=200, M=50: 9.478716 10.560828 11.582914 12.700542 
# n=500, M=100: 10.09956 10.89559 11.74421 13.31230 
# n=181, M=50: 9.122522 10.445949 11.101600 12.651974 
# n=120, M=50: 9.13298 10.00126 11.11184 12.13946
# n=96,  M=50: 8.905094 9.756101 10.915732 12.329864 







