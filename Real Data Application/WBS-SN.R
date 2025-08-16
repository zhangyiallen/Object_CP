library(BatchGetSymbols)
library(reshape2)
library(MASS)
library(tidyquant)

filled.contour3 <-function (x = seq(0, 1, length.out = nrow(z)),
                            y = seq(0, 1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
                            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
                            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
                            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
                            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
                            axes = TRUE, frame.plot = axes,mar, ...) 
{
  # modification by Ian Taylor of the filled.contour function
  # to remove the key and facilitate overplotting with contour()
  # further modified by Carey McGilliard and Bridget Ferris
  # to allow multiple plots on one page
  
  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      }
      else {
        z <- x
        x <- seq.int(0, 1, length.out = nrow(z))
      }
    }
    else stop("no 'z' matrix specified")
  }
  else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
    stop("increasing 'x' and 'y' values expected")
  # mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
  # on.exit(par(par.orig))
  # w <- (3 + mar.orig[2]) * par("csi") * 2.54
  # par(las = las)
  # mar <- mar.orig
  plot.new()
  # par(mar=mar)
  plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
    stop("no proper 'z' matrix specified")
  if (!is.double(z)) 
    storage.mode(z) <- "double"
  .filled.contour(as.double(x), as.double(y), z, as.double(levels), 
                  col = col)
  if (missing(plot.axes)) {
    if (axes) {
      title(main = "", xlab = "", ylab = "")
      Axis(x, side = 1)
      Axis(y, side = 2)
    }
  }
  else plot.axes
  if (frame.plot) 
    box()
  if (missing(plot.title)) 
    title(...)
  else plot.title
  invisible()
}

filled.legend <- function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
                                                                        length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
                           ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
                           levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
                           col = color.palette(length(levels) - 1), plot.title, plot.axes, 
                           key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
                           axes = TRUE, frame.plot = axes, ...) 
{
  # modification of filled.contour by Carey McGilliard and Bridget Ferris
  # designed to just plot the legend
  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      }
      else {
        z <- x
        x <- seq.int(0, 1, length.out = nrow(z))
      }
    }
    else stop("no 'z' matrix specified")
  }
  else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
    stop("increasing 'x' and 'y' values expected")
  #  mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
  #  on.exit(par(par.orig))
  #  w <- (3 + mar.orig[2L]) * par("csi") * 2.54
  #layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
  #  par(las = las)
  #  mar <- mar.orig
  #  mar[4L] <- mar[2L]
  #  mar[2L] <- 1
  #  par(mar = mar)
  # plot.new()
  plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
              yaxs = "i")
  rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
  if (missing(key.axes)) {
    if (axes) 
      axis(4)
  }
  else key.axes
  
  if (frame.plot)
    box()
  if (missing(plot.title))
    title(...)
  else plot.title
  invisible()
  box()
}

#data <- tq_exchange("NASDAQ")
#tickers <- data$symbol

first.date <- "2015-01-01"
last.date <- "2022-12-31"

df.SP500 <- GetSP500Stocks()
tickers <- df.SP500$Tickers

l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date, 
                         type.return="log",
                         freq.data = "daily")

head(l.out$df.control)
head(l.out$df.tickers)
#ret.adjusted.prices <- calc.ret(l.out$df.tickers$price.close,l.out$df.tickers$ticker,type.return="arit")

date <- unique(l.out$df.tickers$ref.date)

dataframe <- data.frame("Date"=l.out$df.tickers$ref.date, "Company" = l.out$df.tickers$ticker, "Return" = l.out$df.tickers$ret.adjusted.prices)
dataframe <- dcast(dataframe, Date ~ Company, value.var = "Return")

DateArray <- sapply(date, function(s){
  s<-as.character(s)
  return(strtoi(unlist(strsplit(s, "-")), 10L))  
})
#save(DateArray, file="date.RData")

dataframe <- dataframe[,-1]
X <- as.matrix(dataframe)
colnames(X) <- NULL
X <- t(X)

u1 <- quantile(X, 0.01, na.rm = TRUE)
u2 <- quantile(X, 0.99, na.rm = TRUE)
h1 <- (u2-u1)/10

############### plot densities ########################
dens <- vector(mode = "list", length = 96)
grid_size = 51
for (yr in c(2015:2022)) {
  for (mon in c(1:12)) {
    index <- which(DateArray[1,]==yr & DateArray[2,] == mon)

    x <- as.vector(X[,index])
    i <- (yr-2015)*12 + mon
    dens[[i]] <- density(x, bw=h1, n=grid_size, from=u1, to=u2, na.rm = TRUE)
  }
}

Y <- sapply(c(1:length(dens)), function(i){
  return(dens[[i]]$y)
})
dat <- list("x" = c(1:length(dens)), "y"=c(1:51), "z"=t(Y))
par(mar = c(3, 3, 2, 2))
filled.contour3(dat, xlim = c(1, length(dens)), ylim = c(1, 51), zlim = c(0, max(Y)), cex.axis=1.5, cex.lab=1.5, axes = FALSE)
axis(1, at=c(1, 16, 32, 48, 64, 80, 96), labels= c("", "Apr. 2016", "Aug. 2017", "Dec. 2018", "Apr. 2020", "Aug. 2021", "Dec. 2022") )
axis(2, at=c(1, 10, 20, 30, 40, 51), labels= c(-0.06, -0.038, -0.015,  0.008,  0.031,  0.056))
lines(c(rep(61, 51)), c(1:51), col="red", lty=2, lwd=2)

plot.new()
par(new = "TRUE",plt = c(0.05,0.1,0.25,0.85), las = 0, cex.axis = 1)
filled.legend(dat, xlim = c(1, length(dens)), ylim = c(1, 51), zlim = c(0, max(Y)), las=0)
dev.off()


library(pracma)

######################################################################

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
wbs1 <- function(s,e, Data){
  print(c(s,e))
  if((e-s)<20){
    return(NULL)
  }else{
    Mse <- I[I[,1]>=s,]
    if(length(Mse)==0){
      return(NULL)
    }else if(length(Mse)==2){
      if(Mse[2]>e){
        return(NULL)
      }else{
        index <- c(Mse[1]:Mse[2])
        temp <- Data[index,]
        val = get_D1(t(temp))
        #print(val)
        if(val[1] > xi){
          b0 = val[2] + Mse[1] -1
          return(c(wbs1(s,b0, Data), b0, wbs1(b0+1,e, Data)))
        }else{
          return(NULL)
        }
      }
    }else{
      Mse <- Mse[Mse[,2]<=e,]
      if(length(Mse)==0){
        return(NULL)
      }else if(length(Mse)==2){
        index <- c(Mse[1]:Mse[2])
        temp <- Data[index,]
        val = get_D1(t(temp))
        #print(val)
        if(val[1] > xi){
          b0 = val[2] + Mse[1] - 1
          return(c(wbs1(s,b0, Data),b0, wbs1(b0+1,e, Data)))
        }else{
          return(NULL)
        }
      }else{
        k <- dim(Mse)[1]
        values <- matrix(rep(0,k*2), k, 2)
        for(i in c(1:k)){
          
          index <- c(Mse[i,1]:Mse[i,2])
          temp <- Data[index,]
          values[i,] <- get_D1(t(temp))
        }
        m0 <- which.max(values[,1]) 
        b0 <- values[m0,2] + Mse[m0,1]-1
        if(length(m0)>0 && values[m0,1] > xi){
          return(c(wbs1(s,b0, Data),b0, wbs1(b0+1,e, Data)))
        }else{
          return(NULL)
        }
      }
    }
  }
}


set.seed(1)
n=96
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
I <- get_intervals(n, M)
I <- floor(I*n)
#xi <- 10.00126
xi <- 9.756101

library(fdadensity)
qSup <- seq(from=0, to = 1, length.out = 51)
qf <- sapply(dens, function(den){
  fdadensity::dens2quantile(den$y, dSup = den$x, qSup = qSup)
})
wbs1(1, n, t(qf))






