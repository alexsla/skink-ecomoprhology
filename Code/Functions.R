mode <- function(data) {
  # Function for mode estimation of a continuous variable
  # Kernel density estimation by Ted Harding & Douglas Bates (found on RSiteSearch)	
  
  x<-data
  lim.inf=min(x)-1; lim.sup=max(x)+1
  
  hist(x,freq=FALSE,breaks=seq(lim.inf,lim.sup,0.2))
  s<-density(x,from=lim.inf,to=lim.sup,bw=0.2)
  n<-length(s$y)
  v1<-s$y[1:(n-2)];
  v2<-s$y[2:(n-1)];
  v3<-s$y[3:n]
  ix<-1+which((v1<v2)&(v2>v3))
  
  lines(s$x,s$y,col="red")
  points(s$x[ix],s$y[ix],col="blue")
  
  md <- s$x[which(s$y==max(s$y))] 
  
  md
}

eigval_test = function(pca)
{
  #extract the eigenvalues and apply Kaiser-Guttman criterion for axes selection
  #***********************************
  ev=pca$sdev^2
  
  
  # Broken stick model
  n <- length(ev)
  bsm <- data.frame(j=seq(1:n), p=0)
  bsm$p[1] <- 1/n
  for (i in 2:n) {
    bsm$p[i] = bsm$p[i-1] + (1/(n + 1 - i))
  }
  bsm$p <- 100*bsm$p/n
  
  ### Plot eigenvalues and % of variance for each axis
  par(mfrow=c(2,1))
  barplot(ev, main="Eigenvalues", col="bisque", las=2)
  abline(h=mean(ev), col="red")  # average eigenvalue
  legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
  barplot(t(cbind(100*ev/sum(ev),bsm$p[n:1])), beside=TRUE, 
          main="% variance", col=c("bisque",2), las=2)
  legend("topright", c("Variance Explained", "Broken stick model"), 
         pch=15, col=c("bisque",2), bty="n")
  par(mfrow=c(1,1))
}


get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}