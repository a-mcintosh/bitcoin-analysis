#  ------------------------------  ------------------------------
#  -- R code to pull in bitcoin csv file and plot analysis
#  -- This script and the .csv file could live in ~/projects/bitcoin-analysis
#  ------------------------------  ------------------------------
#  -- in R, type the command
#  -- > source("~/projects/bitcoin-analysis/R-bitcoin.analysis.R")
#  ------------------------------  ------------------------------
bitcoin_lab <- read.csv("~/projects/bitcoin-analysis/bitcoin-lab-blocktimes.csv", header=F)
colnames(bitcoin_lab) <- c("nHeight","MedianTimePrev","nTime", "calctime")
dev.new(width=16, height=10.5)
par( mfrow=c(2,2))

#  ------------------------------  ------------------------------
present<-c(50:10008)
lambda <- 10
one_block<-751.8371
breaks <- seq(one_block, 14*one_block, one_block/2)
x <- c(1:26)
y <- dpois( x, lambda)
nblock <- 10008
plot( x, nblock*y, col="black", pch=3, ylab="Block Count", xlab="MedianTimePast (half-block units)", ylim=c(0,0.15*nblock));
points( type="p", table( cut( bitcoin_lab[0:nblock,"calctime"], breaks, right=FALSE)), col="black", ylim=c(0,1250), pch=0)
legend( "topright", pch=c(0,3), title="Source of Data", c("blockchain", "Poisson calculation"))
title(main="Early Bitcoin Statistics", sub=paste("First", toString(nblock), "Blocks"))
abline( v=lambda, col="red" )

#  ------------------------------  ------------------------------
present<-c(50:10008)
plot(bitcoin_lab[present,"nHeight"], bitcoin_lab[present,"calctime"], ylab="MedianTimePast", xlab="nBlock", pch=4, cex=0.25)
abline( h=lambda / 2 * one_block, col="red" )
title(main="MedianTimePast", sub=paste("Blocktime", toString(one_block), "seconds"))

present<-c(1387:1928)
points(bitcoin_lab[present,"nHeight"], bitcoin_lab[present,"calctime"], pch=4, cex=0.25, col="blue")

#  ------------------------------  ------------------------------
plot( x, nblock*y, col="black", pch=3, ylab="Block Count", xlab="MedianTimePast (selected)", ylim=c(0,0.15*nblock));
points( type="p", table( cut( bitcoin_lab[present,"calctime"], breaks, right=FALSE)), col="blue", ylim=c(0,1250), pch=0)
legend( "topright", pch=c(0,3), title="Source of Data", c("blockchain 1387-1928", "Poisson calculation"))
title(main="Poisson Descrepancy Highlighted", sub=paste("First", "1387-1928", "Blocks"))
abline( v=lambda, col="red" )


#  ------------------------------  ------------------------------
present<-c(1387:1928)
plot(bitcoin_lab[present,"nHeight"], bitcoin_lab[present,"calctime"], ylab="MedianTimePast", xlab="nBlock", pch=4, cex=0.25, col="blue")
abline( h=lambda / 2 * one_block, col="red" )
title(main="Highlighted MedianTimePast", sub=paste("Fast Miners"))

#  ------------------------------  ------------------------------


