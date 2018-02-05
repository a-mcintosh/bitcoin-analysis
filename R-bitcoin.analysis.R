#  ------------------------------  ------------------------------
#  -- Aubrey McIntosh, PhD  2018-02-04
#  -- R code to pull in bitcoin csv file and create some plots
#  -- This script and the .csv file could live in ~/projects/bitcoin-analysis
#  ------------------------------  ------------------------------
#  -- in R, type the command
#  -- > source("~/projects/bitcoin-analysis/R-bitcoin.analysis.R")
#  ------------------------------  ------------------------------
bitcoin_lab <- read.csv("~/projects/bitcoin-analysis/bitcoin-lab-blocktimes.csv", header=F)
colnames(bitcoin_lab) <- c("nHeight","MedianTimePrev","nTime", "calctime")
# png("bitcoin-analysis.png")
dev.new(width=16, height=10)
par( mfrow=c(3,2))

#  ------------------------------  ------------------------------
#  --  Histogram of MedianPrevTime
present<-c(2:14,21:27,34:10007)
lambda <- 12
x <- c(0:30)
y <- dpois( x, lambda)
nblock <- length(present)
plot( x, nblock*y, col="black", pch=3, ylab="Block Count", xlab="MedianTimePast (half-block units)", ylim=c(0,0.15*nblock));

one_block <- (bitcoin_lab[10007,"nTime"]-bitcoin_lab[2,"nTime"]) / ( 10007 - 2)
breaks <- seq(0, 16*one_block, one_block/2)
points( type="p", table( cut( bitcoin_lab[present,"calctime"], breaks, right=FALSE)), col="black", ylim=c(0,1250), pch=0)
legend( "topright", pch=c(0,3), title="Source of Data", c("blockchain", "Poisson calculation"))
title(main="Early Bitcoin Statistics", sub=paste("Selected", toString(nblock), "Blocks"))
abline( v=lambda, col="red" )
abline( v=2*lambda, col="green" )

#  ------------------------------  ------------------------------
#  -- graph MedianPrevTime vs nHeight
plot(bitcoin_lab[present,"nHeight"], bitcoin_lab[present,"calctime"], ylab="MedianTimePast", xlab="nBlock", pch=4, cex=0.25)
abline( h=lambda / 2 * one_block, col="red" )
abline( h=lambda     * one_block, col="green" )
title(main="MedianTimePast", sub=paste("Blocktime", toString(one_block), "seconds"))

present<-c(1387:1928)
points(bitcoin_lab[present,"nHeight"], bitcoin_lab[present,"calctime"], pch=4, cex=0.25, col="blue")

#  ------------------------------  ------------------------------
plot( x, nblock*y, col="black", pch=3, ylab="Block Count", xlab="MedianTimePast (selected)", ylim=c(0,0.15*nblock));
points( type="p", table( cut( bitcoin_lab[present,"calctime"], breaks, right=FALSE)), col="blue", ylim=c(0,1250), pch=0)
legend( "topright", pch=c(0,3), title="Source of Data", c("blockchain 1387-1928", "Poisson calculation"))
title(main="Poisson Descrepancy Highlighted", sub=paste("First", "1387-1928", "Blocks"))
abline( v=lambda, col="red" )
abline( v=2*lambda, col="green" )


#  ------------------------------  ------------------------------
present<-c(1387:1928)
plot(bitcoin_lab[present,"nHeight"], bitcoin_lab[present,"calctime"], ylab="MedianTimePast", xlab="nBlock", pch=4, cex=0.25, col="blue")
abline( h=lambda / 2 * one_block, col="red" )
title(main="Highlighted MedianTimePast", sub=paste("Fast Miners"))


#  ------------------------------  ------------------------------
#  notes that don't really work.
#  intended for a data-vetting operation

bitcoin_lab[1:10,"calctime"] < 2000
present <- bitcoin_lab[0:10008,"calctime"] < 29 * one_block
present <- bitcoin_lab[ bitcoin_lab[0:10008,"calctime"] < 20000, "nHeight" ]
present <- bitcoin_lab[ bitcoin_lab[0:10008,"calctime"] > 29*one_block, "nHeight" ]
#  ------------------------------  ------------------------------
#  ------------------------------  ------------------------------
# dev.off()



