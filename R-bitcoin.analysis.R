#  ------------------------------  ------------------------------
#  -- Aubrey McIntosh, PhD  2018-02-04
#  -- R code to pull in a bitcoin csv file and create some plots
#  -- This script and the .csv file could live in ~/projects/bitcoin-analysis
#  ------------------------------  ------------------------------
#  -- in R, type the command
#  -- > source("~/projects/bitcoin-analysis/R-bitcoin.analysis.R")
#  ------------------------------  ------------------------------

#  ------------------------------  ------------------------------
#  --  N.B.  an R table is indexed from "1" and the Poisson distribution starts at "0"
#  --  The limits in the code were chosen so that the Poisson bin numbers are correct, and
#  --  the R histogram correctly lines up with the Poisson calculation
#  ------------------------------  ------------------------------

#  ------------------------------  ------------------------------
#  --  set up the environment
bitcoin_lab <- read.csv("~/projects/bitcoin-analysis/bitcoin-lab-blocktimes.csv", header=F)
colnames(bitcoin_lab) <- c("nHeight","MedianTimePrev","nTime", "calctime")
png("bitcoin-analysis.png", width=800, height=600)
#dev.new()
par( mfrow=c(2,2))

#  ------------------------------  ------------------------------
#  --  pre conditioning the data
one_block_time <- 10 * 60	#A well known bitcoin number.  10 min/block * 60 sec/min
initial_range=c(0:10007)
temp_vetted <- bitcoin_lab[ bitcoin_lab[initial_range,"calctime"] < (2.5 * 6 * one_block_time), "nHeight" ]
vetted <- temp_vetted[ temp_vetted < (length(initial_range)-1) ]
nblock <- length(vetted)
one_block_time <- sum( bitcoin_lab[vetted,"calctime"]) / nblock / 6

#  ------------------------------  ------------------------------
#  --  Histogram of MedianPrevTime
lambda <- 12
x <- c(1:29)
y <- dpois( x, lambda)
y_lim <- ceiling(nblock*max(y)/0.8)

plot( x, nblock*y, col="black", pch=3, ylab="Block Count", xlab="MedianTimePast (half-block units)", ylim=c(0, y_lim));

breaks <- seq(one_block_time/2, 15*one_block_time, one_block_time/2)
points( type="p", table( cut( bitcoin_lab[vetted,"calctime"], breaks, right=FALSE)), col="black", pch=0)
legend( "topright", pch=c(0,3), title="Source of Data", c("blockchain", "Poisson calculation"))
title(main="Early Bitcoin Statistics", sub=paste("vetted", toString(nblock), "Blocks"))
abline( v=lambda, col="red" )

#  ------------------------------  ------------------------------
#  -- graph MedianPrevTime vs nHeight
plot(bitcoin_lab[vetted,"nHeight"], bitcoin_lab[vetted,"calctime"], ylab="MedianTimePast", xlab="nBlock", pch=4, cex=0.25)
abline( h=lambda / 2 * one_block_time, col="red" )
title(main="MedianTimePast", sub=paste("Blocktime", toString(one_block_time), "seconds"))

anomoly<-c(1387:1928)
points(bitcoin_lab[anomoly,"nHeight"], bitcoin_lab[anomoly,"calctime"], pch=4, cex=0.25, col="blue")

#  ------------------------------  ------------------------------
#  --  Histogram of MedianPrevTime anomoly nHeight 1387:1928

plot( x, nblock*y, col="black", pch=3, ylab="Block Count", xlab="MedianTimePast (vetted)", ylim=c(0,y_lim));
points( table( cut( bitcoin_lab[anomoly,"calctime"], breaks, right=FALSE)), type="p", col="blue", ylim=c(0,1250), pch=0)
legend( "topright", pch=c(0,3), title="Source of Data", c("blockchain 1387-1928", "Poisson calculation"))
title(main="Poisson Descrepancy Highlighted", sub=paste("First", "1387-1928", "Blocks"))
abline( v=lambda, col="red" )


#  ------------------------------  ------------------------------
#  -- graph MedianPrevTime vs nHeight anomoly
old_vetted <- vetted
vetted<-c(1387:1928)
plot(bitcoin_lab[vetted,"nHeight"], bitcoin_lab[vetted,"calctime"], ylab="MedianTimePast", xlab="nBlock", pch=4, cex=0.25, col="blue")
points( table( cut( bitcoin_lab[anomoly,"calctime"], breaks, right=FALSE)), type="p", col="blue", ylim=c(0,1250), pch=0)
abline( h=lambda / 2 * one_block_time, col="red" )
title(main="Highlighted MedianTimePast", sub=paste("Fast Miners"))
vetted <- old_vetted


#  ------------------------------  ------------------------------
#  Workbench

#  cleaned before this commit

#  ------------------------------  ------------------------------
dev.off()



