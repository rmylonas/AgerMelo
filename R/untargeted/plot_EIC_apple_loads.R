#####
# use the first part of stripchart_script to select a biomarker

mz.pinklady <- mz.h$pinklady[match.id[1,]$pinklady]


# plot TIC
library(plyr)
library(reshape2)

# break the chromatogram into 300 rt parts
rt.groups <- cut(rt, 300, labels=seq(min(rt), max(rt), length.out=300))
rt.groups.m <- melt(rt.groups)

par(mfrow=c(2,5)) # all plots on one page 
for(i in 1:10){
	my.df <- data.frame(rt=rt.groups.m, int=DM[i,])
	tic <- ddply(my.df, .(value), summarize, int = sum(int))
	
	tic.rt <- as.numeric(as.vector(tic$value))

	# remove the first entry to make graph prettier (we should better fill missing values with 0..)
	tic.int <- tail(tic$int, -1)
	tic.rt <- tail(tic.rt, -1)

	plot(tic.rt, tic.int, type="l", xlim=c(min(tic.rt), max(tic.rt)))
}

# combined plot
my.df <- data.frame(rt=rt.groups.m, int=DM[1,])
tic <- ddply(my.df, .(value), summarize, int = sum(int))

tic.rt <- as.numeric(as.vector(tic$value))
tic.int <- tic$int

plot(tic.rt, tic.int, type="l", xlim=c(min(tic.rt), max(tic.rt)))
points(res$fuji[,1], res$fuji[,4], col="darkgreen")
points(res$golden[,1], res$golden[,4], col="red")
points(res$pinklady[,1], res$pinklady[,4], col="green")

