library(BioMark)
library(hash)

# clean data
rm(list=ls())

path.root <- '/home/mylonasr/work/ager_melo/'
path.metams <- paste0(path.root, 'data/metams/')
path.png <- '/tmp/png/'


varieties <- c('fuji', 'golden', 'pinklady')
mode <- 'neg'

res <- hash()

for(variety in varieties){

	exp <- paste0(variety, '_', mode)

	print(exp)

	# load and prepare data
	load(paste0(path.metams, exp, '_prepro.RData'))

	# get the only samples which were not stored
	DM.racc <-DM[grep("racc", rownames(DM)),]

	# get the conditions 
	trait <- sapply(rownames(DM.racc),function(x){
		one <- strsplit(x,"_")[[1]][7]
	})

	# # order the factors
	trait <- factor(trait, levels=c("low", "medium", "high"))

	# only look at extrem cases "low" and "high"
	DM.racc.bin <- DM.racc[!(trait == "medium"),]
	trait.bin <- trait[!(trait == "medium")]
	trait.bin <- factor(trait.bin)

	# compute the t-test
	ps <- rep(0,ncol(DM.racc.bin))
	for (i in 1:length(ps)){
		ps[i] <- t.test(DM.racc.bin[trait.bin == "low",i], DM.racc.bin[trait.bin == "high",i], alternative = "two.sided")$p.value
	}

	# bonferroni correction (bonferroni is strict, BH less strict)
	ps.adj <- p.adjust(ps, method = "BH")
	selected <- which(ps.adj <= 0.05)

	# applying Higher Critisims to t-test
	# ps.biom <- get.biom(DM.racc.bin, trait.bin, fmethod = "studentt", type = "HC")

	# applying stability based to t-test
	ps.biom <- get.biom(DM.racc.bin, trait.bin, fmethod = "studentt", type = "stab")
	sel.biom <- selection(ps.biom)$studentt[[1]]

	# create table of rt, mz and pcgroup and order it by rt
	tableout <- cbind(rt[sel.biom], mz[sel.biom], pcgroup[sel.biom], colMeans(DM.racc.bin[,sel.biom]), sel.biom)
	tableout <- tableout[order(rt[sel.biom]),]

	# plot TIC (cut => rowsum)

	# plot BPI (cut  => biggest per group)

	res[variety] <- tableout
}


# plot the identifications
rt.max <- max(res$pinklady[,1], res$fuji[,1], res$golden[,1])
mz.max <- max(res$pinklady[,2], res$fuji[,2], res$golden[,2])

plot(res$golden, col="red", xlim=c(0, rt.max), ylim=c(0, mz.max), xlab="retention time [min]", ylab="m/z")
points(res$fuji, col="darkgreen")
points(res$pinklady, col="blue")


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

