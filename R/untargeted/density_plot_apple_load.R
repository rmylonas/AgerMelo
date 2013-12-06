library(BioMark)
library(hash)

# clean data
rm(list=ls())

path.root <- '/home/mylonasr/work/ager_melo/'
path.metams <- paste0(path.root, 'data/metams/')
path.png <- '/tmp/png/'


varieties <- c('fuji', 'golden', 'pinklady')
mode <- 'pos'

res <- hash()
DM.h <- hash()
rt.h <- hash()
mz.h <- hash()
pcgroup.h <- hash()

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
	DM.h[variety] <- DM
	rt.h[variety] <- rt
	mz.h[variety] <- mz
	pcgroup.h[variety] <- pcgroup
}

# plot the identifications
rt.max <- max(res$pinklady[,1], res$fuji[,1], res$golden[,1])
mz.max <- max(res$pinklady[,2], res$fuji[,2], res$golden[,2])

plot(res$golden, col="red", xlim=c(0, rt.max), ylim=c(0, mz.max), xlab="retention time [min]", ylab="m/z")
points(res$fuji, col="green")
points(res$pinklady, col="blue")

# select group of interest
rt.range <- c(3.0, rt.max)

# pg.pinklady <- unique(res$pinklady[res$pinklady[,1] >= rt.range[1] & res$pinklady[,1] <= rt.range[2],3])
# pg.golden <- unique(res$golden[res$golden[,1] >= rt.range[1] & res$golden[,1] <= rt.range[2],3])
# pg.fuji <- unique(res$fuji[res$fuji[,1] >= rt.range[1] & res$fuji[,1] <= rt.range[2],3])

# keep only once with more than min.markers biomarkers in pcgroup
min.markers <- 2
pg.pinklady <- as.numeric(names(which(table(res$pinklady[res$pinklady[,1] >= rt.range[1] & res$pinklady[,1] <= rt.range[2],3]) >= min.markers)))
pg.golden <- as.numeric(names(which(table(res$golden[res$golden[,1] >= rt.range[1] & res$golden[,1] <= rt.range[2],3]) >= min.markers)))
pg.fuji <- as.numeric(names(which(table(res$fuji[res$fuji[,1] >= rt.range[1] & res$fuji[,1] <= rt.range[2],3]) >= min.markers)))


# get all rt and mz, hope no one will ever see this code..
rt.pinklady <- vector()
mz.pinklady <- vector()
id.pinklady <- vector()
for(pg in pg.pinklady){
		rt.pinklady <- c(rt.pinklady, rt.h$pinklady[which(pcgroup.h$pinklady == pg)])
		mz.pinklady <- c(mz.pinklady, mz.h$pinklady[which(pcgroup.h$pinklady == pg)])
		id.pinklady <- c(id.pinklady, which(pcgroup.h$pinklady == pg))	
}

rt.golden <- vector()
mz.golden <- vector()
id.golden <- vector()
for(pg in pg.golden){
		rt.golden <- c(rt.golden, rt.h$golden[which(pcgroup.h$golden == pg)])
		mz.golden <- c(mz.golden, mz.h$golden[which(pcgroup.h$golden == pg)])
		id.golden <- c(id.golden, which(pcgroup.h$golden == pg))	
}

rt.fuji <- vector()
mz.fuji <- vector()
id.fuji <- vector()
for(pg in pg.fuji){
		rt.fuji <- c(rt.fuji, rt.h$fuji[which(pcgroup.h$fuji == pg)])
		mz.fuji <- c(mz.fuji, mz.h$fuji[which(pcgroup.h$fuji == pg)])
		id.fuji <- c(id.fuji, which(pcgroup.h$fuji == pg))	
}


# density plots

# add fake start and end points
rt.golden <- c(rt.golden, 0.0, 40.0)
rt.fuji <- c(rt.fuji, 0.0, 40.0)
rt.pinklady <- c(rt.pinklady, 0.0, 40.0)

par(mfrow=c(3,1))
mybw = 0.01
plot(density(rt.golden, bw=mybw), main="Golden", xlab="Retention time [min]", xlim=c(3,20), ylim=c(0,14), lab=c(10,5,7))
plot(density(rt.fuji, bw=mybw), main="Fuji", xlab="Retention time [min]", xlim=c(3,20), ylim=c(0,14), lab=c(10,5,7))
plot(density(rt.pinklady, bw=mybw), main="Pink Lady", xlab="Retention time [min]", xlim=c(3,20), ylim=c(0,14), lab=c(10,5,7))


