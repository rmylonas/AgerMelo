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
pg.pinklady <- unique(res$pinklady[res$pinklady[,1] >= rt.range[1] & res$pinklady[,1] <= rt.range[2],3])
pg.golden <- unique(res$golden[res$golden[,1] >= rt.range[1] & res$golden[,1] <= rt.range[2],3])
pg.fuji <- unique(res$fuji[res$fuji[,1] >= rt.range[1] & res$fuji[,1] <= rt.range[2],3])


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

# and plot them
rt.max <- max(rt.pinklady, rt.golden, rt.fuji)
mz.max <- max(mz.pinklady, mz.golden, mz.fuji)

# plot all the points from pcgroups
plot(rt.golden, mz.golden, xlim=rt.range, ylim=c(0, mz.max), col="red", xlab="retention time [min]", ylab="m/z")
points(rt.pinklady, mz.pinklady, col="blue")
points(rt.fuji, mz.fuji, col="green")

# plot biomarkers solid
points(res$golden, col="red", pch=19)
points(res$fuji, col="green", pch=19)
points(res$pinklady, col="blue", pch=19)

# find matching mz in all three species
mz.match <- NULL # data.frame(golden=numeric(), fuji=numeric(), pinklady=numeric())
match.id <- NULL
mz.tol <- 0.01 # 0.005

for(i in 1:length(mz.pinklady)){
	pink <- mz.pinklady[i]
	for(j in 1:length(mz.fuji)){
		fu <- mz.fuji[j]
		if(pink <= (fu+mz.tol) & pink >= (fu-mz.tol)){
			for(k in 1:length(mz.golden)){
				go <- mz.golden[k]
				if(pink <= (go+mz.tol) & pink >= (go-mz.tol)){
					mz.match <- rbind(mz.match, data.frame(golden=k, fuji=j, pinklady=i))
					match.id <- rbind(match.id, data.frame(golden=id.golden[k], fuji=id.fuji[j], pinklady=id.pinklady[i]))
				}
			}
		}
	}
}



# plot all possible markers as boxplots

# plot all possible markers
par(mfrow=c(5, as.integer(nr.matches/5) + 1))

for(i in 1:nr.matches){

	id <- match.id[i,]
	# if they're all the same we got a marker
	# let's plot the stripcharts
	rt.h$pinklady[id$pinklady]
	rt.h$fuji[id$fuji]
	rt.h$golden[id$golden]

	mz.h$pinklady[id$pinklady]
	mz.h$fuji[id$fuji]
	mz.h$golden[id$golden]

	# get low values
	pinklady.racc <- DM.h$pinklady[,id$pinklady]
	pinklady.racc <- pinklady.racc[grep("racc", names(pinklady.racc))]
	pinklady.low <- pinklady.racc[grep("low", names(pinklady.racc))]
	pinklady.high <- pinklady.racc[grep("high", names(pinklady.racc))]

	fuji.racc <- DM.h$fuji[,id$fuji]
	fuji.racc <- fuji.racc[grep("racc", names(fuji.racc))]
	fuji.low <- fuji.racc[grep("low", names(fuji.racc))]
	fuji.high <- fuji.racc[grep("high", names(fuji.racc))]

	golden.racc <- DM.h$golden[,id$golden]
	golden.racc <- golden.racc[grep("racc", names(golden.racc))]
	golden.low <- golden.racc[grep("low", names(golden.racc))]
	golden.high <- golden.racc[grep("high", names(golden.racc))]

	ho <- data.frame(pinklady.low=pinklady.low, pinklady.high=pinklady.high, fuji.low=fuji.low, fuji.high=fuji.high, golden.low=golden.low, golden.high=golden.high)
	boxplot(ho, pch=1, vertical=TRUE, main=i)

}


# # get highest intensity positions
# which.max(colMeans(DM.h$golden[,id.golden[mz.match$golden]]))
# which.max(colMeans(DM.h$fuji[,id.fuji[mz.match$fuji]])) 
# which.max(colMeans(DM.h$pinklady[,id.pinklady[mz.match$pinklady]]))


# plot only 6 most intense markers
#match.id <- match.id[c(1,4,5,14,16,23,25,26,27),]
match.id <- match.id[head(order(colMeans(DM.h$pinklady[,id.pinklady[mz.match$pinklady]]), decreasing=TRUE),6), ]

par(mfrow=c(3,2))

nr.matches <- length(match.id[,1])

for(i in 1:nr.matches){

	id <- match.id[i,]
	# if they're all the same we got a marker
	# let's plot the stripcharts
	rt.h$pinklady[id$pinklady]
	rt.h$fuji[id$fuji]
	rt.h$golden[id$golden]

	mz.h$pinklady[id$pinklady]
	mz.h$fuji[id$fuji]
	mz.h$golden[id$golden]

	# get low values
	pinklady.racc <- DM.h$pinklady[,id$pinklady]
	pinklady.racc <- pinklady.racc[grep("racc", names(pinklady.racc))]
	pinklady.low <- pinklady.racc[grep("low", names(pinklady.racc))]
	pinklady.high <- pinklady.racc[grep("high", names(pinklady.racc))]

	fuji.racc <- DM.h$fuji[,id$fuji]
	fuji.racc <- fuji.racc[grep("racc", names(fuji.racc))]
	fuji.low <- fuji.racc[grep("low", names(fuji.racc))]
	fuji.high <- fuji.racc[grep("high", names(fuji.racc))]

	golden.racc <- DM.h$golden[,id$golden]
	golden.racc <- golden.racc[grep("racc", names(golden.racc))]
	golden.low <- golden.racc[grep("low", names(golden.racc))]
	golden.high <- golden.racc[grep("high", names(golden.racc))]

	ho <- data.frame(pinklady.low=pinklady.low, pinklady.high=pinklady.high, fuji.low=fuji.low, fuji.high=fuji.high, golden.low=golden.low, golden.high=golden.high)
	stripchart(ho, pch=1, vertical=TRUE, main=i)

}


