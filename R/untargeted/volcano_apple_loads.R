rm(list=ls())

mode <- 'pos'
variety <- 'pinklady'

modes <- c('pos', 'neg')
varieties <- c('golden', 'fuji', 'pinklady')

path.root <- '/home/mylonasr/work/ager_melo/'
path.metams <- paste0(path.root, 'data/metams/')
path.png <- '/tmp/png/'


par(mfrow=c(2,3))

for(mode in modes){

	for(variety in varieties){

		exp <- paste0(variety, '_', mode)

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


		# get biomarkers
		ps.biom <- get.biom(DM.racc.bin, trait.bin, fmethod = "studentt", type = "stab")
		sel.biom <- selection(ps.biom)$studentt[[1]]

		# compute the t-test
		ps <- rep(0,ncol(DM.racc.bin))
		for (i in 1:length(ps)){
			ps[i] <- t.test(DM.racc.bin[trait.bin == "low",i], DM.racc.bin[trait.bin == "high",i], alternative = "two.sided")$p.value
		}
		#ps.adj <- p.adjust(ps, method = "bonferroni")
		ps.adj <- p.adjust(ps, method = "BH")


		# compute the fold-change 
		fc <- rep(0,ncol(DM.racc.bin))
		for (i in 1:length(ps)){

			# add a 0.000001 because of the 0 values..
			corr <- 0.000001
			# low <- DM.racc.bin[trait.bin == "low",i] + corr
			# high <- DM.racc.bin[trait.bin == "high",i] + corr

			# # reorder names
			# names(low) <- sapply(names(low),function(x){ low <- strsplit(x,"_")[[1]][5] })
			# names(high) <- sapply(names(high),function(x){ high <- strsplit(x,"_")[[1]][5] })
			# low <- low[order(names(low))]
			# high <- high[order(names(high))]

			# fc[i] <- mapply("/", high, low)

			# let's compute it on the mean, since the sample names are differing.. (which is weird)
			low <- mean(DM.racc.bin[trait.bin == "low",i]) + corr
			high <- mean(DM.racc.bin[trait.bin == "high",i]) + corr
			fc[i] <- high / low

		}


		ps.log10 <- -1 * log10(ps.adj)
		fc.log2 <- log2(fc)
		plot(fc.log2, ps.log10, main=exp, xlim=c(-3,3), ylim=c(0, 3))

		abline(v = 0.0)
		#abline(h = 1.30103)

		#plot biomarkers in green
		sel.biom <- sel.biom[which(ps.log10[sel.biom] > 0.5)]
		points(fc.log2[sel.biom], ps.log10[sel.biom], col="green", pch=19)

	}
}



