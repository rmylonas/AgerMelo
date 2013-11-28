library('randomForest')
library('BioMark')
library('PCA')


varieties <- c('fuji','golden', 'pinklady')
modes <- c('pos', 'neg')

path.root <- '/home/mylonasr/work/ager_melo/'
path.metams <- paste0(path.root, 'data/metams/')
path.png <- '/tmp/png/'

for(variety in varieties){

	for(mode in modes){

		exp <- paste0(variety, '_', mode)
		print('###########################################################')
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

		# look at PCA
		# open png file
		png(paste0(path.png, exp, "_pca.png"))
		mypca <- PCA(scale(DM.racc))
		scoreplot(mypca, col=trait)
		# close file
		dev.off()

		# run randomForest on the data
		forest <- randomForest(DM.racc, trait)
		print(levels(trait))
		print(forest)

		# only look at extrem cases "low" and "high"
		DM.racc.bin <- DM.racc[!(trait == "medium"),]
		trait.bin <- trait[!(trait == "medium")]
		trait.bin <- factor(trait.bin)
		forest <- randomForest(DM.racc.bin, trait.bin)
		print(levels(trait.bin))
		print(forest)

		png(paste0(path.png, exp, "_biomark.png"))
		# look at studentt values using BioMark
		ps <- get.biom(DM.racc.bin,trait.bin, fmethod = "studentt", type = "coef")
		pp <- coef(ps)
		hist(pp$studentt)
		dev.off()

		# compute the t-test
		ps <- rep(0,ncol(DM.racc.bin))
		for (i in 1:length(ps)){
			ps[i] <- t.test(DM.racc.bin[trait.bin == "low",i], DM.racc.bin[trait.bin == "high",i], alternative = "two.sided")$p.value
		}

		png(paste0(path.png, exp, "_ttest.png"))
		hist(ps, xlim=c(0,0.1), breaks=200, col="lightgrey")
		abline(v = 0.01, col = "red")
		abline(v = 0.05, col = "red")
		dev.off()
	}
}
