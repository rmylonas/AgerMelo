
library(randomForest)

# list of experiment names
varieties <- c('golden','fuji', 'pinklady')

root_path <- '/home/mylonasr/work/ager_melo/'
data_path <- paste0(root_path, 'data/targeted/')

for(variety in varieties){
	exp <- paste0(variety)

	DM <- read.table(paste0(data_path, exp, '.csv'), sep=',', header=TRUE)
	DM.desc <- DM[,1:5]
	DM <- DM[,6:30]

	#DM <- DM/rowSums(DM)

	# select only racc
	#DM.racc <- DM[which(DM.desc$campionamento == "racc"),]
	# and only low and high
	filter.DM <- which(DM.desc$campionamento == "racc" & DM.desc$anno == 2012 & DM.desc$tesi != 2)

	DM.racc.bin <- DM[filter.DM,]

		# remove colums with only 0 values
	if(length(which(colSums(DM.racc.bin) == 0)) >= 1){
		DM.racc.bin <- DM.racc.bin[,-(which(colSums(DM.racc.bin) == 0))]
	}

	# and select the trait
	trait.bin <- factor(DM.desc$tesi[filter.DM])

	# applying stability based to t-test
	ps.biom <- get.biom(DM.racc.bin, trait.bin, fmethod = "studentt", type = "HC")
	sel.biom <- selection(ps.biom)$studentt[[1]]

	# compute p-values
	ps <- rep(0,ncol(DM.racc.bin))
	for (i in 1:length(ps)){
		ps[i] <- t.test(DM.racc.bin[trait.bin == 1,i], DM.racc.bin[trait.bin == 3,i], alternative = "two.sided")$p.value
	}

	ps.adj <- p.adjust(ps, method = "BH")
	selected.adj <- which(ps.adj <= 0.05)
	selected <- which(ps <= 0.05)

	print(exp)
	print(colnames(DM.racc.bin)[selected.adj])
	print(colnames(DM.racc.bin)[selected])

	# random forest
	forest <- randomForest(DM.racc.bin, trait.bin)
}