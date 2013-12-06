
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

		# remove colums with only 0 values
	if(length(which(colSums(DM) == 0)) >= 1){
		DM <- DM[,-(which(colSums(DM) == 0))]
	}

	# and select the trait
	trait.bin <- factor(DM.desc$campionamento)

	# applying stability based to t-test
	ps.biom <- get.biom(DM, trait.bin, fmethod = "studentt", type = "HC")
	sel.biom <- selection(ps.biom)$studentt[[1]]

	# compute p-values
	ps <- rep(0,ncol(DM))
	for (i in 1:length(ps)){
		ps[i] <- t.test(DM[trait.bin == "racc",i], DM[trait.bin == "cons",i], alternative = "two.sided")$p.value
	}
	ps.adj <- p.adjust(ps, method = "BH")
	selected <- which(ps.adj <= 0.01)

	print(exp)
	print(selected)
	print(colnames(DM)[selected])
	# print(sort(ps.adj))

	# random forest
	#forest <- randomForest(DM.racc.bin, trait.bin)
}