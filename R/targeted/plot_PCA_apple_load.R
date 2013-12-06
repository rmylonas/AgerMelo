library(PCA)

# PCA plots of ager_melo data


# list of experiment names
varieties <- c('golden','fuji', 'pinklady')

root_path <- '/home/mylonasr/work/ager_melo/'
data_path <- paste0(root_path, 'data/targeted/')
png_path <- '/tmp/png/'

for(variety in varieties){

	exp <- paste0(variety)

	print(exp)

	# load and prepare data
	DM <- read.table(paste0(data_path, exp, '.csv'), sep=',', header=TRUE)
	DM.desc <- DM[,1:5]
	DM <- DM[,6:30]

	# open png file
	png(paste0(png_path, exp, ".png"))

	# normalization
	#DM <- DM/rowSums(DM)

	# cut the end of the chromatogram
	# idin <- rt < 23
	# mypca <- PCA(scale(sqrt(DM)[,idin]))

	# select only extremes
	#filter.DM <- which(DM.desc$campionamento == "racc" & DM.desc$anno == 2012)
	filter.DM <- which(DM.desc$campionamento == "racc")

	DM.racc <- DM[filter.DM,]
	DM.desc <- DM.desc[filter.DM,]


	# remove colums with only 0 values
	if(length(which(colSums(DM.racc) == 0)) >= 1){
		DM.racc <- DM.racc[,-(which(colSums(DM.racc) == 0))]
	}

	#mypca <- PCA(scale(DM.racc))
	mypca <- PCA(scale(sqrt(DM.racc), scale=FALSE))

	# set colors
	mycol <- rep(1,times=nrow(DM.racc))
	mycol[which(DM.desc$tesi == 1)] = "blue"
	mycol[which(DM.desc$tesi == 2)] = "green"
	mycol[which(DM.desc$tesi == 3)] = "red"

	# # set symbols
	# mypch <- rep(1,times=nrow(DM.racc))
	# mypch[which(DM.desc$tesi == 1)] = 6
	# mypch[which(DM.desc$tesi == 2)] = 0
	# mypch[which(DM.desc$tesi == 3)] = 2

	mypch <- rep(1,times=nrow(DM.racc))
	mypch[which(DM.desc$anno == 2011)] = 1
	mypch[which(DM.desc$anno == 2012)] = 2


	scoreplot(mypca, col=mycol, pch=mypch)
	legend("bottomright", c('high', 'medium', 'low'), pch=c(2, 0, 6), col=c("red", "green", "blue"))

	# close file
	dev.off()
} 