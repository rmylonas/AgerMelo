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

	# remove colums with only 0 values
	if(length(which(colSums(DM) == 0)) >= 1){
		DM <- DM[,-(which(colSums(DM) == 0))]
	}


	mypca <- PCA(scale(sqrt(DM)))

	# set colors
	mycol <- rep(1,times=nrow(DM))
	mycol[which(DM.desc$campionamento == "racc")] = "darkgreen"
	mycol[which(DM.desc$campionamento == "cons")] = "blue"

	# set symbols
	mypch <- rep(1,times=nrow(DM))
	mypch[which(DM.desc$campionamento == "racc")] = 0
	mypch[which(DM.desc$campionamento == "cons")] = 2

	scoreplot(mypca, col=mycol, pch=mypch)
	legend("bottomright", c('harvested', 'stored'), pch=c(0, 2), col=c("darkgreen", "blue"))


	# close file
	dev.off()


} 