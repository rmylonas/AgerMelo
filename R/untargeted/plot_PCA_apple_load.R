library(PCA)

# PCA plots of ager_melo data


# list of experiment names
varieties <- c('golden','fuji', 'pinklady')
modes <- c('pos', 'neg')

root_path <- '/home/mylonasr/work/ager_melo/'
data_path <- paste0(root_path, 'data/metams/')
perl_path <- paste0(root_path, 'perl/add_factor.pl')
png_path <- '/tmp/png/'

for(variety in varieties){

	for(mode in modes){

		exp <- paste0(variety, '_', mode)

		print(exp)

		# load and prepare data
		load(paste0(data_path, exp, '_prepro.RData'))

		# open png file
		png(paste0(png_path, exp, ".png"))

		# normalization
		DM <- DM/rowSums(DM)

		# cut the end of the chromatogram
		# idin <- rt < 23
		# mypca <- PCA(scale(sqrt(DM)[,idin]))

		# select only extremes
		# get the only samples which were not stored
		DM.racc.bin <-DM[c(grep("racc.+(low|high|medium)+", rownames(DM), perl=TRUE),grep("QC", rownames(DM))) ,]
		#DM.racc.bin <-DM[grep("racc.+(low|high)+", rownames(DM), perl=TRUE),]


		mypca <- PCA(scale(DM.racc.bin))
		#mypca <- PCA(DM.racc.bin)

		# set colors
		mycol <- rep(1,times=nrow(DM.racc.bin))
		mycol[grep("QC", rownames(DM.racc.bin))] <- "black"
		mycol[grep("low", rownames(DM.racc.bin))] <- "blue"
		mycol[grep("high", rownames(DM.racc.bin))] <- "red"
		mycol[grep("medium", rownames(DM.racc.bin))] <- "darkgreen"

		# set symbols
		mypch <- rep(1,times=nrow(DM.racc.bin))
		mypch[grep("low", rownames(DM.racc.bin))] <- 6
		mypch[grep("high", rownames(DM.racc.bin))] <- 2
		mypch[grep("medium", rownames(DM.racc.bin))] <- 0
		mypch[grep("QC", rownames(DM.racc.bin))] <- 1
		# mypch[grep("2011", rownames(DM))] <- 0
		# mypch[grep("2012", rownames(DM))] <- 2
		scoreplot(mypca, col=mycol, pch=mypch)
		legend("bottomright", c('high', 'medium', 'low', 'QC'), pch=c(2, 0, 6, 1), col=c("red", "green", "blue", "black"))

		# close file
		dev.off()


	}


} 