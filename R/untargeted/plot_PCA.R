library(PCA)

# PCA plots of ager_melo data


# list of experiment names
varieties <- c('fuji','golden', 'pinklady')
modes <- c('pos', 'neg')

root_path <- '/home/mylonasr/work/ager_melo/'
data_path <- paste0(root_path, 'data/metams/')
perl_path <- paste0(root_path, 'perl/add_factor.pl')
png_path <- '/tmp/png/'

for(variety in varieties){

	for(mode in modes){

		exp <- paste0(variety, '_', mode)

		# load and prepare data
		load(paste0(data_path, exp, '_prepro.RData'))

		# open png file
		png(paste0(png_path, exp, ".png"))

		# normalization
		#DM <- DM/rowSums(DM)

		# cut the end of the chromatogram
		# idin <- rt < 23
		# mypca <- PCA(scale(sqrt(DM)[,idin]))
		mypca <- PCA(scale(DM))

		# set colors
		mycol <- rep(1,times=nrow(DM))
		mycol[grep("QC", rownames(DM))] <- "black"
		mycol[grep("high", rownames(DM))] <- "red"
		mycol[grep("medium", rownames(DM))] <- "green"
		mycol[grep("low", rownames(DM))] <- "blue"
		# set symbols
		mypch <- rep(1,times=nrow(DM))
		mypch[grep("racc", rownames(DM))] <- 0
		mypch[grep("cons", rownames(DM))] <- 2
		# mypch[grep("2011", rownames(DM))] <- 0
		# mypch[grep("2012", rownames(DM))] <- 2
		scoreplot(mypca, col=mycol, pch=mypch)

		# close file
		dev.off()

	}


} 