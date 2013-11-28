library(metaMS)
library(PCA)

# PCA plots of ager_melo data


# list of experiment names
varieties <- c('fuji','golden', 'pink_lady')
modes <- c('pos', 'neg')

root_path <- '/home/mylonasr/work/ager_melo/'
data_path <- paste0(root_path, 'data/')
perl_path <- paste0(root_path, 'perl/add_factor.pl')
png_path <- '/tmp/png/'

for(variety in varieties){

	for(mode in modes){

		exp <- paste0(variety, '_', mode)

		# load and prepare data
		load(paste0(data_path, exp, '/xcmsXsetObject.RData'))
		sample.nr <- dim(peakTable)[2]
		DM <- t(peakTable[,3:sample.nr])
		rt <- peakTable[,"rt"]
		mz <- peakTable[,"mz"]

		# export rowname csv
		write.table(rownames(DM), paste0("/tmp/", exp, "_rownames.csv"), row.names=FALSE, col.names=FALSE, sep=",")

		# call external perl script
		system(paste0(perl_path, " /tmp/", exp, "_rownames.csv ", data_path, "sample_descr_", variety, ".csv > /tmp/", exp, "_new_rownames.csv"))

		# load new rownames
		sample.names <- read.csv(paste0("/tmp/", exp, "_new_rownames.csv"), header=FALSE)

		# open png file
		png(paste0(png_path, exp, "_apples.png"))

		# normalization
		#DM <- DM/rowSums(DM)

		# cut the end of the chromatogram
		# idin <- rt < 23
		# mypca <- PCA(scale(sqrt(DM)[,idin]))
		mypca <- PCA(scale(DM))

		# set colors
		mycol <- rep(1,times=nrow(DM))
		mycol[grep("QC", sample.names$V1)] <- "black"
		mycol[grep("high", sample.names$V1)] <- "red"
		mycol[grep("medium", sample.names$V1)] <- "green"
		mycol[grep("low", sample.names$V1)] <- "blue"
		# set symbols
		mypch <- rep(1,times=nrow(DM))
		mypch[grep("racc", sample.names$V1)] <- 0
		mypch[grep("cons", sample.names$V1)] <- 2
		# mypch[grep("2011", sample.names$V1)] <- 0
		# mypch[grep("2012", sample.names$V1)] <- 2
		scoreplot(mypca, col=mycol, pch=mypch)

		# close file
		dev.off()

	}


} 