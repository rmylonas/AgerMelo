# plot TIC
library(plyr)
library(reshape2)


# load some data
path.root <- '/home/mylonasr/work/ager_melo/'
path.metams <- paste0(path.root, 'data/metams/')
path.png <- '/tmp/png/'

varieties <- c('golden', 'fuji', 'pinklady')
variety.cols <- c('red', 'green', 'blue')
mode <- 'pos'

plot.initialized <- FALSE

# add all 3 species

for(i in 1:length(varieties)){

	variety <- varieties[i]
	color <- variety.cols[i]
	exp <- paste0(variety, '_', mode)

	# load and prepare data
	load(paste0(path.metams, exp, '_prepro.RData'))

	# get the only samples which were not stored
	DM.qc <-DM[grep("QC", rownames(DM)),]

	# break the chromatogram into 300 rt parts
	rt.groups <- cut(rt, 300, labels=seq(min(rt), max(rt), length.out=300))
	rt.groups.m <- melt(rt.groups)

	for(i in 1:length(DM.qc[,1])){
		my.df <- data.frame(rt=rt.groups.m, int=DM.qc[i,])
		tic <- ddply(my.df, .(value), summarize, int = sum(int))
		
		tic.rt <- as.numeric(as.vector(tic$value))

		# remove the first entry to make graph prettier (we should better fill missing values with 0..)
		tic.int <- tail(tic$int, -1)
		tic.rt <- tail(tic.rt, -1)

		if(plot.initialized){
			lines(tic.rt, tic.int, col=color)
		}else{
			plot(tic.rt, tic.int, type="l", xlim=c(min(tic.rt), max(tic.rt)), col=color)
			plot.initialized <- TRUE
		}
		
	}

}




