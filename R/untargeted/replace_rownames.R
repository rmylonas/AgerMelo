###################################################################
#	 prepare the data-matrix DM and replace the rownames
###################################################################

# remove all objects before processing
rm(list=ls())

# list of experiment names
varieties <- c('fuji','golden', 'pinklady')
modes <- c('pos', 'neg')

path.root <- '/home/mylonasr/work/ager_melo/'
path.metams <- paste0(path.root, 'data/metams/')
path.sampledesc <- paste0(path.root, 'data/sample_desc/')
path.perl <- paste0(path.root, 'perl/add_factor.pl')

for(variety in varieties){

	for(mode in modes){

		exp <- paste0(variety, '_', mode)

		# load and prepare data
		load(paste0(path.metams, exp, '.RData'))
		sample.nr <- dim(out$PeakTable)[2]
		DM <- t(out$PeakTable[,6:sample.nr])
		rt <- out$PeakTable[,"rt"]
		mz <- out$PeakTable[,"mz"]
		pcgroup <- out$PeakTable[,"pcgroup"]
		adduct <- out$PeakTable[,"adduct"]
		isotopes <- out$PeakTable[,"isotopes"]

		# write current row names
		write.table(rownames(DM), paste0("/tmp/", exp, "_rownames.csv"), row.names=FALSE, col.names=FALSE, sep=",")

		# call external perl script
		system(paste0(path.perl, " /tmp/", exp, "_rownames.csv ", path.sampledesc, "sample_descr_", variety, ".csv > /tmp/", exp, "_new_rownames.csv"))

		rownames.new <- read.table(paste0("/tmp/", exp, "_new_rownames.csv"), header=FALSE)
		rownames(DM) <- rownames.new[,1]

		# save prepared data
		save(DM, rt, mz, pcgroup, adduct, isotopes, file=paste0(path.metams, exp, '_prepro.RData'))
	}

}
