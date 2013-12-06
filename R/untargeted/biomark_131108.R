library("BioMark")


# list of experiment names
varieties <- c('fuji','golden', 'pink_lady')
modes <- c('pos', 'neg')

# paths
data_path <- '/home/mylonasr/work/ager_melo/data/'


# create resulting data.frames
pls.pos <- data.frame(rt=numeric(), mz=numeric(), sample=character(), stringsAsFactors=TRUE)
pls.neg <- data.frame(rt=numeric(), mz=numeric(), sample=character(), stringsAsFactors=TRUE)
studentt.pos <- data.frame(rt=numeric(), mz=numeric(), sample=character(), stringsAsFactors=TRUE)
studentt.neg <- data.frame(rt=numeric(), mz=numeric(), sample=character(), stringsAsFactors=TRUE)

for(variety in varieties){

	for(mode in modes){

		exp <- paste0(variety, '_', mode)

		# load and prepare data
		load(paste0(data_path, exp, '/xcmsXsetObject.RData'))

		# load the data
		sample.nr <- dim(peakTable)[2]
		DM <- t(peakTable[,3:sample.nr])
		rt <- peakTable[,"rt"]
		mz <- peakTable[,"mz"]

		# remove QC
		DM.filt <- DM[grepl("QC", rownames(DM)) == FALSE,]

		# create group factors
		groups <- c(rep("racc",dim(DM.filt)[1]))
		groups[grep("cons", rownames(DM.filt))] <- "cons"
		groups <- as.factor(groups)

		bmarks <- get.biom(DM.filt, groups, fmethod="pls")
		#bmarks <- get.biom(DM.filt, groups, fmethod="studentt" type="stab", scale.p="auto")

		# add to pls table
		rt.pls <- rt[bmarks$pls[[1]]$biom.indices]
		mz.pls <- mz[bmarks$pls[[1]]$biom.indices]
		pls <- data.frame(rt.pls, mz.pls, rep(variety, length(mz.pls)))
		colnames(pls) <- c("rt", "mz", "sample")

		if(mode == "neg"){
			pls.neg <- rbind(pls.neg, pls)
		}else{
			pls.pos <- rbind(pls.pos, pls)
		}

		# add to studentt table
		rt.studentt <- rt[bmarks$studentt[[1]]$biom.indices]
		mz.studentt <- mz[bmarks$studentt[[1]]$biom.indices]
		studentt <- data.frame(rt.studentt, mz.studentt, rep(variety, length(mz.studentt)))
		colnames(studentt) <- c("rt", "mz", "sample")

		if(mode == "neg"){
			studentt.neg <- rbind(studentt.neg, studentt)
		}else{
			studentt.pos <- rbind(studentt.pos, studentt)
		}
	}
}


library("ggplot2")
# plot
p <- ggplot(pls.pos, aes(rt, mz))
p <- p + geom_point(aes(colour=sample, shape=sample), size=3, alpha=0.5)
ggsave("/tmp/pls_pos.png")





write.csv(pls.pos, file = "/tmp/pls_pos.csv")
write.csv(pls.neg, file = "/tmp/pls_neg.csv")



