# install metaMS from local source
install.packages("/home/mylonasr/development/metaMS/metaMS/metaMS", repos = NULL, type="source")
install.packages("/home/mylonasr/Downloads/PCA", repos = NULL, type="source")

# load libraries
#library(metaMS)
library(PCA)

# load the data
load("/home/mylonasr/work/ager_melo/data/golden_pos/xcmsXsetObject.RData")

# look at data
ls()
dim(peakTable)

# simple plots
plot(xset@peaks[,"rt"], xset@peaks[,"mz"], type="p")
plot(xset@groups[,"rtmed"], xset@groups[,"mzmed"], type="p")

# prepare PCA
DM <- t(peakTable[,3:146])
str(DM)
mz <- peakTable[,"mz"]
rt <- peakTable[,"rt"]
mypca <- PCA(scale(DM))

# prepare colors
mycol <- rep(1,times=nrow(DM))
mycol[grep("QC", rownames(DM))] <- 2

# and plot
scoreplot(mypca, col=mycol)

# identify names of points by clicking on them
identify(scores(mypca)[,1],scores(mypca)[,2],labels=rownames(DM))

# normalization
DMn <- DM/rowSums(DM)

# plot changes of total int in time
mycol[grep("rac", rownames(DM))] <- 3
plot(rowSums(DM), col=mycol, type="b")


# some plots to visualize the PCA
plot(loadings(mypca)[,1])
plot(rt, loadings(mypca)[,1])
biplot(mypca)


# use "sqrt", because we look at counts => follow a poisson-distribution
mypca <- PCA(scale(sqrt(DM)))
# with parameter "scale=FALSE" it doesn't work (?)

# have to call plot first ??
#identify(rt, loadings(mypca)[,1], labels=mz)

# don't know what we're doing here
markid <- which(abs(loadings(mypca)[,1]) > 0.1)
cbind(mz[markid], rt[markid])

# cut the end of the chromatogram
idin <- rt < 23
mypca1 <- PCA(scale(sqrt(DM)[,idin]))
scoreplot(mypca1, col=mycol)


# read the table containing sample information
#samples <-read.csv("/home/mylonasr/work/ager_melo/data/sample_descr_golden.csv")
write.table(rownames(DM), "/tmp/rownames.csv", row.names=FALSE, col.names=FALSE, sep=",")

# call perl script
# /home/mylonasr/work/ager_melo/perl/add_factor.pl /tmp/rownames.csv sample_descr_golden.csv > /tmp/new_rownames.csv

rownames <- read.csv("/tmp/new_rownames.csv")
mycol[grep("high", sample.names$V1)] <- 2

mycol[grep("low", sample.names$V1)] <- 3