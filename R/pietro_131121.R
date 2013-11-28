library('randomForest')
library('PCA')


# load the data
load("/home/mylonasr/work/ager_melo/data/golden_pos/xcmsXsetObject.RData")
DM <- t(peakTable[,3:146])
rownames(DM)


# replace the rownames with the once containing the #apples-per-tree information
rownames.new <- read.table('/tmp/golden_pos_new_rownames.csv', header=FALSE)
rownames(DM) <- rownames.new[,1]


# get the only samples which were not stored
DM.racc <-DM[grep("racc", rownames(DM)),]
str(DM.racc)


# get the conditions 
trait <- sapply(rownames(DM.racc),function(x){
	one <- strsplit(x,"_")[[1]][7]
})

# order the factors
trait <- factor(trait, levels=c("low", "medium", "high"))



# run randomForest on the data
forest <- randomForest(DM.racc, trait)
forest
forest$importance


# only look at extrem cases "low" and "high"
DM.racc.bin <- DM.racc[!(trait == "medium"),]
trait.bin <- trait[!(trait == "medium")]
trait.bin <- factor(trait.bin)
forest <- randomForest(DM.racc.bin, trait.bin)


# look at PCA
mypca <- PCA(scale(DM.racc))
scoreplot(mypca, col=trait)
# maybe have to call X11()


# look at studentt values using BioMark
ps <- get.biom(DM.racc.bin,trait.bin, fmethod = "studentt", type = "coef")
pp <- coef(ps)
hist(pp$studentt)


# compute the t-test
ps <- rep(0,ncol(DMracc.bin))
for (i in 1:length(ps)){
	ps[i] <- t.test(DMracc.bin[trait.bin == "medium",], DMracc.bin[trait.bin == "high",], alternative = "two.sided")$p.value
}

