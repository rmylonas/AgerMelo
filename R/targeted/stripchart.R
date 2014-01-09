
# + }
# [1] "golden"
# [1]  6 18
# [1] "coniferyl.alcohol" "vanillin"         
# [1] "fuji"
# [1]  5  6 14 18
# [1] "chlorogenic.acid"            "coniferyl.alcohol"          
# [3] "procyanidin.B2...B4..as.B2." "vanillin"                   
# [1] "pinklady"
# [1]  2 14
# [1] "esculin"                     "procyanidin.B2...B4..as.B2."
# > 



root_path <- '/home/mylonasr/work/ager_melo/'
data_path <- paste0(root_path, 'data/targeted/')

exp <- 'fuji'

my.markers <- c(5)

	# open png file
	#png(paste0(png_path, exp, ".png"))

	DM <- read.table(paste0(data_path, exp, '.csv'), sep=',', header=TRUE)
	DM.desc <- DM[,1:5]
	DM <- DM[,6:30]


	mycol <- rep(1,times=nrow(DM))
	mycol[which(DM.desc$anno == 2011)] = "blue"
	mycol[which(DM.desc$anno == 2012)] = "green"

	mypch <- rep(1,times=nrow(DM))
	mypch[which(DM.desc$tesi == 1)] = 2
	mypch[which(DM.desc$tesi == 2)] = 1
	mypch[which(DM.desc$tesi == 3)] = 19

	#plot(rowSums(DM), col=mycol, pch=mypch)


	DM <- DM[which(DM.desc$anno == 2012),]

	#DM <- DM/rowSums(DM)

		# remove colums with only 0 values
	if(length(which(colSums(DM) == 0)) >= 1){
		DM <- DM[,-(which(colSums(DM) == 0))]
	}

	# and select the trait
	trait.bin <- factor(DM.desc$campionamento)
	DM.interest <- DM[,my.markers]

	low <- DM.interest[which(DM.desc$tesi == 1)]
	medium <- DM.interest[which(DM.desc$tesi == 2)]
	high <- DM.interest[which(DM.desc$tesi == 3)]

	t.test(low, high, alternative = "two.sided")$p.value


	#ho <- data.frame(low, medium, high)
	ho <- data.frame(low, high)
	#my.labels <- c("coniferyl alcohol\nharvested", "coniferyl alcohol\nstored")
	#colnames(ho) <- my.labels
	#stripchart(ho, pch=1, vertical=TRUE, main="Golden: Luteolin-7-O-Glc", method = "jitter")
	stripchart(ho, pch=1, vertical=TRUE, main="Fuji 2012 - Chlorogenic acid", method = "jitter")








root_path <- '/home/mylonasr/work/ager_melo/'
data_path <- paste0(root_path, 'data/targeted/')

exp <- 'fuji'

my.markers <- c(5)
my.names <- c("coniferyl alcohol", "vanillin", "procyanidin B2 + B4 (as B2)", "chlorogenic acid")

	DM <- read.table(paste0(data_path, exp, '.csv'), sep=',', header=TRUE)
	DM.desc <- DM[,1:5]
	DM <- DM[,6:30]

	# open png file
	#png(paste0(png_path, exp, ".png"))

	DM <- read.table(paste0(data_path, exp, '.csv'), sep=',', header=TRUE)
	DM.desc <- DM[,1:5]
	DM <- DM[,6:30]

	#DM <- DM/rowSums(DM)

		# remove colums with only 0 values
	if(length(which(colSums(DM) == 0)) >= 1){
		DM <- DM[,-(which(colSums(DM) == 0))]
	}

	# and select the trait
	trait.bin <- factor(DM.desc$campionamento)
	DM.interest <- DM[,my.markers]

	low.1 <- DM.interest[which(DM.desc$tesi == 1)]
		medium.1 <- DM.interest[which(DM.desc$tesi == 2)]
	high.1 <- DM.interest[which(DM.desc$tesi == 3)]

	t.test(low.1, high.1, alternative = "two.sided")$p.value
	# normalization
	# low.1 <- low.1/max(c(low.1, high.1))
	# high.1 <- high.1/max(c(low.1, high.1))




	ho <- data.frame(low.1, medium.1, high.1)
	#my.labels <- c("coniferyl alcohol\nharvested", "coniferyl alcohol\nstored")
	#colnames(ho) <- my.labels
	#stripchart(ho, pch=1, vertical=TRUE, main="Golden: Luteolin-7-O-Glc", method = "jitter")
	stripchart(ho, pch=1, vertical=TRUE, main="Fuji", method = "jitter")











exp <- 'pinklady'

my.markers <- c(14, 2)
my.names <- c("procyanidin", "esculin")

	DM <- read.table(paste0(data_path, exp, '.csv'), sep=',', header=TRUE)
	DM.desc <- DM[,1:5]
	DM <- DM[,6:30]

	# open png file
	#png(paste0(png_path, exp, ".png"))

	DM <- read.table(paste0(data_path, exp, '.csv'), sep=',', header=TRUE)
	DM.desc <- DM[,1:5]
	DM <- DM[,6:30]

	#DM <- DM/rowSums(DM)

		# remove colums with only 0 values
	if(length(which(colSums(DM) == 0)) >= 1){
		DM <- DM[,-(which(colSums(DM) == 0))]
	}

	# and select the trait
	trait.bin <- factor(DM.desc$campionamento)
	DM.interest <- DM[,my.markers]

	low.1 <- DM.interest[which(DM.desc$campionamento == "racc"),1]
	high.1 <- DM.interest[which(DM.desc$campionamento == "cons"), 1]
	low.2 <- DM.interest[which(DM.desc$campionamento == "racc"), 2]
	high.2 <- DM.interest[which(DM.desc$campionamento == "cons"), 2]


	# normalization
	low.1 <- low.1/max(c(low.1, high.1))
	high.1 <- high.1/max(c(low.1, high.1))
	low.2 <- low.2/max(c(low.2, high.2))
	high.2 <- high.2/max(c(low.2, high.2))


	ho <- data.frame(low.1, high.1, low.2, high.2)
	my.labels <- c("procyanidin\nharvested", "procyanidin\nstored", "esculin\nharvested", "esculin\nstored")
	colnames(ho) <- my.labels
	#stripchart(ho, pch=1, vertical=TRUE, main="Golden: Luteolin-7-O-Glc", method = "jitter")
	stripchart(ho, pch=1, vertical=TRUE, main="Pink Lady", method = "jitter")














exp <- 'fuji'

my.markers <- c(6, 18, 5, 14)
my.names <- c("coniferyl alcohol", "vanillin", "procyanidin B2 + B4 (as B2)", "chlorogenic acid")

	DM <- read.table(paste0(data_path, exp, '.csv'), sep=',', header=TRUE)
	DM.desc <- DM[,1:5]
	DM <- DM[,6:30]

	# open png file
	#png(paste0(png_path, exp, ".png"))

	DM <- read.table(paste0(data_path, exp, '.csv'), sep=',', header=TRUE)
	DM.desc <- DM[,1:5]
	DM <- DM[,6:30]

	#DM <- DM/rowSums(DM)

		# remove colums with only 0 values
	if(length(which(colSums(DM) == 0)) >= 1){
		DM <- DM[,-(which(colSums(DM) == 0))]
	}

	# and select the trait
	trait.bin <- factor(DM.desc$campionamento)
	DM.interest <- DM[,my.markers]

	low.1 <- DM.interest[which(DM.desc$campionamento == "racc"),1]
	high.1 <- DM.interest[which(DM.desc$campionamento == "cons"), 1]
	low.2 <- DM.interest[which(DM.desc$campionamento == "racc"), 2]
	high.2 <- DM.interest[which(DM.desc$campionamento == "cons"), 2]
	low.3 <- DM.interest[which(DM.desc$campionamento == "racc"), 3]
	high.3 <- DM.interest[which(DM.desc$campionamento == "cons"), 3]
	low.4 <- DM.interest[which(DM.desc$campionamento == "racc"), 4]
	high.4 <- DM.interest[which(DM.desc$campionamento == "cons"), 4]

	# normalization
	low.1 <- low.1/max(c(low.1, high.1))
	high.1 <- high.1/max(c(low.1, high.1))
	low.2 <- low.2/max(c(low.2, high.2))
	high.2 <- high.2/max(c(low.2, high.2))
	low.3 <- low.3/max(c(low.3, high.3))
	high.3 <- high.3/max(c(low.3, high.3))
	low.4 <- low.4/max(c(low.4, high.4))
	high.4 <- high.4/max(c(low.4, high.4))



	ho <- data.frame(low.1, high.1, low.2, high.2, low.3, high.3, low.4, high.4)
	my.labels <- c("coniferyl alcohol\nharvested", "coniferyl alcohol\nstored", "vanillin\nharvested", "vanillin\nstored", 
		"procyanidin\nharvested", "procyanidin\nstored", "chlorogenic acid\nharvested", "chlorogenic acid\nstored")
	colnames(ho) <- my.labels
	#stripchart(ho, pch=1, vertical=TRUE, main="Golden: Luteolin-7-O-Glc", method = "jitter")
	stripchart(ho, pch=1, vertical=TRUE, main="Fuji", method = "jitter")









exp <- 'golden'

my.markers <- c(6, 18)
my.names <- c("coniferyl alcohol", "vanillin")

	DM <- read.table(paste0(data_path, exp, '.csv'), sep=',', header=TRUE)
	DM.desc <- DM[,1:5]
	DM <- DM[,6:30]

	# open png file
	#png(paste0(png_path, exp, ".png"))

	DM <- read.table(paste0(data_path, exp, '.csv'), sep=',', header=TRUE)
	DM.desc <- DM[,1:5]
	DM <- DM[,6:30]

	#DM <- DM/rowSums(DM)

		# remove colums with only 0 values
	if(length(which(colSums(DM) == 0)) >= 1){
		DM <- DM[,-(which(colSums(DM) == 0))]
	}

	# and select the trait
	trait.bin <- factor(DM.desc$campionamento)
	DM.interest <- DM[,my.markers]

	low.1 <- DM.interest[which(DM.desc$campionamento == "racc"),1]
	high.1 <- DM.interest[which(DM.desc$campionamento == "cons"), 1]
	low.2 <- DM.interest[which(DM.desc$campionamento == "racc"), 2]
	high.2 <- DM.interest[which(DM.desc$campionamento == "cons"), 2]

	ho <- data.frame(low.1, high.1, low.2, high.2)
	my.labels <- c("coniferyl alcohol\nharvested", "coniferyl alcohol\nstored", "vanillin\nharvested", "vanillin\nstored")
	colnames(ho) <- my.labels
	#stripchart(ho, pch=1, vertical=TRUE, main="Golden: Luteolin-7-O-Glc", method = "jitter")
	stripchart(ho, pch=1, vertical=TRUE, main="Golden", method = "jitter")
	



