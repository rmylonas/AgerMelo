library(metaMS)

data(FEMsettings)

path.save <- "/home/mylonasr/ager_melo/"
path.root <- "/remote/watersnew/experiments/Ager melo_Sept2013/Ager_Melo_sept2013.PRO/CDFfile/_RP/"
samples <- c("fuji neg", "fuji pos", "golden neg", "pinklady neg", "pinklady pos")




for(sample in samples){

	fnames <- list.files(paste0(path.root, sample), "CDF", full.names = TRUE)

	idout <- grep("blank|STD", fnames)

	fnames1 <- fnames[-idout]

	out <- runLC(fnames1, settings = FEMsettings$Synapt.QTOF.RP)

	save(out, file = paste0(path.save, sample, ".RData"))

}