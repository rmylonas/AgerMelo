library("metaMS")
library(CAMERA)

load("/home/mylonasr/development/metaMS/metaMS/metaMS/data/FEMsettings.RData")

load("/home/mylonasr/work/ager_melo/data/golden_pos/xcmsXsetObject.RData")




# Create an xsAnnotate object
an <- xsAnnotate(xset)
# Group based on RT
anF <- groupFWHM(an, perfwhm = FEMsettings$Synapt.QTOF.NP$CAMERA$perfwhm)
# Annotate isotopes
anI <- findIsotopes(anF, mzabs = 0.01)
# Verify grouping
anIC <- groupCorr(anI, cor_eic_th = FEMsettings$Synapt.QTOF.NP$CAMERA$cor_eic_th)
#Annotate adducts
anFA <- findAdducts(anIC, polarity="positive")
