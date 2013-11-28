

load("/home/mylonasr/work/ager_melo/data/metams/golden_pos_metams.RData")
DM <- t(out$PeakTable[,6:146])

# list of experiment names
varieties <- c('fuji','golden', 'pink_lady')
modes <- c('pos', 'neg')

root_path <- '/home/mylonasr/work/ager_melo/'
data_path <- paste0(root_path, 'data/')
perl_path <- paste0(root_path, 'perl/add_factor.pl')


variety <- 'golden'
mode <- 'pos'

exp <- paste0(variety, '_', mode)

write.table(rownames(DM), paste0("/tmp/", exp, "_rownames.csv"), row.names=FALSE, col.names=FALSE, sep=",")

# call external perl script
system(paste0(perl_path, " /tmp/", exp, "_rownames.csv ", data_path, "sample_descr_", variety, ".csv > /tmp/", exp, "_new_rownames.csv"))

rownames.new <- read.table(paste0("/tmp/", exp, "_new_rownames.csv"), header=FALSE)
rownames(DM) <- rownames.new[,1]

