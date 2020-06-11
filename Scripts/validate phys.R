# # Check EMG to EMG.filter ---------------------------------------------
which.emg1 <- which(str_split_fixed(colnames(phys.data),".s",2)[,1]=="emg1")
which.emg1.filter <- which(str_split_fixed(colnames(phys.data),".s",2)[,1]=="emg1filter")
emg1 <- stack(phys.data[,which.emg1])
emg1.filter <- stack(phys.data[,which.emg1.filter])
cor(emg1[,1], emg1.filter[,1])

# Check EMG2 to EMG2.filter
which.emg2 <- which(str_split_fixed(colnames(phys.data),".s",2)[,1]=="emg2")
which.emg2.filter <- which(str_split_fixed(colnames(phys.data),".s",2)[,1]=="emg2filter")
emg2 <- stack(phys.data[,which.emg2])
emg2.filter <- stack(phys.data[,which.emg2.filter])
cor(emg2[,1], emg2.filter[,1])


# # Find outliers ---------------------------------------------------------
low.responsive.scl <- apply(phys.data[,1:22],2, function(x) which(x<2))
low.responsive.scl <- unique(unlist(low.responsive.scl))
change.scl <- phys.data[low.responsive.scl,22] - phys.data[low.responsive.scl,1]
low.responsive.scl[which(change.scl<0.1)]  
# below I write row numbers not respondent numbers!
# after manual inspection I'd say 26 44 70 73 76 77 79 91 93 94 96 101 106 are  unresponsive (- or zeros values)

unusual.emg1 <- apply(phys.data[,23:44],2, function(x) scale(x))
which.unusual.emg1 <- apply(unusual.emg1,2, function(x) which(abs(x)>2))
# 10 is systematically higher; 16 higher after sync 4; 72, 80, 91
# 24 = wire before eye, after block 2 this was resolved. 
# maybe 51?

unusual.emg1filter <- apply(phys.data[,67:88],2, function(x) scale(x))
which.unusual.emg1filter <- apply(unusual.emg1filter,2, function(x) which(abs(x)>2))
# looks a bit better, 16 no longer outlier


unusual.emg2 <- apply(phys.data[,45:66],2, function(x) scale(x))
which.unusual.emg2 <- apply(unusual.emg2,2, function(x) which(abs(x)>2))
# 4, 9, 10, 11, 33, 67, 72 systematically high. 10 high beginning and end. 56 normal until sync 6.
# 56 wirest lost

unusual.emg2filter <- apply(phys.data[,89:110],2, function(x) scale(x))
which.unusual.emg2filter <- apply(unusual.emg2filter,2, function(x) which(abs(x)>2))
# looks a bit better, 10 only bad at last two syncs. 56 also better, still quite high values in middle.

scl.outlier <- phys.data$respondent[c(26, 44, 70, 73, 76, 77, 79, 91, 93, 94, 96, 101, 106)]
emg1.outlier <- phys.data$respondent[c(10,16,24,72,80,91)]
emg2.outlier <- phys.data$respondent[c(4,9,10,11,33,67,56, 72)]

# # Remove outliers from full.data ----------------------------------------
scl.vars <- which(str_sub(colnames(full.data),-3,-1)=="scl")
emg1.vars <- which(str_sub(colnames(full.data),-4,-1)=="emg1")
emg2.vars <- which(str_sub(colnames(full.data),-4,-1)=="emg2")

full.data[which(full.data$respondent%in%scl.outlier),scl.vars] <- NA
full.data[which(full.data$respondent%in%emg1.outlier),emg1.vars] <- NA
full.data[which(full.data$respondent%in%emg2.outlier),emg2.vars] <- NA

save(full.data, file="Scripts/full.data.RData")
