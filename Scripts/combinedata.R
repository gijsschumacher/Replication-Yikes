# We need to retrieve four types of data:
# 1. The survey responses in the presentation data (questions during experimental protocol)
# 2. The sequence of stimuli saved in the presentation data
# 3. The physiological data; and order it according to the order of stimuli.
# 4. The pre-experimental survey data from qualtrics

# Basic setup -------------------------------------------------------------
source("Scripts/combinefunctions.R")

#root <- str_split_fixed(getwd(), "/Papers",2)[,1]

# Retrieve presentation data ----------------------------------------------

### Main data Uva Lab
data.folder <- paste0(root,"/Data/Lab Data Collection Fall 2019/Uva Lab/")
presentation <- paste0(data.folder,"/presentation/")
files <- list.files(presentation, pattern="*hotpol.txt", full.names=TRUE)

presentation.data <- lapply(files, read.presentation)
presentation.data <- do.call("rbind",presentation.data)
presentation.data$respondent <- as.numeric(str_split_fixed(str_split_fixed(files,"presentation/",2)[,2],"_",2)[,1])

factor.vars <- c(4:27,31:42,44:64)
test <- apply(presentation.data[,factor.vars],2, function(x) as.numeric(as.character(x)))
test <- ifelse(test==-1,50,test) # if people didn't press the slider, -1 was returned, we interpret this as the begin value
presentation.data <- data.frame(presentation.data[,c(1:3,28:30,43)], test) ### chekc this

### Disgust self-report uva lab
files <- list.files(presentation, pattern="*_.txt", full.names=TRUE)[-c(16:19)] # we excluded 46-49 b/c of errors in protocol
marker.files <- list.files(presentation, pattern="*main.log", full.names=TRUE)

disgust.data <- lapply(seq(1,length(files),1), function(x) read.disgust(files[x],marker.files[x])) # ignore read.table type warnings
disgust.data <- do.call("rbind",disgust.data)
disgust.data$respondent <- as.numeric(str_split_fixed(str_split_fixed(files,"presentation/",2)[,2],"_.",2)[,1])

### Main data inscience data
data.folder <- paste0(root,"/Data/Lab Data Collection Fall 2019/InScience/")
presentation <- paste0(data.folder,"presentation/")
files <- list.files(presentation, pattern="*hotpol.txt", full.names=TRUE)

presentation.data2 <- lapply(files, read.presentation)
presentation.data2 <- do.call("rbind",presentation.data2)
presentation.data2$respondent <- as.numeric(str_split_fixed(str_split_fixed(files,"presentation/",2)[,2],"_",2)[,1])

factor.vars <- c(4:27,31:42,44:64)
test <- apply(presentation.data2[,factor.vars],2, function(x) as.numeric(as.character(x)))
test <- ifelse(test==-1,50,test) # if people didn't press the slider, -1 was returned, we interpret this as the begin value
test <- ifelse(test==-999,NA,test)
presentation.data2 <- data.frame(presentation.data2[,c(1:3,28:30,43)], test)

### Disgust self-report inscience
files <- list.files(presentation, pattern="*_.txt", full.names=TRUE)
marker.files <- list.files(presentation, pattern="*main.log", full.names=TRUE)
disgust.data2 <- lapply(seq(1,length(files),1), function(x) read.disgust(files[x],marker.files[x])) # ignore read.table type warnings
disgust.data2 <- do.call("rbind",disgust.data2)
disgust.data2$respondent <- as.numeric(str_split_fixed(str_split_fixed(files,"presentation/",2)[,2],"_.",2)[,1])

### Bind together
disgust.data.all <- rbind(disgust.data, disgust.data2)
presentation.data.all <- rbind(presentation.data, presentation.data2)
presentation.data.all <- merge(presentation.data.all, disgust.data.all, by="respondent")



# Retrieve vsrrp data -----------------------------------------------------

# Uva Lab
data.folder <- paste0(root,"/Data/Lab Data Collection Fall 2019/Uva Lab/")
vrdf <- paste0(data.folder,"/Vsrrp/Reduced")
files.uva <- list.files(vrdf, full.names=TRUE)

# Inscience
data.folder <- paste0(root,"/Data/Lab Data Collection Fall 2019/InScience/")
vrdf <- paste0(data.folder,"/Vsrrp/Reduced")
files.inscience <- list.files(vrdf, full.names=TRUE)
files <- c(files.inscience, files.uva)
filter <- ifelse(str_split_fixed(files,"_",2)[,2]=="1.vrdf",1,0)

scl <- lapply(files[filter==0],read.vrdf,begin=10, measure="scl")  ### 10 here is the first row in the raw output scl begins
scl <- do.call("rbind",scl)
colnames(scl) <- paste0("scl.sync.",seq(1,22,1))

emg1 <- lapply(files[filter==0],read.vrdf,begin=39, measure="emg1")  ### 39 here is the first row in the raw output emg1 begins
emg1 <- do.call("rbind",emg1)
colnames(emg1) <- paste0("emg1.sync.",seq(1,22,1))

emg2 <- lapply(files[filter==0],read.vrdf,begin=68, measure="emg1")  ### 68 here is the first row in the raw output emg2 begins
emg2 <- do.call("rbind",emg2)
colnames(emg2) <- paste0("emg2.sync.",seq(1,22,1))

emg1.filter <- lapply(files[filter==1],read.vrdf,begin=39, measure="emg1")  ### 10 here is the first row in the raw output scl begins
emg1.filter <- do.call("rbind",emg1.filter)
colnames(emg1.filter) <- paste0("emg1filter.sync.",seq(1,22,1))

emg2.filter <- lapply(files[filter==1],read.vrdf,begin=68, measure="emg2")  ### 10 here is the first row in the raw output scl begins
emg2.filter <- do.call("rbind",emg2.filter)
colnames(emg2.filter) <- paste0("emg2filter.sync.",seq(1,22,1))

phys.data <- data.frame(scl,emg1,emg2, emg1.filter, emg2.filter)
phys.data$respondent <- as.numeric(str_split_fixed(str_split_fixed(files[filter==0],"Reduced/",2)[,2],".v",2)[,1])
save(phys.data, file="Scripts/phys.data.RData")

# Now transform the data so that each column phys.data matches with the right condition
output.scl <- lapply(presentation.data.all$respondent,identify.main.conditions, "scl")
output.scl <- do.call("rbind",output.scl)

output.emg1 <- lapply(presentation.data.all$respondent,identify.main.conditions, "emg1")
output.emg1 <- do.call("rbind",output.emg1)

output.emg2 <- lapply(presentation.data.all$respondent,identify.main.conditions, "emg2")
output.emg2 <- do.call("rbind",output.emg2)

output.emg1.filter <- lapply(presentation.data.all$respondent,identify.main.conditions, "emg1filter")
output.emg1.filter <- do.call("rbind",output.emg1.filter)

output.emg2.filter <- lapply(presentation.data.all$respondent,identify.main.conditions, "emg2filter")
output.emg2.filter <- do.call("rbind",output.emg2.filter)

joint.output <- data.frame(output.scl,output.emg1,output.emg2, output.emg1.filter, output.emg2.filter)
full.data <- merge(presentation.data.all, joint.output, by="respondent")


# Retrieve qualtrics data -----------------------------------------------
data_nijmegen <- read.csv(paste0(root,"/Data/Lab Data Collection Fall 2019/InScience/Nijmegen_qualtrics.csv")) 
data_amsterdam <- read.csv(paste0(root,"/Data/Lab Data Collection Fall 2019/UvA Lab/Uva_qualtrics.csv"), stringsAsFactors = FALSE)
colnames(data_amsterdam)[which(colnames(data_amsterdam)=="Student")] <- "student"
keep <- which(colnames(data_amsterdam)%in%colnames(data_nijmegen))
data_amsterdam <- data_amsterdam[3:71,keep]
data_amsterdam <- data.frame(apply(data_amsterdam, 2, as.numeric)) ### to convert character to numeric
data_amsterdam <- data_amsterdam[-which(data_amsterdam$ID==1),] # this is a respondent ID 1 ...  A mistake?
data_amsterdam$alcohol <- NA # not collected
data_amsterdam$dsr_DO <- NA # not retrieved from qualtrics
data_amsterdam$FL_12_DO <- NA # not retrieved from qualtrics
data_amsterdam <- data_amsterdam[,order(names(data_amsterdam))]
data_nijmegen <- data_nijmegen[,order(names(data_nijmegen))]
data_qualtrics <- rbind(data_amsterdam,data_nijmegen)
colnames(data_qualtrics)[which(colnames(data_qualtrics)=="ID")] <- "respondent"

full.data <- merge(full.data, data_qualtrics, by="respondent")
save(full.data, file="Scripts/full.data.RData")
