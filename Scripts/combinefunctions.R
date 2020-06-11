# We have three types of data. This scripts contains function to read in these data
# 1. Physiology: software is VSRRP98, file extension is .vrdf
# 2. Responses during experiment. Software is Presentation, different extensions
# 3. Survey responses. This is a qualtrics file

# This function reads in the responses to questions in the presentation data
read.presentation <- function(file){  
temp <- t(read.delim(file, header=FALSE))
partisanship <- t(data.frame(c(temp[c(1,2),c(17:20)])))
colnames(partisanship) <- paste0("partisan",seq(1,8,1))
inparty <- temp[,14][2]
outparty <- temp[,15][2]
if(temp[1,21]=="inparty_emo"){inparty.emotions <- t(data.frame(temp[2,c(22:33)]))
outparty.emotions <- t(data.frame(temp[2,c(35:46)]))
condition.in <- 1} # in-party is first
if(temp[1,21]=="outparty_emo"){outparty.emotions <- t(data.frame(temp[2,c(22:33)]))
inparty.emotions <- t(data.frame(temp[2,c(35:46)]))
condition.in <- 2} # in-party is second
colnames(inparty.emotions) <- paste0("inparty_",temp[1,c(22:33)])
colnames(outparty.emotions) <- paste0("outparty_",temp[1,c(22:33)])

condition1 <- temp[,48][1]
condition2 <- temp[,63][1]
condition.mv.in <- ifelse(condition1%in%c("in_lobby", "in_bedrijf"),1,2) #### 1 = In eerst, out tweede, 2 = omgekeerd
condition.mv.bedrijf <- ifelse(condition1%in%c("in_bedrijf","out_bedrijf"),1,2) ### 1 = bedrijf eerst & lobby tweede,  2 = omgekeerd

if(condition.mv.in==1){condition.mv.in.emotions <- t(data.frame(temp[2,c(50:61)]))
condition.mv.out.emotions <- t(data.frame(temp[2,c(65:76)]))}

if(condition.mv.in==2){condition.mv.out.emotions <- t(data.frame(temp[2,c(50:61)]))
condition.mv.in.emotions <- t(data.frame(temp[2,c(65:76)]))}

colnames(condition.mv.in.emotions) <- paste0("condition.mv.in_",temp[1,c(22:33)])
colnames(condition.mv.out.emotions) <- paste0("condition.mv.out_",temp[1,c(22:33)])

output <- data.frame(partisanship,inparty,outparty,inparty.emotions,outparty.emotions, condition1, condition2, condition.in, condition.mv.in, condition.mv.bedrijf, condition.mv.in.emotions, condition.mv.out.emotions)
rownames(output) <- NULL
output <- output[,order(colnames(output))]
return(output)
}

# This functions reads in the sequence of stimuli from the presentation data
read.disgust <- function(file,marker.file){
  disgust.responses <- read.table(file, sep="\t", nrows=1)[c(158,161,164,167,170,173,176)] ### ignore warning message
  disgust.responses <- ifelse(disgust.responses==-1,50,disgust.responses) ### -1 are people who didnt move the slider
  disgust.films <- read.delim(marker.file,skip=2)[,4]
  films <- c("Start_basket", "A", "B", "C", "D", "E", "F")
  disgust.films <- disgust.films[which(disgust.films%in%films)][1:7]
  disgust.films <- data.frame(matrix(disgust.films, nrow=1, ncol=7), stringsAsFactors = FALSE)
  output <- data.frame(disgust.responses, disgust.films)
  colnames(output)[8:14] <- paste0("pic.order.",seq(1,7,1))
  colnames(output)[1:7] <- paste0("disgust.pic.",disgust.films)
  output <- output[,order(names(output))]
  return(output)
}

# This functions reads in the physiology data
read.vrdf <- function(file, begin, measure){
  raw.data <- readLines(file)
  isi <- seq(begin, begin+21,2)
  output <- sapply(seq(begin,begin+21,1),function(x) {
    data <- as.numeric(unlist(strsplit(raw.data[x],split=" ")))
    if(any(is.na(data))){
      tail <- tail(data[-which(is.na(data))],2) 
      scl <-  data[-which(is.na(data))][-c(1:4)]}
    if(any(is.na(data))==FALSE){
      tail <- tail(data,2)
      scl <-  data[-c(1:4)] 
    }
    output <- ifelse(x%in%isi,mean(tail),
                     ifelse(measure=="scl", mean(scl),
                            mean(data, na.rm=TRUE)))
  })
  return(output)
}

# This functions combines presentation data with vsrrp98 data to order the physiology data
identify.main.conditions <- function(respondent,measure){
  select.data <- which(str_split_fixed(colnames(phys.data),"[.]",3)==measure)
  select.row <- which(phys.data$respondent==respondent)
  row <- which(presentation.data.all$respondent==respondent)
  if(length(select.row)>0){
  data <- phys.data[select.row,select.data]
  ingroup_isi <- ifelse(presentation.data.all$condition.in[row]==1,data[,1],data[,3])  
  ingroup <- ifelse(presentation.data.all$condition.in[row]==1,data[,2],data[,4]) 
  outgroup_isi <- ifelse(presentation.data.all$condition.in[row]==1,data[,3],data[,1]) 
  outgroup <- ifelse(presentation.data.all$condition.in[row]==1,data[,4],data[,2]) 
  ingroup_isi_mv <- ifelse(presentation.data.all$condition.mv.in[row]==1,data[,5],data[,7])
  ingroup_mv <- ifelse(presentation.data.all$condition.mv.in[row]==1,data[,6],data[,8])
  outgroup_isi_mv <- ifelse(presentation.data.all$condition.mv.in[row]==1,data[,7],data[,5])
  outgroup_mv <- ifelse(presentation.data.all$condition.mv.in[row]==1,data[,8],data[,6])  
  output <- data.frame(ingroup_isi, ingroup, outgroup_isi, outgroup, ingroup_isi_mv, ingroup_mv, outgroup_isi_mv, outgroup_mv)
  temp <- seq(which(colnames(presentation.data.all)=="pic.order.1"),which(colnames(presentation.data.all)=="pic.order.7"),1)
  disgust.pics <- presentation.data.all[row,temp]
  pick.column <- cbind(seq(9,21,2),seq(10,22,2))
  basket_isi <- data[,9]
  basket <- data[,10]
  lepel_isi <-  data[,pick.column[which(disgust.pics=="A"),1]]
  lepel  <- data[,pick.column[which(disgust.pics=="A"),2]]
  worms_isi <- data[,pick.column[which(disgust.pics=="B"),1]]
  worms <- data[,pick.column[which(disgust.pics=="B"),2]]
  beker_isi  <- data[,pick.column[which(disgust.pics=="C"),1]]
  beker <- data[,pick.column[which(disgust.pics=="C"),2]]
  lamp_isi <- data[,pick.column[which(disgust.pics=="D"),1]]
  lamp <- data[,pick.column[which(disgust.pics=="D"),2]]
  poop_isi <- data[,pick.column[which(disgust.pics=="E"),1]]
  poop <- data[,pick.column[which(disgust.pics=="E"),2]]
  vomit_isi <- data[,pick.column[which(disgust.pics=="F"),1]]
  vomit <- data[,pick.column[which(disgust.pics=="F"),2]]
  output <- data.frame(output, basket_isi,basket,lepel_isi,lepel, worms_isi,worms,lamp_isi, lamp, beker_isi, beker,poop_isi,poop, vomit_isi, vomit)
  colnames(output) <- paste0(colnames(output),"_",measure)
  output$respondent <- respondent
  return(output)           
}}
