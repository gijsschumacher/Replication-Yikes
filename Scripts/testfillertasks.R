files <- list.files("C:/Users/gschuma1/surfdrive/Data/Lab data collection Fall 2019/temp", full.names=TRUE)

read.vrdf <- function(file, begin, measure){
  raw.data <- readLines(file)
  output <- sapply(seq(begin,begin+12,1),function(x) {
    as.numeric(unlist(strsplit(raw.data[x],split=" "))) })
  means <- sapply(output, mean, na.rm=TRUE)
  sds <- sapply(output, sd, na.rm=TRUE)
  data <- data.frame(means)
  return(data)
}

scl <- lapply(files,read.vrdf,begin=10, measure="scl")  ### 10 here is the first row in the raw output scl begins
scl <- do.call("cbind",scl)
apply(scl,1,mean)


emg1 <- lapply(files,read.vrdf,begin=30, measure="emg1")  ### 10 here is the first row in the raw output scl begins
emg1 <- do.call("cbind",emg1)
apply(emg1,1,mean)


emg2 <- lapply(files,read.vrdf,begin=50, measure="emg2")  ### 10 here is the first row in the raw output scl begins


