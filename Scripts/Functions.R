
#function to code variables to range from 0 to 1
zero1 <- function(x, minx=NA, maxx=NA){
  res <- NA
  if(is.na(minx)) res <- (x - min(x,na.rm=T))/(max(x,na.rm=T) -min(x,na.rm=T))
  if(!is.na(minx)) res <- (x - minx)/(maxx -minx)
  res
}

#function to install packages if they don't exist
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

get_lower_tri<-function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}


winsorize <- function(var, prob){
  low <- paste0(100-prob,"%")
  high <- paste0(prob,"%")  
  quantiles <- quantile(var, probs=seq(0,1,0.05), na.rm=TRUE)
  
  new.var <- ifelse(var<quantiles[low],quantiles[low],
             ifelse(var>quantiles[high],quantiles[high],var))
  return(new.var)
}

do.regression <- function(seq, options){
  vars <- options[seq,]
  outcome <- as.character(vars[1])
  exposure <- as.character(vars[2])
  hypothesis <- str_split_fixed(vars$hypothesis, ":",2)[,1]
  if(hypothesis%in%c("H1", "H2")){regression.output <- lm(get(outcome) ~ get(exposure) + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=data.reshaped)}
  if(hypothesis=="H3") {regression.output <- lm(get(outcome) ~ scale(get(exposure)) + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)}
  if(hypothesis=="H4") {regression.output <- lm(get(outcome) ~ scale(get(exposure)) + treatment +treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=data.reshaped)}
  return(regression.output)
}

extract.results <- function(model){
  coefs <- summary(model)[4]$coefficients[2,c(1,2,4)]
  ci95 <- coefci(model, level=0.95)[2,]
  ci90 <- coefci(model, level=0.9)[2,]
  output <- c(coefs,ci95,ci90)
  return(output)
}

# load local packages
packages <- c("interplot", "stringr",  "stargazer", "ggplot2", "lavaan", "xtable", "lmtest", "sandwich", "multiwayvcov", "data.table", "gridExtra", "memisc")
#ipak(packages)
lapply(packages, require, character.only = TRUE)
