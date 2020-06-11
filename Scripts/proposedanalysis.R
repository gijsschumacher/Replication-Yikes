# This is a proposal for the main analyses in the Yikes paper by Bakker & Schumacher
# The robustness checks are only added verbally here, at the end.

# Create fake data -------------------------------------------------------------
rm(list=ls())
N <- 100
#treatments <- 4

data <- data.frame(matrix(NA,N,13))
data[,1] <- seq(1,N,1)
colnames(data) <- "id"
treatments <- c("isi_1", "ingroup_pic", "isi_2", "outgroup_pic", "isi_3", "ingroup_mv", "isi_4",
                "outgroup_mv")
varnames <- rbind(expand.grid(treatments,"labii"),expand.grid(treatments[c(2,4,6,8)],"sr_disgust"))
colnames(data)[2:13] <- paste0(varnames[,1],"_",varnames[,2])

# generate labii data
for(i in c(2,4,6,8)){data[,i] <- 0} 
for(i in c(3,5,7,9)){data[,i] <- rbeta(N,2,5)}  
for(i in 10:13){data[,i] <- rnorm(N,0,1)} # the real data here should be generated as described in text

# Generate LabiiT following equation 1
data$labiiT_ingroup_pic <- data$ingroup_pic_labii-data$isi_1_labii
data$labiiT_outgroup_pic <- data$outgroup_pic_labii-data$isi_2_labii
data$labiiT_ingroup_mv <- data$ingroup_mv_labii-data$isi_3_labii
data$labiiT_outgroup_mv <- data$outgroup_mv_labii-data$isi_4_labii

# Generate Disgust DiD following equation 4
data$labiiT_ingroup_did <- data$labiiT_ingroup_mv - data$labiiT_ingroup_pic
data$labiiT_outgroup_did <- data$labiiT_outgroup_mv - data$labiiT_outgroup_pic

data$disgustT_ingroup_did <- data$ingroup_mv_sr_disgust - data$ingroup_pic_sr_disgust
data$disgustT_outgroup_did <- data$outgroup_mv_sr_disgust - data$outgroup_pic_sr_disgust

# Generate Disgust DiD image following equation 6
data$labiiT_out_vs_in <- data$labiiT_outgroup_pic - data$labiiT_ingroup_pic
data$sr_disgustT_out_vs_in <- data$outgroup_pic_sr_disgust - data$ingroup_pic_sr_disgust

# Covariates
data$partisanship.strength <- rnorm(N, 0, 1)
data$moral.disgust <- rnorm(N,0, 1)
data$pathogen.avoid <- rnorm(N,0, 1)
data$sexual.disgust <- rnorm(N,0, 1)

# control vars
data$treatment.order1 <- sample(seq(0,1,1),N, replace=TRUE) # 0 = out-group first, 1 = in-group first
data$treatment.order2 <- sample(seq(0,1,1),N, replace=TRUE) # 0 = moral violation first, 1 = visual disgust first
data$exp.events <- sample(seq(0,1,1),N, replace=TRUE) # 0 = nothing happened in experiment, 1 = something happened in experiment
data$compensation <- sample(seq(0,1,1),N, replace=TRUE) # 0 = research credit, 1 = financial compensation
data$age <- rnorm(N, 35, 10)
data$gender <- sample(c(0,1),N,replace=TRUE)
data$education <- factor(sample(seq(1,6,1),N,replace=TRUE))
data$student <- sample(c(0,1),N,replace=TRUE)
data$temperature <- rnorm(N, 20,1)
data$polknow <- sample(seq(1,4,1),N,replace=TRUE)

# restyle data frame to generate DVs
data.reshaped <- reshape(data, varying=c(colnames(data)[10:11],
                                            colnames(data)[14:15],
                                            colnames(data)[18:19],
                                            colnames(data)[20:21]), 
                            drop=colnames(data)[c(2:9,12:13,16:17,22:23)],
                            times=c("ingroup","outgroup"),
                            v.names=c("sr_disgust_T", "labii_T", "labii_did", "sr_disgust_did"), direction="long", new.row.names=seq(1,200,1))
colnames(data.reshaped)[15] <- "treatment"

# T-tests  ---------------------------------------------------
t <- list()

# labii
t.test(x=data$labiiT_ingroup_pic,y=NULL)
t.test(x=data$labiiT_outgroup_pic,y=NULL)
t.test(x=data$labiiT_ingroup_mv,y=NULL)
t.test(x=data$labiiT_outgroup_mv,y=NULL)

# self report
t.test(x=data$ingroup_pic_sr_disgust,y=NULL)
t.test(x=data$outgroup_pic_sr_disgust,y=NULL)
t.test(x=data$ingroup_mv_sr_disgust,y=NULL)
t.test(x=data$outgroup_mv_sr_disgust,y=NULL)

# Main analyses -----------------------------------------------------------

# H1 as specified by equation 3
h1.phys <- lm(labii_T ~ treatment + treatment.order1 + treatment.order2 + age + gender + education + student + temperature + polknow,
            data=data.reshaped)
h1.feel <- lm(sr_disgust_T ~ treatment + treatment.order1 + treatment.order2  + age + gender + education + student + temperature + polknow,
               data=data.reshaped)

# H2 as specified by equation 5
h2.phys <- lm(labii_did  ~ treatment + treatment.order1 + treatment.order2  + age + gender + education + student + temperature + polknow,
               data=data.reshaped)
h2.feel <- lm(sr_disgust_did ~ treatment + treatment.order1 + treatment.order2 + age + gender + education + student + temperature + polknow,
               data=data.reshaped)

# H3, equation 7
h3.phys <- lm(labiiT_out_vs_in  ~  partisanship.strength +  treatment.order1 + treatment.order2 + age + gender + education + student + temperature + polknow, data=data)
h3.feel <- lm(sr_disgustT_out_vs_in  ~  partisanship.strength +  treatment.order1 + treatment.order2 + age + gender + education + student + temperature + polknow, data=data)

# H4, equation 8
h4.phys <- lm(labii_did  ~ treatment + moral.disgust +  treatment.order1 + treatment.order2 + age + gender + education + student + temperature + polknow,
               data=data.reshaped)
h4.feel <- lm(sr_disgust_did ~ treatment + moral.disgust +  treatment.order1 + treatment.order2+ age + gender + education + student + temperature + polknow,
               data=data.reshaped)


# Robustness checks -----------------------------------------------------
cor(data$did.did.labii[data$comparison.group2==1],data$iat.response[data$comparison.group2==1])  # in group vs out-group

# Still to add
## 2 seconden physiologische activiteit image, alle regressies 
## Na 2 seconden physiologische activiteit, alle regressies

## Manipulation check with images

## correlation arousal, corrugator & labii

## correlation self-reported emotions

## descriptives + cfa(emotions) + cfa(partisanship?) + cfa(disgust sensitivity) en alpha's

