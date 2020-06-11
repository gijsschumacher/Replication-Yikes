# Fully stacked analysis --------------------------------------------------

dv <- c(full.data$labiiT_ingroup_pic_win,  full.data$labiiT_outgroup_pic_win,
        full.data$labiiT_ingroup_mv_win,  full.data$labiiT_outgroup_mv_win)
treatment <- factor(rep(seq(0:3),each=length(full.data$respondent)))

respondent <- rep(full.data$respondent,4)
stacked.data <- data.frame(dv,treatment,respondent)
colnames <- c("treatment.order1", "moral", "partisanship.strength", "age","female","Education","student","Temperature","know","amsterdam","reward","Comment_coded")
temp <- full.data[,which(colnames(full.data)%in%colnames)]
temp <- rbind(temp,temp,temp,temp)
stacked.data <- data.frame(stacked.data,temp)
stacked.data$t1 <- ifelse(stacked.data$treatment==1,1,0)
stacked.data$t2 <- ifelse(stacked.data$treatment==2,1,0)
stacked.data$t3 <- ifelse(stacked.data$treatment==3,1,0)
stacked.data$t4 <- ifelse(stacked.data$treatment==4,1,0)

stacked.analysis1 <- lm(dv ~ t1 + t2 + t4 + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=stacked.data)
coeftest(stacked.analysis1, cluster.vcov(stacked.analysis1, stacked.data$respondent))


# Concordance -------------------------------------------------------------

concord1 <- lm(ingroup_sr_disgust ~ labiiT_ingroup_pic_win + partisanship.strength + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)
concord2 <- lm(outgroup_sr_disgust ~ labiiT_outgroup_pic_win + partisanship.strength + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)
concord3 <- lm(ingroup_mv_sr_disgust ~ labiiT_ingroup_mv_win + corrT_ingroup_mv_win + partisanship.strength + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)
concord3a <- lm(ingroup_mv_sr_disgust ~ labiiT_ingroup_mv_win*partisanship.strength + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)
concord4 <- lm(outgroup_mv_sr_disgust ~ labiiT_outgroup_mv_win  + corrT_ingroup_mv_win + partisanship.strength + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)
concord5 <- lm(disgustT_ingroup_did ~  labiiT_ingroup_did_win + partisanship.strength + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)
concord6 <- lm(disgustT_outgroup_did ~ labiiT_outgroup_did_win + partisanship.strength + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)

temp <- lm(sr_disgust_did ~ labii_did_win*treatment +  corr_did_win +  treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=data.reshaped)
interpol(temp, var1="labii_did_win", var2="treatment")

# Concordance: ingroup and outgroup -------------------------------------------------------------
try<- lm(disgustT_ingroup_did ~ labiiT_ingroup_did + partisanship.strength + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)
try<- lm(disgustT_ingroup_did ~ labiiT_ingroup_did*partisanship.strength + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)

interplot(m=try, var1="labiiT_ingroup_did", var2="partisanship.strength")
interplot(m=try, var1="partisanship.strength", var2="labiiT_ingroup_did")

try<- lm(disgustT_ingroup_did ~ labiiT_ingroup_did_win + partisanship.strength + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)
try<- lm(disgustT_ingroup_did ~ labiiT_ingroup_did_win*partisanship.strength + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)

interplot(m=try, var1="labiiT_ingroup_did_win", var2="partisanship.strength")
interplot(m=try, var1="partisanship.strength", var2="labiiT_ingroup_did_win")

#outgroup
try<- lm(disgustT_outgroup_did ~ labiiT_outgroup_did + moral + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)
try<- lm(disgustT_outgroup_did ~ labiiT_outgroup_did*moral + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)

try<- lm(disgustT_outgroup_did ~ labiiT_outgroup_did_win + moral + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)
try<- lm(disgustT_outgroup_did ~ labiiT_outgroup_did_win*moral + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)

interplot(m=try, var1="labiiT_outgroup_did_win", var2="moral")
interplot(m=try, var1="moral", var2="labiiT_outgroup_did_win")



summary(try)

try<- lm(disgustT_outgroup_did ~ labiiT_outgroup_did + partisanship.strength + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)
try<- lm(disgustT_outgroup_did ~ labiiT_outgroup_did*partisanship.strength + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)


summary(try)

try<- lm(sr_disgustT_out_vs_in ~ labiiT_out_vs_in*partisanship.strength + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)



try<- lm(sr_disgustT_out_vs_in ~ labiiT_out_vs_in_win + partisanship.strength + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)

try<- lm(sr_disgustT_out_vs_in ~ labiiT_out_vs_in_win*partisanship.strength + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)


try<- lm(sr_disgustT_out_vs_in ~ labiiT_out_vs_in + scale(moral) + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)

try<- lm(sr_disgustT_out_vs_in ~ labiiT_out_vs_in_win + scale(moral) + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)
summary(try)

try<- lm(sr_disgustT_out_vs_in ~ labiiT_out_vs_in_win*moral + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=full.data)

library(interplot)
interplot(m=try, var1="labiiT_out_vs_in_win", var2="moral")
interplot(m=try, var1="moral", var2="labiiT_out_vs_in_win", hist=TRUE)


summary(try)




h3.vars <- expand.grid(c("labiiT_out_vs_in", "labiiT_out_vs_in_win", "sr_disgustT_out_vs_in", "sclT_out_vs_in", "corrT_out_vs_in", "corrT_out_vs_in_win", "sr_angerT_out_vs_in",  "sr_anxietyT_out_vs_in",  "sr_enthusiasmT_out_vs_in"), "partisanship.strength", stringsAsFactors = FALSE)



# 1 congruent (no response), 2 = congruent (response), 3 = down regulators, 4 = partisan parrot
full.data$concordance.in <- ifelse(full.data$labiiT_ingroup_did_win<=0 & full.data$disgustT_ingroup_did<=median(full.data$disgustT_ingroup_did),"Congruent\n(no response)",
                                   ifelse(full.data$labiiT_ingroup_did_win>0 & full.data$disgustT_ingroup_did>median(full.data$disgustT_ingroup_did),"Congruent\n(response)",
                                          ifelse(full.data$labiiT_ingroup_did_win>0 & full.data$disgustT_ingroup_did<=median(full.data$disgustT_ingroup_did),"Downregulator","Partisan\nparrot")))       


library(plyr)

data.con.in <- ddply(full.data, .(concordance.in), summarise,
                     means.p = mean(partisanship.strength),
                     ses.p = sd(moral) / sqrt(length(partisanship.strength)),
                     lo.p = means.p - 1.96*ses.p,
                     hi.p = means.p + 1.96*ses.p,
                     means.m = mean(moral),
                     ses.m = sd(moral) / sqrt(length(moral)),
                     lo.m = means.m - 1.96*ses.m,
                     hi.m = means.m + 1.96*ses.m)
colnames(data.con.in)[1] <- "category"
data.con.in$panel <- "Concordance\ningroup"

full.data$concordance.out <- ifelse(full.data$labiiT_outgroup_did_win<=0 & full.data$disgustT_outgroup_did<=median(full.data$disgustT_outgroup_did),"Congruent\n(no response)",
                                    ifelse(full.data$labiiT_outgroup_did_win>0 & full.data$disgustT_outgroup_did>median(full.data$disgustT_outgroup_did),"Congruent\n(response)",
                                           ifelse(full.data$labiiT_outgroup_did_win>0 & full.data$disgustT_outgroup_did<=median(full.data$disgustT_outgroup_did),"Downregulator","Partisan\nparrot")))                                                                          


data.con.out <- ddply(full.data, .(concordance.out), summarise,
                      means.p = mean(partisanship.strength),
                      ses.p = sd(moral) / sqrt(length(partisanship.strength)),
                      lo.p = means.p - 1.96*ses.p,
                      hi.p = means.p + 1.96*ses.p,
                      means.m = mean(moral),
                      ses.m = sd(moral) / sqrt(length(moral)),
                      lo.m = means.m - 1.96*ses.m,
                      hi.m = means.m + 1.96*ses.m)
colnames(data.con.out)[1] <- "category"
data.con.out$panel <- "Concordance\noutgroup"


full.data$hypocritical.labii <- ifelse(full.data$labiiT_outgroup_did_win > full.data$labiiT_ingroup_did_win,"Hypocritical",
                                       ifelse(full.data$labiiT_outgroup_did_win < full.data$labiiT_ingroup_did_win,"Hypercritical","Neutral"))

full.data$hypocritical.sr <- ifelse(full.data$disgustT_outgroup_did > full.data$disgustT_ingroup_did,"Hypocritical",
                                    ifelse(full.data$disgustT_outgroup_did < full.data$disgustT_ingroup_did,"Hypercritical","Neutral"))

data.hypocrisy.labii <- ddply(full.data, .(hypocritical.labii), summarise,
                              means.p = mean(partisanship.strength),
                              ses.p = sd(moral) / sqrt(length(partisanship.strength)),
                              lo.p = means.p - 1.96*ses.p,
                              hi.p = means.p + 1.96*ses.p,
                              means.m = mean(moral),
                              ses.m = sd(moral) / sqrt(length(moral)),
                              lo.m = means.m - 1.96*ses.m,
                              hi.m = means.m + 1.96*ses.m)
colnames(data.hypocrisy.labii)[1] <- "category"
data.hypocrisy.labii$panel <- "Dif Labii\nIn-Out"

data.hypocrisy.sr <- ddply(full.data, .(hypocritical.sr), summarise,
                           means.p = mean(partisanship.strength),
                           ses.p = sd(moral) / sqrt(length(partisanship.strength)),
                           lo.p = means.p - 1.96*ses.p,
                           hi.p = means.p + 1.96*ses.p,
                           means.m = mean(moral),
                           ses.m = sd(moral) / sqrt(length(moral)),
                           lo.m = means.m - 1.96*ses.m,
                           hi.m = means.m + 1.96*ses.m)
colnames(data.hypocrisy.sr)[1] <- "category"
data.hypocrisy.sr$panel <- "Dif self-report\nIn-Out"

data <- rbind(data.con.in, data.con.out, data.hypocrisy.labii, data.hypocrisy.sr)

partisanship <- ggplot(data, aes(x=category, y=means.p)) +
  geom_point() +
  geom_errorbar(aes(ymin=lo.p,ymax=hi.p), data=data) +
  facet_wrap(.~panel, nrow=2, scales="free_x") + xlab("") +  ylab("Means Partisanship")

moral <- ggplot(data, aes(x=category, y=means.m)) +
  geom_point() +
  geom_errorbar(aes(ymin=lo.m,ymax=hi.m), data=data) +
  facet_wrap(.~panel, nrow=2, scales="free_x") + xlab("") + ylab("Means Moral")


# # Reanalysis Review -----------------------------------------------------
load("full.data.Rdata")

# SCL winsorized analysis
temp <- apply(full.data[,which(colnames(full.data)=="scl_basket"):which(colnames(full.data)=="scl_vomit")],2,winsorize,95)
colnames(temp) <- paste0(colnames(temp), "_win")
full.data <- data.frame(full.data,temp)

scl.check <- stack(full.data[,which(colnames(full.data)=="scl_basket_win"): which(colnames(full.data)=="scl_vomit_win")])
colnames(scl.check) <- c("scl_win", "picture")
# First run lines 8-18 of analysis.R
disgust.check <- data.frame(disgust.check,scl.check)

mp.check4.win <- lm(scl_win~ picture + treatment.order + amsterdam, data=disgust.check)
mp.check4.win.robust <- coeftest(mp.check4.win, cluster.vcov(mp.check4.win,disgust.check$respondent))

# more/less labii activity in amsterdam/nijmegen
disgust.check$temperatuur <- rep(full.data$Temperature, 7)
disgust.check$reward <- rep(full.data$reward,7) 

amsterdam <- lm(labiiT ~ factor(amsterdam), data=disgust.check)
coeftest(amsterdam, cluster.vcov(amsterdam, disgust.check$respondent))
amsterdam_win <- lm(labiiT_win ~ factor(amsterdam), data=disgust.check)
coeftest(amsterdam_win, cluster.vcov(amsterdam_win, disgust.check$respondent))

reward <- lm(labiiT ~ reward, data=disgust.check)
coeftest(reward, cluster.vcov(reward, disgust.check$respondent))
reward_win <- lm(labiiT_win ~ reward, data=disgust.check)
coeftest(reward_win, cluster.vcov(reward_win, disgust.check$respondent))

reward_win_moneyref <- lm(labiiT_win ~ C(reward,contr.treatment(3, base=2)), data=disgust.check) # now with money as reference
coeftest(reward_win_moneyref, cluster.vcov(reward_win_moneyref, disgust.check$respondent))
reward_moneyref <- lm(labiiT ~ C(reward,contr.treatment(3, base=2)), data=disgust.check) # now with money as reference
coeftest(reward_moneyref, cluster.vcov(reward_moneyref, disgust.check$respondent))

temperatuur <- lm(labiiT ~ temperatuur, data=disgust.check)
coeftest(temperatuur, cluster.vcov(temperatuur, disgust.check$respondent))
temperatuur_win <- lm(labiiT_win ~ temperatuur, data=disgust.check)
coeftest(temperatuur_win, cluster.vcov(temperatuur, disgust.check$respondent))

# fewer covariates in main regression 
summary(lm(labii_T_win ~ treatment + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=data.reshaped))
summary(lm(labii_T_win ~ treatment + treatment.order1  + age + female + Education + student + know + reward +  Comment_coded, data=data.reshaped))

summary(lm(labii_did_win ~ treatment + treatment.order1  + age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, data=data.reshaped))
summary(lm(labii_did_win ~ treatment + treatment.order1  + age + female + Education + student + know + reward +  Comment_coded, data=data.reshaped))

