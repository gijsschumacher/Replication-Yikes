#root <- str_split_fixed(getwd(), "/Papers",2)[,1]
#overleaf <- ifelse(str_split_fixed(root, "/",4)[,3]=="gschuma1","C:/Users/gschuma1/Dropbox/Apps/Overleaf/PLS registered report",
#                   "C:/Users/bbakker1/Dropbox/Apps/ShareLaTeX/PLS registered report")

overleaf <- root

# Manipulation check ------------------------------------------------------
disgust.check <- stack(full.data[,which(colnames(full.data)=="labiiT_basket"): which(colnames(full.data)=="labiiT_vomit")])
colnames(disgust.check) <- c("labiiT", "picture")
disgust.check$picture <- str_split_fixed(disgust.check$picture, "[_]",2)[,2]
disgust.check$respondent <- rep(full.data$respondent, 7)
treatment.order <- c(full.data$pic.order.A, full.data$pic.order.B,full.data$pic.order.C,full.data$pic.order.D,full.data$pic.order.E,full.data$pic.order.F)
disgust.check$treatment.order <- c(rep(1,dim(full.data)[1]), treatment.order)
disgust.check$amsterdam <- rep(full.data$amsterdam, 7)
disgust.check$labiiT_win <- stack(full.data[,which(colnames(full.data)=="labiiT_basket_win"): which(colnames(full.data)=="labiiT_vomit_win")])[,1]

mp.check1 <- lm(labiiT ~ picture + treatment.order + amsterdam, data=disgust.check)
mp.check1.robust <- coeftest(mp.check1, cluster.vcov(mp.check1,disgust.check$respondent))

mp.check1.win <- lm(scale(labiiT_win) ~ picture + treatment.order + amsterdam, data=disgust.check)
mp.check1.win.robust <- coeftest(mp.check1.win, cluster.vcov(mp.check1.win,disgust.check$respondent))

selfreport <- c(full.data$disgust.pic.Start_basket, full.data$disgust.pic.A,full.data$disgust.pic.B,full.data$disgust.pic.C,full.data$disgust.pic.D,full.data$disgust.pic.E,full.data$disgust.pic.F)
disgust.check <- data.frame(disgust.check,selfreport)

mp.check2 <- lm(selfreport ~ picture + treatment.order + amsterdam, data=disgust.check)
mp.check2.robust <- coeftest(mp.check2, cluster.vcov(mp.check2,disgust.check$respondent))

cor.check <- stack(full.data[,which(colnames(full.data)=="corT_basket"): which(colnames(full.data)=="corT_vomit")])
colnames(cor.check) <- c("corrugator", "picture")
cor.check <- data.frame(cor.check,disgust.check)
cor.check$corrugator_win <- stack(full.data[,which(colnames(full.data)=="corT_basket_win"): which(colnames(full.data)=="corT_vomit_win")])[,1]
cor.check$picture<-factor(cor.check$picture, levels = c("corT_basket", "corT_beker", "corT_lamp", "corT_lepel",  "corT_poop", "corT_vomit", "corT_worms")) #this is necessary because otherwise the results are in different order then other 3 models
mp.check3 <- lm(corrugator ~ picture + treatment.order + amsterdam, data=cor.check)
mp.check3.robust <- coeftest(mp.check3, cluster.vcov(mp.check3,disgust.check$respondent))

mp.check3.win <- lm(corrugator_win ~ picture + treatment.order + amsterdam, data=cor.check)
mp.check3.win.robust <- coeftest(mp.check3.win, cluster.vcov(mp.check3.win,cor.check$respondent))

scl.check <- stack(full.data[,which(colnames(full.data)=="scl_basket"): which(colnames(full.data)=="scl_vomit")])
colnames(scl.check) <- c("scl", "picture")
disgust.check <- data.frame(disgust.check,scl.check)

mp.check4 <- lm(scl ~ picture + treatment.order + amsterdam, data=disgust.check)
mp.check4.robust <- coeftest(mp.check4, cluster.vcov(mp.check4,disgust.check$respondent))

#Make plot for manipulation check

mp_graph.1<-as.data.frame(mp.check1.robust[2:7, 1:2])
mp_graph.1win <-as.data.frame(mp.check1.win.robust[2:7, 1:2])
mp_graph.2<-as.data.frame(mp.check2.robust[2:7, 1:2])
mp_graph.3<-as.data.frame(mp.check3.robust[2:7, 1:2])
mp_graph.3win <-as.data.frame(mp.check3.win.robust[2:7, 1:2]) 
mp_graph.4<-as.data.frame(mp.check4.robust[2:7, 1:2])

mp_graph_comb<-rbind(mp_graph.1,mp_graph.1win, mp_graph.2, mp_graph.3, mp_graph.3win, mp_graph.4)
colnames(mp_graph_comb) <-c("beta", "se")
mp_graph_comb$stimuli<-rep(c("Neutral: Cup", "Neutral: Lamp", "Neutral: Spoon", "Disgust: Poop", "Disgust: Vomit", "Disgust: Worms"),6)
mp_graph_comb$dv<-c(rep("Labii", 12), rep("Self-report", 6), rep("Corrugator", 12), rep("Skin\n Conductance", 6))
mp_graph_comb$preg<-c(rep("preregistered", 6), rep("exploratory", 6), rep("preregistered", 12), rep("exploratory",6),rep("preregistered",6))

mp_graph_comb$stimuli <- factor(mp_graph_comb$stimuli,levels = c("Neutral: Cup", "Neutral: Lamp", "Neutral: Spoon", "Disgust: Poop", "Disgust: Vomit", "Disgust: Worms"))
mp_graph_comb$dv <- factor(mp_graph_comb$dv,levels = c("Labii", "Self-report", "Corrugator", "Skin\n Conductance"))

#Confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

mp_graph_comb$grouping_colour <-ifelse(mp_graph_comb$pre=="preregistered", 1,0)
mp_graph_comb$grouping_colour<-as.factor(mp_graph_comb$grouping_colour)

fig_manipulation<-ggplot(mp_graph_comb, aes(x = stimuli)) + facet_grid(vars(stimuli), vars(dv), scales = "free", space="fixed") + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + theme_bw() + coord_flip() + geom_pointrange(aes(x = stimuli, colour=grouping_colour, y = beta, ymin = beta - se*interval2, ymax = beta + se*interval2), lwd = 1/2, position = position_dodge(width = 1/2), shape = 21) + ylab("Unstandardized OLS regression coefficients") + xlab("")  + geom_linerange(aes(x = stimuli, colour=grouping_colour, ymin = beta - se*interval1,  ymax = beta + se*interval1), lwd = 1, position = position_dodge(width = 1/2)) + scale_color_manual(values=c('Grey','Black'), guide = FALSE) + theme(axis.text.x = element_text(angle = 90, hjust = 1))  +  theme(strip.text.y = element_text(angle = 0), axis.text.y=element_blank(),axis.ticks.y=element_blank()) 
ggsave(fig_manipulation, file= paste0(overleaf,"/figures/manipulation_results.pdf"), dpi=900, width=6, height=5)

# Restyle data frame to generate DVs-----------------------
sr_disgust_T <- c(full.data$ingroup_sr_disgust,full.data$outgroup_sr_disgust)
sr_anger_T <- c(full.data$ingroup_sr_anger, full.data$outgroup_sr_anger)
sr_anxiety_T<-c(full.data$ingroup_sr_anxiety, full.data$outgroup_sr_anxiety)
sr_enthusiasm_T<-c(full.data$ingroup_sr_enthusiasm, full.data$outgroup_sr_enthusiasm)

labii_T <- c(full.data$labiiT_ingroup_pic,  full.data$labiiT_outgroup_pic)
labii_did <- c(full.data$labiiT_ingroup_did,  full.data$labiiT_outgroup_did)
labii_T_win <- c(full.data$labiiT_ingroup_pic_win,  full.data$labiiT_outgroup_pic_win)
labii_did_win <- c(full.data$labiiT_ingroup_did_win,  full.data$labiiT_outgroup_did_win)

sr_disgust_did <- c(full.data$disgustT_ingroup_did,  full.data$disgustT_outgroup_did)
sr_anger_did<-c(full.data$angerT_ingroup_did,  full.data$angerT_outgroup_did)
sr_anxiety_did<-c(full.data$anxietyT_ingroup_did,  full.data$anxietyT_outgroup_did)
sr_enthusiasm_did<-c(full.data$enthusiasmT_ingroup_did,  full.data$enthusiasmT_outgroup_did)

corr_T <- c(full.data$corrT_ingroup_pic,  full.data$corrT_outgroup_pic)
corr_did <- c(full.data$corrT_ingroup_did,  full.data$corrT_outgroup_did)
corr_T_win <- c(full.data$corrT_ingroup_pic_win,  full.data$corrT_outgroup_pic_win)
corr_did_win <- c(full.data$corrT_ingroup_did_win,  full.data$corrT_outgroup_did_win)

scl_T <- c(full.data$sclT_ingroup_pic,  full.data$sclT_outgroup_pic)
scl_did <- c(full.data$sclT_ingroup_did,  full.data$sclT_outgroup_did)

data.reshaped <- data.frame(sr_disgust_T, sr_anger_T, sr_anxiety_T, sr_enthusiasm_T, labii_T, labii_did, labii_T_win,labii_did_win, sr_disgust_did, sr_anger_did, sr_anxiety_did, sr_enthusiasm_did, corr_T, corr_did, corr_T_win, corr_did_win, scl_T, scl_did)
data.reshaped$treatment <- c(rep(0,length(full.data$ingroup_sr_disgust)),rep(1,length(full.data$ingroup_sr_disgust)))
data.reshaped$respondent <- c(full.data$respondent,full.data$respondent)
data.reshaped <- merge(data.reshaped, full.data, by="respondent")

# T-tests against 0  ---------------------------------------------------

#Get results in a Table
options <- expand.grid(c("labiiT_ingroup_pic", "labiiT_outgroup_pic", "labiiT_ingroup_mv", "labiiT_outgroup_mv", "labiiT_ingroup_pic_win", "labiiT_outgroup_pic_win", "labiiT_ingroup_mv_win", "labiiT_outgroup_mv_win", "ingroup_sr_disgust", "outgroup_sr_disgust", "ingroup_mv_sr_disgust", "outgroup_mv_sr_disgust"))

output <- matrix(NA, dim(options)[1],6)

for(i in 1:dim(options)[1]){
  var1 <- which(colnames(full.data)==options[i,1])
  test <- t.test(full.data[,var1], y=NULL)
  output[i,1] <- test$statistic
  output[i,2] <- test$parameter
  output[i,3] <- test$conf.int[1]
  output[i,4] <- test$conf.int[2]
  output[i,5] <- test$p.value
  output[i,6] <- test$estimate[1]
}

#colnames(output) <- c("t", "df", "lower CI (2.5)", "upper CI (97.5)", "p", "Mean")

output <- data.frame(options,output)
#output<- output[,c(1,2,3,6,7,5, 6)]

colnames(output) <- c("Condition", "t-value", "df", "lower CI (2.5)", "upper CI (97.5)","p-value", "Mean")
output$Preregistered <- c(rep("Pre-registered", 4), rep("Exploratory", 4), rep("Pre-registered", 4))
output$Measure <-c(rep("Labii",8), rep("Self-report", 4))
output<- output[,c(8,1,9,2,3,7,4,5,6)]

output$Condition<-rep(c("Ingroup", "Outgroup", "Ingroup + MV", "Outgroup + MV"), 3)
print(xtable(output,  type = "latex", digits=c(NA,NA,NA,NA,3,0,3,3,3,3), caption="T-test of differences in disgust (Labii and self-report) compared to 0 (pre-registered and exploratory)", label="tab:prereg_ttest"), caption.placement = 'top', file = paste0(overleaf,"/tables/Ttest_zero.tex"), size="\\fontsize{8pt}{10pt}\\selectfont", include.rownames=FALSE)

# T-tests of group differences-------------------------
options <- expand.grid(c("labiiT_outgroup_pic", "labiiT_ingroup_mv"), c("labiiT_ingroup_pic"))
output <- matrix(NA, dim(options)[1],6)

for(i in 1:dim(options)[1]){
  var1 <- which(colnames(full.data)==options[i,1])
  var2 <- which(colnames(full.data)==options[i,2])
  test <- t.test(full.data[,var1], full.data[,var2], paired=TRUE)
  output[i,1] <- test$statistic
  output[i,2] <- test$parameter
  output[i,3] <- test$conf.int[1]
  output[i,4] <- test$conf.int[2]
  output[i,5] <- test$p.value
  output[i,6] <- test$estimate
}
output1 <- data.frame(options,output)

options <- expand.grid(c("labiiT_outgroup_pic", "labiiT_ingroup_mv"), c("labiiT_outgroup_mv"))
output <- matrix(NA, dim(options)[1],6)

for(i in 1:dim(options)[1]){
  var1 <- which(colnames(full.data)==options[i,1])
  var2 <- which(colnames(full.data)==options[i,2])
  test <- t.test(full.data[,var1], full.data[,var2], paired=TRUE)
  output[i,1] <- test$statistic
  output[i,2] <- test$parameter
  output[i,3] <- test$conf.int[1]
  output[i,4] <- test$conf.int[2]
  output[i,5] <- test$p.value
  output[i,6] <- test$estimate
}
output2 <- data.frame(options,output)

options <- expand.grid(c("labiiT_outgroup_pic_win", "labiiT_ingroup_mv_win"), c("labiiT_ingroup_pic_win"))
output <- matrix(NA, dim(options)[1],6)

for(i in 1:dim(options)[1]){
  var1 <- which(colnames(full.data)==options[i,1])
  var2 <- which(colnames(full.data)==options[i,2])
  test <- t.test(full.data[,var1], full.data[,var2], paired=TRUE)
  output[i,1] <- test$statistic
  output[i,2] <- test$parameter
  output[i,3] <- test$conf.int[1]
  output[i,4] <- test$conf.int[2]
  output[i,5] <- test$p.value
  output[i,6] <- test$estimate
}
output3 <- data.frame(options,output)

options <- expand.grid(c("labiiT_outgroup_pic_win", "labiiT_ingroup_mv_win"), c("labiiT_outgroup_mv_win"))
output <- matrix(NA, dim(options)[1],6)

for(i in 1:dim(options)[1]){
  var1 <- which(colnames(full.data)==options[i,1])
  var2 <- which(colnames(full.data)==options[i,2])
  test <- t.test(full.data[,var1], full.data[,var2], paired=TRUE)
  output[i,1] <- test$statistic
  output[i,2] <- test$parameter
  output[i,3] <- test$conf.int[1]
  output[i,4] <- test$conf.int[2]
  output[i,5] <- test$p.value
  output[i,6] <- test$estimate
}
output4 <- data.frame(options,output)

options <- expand.grid(c("outgroup_sr_disgust", "ingroup_mv_sr_disgust"), c("ingroup_sr_disgust"))
output <- matrix(NA, dim(options)[1],6)

for(i in 1:dim(options)[1]){
  var1 <- which(colnames(full.data)==options[i,1])
  var2 <- which(colnames(full.data)==options[i,2])
  test <- t.test(full.data[,var1], full.data[,var2], paired=TRUE)
  output[i,1] <- test$statistic
  output[i,2] <- test$parameter
  output[i,3] <- test$conf.int[1]
  output[i,4] <- test$conf.int[2]
  output[i,5] <- test$p.value
  output[i,6] <- test$estimate
}
output5 <- data.frame(options,output)

options <- expand.grid(c("outgroup_sr_disgust", "ingroup_mv_sr_disgust"), c("outgroup_mv_sr_disgust"))
output <- matrix(NA, dim(options)[1],6)

for(i in 1:dim(options)[1]){
  var1 <- which(colnames(full.data)==options[i,1])
  var2 <- which(colnames(full.data)==options[i,2])
  test <- t.test(full.data[,var1], full.data[,var2], paired=TRUE)
  output[i,1] <- test$statistic
  output[i,2] <- test$parameter
  output[i,3] <- test$conf.int[1]
  output[i,4] <- test$conf.int[2]
  output[i,5] <- test$p.value
  output[i,6] <- test$estimate
}
output6 <- data.frame(options,output)

output_total <- rbind(output1, output2, output3, output4, output5, output6)

colnames(output_total) <- c("Condition 1", "Condition 2", "t", "df", "2.5 CI", "97.5 CI", "p", "M diff")
output_total<- output_total[,c(1,2,8,3,4,7,5,6)]

output_total$Preregistered <- c(rep("Pre-registered", 4), rep("Exploratory", 4), rep("Pre-registered", 4))
output_total$Measure <-c(rep("Labii",8), rep("Self-report", 4))
output_total$"Condition 1"<-rep(c("Outgroup", "Ingroup + MV", "Outgroup", "Ingroup + MV"), 3)
output_total$"Condition 2"<-rep(c("Ingroup", "Ingroup", "Outgroup + MV", "Outgroup + MV"), 3)
output_total<- output_total[,c(9,10, 1,2,3,4,5,6,7,8)]

print(xtable(output_total,  digits=c(NA, NA,NA,NA,NA,3,3,0,3,3,3), type = "latex", caption="T-test of differences in disgust (Labii and self-report) between conditions (pre-registered and exploratory)", label="tab:prereg_ttest_conditions"), caption.placement = 'top', file = paste0(overleaf,"/Tables/Ttest_conditions.tex"), size="\\fontsize{8pt}{10pt}\\selectfont", include.rownames=FALSE)

# Main analyses & Robustness checks -----------------------------------------------------------
h1.vars <- expand.grid(c("labii_T", "labii_T_win", "sr_disgust_T",  "scl_T", "corr_T", "corr_T_win", "sr_anger_T", "sr_anxiety_T", "sr_enthusiasm_T"), "treatment", stringsAsFactors = FALSE)
h2.vars <- expand.grid(c("labii_did", "labii_did_win", "sr_disgust_did", "scl_did", "corr_did", "corr_did_win", "sr_anger_did", "sr_anxiety_did", "sr_enthusiasm_did"), "treatment", stringsAsFactors = FALSE)
h3.vars <- expand.grid(c("labiiT_out_vs_in", "labiiT_out_vs_in_win", "sr_disgustT_out_vs_in", "sclT_out_vs_in", "corrT_out_vs_in", "corrT_out_vs_in_win", "sr_angerT_out_vs_in",  "sr_anxietyT_out_vs_in",  "sr_enthusiasmT_out_vs_in"), "partisanship.strength", stringsAsFactors = FALSE)
h4.vars <- expand.grid(c("labii_did", "labii_did_win", "sr_disgust_did", "scl_did", "corr_did", "corr_did_win",  "sr_anger_did", "sr_anxiety_did", "sr_enthusiasm_did"), c("moral", "pathogen", "sexual"), stringsAsFactors = FALSE) 
options <- rbind(h1.vars,h2.vars,h3.vars,h4.vars)
options$hypothesis <- c(rep(c("H1: Visuals","H2: Moral violation","H3: Visuals"), each=9), rep("H4: Moral violation",27))
options$main <- c(rep(c(rep("Main",3),rep("Robust",6)),4),rep("Robust",18))

regression.output <- lapply(seq(1,dim(options)[1]),do.regression,options)
regression.results <- sapply(regression.output, extract.results)
regression.results <- data.frame(options, t(regression.results))
colnames(regression.results)[5:11] <- c("B", "SE", "P", "lo95","hi95", "lo90", "hi90")

regression.results$IV<- c(rep("Outparty vs\nInparty",9), rep("Outparty vs\nInparty",9), rep("Partisan\n Identity\n Strength",9), rep("Moral\n disgust",9), rep("Pathogen\n disgust", 9), rep("Sexual\n disgust", 9))
regression.results$measure<-rep(c("Labii", "Labii", "Self-reported\nDisgust", "SCL", "Corrugator", "Corrugator", "Self-reported\nAnger", "Self-reported\nAnxiety", "Self-reported\nEnthusiasm"),6)
regression.results$DV<-rep(c("Preregistered", "Exploratory", "Preregistered","Preregistered","Preregistered",  "Exploratory", "Preregistered", "Preregistered", "Preregistered"),6)

regression.results$grouping_colour <-ifelse(regression.results$DV=="Preregistered", 1,0)
regression.results$grouping_colour<-as.factor(regression.results$grouping_colour)
regression.results$hypothesis<-as.factor(regression.results$hypothesis)
regression.results$measure<-as.factor(regression.results$measure)

# Subset for preregistered tests of hypotheses
regression.results.main <- regression.results[regression.results$main=="Main",]

fig_h1_4<- ggplot(regression.results.main, aes(x = IV)) + 
            facet_grid(vars(hypothesis), vars(measure), 
            scales = "free", space="fixed") + 
            geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)+ coord_flip() + 
            theme_bw() + geom_pointrange(aes(x = IV, colour=grouping_colour, y = B, ymin = lo95, ymax = hi95), lwd = 1/2, position = position_dodge(width = 1/2), shape = 21) + 
            ylab("Unstandardized OLS regression coefficients") + xlab("") + 
            geom_linerange(aes(x = IV, colour=grouping_colour, ymin = lo90,  ymax = hi90), lwd = 1, position = position_dodge(width = 1/2)) + scale_color_manual(values=c('Grey','Black'), guide = FALSE) + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))  +  theme(strip.text.y = element_text(angle = 0))  

ggsave(fig_h1_4, file= paste0(overleaf,"/figures/H1_4_results.pdf"), dpi=900, width=6, height=5)

#correlations in H2 between labii and self-report
cor.test(data.reshaped$labii_did, data.reshaped$sr_disgust_did)
cor.test(data.reshaped$labii_did_win, data.reshaped$sr_disgust_did)

# Subset for preregistered robustness checks
regression.results.robustness <- regression.results[regression.results$Var2=="treatment"| regression.results$Var2=="partisanship.strength"| regression.results$Var2=="moral",]
regression.results.robustness <- regression.results.robustness[regression.results.robustness$main=="Robust",]

fig_h1_4_robustness<- ggplot(regression.results.robustness, aes(x = IV)) + 
  facet_grid(vars(hypothesis), vars(measure), 
             scales = "free", space="fixed") + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)+ coord_flip() + 
  theme_bw() + geom_pointrange(aes(x = IV, colour=grouping_colour, y = B, ymin = lo95, ymax = hi95), lwd = 1/2, position = position_dodge(width = 1/2), shape = 21) + 
  ylab("Unstandardized OLS regression coefficients") + xlab("") + 
  geom_linerange(aes(x = IV, colour=grouping_colour, ymin = lo90,  ymax = hi90), lwd = 1, position = position_dodge(width = 1/2)) + scale_color_manual(values=c('Grey','Black'), guide = FALSE) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  +  theme(strip.text.y = element_text(angle = 0), strip.text.x=element_text(size=8))  

ggsave(fig_h1_4_robustness, file= paste0(overleaf,"/figures/H1_4_robustness.pdf"), dpi=900, width=6, height=5)



# Subset for preregistered robustness checks: sexual and pathogen disgust-------------
regression.results.robustness <- regression.results[regression.results$IV=="Pathogen\n disgust"| regression.results$IV=="Sexual\n disgust",]

regression.results.robustness$measure<-factor(regression.results.robustness$measure, levels = c("Labii", "Self-reported\nDisgust", "Corrugator", "SCL", "Self-reported\nAnger", "Self-reported\nAnxiety", "Self-reported\nEnthusiasm"))

fig_h1_4_robustness_disgust<-ggplot(regression.results.robustness, aes(x = IV)) + 
  facet_grid(vars(hypothesis), vars(measure), 
             scales = "free", space="fixed") + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)+ coord_flip() + 
  theme_bw() + geom_pointrange(aes(x = IV, colour=grouping_colour, y = B, ymin = lo95, ymax = hi95), lwd = 1/2, position = position_dodge(width = 1/2), shape = 21) + 
  ylab("Unstandardized OLS regression coefficients") + xlab("") + 
  geom_linerange(aes(x = IV, colour=grouping_colour, ymin = lo90,  ymax = hi90), lwd = 1, position = position_dodge(width = 1/2)) + scale_color_manual(values=c('Grey','Black'), guide = FALSE) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  +  theme(strip.text.y = element_text(angle = 0), strip.text.x = element_text(angle = 90))  

ggsave(fig_h1_4_robustness_disgust, file= paste0(overleaf,"/figures/H1_4_robustness_dsr.pdf"), dpi=900, width=6, height=5)



