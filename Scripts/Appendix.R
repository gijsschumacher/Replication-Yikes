#root <- str_split_fixed(getwd(), "/Papers",2)[,1]

#overleaf <- ifelse(str_split_fixed(root, "/",4)[,3]=="gschuma1","C:/Users/gschuma1/Dropbox/Apps/Overleaf/PLS registered report",
#                   "C:/Users/bbakker1/Dropbox/Apps/ShareLaTeX/PLS registered report")

#Descriptive statistics covariates---------------------

full.data$Male<-ifelse(full.data$female==0, 1,0)
full.data$Female<-ifelse(full.data$female==1, 1,0)
full.data$Nonbinary<-ifelse(full.data$female==2, 1,0)

full.data$Edu1<-ifelse(full.data$Education=="Secondary", 1,0)
full.data$Edu2<-ifelse(full.data$Education=="Secondary Vocational", 1,0)
full.data$Edu3<-ifelse(full.data$Education=="Higher Vocational", 1,0)
full.data$Edu4<-ifelse(full.data$Education=="University", 1,0)

full.data$reward1 <-ifelse(full.data$reward=="Credit", 1,0)
full.data$reward2 <-ifelse(full.data$reward=="Money", 1,0)
full.data$reward3 <-ifelse(full.data$reward=="Voluntary", 1,0)

descriptives<- data.frame(full.data$treatment.order1, full.data$partisanship.strength, full.data$moral, full.data$sexual, full.data$pathogen, full.data$age, full.data$Male, full.data$Female, full.data$Nonbinary, full.data$Edu1, full.data$Edu2, full.data$Edu3, full.data$Edu4,  full.data$student, full.data$know, full.data$amsterdam, full.data$reward1, full.data$reward2,full.data$reward3, full.data$Comment_coded)

descriptives$full.data.partisanship.strength<-scale(descriptives$full.data.partisanship.strength)
descriptives$full.data.moral<-scale(descriptives$full.data.moral)
descriptives$full.data.sexual<-scale(descriptives$full.data.sexual)
descriptives$full.data.pathogen<-scale(descriptives$full.data.pathogen)

colnames(descriptives)<- c("Treatment order", "Partisan Social Identity Strength", "Moral disgust", "Sexual disgust", "Pathogen disgust", "Age", "Sex: Male", "Sex: Female", "Sex: Non-binary", "Education: Secondary", "Education: Secondary Vocational", "Education: Higher Vocational", "Education: University", "Student", "Political Knowledge", "Location: Amsterdam", "Reward: Credit", "Reward: Money", "Reward: Voluntary", "Lab event")

stargazer(descriptives, title="Descriptive statistics covariates", align=TRUE, out=paste0(overleaf,"/tables/descriptives_covariates.tex"),label="tab:descriptives", digits=3,  dep.var.labels.include = FALSE)

#Assessment of the DSR 21------------------

#Confirmator factor analysis
CFA_dsr <-'moral = ~ NA*dsr_1 + dsr_4 + dsr_7+ dsr_10+ dsr_13+ dsr_16+ dsr_19 
sexual = ~ NA*dsr_2 + dsr_5  + dsr_8  + dsr_11 + dsr_14 + dsr_17 + dsr_20
pathogen = ~ NA*dsr_3 + dsr_6 + dsr_9 + dsr_12 + dsr_15 + dsr_18 + dsr_21

moral ~~ 1*moral
sexual ~~ 1*sexual
pathogen ~~ 1*pathogen
' 
fit<-cfa(CFA_dsr, ordered=c("dsr_1", "dsr_2", "dsr_3", "dsr_4", "dsr_5", "dsr_6", "dsr_7", "dsr_8", "dsr_9", "dsr_10", "dsr_11", "dsr_12", "dsr_13", "dsr_14", "dsr_15", "dsr_16", "dsr_17", "dsr_18", "dsr_19", "dsr_20", "dsr_21"), data=full.data)
summary(fit, fit.measures=TRUE)

#assess fit
p<-parameterEstimates(fit, standardized=TRUE) %>%  dplyr::select(std.all, pvalue)
p <- p[1:21, ] 
names(p) <- c("Standardized Factor Loading", "p-value")
(setattr(p, "row.names", c("moral1", "moral2", "moral3", "moral4", "moral5", "moral6", "moral7", "sexual1", "sexual2", "sexual3", "sexual4", "sexual5", "sexual6", "sexual7", "pathogen1", "pathogen2", "pathogen3", "pathogen4", "pathogen5", "pathogen6", "pathogen7")))

print(xtable(p,  type = "latex", caption= "DSR-21: Standardized Factor Loadings", label="tab:dsr_factor"), caption.placement = 'top', file = paste0(overleaf,"/tables/DSR_factor.tex"), size="small")

#Correlation matrix
dimensions<-with(full.data, data.frame(moral, sexual, pathogen), na.rm=T)
colnames(dimensions)<-c("Moral", "Sexual", "Pathogen")
dimensions <- na.omit(dimensions)
cor_all <- melt(get_lower_tri(round(cor(dimensions[complete.cases(dimensions), ]),2)), na.rm=T)
full_corr <- ggplot(cor_all, aes(Var2, Var1, fill = value))+  geom_tile(color = "white")+   scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") + theme_minimal()+  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1))+  coord_fixed() + geom_text(aes(Var2, Var1, label = gsub("0\\.", "\\.", value)), color = "black", size = 2) +  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), legend.justification = c(1, 0), legend.position = c(0.6, 0.7), legend.direction = "horizontal")+ guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", title.hjust = 0.5)) + ggtitle("Pearson correlation between Moral, Sexual and Pathogen disgust") + theme(legend.position="none")
ggsave(full_corr, file= paste0(overleaf,"/Figures/DSR_correlation.pdf"), dpi=900)

#DSR: moral disgust
alpha_moral <- data.frame(full.data$dsr_1, full.data$dsr_4 , full.data$dsr_7 , full.data$dsr_10 , full.data$dsr_13 , full.data$dsr_16, full.data$dsr_19)
psych::alpha(alpha_moral)

#DSR: sexual disgust
alpha_sexual <- data.frame(full.data$dsr_2, full.data$dsr_5 , full.data$dsr_8 , full.data$dsr_11 , full.data$dsr_14 , full.data$dsr_17, full.data$dsr_20)
psych::alpha(alpha_sexual)

#DSR: pathogen
alpha_pathogen <- data.frame(full.data$dsr_3, full.data$dsr_6 , full.data$dsr_9 , full.data$dsr_12 , full.data$dsr_15 , full.data$dsr_18, full.data$dsr_21)
psych::alpha(alpha_pathogen)



#Descriptive statistics of the emotions in the moral violations experiment---------------------
descriptives<- data.frame(full.data$ingroup_sr_disgust, full.data$outgroup_sr_disgust, full.data$ingroup_mv_sr_disgust, full.data$outgroup_mv_sr_disgust, full.data$ingroup_sr_anxiety, full.data$outgroup_sr_anxiety, full.data$ingroup_mv_sr_anxiety, full.data$outgroup_mv_sr_anxiety, full.data$ingroup_sr_anger, full.data$outgroup_sr_anger, full.data$ingroup_mv_sr_anger, full.data$outgroup_mv_sr_anger, full.data$ingroup_sr_enthusiasm, full.data$outgroup_sr_enthusiasm, full.data$ingroup_mv_sr_enthusiasm, full.data$outgroup_mv_sr_enthusiasm)

colnames(descriptives)<- c("Disgust ingroup", "Disgust outgroup", "Disgust ingroup + MV", "Disgust outgroup + MV", "Anxiety ingroup", "Anxiety outgroup", "Anxiety ingroup + MV", "Anxiety outgroup + MV", "Anger ingroup", "Anger outgroup", "Anger ingroup + MV", "Anger outgroup + MV" , "Enthusiasm ingroup", "Enthusiasm outgroup", "Enthusiasm ingroup + MV", "Enthusiasm outgroup + MV")  

stargazer(descriptives, title="Descriptive statistics self-reported emotions Moral Violations experiment", align=TRUE, out=paste0(overleaf,"/tables/selfreport_descriptives.tex"),label="tab:descriptives_emotions", digits=3,  dep.var.labels.include = FALSE)

# disgust ingroup_sr_disgust
alpha<-list() 
alpha[[1]]<-psych::alpha(data.frame(full.data$inparty_walging, full.data$inparty_afkeer, full.data$inparty_minachting))
# disgust outgroup_sr_disgust
alpha[[2]]<-psych::alpha(data.frame(full.data$outparty_walging, full.data$outparty_afkeer, full.data$outparty_minachting))
# disgust ingroup_mv_sr_disgust
alpha[[3]]<-psych::alpha(data.frame(full.data$condition.mv.in_afkeer, full.data$condition.mv.in_minachting, full.data$condition.mv.in_walging))
# disgust outgroup_mv_sr_disgust
alpha[[4]]<-psych::alpha(data.frame(full.data$condition.mv.out_afkeer, full.data$condition.mv.out_minachting, full.data$condition.mv.out_walging))

# anxiety: ingroup_sr_anxiety
alpha[[5]]<-psych::alpha(data.frame(full.data$inparty_bezorgd , full.data$inparty_angstig , full.data$inparty_bang))
# anxiety: outgroup_sr_anxiety
alpha[[6]]<-psych::alpha(data.frame(full.data$outparty_bezorgd , full.data$outparty_angstig , full.data$outparty_bang))
# anxiety: ingroup_mv_sr_anxiety
alpha[[7]]<-psych::alpha(data.frame(full.data$condition.mv.in_angstig , full.data$condition.mv.in_bang , full.data$condition.mv.in_bezorgd))
# anxiety: outgroup_mv_sr_anxiety
alpha[[8]]<-psych::alpha(data.frame(full.data$condition.mv.out_angstig , full.data$condition.mv.out_bang , full.data$condition.mv.out_bezorgd))


# anger: ingroup_sr_anger
alpha[[9]]<-psych::alpha(data.frame(full.data$inparty_hatelijk , full.data$inparty_boos , full.data$inparty_bitter))
# anger: outgroup_sr_anger 
alpha[[10]]<-psych::alpha(data.frame(full.data$outparty_hatelijk , full.data$outparty_boos , full.data$outparty_bitter))
# anger: ingroup_mv_sr_anger
alpha[[11]]<-psych::alpha(data.frame(full.data$condition.mv.in_boos , full.data$condition.mv.in_bitter , full.data$condition.mv.in_hatelijk))
# anger: outgroup_mv_sr_anger 
alpha[[12]]<-psych::alpha(data.frame(full.data$condition.mv.out_boos , full.data$condition.mv.out_bitter , full.data$condition.mv.out_hatelijk))

# enthusiasm

# enthusiasm: ingroup_sr_enthusiasm 
alpha[[13]]<-psych::alpha(data.frame(full.data$inparty_enthousiast , full.data$inparty_blij , full.data$inparty_trots))
# enthusiasm: outgroup_sr_enthusiasm 
alpha[[14]]<-psych::alpha(data.frame(full.data$outparty_enthousiast , full.data$outparty_blij , full.data$outparty_trots))
# enthusiasm: ingroup_mv_sr_enthusiasm 
alpha[[15]]<-psych::alpha(data.frame(full.data$condition.mv.in_blij , full.data$condition.mv.in_trots , full.data$condition.mv.in_enthousiast))
# enthusiasm: outgroup_mv_sr_enthusiasm 
alpha[[16]]<-psych::alpha(data.frame(full.data$condition.mv.out_blij , full.data$condition.mv.out_trots , full.data$condition.mv.out_enthousiast))


cronbachs<-as.data.frame(rbind(alpha[[1]]$total[[1]],alpha[[2]]$total[[1]],alpha[[3]]$total[[1]],alpha[[4]]$total[[1]],alpha[[5]]$total[[1]],alpha[[6]]$total[[1]],alpha[[7]]$total[[1]],alpha[[8]]$total[[1]], alpha[[9]]$total[[1]],alpha[[10]]$total[[1]],alpha[[11]]$total[[1]],alpha[[12]]$total[[1]], alpha[[13]]$total[[1]],alpha[[14]]$total[[1]],alpha[[15]]$total[[1]],alpha[[16]]$total[[1]]))
cronbachs$dv<-c("Disgust ingroup", "Disgust outgroup", "Disgust ingroup + MV", "Disgust outgroup + MV", "Anxiety ingroup", "Anxiety outgroup", "Anxiety ingroup + MV", "Anxiety outgroup + MV", "Anger ingroup", "Anger outgroup", "Anger ingroup + MV", "Anger outgroup + MV",  "Enthusiasm ingroup", "Enthusiasm outgroup", "Enthusiasm ingroup + MV", "Enthusiasm outgroup + MV")
colnames(cronbachs)<-c("Alpha", "Emotion + condition")
cronbachs<- cronbachs[,c(2,1)]


print(xtable(cronbachs,  type = "latex", caption="Morival violation experiment: Cronbach's alpha's of the self-reported emotions", label="tab:selfreport_cronbach"), caption.placement = 'top', file = paste0(overleaf,"/tables/selfreport_cronbach.tex"), size="\\fontsize{10pt}{10pt}\\selectfont", include.rownames=FALSE)

#correlation matrix 1: ingroup
dimensions<-with(full.data, data.frame(ingroup_sr_disgust, ingroup_sr_anger, ingroup_sr_anxiety, ingroup_sr_enthusiasm), na.rm=T)
colnames(dimensions)<-c("Disgust", "Anger", "Anxiety", "Enthusiasm")
dimensions <- na.omit(dimensions)
cor_all <- melt(get_lower_tri(round(cor(dimensions[complete.cases(dimensions), ]),2)), na.rm=T)
corr_ingroup <- ggplot(cor_all, aes(Var2, Var1, fill = value))+  geom_tile(color = "white")+   scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") + theme_minimal()+  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1))+  coord_fixed() + geom_text(aes(Var2, Var1, label = gsub("0\\.", "\\.", value)), color = "black", size = 2) +  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), legend.justification = c(1, 0), legend.position = c(0.6, 0.7), legend.direction = "horizontal")+ guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", title.hjust = 0.5)) + ggtitle("A: Ingroup") + theme(legend.position="none")


#correlation matrix 2: outgroup
dimensions<-with(full.data, data.frame(outgroup_sr_disgust, outgroup_sr_anger, outgroup_sr_anxiety, outgroup_sr_enthusiasm), na.rm=T)
colnames(dimensions)<-c("Disgust", "Anger", "Anxiety", "Enthusiasm")
dimensions <- na.omit(dimensions)
cor_all <- melt(get_lower_tri(round(cor(dimensions[complete.cases(dimensions), ]),2)), na.rm=T)
corr_outgroup <- ggplot(cor_all, aes(Var2, Var1, fill = value))+  geom_tile(color = "white")+   scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") + theme_minimal()+  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1))+  coord_fixed() + geom_text(aes(Var2, Var1, label = gsub("0\\.", "\\.", value)), color = "black", size = 2) +  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), legend.justification = c(1, 0), legend.position = c(0.6, 0.7), legend.direction = "horizontal")+ guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", title.hjust = 0.5)) + ggtitle("C: Outgroup") + theme(legend.position="none")

#correlation matrix 3: ingroup + mv
dimensions<-with(full.data, data.frame(ingroup_mv_sr_disgust, ingroup_mv_sr_anger, ingroup_mv_sr_anxiety, ingroup_mv_sr_enthusiasm), na.rm=T)
colnames(dimensions)<-c("Disgust", "Anger", "Anxiety", "Enthusiasm")
dimensions <- na.omit(dimensions)
cor_all <- melt(get_lower_tri(round(cor(dimensions[complete.cases(dimensions), ]),2)), na.rm=T)
corr_ingroup_mv <- ggplot(cor_all, aes(Var2, Var1, fill = value))+  geom_tile(color = "white")+   scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") + theme_minimal()+  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1))+  coord_fixed() + geom_text(aes(Var2, Var1, label = gsub("0\\.", "\\.", value)), color = "black", size = 2) +  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), legend.justification = c(1, 0), legend.position = c(0.6, 0.7), legend.direction = "horizontal")+ guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", title.hjust = 0.5)) + ggtitle("B: Ingroup + MV") + theme(legend.position="none")


#correlation matrix 4: outgroup + mv
dimensions<-with(full.data, data.frame(outgroup_mv_sr_disgust, outgroup_mv_sr_anger, outgroup_mv_sr_anxiety, outgroup_mv_sr_enthusiasm), na.rm=T)
colnames(dimensions)<-c("Disgust", "Anger", "Anxiety", "Enthusiasm")
dimensions <- na.omit(dimensions)
cor_all <- melt(get_lower_tri(round(cor(dimensions[complete.cases(dimensions), ]),2)), na.rm=T)
corr_outgroup_mv <- ggplot(cor_all, aes(Var2, Var1, fill = value))+  geom_tile(color = "white")+   scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") + theme_minimal()+  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1))+  coord_fixed() + geom_text(aes(Var2, Var1, label = gsub("0\\.", "\\.", value)), color = "black", size = 2) +  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), legend.justification = c(1, 0), legend.position = c(0.6, 0.7), legend.direction = "horizontal")+ guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", title.hjust = 0.5)) + ggtitle("D: Outgroup + MV") + theme(legend.position="none")


### Figure1: Combine 4 samples in one graph-------------------
lay <- rbind(c(1, 2),
             c(3, 4))
fig_cor <- grid.arrange(corr_ingroup, corr_ingroup_mv, corr_outgroup, corr_outgroup_mv, layout_matrix = lay)

ggsave(fig_cor, file=paste0(overleaf,"/figures/MV_emotions.pdf"), dpi=900)



# Output manipulation check----------------
stargazer(mp.check1.robust, mp.check1.win.robust, mp.check2.robust, mp.check3.robust, mp.check3.win.robust, mp.check4.robust, title="Manipulation check: Responses (Labii, self-report, corrugator and skin conductance)", align=TRUE, omit.stat=c("LL","ser","f", "adj.rsq"), covariate.labels=c("Mug", "Lamp", "Spoon", "Poop", "Vomit", "Worms", "Treatment order", "Amsterdam"), notes.append = FALSE, column.labels = c("Labii", "Labii: exp", "Self-report", "Corrugator", "Corrugator: exp", "SCL"), column.separate = c(1,1,1,1,1,1), star.cutoffs = c(.1, .05), label="tab:manipulation", out=paste0(overleaf,"/tables/manipulation_check.tex"), no.space = TRUE, digits=3, notes = c("Untandardized OLS regression coefficients", "*p<.1, **p<.05"), dep.var.labels.include = FALSE)


# Write out regression results------------------------

#H1
stargazer(regression.output[[1]],regression.output[[2]],regression.output[[3]], title="Hypothesis 1: Out-party politicians should elicit stronger disgust responses than in-party politicians.", align=TRUE, omit.stat=c("LL","ser","f", "adj.rsq"), covariate.labels=c("Out-party", "Treatment order", "Age", "Female", "Edu: Secondary vocational", "Edu: Higher vocational", "Edu: University", "Student", 'Temperature', "Knowledge", "Amsterdam", "Reward: Money (ref: credits)", "Reward: Voluntary", "Lab event"), notes.append = FALSE, font.size= "tiny", column.labels = c("Preregistered Labii", "Exploratory Labii", "Preregistered Self-report"), column.separate = c(1,1,1), star.cutoffs = c(.05),  notes = c("Untandardized OLS regression coefficients", "*p<.05"), no.space=TRUE, out=paste0(overleaf,"/tables/H1_results.tex"),label="tab:h1results", digits=3,  dep.var.labels.include = FALSE)

#H2
stargazer(regression.output[[10]],regression.output[[11]],regression.output[[12]], title="Hypothesis 2a/b: Out-party (In-party) politicians accused of moral violations should elicit stronger disgust responses than in-party (Out-party) politicians accused of moral violations..", align=TRUE, omit.stat=c("LL","ser","f", "adj.rsq"), covariate.labels=c("Out-party", "Treatment order", "Age", "Female", "Edu: Secondary vocational", "Edu: Higher vocational", "Edu: University", "Student", 'Temperature', "Knowledge", "Amsterdam", "Reward: Money (ref: credits)", "Reward: Voluntary", "Lab event"), notes.append = FALSE, font.size= "tiny", column.labels = c("Preregistered Labii", "Exploratory Labii", "Preregistered Self-report"), column.separate = c(1,1,1), star.cutoffs = c(.05),  notes = c("Untandardized OLS regression coefficients", "*p<.05"), no.space=TRUE, out=paste0(overleaf,"/tables/H2_results.tex"),label="tab:h2results", digits=3,  dep.var.labels.include = FALSE)

#H3
stargazer(regression.output[[19]],regression.output[[20]],regression.output[[21]], title="Hypothesis 3: Strong partisans have stronger disgust responses to out-party leaders compared to in-party leaders than weak partisans.", align=TRUE, omit.stat=c("LL","ser","f", "adj.rsq"), covariate.labels=c("Partisan Social Identity Strength", "Treatment order", "Age", "Female", "Edu: Secondary vocational", "Edu: Higher vocational", "Edu: University", "Student", 'Temperature', "Knowledge", "Amsterdam", "Reward: Money (ref: credits)", "Reward: Voluntary", "Lab event"), notes.append = FALSE, font.size= "tiny", column.labels = c("Preregistered Labii", "Exploratory Labii", "Preregistered Self-report"), column.separate = c(1,1,1), star.cutoffs = c(.05),  notes = c("Untandardized OLS regression coefficients", "*p<.05"), no.space=TRUE, out=paste0(overleaf,"/tables/H3_results.tex"),label="tab:h3results", digits=3,  dep.var.labels.include = FALSE)

#H4
stargazer(regression.output[[28]],regression.output[[29]],regression.output[[30]], title="Hypothesis 4: Individuals higher on moral disgust sensitivity, compared to those lower on moral disgust sensitivity, have a stronger disgust response to our moral violation treatments.", align=TRUE, omit.stat=c("LL","ser","f", "adj.rsq"), covariate.labels=c("Moral disgust", "Out-party", "Treatment order", "Age", "Female", "Edu: Secondary vocational", "Edu: Higher vocational", "Edu: University", "Student", 'Temperature', "Knowledge", "Amsterdam", "Reward: Money (ref: credits)", "Reward: Voluntary", "Lab event"), notes.append = FALSE, font.size= "tiny", column.labels = c("Preregistered Labii", "Exploratory Labii", "Preregistered Self-report"), column.separate = c(1,1,1), star.cutoffs = c(.05),  notes = c("Untandardized OLS regression coefficients", "*p<.05"), no.space=TRUE, out=paste0(overleaf,"/tables/H4_results.tex"),label="tab:h4results", digits=3,  dep.var.labels.include = FALSE)

#Check if missing is systematic
data.reshaped$labii_missing<-ifelse(is.na(data.reshaped$labii_T)==TRUE, 1,0)

missing <- glm(labii_missing ~ age + female + Education + student + Temperature + know + amsterdam + reward + Comment_coded, family="binomial", data=data.reshaped)
summary(missing)

