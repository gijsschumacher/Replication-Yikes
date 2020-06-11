
full.data$amsterdam <- ifelse(full.data$respondent<45,0,1)
# qualtrics vars ----------------------------------------------------------
full.data$know1_correct<-ifelse(full.data$know1==4, 1,0) #5 years
full.data$know2_correct<-ifelse(full.data$know2==2, 1,0) #VVD
full.data$know3_correct<-ifelse(full.data$know3==5, 1,0) #Juncker
full.data$know4_correct<-ifelse(full.data$know4==2, 1,0) #Macron
full.data$know5_correct<-ifelse(full.data$know5==1, 1,0) #Georgieva
full.data$know<-(full.data$know1_correct + full.data$know2_correct + full.data$know3_correct + full.data$know4_correct + full.data$know5_correct)/5

# check here if full.data#Education<2
full.data$Education <- factor(full.data$Education-2, levels=c(0,1,2,3), labels=c("Secondary", "Secondary Vocational", "Higher Vocational", "University"))

full.data$age <- full.data$Age_1+17

moral <- data.frame(full.data$dsr_1, full.data$dsr_4 , full.data$dsr_7 , full.data$dsr_10 , full.data$dsr_13 , full.data$dsr_16, full.data$dsr_19)
full.data$moral <- rowMeans(moral, na.rm=T)
sexual <- data.frame(full.data$dsr_2, full.data$dsr_5 , full.data$dsr_8 , full.data$dsr_11 , full.data$dsr_14 , full.data$dsr_17, full.data$dsr_20)
full.data$sexual <- rowMeans(sexual, na.rm=T)
pathogen <- data.frame(full.data$dsr_3, full.data$dsr_6 , full.data$dsr_9 , full.data$dsr_12 , full.data$dsr_15 , full.data$dsr_18, full.data$dsr_21)
full.data$pathogen<-rowMeans(pathogen, na.rm=T)

full.data$female <- full.data$Sex-1
full.data$treatment.order1 <- full.data$condition.in-1
#full.data$treatment.order2 <- # 0 = moral violation first, 1 = visual disgust first ....We forgot to do this.
full.data$student <- full.data$student-1


# presentation vars -------------------------------------------------------
full.data$partisanship.strength <- scale(apply(full.data[,paste0("partisan",seq(1,8,1))],1,mean))

# disgust
full.data$ingroup_mv_sr_disgust <- rowMeans(data.frame(full.data$condition.mv.in_afkeer, full.data$condition.mv.in_minachting, full.data$condition.mv.in_walging))
full.data$ingroup_sr_disgust <- rowMeans(data.frame(full.data$inparty_walging, full.data$inparty_afkeer, full.data$inparty_minachting))
full.data$outgroup_mv_sr_disgust <- rowMeans(data.frame(full.data$condition.mv.out_afkeer, full.data$condition.mv.out_minachting, full.data$condition.mv.out_walging))
full.data$outgroup_sr_disgust <- rowMeans(data.frame(full.data$outparty_walging, full.data$outparty_afkeer, full.data$outparty_minachting))

# anxiety
full.data$ingroup_mv_sr_anxiety <- rowMeans(data.frame(full.data$condition.mv.in_angstig , full.data$condition.mv.in_bang , full.data$condition.mv.in_bezorgd))
full.data$ingroup_sr_anxiety <- rowMeans(data.frame(full.data$inparty_bezorgd , full.data$inparty_angstig , full.data$inparty_bang))
full.data$condition.mv.out_bang<-ifelse(full.data$condition.mv.out_bang==-999, NA, full.data$condition.mv.out_bang) # i think this solves something we already solved before
full.data$outgroup_mv_sr_anxiety <- rowMeans(data.frame(full.data$condition.mv.out_angstig , full.data$condition.mv.out_bang , full.data$condition.mv.out_bezorgd))
full.data$outgroup_sr_anxiety <- rowMeans(data.frame(full.data$outparty_bezorgd , full.data$outparty_angstig , full.data$outparty_bang))

# anger
full.data$ingroup_mv_sr_anger <- rowMeans(data.frame(full.data$condition.mv.in_boos , full.data$condition.mv.in_bitter , full.data$condition.mv.in_hatelijk))
full.data$ingroup_sr_anger <- rowMeans(data.frame(full.data$inparty_hatelijk , full.data$inparty_boos , full.data$inparty_bitter))
full.data$outgroup_mv_sr_anger <- rowMeans(data.frame(full.data$condition.mv.out_boos , full.data$condition.mv.out_bitter , full.data$condition.mv.out_hatelijk))
full.data$outgroup_sr_anger <- rowMeans(data.frame(full.data$outparty_hatelijk , full.data$outparty_boos , full.data$outparty_bitter))

# enthusiasm
full.data$ingroup_mv_sr_enthusiasm <- rowMeans(data.frame(full.data$condition.mv.in_blij , full.data$condition.mv.in_trots , full.data$condition.mv.in_enthousiast))
full.data$ingroup_sr_enthusiasm <- rowMeans(data.frame(full.data$inparty_enthousiast , full.data$inparty_blij , full.data$inparty_trots))
full.data$outgroup_mv_sr_enthusiasm <- rowMeans(data.frame(full.data$condition.mv.out_blij , full.data$condition.mv.out_trots , full.data$condition.mv.out_enthousiast))
full.data$outgroup_sr_enthusiasm <- rowMeans(data.frame(full.data$outparty_enthousiast , full.data$outparty_blij , full.data$outparty_trots))

temp <- full.data[,which(colnames(full.data)=="pic.order.1"): which(colnames(full.data)=="pic.order.7")]
full.data$pic.order.A <- apply(temp, 1, function(x) which(x=="A"))
full.data$pic.order.B <- apply(temp, 1, function(x) which(x=="B"))
full.data$pic.order.C <- apply(temp, 1, function(x) which(x=="C"))
full.data$pic.order.D <- apply(temp, 1, function(x) which(x=="D"))
full.data$pic.order.E <- apply(temp, 1, function(x) which(x=="E"))
full.data$pic.order.F <- apply(temp, 1, function(x) which(x=="F"))

# vsrrp vars ---------------------------------------------------------------
full.data$labiiT_ingroup_pic <- full.data$ingroup_emg2-full.data$ingroup_isi_emg2
full.data$labiiT_outgroup_pic <- full.data$outgroup_emg2-full.data$outgroup_isi_emg2
full.data$labiiT_ingroup_mv <- full.data$ingroup_mv_emg2-full.data$ingroup_isi_mv_emg2
full.data$labiiT_outgroup_mv <- full.data$outgroup_mv_emg2-full.data$outgroup_isi_mv_emg2

# Generate disgusting images responses
full.data$labiiT_basket <- full.data$basket_emg2 - full.data$basket_isi_emg2
full.data$labiiT_lepel <- full.data$lepel_emg2 - full.data$lepel_isi_emg2
full.data$labiiT_worms <- full.data$worms_emg2 - full.data$worms_isi_emg2
full.data$labiiT_lamp <- full.data$lamp_emg2 - full.data$lamp_isi_emg2
full.data$labiiT_beker <- full.data$beker_emg2 - full.data$beker_isi_emg2
full.data$labiiT_poop <- full.data$poop_emg2 - full.data$poop_isi_emg2
full.data$labiiT_vomit <- full.data$vomit_emg2 - full.data$vomit_isi_emg2

full.data$corT_basket <- full.data$basket_emg1 - full.data$basket_isi_emg1
full.data$corT_lepel <- full.data$lepel_emg1 - full.data$lepel_isi_emg1
full.data$corT_worms <- full.data$worms_emg1 - full.data$worms_isi_emg1
full.data$corT_lamp <- full.data$lamp_emg1 - full.data$lamp_isi_emg1
full.data$corT_beker <- full.data$beker_emg1 - full.data$beker_isi_emg1
full.data$corT_poop <- full.data$poop_emg1 - full.data$poop_isi_emg1
full.data$corT_vomit <- full.data$vomit_emg1 - full.data$vomit_isi_emg1

full.data$scl_basket <- full.data$basket_scl - full.data$basket_isi_scl
full.data$scl_lepel <- full.data$lepel_scl - full.data$lepel_isi_scl
full.data$scl_worms <- full.data$worms_scl - full.data$worms_isi_scl
full.data$scl_lamp <- full.data$lamp_scl - full.data$lamp_isi_scl
full.data$scl_beker <- full.data$beker_scl - full.data$beker_isi_scl
full.data$scl_poop <- full.data$poop_scl - full.data$poop_isi_scl
full.data$scl_vomit <- full.data$vomit_scl - full.data$vomit_isi_scl

# Make winsorized vars
temp <- apply(full.data[,which(colnames(full.data)=="labiiT_ingroup_pic"):which(colnames(full.data)=="corT_vomit")],2,winsorize,95)
colnames(temp) <- paste0(colnames(temp), "_win")
full.data <- data.frame(full.data,temp)

# Generate Disgust DiD following equation 4
full.data$labiiT_ingroup_did <- full.data$labiiT_ingroup_mv - full.data$labiiT_ingroup_pic
full.data$labiiT_outgroup_did <- full.data$labiiT_outgroup_mv - full.data$labiiT_outgroup_pic
full.data$labiiT_ingroup_did_win <- full.data$labiiT_ingroup_mv_win - full.data$labiiT_ingroup_pic_win
full.data$labiiT_outgroup_did_win <- full.data$labiiT_outgroup_mv_win - full.data$labiiT_outgroup_pic_win

full.data$disgustT_ingroup_did <- full.data$ingroup_mv_sr_disgust - full.data$ingroup_sr_disgust
full.data$disgustT_outgroup_did <- full.data$outgroup_mv_sr_disgust - full.data$outgroup_sr_disgust

full.data$angerT_ingroup_did <- full.data$ingroup_mv_sr_anger - full.data$ingroup_sr_anger
full.data$angerT_outgroup_did <- full.data$outgroup_mv_sr_anger - full.data$outgroup_sr_anger

full.data$anxietyT_ingroup_did <- full.data$ingroup_mv_sr_anxiety - full.data$ingroup_sr_anxiety
full.data$anxietyT_outgroup_did <- full.data$outgroup_mv_sr_anxiety - full.data$outgroup_sr_anxiety

full.data$enthusiasmT_ingroup_did <- full.data$ingroup_mv_sr_enthusiasm - full.data$ingroup_sr_enthusiasm
full.data$enthusiasmT_outgroup_did <- full.data$outgroup_mv_sr_enthusiasm - full.data$outgroup_sr_enthusiasm

# Generate Disgust DiD image following equation 6
full.data$labiiT_out_vs_in <- full.data$labiiT_outgroup_pic - full.data$labiiT_ingroup_pic
full.data$sr_disgustT_out_vs_in <- full.data$outgroup_sr_disgust - full.data$ingroup_sr_disgust
full.data$labiiT_out_vs_in_win <- full.data$labiiT_outgroup_pic_win - full.data$labiiT_ingroup_pic_win

full.data$sr_angerT_out_vs_in <- full.data$outgroup_sr_anger - full.data$ingroup_sr_anger
full.data$sr_anxietyT_out_vs_in <- full.data$outgroup_sr_anxiety - full.data$ingroup_sr_anxiety
full.data$sr_enthusiasmT_out_vs_in <- full.data$outgroup_sr_enthusiasm - full.data$ingroup_sr_enthusiasm



# Generate SCL and Corrugator vars for robustness
# Corrugator
full.data$corrT_ingroup_pic <- full.data$ingroup_emg1-full.data$ingroup_isi_emg1
full.data$corrT_outgroup_pic <- full.data$outgroup_emg1-full.data$outgroup_isi_emg1
full.data$corrT_ingroup_mv <- full.data$ingroup_mv_emg1-full.data$ingroup_isi_mv_emg1
full.data$corrT_outgroup_mv <- full.data$outgroup_mv_emg1-full.data$outgroup_isi_mv_emg1

temp <- apply(full.data[,which(colnames(full.data)=="corrT_ingroup_pic"):which(colnames(full.data)=="corrT_outgroup_mv")],2,winsorize,95)
colnames(temp) <- paste0(colnames(temp), "_win")
full.data <- data.frame(full.data,temp)

full.data$corrT_ingroup_did <- full.data$corrT_ingroup_mv - full.data$corrT_ingroup_pic
full.data$corrT_outgroup_did <- full.data$corrT_outgroup_mv - full.data$corrT_outgroup_pic
full.data$corrT_ingroup_did_win <- full.data$corrT_ingroup_mv_win - full.data$corrT_ingroup_pic_win
full.data$corrT_outgroup_did_win <- full.data$corrT_outgroup_mv_win - full.data$corrT_outgroup_pic_win
full.data$corrT_out_vs_in <- full.data$corrT_outgroup_pic - full.data$corrT_ingroup_pic
full.data$corrT_out_vs_in_win <- full.data$corrT_outgroup_pic_win - full.data$corrT_ingroup_pic_win

# SCL
full.data$sclT_ingroup_pic <- full.data$ingroup_scl-full.data$ingroup_isi_scl
full.data$sclT_outgroup_pic <- full.data$outgroup_scl-full.data$outgroup_isi_scl
full.data$sclT_ingroup_mv <- full.data$ingroup_mv_scl-full.data$ingroup_isi_mv_scl
full.data$sclT_outgroup_mv <- full.data$outgroup_mv_scl-full.data$outgroup_isi_mv_scl
full.data$sclT_ingroup_did <- full.data$sclT_ingroup_mv - full.data$sclT_ingroup_pic
full.data$sclT_outgroup_did <- full.data$sclT_outgroup_mv - full.data$sclT_outgroup_pic
full.data$sclT_out_vs_in <- full.data$sclT_outgroup_pic - full.data$sclT_ingroup_pic

# Make compensation var
temp <- read.csv(paste0(root, "/Data/Lab data collection Fall 2019/Reward_participants.csv"), sep=";")
colnames(temp)[1:2] <- c("respondent","reward")
temp$reward <- factor(ifelse(is.na(temp$reward),2,temp$reward), levels=c(0,1,2), labels=c("Credit", "Money", "Voluntary"))
                      ### 0: credit, 1:money; 2: voluntary

full.data <- merge(full.data, temp, by="respondent")

save(full.data, file="Scripts/full.data.Rdata")
