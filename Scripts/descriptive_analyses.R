#load("psych")
#load("lavaan")



# Self-reported emotions --------------------------------------------------

### cfa for each condition, identifying the four emotions 
disgust <- data.frame(full.data$condition.mv.in_afkeer , full.data$condition.mv.in_minachting , full.data$condition.mv.in_walging,
full.data$inparty_walging , full.data$inparty_afkeer , full.data$inparty_minachting,
full.data$condition.mv.out_afkeer , full.data$condition.mv.out_minachting , full.data$condition.mv.out_walging,
full.data$outparty_walging , full.data$outparty_afkeer , full.data$outparty_minachting)



# dit aanpassen
disgust <-'disgust = ~ NA*dsr_1 + dsr_4 + dsr_7+ dsr_10+ dsr_13+ dsr_16+ dsr_19 
sexual = ~ NA*dsr_2 + dsr_5  + dsr_8  + dsr_11 + dsr_14 + dsr_17 + dsr_20
pathogen = ~ NA*dsr_3 + dsr_6 + dsr_9 + dsr_12 + dsr_15 + dsr_18 + dsr_21

moral ~~ 1*moral
sexual ~~ 1*sexual
pathogen ~~ 1*pathogen
' 
fit<-cfa(CFA_dsr, data=data)
