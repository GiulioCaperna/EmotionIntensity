#----------------------------------------------------------------#
#
# Analysis script for the paper:
# "The intensity of emotion: Altered processing of others' 
#  facial expressions in congenital facial palsy"
# 
# Schiano-Lomoriello, A., Caperna, G., De Stefani E., Ferrari P.F., and Sessa P.
#
#----------------------------------------------------------------#

rm(list=ls())
#---------------------- LIBRARIES -------------------------------
library(plyr)
library(tidyverse)
library(FactoMineR)
library(ggpubr)
library(smacof)
library(readxl)
library(ggrepel)


#--------------------- P1 Component -----------------------------
#
# Importing the data

raw_control_rate <- read_excel("C:/Users/capergi/Dropbox/Moskon/moebius/Control_MoebiusRating.xlsx")
raw_moeb_rate <- read_excel("C:/Users/capergi/Dropbox/Moskon/moebius/MoebiusRating_all.xlsx") 
control_mask<-read_excel("C:/Users/capergi/Dropbox/Moskon/moebius/CTRL_Mask_10sbj2.xlsx")   

### in the first part of the code, the data contains another group of subjects called "Mask" 
### this group, was finally not used in the present article. The "mask" data is excluded in the code.

# delete an error observation
raw_moeb_rate1 = raw_moeb_rate[-151,]

# copy emotion level for subject 1 from subject 2 (the list of emotions is the same)
raw_moeb_rate1[1:150,10]=raw_moeb_rate1[151:300,10]

# delete subject 99 (due to an error)
sbj99 = which(raw_moeb_rate1$Subject==99)
moeb_rate = raw_moeb_rate1[-(sbj99),]
moeb_rate$Rating = as.numeric(moeb_rate$Rating)
cont_rate = raw_control_rate
mask_rate = control_mask

rm(control_mask, raw_control_rate, raw_moeb_rate, raw_moeb_rate1)


cont_rate$Rating=as.numeric(cont_rate$Rating)

# change emotion level "neutral" to fit with the others
moeb_rate$EmotionLevel[which(moeb_rate$EmotionLevel=="neutral")] = "neutral 0"
cont_rate$EmotionLevel[which(cont_rate$EmotionLevel=="neutral")] = "neutral 0"
mask_rate$EmotionLevel[which(mask_rate$EmotionLevel=="neutral")] = "neutral 0"

#preparing CTRL data to be merged with MBS
cont_rate$Session = cont_rate$Session+1


names(mask_rate)[1] = names(moeb_rate)[1]

three_rate=rbind(moeb_rate,cont_rate,mask_rate)

###correction in misreported sbj 1 session value
three_rate$Session[three_rate$Subject==1 & three_rate$Session==2] = 1


three_rate$cc = vector("character",dim(three_rate)[1])
three_rate$cc[three_rate$Session==2]="Control"
three_rate$cc[three_rate$Session==1]="Moebius"
three_rate$cc[three_rate$Session==3]="Mask"

table(three_rate$Session,three_rate$Subject)

###correction numbers id control
three_rate$Subject=three_rate$Subject%%111+three_rate$Subject%/%111


### emotion_shown emotion_level
three_rate$emotion_shown=unlist(strsplit(three_rate$EmotionLevel,fixed = T,split = " "))[seq(1,9900,2)]
three_rate$emotion_level=as.numeric(unlist(strsplit(three_rate$EmotionLevel,fixed = T,split = " "))[seq(2,9900,2)])


meanrates=tapply(three_rate$Rating,paste(three_rate$cc,three_rate$`Procedure[Block]`, three_rate$EmotionLevel),mean)
sdrates=tapply(three_rate$Rating,paste(three_rate$cc,three_rate$`Procedure[Block]`, three_rate$EmotionLevel),sd)

splitted=strsplit(names(meanrates),split = " ")

#creation of the main dataset
emo2=data.frame(t(data.frame(splitted)))

emo2$meanrates=meanrates
emo2$sdrates=sdrates

###subtraction of mean value of neutral face
emo2$relrates=0
for (i in unique(emo2$X2)){
print(i)
    for (j in unique(emo2$X1)){
print(j)
  subgroup= which(emo2$X1==j&emo2$X2==i)
  print(length(subgroup))
  rel_neu= which(emo2$X1==j&emo2$X2==i&emo2$X4==0)
  print(length(rel_neu))
  emo2$relrates[subgroup]=emo2$meanrates[subgroup]-emo2$meanrates[rel_neu]
  }
  
}

#### Chang name column "Proc"
names(three_rate)[9]="ProcEmo"


## Pairwise test of difference from neutral 
emo3=emo2[(emo2$X3!="neutral"),]
emo3$test_noneut=rep(NA, dim(emo3)[1])
colnames(emo3)[1:4]=c("cc","ProcEmo","emotion_shown","emotion_level")
for (tt in 1:dim(emo3)[1]){
  bibu=subset(x = three_rate$Rating,subset = three_rate$ProcEmo==emo3$ProcEmo[tt]&three_rate$emotion_shown==emo3$emotion_shown[tt]&three_rate$emotion_level==emo3$emotion_level[tt]&three_rate$cc==emo3$cc[tt])
  bubi=subset(x = three_rate$Rating,subset = three_rate$ProcEmo==emo3$ProcEmo[tt]&three_rate$EmotionLevel=="neutral 0"&three_rate$cc==emo3$cc[tt])
  if (length(bibu)!=11|length(bubi)!=33)
  print("wrong number of observations")
  emo3$test_noneut[tt]=round(wilcox.test(bubi,bibu)$p.value,3)
}
emo3$nnmed=rep(NA, dim(emo3)[1])
emo3$nnstat=rep(NA, dim(emo3)[1])
for (tt in 1:dim(emo3)[1]){
  bibu=subset(x = three_rate$Rating,subset = three_rate$ProcEmo==emo3$ProcEmo[tt]&three_rate$emotion_shown==emo3$emotion_shown[tt]&three_rate$emotion_level==emo3$emotion_level[tt]&three_rate$cc==emo3$cc[tt])
  bubi=subset(x = three_rate$Rating,subset = three_rate$ProcEmo==emo3$ProcEmo[tt]&three_rate$EmotionLevel=="neutral 0"&three_rate$cc==emo3$cc[tt])
  if (length(bibu)!=11|length(bubi)!=33)
    print("wrong number of observations")
  emo3$nnmed[tt]=round(wilcox.test(bubi,bibu,conf.int = T)$estimate,2)
  emo3$nnstat[tt]=round(wilcox.test(bubi,bibu,conf.int = T)$statistic,1)
}

###### onetail test
emo3$test_noneut2=rep(NA, dim(emo3)[1])
emo3$nnmed2=rep(NA, dim(emo3)[1])
emo3$nnstat2=rep(NA, dim(emo3)[1])
for (tt in 1:dim(emo3)[1]){
  bibu=subset(x = three_rate$Rating,subset = three_rate$ProcEmo==emo3$ProcEmo[tt]&three_rate$emotion_shown==emo3$emotion_shown[tt]&three_rate$emotion_level==emo3$emotion_level[tt]&three_rate$cc==emo3$cc[tt])
  bubi=subset(x = three_rate$Rating,subset = three_rate$ProcEmo==emo3$ProcEmo[tt]&three_rate$EmotionLevel=="neutral 0"&three_rate$cc==emo3$cc[tt])
  if (length(bibu)!=11|length(bubi)!=33)
    print("wrong observations number")
  emo3$nnmed2[tt]=round(wilcox.test(bubi,bibu,alternative="less",conf.int = T)$estimate,2)
  emo3$nnstat2[tt]=round(wilcox.test(bubi,bibu,alternative="less",conf.int = T)$statistic,1)
  emo3$test_noneut2[tt]=round(wilcox.test(bubi,bibu,alternative="less")$p.value,3)

  }

wilcox.test(bibu,bubi,alternative = "greater",conf.int = T)
table(emo3$test_noneut<0.05,emo3$cc)


#identification of coherent trials
observed_right<-c(0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,
                    0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1)

emo3$right_emo <- observed_right

## pvalues of coherent different from second
emo3$dif_da_sec=1

for (tt in 1:dim(emo3)[1]){
  if (emo3$right_emo[tt]==1){
 
  bibu=subset(x = three_rate$Rating,subset = three_rate$ProcEmo==emo3$ProcEmo[tt]&three_rate$emotion_shown==emo3$emotion_shown[tt]&three_rate$emotion_level==emo3$emotion_level[tt]&three_rate$cc==emo3$cc[tt])
  positions=which( emo3$right_emo==0 & emo3$emotion_shown==emo3$emotion_shown[tt] & emo3$emotion_level==emo3$emotion_level[tt] & emo3$cc==emo3$cc[tt])
  vs=positions[which(emo3$relrates[positions]==max(emo3$relrates[positions]))][1]##[1] perchè possono esserci due vs
  bubi=subset(x = three_rate$Rating,subset = three_rate$ProcEmo==emo3$ProcEmo[vs]&three_rate$emotion_shown==emo3$emotion_shown[vs]&three_rate$emotion_level==emo3$emotion_level[vs]&three_rate$cc==emo3$cc[vs])
  #print(paste0(mean(bibu)," ",mean(bubi)))
  if (length(bibu)!=11|length(bubi)!=11)
  print("errato numero di osservazioni")
  emo3$dif_da_sec[tt]=round(wilcox.test(bibu,bubi,paired = TRUE)$p.value,3)##lascio il test a due code
  print(paste0(emo3$ProcEmo[tt]," ",emo3$emotion_shown[tt]," ",emo3$emotion_level[tt]," ",emo3$cc[tt]))
  print(c(sort(bibu),round(mean(bibu),2)))
  print(paste0(emo3$ProcEmo[vs]," ",emo3$emotion_shown[vs]," ",emo3$emotion_level[vs]," ",emo3$cc[vs]))      
  print(c(sort(bubi),round(mean(bubi),2)))
  print(emo3$dif_da_sec[tt])
  
  }
}

emo3$dsmed=rep(NA, dim(emo3)[1])
emo3$dsstat=rep(NA, dim(emo3)[1])
for (tt in 1:dim(emo3)[1]){
  if (emo3$right_emo[tt]==1){
    
    bibu=subset(x = three_rate$Rating,subset = three_rate$ProcEmo==emo3$ProcEmo[tt]&three_rate$emotion_shown==emo3$emotion_shown[tt]&three_rate$emotion_level==emo3$emotion_level[tt]&three_rate$cc==emo3$cc[tt])
    positions=which( emo3$right_emo==0 & emo3$emotion_shown==emo3$emotion_shown[tt] & emo3$emotion_level==emo3$emotion_level[tt] & emo3$cc==emo3$cc[tt])
    vs=positions[which(emo3$relrates[positions]==max(emo3$relrates[positions]))][1]##[1] choosing one of the two possible comparisons
    bubi=subset(x = three_rate$Rating,subset = three_rate$ProcEmo==emo3$ProcEmo[vs]&three_rate$emotion_shown==emo3$emotion_shown[vs]&three_rate$emotion_level==emo3$emotion_level[vs]&three_rate$cc==emo3$cc[vs])
    #print(paste0(mean(bibu)," ",mean(bubi)))
    if (length(bibu)!=11|length(bubi)!=11)
      print("errato numero di osservazioni")
    emo3$dsmed[tt]=round(wilcox.test(bibu,bubi,paired = TRUE, conf.int = T)$estimate,2)##two tails
    emo3$dsstat[tt]=round(wilcox.test(bibu,bubi,paired = TRUE, conf.int = T)$statistic,1)
    print(paste0(emo3$ProcEmo[tt]," ",emo3$emotion_shown[tt]," ",emo3$emotion_level[tt]," ",emo3$cc[tt]))
    print(c(sort(bibu),round(mean(bibu),2)))
    print(paste0(emo3$ProcEmo[vs]," ",emo3$emotion_shown[vs]," ",emo3$emotion_level[vs]," ",emo3$cc[vs]))      
    print(c(sort(bubi),round(mean(bubi),2)))
    print(emo3$dif_da_sec[tt])
    
  }
}


##specification of coherent emotion in three_rate dataset
three_right_emo <- read.csv("C:/Users/capergi/Dropbox/Moskon/moebius/list_coherent_emotion.csv")
three_rate$right_emo <- three_right_emo[,2]


###test per emozione right emotion
for (li in unique(three_rate$emotion_shown )){
  if (li!="neutral"){
bibu=subset(x = three_rate$Rating,subset=three_rate$cc=="Moebius"&three_rate$right_emo==1&three_rate$emotion_shown==li)
bubi=subset(x = three_rate$Rating,subset=three_rate$cc=="Control"&three_rate$right_emo==1&three_rate$emotion_shown==li)
print(c(li,round(mean(bibu),3),round(mean(bubi),3),round(wilcox.test(bibu,bubi)$p.value,3)))}}

###test per emozione wrong emotion
for (li in unique(three_rate$emotion_shown )){
  if (li!="neutral"){
    bibu=subset(x = three_rate$Rating,subset=three_rate$cc=="Moebius"&three_rate$right_emo!=1&three_rate$emotion_shown==li)
    bubi=subset(x = three_rate$Rating,subset=three_rate$cc=="Control"&three_rate$right_emo!=1&three_rate$emotion_shown==li)

    print(c(li,round(mean(bibu),3),round(mean(bubi),3),round(wilcox.test(bibu,bubi)$p.value,3)))}}

three_rate$neutral_rate=NA
three_rate$relative_rating=NA
for ( i in 1:dim(three_rate)[1]){
  three_rate$neutral_rate[i]=  mean(subset(three_rate,subset =three_rate$Subject==three_rate$Subject[i]&
                                             three_rate$cc==three_rate$cc[i]&
                                             three_rate$emotion_shown=="neutral"&
                                             three_rate$ProcEmo==three_rate$ProcEmo[i])$Rating)
  three_rate$relative_rating[i]=three_rate$Rating[i]-three_rate$neutral_rate[i]
  
}


###test comparison between neutrals
for (pro in unique(three_rate$ProcEmo )){
  bibu=subset(x = three_rate$Rating,subset=three_rate$cc=="Moebius"&three_rate$emotion_shown=="neutral"&three_rate$ProcEmo==pro)
  bubi=subset(x = three_rate$Rating,subset=three_rate$cc=="Control"&three_rate$emotion_shown=="neutral"&three_rate$ProcEmo==pro)
  wil_test <- wilcox.test(bibu,bubi,paired = T, conf.int = T)
  print(c(pro,round(mean(bibu),3),round(mean(bubi),3),round(wil_test$p.value,3), wil_test$statistic, wil_test$parameter, wil_test$conf.int ))}




##### second half

emo_conmoe=emo3[emo3$cc!="Mask",]
conmoe_rate=three_rate[three_rate$cc!="Mask",]

levels(emo_conmoe$cc)
emo_conmoe$cc=droplevels(emo_conmoe$cc)
levels(emo_conmoe$cc)


#Define new labels for presentation
levels(emo_conmoe$ProcEmo)
levels(emo_conmoe$emotion_shown)
emo_conmoe$names_block=revalue(emo_conmoe$ProcEmo, c("AngerProc" = "Anger Block",
                                                     "DisgustProc"= "Disgust Block",
                                                     "FearProc" = "Fear Block" ,
                                                     "HappynessProc" = "Happiness Block" ,
                                                     "SadnessProc" = "Sadness Block"   ,
                                                     "SurpriseProc" = "Surprise Block"))
emo_conmoe$names_emo=revalue(emo_conmoe$emotion_shown, 
                             c("afraid"="Fearful",
                               "angry" ="Angry",    
                               "disgusted" = "Disgusted",
                               "happy" = "Happy",    
                               "neutral"= "Neutral",   
                               "sad" ="Sad",      
                               "surprised"="Surprised"))
emo_conmoe$lab_emo=revalue(emo_conmoe$emotion_shown, 
                           c("afraid"="Fea",
                             "angry" ="Ang",    
                             "disgusted" = "Dis",
                             "happy" = "Hap",    
                             "neutral"= "Neu",   
                             "sad" ="Sad",      
                             "surprised"="Sur"))


# Plots of relative measures
###Difference of coherent stimuli from neutral stimuli. (Figure 2 in last update) 
ggplot(data=emo_conmoe[which(emo_conmoe$right_emo==T),], aes(x=emotion_level, y=relrates)) +
  geom_line(aes(color=cc, group=cc), size=1) +  geom_point(data=emo_conmoe[emo_conmoe$test_noneut2<0.05&emo_conmoe$right_emo==T,], aes(color=cc),size=2 )+  #geom_point(size=2)+
  facet_grid(~names_emo,scales="free")+
  scale_color_manual(values = c("darkgrey", "black"))+
  #scale_linetype_manual(values=c("longdash", "dotted"))+
  guides(colour=guide_legend(title="Rated\nEmotional\nContent"))+
  scale_x_discrete("Expression Intensity") +
  scale_y_continuous("Average Rating",limits=c(-1,6)) +
  #???theme_bw()+
  ggtitle("Faces Rated")




###Difference of coherent stimuli from neutral stimuli. (Figure 2 in last update) 
ggplot(data=emo_conmoe[which(emo_conmoe$right_emo==T),], aes(x=emotion_level, y=relrates, group=names_block, colour=names_block)) +
  geom_line() +  geom_point(data=emo_conmoe[emo_conmoe$test_noneut2<0.05&emo_conmoe$right_emo==T,],size=3)+
  facet_grid(cc~names_emo,scales="free")+
  guides(colour=guide_legend(title="Rated\nEmotional\nContent"))+
  scale_x_discrete("Expression Intensity") +
  scale_y_continuous("Average Rating",limits=c(-3,6)) +
  ggtitle("Faces Rated")


###Difference of incoherent stimuli from neutral stimuli. (Figure 3 in last update)
ggplot(data=emo_conmoe[which(emo_conmoe$right_emo==F),], aes(x=emotion_level, y=relrates, group=names_block, colour=names_block)) +
  geom_line() +  geom_point(data=emo_conmoe[emo_conmoe$test_noneut<0.051&emo_conmoe$right_emo==F,],size=3)+
  facet_grid(cc~names_emo,scales="free")+
  guides(colour=guide_legend(title="Rated\nEmotional\nContent"))+
  scale_x_discrete("Expression Intensity") +
  scale_y_continuous("Average Rating",limits=c(-3,6)) +
  ggtitle("Faces Rated")


#Difference of the coherent stimuli from the second. (Figure 4 in last update)
ggplot(data=emo_conmoe, aes(x=emotion_level, y=relrates, group=names_block, colour=names_block)) +
  geom_line() +  geom_point(data=emo_conmoe[emo_conmoe$dif_da_sec<0.05,],size=3)+
  facet_grid(cc~names_emo,scales="free")+
  guides(colour=guide_legend(title="Rated\nEmotional\nContent"))+
  scale_x_discrete("Expression Intensity") +
  scale_y_continuous("Average Rating",limits=c(-3,6)) +
  ggtitle("Faces Rated") 


# check on p-value <0.05 in various comparisons
sign_coe_neut=emo_conmoe%>% filter(right_emo==T& test_noneut2<0.051)  %>%  select(cc, names_block, names_emo, emotion_level, test_noneut2, nnmed2,nnstat2)

sign_incoe_neut=emo_conmoe %>% filter(right_emo==F& test_noneut<0.051)  %>%  select(cc, names_block, names_emo, emotion_level, test_noneut, nnmed,nnstat)

sign_coe_sec=emo_conmoe%>% filter(right_emo==T& dif_da_sec<0.051)  %>%  select(cc, names_block, names_emo, emotion_level, dif_da_sec, dsmed,dsstat)



###Data prep for MDS
colnames(emo2)[1:4]=c("cc","ProcEmo","emotion_shown","emotion_level")

conmoe2=emo2[emo2$cc!="Mask",]
rat_nomas=three_rate[three_rate$cc!="Mask",]


data_mds=select(rat_nomas, Subject, targetimage,
                ProcEmo, EmotionLevel, Rating, cc, emotion_shown,
                emotion_level,right_emo,relative_rating, neutral_rate)



data_mds$names_block=revalue(data_mds$ProcEmo, c("AngerProc" = "Anger Block",
                                                 "DisgustProc"= "Disgust Block",
                                                 "FearProc" = "Fear Block" ,
                                                 "HappynessProc" = "Happiness Block" ,
                                                 "SadnessProc" = "Sadness Block"   ,
                                                 "SurpriseProc" = "Surprise Block"))
data_mds$names_emo=revalue(data_mds$emotion_shown, 
                           c("afraid"="Fearful",
                             "angry" ="Angry",    
                             "disgusted" = "Disgusted",
                             "happy" = "Happy",    
                             "neutral"= "Neutral",   
                             "sad" ="Sad",      
                             "surprised"="Surprised"))

data_mds$lab_emo=revalue(data_mds$emotion_shown, 
                         c("afraid"="Fea",
                           "angry" ="Ang",    
                           "disgusted" = "Dis",
                           "happy" = "Hap",    
                           "neutral"= "Neu",   
                           "sad" ="Sad",      
                           "surprised"="Sur"))

data_mds=mutate(data_mds,  Stimulus=str_c(data_mds$lab_emo, data_mds$emotion_level,sep=" "))

data_mds=mutate(data_mds,  step2=str_c(data_mds$cc,data_mds$Subject,data_mds$names_block,sep=" "))



simplified <- data_mds %>% 
  group_by(cc, Subject, Stimulus, names_block) %>%
  summarise(Avg_Rating=mean(Rating))%>%
  spread(key = Stimulus,value = Avg_Rating) %>% 
  ungroup()


ctrl_mds=simplified %>% filter(cc=="Control")
moeb_mds=simplified %>% filter(cc=="Moebius")


this_fit <- smacofSym(dist(t(ctrl_mds[,-(1:3)])), ndim = 2, type = "interval")     
fit_plot <- bind_cols(sbj=colnames(ctrl_mds)[-(1:3)], group= substr(colnames(ctrl_mds)[-(1:3)],1,3) , this_fit$conf) %>% 
  mutate(exp= "Control")
fitton <- fit_plot
this_fit <- smacofSym(dist(t(moeb_mds[,-(1:3)])), ndim = 2, type = "interval")     
fit_plot <- bind_cols(sbj=colnames(moeb_mds)[-(1:3)], group= substr(colnames(moeb_mds)[-(1:3)],1,3) , this_fit$conf) %>% 
  mutate(exp= "Moebius")
fitton <- bind_rows(fitton, fit_plot)





ggplot(fitton, aes(x= D1, y= D2, color= group))+
  geom_point(size=2, show.legend = F)+
  geom_text_repel(aes(x = D1, y = D2, label = sbj),
                  box.padding = unit(0.1, "lines"), force = 5,
                  segment.color = NA, size=5,show.legend = F) +
  scale_color_discrete(name="Emotion")+
  facet_grid(.~exp)+
  ggtitle("Maps of Stimuli")+
  scale_y_continuous(limits= c(-1,1),breaks=seq(-1,1,0.4))+
  scale_x_continuous(limits= c(-1,1),breaks=seq(-1,1,0.4))


###code for test on permutations
try <- t(moeb_mds[,-(1:3)])
this_fit <- smacofSym(dist(try), ndim = 3, type = "interval")    
perma <- permtest(object = this_fit, data = t(try), 
                  nrep = 1000, method.dat = "euclidean", verbose= F)




