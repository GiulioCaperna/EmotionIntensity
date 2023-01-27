#----------------------------------------------------------------#
#
# Analysis script for the paper:
# "The intensity of emotion: Altered motor simulation impairs 
#   processing of facial expressions in congenital facial palsy"
#
#----------------------------------------------------------------#

#---------------------- LIBRARIES -------------------------------
library(plyr)
library(tidyverse)
library(FactoMineR)
library(ggpubr)
library(smacof)
library(readxl)
library(ggrepel)


#--------------------- P1 Component -----------------------------

load("datalarge.RData")
##The code for single case analysis is in "Single Case Analysis Code.R

meanrates=tapply(three_rate$Rating,paste(three_rate$cc,three_rate$`Procedure[Block]`, three_rate$EmotionLevel),mean)

sdrates=tapply(three_rate$Rating,paste(three_rate$cc,three_rate$`Procedure[Block]`, three_rate$EmotionLevel),sd)
quart1rates=tapply(three_rate$Rating,paste(three_rate$cc,three_rate$`Procedure[Block]`, three_rate$EmotionLevel),function(x) quantile(x,0.25))
quart3rates=tapply(three_rate$Rating,paste(three_rate$cc,three_rate$`Procedure[Block]`, three_rate$EmotionLevel),function(x) quantile(x,0.75))
splitted=strsplit(names(meanrates),split = " ")


#creation of the main dataset
emo2=data.frame(t(data.frame(splitted)))

emo2$meanrates=meanrates
emo2$sdrates=sdrates
emo2$quart1rates=quart1rates
emo2$quart3rates=quart3rates


####repeated for larger dataset
### for the reader: this section was needed to adapt the old code to the new version with more subjects
### we preferred to stick to the previous code scheme to reduce the possible typos and simplify the read of the reviewer in case of them being the same person
meanrates_lrg=tapply(three_rate$Rating,paste(three_rate$cc, three_rate$`Procedure[Block]`, three_rate$EmotionLevel,three_rate$id_n),mean)

sdrates_lrg=tapply(three_rate$Rating,paste(three_rate$cc,three_rate$`Procedure[Block]`, three_rate$EmotionLevel,three_rate$id_n),sd)
quart1rates_lrg=tapply(three_rate$Rating,paste(three_rate$cc,three_rate$`Procedure[Block]`, three_rate$EmotionLevel,three_rate$id_n),function(x) quantile(x,0.25))
quart3rates_lrg=tapply(three_rate$Rating,paste(three_rate$cc,three_rate$`Procedure[Block]`, three_rate$EmotionLevel,three_rate$id_n),function(x) quantile(x,0.75))
splitted_lrg=strsplit(names(meanrates_lrg),split = " ")

emo2_lrg=data.frame(t(data.frame(splitted_lrg)))

emo2_lrg$meanrates=meanrates_lrg
emo2_lrg$sdrates=sdrates_lrg
emo2_lrg$quart1rates=quart1rates_lrg
emo2_lrg$quart3rates=quart3rates_lrg
####end of repetition


asked <- unique(emo2$X2)
shown <- unique(emo2$X3)[c(2,3,1,4,6,7)]

coherence_df <- data.frame(asked,shown )

###subtraction of mean value of neutral face
emo2$relrates=0
emo2$neu_rates=0
for (i in unique(emo2$X2)){
  print(i)
  for (j in unique(emo2$X1)){
    print(j)
    subgroup= which(emo2$X1==j&emo2$X2==i)
    print(length(subgroup))
    rel_neu= which(emo2$X1==j&emo2$X2==i&emo2$X4==0)
    print(length(rel_neu))
    emo2$relrates[subgroup]=emo2$meanrates[subgroup]-emo2$meanrates[rel_neu]
    emo2$neu_rates[subgroup]=emo2$meanrates[rel_neu]
  }
  
}

#### Chang name column "Proc"
names(three_rate)[9]="ProcEmo"


## Pairwise test of difference from neutral 
library(rcompanion)

#%#

emo3=emo2[(emo2$X3!="neutral"),]
emo3$test_noneut=rep(NA, dim(emo3)[1])
emo3$eff_size_noneu = NA
emo3$zeta_noneu = NA
colnames(emo3)[1:4]=c("cc","ProcEmo","emotion_shown","emotion_level")


for (tt in 1:dim(emo3)[1]){
  bibu <-  three_rate %>% 
    filter(ProcEmo== emo3$ProcEmo[tt]) %>% 
    filter(emotion_shown== emo3$emotion_shown[tt]) %>% 
    filter(emotion_level== emo3$emotion_level[tt]) %>% 
    filter(cc== emo3$cc[tt]) %>% 
    select(Rating) %>% 
    pull()
  
  bubi <- three_rate %>% 
    filter(ProcEmo== emo3$ProcEmo[tt]) %>% 
    filter(emotion_level== 0) %>% 
    filter(cc== emo3$cc[tt]) %>% 
    select(Rating) %>% 
    pull()
  
  
  #print(c(length(bibu), length(bubi)))
  
  emo3$test_noneut[tt]=round(wilcox.test(bubi,bibu)$p.value,3)
  
  nbibu=length(bibu)
  nbubi=length(bubi)
  pretest <- data.frame(x= c(bibu,bubi), g=c(rep("Emo",nbibu), rep("Neutral",nbubi)))
  
  wil.t2 <- wilcoxonR(pretest$x, pretest$g)
  
  emo3$zeta_noneu[tt]= wil.t2* sqrt(nbibu+nbubi)
  emo3$eff_size_noneu[tt] = wil.t2^2
  
}


emo3$nnmed=rep(NA, dim(emo3)[1])
emo3$nnstat=rep(NA, dim(emo3)[1])

for (tt in 1:dim(emo3)[1]){
  bibu <-  three_rate %>% 
    filter(ProcEmo== emo3$ProcEmo[tt]) %>% 
    filter(emotion_shown== emo3$emotion_shown[tt]) %>% 
    filter(emotion_level== emo3$emotion_level[tt]) %>% 
    filter(cc== emo3$cc[tt]) %>% 
    select(Rating) %>% 
    pull()
  
  bubi <- three_rate %>% 
    filter(ProcEmo== emo3$ProcEmo[tt]) %>% 
    filter(emotion_level== 0) %>% 
    filter(cc== emo3$cc[tt]) %>% 
    select(Rating) %>% 
    pull()
  
  
  emo3$nnmed[tt]=round(wilcox.test(bubi,bibu,conf.int = T)$estimate,2)
  emo3$nnstat[tt]=round(wilcox.test(bubi,bibu,conf.int = T)$statistic,1)
}


###### onetail test
emo3$test_noneut2=rep(NA, dim(emo3)[1])
emo3$eff_size_noneu2 = NA
emo3$zeta_noneu2 = NA
emo3$nnmed2=rep(NA, dim(emo3)[1])
emo3$nnstat2=rep(NA, dim(emo3)[1])

for (tt in 1:dim(emo3)[1]){
  bibu <-  three_rate %>% 
    filter(ProcEmo== emo3$ProcEmo[tt]) %>% 
    filter(emotion_shown== emo3$emotion_shown[tt]) %>% 
    filter(emotion_level== emo3$emotion_level[tt]) %>% 
    filter(cc== emo3$cc[tt]) %>% 
    select(Rating) %>% 
    pull()
  
  bubi <- three_rate %>% 
    filter(ProcEmo== emo3$ProcEmo[tt]) %>% 
    filter(emotion_level== 0) %>% 
    filter(cc== emo3$cc[tt]) %>% 
    select(Rating) %>% 
    pull()
  
  if ( (length(bibu)!=11|length(bubi)!=33)&(length(bibu)!=33|length(bubi)!=99))
    print("wrong observations number")
  emo3$nnmed2[tt]=round(wilcox.test(bubi,bibu,alternative="less",conf.int = T)$estimate,2)
  emo3$nnstat2[tt]=round(wilcox.test(bubi,bibu,alternative="less",conf.int = T)$statistic,1)
  piva=wilcox.test(bubi,bibu,alternative="less")$p.value
  emo3$test_noneut2[tt]=round(piva,3)
  
  nbibu=length(bibu)
  nbubi=length(bubi)
  pretest <- data.frame(x= c(bibu,bubi), g=c(rep("Emo",nbibu), rep("Neutral",nbubi)))
  
  wil.t2 <- wilcoxonR(pretest$x, pretest$g)
  
  emo3$zeta_noneu2[tt]= sign(wil.t2)*qnorm(piva, lower.tail = F)
  emo3$eff_size_noneu2[tt] = wil.t2^2
  
}###

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

table(emo3$ProcEmo, emo3$emotion_shown, emo3$right_emo)

## pvalues of coherent different from second
emo3$dif_da_sec = 1
emo3$eff_size_sec = NA
emo3$zeta_sec = NA


for (tt in 1:dim(emo3)[1]){
  if (emo3$right_emo[tt]==1){
    
    bibu <-  three_rate %>% 
      filter(ProcEmo== emo3$ProcEmo[tt]) %>% 
      filter(emotion_shown== emo3$emotion_shown[tt]) %>% 
      filter(emotion_level== emo3$emotion_level[tt]) %>% 
      filter(cc== emo3$cc[tt]) %>% 
      select(Rating) %>% 
      pull()
    
    positions=which(emo3$right_emo==0 & emo3$emotion_shown==emo3$emotion_shown[tt] & emo3$emotion_level==emo3$emotion_level[tt] & emo3$cc==emo3$cc[tt])
    vs=positions[which(emo3$relrates[positions]==max(emo3$relrates[positions]))][1]##[1] because two alternetives are possible 
    
    bubi <-  three_rate %>% 
      filter(ProcEmo== emo3$ProcEmo[vs]) %>% 
      filter(emotion_shown== emo3$emotion_shown[vs]) %>% 
      filter(emotion_level== emo3$emotion_level[vs]) %>% 
      filter(cc== emo3$cc[vs]) %>% 
      select(Rating) %>% 
      pull()
    #print(paste0(mean(bibu)," ",mean(bubi)))
    if ((length(bibu)!=11|length(bubi)!=11)&(length(bibu)!=33|length(bubi)!=33))
      print("errato numero di osservazioni")
    emo3$dif_da_sec[tt]=round(wilcox.test(bibu,bubi,paired = TRUE)$p.value,3)##two-tails
    
    
    n=length(bibu)##equal length
    pretest <- data.frame(x= c(bibu,bubi), g=c(rep("Right",n), rep("Second",n)))
    
    wil.t2 <- wilcoxonPairedR(pretest$x, pretest$g)
    
    emo3$zeta_sec[tt]= wil.t2* sqrt(n)
    emo3$eff_size_sec[tt] = wil.t2^2
    
    
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
    
    
    bibu <-  three_rate %>% 
      filter(ProcEmo== emo3$ProcEmo[tt]) %>% 
      filter(emotion_shown== emo3$emotion_shown[tt]) %>% 
      filter(emotion_level== emo3$emotion_level[tt]) %>% 
      filter(cc== emo3$cc[tt]) %>% 
      select(Rating) %>% 
      pull()
    
    positions=which( emo3$right_emo==0 & emo3$emotion_shown==emo3$emotion_shown[tt] & emo3$emotion_level==emo3$emotion_level[tt] & emo3$cc==emo3$cc[tt])
    vs=positions[which(emo3$relrates[positions]==max(emo3$relrates[positions]))][1]##[1] choosing one of the two possible comparisons
    
    bubi <-  three_rate %>% 
      filter(ProcEmo== emo3$ProcEmo[vs]) %>% 
      filter(emotion_shown== emo3$emotion_shown[vs]) %>% 
      filter(emotion_level== emo3$emotion_level[vs]) %>% 
      filter(cc== emo3$cc[vs]) %>% 
      select(Rating) %>% 
      pull()
    
    if ((length(bibu)!=11|length(bubi)!=11)&(length(bibu)!=33|length(bubi)!=33))
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

three_rate <- three_rate %>% 
  mutate(right_emo= 0) %>% 
  mutate(right_emo= case_when(
    ProcEmo == coherence_df[1,1]& emotion_shown == coherence_df[1,2] ~ 1,   
    ProcEmo == coherence_df[2,1]& emotion_shown == coherence_df[2,2] ~ 1,
    ProcEmo == coherence_df[3,1]& emotion_shown == coherence_df[3,2] ~ 1,   
    ProcEmo == coherence_df[4,1]& emotion_shown == coherence_df[4,2] ~ 1,
    ProcEmo == coherence_df[5,1]& emotion_shown == coherence_df[5,2] ~ 1,   
    ProcEmo == coherence_df[6,1]& emotion_shown == coherence_df[6,2] ~ 1
  ))

### relative rating for first results table
three_rate$neutral_rate=NA
three_rate$relative_rating=NA
for ( i in 1:dim(three_rate)[1]){
  three_rate$neutral_rate[i]=  mean(subset(three_rate,subset =three_rate$Subject==three_rate$Subject[i]&
                                             three_rate$cc==three_rate$cc[i]&
                                             three_rate$emotion_shown=="neutral"&
                                             three_rate$ProcEmo==three_rate$ProcEmo[i])$Rating)
  three_rate$relative_rating[i]=three_rate$Rating[i]-three_rate$neutral_rate[i]
  
}

####exact code of table, tests between relative ratings
for (li in unique(three_rate$emotion_shown )){
  if (li!="neutral"){
    
    bibu <-  three_rate %>%
      filter(right_emo== 1) %>%
      filter(emotion_shown== li) %>%
      filter(cc== "Moebius") %>%
      select(relative_rating) %>%
      pull()
    bubi <-  three_rate %>%
      filter(right_emo== 1) %>%
      filter(emotion_shown== li) %>%
      filter(cc== "Control") %>%
      select(relative_rating) %>%
      pull()
    
    n1= length(bibu)
    n2= length(bubi)
    N=n1+n2
    
    print(c(li))
    
    wil.t <- wilcox.test(bibu,bubi,paired = F,conf.int = TRUE)
    print(wil.t$estimate)
    print(wil.t$conf.int[1:2])
    print(wil.t$p.value)
    Zwil= qnorm(wil.t$p.value/2)
    print("Zwil")
    print(Zwil)
    rwil <- Zwil/sqrt(N)
    #print("rwil")
    #print(rwil)
    print(paste0("r is =", round(rwil,3)))
    print(paste0("r-squared is =", round(rwil^2,3)))
    
    print(wilcoxonR(bubi, bibu,paired = F))
    
    print(" ")
    
  }}

#### tests between Ratings (absolute)
for (li in unique(three_rate$emotion_shown )){
  if (li!="neutral"){
    
    bibu <-  three_rate %>%
      filter(right_emo== 1) %>%
      filter(emotion_shown== li) %>%
      filter(cc== "Moebius") %>%
      select(Rating) %>%
      pull()
    bubi <-  three_rate %>%
      filter(right_emo== 1) %>%
      filter(emotion_shown== li) %>%
      filter(cc== "Control") %>%
      select(Rating) %>%
      pull()
    
    n1= length(bibu)
    n2= length(bubi)
    N=n1+n2
    
    print(c(li))
    
    wil.t <- wilcox.test(bibu,bubi,paired = F,conf.int = TRUE)
    print(wil.t$estimate)
    print(round(wil.t$conf.int[1:2]),3)
    print(wil.t$p.value)
    Zwil= qnorm(wil.t$p.value/2)
    print("Zwil")
    print(Zwil)
    rwil <- Zwil/sqrt(N)
    print(paste0("r is =", round(rwil,3)))
    print(paste0("r-squared is =", round(rwil^2,3)))
    
    print(wilcoxonR(bubi, bibu,paired = F))
    
    print(" ")
    
  }}


###test comparison between neutrals
for (pro in unique(three_rate$ProcEmo )){
  
  bibu <-  three_rate %>%
    filter(ProcEmo== pro) %>%
    filter(emotion_shown== "neutral") %>%
    filter(cc== "Moebius") %>%
    select(Rating) %>%
    pull()
  bubi <-  three_rate %>%
    filter(ProcEmo== pro) %>%
    filter(emotion_shown== "neutral") %>%
    filter(cc== "Control") %>%
    select(Rating) %>%
    pull()
  
  print(summary(bibu))
  print(summary(bubi))
  n1= length(bibu)
  n2= length(bubi)
  N=n1+n2
  
  wil_test <- wilcox.test(bubi,bibu,paired = F, conf.int = T)
  print(pro)
  print(wil_test$estimate)
  print(wil_test$conf.int[1:2])
  print(c(round(wil_test$p.value,3))) 
  print(c(round(wil_test$statistic,3), wil_test$parameter, round(wil_test$conf.int,3) ))
  Zwil= qnorm(wil_test$p.value/2)
  print(paste0("Z=", Zwil))
  rwil <- Zwil/sqrt(N)
  #print("rwil")
  #print(rwil)
  print(paste0("r-squared is =", round(rwil^2,3)))
  print(" ")
}


##### Analysis on stimuli

emo_conmoe=emo3[emo3$cc!="Mask",]
conmoe_rate=three_rate[three_rate$cc!="Mask",]


#Define new labels for visualisation

emo_conmoe$ProcEmo <- as.factor(emo_conmoe$ProcEmo)
emo_conmoe$emotion_shown <- as.factor(emo_conmoe$emotion_shown)


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

library(ggthemes)

# Plots of relative measures
emo_conmoe$cc <-  recode_factor(emo_conmoe$cc, Control= "Control", Moebius= "MBS")

###Preparation of figure for comparison on the primary emotion
dat_fig4 <- emo_conmoe %>% 
  distinct(cc,ProcEmo, .keep_all = T) %>% 
  mutate(emotion_shown = as.factor("Neutral")) %>% 
  mutate(emotion_level = as.factor(0)) %>% 
  mutate(meanrates = neu_rates) %>% 
  mutate(right_emo = 0) %>% 
  mutate(test_noneut = 0.05) %>% 
  mutate(names_emo = "Neutral") %>% 
  select(c(cc:neu_rates, test_noneut,right_emo, names_emo,names_block))

emo_conmoe2 <- bind_rows(emo_conmoe, dat_fig4)

library(cowplot)
library(RColorBrewer)
pal_vec <- brewer.pal(n = 8, name = "Set1")

###  Mean rating of coherent stimuli from neutral stimuli. (Figure 5 in January 2023 Update) 
emo_conmoe <- emo_conmoe %>% 
  mutate(minimum= meanrates-sdrates,maximum= meanrates+sdrates )


ggplot(data=emo_conmoe[which(emo_conmoe$right_emo==T),], aes(x=emotion_level, y=meanrates)) +
  geom_line(aes(y= neu_rates, colour=cc, group=cc), size=1, alpha=0.5) +  
  geom_line(aes(color=cc, group=cc), size=1) +  
  geom_point(data=emo_conmoe[emo_conmoe$test_noneut2<0.05&emo_conmoe$right_emo==T,], aes(color=cc),size=2 )+
  
  facet_grid(~names_emo,scales="free")+
  scale_color_manual(values = c("#80146E", "#879FDB"))+
  guides(colour=guide_legend(title=""))+
  scale_x_discrete("Level of emotion intensity") +
  scale_y_continuous("Average Rating",limits=c(0,10), breaks = c(0,2,4,6,8,10)) +
  theme_minimal_hgrid()+
  theme(plot.title= element_text(face= "bold"),
        #axis.title.x = element_text(face= "bold"),
        axis.text.x = element_text(size= 10),
        #axis.title.y = element_text(face= "bold"),
        axis.text.y = element_text(size= 10)        
  )+
  ggtitle("Faces Rated")

ggsave(filename= "fig5_lrg_coherent.tiff", units="in", width=8, height=4,dpi=300, compression = 'lzw')



###without Neutral but with absolute means
###mean of the ratings (not in the paper anymore)
# ggplot(data=emo_conmoe[which(emo_conmoe$right_emo==F),], aes(x=emotion_level, y=meanrates, group=names_block, colour=names_block)) +
#   geom_line() +  geom_point(data=emo_conmoe[emo_conmoe$test_noneut<0.051&emo_conmoe$right_emo==F,],size=2)+
#   #geom_point(data=emo_conmoe2[emo_conmoe2$test_noneut<0.051&emo_conmoe2$right_emo==F&emo_conmoe2$names_emo=="Neutral",],size=2, shape=15)+
#   facet_grid(cc~names_emo,scales="free")+
#   guides(colour=guide_legend(title=""))+
#   scale_x_discrete("Level of emotion intensity") +
#   scale_y_continuous("Average Rating",limits=c(0,6.5), breaks=c(0,2,4,6)) +
#   theme_minimal_hgrid()+
#   scale_colour_brewer(palette = "Set1")+
#   theme(plot.title= element_text(face= "bold"),
#         #axis.title.x = element_text(face= "bold"),
#         axis.text.x = element_text(size= 10),
#         #axis.title.y = element_text(face= "bold"),
#         axis.text.y = element_text(size= 10),
#         strip.text.y = element_text(face= "bold"),
#         panel.spacing.y = unit(1.5,units = "lines")
#   )+
#   ggtitle("Faces Rated")
# 
# ggsave(filename= "nofig1_lrg_incoherent_withoutneu.tiff", units="in", width=8, height=5,dpi=300, compression = 'lzw')


###indipendent neutral (not in the paper anymore)
###Difference of incoherent stimuli from neutral stimuli. 
# ggplot(data=emo_conmoe2[which(emo_conmoe2$right_emo==F),], aes(x=emotion_level, y=meanrates, group=names_block, colour=names_block)) +
#   geom_point(data=emo_conmoe2[emo_conmoe2$right_emo==F&emo_conmoe2$names_emo=="Neutral",],size=2, shape=15)+
#   facet_grid(.~cc,scales="free")+
#   guides(colour=guide_legend(title=""))+
#   scale_x_discrete("Level of emotion Intensity") +
#   scale_y_continuous("Average Rating",limits=c(2,4),breaks = c(2,3,4)) +
#   theme_minimal_hgrid()+
#   ggtitle("Evaluation of neutral faces")+
#   scale_colour_brewer(palette = "Set1")+
#   theme(plot.title= element_text(face= "bold"),
#         #        axis.title.x = element_text(face= "bold"),
#         axis.text.x = element_text(size= 10),
#         #       axis.title.y = element_text(face= "bold"),
#         axis.text.y = element_text(size= 10),
#         strip.text.y = element_text(face= "bold"),
#         panel.spacing.y = unit(1.5,units = "lines")
#   )
# 
# ggsave(filename= "nofig2_lrg_indy neut.tiff", units="in", width=4, height=3,dpi=300, compression = 'lzw')
# 

# Suggestions from the last reviewer
# Test of interaction between baseline rating (Neutral) and Emotions' Rating
summary(lm(Rating~cc*ProcEmo,data = conmoe_rate, subset = emotion_shown== "neutral"))

summary(lm(Rating~ProcEmo,data = conmoe_rate, subset = emotion_shown== "neutral"&id_n %in%c("m")  ))
conmoe_rate$isemo <- as.factor(conmoe_rate$emotion_level!=0)
three_rate$isemo <- as.factor(three_rate$emotion_level!=0)
three_rate$right_emo_ <- as.factor(1- 1*is.na(three_rate$right_emo)+ 1*(three_rate$emotion_level==0) )

###
##regression analysis with interactions
###
library(lsmeans)
library(phia)

##check group means 
three_rate %>% 
  filter(right_emo_==1&cc!="Mask") %>% 
  group_by(isemo, cc) %>% 
  summarise(mean(Rating)) 

#create dataset for linear regression
three_rate_forse <- three_rate %>% 
  filter(cc != "Mask") %>% 
  mutate(cc=as.factor(cc))



fit_1cohe <- lm(Rating~isemo*cc,data = three_rate_forse, subset = (right_emo_==1) )
summary(fit_1cohe)

emoji <- unique(as.character(three_rate_forse$emotion_shown))

three_rate_forse$emotion_shown2= factor(as.character(three_rate_forse$emotion_shown), 
                                        levels = c("neutral",emoji[-5]))



testInteractions(fit_1cohe, pairwise = "cc", fixed="isemo")
testInteractions(fit_1cohe, pairwise = "isemo", fixed="cc", adjustment = "fdr")

car::Anova(fit_1cohe, type="III") 

levels(three_rate_forse$cc)

leastsquare = lsmeans(fit_1cohe,c("cc","isemo"))


Ctrl_MoeF = list(Red_line1   = c(1, -1,0,0))
Ctrl_MoeT = list(Red_line1   = c(0,0,1, -1))
F_T_Ctrl = list(Red_line1   = c(1,0, -1,0))
F_T_Moe = list(Red_line1   = c(0,1,0, -1))


Test = contrast(leastsquare, F_T_Ctrl)

test(Test, joint=TRUE)



##All interactions together with neutral as reference
fit_1cohe_top <- lm(Rating~emotion_shown2*cc, data = three_rate_forse, 
                    subset = (right_emo_==1&cc!="Mask") )
summary(fit_1cohe_top)

###Models considering only one emaotion shown VS neutral
fit_1cohe_sad <- lm(Rating~isemo*cc,data = three_rate, 
                    subset = (right_emo_==1&cc!="Mask")&(emotion_shown=="neutral"|emotion_shown=="sad") )
summary(fit_1cohe_sad)

fit_1cohe_hap <- lm(Rating~isemo*cc,data = three_rate, 
                    subset = (right_emo_==1&cc!="Mask")&(emotion_shown=="neutral"|emotion_shown=="happy") )
summary(fit_1cohe_hap)

fit_1cohe_ang <- lm(Rating~isemo*cc,data = three_rate, 
                    subset = (right_emo_==1&cc!="Mask")&(emotion_shown=="neutral"|emotion_shown=="angry") )
summary(fit_1cohe_ang)

fit_1cohe_afr <- lm(Rating~isemo*cc,data = three_rate, 
                    subset = (right_emo_==1&cc!="Mask")&(emotion_shown=="neutral"|emotion_shown=="afraid") )
summary(fit_1cohe_afr)

fit_1cohe_dis <- lm(Rating~isemo*cc,data = three_rate, 
                    subset = (right_emo_==1&cc!="Mask")&(emotion_shown=="neutral"|emotion_shown=="disgusted") )
summary(fit_1cohe_dis)

fit_1cohe_sur <- lm(Rating~isemo*cc,data = three_rate, 
                    subset = (right_emo_==1&cc!="Mask")&(emotion_shown=="neutral"|emotion_shown=="surprised") )
summary(fit_1cohe_sur)








##prepare fig6
best_wrong <- emo_conmoe %>% 
  filter(right_emo == 0) %>% 
  group_by(cc, emotion_shown, emotion_level) %>% 
  mutate(bestwrong= max(meanrates)) %>% 
  #select(c(bestwrong, everything())) %>% 
  filter(meanrates == bestwrong) %>% 
  select(c(cc, emotion_shown, emotion_level, bestwrong,names_block)) %>% 
  rename(bestnames= names_block)%>%
  distinct(.keep_all = T)

best_wrong_rel <- emo_conmoe %>% 
  filter(right_emo == 0) %>% 
  group_by(cc, emotion_shown, emotion_level) %>% 
  mutate(bestwrongrel= max(relrates)) %>% 
  #select(c(bestwrong, everything())) %>% 
  filter(relrates == bestwrongrel) %>% 
  select(c(cc, emotion_shown, emotion_level, bestwrongrel,names_block)) %>% 
  rename(bestnamesrel= names_block)%>%
  distinct(.keep_all = T)


only_right <- emo_conmoe %>% 
  filter(right_emo == 1) %>% 
  right_join(best_wrong, by= c("cc", "emotion_shown", "emotion_level")) %>% 
  right_join(best_wrong_rel, by= c("cc", "emotion_shown", "emotion_level")) %>% 
  mutate(diffsec = (meanrates - bestwrong)) %>% 
  mutate(diffsecrel = (relrates - bestwrongrel)) 




#Difference of the coherent stimuli from the second.
##with absolute ratings 
##(not in the paper)
ggplot(data=only_right, aes(x=emotion_level, y=diffsec, group=names_block, colour=names_block)) +
  geom_line(size=1) +  geom_point(data=only_right[only_right$dif_da_sec<0.05,],size=3)+
  facet_grid(cc~names_emo,scales="free")+
  guides(colour=guide_legend(title=""))+
  scale_x_discrete("Level of emotion intensity") +
  scale_y_continuous("Difference of the Average Rating",limits=c(-3,5)) +
  theme_minimal_hgrid()+
  scale_color_manual(values=pal_vec[c(1,2,3,4,5,8,7)])+
  ggtitle("Faces Rated") +
  geom_hline(yintercept=0, col= "grey")+
  theme(plot.title= element_text(face= "bold"),
        axis.title.x = element_text(face= "bold"),
        axis.text.x = element_text(size= 10),
        axis.title.y = element_text(face= "bold"),
        axis.text.y = element_text(size= 10),
        strip.text.y = element_text(face= "bold"),
        panel.spacing.y = unit(1.5,units = "lines")
  )

ggsave(filename= "nofig3_lrg_diff_second_abs.tiff", units="in", width=8, height=5,dpi=300, compression = 'lzw')

##with relative ratings (FIGURE 6)
ggplot(data=only_right, aes(x=emotion_level, y=relrates, group=names_block, colour=names_block)) +
  geom_line(size=1) +  geom_point(data=only_right[only_right$dif_da_sec<0.05,],size=2)+
  geom_line(aes( y= bestwrongrel), color="grey",size=1)+
  geom_point(aes( y= bestwrongrel, colour= bestnamesrel), shape=15,size=1.2)+
  facet_grid(cc~names_emo,scales="free")+
  guides(colour=guide_legend(title=""))+
  scale_x_discrete("Level of emotion intensity") +
  scale_y_continuous("Difference of the Average Rating",limits=c(-3,5.5)) +
  theme_minimal_hgrid()+
  scale_color_manual(values=pal_vec[c(1,2,3,4,5,8,7)])+
  ggtitle("Faces Rated") +
  geom_hline(yintercept=0, col= "grey")+
  theme(plot.title= element_text(face= "bold"),
        #      axis.title.x = element_text(face= "bold"),
        axis.text.x = element_text(size= 10),
        #      axis.title.y = element_text(face= "bold"),
        axis.text.y = element_text(size= 10),
        strip.text.y = element_text(face= "bold"),
        panel.spacing.y = unit(1.5,units = "lines")
  )

ggsave(filename= "fig6_lrg_diff_second_rel.tiff", units="in", width=8, height=5,dpi=300, compression = 'lzw')


# check on p-value <0.05 in various comparisons
sign_coe_neut=emo_conmoe%>% 
  filter(right_emo==T& test_noneut2<0.051)  %>%  
  select(cc, names_block, names_emo, emotion_level, test_noneut2, zeta_noneu2, eff_size_noneu2) %>% 
  rename(p_Value= test_noneut2, Z= zeta_noneu2, R2=eff_size_noneu2)

write_csv(sign_coe_neut, "supplem_lrg_coherent_noneutral.csv")

sign_incoe_neut=emo_conmoe %>% 
  filter(right_emo==F& test_noneut<0.051)  %>%  
  select(cc, names_block, names_emo, emotion_level, test_noneut, zeta_noneu, eff_size_noneu)%>% 
  rename(p_Value= test_noneut, Z= zeta_noneu, R2=eff_size_noneu)

write_csv(sign_incoe_neut, "supplem_lrg_incoherent_noneutral.csv")

sign_coe_sec=emo_conmoe%>% 
  filter(right_emo==T& dif_da_sec<0.051)  %>%  
  select(cc, names_block, names_emo, emotion_level, dif_da_sec, zeta_sec, eff_size_sec)%>% 
  rename(p_Value= dif_da_sec, Z= zeta_sec, R2=eff_size_sec)

write_csv(sign_coe_sec, "supplem_lrg_coherent_nosecond.csv")



###Data prep for MDS
colnames(emo2)[1:4]=c("cc","ProcEmo","emotion_shown","emotion_level")

conmoe2=emo2[emo2$cc!="Mask",]
rat_nomas=three_rate[three_rate$cc!="Mask",]


data_mds=select(rat_nomas, Subject, id_n, targetimage,
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
#lab_emo
data_mds=mutate(data_mds,  Stimulus=str_c(data_mds$emotion_shown, data_mds$emotion_level,sep=" "))

data_mds=mutate(data_mds,  step2=str_c(data_mds$cc,data_mds$Subject,data_mds$names_block,sep=" "))



simplified <- data_mds %>% 
  group_by(cc, Subject, id_n,Stimulus, names_block) %>%
  summarise(Avg_Rating=mean(Rating))%>%
  spread(key = Stimulus,value = Avg_Rating) %>% 
  ungroup()


ctrl_mds=simplified %>% filter(cc=="Control")
moeb_mds=simplified %>% filter(cc=="Moebius")


this_fit <- smacofSym(dist(t(ctrl_mds[,-(1:4)])), ndim = 2, type = "interval")     
fit_plot <- bind_cols(sbj=colnames(ctrl_mds)[-(1:4)], group= substr(colnames(ctrl_mds)[-(1:4)],1,3) , this_fit$conf) %>% 
  mutate(exp= "Control")
fitton <- fit_plot
this_fit <- smacofSym(dist(t(moeb_mds[,-(1:4)])), ndim = 2, type = "interval")     
fit_plot <- bind_cols(sbj=colnames(moeb_mds)[-(1:4)], group= substr(colnames(moeb_mds)[-(1:4)],1,3) , this_fit$conf) %>% 
  mutate(exp= "Moebius")
fitton <- bind_rows(fitton, fit_plot)




brewer.pal(n = 7, name = "Set1")


###FIGURE 7
ggplot(fitton, aes(x= D1, y= D2, color= group))+
  geom_point(size=2, show.legend = F)+
  geom_text_repel(aes(x = D1, y = D2, label = sbj),
                  box.padding = unit(0.1, "lines"), force = 5,
                  segment.color = NA, size=3,show.legend = F) +
  #scale_color_discrete(name="Emotion")+
  #scale_color_manual(values=c("#e41a1c","#377eb8","#4daf4a","#984ea3","#f781bf","#ff7f00","#a65628"))+
  #scale_color_manual(values=c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#a65628","#f781bf"))+
  scale_color_manual(values=pal_vec[c(1,2,3,4,7,5,8)])+
  facet_grid(.~exp,labeller = as_labeller(c(Control = "Control", Moebius= "MBS")))+
  ggtitle("Maps of Stimuli")+
  theme_minimal()+
  theme(plot.title= element_text(face= "bold"),
        #axis.title.x = element_text(face= "bold"),
        axis.text.x = element_text(size= 10),
        #axis.title.y = element_text(face= "bold"),
        axis.text.y = element_text(size= 10)        
  )+
  theme(panel.border = element_rect(linetype = "solid", color = "darkgrey", fill = NA),strip.text = element_text(size = rel(1.3)))+
  scale_y_continuous(limits= c(-1,1),breaks=seq(-1,1,0.4))+
  scale_x_continuous(limits= c(-1,1),breaks=seq(-1,1,0.4))


ggsave(filename= "fig7_lrg_stimuli_map.tiff", units="in", width=12, height=7,dpi=300, compression = 'lzw')

###code for test on permutations
try <- t(ctrl_mds[,-(1:4)])
this_fit <- smacofSym(dist(try), ndim = 3, type = "interval")    
this_fit$stress

perma <- permtest(object = this_fit, data = t(try), 
                  nrep = 1000, method.dat = "euclidean", verbose= F)


