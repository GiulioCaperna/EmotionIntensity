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
##The code for group analysis is in "AnalysisScript33sbj.R"

#three_rate$Rating <- as.numeric(three_rate$Rating)
meanrates=tapply(three_rate$Rating,paste(three_rate$cc,three_rate$`Procedure[Block]`, three_rate$EmotionLevel),mean)

sdrates=tapply(three_rate$Rating,paste(three_rate$cc,three_rate$`Procedure[Block]`, three_rate$EmotionLevel),sd)
splitted=strsplit(names(meanrates),split = " ")


#creation of the main dataset
emo2=data.frame(t(data.frame(splitted)))

emo2$meanrates=meanrates
emo2$sdrates=sdrates


####repeated for large dataset
meanrates_lrg=tapply(three_rate$Rating,paste(three_rate$cc, three_rate$`Procedure[Block]`, three_rate$EmotionLevel,three_rate$id_n),mean)

sdrates_lrg=tapply(three_rate$Rating,paste(three_rate$cc,three_rate$`Procedure[Block]`, three_rate$EmotionLevel,three_rate$id_n),sd)
splitted_lrg=strsplit(names(meanrates_lrg),split = " ")

emo2_lrg=data.frame(t(data.frame(splitted_lrg)))

emo2_lrg$meanrates=meanrates_lrg
emo2_lrg$sdrates=sdrates_lrg
####end of repetition


asked <- unique(emo2$X2)
shown <- unique(emo2$X3)[c(2,3,1,4,6,7)]

coherence_df <- data.frame(asked,shown )

####
###test single comp
emo2_lrg <- emo2_lrg %>% dplyr::mutate(meanrates/sdrates)
res_crhow <- three_rate %>% 
  select(c(Subject, cc,id_n, `Procedure[Block]`,emotion_shown,emotion_level, Rating)) %>% 
  filter(cc=="Moebius")
res_crhow$meanctrl <- NA
res_crhow$sdctrl <- NA
res_crhow$t_value <- NA
res_crhow$prop <- NA
res_crhow$coherent <- NA
# i=1
#  sbj=1

table(emo2$X1)
mbs_cases <- which(emo2$X1=="Moebius")

for (i in mbs_cases){

  
  for (sbj in 1:11){
    this_proc <- emo2[i,"X2"]
    this_emoshow <- emo2[i,"X3"]
    this_emolev <- as.numeric(emo2[i,"X4"])
    
      
    ctrl <- three_rate %>% 
      filter(cc=="Control") %>% 
      filter(`Procedure[Block]`==this_proc) %>% 
      filter(emotion_shown==this_emoshow) %>% 
      filter(emotion_level==this_emolev) %>%
      filter(Subject!=sbj) %>% 
      summarise(meanrates= mean(Rating), sdrates= sd(Rating))
    
    ctrl_m <- ctrl$meanrates
    ctrl_sd <- ctrl$sdrates
    
    mbs_tibble <-  three_rate %>% 
      filter(cc=="Moebius") %>% 
      filter(`Procedure[Block]`==this_proc) %>% 
      filter(emotion_shown==this_emoshow) %>%
      filter(emotion_level==this_emolev) %>% 
      filter(Subject==sbj) %>% 
      select(Rating) 
    
    
    mbs <- mbs_tibble$Rating
    
    res_row <- which(res_crhow$Subject==sbj&
                       res_crhow$`Procedure[Block]`==this_proc&
                       res_crhow$emotion_shown==this_emoshow&
                       res_crhow$emotion_level==this_emolev)
    
    res_crhow[res_row,"meanctrl"] <- ctrl_m
    res_crhow[res_row,"sdctrl"] <- ctrl_sd
    
    
    #print(paste0(c(this_proc," ", this_emoshow," ",this_emolev, " " , sbj)))
    # print(mbs)
    # print(ctrl)
    crhow_t <- (mbs- ctrl_m)/(ctrl_sd*sqrt(31/30))
    prop <- pt(crhow_t, 29)
    
    res_crhow[res_row,"t_value"] <- crhow_t
    res_crhow[res_row,"prop"] <- prop
    res_crhow[res_row,"coherent"] <- this_emoshow== coherence_df[coherence_df$asked==this_proc,2]
    
    
    
  }
  print((i- min(mbs_cases))/length(mbs_cases))
}
write.csv(res_crhow,file = "results_individuals_mbs_large.csv")
crhow_mbs <- res_crhow

###repetition for controls
res_crhow <- three_rate %>% 
  select(c(Subject, cc,id_n, `Procedure[Block]`,emotion_shown,emotion_level, Rating)) %>% 
  filter(cc=="Control")
res_crhow$meanctrl <- NA
res_crhow$sdctrl <- NA
res_crhow$t_value <- NA
res_crhow$prop <- NA
res_crhow$coherent <- NA

ctrl_cases <- which(emo2_lrg$X1=="Control")


for (i in ctrl_cases){
  
  
  for (sbj in 1:11){
    for( id in c("a","b","c")){
    this_proc <- emo2_lrg[i,"X2"]
    this_emoshow <- emo2_lrg[i,"X3"]
    this_emolev <- as.numeric(emo2_lrg[i,"X4"])
    
    
    ctrl <- three_rate %>% 
      filter(cc=="Control") %>% 
      filter(`Procedure[Block]`==this_proc) %>% 
      filter(emotion_shown==this_emoshow) %>% 
      filter(emotion_level==this_emolev) %>%
      filter(Subject!=sbj) %>% 
      summarise(meanrates= mean(Rating), sdrates= sd(Rating))
    
    ctrl_m <- ctrl$meanrates
    ctrl_sd <- ctrl$sdrates
    
    obs_tibble <-  three_rate %>% 
      filter(cc=="Control") %>% 
      filter(`Procedure[Block]`==this_proc) %>% 
      filter(emotion_shown==this_emoshow) %>%
      filter(emotion_level==this_emolev) %>% 
      filter(Subject==sbj) %>% 
      filter(id_n==id) %>% 
      select(Rating) 
    
    
    obs <- obs_tibble$Rating
    
    res_row <- which(res_crhow$Subject==sbj&
                       res_crhow$id_n==id&
                       res_crhow$`Procedure[Block]`==this_proc&
                       res_crhow$emotion_shown==this_emoshow&
                       res_crhow$emotion_level==this_emolev)
    
    res_crhow[res_row,"meanctrl"] <- ctrl_m
    res_crhow[res_row,"sdctrl"] <- ctrl_sd
    
    
    #print(paste0(c(this_proc," ", this_emoshow," ",this_emolev, " " , sbj)))
    # print(mbs)
    # print(ctrl)
    crhow_t <- (obs- ctrl_m)/(ctrl_sd*sqrt(31/30))
    prop <- pt(crhow_t, 29)
    
    res_crhow[res_row,"t_value"] <- crhow_t
    res_crhow[res_row,"prop"] <- prop
    res_crhow[res_row,"coherent"] <- this_emoshow== coherence_df[coherence_df$asked==this_proc,2]
    
    
    
  }}
  print((i- min(ctrl_cases))/length(ctrl_cases))
}

write.csv(res_crhow,file = "results_individuals_ctrl_lrg.csv")

####
crhow_all <- bind_rows(crhow_mbs,res_crhow)
rm(crhow_mbs)

crhow_all <- crhow_all %>% 
  mutate(Zcc = (Rating-meanctrl)/sdctrl)

###
###non finisce qui
###

crhow_all$sig <- (crhow_all$prop<0.05|crhow_all$prop>0.95)*1

crhow_all$names_emo <- revalue(crhow_all$emotion_shown, 
                               c("afraid"="Fearful",
                                 "angry" ="Angry",    
                                 "disgusted" = "Disgusted",
                                 "happy" = "Happy",    
                                 "neutral"= "Neutral",   
                                 "sad" ="Sad",      
                                 "surprised"="Surprised"))

crhow_all %>% 
  filter(emotion_shown!="neutral") %>% 
  mutate(fac_emo=as.factor(emotion_level)) %>% 
  filter(coherent == "TRUE",sig==1) %>% 
  # group_by(cc,Subject, emotion_shown) %>% 
  # summarise(Zcc = mean(Zcc)) %>% 
  mutate(dir = ifelse(Zcc < 0, "neg", "pos")) %>%
  ggplot(aes(x = Zcc, y = factor(Subject), group = fac_emo, color=dir)) +
  facet_grid(id_n~names_emo,drop = FALSE) +
  geom_point(size = 1.5,
             position = position_dodge(width = 0.7),
             show.legend = FALSE) +
  geom_linerange(aes(xmin = 0, xmax = Zcc), 
                 position = position_dodge(width = 0.7),
                 size = 0.5,
                 show.legend = FALSE) +
  
  xlim(c(-3,3)) +
  theme_minimal(base_size = 15) +
  theme(plot.title= element_text(face= "bold"),
        #axis.title.x = element_text(face= "bold"),
        axis.text.x = element_text(size= 10),
        #axis.title.y = element_text(face= "bold"),
        axis.text.y = element_text(size= 10),
        panel.spacing.y = unit(1.5,units = "lines")
  )+
  ylab("Subjects") 

Session_labs <- c(
  a = "Control A",
  b = "Control B",
  c = "Control C",
  m = "MBS"
)

###Graph type b
crhow_all %>% 
  filter(emotion_shown!="neutral") %>% 
  mutate(fac_emo=as.factor(emotion_level)) %>% 
  filter(coherent == "TRUE") %>%
  #filter(sig==1) %>% 
  # group_by(cc,Subject, emotion_shown) %>% 
  # summarise(Zcc = mean(Zcc)) %>% 
  mutate(dir = ifelse(Zcc < 0, "neg", "pos")) %>%
  ggplot(aes(x = Zcc, y = factor(Subject), group = fac_emo)) +
  facet_grid(id_n~names_emo,drop = FALSE, labeller=labeller(id_n= Session_labs )) +
  geom_point(size = 1.1,
             position = position_dodge(width = 0.7),
             show.legend = FALSE, color= "lightgrey") +
  geom_linerange(aes(xmin = 0, xmax = Zcc), 
                 position = position_dodge(width = 0.7),
                 size = 0.45, color= "lightgrey",
                 show.legend = FALSE) +
  geom_linerange(data=crhow_all %>% 
                   filter(emotion_shown!="neutral") %>% 
                   mutate(fac_emo=as.factor(emotion_level)) %>% 
                   filter(coherent == "TRUE",sig==1) %>% 
                   mutate(dir = ifelse(Zcc < 0, "neg", "pos")),
                 aes(xmin = 0, xmax = Zcc, col=dir), 
                 position = position_dodge(width = 0.7),
                 size = 0.45,
                 show.legend = FALSE) +
  geom_point(data=crhow_all %>% 
               filter(emotion_shown!="neutral") %>% 
               mutate(fac_emo=as.factor(emotion_level)) %>% 
               filter(coherent == "TRUE",sig==1) %>% 
               mutate(dir = ifelse(Zcc < 0, "neg", "pos")),
             aes(x = Zcc, y = factor(Subject), group = fac_emo, color=dir),
             size = 1.3,
             position = position_dodge(width = 0.7),
             show.legend = FALSE)+
  xlim(c(-5,3)) +
  theme_minimal(base_size = 15) +
  theme(plot.title= element_text(face= "bold"),
        #axis.title.x = element_text(face= "bold"),
        axis.text.x = element_text(size= 10),
        #axis.title.y = element_text(face= "bold"),
        axis.text.y = element_text(size= 8),
        panel.spacing.y = unit(1.5,units = "lines")
  )+
  ylab("Subjects") 
###
ggsave(filename= "single_lrg.tiff", units="in", width=8, height=6,dpi=600, compression = 'lzw')


sens_alpha <- data.frame(alpha=vector("numeric",100), Control=vector("numeric",100), MBS=vector("numeric",100)) 

for (i in 1:100){
  thresh <- seq(0.001,0.10,0.001)[i] 
  cem <- table((crhow_all$prop<thresh|crhow_all$prop>(1-thresh))*1, crhow_all$cc,crhow_all$coherent)[,,2]  
  
  sens_alpha[i,] <- c(thresh, cem[2,]/cem[1,]) 
  print(sens_alpha[i,])
}

sens_alpha <- pivot_longer(sens_alpha,cols = Control:MBS,names_to = "Group")

library(cowplot)
ggplot(sens_alpha,aes(x=alpha, y=value, col=Group))+
  geom_point()+
  scale_color_manual(values = c("#80146E", "#879FDB"))+
  theme_minimal_hgrid()+
  theme(plot.title= element_text(face= "bold"),
        #axis.title.x = element_text(face= "bold"),
        axis.text.x = element_text(size= 10),
        #axis.title.y = element_text(face= "bold"),
        axis.text.y = element_text(size= 10)        
  )+
  guides(colour=guide_legend(title=""))+
  ylab("Proportion of extreme ratings")
ggsave(filename= "sensitivity_large.tiff", units="in", width=8, height=4,dpi=300, compression = 'lzw')



# crhow_all$emotion_shown <-  factor(crhow_all$emotion_shown,levels = unique(crhow_all$emotion_shown)[c(5,1,2,3,4,6,7)])
# summary(glm(relative_rating~cc*coherent+emotion_shown*cc+ coherent*emotion_shown,family = "gaussian",
#             data=crhow_all))
# summary(glm(sig~cc,family = "binomial",data=crhow_all %>% filter(coherent==FALSE)))

