library(dplyr)
library(readr)
library(reshape2)
library(ggplot2)
library(parallel)

nbcores <-  detectCores() -2

#loading data
setwd("~/dev/exmodeloZombies/")
df <-  read_csv("replications_1000.csv")
names(df)
dfParams <-  subset(df, select = c(23:29))
##Identification of unique parameterization 

#SIMULATION seed vary from a replication to another -> removed before unique() 
uniqueParam <- unique(dfParams[,-4])
uniqueParam$paramID <-  1:200
uniqueParam <-  as.data.frame(uniqueParam)

dfZombieslong <-  read_csv("ZombiePopulation.csv")
dfHumanslong <-  read_csv("HumanPopulation.csv")
names(dfZombieslong)


#JOIN uniqueParam with df then split into human history records and zombies history records
dfHumans  <- subset(df,select = c(1:29))
dfHumans <-  inner_join(dfHumans, uniqueParam)
dfZombies  <- subset(df,select = c(30:51,23:29))
dfZombies <-  inner_join(dfZombies, uniqueParam)


#each line is a run , so we melt it to have a long format 



#formatting in long
#last column must be parametrizationID
longformatter <-  function(lili){
  bckupID <-  lili[24]
  bckupseed <-  lili[23]
    lili <- lili[-c(24,23)]
  longLili <-  melt(lili, id.vars=NULL)
  longLili$paramID <- rep(bckupID,22)
  longLili$stepnumber <-  seq(1:22)
  longLili$seed <-  rep(bckupseed,22)
  rownames(longLili) <- NULL
return(as.vector(longLili))
}



#human history + simulation iD
hh <-  subset(dfHumans, select = c(1:22,26,30))
dfHumanslong <- data.frame()
dfHumanslong <-   do.call("rbind", apply(hh,1,longformatter ))
rm(hh)
rm(dfHumans)



zz <-  subset(dfZombies, select=c(1:22,26,30))
dfZombieslong <-  data.frame()
dfZombieslong <-  do.call("rbind", apply(zz,1,longformatter))
rm(zz)
rm(dfZombies)



write.csv(dfZombieslong, "ZombiePopulation.csv")
write.csv(dfHumanslong, "HumanPopulation.csv")



ph1 <-  ggplot(dfHumanslong, aes(x=stepnumber, y=value, group=seed, color=paramID) )+
  geom_line()+
  scale_color_viridis_c()
ph1



pz1 <-  ggplot(dfZombieslong , aes(x=stepnumber, y=value, group=seed, color=paramID))+
  geom_line()+
  scale_colour_viridis_c()
pz1


#trajectoires à plateaux 



tutu <- dfZombieslong%>% filter(between(paramID, 76, 90))
piou <-  ggplot(tutu , aes(x=stepnumber, y=value, group=seed, color=paramID))+
  geom_line()+
  scale_colour_viridis_c()
piou



# 
# 
# xx <-  inner_join(dfZombieslong, uniqueParam)
# names(xx)
# 
# 
# pp <-  ggplot(head(xx,nrow(xx)/4), aes(x=stepnumber, y=value, group=seed, color=nomTime))+
# geom_line()+
#   scale_colour_viridis_c()
# pp
# 
# pp <-  ggplot(sample_n(xx,200000), aes(x=stepnumber, y=value,  color=nomBoost))+
#   geom_jitter(size=0.3)+
#   scale_colour_viridis_c()
# pp
# 



# mediane moyenne et écart type  par groupe de replication
medmeanZ <-  dfZombieslong %>%  group_by(paramID, stepnumber) %>% summarise(med=median(value), moy=mean(value), sd=sd(value))

medmeanH <-  dfHumanslong %>%  group_by(paramID, stepnumber) %>% summarise(med=median(value), moy=mean(value), sd=sd(value))


medmeanH <-  inner_join(medmeanH, uniqueParam)
medmeanZ <-  inner_join(medmeanZ, uniqueParam)

names(medmeanZ)






paramNames <- c("nomBoost","nomTime","panicDuration","zombieAcuteness","zombieLifespan","zombieSpeedFactor")
paramindex <-  seq(6,11)

#

generateZombiesPlotsOverParams <- function(pIndex) {
  
  lili <-  as.data.frame(medmeanZ[,paramindex[pIndex]])[,1]
  currentparamName <-  paramNames[pIndex]
  cat(pIndex,":",currentparamName,"\n")
  
  
  ppmed <-  ggplot(medmeanZ, aes(x=stepnumber, y=med, group=paramID, color=lili))+
  geom_line(size=0.4, alpha=0.8)+
  labs(title = "Rise and Fall of the Undead", x = "step", y = "Zombies population\n (median over replications)", color=currentparamName)+
  scale_colour_viridis_c()

fifiname <-  paste0("ZombiesMedian_",currentparamName,".png")
ggsave(fifiname)

ppmean <-  ggplot(medmeanZ, aes(x=stepnumber, y=moy, group=paramID,color=lili))+
  geom_line(size=0.4, alpha=0.8)+
  labs(title = "Rise and Fall of the Undead", x = "step", y = "Zombies population\n (mean over replications)", color=currentparamName)+
  scale_colour_viridis_c()
ppmean
fifiname <-  paste0("ZombiesMean_",currentparamName,".png")
ggsave(fifiname)


ppsd <-  ggplot(medmeanZ, aes(x=stepnumber, y=sd, group=paramID,color=lili))+
  geom_line(size=0.4, alpha=0.8)+
  labs(title = "Rise and Fall of the Undead", x = "step", y = "Zombies population\n (standard deviation over replications)", color=currentparamName)+
  scale_colour_viridis_c()
fifiname <-  paste0("ZombiesSD_",currentparamName,".png")
ggsave(fifiname)
}

generateHumansPlotsOverParams <- function(pIndex) {
  
  lili <-  as.data.frame(medmeanH[,paramindex[pIndex]])[,1]
  currentparamName <-  paramNames[pIndex]
  cat(pIndex,":",currentparamName,"\n")
  
  
  ppmed <-  ggplot(medmeanH, aes(x=stepnumber, y=med, group=paramID, color=lili))+
    geom_line(size=0.4, alpha=0.8)+
    labs(title = "Humans Doom", x = "step", y = "Human population\n (median over replications)", color=currentparamName)+
    scale_colour_viridis_c()
  
  fifiname <-  paste0("HumansMedian",currentparamName,".png")
  ggsave(fifiname)
  
  ppmean <-  ggplot(medmeanH, aes(x=stepnumber, y=moy, group=paramID,color=lili))+
    geom_line(size=0.4, alpha=0.8)+
    labs(title = "Humans Doom", x = "step", y = "HUmans population\n (mean over replications)", color=currentparamName)+
    scale_colour_viridis_c()
  ppmean
  fifiname <-  paste0("HumansMean",currentparamName,".png")
  ggsave(fifiname)
  
  
  ppsd <-  ggplot(medmeanH, aes(x=stepnumber, y=sd, group=paramID,color=lili))+
    geom_line(size=0.4, alpha=0.8)+
    labs(title = "Humans Doom", x = "step", y = "Humans population\n (standard deviation over replications)", color=currentparamName)+
    scale_colour_viridis_c()
  fifiname <-  paste0("HumansSD_",currentparamName,".png")
  ggsave(fifiname)
}





# generation des plots 
sapply(1:6, generateZombiesPlotsOverParams)
sapply(1:6, generateHumansPlotsOverParams)




#boxplot par paramID
rm(df)
names(dfHumanslong)


displayTrajByID <-function(id){
zz <-  dfHumanslong %>% filter(paramID==id)
ppp <-  ggplot(zz, aes(x=stepnumber,y=value))+
  geom_boxplot(aes(group=stepnumber), outlier.alpha = 0.5)+
  labs(title = "", x = "step", y = "Human population\n (boxplot over replications)")
print(ppp)
}
displayTrajByID(55)


# 10 premiers paramétrages à la somme des écarts types  la simu minimale 
id25LowSD <- medmeanH %>% group_by(paramID) %>% summarise(sumsd= sum(sd)) %>%  arrange((sumsd)) %>% head(25)

#somme des sd le long de la simu
sumSDbyParamID_H <-  medmeanH %>% group_by(paramID) %>% summarise(sumsd= sum(sd)) 
uniqueParam$sumSD_H <-  sumSDbyParamID_H$sumsd
gaga <- uniqueParam %>% select(-paramID) %>% melt(id.vars=c("sumSD_H")) 

p1p2 <-  ggplot(gaga, aes(x=sumSD_H, y=value))+
  geom_point()+
  scale_color_viridis_c()+
  facet_wrap(~variable,scales = 'free')
p1p2

p3p4 <-  ggplot(uniqueParam, aes(x=panicDuration, y=zombieAcuteness, color=sumSD_H))+
  geom_point()+
  scale_color_viridis_c(direction = -1)
p3p4


p5p6 <-  ggplot(uniqueParam, aes(x=zombieLifespan, y=zombieSpeedFactor, color=sumSD_H))+
  geom_point()+
  scale_color_viridis_c(direction = -1)
p5p6


p3p6 <-  ggplot(uniqueParam, aes(x=panicDuration, y=zombieSpeedFactor, color=sumSD_H))+
  geom_point()+
  scale_color_viridis_c(direction = -1)
p3p6



