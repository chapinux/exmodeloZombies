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



zz <-  subset(dfZombies, select=c(1:22,26,30))
dfZombieslong <-  data.frame()
dfZombieslong <-  do.call("rbind", apply(zz,1,longformatter))
rm(zz)



ph1 <-  ggplot(dfHumanslong, aes(x=stepnumber, y=value, group=seed, color=paramID) )+
  geom_line()+
  scale_color_viridis_c()
ph1



pz1 <-  ggplot(dfZombieslong, aes(x=stepnumber, y=value, group=seed, color=paramID))+
  geom_line()+
  scale_colour_viridis_c()
pz1

xx <-  inner_join(dfZombieslong, uniqueParam)
names(xx)


pp <-  ggplot(head(xx,nrow(xx)/4), aes(x=stepnumber, y=value, group=seed, color=nomTime))+
geom_line()+
  scale_colour_viridis_c()
pp

pp <-  ggplot(sample_n(xx,200000), aes(x=stepnumber, y=value,  color=nomBoost))+
  geom_jitter(size=0.3)+
  scale_colour_viridis_c()
pp




# mediane moyenne et écart type  par groupe de replication
medmeanZ <-  dfZombieslong %>%  group_by(paramID, stepnumber) %>% summarise(med=median(value), moy=mean(value), sd=sd(value))

medmeanH <-  dfHumanslong %>%  group_by(paramID, stepnumber) %>% summarise(med=median(value), moy=mean(value), sd=sd(value))


medmeanH <-  inner_join(medmeanH, uniqueParam)
medmeanZ <-  inner_join(medmeanZ, uniqueParam)

names(medmean)










ppmed <-  ggplot(medmeanZ, aes(x=stepnumber, y=med, group=paramID, color=(gzugzu[,6])))+
  geom_line(size=0.2)+
  labs(title = "Dusk of the Dead", x = "step", y = "Zombies population\n (median over replications)")+
  scale_colour_viridis_c()
ppmed

ppmean <-  ggplot(medmeanZ, aes(x=stepnumber, y=moy, group=paramID, color=paramID))+
  geom_line(size=0.2)+
  labs(title = "Dusk of the Dead", x = "step", y = "Zombies population\n (mean over replications)")+
  scale_colour_viridis_c()
ppmean

ppsd <-  ggplot(medmeanZ, aes(x=stepnumber, y=sd, group=paramID, color=paramID))+
  geom_line(size=0.2)+
  labs(title = "Dusk of the Dead", x = "step", y = "Zombies population\n (standard deviation over replications)")+
  scale_colour_viridis_c()
ppsd



