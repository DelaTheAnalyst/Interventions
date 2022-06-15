install.packages("ggrepel")
install.packages("ddply")
library(reshape2)

library(dplyr)
library(tidyverse)
library(ggthemes)

library(ggthemes)

library(extrafont)
library(plyr)
library(ggrepel)

#######################All interventions

work7<-read.csv("C:/Users/DELALI/Desktop/Dr prosper/excels/all interventions1.csv")
View(work7)

mytable7<-table(work7$What.has.been.your.adaption.plans.,work7$X2..If.you.have.the.interventions.in.place..indicate.how.they.were.designed.)
mytable7_1<-table(work7$X9..Kindly.indicate.whether.you.intend.to.adapt.any.of.the.below.measure.to.address.the.effects.of.climate.change.on.you.or.your.family.s.access.to.energy..,work7$What.has.been.your.adaption.plans.)
#view(mytable7)

mydata7<-as.data.frame.matrix(mytable7)
mydata7_1<-as.data.frame.matrix(mytable7_1)

#view(mydata7)

mydata7$All_Interventions<-row.names(mydata7)
mydata7_1$All_Interventions<-row.names(mydata7_1)


##Using dplyr chaining and filtering
data_long7<- mydata7 %>%
  gather(Adaptations,Responses,-c("All_Interventions")) %>%
  select(All_Interventions,Adaptations,Responses) %>%
  filter(Responses > 0)

view(data_long7)

data_long7_1<- mydata7_1 %>%
  gather(Adaptations,Responses,-c("All_Interventions")) %>%
  select(All_Interventions,Adaptations,Responses) %>%
  filter(Responses > 0)

View(data_long7_1)



inter_V7<-ggplot() + geom_bar(aes(y= Responses,
                                  x = All_Interventions,
                                  fill = Adaptations),
                              data = data_long7,
                              stat = "Identity",position_stack(reverse = TRUE))  

inter_V7<- inter_V7 +  geom_text(data=data_long7, aes(x = All_Interventions, y = Responses ,label = (Responses)),stat = "identity",position = "stack",vjust = 1, size =4)+
  ggtitle("All Interventions")



inter_V7_1<-ggplot() + geom_bar(aes(y= Responses,
                                  x = All_Interventions,
                                  fill = Adaptations),
                              data = data_long7_1,
                              stat = "Identity",position_stack(reverse = TRUE))  

inter_V7_1<- inter_V7 +  geom_text(data=data_long7_1, aes(x = All_Interventions, y = Responses ,label = (Responses)),stat = "identity",position = "stack",vjust = 1, size =4)+
  ggtitle("All Interventions")



inter_V7_1

#############Energy

Mobility<-filter(work7,work7$X9..Kindly.indicate.whether.you.intend.to.adapt.any.of.the.below.measure.to.address.the.effects.of.climate.change.on.you.or.your.family.s.access.to.energy..%in%   c("Removal of water weeds","Using sandbags to impede flooding","Creation of gutters"))
view(Mobility)

##Categorising the data into a table
#mytable1<-table(Food$What.has.been.your.adaption.plans.,Food$X2..If.you.have.the.interventions.in.place..indicate.how.they.were.designed.)
mytable1_1<-table(Mobility$X9..Kindly.indicate.whether.you.intend.to.adapt.any.of.the.below.measure.to.address.the.effects.of.climate.change.on.you.or.your.family.s.access.to.energy..,  Mobility$What.has.been.your.adaption.plans.)


##putting the table into a dataframe matrix
#mydata1<-as.data.frame.matrix(mytable1)
mydata1_1<-as.data.frame.matrix(mytable1_1)

mydata1_1$Mobility_Interventions<-row.names(mydata1_1)

#data_long1<- mydata1 %>%
#  gather(Adaptations,Responses,-c("Mobility_Interventions")) %>%
 # select(Mobility_Interventions,Adaptations,Responses) %>%
  #filter(Responses > 0)
  

data_long1_1<- mydata1_1 %>%
  gather(Adaptations,Responses,-c("Mobility_Interventions")) %>%
  select(Mobility_Interventions,Adaptations,Responses) %>%
  filter(Responses > 0)

view(data_long1_1)

inter_V1_1<-ggplot() + geom_bar(aes(y= Responses,
                                  x = Mobility_Interventions,
                                  fill = Adaptations),
                              data = data_long1_1,
                              stat = "Identity",position_stack(reverse = TRUE))  

inter_V1_1<-inter_V1_1 +  geom_text(data=data_long1_1, aes(x = Mobility_Interventions, y = Responses ,label = (Responses)),stat = "identity",position = "stack",vjust = 1, size =4)+
  ggtitle("Mobility Interventions")

inter_V1_1








