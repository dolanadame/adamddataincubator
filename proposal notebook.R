install.packages('ggplot2')
library(ggplot2)
#plot(openbrewerydb_ALL$state)
#plot(statepopulations$popEST.2018)

statepopulations$nbreweries<-NA
statepopulations[] <- lapply(statepopulations, gsub, pattern=',', replacement='')
statepopulations[] <- lapply(statepopulations, gsub, pattern='%', replacement='')
statepopulations[] <- lapply(statepopulations, gsub, pattern='�', replacement='')


openbrewerydb_ALL[openbrewerydb_ALL$state=='FL San',]

openbrewerydbALLfiltered<-openbrewerydb_ALL[openbrewerydb_ALL$state!=''& openbrewerydb_ALL$state!='FL San'& openbrewerydb_ALL$state!='District of Columbia',]

rm(liststates)
liststates=as.character(unique(openbrewerydbALLfiltered$state))
liststates

i=1
breweriesbystate<-data.frame(State=character(),nbreweries=double(), stringsAsFactors = FALSE)
for(state in liststates){
  nbreweries=nrow(openbrewerydb_ALL[openbrewerydbALLfiltered$state==state,])
  breweriesbystate[i,1]=state
  breweriesbystate[i,2]=nbreweries
  i=i+1
}


mergedpopbrew<-merge(breweriesbystate,statepopulations,by='State')
mergedpopbrew$nbreweries.y<-NULL
mergedpopbrew$nbreweries<-mergedpopbrew$nbreweries.x
mergedpopbrew$nbreweries.x<-NULL

mergedpopbrew[] <- lapply(mergedpopbrew, gsub, pattern='%', replacement='')

mergedpopbrew$popCEN.2010<-as.numeric(mergedpopbrew$popCEN.2010)
mergedpopbrew$popEST.2018<-as.numeric(mergedpopbrew$popEST.2018)
mergedpopbrew$nbreweries<-as.numeric(mergedpopbrew$nbreweries)
mergedpopbrew$percincr<-as.numeric(mergedpopbrew$percincr)

#mergedpopbrew.outlieromit<-mergedpopbrew[mergedpopbrew$State!='California'&mergedpopbrew$State!='Illinois'&mergedpopbrew$State!='Pennsylvania'&mergedpopbrew$State!='New York'&mergedpopbrew$State!='Michigan'&mergedpopbrew$State!='Ohio',]

#percincrbrew.outlieromitlm<-lm(percincr~nbreweries, data=mergedpopbrew.outlieromit)
#summary(percincrbrew.outlieromitlm)
#plot(percincr~nbreweries, data=mergedpopbrew.outlieromit)
#abline(percincrbrew.outlieromitlm)

#percincrbrewlm<-lm(percincr~nbreweries, data=mergedpopbrew)
#summary(percincrbrewlm)
#plot(percincr~nbreweries, data=mergedpopbrew)
#abline(percincrbrewlm)



rownames(mergedpopbrew)<-mergedpopbrew$State


#
popbrewlm<-lm(nbreweries~popEST.2018, data=mergedpopbrew)
summary(popbrewlm)
plot(nbreweries~popEST.2018, data=mergedpopbrew, ylab='Number of Breweries', xlab='Population in 2018 (estimate)', main='State Brewery Number by Population')
with(mergedpopbrew[1:50,],text(nbreweries~popEST.2018, labels=row.names(mergedpopbrew[1:50,]),pos=4))
abline(popbrewlm, col='Red')
r2label<-paste0('R^2 = ',format(summary(popbrewlm)$adj.r.squared, digits=4))
text(x=5e6,y=900,labels=r2label)

mergedpopbrew$breweriespercap<-NA
mergedpopbrew$breweriespercap<-mergedpopbrew$nbreweries/mergedpopbrew$popEST.2018
plot(breweriespercap~popEST.2018, data=mergedpopbrew, xlab='Population in 2018 (estimate)', ylab='Breweries per Capita', main='Overrepresentation of Breweries by Population')
with(mergedpopbrew[1:50,],text(breweriespercap~popEST.2018, labels=row.names(mergedpopbrew[1:50,]),pos=4))



popbrewloglm<-lm(log(nbreweries)~log(popEST.2018), data=mergedpopbrew)
summary(popbrewloglm)
plot(log(nbreweries)~log(popEST.2018), data=mergedpopbrew, ylab='Number of Breweries', xlab='Population in 2018 (estimate)', main='State Brewery Number by Population')
with(mergedpopbrew[1:50,],text(log(nbreweries)~log(popEST.2018), labels=row.names(mergedpopbrew[1:50,]),pos=4))
abline(popbrewloglm, col='Red')
r2label<-paste0('R^2 = ',format(summary(popbrewloglm)$adj.r.squared, digits=4))
text(x=14,y=6.5,labels=r2label)

plot(log(breweriespercap)~log(popEST.2018), data=mergedpopbrew, xlab='Population in 2018 (estimate)', ylab='Breweries per Capita', main='Overrepresentation of Breweries by Population')
with(mergedpopbrew[1:50,],text(log(breweriespercap)~log(popEST.2018), labels=row.names(mergedpopbrew[1:50,]),pos=4))



#montanabreweries<-subset(openbrewerydb_ALL, openbrewerydb_ALL$state=='Montana')
#montanabreweries$city<-as.character(montanabreweries$city)
#montanabreweries$city<-as.factor(montanabreweries$city)
#montcities<-as.data.frame(table(montanabreweries$city))
#print(montcities)

#vtbreweries<-subset(openbrewerydb_ALL, openbrewerydb_ALL$state=='Vermont')
#vtbreweries$city<-as.character(vtbreweries$city)
#vtbreweries$<city<-as.factor(vtbreweries$city)
#vtcities<-as.data.frame(table(vtbreweries$city))
#print(vtcities)
#plot(vtcities)
#sum(vtcities$Freq)



citynumbers<-as.data.frame(table(openbrewerydbALLfiltered$city))
head(citynumbers)
plot(citynumbers)
citynumberstop<-subset(citynumbers, citynumbers$Freq>=20)
head(citynumberstop)
print(citynumberstop)
plot(citynumberstop)
hist(citynumberstop$Freq)

rm(topstatesdb)
listtopstates<-c('Montana','Maine','Colorado','Oregon','Alaska','Wyoming','New Hampshire','Washington')
topstatesdb<-subset(openbrewerydbALLfiltered, openbrewerydbALLfiltered$state=='Vermont')
for(place in 1:8){
  newsub<-subset(openbrewerydbALLfiltered, openbrewerydbALLfiltered$state==listtopstates[place])
  topstatesdb<-rbind(topstatesdb, newsub)
}
topstatesdb$city<-as.character(topstatesdb$city)
topstatesdb$state<-as.character(topstatesdb$state)

listtopstates<-c('Vermont','Montana','Maine','Colorado','Oregon','Alaska','Wyoming','New Hampshire','Washington')
citiesbytopstate<-data.frame(city=as.character(),freq=as.numeric(),state=as.character())
for(place in 1:9){
  newsub<-subset(topstatesdb,topstatesdb$state==listtopstates[place])
  newtable<-as.data.frame(table(newsub$city))
  newtable$state<-listtopstates[place]
  citiesbytopstate<-rbind(citiesbytopstate,newtable)
}
boxplot(Freq~state, data=citiesbytopstate, xlab='State', ylab='Number of Breweries in Each City', main='Number of Breweries in Cities for the Top States by Breweries per Capita')
boxplot(Freq~state, data=citiesbytopstate[-c(179,313,527),], xlab='State', ylab='Number of Breweries in Each City', main='Number of Breweries in Cities for the Top States by Breweries per Capita (Seattle, Portland, and Denver Omitted)')

#boxplot(log2(Freq)~state, data=citiesbytopstate, xlab='State', ylab='Number of Breweries in Each City (log 2)', main='Number of Breweries in Cities for the Top States by Breweries per Capita')

ggplot(citiesbytopstate, aes(x=state,y=Freq, fill=state))+geom_violin()+scale_y_continuous(trans = 'log2')+labs(title='City Brewery Number in Each State with High Breweries per Capita', x='State', y='Number of Breweries per City')+theme_classic()

rm(citypopmerged)
colnames(subest2017all)[colnames(subest2017all)=="NAME"] <- "city"
citypopmerged<-merge(subest2017all,openbrewerydbALLfiltered,by='city')

#manually curated citypopulations with brewery numbers over 20.  noticed some cities have redundant names in multiple states.  Omitted those for which the number of breweries in a city was affected by more than 25% by redundant city names.

citypopbrewlm<-lm(NumBrew~Population2017, data=citypopmanual)
plot(NumBrew~Population2017, data=citypopmanual, xlab='City Population', ylab='Number of Breweries in City', main='City Brewery Number by Population')
abline(citypopbrewlm, col='Red')
r2label<-paste0('R^2 = ',format(summary(citypopbrewlm)$adj.r.squared, digits=4))
text(x=5e5,y=80.5,labels=r2label)







