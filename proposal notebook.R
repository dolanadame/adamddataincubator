
plot(openbrewerydb_ALL$state)
plot(statepopulations$popEST.2018)

statepopulations$nbreweries<-NA
statepopulations[] <- lapply(statepopulations, gsub, pattern=',', replacement='')
statepopulations[] <- lapply(statepopulations, gsub, pattern='%', replacement='')
statepopulations[] <- lapply(statepopulations, gsub, pattern='ÿ', replacement='')


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



popbrewloglm<-lm(log(popEST.2018)~log(nbreweries), data=mergedpopbrew)
summary(popbrewlm)
plot(log(nbreweries)~log(popEST.2018), data=mergedpopbrew, ylab='Number of Breweries', xlab='Population in 2018 (estimate)', main='State Brewery Number by Population')
with(mergedpopbrew[1:50,],text(log(popEST.2018)~log(nbreweries), labels=row.names(mergedpopbrew[1:50,]),pos=4))
abline(popbrewloglm, col='Red')
r2label<-paste0('R^2 = ',format(summary(popbrewloglm)$adj.r.squared, digits=4))
text(x=3,y=17,labels=r2label)

plot(log(breweriespercap)~log(popEST.2018), data=mergedpopbrew, xlab='Population in 2018 (estimate)', ylab='Breweries per Capita', main='Overrepresentation of Breweries by Population')
with(mergedpopbrew[1:50,],text(log(breweriespercap)~log(popEST.2018), labels=row.names(mergedpopbrew[1:50,]),pos=4))



montanabreweries<-subset(openbrewerydb_ALL, openbrewerydb_ALL$state=='Montana')
montanabreweries$city<-as.character(montanabreweries$city)
montanabreweries$city<-as.factor(montanabreweries$city)
montcities<-as.data.frame(table(montanabreweries$city))
print(montcities)

vtbreweries<-subset(openbrewerydb_ALL, openbrewerydb_ALL$state=='Vermont')
vtbreweries$city<-as.character(vtbreweries$city)
vtbreweries$<city<-as.factor(vtbreweries$city)
vtcities<-as.data.frame(table(vtbreweries$city))
print(vtcities)
plot(vtcities)
sum(vtcities$Freq)
