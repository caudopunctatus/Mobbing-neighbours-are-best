
getwd()

testpairdata=read.csv("L:\\Januar 2016_top aktuell\\Damaris\\Damaris\\Wiederaufnahme\\2016_11_09\\mobbing_light_protocol_comments_refreshed09_2016.csv")

#create variables: 
#T-> Testpair; 
#build -> buildinglevel(0=complete nest); att-> attacks end presentation; app-> approaches end presentation
#mobb/nonmobb-> next to mobber or nonmobber
Tbuildmobb=as.numeric(as.character(read.csv("L:\\Januar 2016_top aktuell\\Damaris\\Damaris\\Wiederaufnahme\\2016_11_09\\mobbing_light_protocol_comments_refreshed09_2016.csv", header=TRUE, sep=",")[c(1,2,3,4,5,6,7,8,9,10,11,12,13),10]))
Tbuildnonmobb=as.numeric(as.character(read.csv("L:\\Januar 2016_top aktuell\\Damaris\\Damaris\\Wiederaufnahme\\2016_11_09\\mobbing_light_protocol_comments_refreshed09_2016.csv", header=TRUE, sep=",")[c(1,2,3,4,5,6,7,8,9,10,11,12,13),11]))
Tattmobb=as.numeric(as.character(read.csv("L:\\Januar 2016_top aktuell\\Damaris\\Damaris\\Wiederaufnahme\\2016_11_09\\mobbing_light_protocol_comments_refreshed09_2016.csv", header=TRUE, sep=",")[c(1,2,3,4,5,6,7,8,9,10,11,12,13),12]))
Tattnonmobb=as.numeric(as.character(read.csv("L:\\Januar 2016_top aktuell\\Damaris\\Damaris\\Wiederaufnahme\\2016_11_09\\mobbing_light_protocol_comments_refreshed09_2016.csv", header=TRUE, sep=",")[c(1,2,3,4,5,6,7,8,9,10,11,12,13),14]))
Tappmobb=as.numeric(as.character(read.csv("L:\\Januar 2016_top aktuell\\Damaris\\Damaris\\Wiederaufnahme\\2016_11_09\\mobbing_light_protocol_comments_refreshed09_2016.csv", header=TRUE, sep=",")[c(1,2,3,4,5,6,7,8,9,10,11,12,13),13]))
Tappnonmobb=as.numeric(as.character(read.csv("L:\\Januar 2016_top aktuell\\Damaris\\Damaris\\Wiederaufnahme\\2016_11_09\\mobbing_light_protocol_comments_refreshed09_2016.csv", header=TRUE, sep=",")[c(1,2,3,4,5,6,7,8,9,10,11,12,13),15]))


#check Data
data.frame(Tbuildmobb,Tbuildnonmobb,Tattmobb,Tattnonmobb,Tappmobb,Tappnonmobb)

#statistical test
#binomial test, 11 pairs next to mobber, 2 next to non mobber
binom.test(c(11,2),p=1/2)
binom.test(11,13,p=0.5) #same like line before

#Wilcoxon nest level -> welche einstellungen??
wilcox.test(Tbuildmobb,Tbuildnonmobb,paired=TRUE,correct = FALSE)

wilcox.test(Tattnonmobb,Tattmobb,paired=TRUE)

wilcox.test(Tappnonmobb,Tappmobb,paired=TRUE)

#Figures
boxplot(Tbuildmobb,Tbuildnonmobb,main="Nest building of the focal pair",ylab="Building category")

boxplot(Tattmobb,Tattnonmobb,main="Nest defense of the focal pair",ylab="Number of attacks")

boxplot(Tappmobb,Tappnonmobb,main="Nest defense of the focal pair",ylab="Number of approaches")


#verzweiflungstests

boxplot(Tbuildmobb,Tbuildnonmobb)


test=data.frame(Tbuildmobb,Tbuildnonmobb)
boxplot(test)

?boxplot

mydata = read.csv("D:\\mobbing\\Punc statistic 2016\\mobbing_light_protocol_comments_refreshed09_2016.csv")

