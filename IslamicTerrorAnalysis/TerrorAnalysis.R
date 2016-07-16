##Data Obtained from www.TheReligionOfPeace.com

##Pulling in Data from CSV scraped from website
attacks <- read.csv("~/IslamicTerror/2016.csv", stringsAsFactors = FALSE, header = FALSE)
attacks <- rbind(attacks, read.csv("~/IslamicTerror/2015.csv", stringsAsFactors = FALSE, header = FALSE))
attacks <- rbind(attacks, read.csv("~/IslamicTerror/2014.csv", stringsAsFactors = FALSE, header = FALSE))
attacks <- rbind(attacks, read.csv("~/IslamicTerror/2013.csv", stringsAsFactors = FALSE, header = FALSE))
attacks <- rbind(attacks, read.csv("~/IslamicTerror/2012.csv", stringsAsFactors = FALSE, header = FALSE))
attacks <- rbind(attacks, read.csv("~/IslamicTerror/2011.csv", stringsAsFactors = FALSE, header = FALSE))

##Organizing to see the top ten countries with the most Terror Attacks since 2011
attacksByCountry <- as.data.frame(table(attacks$Country))
attacksByCountry <- attacksByCountry[order(attacksByCountry$Freq,decreasing = TRUE),]
attacksByCountry <- attacksByCountry[1:10,]
##Graph attacksByCountry
ggplot(attacksByCountry, aes(x = attacksByCountry$Var1, y = attacksByCountry$Freq)) + geom_bar(stat = "identity")+xlab("Country")+ylab("Number of Attacks")+ggtitle("Islamic Terror Attacks: Top 10 Countries 2011-2016")

##Organizing to see the top ten countries with the most Terror Deaths since 2011
countries <- as.character(unique(attacks$Country))
deaths <- NULL
for(i in 1:length(countries)) {
  temp <- deathsByCountry$Killed[which(deathsByCountry$Country == countries[i])]
  deaths[i] <- sum(temp)
}

deathsPerCountry <- as.data.frame(deathsPerCountry, stringsAsFactors = FALSE)
deathsPerCountry$deaths <- as.numeric(deathsPerCountry$deaths)
colnames(deathsPerCountry) <- c("countries", "deaths")
deathsPerCountry <- deathsPerCountry[order(deathsPerCountry$deaths, decreasing = TRUE),]
deathsPerCountry <- deathsPerCountry[1:10,]
##Graph deathsPerCountry 
ggplot(deathsPerCountry, aes(x = deathsPerCountry$countries, y = deathsPerCountry$deaths)) + geom_bar(stat = "identity")+xlab("Country")+ylab("Total Deaths From Islamic Terrorism")+ggtitle("Islamic Terror Attacks: Deaths By Country since 2011")

attacks$Date <- strptime(attacks$Date, format = "%Y.%m.%d")

attacks <-attacks[order(attacks$Date),]

ggplot(attacks, aes(x=attacks$Date,y=cumsum(attacks$Killed))) +geom_line() + xlab("Time") + ylab("Deaths by Islamic Terror Attacks") + ggtitle("Cumulative Deaths from Islamic Terrorism since 2011")

library(tm)
library(SnowballC)
library(wordcloud)

terrorCorpus <- Corpus(VectorSource(attacks$Description))
terrorCorpus <- tm_map(terrorCorpus,PlainTextDocument)
terrorCorpus <- tm_map(terrorCorpus, removeWords, c(stopwords('english'),"one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"))
wordcloud(terrorCorpus, max.words = 200, random.order = FALSE)


