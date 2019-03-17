#loading the dataset
library(readr)
fifa <- read_csv("F:/ML/seed/Fifa19/data.csv")
#View(fifa)
summary(fifa)

#---------------------------REMOVING UNNECESSARY LABELS--------------------------#


fifa[ ,c('Flag', 'ID', 'Photo', 'Club Logo', 'Loaned From', 'Work Rate', 'Jersey Number','Real Face', 
         'Body Type', 'Release Clause', 'Special', 'X1','Joined', 'Contract Valid Until')] <- list(NULL)
#View(fifa)
#View(fifa[1:5,])


#---------------------------------DATA CLEANING---------------------------------#

#1. Cleaning the Value and the Wage column by removing the euro sign and converting them
#   numeric according the value stated by character.

# """converting the character wage into numeric and removing euro sign"""
#substituting the euro sign with space
fifa$Wage <- gsub('[???]','',fifa$Wage) 
# spliting the string in two parts, initial string the last symbol(M or k in our case)
fifa$lastW <- sapply(strsplit(as.character(fifa$Wage), ""), tail, 1)  
#data$lastW

library(dplyr)
library(stringr) #library for string operations

# """ gives the numeric eg: 1234, out of ax_1234ytp"""
extracting <- function(z){
  regexp <- "[[:digit:]]+" #prepare regular expression
  str_extract(z, regexp)   #process string
}

#extracting the numbers out of the string
temp1 <- sapply(fifa$Wage, extracting) 
#converting those numbers to numeric
fifa$Wage <- as.numeric(temp1)  
#if the last part is 'M' replace with 1000000 or else with 1000
fifa$Wage <- ifelse(fifa$lastW == "M", fifa$Wage * 1000000, fifa$Wage * 1000) 

#converting the value to numeric (same process with Value column)
fifa$Value <- gsub('[???]','',fifa$Value)
fifa$lastV <- sapply(strsplit(as.character(fifa$Value), ""), tail, 1)
#fifa$lastV

#extracing the numeric and replacing with values (same process as of above for Wage)
temp2 <- sapply(fifa$Value, extracting)
fifa$Value <- as.numeric(temp2)
fifa$Value <- ifelse(fifa$lastV == "M", fifa$Value * 1000000, fifa$Value * 1000)

fifa[ ,c('lastW', 'lastV')] <-list(NULL) #removing them as it adds them to the dataset


#2. Converting the height into numeric format (INCHES)

#converted height to inches, eg = 5'9 is 69 inches 
fifa$Height <- sapply(strsplit(as.character(fifa$Height),"'"), function(x){12*as.numeric(x[1]) + as.numeric(x[2])})


#3. Convertng the weight to numeric (LBS)
fifa$Weight <- gsub('lbs','',fifa$Weight) 
fifa$Weight <- as.numeric(fifa$Weight)


#---------------------MINING & MACHINE LEARNING TECHNIQUE-----------------------------#


#checking the class of our labels
sapply(df, class)


#1. Applying the PCA, for dimension reduction looking for patterns

#removing the characters column, as we are running PCA 
df <- fifa[ -c(1,3,6,9,13)] #removing unnecessary data like characters name
df <- df[, -c(11:36)] # removing unnecessary data
df <- scale(df) # standardizing the data
dim(df) #look at dimension
#pca which will deal with the na values as well, if have any
pc <- princomp(na.omit(df), cor = TRUE)
summary(pc) #look at the proportion of variance for PC selection
df1<-predict(pc) #components after pca
df1 <- df1[,1:5] #retrieving only components as they cover >75% variance
#plot
biplot(pc)
plot(df1, col=3) #finding two clusters 



#2. finding young talent 

#Age versus performance
library(ggplot2)
plot<- ggplot(fifa, aes(fifa$Age, fifa$Overall)) + geom_point() +
  labs(x='Age',y='Performance',title='Age distribution')
plot

#Finding talent
my.fifat <- fifa[(fifa$Age <= 20) & (fifa$Overall >= 80), ]
#new budding talent 
my.fifat2 <- fifa[(fifa$Age <= 17) & (fifa$Potential >= 85), ]

#3. Country with most potential players
# here we are not looking for the numbers but average players by country

fifa %>%
  select(Nationality,Potential) %>%
  group_by(Nationality) %>%
  summarise(m=sum(Potential)) %>%
  arrange(desc(m)) %>%
  head(5) %>%
  ggplot(aes(x=Nationality,y=m)) + geom_bar(stat='identity',fill='#29BF12',color='black') +
  labs(x='Country',y='Potential',title='Countrys total Potential ')

fifa %>%
  select(Nationality,Potential) %>%
  group_by(Nationality) %>%
  summarise(m=mean(Potential)) %>%
  arrange(desc(m)) %>%
  head(5) %>%
  ggplot(aes(x=Nationality,y=m)) + geom_bar(stat='identity',fill='yellow',color='black') +
  labs(x='Country',y='Potential',title='Countrys mean potential')


#4. team that could win UEFA based on statistics of player performance
fifa %>%
  select(Club,Overall) %>%
  group_by(Club) %>%
  summarise(m=mean(Overall)) %>%
  arrange(desc(m)) %>%
  head(4) %>%
  ggplot(aes(x=Club,y=m))+geom_bar(stat='identity',fill='#FF6666',color='blue') +
  labs(x='Club Name',y='Overall',title='Top clubs') 
