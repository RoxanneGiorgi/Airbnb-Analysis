#import the dataset
air = read.csv("Airbnb2.csv")
air = air[,-c(1)] 

# removing the NAs 
which(is.na(air))
any(is.na(air))
air = na.omit(air)
any(is.na(air))

#Removing the 3 zipcodes
air = air[!grepl(75116, air$zipcode),]
air = air[!grepl(92130, air$zipcode),]
air = air[!grepl(95170, air$zipcode),]

#now we should have 833 obs

air$square_meter = air$square_feet*0.32048 #creating square meter column

# Changing some columns to factors
air$zipcode = as.factor(air$zipcode)
air$bedrooms = as.factor(air$bedrooms)
air$property_type = as.factor(air$property_type)

#Turning columns 26, 27, 29, 30 to numerics (theyre prices right now)
#removing the dollar sign
air$price <- as.numeric(gsub("[$,]", "", air$price))
air$security_deposit <- as.numeric(gsub("[$,]", "", air$security_deposit))
air$cleaning_fee <- as.numeric(gsub("[$,]", "", air$cleaning_fee))
air$extra_people <- as.numeric(gsub("[$,]", "", air$extra_people))

####### Graph 1: Property Type per district

g <- ggplot(air, aes(zipcode))

g + geom_bar(aes(fill=property_type), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Property Type", 
       subtitle="Property Types per District") 

####### Graph 2: Word Plot of Description
#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#turn description into a corpus
text = air$description
docs <- Corpus(VectorSource(text))
inspect(docs)

# Text Transformation
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
#docs <- tm_map(docs, removeWords, c("paris", "apartment","room","bed","kitchen","floor","located","bathroom","bedroom","flat")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)


# Build term document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Make the word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, scale=c(3.5,0.25),
          colors=brewer.pal(8, "Dark2"))
##### Graph 3: Find words that occur at least 4 times
findFreqTerms(dtm, lowfreq = 4)
# what words are associated with living
findAssocs(dtm, terms = "living", corlimit = 0.3)

head(d, 10)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

####### Graph 4: Boxplot Price and Response time
theme_set(theme_classic())
attach(air)
# Plot
install.packages("ggthemes")
library(ggthemes)
library(ggplot2)
g <- ggplot(air, aes(host_response_time, price))
g + geom_boxplot(aes(fill= price))+theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Price vs. Host Response Time ", 
       x="Host Response Time",
       y="Price") + scale_y_continuous(name="Price", limits=c(0, 600))

### Graph 5: Diverging Bar Plot
# Diverging Barcharts
ggplot(air, aes(x=price, y=zipcode)) + 
  geom_bar(stat='identity', aes(fill=price), width=.5)  +
  scale_fill_manual(name="Price", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
       labs(title= "Diverging Bars") + 
  coord_flip()

############# R Shiny #############
install.packages("shiny")
library(shiny)
 

setView(lng=2.3522, lat=48.8566 , zoom=10)