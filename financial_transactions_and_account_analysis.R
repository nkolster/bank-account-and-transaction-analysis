# Loading libraries used later
library(xts)
library(quantmod)

setwd("~/datafolder")

# Reading in the categories
cats <- read.table("categories.csv", header=T, sep=",")

# Reading in the transactions
tx <- read.table("anonymized-test-transactions.csv", header=T, sep=",",stringsAsFactors=F)

# Category id less than 27 means the data has been classified and can be used as training data.
tr <-tx[tx$category_id < 27,]

# Category 27 means the data has not been classified so it is the test data. 
test <- tx[tx$category_id == 27,]

# Now the data has been split up in training (tr) and test (test) data.

# Rows in tr
nrow(tr)
# 14892
 nrow(test)
# 21454
nrow(tr)/nrow(tx)
# 0.4097287
# 40% Of the data is classified

hist(tr$category_id)
# Getting initial understanding of the distributions between categories
# http://i.imgur.com/tYVOhdX.png


sort(table(tr$category_id), decreasing=T)
# 26   10   17   13   24   12   23   18   14   11   16   25   21   22   19   20   15 
# 4905 2907 2555 1451  596  509  503  269  235  215  215  191  153   91   50   27   20 
# Some large categories, will focus first on them


# Looking at the contact_id's 
head(sort(table(tr$contact_id), decreasing=T))
# 7670951 7669873 7671704 7679644 7669881 7670534 
# 270     159     145     140     119     116 

View(tr[tr$contact_id==7670951,])
# Some i-tunes are in media, some in sonstiges. Why?

View(tr[tr$contact_id==7669873,])
#All in sonstiges

View(tr[tr$contact_id==7671704,])
# All in sonstiges
# User bank account varies for this. Is that possible for the same contact ID?


View(tr[tr$category_id==26,])
# This is not a very intresting category although the largest. Also has i-tunes for some reason.

View(tr[tr$category_id==10,])
# Bargeld are even numbers and contain UHR

View(tr[tr$category_id==17,])
# Lebensmittel is large but at first look no clear tell-tales

View(tr[tr$category_id==13,])
# Contains EREF etc. Mabye contact id can help here, could check.

# Looking into forming straightforward rule for bargeld.
# In Training data transaction_type has either geldautomat or auszahlung for most items in Bargeld.

#Category 10
tr$geldautomat_or_auszahlung <- ( grepl("geldautomat", tr$transaction_type, ignore.case=T) | grepl("auszahlung", tr$transaction_type, ignore.case=T) )
# Could still add if the amount is negative and a round number but dont have time for that now
View(tr[tr$geldautomat_or_auszahlung == F & tr$category_id == 10,])
#Around 80 from training data not caught by this, for this time and effort that is ok

# Applying the rule for test data.
test$geldautomat_or_auszahlung <- ( grepl("geldautomat", test$transaction_type, ignore.case=T) | grepl("auszahlung", test$transaction_type, ignore.case=T) )
test$category_id <- ifelse(test$geldautomat_or_auszahlung, 10, 27)


View(test[test$category_id == 10,])
nrow(test[test$category_id == 10,])
# [1] 14 
# Classified a whole of 14 operations!! wohoo:)


#Ok, assuming training and test data is not similarly distributed so changing approach a little.

# We take a look at the frequency of the words occurring in the usage field 
usagestring <- paste(test$usage, collapse='')
words.list <- strsplit(usagestring, "\\W+", perl=TRUE)
words.vector <- unlist(words.list)
freq.list <- table(words.vector)

head(sort(freq.list, decreasing=T), n=300)

words <- head(sort(freq.list, decreasing=T), n=300)
words <- as.matrix(words)

#filtering out those that are only numeric since we dont have the time to look for patterns in those.
words_test <- words[is.na(as.numeric(rownames(words))),]

#we get this list of frequencys of words occurring in the usage field:
http://i.imgur.com/EN9JDyy.png

# Some of the words such reiseburo, Internet, telefone, Vodafone, Itunes, Miete etc. etc. can immediately help us categorize the transactions easily. 
# Some are harder such as Amazon where you can buy many different things such as electronics or books.
# From a data-science perspective these simple cases are however not particularly interesting, although they are important.
#
# Liebe can also be interpreted as many different things, would be interesting to look into this.
# With more time would make the word frequency distribution also of-course case-insensitive

# Now we have an idea of the word-frequency in the test data-set. The training and test data-set seem to be somewhat differently composed however.
# Therefore it is a good idea to look at both data-sets to get an idea of what we can use to learn from.
# At this point we there need to take a look at the training data-set to get an overview, which has the already classified items.

usagestring <- paste(tr$usage, collapse='')
words.list <- strsplit(usagestring, "\\W+", perl=TRUE)
words.vector <- unlist(words.list)
freq.list <- table(words.vector)

head(sort(freq.list, decreasing=T), n=300)

words <- head(sort(freq.list, decreasing=T), n=300)
words <- as.matrix(words)

words[is.na(as.numeric(rownames(words))),]

# The word frequencies in the test-data are quite different from the training data
# http://i.imgur.com/rFhqF3X.png
# We will focus on the most frequently occurring terms in the test data to focus on the features that potentially can classify most items.
# That way we can stand a change of getting the most bang for the buck, that is classified items per time/effort.


# First question: Is the frequently occurring term SVWZ significant in determining the category?
tr$SVWZ <- grepl("svwz", tr$usage, ignore.case=T)
#View(tr[tr$SVWZ,])
table(tr[tr$SVWZ,]$category_id)
10  11  12  13  14  15  16  18  19  20  22  23  24  25  26 
8   3   5 340   4  15  67  26  41   8  23  31 159  68 216 

length(tr[tr$SVWZ,]$category_id)
#[1] 1014
# So it gives 30% chance of the category_id being 13 (Handy and internet)


# Lets write a function for this:
findWord <- function(word, data) {
  data$foundWord <- grepl(word, data$usage, ignore.case=T)
  data  
}


# Getting a feel for how important the most frequently occurring words in the test-data have been in the training data
# Taking 10 here as an arbitrary number, they seem to matter all the way to 250

for (i in 1:100) {
  term <- names(words_test[i])
  cat(" \n term: ", term, " \n ")
  data <- findWord(term, tr)
  distribution <- table(data[data$foundWord,]$category_id)
  cat("Distribution in training data: ")
  print(sort(distribution, decreasing=T))
  cat("Total occurrences: ", sum(distribution), " \n ")
}

# Some of these terms have explanatory power based on the training data.

# For example 2/3 of the transactions containing NR are classified in category 13 (Handy & Internet)
# term:  NR  
# Distribution in training data: 
#   13   26   18   24   10   22   23   25   11   20   12   16   17   14   19 
# 1007  232  146   70   44   38   20   17    5    3    2    2    2    1    1  
# Total occurrences:  1590  

# Some other are quite interesting as well.
# Barauszahlung sounds obvious but it is not present in training data so it is then discretionary.

# Combining these factors that have some explanatory power we can try forming a model to classify the transactions based on the training data.


# Engineering features based on the terms
# The number of terms can be higher when the amount of data is increased.
# Now we choose a low number since time is limited to choose the most significant ones.

tr <-tx[tx$category_id < 27,]
for (i in 1:40) {
  term <- names(words_test[i])
  data <- findWord(term, tr)
  tr <- cbind(tr, as.factor(data$foundWord))
  colnames(tr)[length(colnames(tr))] <- term
}

# Some candidates to use for a RF classifier: transaction_type, amount 
# Some words that intuitively have explanatory power: gehalt miete lohn etc.
# Unfortunately cannot be used for machine learning because training data does not have it: auszahlung
colnames(tr)

dataformodel <- tr[,(16:length(colnames(tr)))]
dataformodel$amount <- tr$amount
dataformodel$contact_id <- tr$contact_id
#dataformodel$transaction_type <- tr$transaction_type
dataformodel$category_id <- as.factor(tr$category_id)

cat(colnames(dataformodel))
library(randomForest)

fit <- randomForest(category_id ~ amount + contact_id,
                    data=dataformodel, importance=TRUE )

fit <- randomForest(category_id ~ . ,                   data=dataformodel, importance=TRUE )

fit 

importance(fit)
# http://i.imgur.com/Xxh93pI.png



# GLM
fitglm <- glm(category_id ~ ., family = binomial(logit),
              data=dataformodel)

# Based on this quick excercise we are able to explain 65% of the classifications in the training set.
# However: 
# Excercise done very quickly and many shortcuts taken that would amount to better quality analysis in real life.
# Probably a healthy dose of overfitting is present, but it will be for later excercises to increase amount of data and work against that.


# 
# fit <- randomForest(category_id ~ amount + contact_id + SVWZ,
#                     data=tr, importance=TRUE )


# Applying the model to test data
for (i in 1:20) {
  term <- names(words_test[i])
  data <- findWord(term, test)
  test <- cbind(test, as.factor(data$foundWord))
  colnames(test)[length(colnames(test))] <- term
}

test$predicted <- predict(fit, test)

head(test)

# In this excercise we constructed a simple and fast model for the classification of transactions.
# To improve with more time:
# Word frequency calculations should be case insensitive
# Use other characters also as separator, such as "+" for example
# Many quick wins could probably be achieved by more manual labor, eg. barauszahlung and other more discretionary measures.
# 

