# Setting Working Directory 
#setwd("D:/Personal/marketing analytics")
getwd()

# load the required libraries:
#install.packages('arules')
library(arules)
#install.packages('arulesViz')
library(arulesViz)
library(datasets)
library(Matrix)

# load the data: 

#1.1 Converting single format data set into transactions after cleaning data
#following fucntion reads the basket format data
groceries_1 <- read.transactions('groceries.csv',rm.duplicates= TRUE,format ="basket", sep = ",")

#Performing EDA to acertian rules: 
length(groceries_1)
# there are 9835 transcations

# View doesnt work in this case and hence we use inspect to view the data 
inspect(groceries_1[1])
#the first basket contains {citrus fruit,margarine, ready soups,  semi-finished bread}

# Checking the first 10 transaction to check for format issues or check for data loss
inspect(groceries_1[1:10])

#indicates the number of items in each basket reffered to as length
summary(size(groceries_1))


summary(groceries_1)
#there are 9835 transcations and 169 items

#1.2 Check the frequency of items purchased by plotting Item frequency graphs

#Check the frequency of items purchased by plotting Item frequency graphs
itemFrequencyPlot(groceries_1, support = 0.2, main="Item Frequency with 20% support")
# support of 0.2 means the item appears 20% of times in total transcations

#since there was only one item 
#Relaxing the support to 0.1 - to find out itemset which occurs 10% in transcations
itemFrequencyPlot(groceries_1, support = 0.1,main="Item Frequency with 10% support")

# Top N graphs - arranged by support
itemFrequencyPlot(groceries_1, topN = 10,main="Top 10 Items",type="absolute")


# 2.1 Deriving association rules using Apriori recommendation algorithm
#2.2 Setting the minimum support, minimum confidence, minimum length as input parameters.

#Association Rules
groceryrules <- apriori(groceries_1, parameter = list(support = 0.001, confidence = 0.60, minlen = 2))
#support 0.001 means it appears in 0.1 % of the trasncations
#confidende 0f 0.6 means 60% the item is likely to be brought with other item
#len of 2 means only those baskets which has atlest 2 items will be considered
#2918 rules got generated -  which maybe a lot to manage hence increasing the threshold

summary(groceryrules)

groceryrules1 <- apriori(groceries_1, parameter = list(support = 0.001, confidence = 0.80, minlen = 2))

?apriori
summary(groceryrules1)
#410 number of rules

inspect(groceryrules1[1:5])

#sorting the rules by lift
groceryrules1 <- sort(groceryrules1, by="lift")

# 2.3 Obtaining Association rules and pruning them as needed to arrive at a final set of rules

#removing reduandant rules
subset.matrix <- is.subset(groceryrules1, groceryrules1)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
groceryrules.pruned <- groceryrules1[!redundant]

summary(groceryrules.pruned)
inspect(groceryrules.pruned[1:5])

#rules reduced to 370 from 410

#2.4 Analysing the top rules based on lift value
#3.1 Interpret the top 5 rules

groceryrules.pruned <- sort(groceryrules.pruned, by="lift")

inspect(groceryrules.pruned[1:5])

# 3.2 Plot the rules in a graph/scatter plot in R   

plot(groceryrules.pruned,method="scatterplot")
#it creates scatter plot of 370 rules
#suppot on x axis, confience of y axis and the lift incicated by the color


#plots for specific recommendation products
#top 5 rules by lift
top5subRules_lift <- head(groceryrules.pruned, n = 5, by = "lift")
plot(top5subRules_lift, method = "graph",  engine = "htmlwidget")

#top 5 rules by confidence
top5subRules_confidence <- head(groceryrules.pruned, n = 5, by = "confidence")
plot(top5subRules_confidence, method = "graph",  engine = "htmlwidget")

#top 5 rules by support 
top5subRules_support <- head(groceryrules.pruned, n = 5, by = "support")
plot(top5subRules_support, method = "graph",  engine = "htmlwidget")

# Observations based on Market Basket analysis

whole.milk.sub <- subset(groceryrules.pruned, subset = rhs %in% "whole milk")
summary(whole.milk.sub)

#There are 224 rules out of 370 rules in which the recommendation is "whole milk"

other.vegetable.sub <- subset(groceryrules.pruned, subset = rhs %in% "other vegetables")
summary(other.vegetable.sub)

#There are 122 rules out of 370 rules in which the recommendation is "whole milk"

#majority of the rules 346 pertian to whole milk and other vegetables
root.vegetable.sub <- subset(groceryrules.pruned, subset = rhs %in% "root vegetables")
inspect(root.vegetable.sub)

eggs.vegetable.sub <- subset(groceryrules.pruned, subset = lhs %in% "domestic eggs")
summary(eggs.vegetable.sub)


confidence_one <- subset(groceryrules.pruned, confidence > 0.99)
summary(confidence_one)
inspect(confidence_one)

# 4) Actionable Insights and Recommendations

#The assoicate rules can be used to plan the shelf place
# for example whole milk and other vegetables can be kept at a prominent place

#the association rules can be used to build recommendation system - in the online retail form, which acts as reminder and liked by consumers
#here high lift values can be considered

#assocation rules can desing promotional campaign
#this could infact be for rules which have lower confidence value - as they are not always picked-up
#pickign out rules with hign confidence will not be fruitful as in any case consumers are going to buy those items

#filtering the association rules, the retailer should be able to find the associate elements
#for exmaple 
citrus.fruit.sub <- subset(groceryrules.pruned, subset = lhs %in% "citrus fruit" & confidence > 0.85)
summary(citrus.fruit.sub)
plot(citrus.fruit.sub, method = "graph",  engine = "htmlwidget")

# there are 25 rules which has citrus fruit as antecedent items which can used for product bundling or other schemes


#rules with low confidence but high lift
groceryrules2 <- apriori(groceries_1, parameter = list(support = 0.001, confidence = 0.20, minlen = 2))
low_confidence <- subset(groceryrules2, confidence < 0.4, lift > 10)
summary(low_confidence)

top5subRules_low_confidence <- head(low_confidence, n = 5, by = "lift")
plot(top5subRules_low_confidence, method = "graph",  engine = "htmlwidget")