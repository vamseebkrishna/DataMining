actor <- read.delim("1999-2008-actor.txt", header = FALSE, sep = ";" ,col.names = c("tid", "actorname"), strip.white = TRUE)
genre <- read.delim("1999-2008-genre.txt", header = FALSE, sep = ";", col.names = c("tid", "moviegenre", "year"), strip.white = TRUE)

library(stringr)
library(dplyr)
library(textclean)
library(arules)
library(arulesViz)
library(plyr)
library(knitr)
library(lubridate)
library(data.table)
library(tidyverse)

str_detect(actor$actorname, "\\?")
# Replace patters with 'NA'
actor$actorname = gsub("\\?", NA, actor$actorname)
#Remove NA rows
actor <- na.omit (actor, "actorname")

actor.genre <- merge(actor, genre, by = "tid")
data <- subset(actor.genre, year!= '1999')

#combining tuples with same tids to one -- not used
#transactionData <- ddply(actor.genre, c("tid"), function(actor.genre)paste(actor.genre$actorname, collapse = ','))
#transactionData
#agbasketformwithdup <- merge(transactionData, actor.genre[ ,c("tid" , "year", "moviegenre")], by = "tid" )
#View(agbasketformwithdup)

#write.csv(agbasketformwithdup, "transactions.csv", row.names = FALSE)
#remove tid before rule mining
#agbasketformwithdup$tid <- NULL
#dropping duplicate tuples
#write.csv((agbasketformwithdup %>% distinct()), "transactions.csv", row.names = FALSE)

data$tid <- NULL
write.csv(data , "transactions.csv", row.names = FALSE)

#trans <- as(actor.genre $tid , "transactions")
agbasketform <- read.csv("transactions.csv")
trans1 <-  read.transactions("transactions.csv", format = 'basket', sep = ',', header = FALSE, skip =1)
trans1
summary(trans1)
class(trans1)
inspect(trans1)

#trans<-as(rules1,"transactions")
#inspect(head(trans,3))

ruleset_1 <- apriori(trans1, parameter = list(supp =0.00006, conf = 0.5,target="rules"))

ruleset_2 <- apriori(trans1, parameter = list(supp =0.00006, conf = 0.7,target="rules"))

ruleset_3 <- apriori(trans1, parameter = list(supp =0.00006, conf = 0.8,target="rules"))

ruleset_4 <- apriori(trans1, parameter = list(supp =0.00009, conf = 0.5,target="rules"))

ruleset_5 <- apriori(trans1, parameter = list(supp =0.00009, conf = 0.7,target="rules"))

ruleset_6 <- apriori(trans1, parameter = list(supp =0.00009, conf = 0.8,target="rules"))

ruleset_7 <- apriori(trans1, parameter = list(supp =0.000014, conf = 0.5,target="rules"))

ruleset_8 <- apriori(trans1, parameter = list(supp =0.000014, conf = 0.7,target="rules"))

ruleset_9 <- apriori(trans1, parameter = list(supp =0.000014, conf = 0.8,target="rules"))



inspect(sort(ruleset_1))

plot(ruleset_1, main = "Ruleset_1",control=list(col=sequential_hcl(100)))
plot(ruleset_2, main = "Ruleset_2",control=list(col=sequential_hcl(100)))
plot(ruleset_3, main = "Ruleset_3",control=list(col=sequential_hcl(100)))
plot(ruleset_4, main = "Ruleset_4",control=list(col=sequential_hcl(100)))
plot(ruleset_5, main = "Ruleset_5",control=list(col=sequential_hcl(100)))
plot(ruleset_6, main = "Ruleset_6",control=list(col=sequential_hcl(100)))
plot(ruleset_7, main = "Ruleset_7",control=list(col=sequential_hcl(100)))
plot(ruleset_8, main = "Ruleset_8",control=list(col=sequential_hcl(100)))
plot(ruleset_9, main = "Ruleset_9",control=list(col=sequential_hcl(100)))


# Lift analysis > 1
Lift_rules_2 = subset(ruleset_2, lift > 1)
#print top-5  in descending order of lift
inspect(head(sort(Lift_rules_2, by="lift"), 5))
summary(Lift_rules_1)

plot(head(sort(Lift_rules_9, by="lift"), 5), main = "Lift > 1 Ruleset_8", control=list(col=sequential_hcl(100)))


# Lift analysis = 1
Lift_rules_3 = subset(ruleset_1, lift = 1)
#print top-5  in descending order of lift

inspect(head(sort(Lift_rules_3, by="lift"), 5))
summary(Lift_rules_1)

# Lift analysis < 1
Lift_rules_1 = subset(ruleset_3, lift < 1)
#print top-5  in descending order of lift
inspect(tail(sort(Lift_rules_1, by="lift"), 5))
summary(Lift_rules_1)





#trans1 <- as(rules1, 'data.frame')
#inspect(head(trans,3))

fqit_1_IT1 <- apriori(trans1, parameter = list(supp =0.00006,target="frequent itemsets", minlen= 1, maxlen =1))

fqit_1_IT2 <- apriori(trans1, parameter = list(supp =0.00006,target="frequent itemsets", minlen= 2, maxlen =2))

fqit_1_IT3 <- apriori(trans1, parameter = list(supp =0.00006,target="frequent itemsets", minlen= 3, maxlen =3))

fqit_1_IT4 <- apriori(trans1, parameter = list(supp =0.00006,target="frequent itemsets", minlen= 4, maxlen =4))


#inspect(sort(fqit_1))


fqit_2_IT1 <- apriori(trans1, parameter = list(supp =0.00009,target="frequent itemsets", minlen= 1, maxlen =1))

fqit_2_IT2 <- apriori(trans1, parameter = list(supp =0.00009,target="frequent itemsets", minlen= 2, maxlen =2))

fqit_2_IT3 <- apriori(trans1, parameter = list(supp =0.00009,target="frequent itemsets", minlen= 3, maxlen =3))

fqit_2_IT4 <- apriori(trans1, parameter = list(supp =0.00009,target="frequent itemsets", minlen= 4, maxlen =4))

#inspect(sort(fqit_2))

fqit_3_IT1 <- apriori(trans1, parameter = list(supp =0.000014, target="frequent itemsets", minlen= 1, maxlen =1))

fqit_3_IT2 <- apriori(trans1, parameter = list(supp =0.000014, target="frequent itemsets", minlen= 2, maxlen =2))

fqit_3_IT3 <- apriori(trans1, parameter = list(supp =0.000014, target="frequent itemsets", minlen= 3, maxlen =3))

fqit_3_IT4 <- apriori(trans1, parameter = list(supp =0.000014, target="frequent itemsets", minlen= 4, maxlen =4))
#inspect(sort(fqit_3))

#candidate itemsets

cnit_1_IT1 <- apriori(trans1, parameter = list(supp =0.000006,target="frequent itemsets", minlen= 1, maxlen =1))

cnit_1_IT2 <- apriori(trans1, parameter = list(supp =0.000006,target="frequent itemsets", minlen= 2, maxlen =2))

cnit_1_IT3 <- apriori(trans1, parameter = list(supp =0.000006,target="frequent itemsets", minlen= 3, maxlen =3))

cnit_2_IT1 <- apriori(trans1, parameter = list(supp =0.000009,target="frequent itemsets", minlen= 1, maxlen =1))

cnit_2_IT2 <- apriori(trans1, parameter = list(supp =0.000009,target="frequent itemsets", minlen= 2, maxlen =2))

cnit_2_IT3 <- apriori(trans1, parameter = list(supp =0.000009,target="frequent itemsets", minlen= 3, maxlen =3))

cnit_3_IT1 <- apriori(trans1, parameter = list(supp =0.0000014, target="frequent itemsets", minlen= 1, maxlen =1))

cnit_3_IT2 <- apriori(trans1, parameter = list(supp =0.0000014, target="frequent itemsets", minlen= 2, maxlen =2))

cnit_3_IT3 <- apriori(trans1, parameter = list(supp =0.0000014, target="frequent itemsets", minlen= 3, maxlen =3))

