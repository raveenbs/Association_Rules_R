
#############  Association Rules ##################

###################################################
# Step 2: Set the Working Directory and install the packages
###################################################
setwd("~/raveen/")
library('arules')
library('arulesViz')

###################################################
#Step 3: Read in the Data for Modeling
###################################################
df_groceries <- read.csv("Groceries_dataset.csv")
View(df_groceries)

###################################################
#Step 4:Sort and Review the Data
###################################################

df_groceries$Date <- as.Date(df_groceries$Date, format = "%d-%m-%Y")
df_sorted <- df_groceries[with(df_groceries,order(Member_number,Date)),]
#convert member number to numeric
df_sorted$Member_number <- as.numeric(df_sorted$Member_number)
#convert item description to categorical format
df_sorted$itemDescription <- as.factor(df_sorted$itemDescription)
View(df_sorted)

###################################################
#Step 5:Convert dataframe to transaction format
###################################################
#detach dplyr package
if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
  detach(package:dplyr, unload=TRUE)
}

#group all the items that were bought together; by the same customer on the same date
library(plyr)
df_itemList <- ddply(df_groceries, c("Member_number","Date"), 
                     function(df1)paste(df1$itemDescription,collapse = ","))

###################################################
#Step 6:Remove columns except Itemlist and save data
###################################################

#remove member number and date
df_itemList$Member_number <- NULL
df_itemList$Date <- NULL

#assign column name
colnames(df_itemList) <- c("itemList")

#write to csv format
write.csv(df_itemList,"ItemList.csv", quote = FALSE, row.names = TRUE)


###################################################
#Step 7: Read in the Data for Modeling
###################################################
#load package required
library(arules)

#convert csv file to basket format
txn = read.transactions(file="ItemList.csv", rm.duplicates= FALSE, 
                        format="basket",sep=",",cols=1);

#remove quotes from transactions
txn@itemInfo$labels <- gsub("\"","",txn@itemInfo$labels)

txn@itemInfo

#visualize the first 100 transactions
image(txn[1:100,])

#bar plot
itemFrequencyPlot(txn, topN =5)

###################################################
#Step 8: Mine the Association Rules
###################################################

#run apriori algorithm
basket_rules <- apriori(txn,parameter = list(minlen=2,sup = 0.001, conf = 0.01, target="rules"))

#view rules
inspect(basket_rules)

#Alternative to inspect() is to convert rules to a dataframe and then use View()
df_basket <- as(basket_rules,"data.frame")
View(df_basket)


###################################################
#Step 9: Plot the rules
###################################################

plot(basket_rules)

plot(basket_rules[1:10,], method="graph", control=list(type="items"))

itemFrequencyPlot(txn, topN = 5)

# Visualize rules as a scatter plot 
plot (basket_rules[1:10,], control=list(jitter=2))

###################################################
#Step 10: Extract the best 3 Rules
###################################################
rules_high_lift <- head(sort(basket_rules, by="lift"), 3)
inspect(rules_high_lift)
plot(rules_high_lift, method="graph", control=list(type="items"))

