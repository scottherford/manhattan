# Author: Benjamin Reddy
# Taken from pages 49-50 of O'Neil and Schutt

library(plyr)

# http://www1.nyc.gov/site/finance/taxes/property-rolling-sales-data.page


# read csv file
manhattan <- read.csv("data/rollingsales_manhattan.csv",skip=4,header=TRUE)

## Check the data
head(manhattan)
summary(manhattan)
str(manhattan) # Very handy function!
#Compactly display the internal structure of an R object.


## clean/format the data with regular expressions
## More on these later. For now, know that the
## pattern "[^[:digit:]]" refers to members of the variable name that
## start with digits. We use the gsub command to replace them with a blank space.
# We create a new variable that is a "clean' version of sale.price.
# And sale.price.n is numeric, not a factor.
manhattan$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","", manhattan$SALE.PRICE))
count(is.na(manhattan$SALE.PRICE.N))

names(manhattan) <- tolower(names(manhattan)) # make all variable names lower case

## Get rid of leading digits
manhattan$gross.sqft <- as.numeric(gsub("[^[:digit:]]","", manhattan$gross.square.feet))
manhattan$land.sqft <- as.numeric(gsub("[^[:digit:]]","", manhattan$land.square.feet))
manhattan$year.built <- as.numeric(as.character(manhattan$year.built))

## do a bit of exploration to make sure there's not anything
## weird going on with sale prices
attach(manhattan)
hist(sale.price.n) 
detach(manhattan)

## keep only the actual sales

manhattan.sale <- manhattan[manhattan$sale.price.n!=0,]
png("analysis/grosssqftvssaleprice.png")
plot(manhattan.sale$gross.sqft,manhattan.sale$sale.price.n)
dev.off()
png("analysis/log10grosssqftvssaleprice.png")
plot(log10(manhattan.sale$gross.sqft),log10(manhattan.sale$sale.price.n))
dev.off()

## for now, let's look at 1-, 2-, and 3-family homes
manhattan.homes <- manhattan.sale[which(grepl("FAMILY",manhattan.sale$building.class.category)),]
dim(manhattan.homes)
png("analysis/log10grosssqftvssaleprice-123familyhomes.png")
plot(log10(manhattan.homes$gross.sqft),log10(manhattan.homes$sale.price.n))
dev.off()
summary(manhattan.homes[which(manhattan.homes$sale.price.n<100000),])
""

## remove outliers that seem like they weren't actual sales
manhattan.homes$outliers <- (log10(manhattan.homes$sale.price.n) <=5) + 0
manhattan.homes <- manhattan.homes[which(manhattan.homes$outliers==0),]
png("analysis/grosssqftvssaleprice-removedoutliers.png")
plot(log10(manhattan.homes$gross.sqft),log10(manhattan.homes$sale.price.n))
dev.off()