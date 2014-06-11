housing <- read.table("http://www.jaredlander.com/data/housing.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

names(housing) <- c("Neighbourhood", "Class", "Units", "YearBuilt", "SqFt", "Income", "IncomePerSqFt", "Expense",  
                    "ExpensePerSqFT", "NetIncome", "Value", "ValuePerSqFt", "Boro" )
head(housing)

summary(housing)


# strategy
#1 full data audit and visuals of potential outliars
#   - summary(dset) is very good
#2 look to see if log / exp transformations would be useful
#3 need to interpret what the graphs are telling us (are they bi-model, what is the impact of such)
#4 Automate the process

#plots

#histogram
ggplot(housing, aes(x=ValuePerSqFt)) + geom_histogram(binwidth=10) + labs(x = "Value Per Square Foot")

#bimodel distribution

#coloured overlapping histogram

ggplot(housing, aes(x=ValuePerSqFt, fill= Boro)) + geom_histogram(binwidth = 10) + labs(x="Value Per Square Foot")

#brooklin is cheap, so is bronx, others are obsured

#coloured independent histograms

ggplot(housing, aes(x=ValuePerSqFt, fill= Boro)) + geom_histogram(binwidth = 10) + labs(x="Value Per Square Foot") +
    facet_wrap(~Boro)

#multiple regression

house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data = housing) 
summary(house1)

# look at coef

house1$coefficients
coefficients(house1)
require(coefplot)
coefplot(house1)

# try with multplicative relations

# * instead of + includes the interaction AND the individual variables being included
# : instead of + just includes the interaction terms

house2 <- lm(ValuePerSqFt ~ Units * SqFt + Boro, data = housing)
coefplot(house2)
house3 <- lm(ValuePerSqFt ~ Units : SqFt + Boro, data = housing)
coefplot(house3)
summary(house2)
house4 <- lm(ValuePerSqFt ~ Units * SqFt * Income, data = housing)
coefplot(house4)
summary(house4)

house5 <- lm(ValuePerSqFt ~ Class * Boro, data = housing)
coefplot(house5)
summary(house5)

# sqft and units appear uselss, apparently got to test their ratio - need a function to do this. I
#I means 'do this maths opperation first'

house6 <- lm(ValuePerSqFt ~ I(SqFt/Units) + Boro, housing)
coefplot(house6)
summary(house6)

# very very good for comparing coefficients 
multiplot(house1, house2, house3)

#bring in new data, See how the data fits

housingNew <- read.table("http://jaredlander.com/data/housingNew.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

head(housingNew)

housingPredict <- predict(house1, newdata = housingNew, se.fit = TRUE, interval = "prediction", level = .95)


# find the proportion of Boroughs - proc freq

prop.table(table(housing$Boro))
prop.table(table(housing$Boro, housing$Class),1)

summary(housing)

#conditionally assigning a column
#test$Survived <- 0

#rbind append two datasets (must have same number of columns)
#test$Survived[test$Sex == 'female'] <- 1
