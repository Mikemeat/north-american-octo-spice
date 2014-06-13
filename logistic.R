# logistic regression example - used for propensity modelling

#American Comunity Survey

#read in data

acs <- read.table("http://jaredlander.com/data/acs_ny.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

#explore

head(acs)
summary(acs)

require(Amelia)
missmap(acs, main="American Community Survey - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)

#list of variables

#does a household have a greater income than $150k? 


acs$Income <- with(acs, FamilyIncome >= 150000)

require(ggplot2) 
require(useful)
require(coefplot)

ggplot(acs, aes(x=FamilyIncome)) + geom_density(fill="grey", color = "grey") + geom_vline(xintercept = 150000) 
+ scale_x_continuous(label = multiple.dollar, limits=c(0,1000000))


income1 <- glm(Income ~ HouseCosts + NumWorkers + OwnRent + NumBedrooms + FamilyType, data = acs, family = binomial(link="logit"))

coefplot(income1)

invlogit <- function(x)
{
    1/(1 + exp(-x))
}
invlogit(income1$coefficients)

inv <- invlogit(income1$coefficients)

summary(acs$NumWorkers)

#convert to prob

#logit(p) = log(p/(1-p))= β0 + β1*x1 + ... + βk*xk


prob <- function(x)
{
    #p/(1-p) = x 
    #p = x - xp
    #P+xp = x
    #(1+x)p = x
    # p = x/(1+x)
    
     x/(1+x)
}

# convert to probabilities
prob(inv)

# find the proportion of Boroughs - proc freq

prop.table(table(acs$NumBedrooms, acs$Income))
ggplot(acs, aes(x = NumBedrooms)) + geom_histogram(binwidth = 1) + facet_wrap(~Income)

prop.table(table(housing$Boro, housing$Class),1)