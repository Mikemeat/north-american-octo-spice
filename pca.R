#pca example

#replicate this in your own adventure traveller

 Price <- c(6,7,6,5,7,6,5,6,3,1,2,5,2,3,1,2)
 Software <- c(5,3,4,7,7,4,7,5,5,3,6,7,4,5,6,3)
 Aesthetics <- c(3,2,4,1,5,2,2,4,6,7,6,7,5,6,5,7)
 Brand <- c(4,2,5,3,5,3,1,4,7,5,7,6,6,5,5,7)
 data <- data.frame(Price, Software, Aesthetics, Brand)

 
 #two types of pca code
 
 #princomp()  - uses variance covariance matrix - more featured
 #prcomp() - uses correlation coefficient matrix
 
pca <- princomp(data, cor=T)
 summary(pca, loadings = T)
 
 
 #loadings - the coefficent of each variable
 
 #Comp.1 = -0.523 * Price - 0.177 * Software + 0.597 * Aesthetics + 0.583 * Brand
 
 #looking for a high standard divation - at or around 1, so take comp 1 & 2
# looking at the cumulative proportion of variance around 80% is good enough 
 
 
 head(pca)
 plot(pca$scores[,1])
 barplot(pca$scores[,1])
 
 
#do the new variables help predict which type of software each person uses?
 
  OS <- c(0,0,0,0,1,0,0,0,1,1,0,1,1,1,1,1)
 
  model <- glm(OS ~ pca$scores[,1], family=binomial)
  summary(model)
 coefplot(model)
 
 fitted(model)