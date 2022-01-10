library(dplyr)
library(readr)
library(PerformanceAnalytics)
library(e1071) 
library(ggplot2)
library(PerformanceAnalytics)
library(caret)
library(corrplot)
library(ggplot2)
library(ggcorrplot)

Childbirths <- read.csv("Childbirths.csv")

colnames(Childbirths) <- c(
  "ID"
  ,"length"
  ,"Birthweight"
  ,"headcirumference"
  ,"Gestation"
  ,"smoker"
  ,"motherage"
  ,"mnocig"
  ,"mheight"
  ,"mppwt"
  ,"fage"
  ,"fedyrs"
  ,"fnocig"
  ,"fheight"
  ,"lowbwt"
  ,"mage35"
)

#### Desc Analysis Birthweight ####

Childbirths_2 <- Childbirths[,-c(1,2,3,4)]

# Histogram
ggplot(data=Childbirths, aes(x = Birthweight)) + 
  geom_histogram(aes(fill=Birthweight < 2.67),bins=17) +
   xlab("Birthweight (kg)") + ylab("# Babies") +
    scale_x_continuous(n.breaks = 17) +
      labs(col="test")


# Summary
summary(Childbirths$Birthweight)

# Skewness
skewness(Childbirths$Birthweight)

# Outliers
boxplot.stats(Childbirths$Birthweight, coef = 1.5, do.conf = TRUE, do.out = TRUE)

# Box Plot
ggplot(data=Childbirths, aes(y = Birthweight)) + ylab("Birthweight (kg)") + 
  geom_boxplot(outlier.shape=4,outlier.size=3, fill = '#90be6d', outlier.color = '#f3722c')

summary(Childbirths$Birthweight)

kurtosis(Childbirths$Birthweight) 


res_all <- cor(Childbirths_2)
corrplot(res_all, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


r <- cor(Childbirths, method=c("pearson"))

ggcorrplot(r, hc.order = TRUE, type = "lower", lab = TRUE)



# Weight Vs Height
ggplot(data=Childbirths, aes(x = mppwt, y = mheight)) + 
  geom_point(colour = "#F3722C") + 
  xlab("Mother Pre Patern Weight") + ylab("Mother Height")  + geom_smooth(method="lm")

# Mother Weight Vs Baby Weight
ggplot(data=Childbirths, aes(x = Birthweight, y = mheight)) + 
  geom_point(colour = "#F3722C") + 
  xlab("Birthweight") + ylab("Mother Height")  + geom_smooth(method="lm")

# Mother Height Vs Baby Weight
ggplot(data=Childbirths, aes(x = Birthweight , y = mppwt)) + 
  geom_point(colour = "#F3722C") + 
  xlab("Birthweight") + ylab("Mother Weight")  + geom_smooth(method="lm")


cor.test(Childbirths$mheight, Childbirths$Birthweight, method=c("spearman"))
cor.test(Childbirths$mppwt, Childbirths$Birthweight, method=c("spearman"))

res_all <- cor(Childbirths)
corrplot(res_all, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


#scatter plot
ggplot(data=Childbirths, aes(x = Birthweight, y = (fheight+mheight))) + 
  geom_point( ) + geom_smooth(method="lm")


#scatter plot
ggplot(data=Childbirths, aes(x = Birthweight, y = Gestation)) + 
  geom_point( ) + geom_smooth(method="lm")


#scatter plot
ggplot(data=Childbirths, aes(x = Birthweight, y = mheight)) + 
  geom_point( ) + geom_smooth(method="lm")


install.packages("corrplot")
library(corrplot)
res <- cor(Childbirths_2)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

mod1 <- glm(lowbwt~ Gestation + 
              headcirumference+
              mppwt+
              fage+
              fedyrs+
              smoker+
              mage35
            ,data=Childbirths_2)

summary(mod1)

mod2 <- glm(lowbwt~ Gestation + 
              headcirumference+
              mppwt
            ,data=Childbirths_2)

summary(mod2)


#### PCA
## Looks for underlying relationships

scale(Childbirths_2$Gestation, center=TRUE, scale=TRUE)

# why scale - put data in comparable scale
# make data as cohesive as possible
# we don't want to scale birthweight because it is binomial and this is what we want to predict
Childbirths_Scale <- as.data.frame(scale(Childbirths_2[,-11], center=TRUE, scale=TRUE))

pca <- princomp(Childbirths_Scale, cor=T)
summary(pca, loadings=T)

# gives comp which tries to identify relationship
# want SD to be high, larger SD = better. Looking for > 1 (everything after 5)
# cumulative = % explanation of the data. looking for highest
 

model <- glm(Childbirths_2$lowbwt ~ pca$scores[,4], family=binomial)
summary(model)

# looking for lower AIC
# deriving a variable
# rename
Childbirths_Scale$PCA_Mod <- 
  Childbirths_Scale$Gestation*0.481
  Childbirths_Scale$smoker*0.144
  Childbirths_Scale$motherage*-0.107
  Childbirths_Scale$mnocig*0.229
  Childbirths_Scale$mheight*-0.304
  Childbirths_Scale$mppwt*-0.337
  Childbirths_Scale$fedyrs*0.521
  Childbirths_Scale$fnocig*-0.235
  Childbirths_Scale$fheight*0.366


summary(Childbirths_Scale$PCA_Mod)

Childbirths_Scale$lowbwt <- Childbirths_2$lowbwt

model_PCA <- glm(Childbirths_Scale$lowbwt ~ Childbirths_Scale$PCA_Mod, family=binomial)


summary(model_PCA)

plot(model_PCA)

summary(model_All)


###removing outliers
myData <- Childbirths_Scale[-c(24, 25, 18), ]


model_PCA_outlier <- glm(myData$lowbwt ~ myData$PCA_Mod, family=binomial)

plot(model_PCA_outlier)

summary(model_PCA_outlier)


model_PCA_outlier_2 <- glm(myData$lowbwt ~ 
       myData$Gestation + 
       myData$smoker + 
       myData$motherage + 
       myData$mnocig +
       myData$mheight +
       myData$mppwt +
       myData$fedyrs +
       myData$fnocig +
       myData$fheight
    , family=binomial)

summary(model_PCA_outlier_2)

#### Assumption Checking

## 1. resids vs fitted - want a linear relationship

plot(model_PCA)

## plot each var
## disqualify findings due to no linear relationship

library(ggplot2)
ggplot(Childbirths_Scale, aes(x = Gestation, y= predict(model_PCA, Childbirths_Scale ))) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw()

ggplot(Childbirths_Scale, aes(x = headcirumference, y= predict(mod2_Scale, Childbirths_Scale ))) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw()

ggplot(Childbirths_Scale, aes(x = mppwt, y= predict(mod2_Scale, Childbirths_Scale ))) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw()


ggplot(Childbirths_Scale, aes(x = fheight, y= predict(mod2_Scale, Childbirths_Scale ))) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw()

## 2. influencial values
## any extreme outliers using cooks distance
## exclude vals of 3.5 or more

plot(mod2_Scale)
       
## 3. multicoilinearity 
## tied in correlation feed plot
## drop ID
## not including anything that is correlated


###assessment of model

summary(mod2_Scale)

## lwbtw = Gestation * (-0.18493) + 
## for every unit of Gestation the log odds of low birth weight decreases by -0.18493


preds <- ifelse(predict(mod2_Scale, Childbirths_Scale, "response") > .5, 1, 0)
#confusion matrix
# accuracy 41/42 - 97.6%
table(preds, Childbirths_Scale$lowbwt)


# Create random model
model_Rand <- glm(Childbirths_Scale$lowbwt ~ 
                    (Childbirths_Scale$mppwt/Childbirths_Scale$mheight) +
                    Childbirths_Scale$smoker+
                    Childbirths_Scale$fheight +
                    Childbirths_Scale$mage35
                  
                  , family=binomial)

# AIC = 12
summary(model_Rand)

plot(model_Rand)


