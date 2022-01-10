Childbirths <- read.csv("Childbirths.csv")

# Rename Columns
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

#removing ID, & birthweight
Childbirths_Parent <- Childbirths[,-c(1,2,3,4,5)]

# we don't want to scale birthweight because it is binomial and this is what we want to predict
Childbirths_Scale <- as.data.frame(scale(Childbirths_Parent[,-10], center=TRUE, scale=TRUE))

# PCA Analysis
pca <- princomp(Childbirths_Scale, cor=T)

# comp 4 better because of proportion of v is higher
summary(pca, loadings=T)

# Add lwbt back to dataframe
Childbirths_Scale$lowbwt <- Childbirths_2$lowbwt

# PCA model chosen is mod 3
PCA_model <- glm(Childbirths_Scale$lowbwt ~ pca$scores[,3], family=binomial)

# AIC = 32.585
summary(PCA_model)

# leverage is low, do see that stan resids have two groups, could be behaviour we don't see in the dataset. some vars approach vals but never hit it
plot(PCA_model)

##### Prediction #####
preds_PCA_model <- ifelse(predict(PCA_model, Childbirths_Scale, "response") > .5, 1, 0)

table(preds_PCA_model, Childbirths_Scale$lowbwt)

Childbirths_Parent_PCA <- Childbirths

# adding PCA Model as Column
Childbirths_Parent_PCA$ParentInfo <- 
  Childbirths_Parent_PCA$smoker*0.373 +
  Childbirths_Parent_PCA$mnocig*0.223 +
  Childbirths_Parent_PCA$mheight*-0.605 +
  Childbirths_Parent_PCA$mppwt*-0.567 +
  Childbirths_Parent_PCA$fedyrs*-0.169 +
  Childbirths_Parent_PCA$fnocig*0.220 +
  Childbirths_Parent_PCA$fheight*-0.188     

PCA_mod_2 <- glm(Childbirths_Parent_PCA$lowbwt ~ 
                   Childbirths_Parent_PCA$ParentInfo +
                   Childbirths_Parent_PCA$length +
                   Childbirths_Parent_PCA$Gestation        
                 , family=binomial)

# AIC 29.806
summary(PCA_mod_2)  

plot(PCA_mod_2)

##### Prediction #####
preds_PCA_mod_2 <- ifelse(predict(PCA_mod_2, Childbirths_Scale, "response") > .5, 1, 0)

table(preds_PCA_mod_2, Childbirths_Scale$lowbwt)

Child_Cor_Vals <- Childbirths_Parent_PCA[,-c(1,3,6,7,8,9,10,11,12,13,14,15,16)]

library(corrplot)
res <- cor(Child_Cor_Vals)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


library(ggcorrplot)
r <- cor(Child_Cor_Vals, method=c("pearson"))

ggcorrplot(r, hc.order = TRUE, type = "lower", lab = TRUE)





