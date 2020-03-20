# ************************************************************************** #
## A1. Load the file hotdog.RData ------------------------------------------
load("hotdog.RData")

## A2. Describe in details the object hotdog. ------------------------------ 
##     What type of object is it? How many units contains? 
##     How many variables? What kind of variables? -------------------------
class(hotdog)
dim(hotdog)
levels(hotdog$Type)
str(hotdog)

## A3. Compute the absolute frequency distribution, ------------------------
##     the relative freq. distr. and the percentage freq. distr.
##     of the variable Type. Print the results in three columns. -----------
b <-table(hotdog$Type)
prop.table(table(hotdog$Type)) #or
a <- table(hotdog$Type)/dim(hotdog)[1]
a

a <-round(prop.table(table(hotdog$Type)),2)
c <-round(prop.table(table(hotdog$Type))*100,2)

cbind("n_i"=b,"f_i"=a,"p_i"=c)

## A4. What is the most appropriate graphical representation for the -------
##     variable Type? ------------------------------------------------------
barplot(table(hotdog$Type))
pie(table(hotdog$Type))

## A5. Represent graphically the freq. distr. in classes for the -----------
##     variable Calorie. ---------------------------------------------------
hist(hotdog$Calorie)

## A6. Compute the freq. distr. in classes of the variable Calorie ---------
##     consider the following classes :
##     [80,120] (120,140] (140,160] (160,200] ------------------------------
new.Calorie <- cut(hotdog$Calorie, breaks = c(80,120,140,160,200), 
              include.lowest = TRUE)

## A7. Plot a new histogram considering the classes in the previous --------
##     question. -----------------------------------------------------------
par(mfrow=c(1,2))
hist(hotdog$Calorie)
hist(hotdog$Calorie, breaks = c(80,120,140,160,200), 
     include.lowest = TRUE)

## A8. Compute the densities plotted on the previous plot. -----------------
width <- diff(c(80,120,140,160,200))
new.CalorieDens <- (table(new.Calorie)/54)/width
hist(hotdog$Calorie, breaks = c(80,120,140,160,200), 
     include.lowest = TRUE)
abline(h=new.CalorieDens, lty=c(1:4),
       col=c(1:4))

## A9. Plot and compare the histograms of the variable Calorie -------------
##     according to different levels of the variable Type and comment. -----
par(mfrow=c(1,3))
hist(hotdog$Calorie[hotdog$Type=="Beef"], 
     breaks = c(80,120,140,160,200), 
     include.lowest = TRUE, main="Beef")
hist(hotdog$Calorie[hotdog$Type=="Meat"], 
     breaks = c(80,120,140,160,200), 
     include.lowest = TRUE, main="Meat")
hist(hotdog$Calorie[hotdog$Type=="Poultry"], 
     breaks = c(80,120,140,160,200), 
     include.lowest = TRUE, main="Poultry")

#well done! go back to the slides 

# ************************************************************************** #
## B1. Compute the mean of the variable Sodium. ----------------------------
mean(hotdog$Sodium)

## B2. Compute the mean for each type of hotdog. ---------------------------
by(hotdog$Sodium, hotdog$Type, mean)

## B3. Compute the global mean as the weighted average of the three -------- 
##     means in point B2. --------------------------------------------------
sum(table(hotdog$Type)/length(hotdog$Type)*
      by(hotdog$Sodium, hotdog$Type, mean))
mean(hotdog$Sodium) #:)

## B4. Make a boxplot for each Type of hotdog (hint: read the help of ------
##     the boxplot function) and comment. ----------------------------------
boxplot(hotdog$Sodium~hotdog$Type)

## B5. Plot the empirical cumulative distr. func. for the variable ---------
##     Sodium. Then do it for each type of hotdog. -------------------------
plot.ecdf(hotdog$Sodium) #build-in fucntion

par(mfrow=c(1,3))
for(i in levels(hotdog$Type)){
plot.ecdf(hotdog$Sodium[hotdog$Type==i], main=i)
   abline(v=quantile(hotdog$Sodium[hotdog$Type==i]), 
          lty=2)
}

## B6. Compute the value of the ecdf for x=477.5 for the three groups. -----
ecdf(hotdog$Sodium[hotdog$Type=="Beef"])(477.5)
ecdf(hotdog$Sodium[hotdog$Type=="Meat"])(477.5)
ecdf(hotdog$Sodium[hotdog$Type=="Poultry"])(477.5)

# ************************************************************************** #
