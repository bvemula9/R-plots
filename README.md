---
title: "RedWine"
author: "Bharath Vemula"
date: "6/25/2017"
output: html_document
---

```{r}
knitr::opts_chunk$set(echo=TRUE, warning=FALSE, message=FALSE)
library(ggplot2)
library(GGally)
library(scales)
library(gridExtra)
library(dplyr)
library(knitr)
library(memisc)

##Load Data
wd<- read.csv('wineQualityReds.csv')

```

#Summary
  Basic summary of the data is obtained with some basic commands in R.  
```{r}
str(wd)
summary(wd)
```

There are 1599 observations with 13 different variables. X is a unique identifier with a integer value. Quality is also an integer value. All other values are numeric but not necessary integers.   

  Here we are primary concerned with wine quality, so lets start with some basic plots.  
```{r}
ggplot(aes(as.factor(quality),fill= quality), data = wd) + geom_bar() +theme_replace() + xlab("quality")
```  
 
  From the data obtained until now some things can be inferred like,
  
* Quality lies between 3 and 8.

* Mean quality is 5.636.

* Median Quality being 6.    

# Univariate Analysis  

## Wine Quality

  Looking at our first plot of wine quality, it roughly has a normal distribution with most rating being in 5 and 6. So lets create an another variable with variable ratings with following categories.
  
* 0-4 : poor

* 5-6: good

* 7-10 :ideal  

```{r, message=FALSE, warning=FALSE}
wd$rating <- ifelse(wd$quality <5, 'bad', ifelse( wd$quality<7, 'average','good'))
wd$rating<- ordered(wd$rating, levels = c('bad','average','good'))
summary(wd$rating)
qplot(wd$rating)
```  

## Univaraiate plots section


```{r, message=FALSE, warning=FALSE}
grid.arrange(qplot(wd$fixed.acidity),
             qplot(wd$volatile.acidity),
             qplot(wd$citric.acid),
             qplot(wd$residual.sugar),
             qplot(wd$chlorides),
             qplot(wd$free.sulfur.dioxide),
             qplot(wd$total.sulfur.dioxide),
             qplot(wd$density),
             qplot(wd$pH),
             qplot(wd$sulphates),
             qplot(wd$alcohol),
             qplot(wd$quality),
             ncol = 4)
```

## Distribution and Outliers

  Looking at the plots above inferred details are as fallows,
  
* Density and pH are normally distributed.

* Qualitatively, residual sugar and chlorides have extreme outlines.

* Fixed and volatile acidity, sulfur dioxides, sulphates, and alcohol seem to be long-tailed.

* Citric acid have many zero values,looks like there is some error in reporting but I am curious to know.  

  Since fixed and volatile acidity are long tailed I plotted them in log10 scale and found them to be normally distributed.  
```{r, message=FALSE, warning=FALSE}
ggplot(data= wd,aes(x=fixed.acidity))+geom_histogram()+scale_x_log10()
ggplot(data= wd,aes(x=volatile.acidity))+geom_histogram()+scale_x_log10()

```
 
  Similarly I plotted citric acid and sulphates to find out if they are normally distributed but found out only sulphates are normally distributed. 
  
```{r, message=FALSE, warning=FALSE}
ggplot(data= wd,aes(x=sulphates))+geom_histogram()+scale_x_log10()
ggplot(data= wd,aes(x=citric.acid))+geom_histogram()+scale_x_log10()
```  

Further investigating the data on total number of zero entries I found that there are 132 in total.  

```{r}
length(subset(wd, citric.acid==0)$citric.acid)
```

## Plots in residual.sugar and chlorides

After removing some extreme outliers in the data, the following plots are obtained.
```{r, message=FALSE, warning=FALSE}
ggplot(data=wd,aes(x=residual.sugar)) + geom_histogram() +
  scale_x_continuous(lim= c(0.5, quantile(wd$residual.sugar, 0.95))) + xlab('residual.sugar(g/dm^3)')

ggplot(data=wd,aes(x=chlorides)) + geom_histogram() +
  scale_x_continuous(lim= c(0.04, quantile(wd$chlorides, 0.95))) + xlab('chlorides (g/dm^3)')

```

  Observing the obtained plots, chlorides seems to follow normal distribution now. Residual sugars is nearly normal with some ouliers between 1-4(generally ideal).
  
***

#Questions

**What is the structure of your dataset?**
```{r}
str(wd)
```
**Did you create any new variables from existing variables in the dataset?**

Yes, I created an ordered factor for rating level and names as 'good', 'poor', 'ideal'.

**Of the features you investigated, were there any unusual distributions? Did you perform any operations on the data to tidy, adjust, or change the form of the data? If so, why did you do this?**

Yes there are some distributions that are unusual. I adjusted these plots by taking log10 values for the plots because more accurate trends can be inferred from bivarite plots.

***

## Bivariate Plots

Wine quality has biggest correlation value to wine quality, so lets start with a basic scatter plot of the both. 
```{r}
ggplot(aes(x=quality, y=alcohol), data = wd) +
  geom_point()
```

Since the original plot is over crowded with too many points lets add alpha values and 0.1, 0.5 and .09 percentile line to observe the general trends.

```{r, message=FALSE, warning=FALSE}
ggplot(aes(x=quality, y=alcohol), data = wd) +
  geom_point(color='#993366', alpha = 1/4) +
  geom_line(stat = 'summary', fun.y=quantile, probs= 0.5, color='#FF6660') + geom_line(stat='summary',fun.y = quantile,probs = .9, linetype =2, color='#FF6660') +
  geom_line(stat = 'summary', fun.y=quantile, probs= 0.1, linetype =2, color='#FF6660')+
            xlab("Wine Grade") + ylab("Alcohol") +
    ggtitle("Wine Qaulity and Alchohol")
 
```


Plot clearly shows trends in increasing wine quality with alcohol content.

## Wine Quality in categories

Here box plots are used to represent categorical values. 


### BoxPlot of quality

```{r}
quality_plot <- function (x, y, ylab) {
return (ggplot(data = wd, aes_string(as.factor(x),y)) +
geom_boxplot(fill = 'green') +
xlab ('quality') + ylab(ylab))

}

grid.arrange( quality_plot( 'quality', 'fixed.acidity', 'fixed.acidity(g/dm^3)'), 
quality_plot('quality', 'volatile.acidity', 'volatile.acidity(g/dm^3)'),
quality_plot('quality', 'citric.acid', 'citric.acid (g / dm^3)'),
quality_plot('quality', 'residual.sugar', 'residual.sugar (g / dm^3)'),
quality_plot('quality', 'chlorides', 'chlorides (g / dm^3)'),
quality_plot('quality', 'free.sulfur.dioxide', 'free.sulphur.dioxide (g / dm^3)'),
quality_plot('quality', 'total.sulfur.dioxide', 'total.sulphur.dioxide (g / dm^3)'),
quality_plot('quality', 'density', 'density (g/cm^3)'),
quality_plot('quality', 'pH', 'pH'),
quality_plot('quality', 'sulphates', 'sulphates (g/dm^3)'),
quality_plot('quality', 'alcohol', 'alcohol (volume %)'),
 
ncol= 4)

```


## BoxPlot of rating 

```{r, message=FALSE, warning=FALSE}
rating_plot <- function(x, y, ylab) {
  return (ggplot(data = wd, aes_string(x, y)) +
   geom_boxplot(fill = 'orange') +
  xlab('rating') + ylab(ylab))
}

grid.arrange( rating_plot( 'quality', 'fixed.acidity', 'fixed.acidity(g/dm^3)'), 
rating_plot('quality', 'volatile.acidity', 'volatile.acidity(g/dm^3)'),
rating_plot('quality', 'citric.acid', 'citric.acid (g / dm^3)'),
rating_plot('quality', 'residual.sugar', 'residual.sugar (g / dm^3)'),
rating_plot('quality', 'chlorides', 'chlorides (g / dm^3)'),
rating_plot('quality', 'free.sulfur.dioxide', 'free.sulphur.dioxide (g / dm^3)'),
rating_plot('quality', 'total.sulfur.dioxide', 'total.sulphur.dioxide (g / dm^3)'),
rating_plot('quality', 'density', 'density (g/cm^3)'),
rating_plot('quality', 'pH', 'pH'),
rating_plot('quality', 'sulphates', 'sulphates (g/dm^3)'),
rating_plot('quality', 'alcohol', 'alcohol (volume %)'),
 
ncol= 4)
```


Observing the above plots some things can be inferred for a good wine,

* Higher sulphur.dioxide and volatile.acidity,

* Lower pH,

* Higher density,

* lower fixed.acidity and citric.acid.

## Correlation of varaiables

Correlation of variables against quality is calculated to further explore,
```{r}


correlations <- c(
  
  cor.test(wd$fixed.acidity, wd$quality)$estimate,
  cor.test(wd$volatile.acidity, wd$quality)$estimate,
  cor.test(wd$citric.acid, wd$quality)$estimate,
  cor.test(log10(wd$residual.sugar), wd$quality)$estimate,
  cor.test(log10(wd$chlorides), wd$quality)$estimate,
  cor.test(wd$free.sulfur.dioxide, wd$quality)$estimate,
  cor.test(wd$total.sulfur.dioxide, wd$quality)$estimate,
  cor.test(wd$density, wd$quality)$estimate,
  cor.test(wd$pH, wd$quality)$estimate,
  cor.test(log10(wd$sulphates), wd$quality)$estimate,
  cor.test(wd$alcohol, wd$quality)$estimate)
names(correlations) <- c('fixed.acidity','volatile.acidity',                          'citric.acid',                                              'log10.residual.sugar',                                     'log10.chlordies',                                          'free.sulfur.dioxide',                                      'total.sulfur.dioxide', 'density',                          'pH', 'log10.sulphates', 'alcohol')
correlations
  
```


 Observing the above results following show a strong correaltion with quality,
 
* alcohal

* sulphates

* citric.acid

* fixed.acidity

To further explore lets plot these highly correlated variables with rating:

```{r}
ggplot( data = wd, aes(x= log10(sulphates), y= alcohol)) + 
  facet_wrap(~rating) +
  geom_point()

ggplot(data = wd, aes(x = volatile.acidity, y = alcohol)) +
  facet_wrap(~rating) +
  geom_point()

ggplot(data = wd, aes(x = citric.acid, y = alcohol)) +
  facet_wrap(~rating) +
  geom_point()

ggplot(data = wd, aes(x = volatile.acidity, y = log10(sulphates))) +
  facet_wrap(~rating) +
  geom_point()

ggplot(data = wd, aes(x = citric.acid, y = log10(sulphates))) +
  facet_wrap(~rating) +
  geom_point()

ggplot(data = wd, aes(x = citric.acid, y = volatile.acidity)) +
  facet_wrap(~rating) +
  geom_point()
```



 From the above plots only one thing is clear: alcohol content heavely effects  rating.


# Multivariate Plots

```{r}
ggplot(data = wd,
       aes(x = citric.acid, y = volatile.acidity,
           color = quality)) +
  geom_point() +
  facet_wrap(~rating)

ggplot(data = wd,
       aes(x = alcohol, y = log10(sulphates),
           color = quality)) +
  geom_point() +
  facet_wrap(~rating)

ggplot(data = wd,
       aes(x = pH, y = alcohol, color = quality)) +
  geom_point() +
  facet_wrap(~rating)
```



## Analysis

These scatter plots are too crowded so I tried to facet by rating. Graphs between four variables citric.acid, fixed.acidity, sulphates and alcohol which shown high correlations with quality and faceted them with rating. I conclude that higher citric.acid and lower fixed.acidity yields better wines. Better wines also have higher alcohol and sulphates and lower pH.

## Linear Multivariable Model

Linear multivariable model was created to predict the wine quality based on chemical properties.

```{r, message=TRUE, warning=TRUE}
# regression
  m1<-lm(quality ~ volatile.acidity,data=wd)
  m2<-update(m1,~. + alcohol)
  m3<-update(m2,~. + sulphates)
  m4<-update(m3,~. + citric.acid)
  m5<-update(m4,~. + chlorides)
  m6<-update(m5,~. + total.sulfur.dioxide)
  m7<-update(m6,~. + density)
  mtable(m1,m2,m3,m4,m5,m6,m7)
```


The model of 6 features has the lowest AIC (Akaike information criterion) number. As the number of features increase the AIC becomes higher. The parameter of the predictor also changed dramatically which shows a sign of overfitting.

The model can be described as:

wine_quality = 2.985 + 0.276xalcohol - 2.985xvolatile.acidity + 0.908xsulphates + 0.065xcitric.acid - -1.763*chlorides - 0.002xtotal.sulfur.dioxide


# Final Plots and Summary


### Alcohol and Wine quality

```{r}
ggplot(data = wd, aes(as.factor(quality), alcohol, fill = rating)) +
  geom_boxplot() +
  ggtitle('Alcohol % on Wine Quality') +
  xlab('Quality') +
  ylab('Alcohol (% volume)') +
  scale_fill_brewer(type = 'seq', palette = 1)
```


From the above plot it is clear that wine quality increases with % of alcohol in it.


### Acids and Wine quality

```{r, message=FALSE, warning=FALSE}
grid.arrange(ggplot(data = wd, aes(x = quality,y =fixed.acidity,
                                   fill = quality)) + 
               ylab('Fixed Acidity (g/dm^3)') +
               xlab('Quality') +
               geom_boxplot(),
             ggplot(data = wd, aes(x = quality,y = volatile.acidity,
                                   fill = quality)) +
               ylab('Volatile Acidity (g/dm^3)') +
               xlab('Quality') +
               geom_boxplot(), 
             ggplot(data = wd, aes(x = quality, y = citric.acid,
                                   fill = quality)) +
               ylab('Citric Acid (g/dm^3)') +
               xlab('Quality') +
               geom_boxplot(), 
             ggplot(data = wd, aes(x = quality, y = pH,
                                   fill = quality)) +
               ylab('pH') +
               xlab('Quality') +
               geom_boxplot())
```



From the above plots it is clear that higher acidic(lower pH) content is seen in highly rated wines.

###  Good and Bad wines

```{r}
ggplot(data = subset(wd, rating != 'average'),
       aes(x = volatile.acidity, y = alcohol,
                      color = rating)) +
  geom_point() +
  ggtitle('Alcohol vs. Volatile Acidity and Wine Quality') +
  xlab('Volatile Acidity (g / dm^3)') +
  ylab('Alcohol (% volume)')
```


Above plots includes only good and bad wines, some things that can be inferred from the plot are:

* High volatile acidity--with few exceptions--kept wine quality down.
* A combination of high alcohol content and low volatile acidity produced better wines.

***

# Reflection

Wine quality depends on many features, through this exploratory data analysis I was able to relate some of the key factors like alcohol content, sulphates, and acidity. The correlations for these variables are within reasonable bounds. The graphs adequately illustrate the factors that make good wines 'good' and bad wines 'bad'.

***

## References

* http://www.winegeeks.com/articles/85/high_alcohol_is_a_wine_fault_not_a_badge_of_honor/

* http://www.winegeeks.com/articles/85/high_alcohol_is_a_wine_fault_not_a_badge_of_honor/

* https://onlinecourses.science.psu.edu/stat857/node/223

* https://github.com/Dalaska/Udacity-Red-Wine-Quality/blob/master/redwine_final.rmd









