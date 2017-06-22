# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(knitr)
cache=TRUE
act <- read.csv(file="activity.csv") # Read CSV (steps, date, interval)
udates<-unique(act[,2])  # Get Unique dates
tsteps<-c()  # Init total steps, hist steps & count of NA
hsteps<-c()
nac<-c()
for (i in 1:61) {
  ssteps<-act[,1][act[,2]==udates[i]]   # extract single date steps
  nac<-rbind(nac,sum(is.na(ssteps)))
  hsteps <- c(hsteps,sum(ssteps)) # histo steps for each date
  dsteps <- c(udates[i],sum(ssteps),mean(ssteps), median(ssteps))
  tsteps <- rbind(tsteps,dsteps) # total steps data frame
}
```

## What is mean total number of steps taken per day?
### 1. Histogram of total #of steps per day


```r
hist (hsteps, main="Histogram - Sum Steps")
```

![](PA1_Rep_Res_files/figure-html/Histogram of Sum Steps-1.png)<!-- -->

### 2. Mean and Median of total # of steps per day

```r
library(xtable)
dtab<-xtable(summary(tsteps[,3:4]))# get to table format
print (dtab, type="html")
```

<!-- html table generated in R 3.4.0 by xtable 1.8-2 package -->
<!-- Thu Jun 22 23:19:00 2017 -->
<table border=1>
<tr> <th>  </th> <th>       V1 </th> <th>       V2 </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> Min.   : 0.1424   </td> <td> Min.   :0   </td> </tr>
  <tr> <td align="right"> 2 </td> <td> 1st Qu.:30.6979   </td> <td> 1st Qu.:0   </td> </tr>
  <tr> <td align="right"> 3 </td> <td> Median :37.3785   </td> <td> Median :0   </td> </tr>
  <tr> <td align="right"> 4 </td> <td> Mean   :37.3826   </td> <td> Mean   :0   </td> </tr>
  <tr> <td align="right"> 5 </td> <td> 3rd Qu.:46.1597   </td> <td> 3rd Qu.:0   </td> </tr>
  <tr> <td align="right"> 6 </td> <td> Max.   :73.5903   </td> <td> Max.   :0   </td> </tr>
  <tr> <td align="right"> 7 </td> <td> NA's   :8   </td> <td> NA's   :8   </td> </tr>
   </table>

## What is the average daily activity pattern?

### 1. Time series plot of 5 min interval data


```r
plot(act[,1] ~ act[,3], type="l", main="Time Series (5 min)", xlab="Interval",
ylab="steps")
```

![](PA1_Rep_Res_files/figure-html/Time Series 5 min-1.png)<!-- -->

### 2. Time Series plot of daily average data

```r
plot(tsteps[,3] ~ tsteps[,1], type="l", main="Time Series (Avg)", xlab="Interval",
ylab="steps")
```

![](PA1_Rep_Res_files/figure-html/Time Series Daily Avg data-1.png)<!-- -->


## Imputing missing values
### 1. Total #of Missing values (Total #of NAs in rows)

```r
totNA <- sum(rowSums(is.na(act))) # # of NAs in rows
```

Number of NAs in rows is 2304.

### 2. Imput Missing NAs with mean Value
### 3. New Dataset with NAs removed

```r
actna<-act     # copy act to actna (NA removed)
for (i in 1:61) {
  ssteps<-actna[,1][actna[,2]==udates[i]]   # extract single date steps
  if (sum(is.na(ssteps) != 0)) {
     actna[,1][is.na(actna[,1])] <- mean(actna[,1], na.rm = TRUE) # to mean
     ssteps<-actna[,1][actna[,2]==udates[i]]    } # re calculate single date steps  
  hsteps <- c(hsteps,sum(ssteps)) # histo steps for each date
  dsteps <- c(udates[i],sum(ssteps),mean(ssteps), median(ssteps))
  tsteps <- rbind(tsteps,dsteps) # total steps data frame
}
```
### 4. Histogram of total #of steps per day, 
### Mean and median of new data set (NAs removed)


```r
hist (hsteps, main="Histogram - Sum Steps - NAs removed")
```

![](PA1_Rep_Res_files/figure-html/Histogram of Sum Steps - NAs removed-1.png)<!-- -->
  

### Are there differences in activity patterns between weekdays and weekends?


```r
dsteps1<-c()
tsteps1<-c()
for (i in 1:61) {
  ssteps1<-act[,1][act[,2]==udates[i]]   # extract single date steps
  ssteps2<-actna[,1][actna[,2]==udates[i]]   # extract single date steps
  dsteps1 <- c(udates[i],mean(ssteps1), median(ssteps1),mean(ssteps2), median(ssteps2))
  tsteps1 <- rbind(tsteps1,dsteps1) # total steps data frame
}
```

### Weekdays and Weekend activities

```r
actna$day <- format(as.Date(actna[,2]),"%w") # get day number
actna$wkd <- ifelse(( actna$day==0) | (actna$day ==6), "WKEND","WKDAY")
```
### . Time Series plot of daily average data

```r
library(lattice)
xyplot(actna[,1] ~ actna[,3] | actna[,5], type="l",xlab="Interval",ylab="Steps")
```

![](PA1_Rep_Res_files/figure-html/Time Series Daily Avg WkDay/WkEnd-1.png)<!-- -->

