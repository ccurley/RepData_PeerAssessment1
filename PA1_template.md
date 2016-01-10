# Reproducible Research: Peer Assessment 1
[Christopher Curley](https://github.com/ccurley)  
9 Jan 2016  

## Loading and preprocessing the data
I forked the repository for the [class](https://github.com/rdpeng/RepData_PeerAssessment1) on Saturday, 8 January 2016.

The following code will check to see if the .zip file is in the repository. If so, then unzip the file to a csv. 

Note that I set the working directory is set to the RepData_PeerAssessment1 repository in GitHub.


```r
if(file.exists("activity.zip")) {
        unzip("activity.zip")
} else {
        print("activity.zip not found. Check default working directory")
}
```
Check to see if the .csv file has been unzipped. If so, Read the unzipped file into a data frame called fitdata.


```r
if(file.exists("activity.csv")) {
        fitdata <- read.csv("activity.csv")
} else {
        print("activity.cvs not found. Check default working directory or unzip activity.zip")
}
```
The result should be a dataframe with 17,568 observations of 3 variables with the following characteristics:


```r
summary(fitdata)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```


```r
names(fitdata)
```

```
## [1] "steps"    "date"     "interval"
```


```r
str(fitdata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


```r
head(fitdata)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

Lots of NAs in steps?


```r
sum(is.na(fitdata$steps)) / nrow(fitdata)
```

```
## [1] 0.1311475
```

## What is mean total number of steps taken per day?
To calculate the mean total steps taken per day, I applied two methods. The first calls my preferred method *ddply* to produce a data frame. However, I'm told it's more R-ish to call *tapply* to produce an array, so I called tapply as well so my friends would get off my back about it.


```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.1.3
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.1.3
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
totalsteps <- ddply(fitdata, .(date), summarize, mean.total.steps = mean(steps))
totalsteps
```

```
##          date mean.total.steps
## 1  2012-10-01               NA
## 2  2012-10-02        0.4375000
## 3  2012-10-03       39.4166667
## 4  2012-10-04       42.0694444
## 5  2012-10-05       46.1597222
## 6  2012-10-06       53.5416667
## 7  2012-10-07       38.2465278
## 8  2012-10-08               NA
## 9  2012-10-09       44.4826389
## 10 2012-10-10       34.3750000
## 11 2012-10-11       35.7777778
## 12 2012-10-12       60.3541667
## 13 2012-10-13       43.1458333
## 14 2012-10-14       52.4236111
## 15 2012-10-15       35.2048611
## 16 2012-10-16       52.3750000
## 17 2012-10-17       46.7083333
## 18 2012-10-18       34.9166667
## 19 2012-10-19       41.0729167
## 20 2012-10-20       36.0937500
## 21 2012-10-21       30.6284722
## 22 2012-10-22       46.7361111
## 23 2012-10-23       30.9652778
## 24 2012-10-24       29.0104167
## 25 2012-10-25        8.6527778
## 26 2012-10-26       23.5347222
## 27 2012-10-27       35.1354167
## 28 2012-10-28       39.7847222
## 29 2012-10-29       17.4236111
## 30 2012-10-30       34.0937500
## 31 2012-10-31       53.5208333
## 32 2012-11-01               NA
## 33 2012-11-02       36.8055556
## 34 2012-11-03       36.7048611
## 35 2012-11-04               NA
## 36 2012-11-05       36.2465278
## 37 2012-11-06       28.9375000
## 38 2012-11-07       44.7326389
## 39 2012-11-08       11.1770833
## 40 2012-11-09               NA
## 41 2012-11-10               NA
## 42 2012-11-11       43.7777778
## 43 2012-11-12       37.3784722
## 44 2012-11-13       25.4722222
## 45 2012-11-14               NA
## 46 2012-11-15        0.1423611
## 47 2012-11-16       18.8923611
## 48 2012-11-17       49.7881944
## 49 2012-11-18       52.4652778
## 50 2012-11-19       30.6979167
## 51 2012-11-20       15.5277778
## 52 2012-11-21       44.3993056
## 53 2012-11-22       70.9270833
## 54 2012-11-23       73.5902778
## 55 2012-11-24       50.2708333
## 56 2012-11-25       41.0902778
## 57 2012-11-26       38.7569444
## 58 2012-11-27       47.3819444
## 59 2012-11-28       35.3576389
## 60 2012-11-29       24.4687500
## 61 2012-11-30               NA
```

```r
hist(totalsteps$mean.total.steps, xlab = "total steps daily - ddply")
```

![](PA1_template_files/figure-html/fitmeansteps-1.png) 

```r
mean(totalsteps$mean.total.steps, na.rm = TRUE)
```

```
## [1] 37.3826
```

```r
median(totalsteps$mean.total.steps, na.rm = TRUE)
```

```
## [1] 37.37847
```
 
or


```r
othertotalsteps <- with(fitdata, tapply(steps, date, FUN = mean, na.rm = FALSE))
othertotalsteps
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA  0.4375000 39.4166667 42.0694444 46.1597222 53.5416667 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
## 38.2465278         NA 44.4826389 34.3750000 35.7777778 60.3541667 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
## 43.1458333 52.4236111 35.2048611 52.3750000 46.7083333 34.9166667 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
## 41.0729167 36.0937500 30.6284722 46.7361111 30.9652778 29.0104167 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##  8.6527778 23.5347222 35.1354167 39.7847222 17.4236111 34.0937500 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
## 53.5208333         NA 36.8055556 36.7048611         NA 36.2465278 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
## 28.9375000 44.7326389 11.1770833         NA         NA 43.7777778 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
## 37.3784722 25.4722222         NA  0.1423611 18.8923611 49.7881944 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
## 52.4652778 30.6979167 15.5277778 44.3993056 70.9270833 73.5902778 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
## 50.2708333 41.0902778 38.7569444 47.3819444 35.3576389 24.4687500 
## 2012-11-30 
##         NA
```

```r
hist(othertotalsteps, xlab = "total steps daily - ttaply")
```

![](PA1_template_files/figure-html/fitmeanstepsother-1.png) 

```r
mean(othertotalsteps, na.rm = TRUE)
```

```
## [1] 37.3826
```

```r
median(othertotalsteps, na.rm = TRUE)
```

```
## [1] 37.37847
```

There are, apparently, multiple stones to kill one bird.

## What is the average daily activity pattern?
To calculate the daily activity pattern by interval, I perform the same operation for the daily averages only grouping by interval

```r
library(plyr)
library(dplyr)
intervalsteps <- ddply(fitdata, .(interval), summarize, mean.interval.steps = mean(steps, na.rm = TRUE))
intervalsteps
```

```
##     interval mean.interval.steps
## 1          0           1.7169811
## 2          5           0.3396226
## 3         10           0.1320755
## 4         15           0.1509434
## 5         20           0.0754717
## 6         25           2.0943396
## 7         30           0.5283019
## 8         35           0.8679245
## 9         40           0.0000000
## 10        45           1.4716981
## 11        50           0.3018868
## 12        55           0.1320755
## 13       100           0.3207547
## 14       105           0.6792453
## 15       110           0.1509434
## 16       115           0.3396226
## 17       120           0.0000000
## 18       125           1.1132075
## 19       130           1.8301887
## 20       135           0.1698113
## 21       140           0.1698113
## 22       145           0.3773585
## 23       150           0.2641509
## 24       155           0.0000000
## 25       200           0.0000000
## 26       205           0.0000000
## 27       210           1.1320755
## 28       215           0.0000000
## 29       220           0.0000000
## 30       225           0.1320755
## 31       230           0.0000000
## 32       235           0.2264151
## 33       240           0.0000000
## 34       245           0.0000000
## 35       250           1.5471698
## 36       255           0.9433962
## 37       300           0.0000000
## 38       305           0.0000000
## 39       310           0.0000000
## 40       315           0.0000000
## 41       320           0.2075472
## 42       325           0.6226415
## 43       330           1.6226415
## 44       335           0.5849057
## 45       340           0.4905660
## 46       345           0.0754717
## 47       350           0.0000000
## 48       355           0.0000000
## 49       400           1.1886792
## 50       405           0.9433962
## 51       410           2.5660377
## 52       415           0.0000000
## 53       420           0.3396226
## 54       425           0.3584906
## 55       430           4.1132075
## 56       435           0.6603774
## 57       440           3.4905660
## 58       445           0.8301887
## 59       450           3.1132075
## 60       455           1.1132075
## 61       500           0.0000000
## 62       505           1.5660377
## 63       510           3.0000000
## 64       515           2.2452830
## 65       520           3.3207547
## 66       525           2.9622642
## 67       530           2.0943396
## 68       535           6.0566038
## 69       540          16.0188679
## 70       545          18.3396226
## 71       550          39.4528302
## 72       555          44.4905660
## 73       600          31.4905660
## 74       605          49.2641509
## 75       610          53.7735849
## 76       615          63.4528302
## 77       620          49.9622642
## 78       625          47.0754717
## 79       630          52.1509434
## 80       635          39.3396226
## 81       640          44.0188679
## 82       645          44.1698113
## 83       650          37.3584906
## 84       655          49.0377358
## 85       700          43.8113208
## 86       705          44.3773585
## 87       710          50.5094340
## 88       715          54.5094340
## 89       720          49.9245283
## 90       725          50.9811321
## 91       730          55.6792453
## 92       735          44.3207547
## 93       740          52.2641509
## 94       745          69.5471698
## 95       750          57.8490566
## 96       755          56.1509434
## 97       800          73.3773585
## 98       805          68.2075472
## 99       810         129.4339623
## 100      815         157.5283019
## 101      820         171.1509434
## 102      825         155.3962264
## 103      830         177.3018868
## 104      835         206.1698113
## 105      840         195.9245283
## 106      845         179.5660377
## 107      850         183.3962264
## 108      855         167.0188679
## 109      900         143.4528302
## 110      905         124.0377358
## 111      910         109.1132075
## 112      915         108.1132075
## 113      920         103.7169811
## 114      925          95.9622642
## 115      930          66.2075472
## 116      935          45.2264151
## 117      940          24.7924528
## 118      945          38.7547170
## 119      950          34.9811321
## 120      955          21.0566038
## 121     1000          40.5660377
## 122     1005          26.9811321
## 123     1010          42.4150943
## 124     1015          52.6603774
## 125     1020          38.9245283
## 126     1025          50.7924528
## 127     1030          44.2830189
## 128     1035          37.4150943
## 129     1040          34.6981132
## 130     1045          28.3396226
## 131     1050          25.0943396
## 132     1055          31.9433962
## 133     1100          31.3584906
## 134     1105          29.6792453
## 135     1110          21.3207547
## 136     1115          25.5471698
## 137     1120          28.3773585
## 138     1125          26.4716981
## 139     1130          33.4339623
## 140     1135          49.9811321
## 141     1140          42.0377358
## 142     1145          44.6037736
## 143     1150          46.0377358
## 144     1155          59.1886792
## 145     1200          63.8679245
## 146     1205          87.6981132
## 147     1210          94.8490566
## 148     1215          92.7735849
## 149     1220          63.3962264
## 150     1225          50.1698113
## 151     1230          54.4716981
## 152     1235          32.4150943
## 153     1240          26.5283019
## 154     1245          37.7358491
## 155     1250          45.0566038
## 156     1255          67.2830189
## 157     1300          42.3396226
## 158     1305          39.8867925
## 159     1310          43.2641509
## 160     1315          40.9811321
## 161     1320          46.2452830
## 162     1325          56.4339623
## 163     1330          42.7547170
## 164     1335          25.1320755
## 165     1340          39.9622642
## 166     1345          53.5471698
## 167     1350          47.3207547
## 168     1355          60.8113208
## 169     1400          55.7547170
## 170     1405          51.9622642
## 171     1410          43.5849057
## 172     1415          48.6981132
## 173     1420          35.4716981
## 174     1425          37.5471698
## 175     1430          41.8490566
## 176     1435          27.5094340
## 177     1440          17.1132075
## 178     1445          26.0754717
## 179     1450          43.6226415
## 180     1455          43.7735849
## 181     1500          30.0188679
## 182     1505          36.0754717
## 183     1510          35.4905660
## 184     1515          38.8490566
## 185     1520          45.9622642
## 186     1525          47.7547170
## 187     1530          48.1320755
## 188     1535          65.3207547
## 189     1540          82.9056604
## 190     1545          98.6603774
## 191     1550         102.1132075
## 192     1555          83.9622642
## 193     1600          62.1320755
## 194     1605          64.1320755
## 195     1610          74.5471698
## 196     1615          63.1698113
## 197     1620          56.9056604
## 198     1625          59.7735849
## 199     1630          43.8679245
## 200     1635          38.5660377
## 201     1640          44.6603774
## 202     1645          45.4528302
## 203     1650          46.2075472
## 204     1655          43.6792453
## 205     1700          46.6226415
## 206     1705          56.3018868
## 207     1710          50.7169811
## 208     1715          61.2264151
## 209     1720          72.7169811
## 210     1725          78.9433962
## 211     1730          68.9433962
## 212     1735          59.6603774
## 213     1740          75.0943396
## 214     1745          56.5094340
## 215     1750          34.7735849
## 216     1755          37.4528302
## 217     1800          40.6792453
## 218     1805          58.0188679
## 219     1810          74.6981132
## 220     1815          85.3207547
## 221     1820          59.2641509
## 222     1825          67.7735849
## 223     1830          77.6981132
## 224     1835          74.2452830
## 225     1840          85.3396226
## 226     1845          99.4528302
## 227     1850          86.5849057
## 228     1855          85.6037736
## 229     1900          84.8679245
## 230     1905          77.8301887
## 231     1910          58.0377358
## 232     1915          53.3584906
## 233     1920          36.3207547
## 234     1925          20.7169811
## 235     1930          27.3962264
## 236     1935          40.0188679
## 237     1940          30.2075472
## 238     1945          25.5471698
## 239     1950          45.6603774
## 240     1955          33.5283019
## 241     2000          19.6226415
## 242     2005          19.0188679
## 243     2010          19.3396226
## 244     2015          33.3396226
## 245     2020          26.8113208
## 246     2025          21.1698113
## 247     2030          27.3018868
## 248     2035          21.3396226
## 249     2040          19.5471698
## 250     2045          21.3207547
## 251     2050          32.3018868
## 252     2055          20.1509434
## 253     2100          15.9433962
## 254     2105          17.2264151
## 255     2110          23.4528302
## 256     2115          19.2452830
## 257     2120          12.4528302
## 258     2125           8.0188679
## 259     2130          14.6603774
## 260     2135          16.3018868
## 261     2140           8.6792453
## 262     2145           7.7924528
## 263     2150           8.1320755
## 264     2155           2.6226415
## 265     2200           1.4528302
## 266     2205           3.6792453
## 267     2210           4.8113208
## 268     2215           8.5094340
## 269     2220           7.0754717
## 270     2225           8.6981132
## 271     2230           9.7547170
## 272     2235           2.2075472
## 273     2240           0.3207547
## 274     2245           0.1132075
## 275     2250           1.6037736
## 276     2255           4.6037736
## 277     2300           3.3018868
## 278     2305           2.8490566
## 279     2310           0.0000000
## 280     2315           0.8301887
## 281     2320           0.9622642
## 282     2325           1.5849057
## 283     2330           2.6037736
## 284     2335           4.6981132
## 285     2340           3.3018868
## 286     2345           0.6415094
## 287     2350           0.2264151
## 288     2355           1.0754717
```

```r
with(intervalsteps, plot(interval, mean.interval.steps, type = "l"))
```

![](PA1_template_files/figure-html/intervalsteps-1.png) 

Now, I need to find the 5-minute interval5-minute interval, on average across all the days in the dataset, contains the maximum number of steps. 

Note to self for future reference: in my first run at this I tried *"with(intervalsteps, which.max(mean.interval.steps))"*, which gave me the row number, but not interval or the mean value in steps. So, instead of putting in a second statement subsetting to the row (which could vary), I collapsed the *which.max* into the subset so it would wouldn't be fixed to row 104.



```r
intervalsteps[which.max(intervalsteps$mean.interval.steps),]
```

```
##     interval mean.interval.steps
## 104      835            206.1698
```

## Inputing missing values
What are the missing values? To be thorough, I checked to makes sure there were no missing dates or intervals, but I wasn't expecting to see any.


```r
sum(is.na(fitdata$steps))
```

```
## [1] 2304
```

```r
sum(is.na(fitdata$date))
```

```
## [1] 0
```

```r
sum(is.na(fitdata$interval))
```

```
## [1] 0
```

Since the exercise is open ended and there are zero values for data, I'm going to say if it's an "NA" then go ahead and stick a 0 in it. I know: I could merge the meaninterval df to the fitdata df, and then loop through it replacing the mean for the interval with the NAs. But, this is my "Snape's 'Just stick a bezoar down his throat!" approach to the assignment -- Professor Snape being smarter than the rest of us.


```r
fitdataskewed <- fitdata
fitdataskewed[is.na(fitdataskewed),1] <- 0
```

And, so if we replot it:


```r
library(plyr)
library(dplyr)
skewedsteps <- ddply(fitdataskewed, .(date), summarize, mean.total.steps = mean(steps))
skewedsteps
```

```
##          date mean.total.steps
## 1  2012-10-01        0.0000000
## 2  2012-10-02        0.4375000
## 3  2012-10-03       39.4166667
## 4  2012-10-04       42.0694444
## 5  2012-10-05       46.1597222
## 6  2012-10-06       53.5416667
## 7  2012-10-07       38.2465278
## 8  2012-10-08        0.0000000
## 9  2012-10-09       44.4826389
## 10 2012-10-10       34.3750000
## 11 2012-10-11       35.7777778
## 12 2012-10-12       60.3541667
## 13 2012-10-13       43.1458333
## 14 2012-10-14       52.4236111
## 15 2012-10-15       35.2048611
## 16 2012-10-16       52.3750000
## 17 2012-10-17       46.7083333
## 18 2012-10-18       34.9166667
## 19 2012-10-19       41.0729167
## 20 2012-10-20       36.0937500
## 21 2012-10-21       30.6284722
## 22 2012-10-22       46.7361111
## 23 2012-10-23       30.9652778
## 24 2012-10-24       29.0104167
## 25 2012-10-25        8.6527778
## 26 2012-10-26       23.5347222
## 27 2012-10-27       35.1354167
## 28 2012-10-28       39.7847222
## 29 2012-10-29       17.4236111
## 30 2012-10-30       34.0937500
## 31 2012-10-31       53.5208333
## 32 2012-11-01        0.0000000
## 33 2012-11-02       36.8055556
## 34 2012-11-03       36.7048611
## 35 2012-11-04        0.0000000
## 36 2012-11-05       36.2465278
## 37 2012-11-06       28.9375000
## 38 2012-11-07       44.7326389
## 39 2012-11-08       11.1770833
## 40 2012-11-09        0.0000000
## 41 2012-11-10        0.0000000
## 42 2012-11-11       43.7777778
## 43 2012-11-12       37.3784722
## 44 2012-11-13       25.4722222
## 45 2012-11-14        0.0000000
## 46 2012-11-15        0.1423611
## 47 2012-11-16       18.8923611
## 48 2012-11-17       49.7881944
## 49 2012-11-18       52.4652778
## 50 2012-11-19       30.6979167
## 51 2012-11-20       15.5277778
## 52 2012-11-21       44.3993056
## 53 2012-11-22       70.9270833
## 54 2012-11-23       73.5902778
## 55 2012-11-24       50.2708333
## 56 2012-11-25       41.0902778
## 57 2012-11-26       38.7569444
## 58 2012-11-27       47.3819444
## 59 2012-11-28       35.3576389
## 60 2012-11-29       24.4687500
## 61 2012-11-30        0.0000000
```

```r
hist(skewedsteps$mean.total.steps, xlab = "total skewed steps daily - ddply")
```

![](PA1_template_files/figure-html/skewedsteps-1.png) 

```r
mean(skewedsteps$mean.total.steps, na.rm = TRUE)
```

```
## [1] 32.47996
```

```r
median(skewedsteps$mean.total.steps, na.rm = TRUE)
```

```
## [1] 36.09375
```

The skewed values, not surprisingly, increase the "0" count in the histogram and lower the mean and median, since we're introducing more zero values to the dataset.


## Are there differences in activity patterns between weekdays and weekends?
For this, I've pulled heavily on Stackoverflow for the simplest and fastest answer.  

"[Stackoverflow](http://stackoverflow.com/questions/33104456/divide-time-series-data-into-weekday-and-weekend-datasets-using-r)"

To answer the question whether there are differences in patterns, I just need to tack a "week end" or "week day" designation to the fitdata df.  


```r
weekday <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}

fitdata$weekday <- as.factor(sapply(fitdata$date, weekday))

par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    dataout <- aggregate(steps ~ interval, data = fitdata, subset = fitdata$weekday == type, FUN = mean)
    plot(dataout, type = "l", main = type)
}
```

![](PA1_template_files/figure-html/lousyplottingsolution-1.png) 
