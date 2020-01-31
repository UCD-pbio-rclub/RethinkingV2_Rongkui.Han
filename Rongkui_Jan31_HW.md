---
title: "HW_Jan31"
author: "Rongkui Han"
date: "1/31/2020"
output: 
  html_document: 
    keep_md: yes
---


```r
data = read.csv("/Users/rongkui/Desktop/StatisticalRethinking/figure4phyE.csv")
head(data)
```

```
##   genotype treatment flat day   epi  int1  int2  int3  pet1  pet2  pet3  pet4
## 1 phyB1/B2     shade    1  21 24.03  3.18  0.00  0.00 14.11  4.88  0.00  0.00
## 2 phyB1/B2     shade    1  28 47.37 21.67 11.27  3.13 31.05 26.81 11.10  2.61
## 3 phyB1/B2     shade    1  35 58.83 40.60 72.29 52.70 42.15 49.60 49.58 30.49
## 4 phyB1/B2     shade    1  21 29.85  2.39  2.41  0.00 14.44 11.55  0.00  0.00
## 5 phyB1/B2     shade    1  28 59.69  3.36 25.52  6.62 35.10 29.15 20.05  9.39
## 6 phyB1/B2     shade    1  35 69.55  4.91 56.64 35.52 49.81 34.65 47.58 40.54
```

### Q1

#### a) subset the data for day 35     


```r
data = data[data$day == 35,]
```

#### b) create a new column "stem_length" that is the sum of epi, int1, int2, and int3    

```r
data["stem_length"] = (data$epi + data$int1 + data$int2 + data$int3)
```

#### c) although flats are listed as 1-6, flats in sun and shade are separate. Create a new column "flat2" that corrects for this.    

```r
data['flat2'] = ifelse(data$treatment == 'shade', data$flat + 6, data$flat)
data
```

```
##       genotype treatment flat day   epi  int1   int2  int3  pet1  pet2  pet3
## 3     phyB1/B2     shade    1  35 58.83 40.60  72.29 52.70 42.15 49.60 49.58
## 6     phyB1/B2     shade    1  35 69.55  4.91  56.64 35.52 49.81 34.65 47.58
## 9     phyB1/B2     shade    2  35 70.22 62.61  59.66 31.25 46.28 56.79 47.93
## 12    phyB1/B2     shade    3  35 54.60 38.64  67.62 35.19 39.11 66.57 68.85
## 15    phyB1/B2     shade    3  35 58.16 55.31 104.74 39.97 44.85 65.33 60.64
## 18    phyB1/B2     shade    5  35 59.88 58.67  74.77 33.33 36.40 39.28 50.71
## 21    phyB1/B2       sun    1  35 37.38 28.14  32.34 15.44 32.52 39.85 45.45
## 24    phyB1/B2       sun    1  35 66.53 36.08  25.82 19.14 50.46 49.34 49.63
## 27    phyB1/B2       sun    2  35 43.99  3.02  37.02 12.15 36.14 40.57 29.88
## 30    phyB1/B2       sun    3  35 67.73 41.21  30.41 20.60 45.82 53.15 44.34
## 33    phyB1/B2       sun    3  35 59.90 45.39  39.43 22.26 51.76 53.40 58.32
## 36    phyB1/B2       sun    5  35 71.59 31.63  45.72 23.95 44.29 65.48 43.06
## 39    phyEami3     shade    1  35 17.56 21.75  18.68 29.44 38.45 40.36 50.76
## 42    phyEami3     shade    2  35 60.65 13.49  47.05 24.56 74.41 91.35 70.59
## 45    phyEami3     shade    5  35 42.75 54.18  51.92 41.96 69.90 85.70 81.31
## 48    phyEami3     shade    5  35 43.62 40.79  32.68 32.72 64.56 81.16 78.71
## 51    phyEami3     shade    6  35 41.19 52.25  41.95 30.71 81.25 92.87 88.74
## 54    phyEami3     shade    6  35 49.12 48.29  48.01 29.91 85.22 95.14 93.37
## 57    phyEami3       sun    1  35 31.98 12.13  19.42 20.50 71.52 62.23 75.68
## 60    phyEami3       sun    2  35 26.98 28.51  15.66 12.88 64.46 70.37 52.62
## 63    phyEami3       sun    5  35 30.78 24.65  26.90 17.85 68.64 74.43 77.20
## 66    phyEami3       sun    5  35 33.30 34.98  28.85 20.25 74.46 78.72 65.91
## 69    phyEami3       sun    6  35 21.76 17.73  18.67 16.03 59.47 70.26 64.29
## 72    phyEami3       sun    6  35 31.21 24.81  20.44 15.66 45.60 79.99 61.02
## 75    phyEami3     shade    1  35 36.27 34.18  35.25 23.27 72.18 88.05 88.93
## 78    phyEami3     shade    1  35 40.50 32.05  25.95 10.15 55.29 60.69 44.11
## 81    phyEami3     shade    4  35 43.22 60.46  37.06 31.78 82.63 64.16 65.35
## 84    phyEami3     shade    5  35 47.20 45.87  38.28 31.63 90.24 95.05 57.90
## 87    phyEami3     shade    5  35 43.48 38.52  39.61 26.87 91.87 87.50 51.00
## 90    phyEami3     shade    6  35 26.36 43.50  29.08 18.76 51.06 36.75 18.52
## 93    phyEami3       sun    1  35 38.93 30.66  19.53  9.02 57.52 65.66 45.75
## 96    phyEami3       sun    1  35 33.25 30.30  14.89 16.07 76.00 78.24 49.58
## 99    phyEami3       sun    6  35 24.67 27.51  10.75  8.79 55.79 36.44 25.96
## 102   phyEami7     shade    2  35 39.16 31.13  40.15 24.13 53.58 64.96 64.12
## 105   phyEami7     shade    2  35 27.71 29.05  31.93 21.61 70.73 71.80 68.93
## 108   phyEami7     shade    3  35 43.18 44.59  50.15 25.79 80.63 87.41 72.11
## 111   phyEami7     shade    4  35 37.34 40.13  61.11 35.06 74.65 89.41 84.21
## 114   phyEami7     shade    5  35 25.82 32.72  41.66 25.35 67.24 75.55 70.26
## 117   phyEami7     shade    6  35 27.59 35.70  50.44 33.24 56.79 91.48 75.32
## 120   phyEami7       sun    2  35 26.56 18.81  23.98 18.02 57.68 63.26 67.39
## 123   phyEami7       sun    4  35 32.10 21.65  20.50 18.04 60.90 57.37 58.29
## 126   phyEami7       sun    6  35 11.45 10.87  11.71 17.68 41.37 62.04 47.85
## 129   phyEami7     shade    1  35 34.30 34.04  45.64 29.77 65.37 87.39 82.99
## 132   phyEami7     shade    2  35 46.25 33.13  47.56 36.13 69.77 73.84 85.15
## 135   phyEami7     shade    3  35 38.46 35.26  54.85 37.66 74.16 71.33 79.65
## 138   phyEami7     shade    3  35 36.76 39.48  42.52 29.26 78.22 74.06 66.15
## 141   phyEami7     shade    4  35 47.56 46.46  28.00 25.43 80.42 64.91 56.05
## 144   phyEami7     shade    6  35 29.42 40.07  62.20 40.67 71.27 96.23 98.57
## 147   phyEami7       sun    1  35 28.06 19.25  26.02 17.02 63.45 61.96 61.73
## 150   phyEami7       sun    3  35 26.62 21.88  23.86 13.92 73.37 65.36 51.99
## 153   phyEami7       sun    3  35 27.00 17.45  19.92 12.96 69.70 49.96 44.68
## 156   phyEami7       sun    4  35 27.57 23.13  25.27 15.62 77.17 65.88 55.88
## 159      phyB1     shade    2  35 82.17  3.58  87.24 42.02 38.49 28.94 75.31
## 162      phyB1     shade    4  35 60.60 58.80  67.75 30.47 41.27 67.16 58.19
## 165      phyB1     shade    4  35 79.68 63.22  64.79 56.70 59.69 58.25 78.14
## 168      phyB1     shade    4  35 87.74 71.27  65.01 30.20 47.51 72.81 57.57
## 171      phyB1     shade    5  35 67.48 59.78  55.99 39.32 53.20 47.26 70.95
## 174      phyB1     shade    6  35 54.60 56.93  55.58 33.68 52.64 53.34 73.45
## 177      phyB1       sun    2  35 77.45 48.23  35.37 29.88 62.41 45.15 62.16
## 180      phyB1       sun    4  35 61.53 45.64  26.77 22.77 38.00 46.46 40.77
## 183      phyB1       sun    4  35 60.07 40.88  46.98 25.08 45.00 60.06 54.68
## 186      phyB1       sun    4  35 58.24 52.78  51.41 37.72 56.85 60.45 80.31
## 189      phyB1       sun    5  35 63.68 33.62  31.73 24.88 51.16 60.36 57.04
## 192      phyB1       sun    6  35 49.30 41.33  25.77 11.59 37.45 32.16 24.27
## 195      phyB2     shade    1  35 29.90 40.47  54.81 33.43 59.38 94.54 77.88
## 198      phyB2     shade    3  35 36.26 64.08  56.68 33.64 68.29 72.61 81.11
## 201      phyB2     shade    4  35 30.14 50.90  43.25 33.25 51.08 47.12 54.85
## 204      phyB2     shade    5  35 40.00 64.82  55.96 36.71 63.58 84.62 62.24
## 207      phyB2     shade    6  35 27.36 45.84  32.90 14.12 61.19 48.15 39.51
## 210      phyB2     shade    6  35 36.20 35.99  18.06  9.16 52.41 50.81 40.60
## 213      phyB2       sun    1  35 33.30 31.52  28.37 15.48 65.90 63.81 48.57
## 216      phyB2       sun    3  35 26.41 28.04  29.20 19.28 60.33 60.06 52.71
## 219      phyB2       sun    4  35 30.86 33.96  22.30 14.17 58.71 43.24 47.30
## 222      phyB2       sun    5  35 24.80 31.72  28.52 12.52 56.94 67.33 51.92
## 225      phyB2       sun    6  35 10.94 15.30  19.20 19.15 56.21 47.69 45.67
## 228      phyB2       sun    6  35 21.46 23.32  23.33 16.90 58.45 82.19 56.51
## 231 Moneymaker     shade    1  35 37.80 46.85  48.61 30.14 68.06 83.27 70.19
## 234 Moneymaker     shade    2  35 41.90 43.18  39.16 26.85 67.35 92.68 62.96
## 237 Moneymaker     shade    2  35 30.30 33.38  43.58 31.57 62.62 94.58 83.10
## 240 Moneymaker     shade    3  35 31.30 53.73  46.52 27.33 56.16 87.10 60.80
## 243 Moneymaker     shade    3  35 39.25 39.20  43.86 14.73 61.31  0.00 46.11
## 246 Moneymaker     shade    4  35 43.74 52.95  40.77 28.62 65.32 75.91 58.40
## 249 Moneymaker       sun    1  35 24.50 31.22  20.77 15.68 66.18 89.30 56.91
## 252 Moneymaker       sun    2  35 25.58 27.93  22.92 15.83 68.57 73.96 67.40
## 255 Moneymaker       sun    2  35 26.16 19.68  21.28 29.02 62.64 61.92 74.27
## 258 Moneymaker       sun    3  35 31.02 29.54  17.75 14.53 67.43 69.29 56.67
## 261 Moneymaker       sun    3  35 33.56 25.40  19.31  9.76 58.06 61.62 30.21
## 264 Moneymaker       sun    4  35 31.69 29.34  21.61 12.91 66.04 57.87 37.26
##      pet4 stem_length flat2
## 3   30.49      224.42     7
## 6   40.54      166.62     7
## 9   28.40      223.74     8
## 12  53.21      196.05     9
## 15  43.54      258.18     9
## 18  33.98      226.65    11
## 21  42.38      113.30     1
## 24  38.70      147.57     1
## 27  27.69       96.18     2
## 30  17.87      159.95     3
## 33  37.05      166.98     3
## 36  22.30      172.89     5
## 39  72.12       87.43     7
## 42  58.01      145.75     8
## 45  40.02      190.81    11
## 48  62.76      149.81    11
## 51  46.56      166.10    12
## 54  38.98      175.33    12
## 57  57.52       84.03     1
## 60  41.95       84.03     2
## 63  36.30      100.18     5
## 66  31.22      117.38     5
## 69  34.13       74.19     6
## 72  23.78       92.12     6
## 75  43.68      128.97     7
## 78  16.67      108.65     7
## 81  39.17      172.52    10
## 84  56.46      162.98    11
## 87  42.85      148.48    11
## 90  11.66      117.70    12
## 93  32.15       98.14     1
## 96  32.63       94.51     1
## 99  11.97       71.72     6
## 102 19.67      134.57     8
## 105 30.59      110.30     8
## 108 33.61      163.71     9
## 111 45.45      173.64    10
## 114 38.04      125.55    11
## 117 61.10      146.97    12
## 120 32.52       87.37     2
## 123 46.57       92.29     4
## 126 56.35       51.71     6
## 129 35.85      143.75     7
## 132 51.56      163.07     8
## 135 37.36      166.23     9
## 138 30.94      148.02     9
## 141 28.35      147.45    10
## 144 44.42      172.36    12
## 147 44.50       90.35     1
## 150 18.66       86.28     3
## 153 19.48       77.33     3
## 156 27.46       91.59     4
## 159 69.52      215.01     8
## 162 41.29      217.62    10
## 165 48.95      264.39    10
## 168 30.27      254.22    10
## 171 30.34      222.57    11
## 174 61.80      200.79    12
## 177 55.62      190.93     2
## 180 38.10      156.71     4
## 183 31.36      173.01     4
## 186 37.22      200.15     4
## 189 27.25      153.91     5
## 192 17.70      127.99     6
## 195 37.71      158.61     7
## 198 44.84      190.66     9
## 201 24.57      157.54    10
## 204 50.75      197.49    11
## 207 24.98      120.22    12
## 210 10.83       99.41    12
## 213 41.15      108.67     1
## 216 30.75      102.93     3
## 219 26.18      101.29     4
## 222 21.08       97.56     5
## 225 43.00       64.59     6
## 228 40.32       85.01     6
## 231 29.46      163.40     7
## 234 29.62      151.09     8
## 237 58.89      138.83     8
## 240 25.80      158.88     9
## 243 13.55      137.04     9
## 246 27.20      166.08    10
## 249 44.68       92.17     1
## 252 32.26       92.26     2
## 255 31.91       96.14     2
## 258 22.97       92.84     3
## 261 19.11       88.03     3
## 264 23.66       95.55     4
```

```r
mean(data$stem_length)
```

```
## [1] 141.2442
```

Ultimately you want to know if any of the mutants have a different length from Moneymaker, in sun or in shade, or if the response to shade differs.    

### Q2
#### a) don't include flat.  Determine whether genotype, treatment, and their interaction are important predictors of stem_length     


```r
library(rethinking)
```

```
## Loading required package: rstan
```

```
## Loading required package: StanHeaders
```

```
## Loading required package: ggplot2
```

```
## rstan (Version 2.19.2, GitRev: 2e1f913d3ca3)
```

```
## For execution on a local, multicore CPU with excess RAM we recommend calling
## options(mc.cores = parallel::detectCores()).
## To avoid recompilation of unchanged Stan programs, we recommend calling
## rstan_options(auto_write = TRUE)
```

```
## Loading required package: parallel
```

```
## Loading required package: dagitty
```

```
## rethinking (Version 1.92)
```

```
## 
## Attaching package: 'rethinking'
```

```
## The following object is masked from 'package:stats':
## 
##     rstudent
```

```r
d = list(
  geno = as.numeric(data$genotype),
  trt = ifelse(data$treatment == "shade", 1, 0),
  sl = data$stem_length
)

mod2a = ulam(
  alist(
    sl ~ dnorm(mu, sigma_sl),
    mu <- g[geno] + t*trt + inter[geno]*trt,
    g[geno]  ~ dnorm(141, 1),
    t ~ dnorm(0,1),
    inter[geno] ~ dnorm(0,1),
    sigma_sl ~ dexp(1)
  ), data = d, chains = 4, cores = 4, iter = 2000, log_lik = TRUE
)
```


```r
precis(mod2a, depth = 2)
```

```
##                  mean        sd       5.5%      94.5%    n_eff      Rhat
## g[1]     140.86993229 0.9610291 139.342222 142.432839 5006.343 0.9997701
## g[2]     141.43515696 0.9997369 139.862500 143.045438 5062.154 0.9995017
## g[3]     141.28223932 0.9969408 139.641908 142.881194 4423.976 0.9993772
## g[4]     140.86473726 0.9917080 139.284886 142.471064 5272.767 0.9992441
## g[5]     140.74951110 0.9769354 139.171827 142.315233 4623.129 1.0009431
## g[6]     140.78240826 0.9906010 139.156661 142.349111 4214.593 1.0000168
## t          0.80494730 0.9813404  -0.780948   2.332001 4448.207 0.9993524
## inter[1]   0.05113992 1.0030479  -1.576164   1.636225 4451.832 1.0002208
## inter[2]   0.31256664 0.9952327  -1.284446   1.889222 5103.910 1.0000776
## inter[3]   0.26791666 0.9747588  -1.313483   1.820070 5471.340 0.9996836
## inter[4]   0.05458381 1.0009074  -1.540853   1.663062 4849.035 0.9996792
## inter[5]   0.03435472 0.9694022  -1.521781   1.592523 4577.473 0.9996832
## inter[6]   0.07101087 1.0045570  -1.558211   1.694379 5900.817 1.0001170
## sigma_sl  39.96638434 2.3095189  36.450756  43.844909 4461.460 1.0002546
```

> Nothing seemed to have any effect...? Try without interaction   


```r
mod2a2 = ulam(
  alist(
    sl ~ dnorm(mu, sigma_sl),
    mu <- g[geno] + t*trt,
    g[geno]  ~ dnorm(141, 1),
    t ~ dnorm(0,1),
    sigma_sl ~ dexp(1)
  ), data = d, chains = 4, cores = 4, iter = 2000, log_lik = TRUE
)
```


```r
precis(mod2a2, depth = 2)
```

```
##                 mean        sd        5.5%      94.5%    n_eff      Rhat
## g[1]     140.8418764 0.9815018 139.2755124 142.415335 6197.758 0.9996941
## g[2]     141.4151581 0.9948940 139.8248750 143.005438 6158.348 0.9996495
## g[3]     141.2753193 1.0378298 139.6365715 142.957286 8374.800 0.9994535
## g[4]     140.8762173 0.9701054 139.3450692 142.442586 6222.511 0.9991327
## g[5]     140.7534920 0.9546851 139.2239941 142.294705 5019.705 0.9999027
## g[6]     140.8179095 0.9919828 139.2193469 142.393988 6084.536 0.9993157
## t          0.7937818 0.9847230  -0.7979999   2.355953 5749.948 0.9996444
## sigma_sl  40.0400392 2.4254183  36.3358312  44.080589 5285.963 0.9997465
```


```r
compare(mod2a, mod2a2)
```

```
##            WAIC    pWAIC     dWAIC    weight       SE       dSE
## mod2a  938.7068 1.096649 0.0000000 0.5548598 17.08328        NA
## mod2a2 939.1474 1.178191 0.4406522 0.4451402 17.16190 0.1055754
```

> The one with interaction is slightly better

b) starting with your best model from a), include flat without pooling     


```r
d2 = list(
  geno = as.numeric(data$genotype),
  trt = ifelse(data$treatment == "shade", 1, 0),
  flat = data$flat,
  sl = data$stem_length
)

mod2b = ulam(
  alist(
    sl ~ dnorm(mu, sigma_sl),
    mu <- g[geno] + t*trt + inter[geno]*trt + bf[flat],
    g[geno]  ~ dnorm(141, 1),
    t ~ dnorm(0,1),
    inter[geno] ~ dnorm(0,1),
    bf[flat] ~dnorm(0,1),
    sigma_sl ~ dexp(1)
  ), data = d2, chains = 4, cores = 4, iter = 2000, log_lik = TRUE
)
```


```r
precis(mod2b, depth = 2)
```

```
##                  mean        sd        5.5%      94.5%    n_eff      Rhat
## g[1]     140.85508219 1.0018892 139.2358019 142.515753 5914.201 0.9994820
## g[2]     141.42224426 0.9774097 139.9005407 143.007478 5901.474 0.9990581
## g[3]     141.27925963 1.0158710 139.6287217 142.927157 5469.742 0.9997910
## g[4]     140.86974877 0.9878940 139.2948828 142.466643 5241.412 0.9993763
## g[5]     140.77349463 1.0093790 139.1543342 142.369575 5248.433 1.0002894
## g[6]     140.78600189 0.9790641 139.2037993 142.338377 5531.420 1.0001049
## t          0.78901889 0.9898903  -0.7562626   2.353746 5138.217 1.0004772
## inter[1]   0.03249154 1.0220649  -1.5918005   1.649421 6065.164 0.9993309
## inter[2]   0.33162362 1.0312069  -1.3147803   1.973875 6433.505 0.9999490
## inter[3]   0.28665452 1.0006348  -1.3099652   1.907862 6155.080 0.9995757
## inter[4]   0.04770119 1.0107715  -1.5777032   1.683535 5547.877 0.9993209
## inter[5]   0.03239574 0.9844127  -1.5557533   1.617777 5482.839 0.9992654
## inter[6]   0.04609417 0.9964544  -1.5644040   1.643832 5112.981 0.9996942
## bf[1]     -0.17531357 0.9558995  -1.6960168   1.375650 5606.611 1.0000201
## bf[2]     -0.04786753 0.9812520  -1.6068948   1.498278 4753.197 1.0004275
## bf[3]      0.04071646 0.9822226  -1.5208944   1.607896 6209.353 1.0001163
## bf[4]      0.21937758 0.9633609  -1.2975641   1.768288 5724.588 0.9996056
## bf[5]      0.12890478 0.9972035  -1.4752203   1.757585 4979.193 0.9992428
## bf[6]     -0.21507037 1.0069183  -1.7872296   1.389025 5809.564 0.9998425
## sigma_sl  39.95855184 2.3494433  36.3743436  43.846760 5282.542 0.9994534
```

c) starting with your best model from a), use a hierarchical model that allows partial pooling across flats     


```r
mod2c = ulam(
  alist(
    sl ~ dnorm(mu, sigma_sl),
    mu <- g[geno] + t*trt + inter[geno]*trt + bf[flat],
    g[geno]  ~ dnorm(141, 1),
    t ~ dnorm(0,1),
    inter[geno] ~ dnorm(0,1),
    bf[flat] ~ dnorm(flat_bar,sigma_flat),
    flat_bar ~ dnorm(0,1), 
    c(sigma_sl, sigma_flat) ~ dexp(1)
  ), data = d2, chains = 4, cores = 4, iter = 2000, log_lik = TRUE
)
```

```
## Warning: There were 213 divergent transitions after warmup. Increasing adapt_delta above 0.95 may help. See
## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
```

```
## Warning: Examine the pairs() plot to diagnose sampling problems
```

```
## Warning: The largest R-hat is 1.11, indicating chains have not mixed.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#r-hat
```

```
## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#bulk-ess
```

```
## Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#tail-ess
```


```r
precis(mod2c, depth = 2)
```

```
##                    mean        sd        5.5%      94.5%      n_eff     Rhat
## g[1]       140.85887526 0.9366688 139.3160683 142.358789 4060.67007 1.000068
## g[2]       141.47405627 1.0042574 139.8279415 143.106526 2453.15430 1.003138
## g[3]       141.32620015 0.9831672 139.7040061 142.847718  568.20126 1.012536
## g[4]       140.89230962 0.9965498 139.3182525 142.476660 3834.56704 1.000292
## g[5]       140.78266063 0.9390834 139.2052879 142.235189 1627.19639 1.006621
## g[6]       140.77216940 0.9669042 139.2553706 142.323703 3252.64057 1.000913
## t            0.69373643 0.9893512  -0.8044265   2.260321   94.91400 1.043659
## inter[1]     0.05179314 0.9704828  -1.5699503   1.637180 3430.16707 1.001074
## inter[2]     0.41914259 1.0337548  -1.2519057   1.913290   78.85086 1.059367
## inter[3]     0.25994797 0.9760807  -1.3112193   1.852178 4394.41858 1.002046
## inter[4]    -0.02338329 0.9606422  -1.5030511   1.564894  496.09605 1.013081
## inter[5]     0.18467270 1.0418269  -1.4613711   1.721701   60.19216 1.067093
## inter[6]     0.17834582 1.0202460  -1.4981324   1.684154  122.53509 1.039171
## bf[1]       -0.27963812 1.9711832  -3.3835012   2.050203  405.36505 1.021552
## bf[2]        0.07135198 1.7903048  -2.5566888   2.571797  624.46676 1.010914
## bf[3]        0.17184952 1.7938561  -2.2968541   2.567987 1797.19264 1.007392
## bf[4]        0.61660961 2.0632716  -1.9377444   3.740906 1022.69458 1.006884
## bf[5]        0.46933971 2.0054790  -2.0709575   3.577426 1148.59924 1.006038
## bf[6]       -0.36946156 1.9351169  -3.5364539   1.978439  472.93938 1.021297
## flat_bar     0.10196198 0.9714112  -1.4485014   1.501671  130.81810 1.038804
## sigma_flat   1.13044524 1.1931145   0.1158100   3.489587  147.80598 1.049581
## sigma_sl    39.99630854 2.2959395  36.3208353  43.690251  623.58588 1.013135
```

Q3) Compare the models, which is preferred?


```r
compare(mod2a, mod2b, mod2c)
```

```
##           WAIC    pWAIC     dWAIC    weight       SE       dSE
## mod2c 938.5102 1.370084 0.0000000 0.3527645 16.97722        NA
## mod2b 938.6589 1.203326 0.1486979 0.3274881 17.07230 0.1565956
## mod2a 938.7068 1.096649 0.1965386 0.3197474 17.08328 0.2205169
```



Q4) Using the hierarchical model, make posterior predictions
a) for average cluster      


```r
post <- extract.samples(mod2c)
str(post)
```

```
## List of 7
##  $ g         : num [1:4000, 1:6] 141 141 142 140 140 ...
##  $ t         : num [1:4000(1d)] 1.8368 0.0748 0.4258 0.7257 -0.1612 ...
##  $ inter     : num [1:4000, 1:6] 1.157 0.542 -0.859 -0.519 1.342 ...
##  $ bf        : num [1:4000, 1:6] -2.8033 0.79 -1.155 -1.9694 0.0399 ...
##  $ flat_bar  : num [1:4000(1d)] 0.159 -1.126 -1.315 -0.543 -0.331 ...
##  $ sigma_flat: num [1:4000(1d)] 2.488 1.501 0.586 2.786 1.49 ...
##  $ sigma_sl  : num [1:4000(1d)] 38.8 42.2 39.8 39.8 41.1 ...
##  - attr(*, "source")= chr "ulam posterior: 4000 samples from mod2c"
```



b) for same clusters   




c) showing the "marginal" from cluster



d) showing new clusters.




Q5) Reparameterize the model to help with divergent transitions (even if there aren't any)


```r
mod5 = ulam(
  alist(
    sl ~ dnorm(mu, sigma_sl),
    mu <- g[geno] + t*trt + inter[geno]*trt + bf[flat] + sigma_flat*norm,
    g[geno]  ~ dnorm(141, 1),
    t ~ dnorm(0,1),
    inter[geno] ~ dnorm(0,1),
    bf[flat] ~ dnorm(flat_bar,1),
    norm ~ dnorm(0,1), 
    flat_bar ~ dnorm(0,1),
    c(sigma_sl, sigma_flat) ~ dexp(1)
  ), data = d2, chains = 4, cores = 4, iter = 2000, log_lik = TRUE
)
```


```r
compare(mod5, mod2c)
```

```
##           WAIC    pWAIC     dWAIC   weight       SE       dSE
## mod2c 938.5102 1.370084 0.0000000 0.551433 16.97722        NA
## mod5  938.9232 1.398635 0.4129245 0.448567 17.08448 0.1718403
```


Q6--optional)
a) Which genotypes differ from MoneyMaker in Sun conditions?
b) Which genotypes differ from MoneyMaker in Shade conditions?
c) Which genotypes differ from MoneyMaker in their response to shade (difference in sun vs shade)?



