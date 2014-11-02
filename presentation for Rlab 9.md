MFE5209 Presentation (Rlab 9)
========================================================
#width: 1920
#height: 1080
width: 1536
height: 864
author:Wu Fuheng,  Rui Dai, Ou Jin, Meng Weiran, Hao Sijia
date: Oct 25, 2014
font-family: 'consolas'
transition: rotate
autosize: true
css: rlab9.css

>
> - (Multi)Cointegration
> - VECM
>

<img src="http://www.rmi.nus.edu.sg/_images/rmilogo_27June2013.jpg" class="imgCenter" border=0/>

Problem Set 1 - Cointegration Analysis of Midcap Prices
========================================================
source: presentation1.R

> The data set midcapD.ts in the fEcofin package has daily returns on 20 midcap stocks in columns 2-21. Columns 1 and 22 contain the date and market returns, respectively. In this section, we will use returns on the first 10 stocks. To find the stock prices from the returns, we use the relationship $P_t = P_0 * exp(r_1 + ...+ r_t)$ where $P_t$ and $r_t$ are the price and log return at time t. The returns will be used as approximations to the log returns. The prices at time 0 are unknown, so we will use $P_0 = 1$ for each stock. This means that the price series we use will be off by multiplicative factors. This does not affect the number of cointegration vectors. If we find that there are cointegration relationships, then it would be necessary to get the price data to investigate trading strategies. Johansen's cointegration analysis will be applied to the prices with the ca.jo function in the urca package.

<img src="http://upload.wikimedia.org/wikipedia/en/thumb/0/0d/Simpsons_FamilyPicture.png/375px-Simpsons_FamilyPicture.png" class="imgRight" border=0 width=20%/>

Question 1
========================================================
css: rlab9.css

> Q1: How many cointegration vectors were found in prices?


```r
rm(list=ls())
library(fEcofin)
library(urca)
# import time series data
if (!exists("midcapD.ts")){
  midcapD.ts=read.csv("8_midcapD.ts.csv",header=T,sep=";")
}
# Columns 1 and 22 contain the date and market returns, respectively.
#head(midcapD.ts)
# the data we are using
data = midcapD.ts[,2:11]
# P_t = P_0 * exp(r_1 + ...+ r_t)
prices= exp(apply(data,2,cumsum))
#controls the number of digits to print when printing numeric values
options(digits=3)
#Conducts the Johansen procedure on prices
summary(ca.jo(prices))
```

Question 1
========================================================
css: rlab9.css



> A1: According to Johansen test, if the test statistic is greater than the critial value, we can reject the NULL hypothesis. In this case, regardless of whether we use a 1%, 5%, or 10% level test, we suggest that no cointegration vector should exist.


```r
summary(ca.jo(prices))
```

```

###################### 
# Johansen-Procedure # 
###################### 

Test type: maximal eigenvalue statistic (lambda max) , with linear trend 

Eigenvalues (lambda):
 [1] 0.1106 0.0690 0.0451 0.0425 0.0392 0.0303 0.0234 0.0183 0.0126 0.0012

Values of teststatistic and critical values of test:

          test 10pct  5pct 1pct
r <= 9 |  0.60   6.5  8.18 11.6
r <= 8 |  6.31  12.9 14.90 19.2
r <= 7 |  9.20  18.9 21.07 25.8
r <= 6 | 11.79  24.8 27.14 32.1
r <= 5 | 15.32  30.8 33.32 38.8
r <= 4 | 19.89  36.2 39.43 44.6
r <= 3 | 21.65  42.1 44.91 51.3
r <= 2 | 22.98  48.4 51.07 57.1
r <= 1 | 35.62  54.0 57.00 63.4
r = 0  | 58.39  59.0 62.42 68.6

Eigenvectors, normalised to first column:
(These are the cointegration relations)

        LSCC.l2 CSGS.l2   EC.l2 NYB.l2 ALTR.l2 APH.l2 CLS.l2  NET.l2
LSCC.l2   1.000   1.000  1.0000  1.000   1.000  1.000  1.000  1.0000
CSGS.l2  -0.578  -0.313  1.3035 -0.675   0.253  2.033 -0.178  0.0817
EC.l2    -1.462   0.555 -2.1038  0.424  -0.154 -2.448  5.117  1.2895
NYB.l2    0.133   0.494  1.7841  0.144  -0.607  0.952 -2.056  0.0661
ALTR.l2  -0.478  -0.909 -0.0909  0.190  -0.382  0.612 -0.374 -0.1198
APH.l2    0.462   0.584  1.2265 -0.384  -0.594  2.450  0.866 -0.4848
CLS.l2   -0.743   0.511 -1.6584  0.421   0.543 -1.545 -0.509 -1.1178
NET.l2    0.321  -0.735 -0.9680 -0.111   0.387 -0.399 -0.704 -0.2828
SBUX.l2   0.175  -0.126  3.0176  0.107   0.313 -5.687 -0.640  0.5181
AYE.l2    1.622  -1.051 -3.8030  0.167   0.110  1.275  0.508 -1.0823
        SBUX.l2  AYE.l2
LSCC.l2  1.0000  1.0000
CSGS.l2  0.6842 -0.6231
EC.l2   -0.7969 -2.0101
NYB.l2   0.0492  1.4554
ALTR.l2 -0.7346  0.0315
APH.l2  -0.8706 -0.1240
CLS.l2   0.6116  0.4650
NET.l2   0.3443 -2.1168
SBUX.l2  0.1381 -0.7932
AYE.l2   1.2233  0.3036

Weights W:
(This is the loading matrix)

        LSCC.l2  CSGS.l2     EC.l2    NYB.l2  ALTR.l2    APH.l2    CLS.l2
LSCC.d -0.04932 -0.01821 -7.21e-03 -0.022796 -0.01975 -0.002012 -0.004329
CSGS.d  0.08464  0.02231 -1.16e-02 -0.005159 -0.01455 -0.004703  0.004656
EC.d    0.01005  0.00253  9.97e-04 -0.005290 -0.00290 -0.000849 -0.003138
NYB.d  -0.01295 -0.00369 -7.04e-05 -0.010523  0.00230 -0.000100  0.004885
ALTR.d -0.03046  0.02345 -3.40e-03 -0.060170 -0.01831 -0.002132 -0.006212
APH.d   0.00254 -0.02458 -4.97e-03 -0.000221  0.00804 -0.000611 -0.005505
CLS.d   0.01403 -0.01810  7.16e-04 -0.018589 -0.02294  0.001648 -0.001582
NET.d  -0.03348  0.02775 -2.37e-03  0.005281 -0.02406  0.000844 -0.002422
SBUX.d -0.01102  0.02846 -7.88e-03 -0.016569  0.00476  0.003421 -0.003414
AYE.d  -0.02319  0.01463  2.00e-03  0.000264  0.00592 -0.001106 -0.000306
         NET.l2   SBUX.l2   AYE.l2
LSCC.d  0.00765 -0.000199 2.71e-04
CSGS.d  0.00629 -0.004114 6.64e-04
EC.d   -0.00316  0.000591 5.70e-04
NYB.d  -0.00253  0.001502 9.38e-04
ALTR.d  0.02037  0.002152 3.46e-05
APH.d   0.01499 -0.000210 1.53e-03
CLS.d   0.00883 -0.003748 8.94e-04
NET.d   0.00625  0.002296 9.48e-04
SBUX.d -0.00164 -0.001303 6.84e-04
AYE.d  -0.00107 -0.002573 3.51e-04
```


Problem Set 2 - Cointegration Analysis of Yields
========================================================
source:presentation2.R
> This example is similar to Example 15.3(*SDAFE*) but uses different yield data. The data are in the mk.zero2 data set in the fEcofin package. There are 55 maturities and they are in the vector mk.maturity . We will use only the first 10 yields. Run


```r
library(MTS)
library(fUnitRoots)
library(fEcofin)
library(urca)
#mk.maturity=read.csv("8_mk.maturity.csv",header=T)
mk.maturity[2:11,] # maturity
#mk.zero2=read.csv("8_mk.zero2.csv",header=T,sep=";")#yield
zos = mk.zero2[,2:11]
zos.vecm = ca.jo(zos)
summary(zos.vecm)
#plotres(res)
```

Question 2
========================================================
source:presentation2.R
> Q2: What maturities are being used? Are they short-, medium-, or longterm, or a mixture of short- and long-term maturities?


```r
ma=mk.maturity[2:11,] # maturity
sort(ma) # sort it
```

```
 [1] 0.167 0.250 0.333 0.417 0.500 0.583 0.667 0.750 0.833 0.917
```

> A2: We choose maturities from mk.maturity(from row 2 to row 11), and they are 0.083, 0.167, 0.25, 0.333, 0.417, 0.5, 0.583, 0.667, 0.75, 0.833. They are **short-term** maturities.

Question 3
========================================================
source:presentation2.R
> - Q3: How many cointegration vectors were found? Use 1% level tests. 
> - A3: According to Johansen test, if the test statistic is greater than the critial value, we can reject the NULL hypothesis. So from the result below, we can rejects that r is equal to 0, less than or equal to 1, 2 and 3 but cannot reject r<=4, so we conclude that r = 4. In other words, 4 cointegration vectors were found using 1% level test.


```r
zos=mk.zero2[,2:11]
zos.vecm = ca.jo(zos)
summary(zos.vecm)
```

```

###################### 
# Johansen-Procedure # 
###################### 

Test type: maximal eigenvalue statistic (lambda max) , with linear trend 

Eigenvalues (lambda):
 [1] 0.7518 0.6630 0.5981 0.5498 0.4675 0.4263 0.3486 0.2474 0.1162 0.0218

Values of teststatistic and critical values of test:

          test 10pct  5pct 1pct
r <= 9 |  1.43   6.5  8.18 11.6
r <= 8 |  8.03  12.9 14.90 19.2
r <= 7 | 18.48  18.9 21.07 25.8
r <= 6 | 27.87  24.8 27.14 32.1
r <= 5 | 36.12  30.8 33.32 38.8
r <= 4 | 40.96  36.2 39.43 44.6
r <= 3 | 51.87  42.1 44.91 51.3
r <= 2 | 59.25  48.4 51.07 57.1
r <= 1 | 70.71  54.0 57.00 63.4
r = 0  | 90.57  59.0 62.42 68.6

Eigenvectors, normalised to first column:
(These are the cointegration relations)

          M.1.l2   M.2.l2  M.3.l2  M.4.l2 M.5.l2    M.6.l2   M.7.l2 M.8.l2
M.1.l2    1.0000   1.0000   1.000   1.000    1.0    1.0000   1.0000   1.00
M.2.l2   -4.7344   0.0688  -5.226  -2.685   -1.1   -3.7018  -0.0754  -2.81
M.3.l2   10.4480 -16.1326  14.436  -1.216  -14.9    4.7046 -16.5754  -2.03
M.4.l2  -11.4070  55.1551 -27.040  11.825   59.1   -2.0077  36.8685  17.14
M.5.l2    0.0861 -95.6143  37.192 -14.876  -92.7   22.2520 -19.4532 -22.91
M.6.l2   16.0781  92.4013 -38.015  -1.399   18.5  -93.9857 -14.0171   2.50
M.7.l2  -18.4791 -46.0499  23.988  15.680  130.5  156.3148   8.9956   8.83
M.8.l2    6.4942  11.3213  -4.385  -0.994 -160.4 -122.2777  13.2719  20.93
M.9.l2    1.9199  -3.1913  -2.660 -14.820   70.3   37.7554 -12.1269 -39.08
M.10.l2  -1.4045   1.0592   0.708   7.487  -10.2   -0.0391   2.0773  16.42
        M.9.l2 M.10.l2
M.1.l2     1.0   1.000
M.2.l2   -13.8  -0.215
M.3.l2    72.0 -16.884
M.4.l2   -99.1  39.882
M.5.l2   -29.7 -20.449
M.6.l2    46.0 -32.564
M.7.l2   254.0  42.682
M.8.l2  -427.4  -1.593
M.9.l2   258.6 -23.037
M.10.l2  -61.9  11.081

Weights W:
(This is the loading matrix)

       M.1.l2 M.2.l2 M.3.l2 M.4.l2  M.5.l2 M.6.l2 M.7.l2 M.8.l2    M.9.l2
M.1.d   11.41 0.0231   2.93 -6.142  1.1392 -2.056 -1.108 -0.196 -0.069935
M.2.d    6.09 0.3243   4.68 -1.920  0.3127 -1.732 -1.555 -0.718  0.000369
M.3.d    3.61 0.6442   5.03 -0.994  0.0122 -1.526 -1.644 -0.819  0.039527
M.4.d    3.51 0.9286   4.65 -1.621 -0.0172 -1.325 -1.482 -0.707  0.057404
M.5.d    4.02 1.0595   4.08 -2.191 -0.0374 -1.143 -1.286 -0.586  0.067680
M.6.d    4.49 0.9969   3.57 -2.271 -0.1221 -0.997 -1.151 -0.517  0.077499
M.7.d    4.85 0.8585   3.15 -2.108 -0.2187 -0.913 -1.060 -0.466  0.087693
M.8.d    5.01 0.7304   2.90 -1.942 -0.2757 -0.890 -0.978 -0.416  0.097029
M.9.d    4.97 0.6658   2.85 -1.912 -0.2788 -0.935 -0.883 -0.363  0.104393
M.10.d   4.85 0.6689   2.89 -2.035 -0.2280 -1.015 -0.765 -0.335  0.109826
       M.10.l2
M.1.d    0.104
M.2.d    0.152
M.3.d    0.168
M.4.d    0.170
M.5.d    0.178
M.6.d    0.190
M.7.d    0.202
M.8.d    0.212
M.9.d    0.217
M.10.d   0.218
```

```r
#plotres(res)
```

Question 4
========================================================
source:presentation2.R 24
> - Q4: Which trading strategy do you recommend?
> - A4: Pairs/Basket Trading.

> The VECM model for the VAR(1) process is:
> $$\Delta Y_t = \alpha \beta^{T} Y_{t-1} + \varepsilon $$
> We know all the time series components in our data $Y_t$ is I(1), i.e. $Y_t$ is I(1). Therefore, $\Delta Y_t$ is I(0). The right hand side $\alpha \beta^{T} Y_{t-1}$ should also be I(0). $\alpha$ is just a loading matrix specifying the speed of mean reversion, so it follows $\beta^{T} Y_{t-1}$ is also I(0), i.e. $\beta^{T} Y_{t}=I(0)$.

> In our case, we call $\beta^{T} Y_{t}$ **compounded yield** as it is constructed from different yields. So we can build a strategy to trade the **compounded yield** according to the error correction model.

Question 4 (continued 1)
========================================================
source:presentation2.R 24

> Strategy Implementation
> - 1. Create a portfolio using the cointegration vector as portfolio weights. This portfolio can use any IR products such as bond, swap, futures.([CMEGroup IR products](http://www.cmegroup.com/trading/interest-rates/))
> - 2. Select a range variable as a signal indicator, say one standard deviation. We allow the compounded yield fluctuate inside the range around it moving average. When the compounded yield is outside the range, a BUY/SELL signal is triggered.


Question 4 (continued 2)
========================================================
source:presentation2.R 24

Get compounded yield $cyield = \beta Y_t$ and test stationarity


```r
beta=zos.vecm@V # Beta after normalization
mbeta=beta[,1:4] # 10x4# meaningful cointegration vector
cyield=as.matrix(zos) %*% mbeta # (AB)' = (B'*A') - cyield should be a stationary time series according to VECM
# stationarity check with Augumented Dickey-Fuller test
adfTest(cyield[,1])@test$p.value
```

```
     
0.01 
```

```r
adfTest(cyield[,2])@test$p.value
```

```
     
0.01 
```

```r
adfTest(cyield[,3])@test$p.value
```

```
       
0.0368 
```

```r
adfTest(cyield[,4])@test$p.value
```

```
     
0.01 
```



Question 4 (continued 3)
========================================================
source:presentation2.R 24


```r
# draw cyield and range lines
plot(cyield[,1],type="o",col='black',ylim=c(-0.005,0.005))
lines(cyield[,2],type="o",col='red',ylim=c(-0.005,0.005))
lines(cyield[,3],type="o",col='green',ylim=c(-0.005,0.005))
lines(cyield[,4],type="l",col='blue',ylim=c(-0.005,0.005))
abline(h = c(-0.0005,0.0005, -0.001,0.001, -0.002,0.002)) # range lines
```

<img src="presentation for Rlab 9-figure/unnamed-chunk-8-1.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" style="display: block; margin: auto;" />

Question 4 (continued 4)
========================================================
source:presentation2.R 24
> Optimization and Basket Selection

> - We already know $\beta^{T} Y_{t}=I(0)$ and not of full rank, we can use Gaussian elimination to transform $\beta^{T}$ to something like a triangular matrix. If there are r cointegration relationships in a n-variable system, there exists a cointegrating vector for each subset of (n-r+1) variables. In our case, use 7 yields instead of 10 to reduce trading cost.

> - Which basket we should select depends on trading costs/fee, bid/ask spread.
