# Compute Winner's Curse Correction

Zachary McCaw <br>
Updated: 20-09-15



### Description

This package calculates the winner's curse correction using the model proposed by [Turley *et al* (2018)](https://pubmed.ncbi.nlm.nih.gov/29292387/), implemented via the Expectation-Maximization algorithm. Also see [MGMM](https://github.com/zrmacc/MGMM) for fitting Gaussian Mixture Models more generally. 

## Installation


```r
devtools::install_github(repo = 'zrmacc/WinCurse')
```

## Model
This package fits the following hierarchical model:
$$
Z_{i} \sim \text{Bern}(\pi) \\
\theta_{i}|(Z_{i}=1) \sim \delta_{0} \\
\theta_{i}|(Z_{i}=0) \sim N(0, \tau^{2}) \\
\hat{\theta}_{i}|\theta_{i} \sim N(\theta_{i}, \sigma_{i}^{2})
$$

where $\pi$ is the probability of the null component, $\delta_{0}$ is a point-mass on zero, $\tau^{2}$ is a variance component for the non-null parameters, $\theta_{i}$ is the true underlying parameter, $\hat{\theta}_{i}$ is the corresponding estimate, and $\sigma_{i}^{2}$ is the sampling variance, assumed known. 

## Examples

### Data

Example may be loaded via:


```r
library(WinCurse)
data(wc_data)
head(wc_data)
```

```
##   non_null        theta         se
## 1        0  0.266490285 0.09712859
## 2        1 -0.013344779 0.10425721
## 3        0 -0.199862356 0.10540926
## 4        0  0.125285959 0.09901475
## 5        1  0.072490809 0.10000000
## 6        0 -0.007482795 0.09950372
```

Here: 

* `non_null` is the generative component, 1 if non-null, 0 if null. 
* `theta` is the estimated parameter. 
* `se` is the standard error of the estimated parameter. 

The true $\pi = 0.75$ and the true $\tau^{2} = 0.05$. 

### Estimation

To fit the winner's curse model: 

```r
set.seed(100)
fit <- fit.WinCurse(
  theta = wc_data$theta,
  se = wc_data$se,
  pi = 0.5,
  tau2 = 1,
  eps = 1e-12
)
show(fit)
```

```
## Estimated null proportion:
## [1] 0.766
## 
## 
## Estimated non-null variance component:
## [1] 0.048
```

#### Outputs

The output of `fit.WinCurse` is an object of class `winCurse` with these slots.

* `@Assignments` containing the component assignments and assignment entropy. Higher entropy means the assignment is less certain. 


```r
head(fit@Assignments)
```

```
##   non_null    entroy
## 1        1 0.8230338
## 2        0 0.5203778
## 3        0 0.9455816
## 4        0 0.7154702
## 5        0 0.5747059
## 6        0 0.5075906
```

* `@Estimates` containing the parameter estimates:


```r
fit@Estimates
```

```
## $pi
## [1] 0.7656391
## 
## $tau2
## [1] 0.04803448
```

* `@Expectations` containing the posterior expectation of $\theta_{i}$ given $\hat{\theta}_{i}$, which is shrunk towards zero. 


```r
head(fit@Expectations)
```

```
## [1]  0.165374887 -0.001272114 -0.059008240  0.020476310  0.008183376
## [6] -0.000698325
```
* `@Responsibilities` containing the posterior membership probabilities to the null and non-null components. 


```r
head(fit@Responsibilities)
```

```
##        null  non_null
## 1 0.2575546 0.7424454
## 2 0.8831021 0.1168979
## 3 0.6364610 0.3635390
## 4 0.8032057 0.1967943
## 5 0.8636100 0.1363900
## 6 0.8874397 0.1125603
```
