# R Packages: sinib
Sum of Independent Non-Identical Binomial Random Variables

## Description
Density, distribution function, quantile function and random generation for the sum of independent non-identical	binomial distribution with parameters `size` and `prob`.
 
## Installation
In R console, type 
```
install.packages('sinib')
library('sinib')
```

## Documentation
The documentation for the four main functions `dsinib`, `psinib`, `qsinib`, and `rsinib` can be assessed in R by typing
```
?dsinib
?psinib
?qsinib
?rsinib
```

## Example
Suppose `S=X1+X2+X3` where `X1~binom(12,0.074)`, `X2~binom(14,0.039)` and `X3~binom(4,0.095)`. If you would like to calculate the probability `P(S=3)` and `P(S=20)`, you would do the following.

```
size=as.integer(c(12, 14, 4))
prob=c(0.074, 0.039, 0.095)
x=c(3,20)
dsinib(x,size,prob)
# [1] 1.675016e-01 3.167624e-18
```