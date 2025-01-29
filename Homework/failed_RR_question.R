## Problem 1

Remake this data with intercept 0! Do it with known regression coefficients and correlation matrix for the predictors.

```{r, eval = FALSE, echo = FALSE}
library(MASS)
library(tidyverse)
set.seed(51)
#create some data for this problem
corr_mat <- lqmm::make.positive.definite(matrix(c(1, 1, 0.2, 0.5, 0.1, 
                                                  1, 1, 0.4, 0.8, 0.3,
                                                  0.2, 0.4, 1, -0.3, 0.,
                                                  0.5, 0.8, -0.3, 1, 0.4,
                                                  0.1, 0.3, 0, 0.4, 1), nrow = 5, ncol = 5))
n <- 100
sim_X_data <- mvrnorm(n, mu = c(3, 0, 2, 100, 1000), Sigma = corr_mat)
#center the columns
sim_X_data <- apply(X = sim_X_data, MARGIN = 2, FUN = function(x){x-mean(x)})
#obtain responses
beta <- c(5, 2, -10, -1/20, 0)
y <- sim_X_data %*% beta + rnorm(n, sd = 3)

dimnames(sim_X_data)[[2]] <- c("x1", "x2", "x3", "x4", "x5")
sim_X_data |> 
  as_tibble() |>
  mutate(y = y[,1] - mean(y[,1])) |>
  select(y, everything()) |>
  write_csv(file = "RR_data.csv")
```

An example data set was simulated and is available at <https://www4.stat.ncsu.edu/online/datasets/RR_data.csv>. Read this data into your software. In `R` we can use

```{r}
library(readr)
sim_data <- read_csv("https://www4.stat.ncsu.edu/online/datasets/RR_data.csv")
```

Consider a multiple linear regression model with the main effects for `x1`, `x2`, `x3`, and `x4` only. We want to fit a ridge regression model manually for this model (that is, using the matrix solution given in the notes). Recall, in R we can coerce a data frame to a matrix using `as.matrix()`, the matrix multiplication operator is `%*%`, we can use `solve()` to find the inverse of a matrix, and `t()` gives us the transpose of a vector or matrix. Also, `identity(4)` gives an identity matrix with four rows and columns.

a. Find the estimated ridge regression coefficients when $\lambda = 0$ and find the value of the sum of squared $\hat{\beta}_j$ values. Repeat with $\lambda = 10$. Repeat with $\lambda = 100$. Repeat with $\lambda = 1000$.

```{r}
#set up the X matrix
X <- sim_data[, 2:6] |>
  as.matrix()
#create I
I <- identity(5)
#define the respose vector
y <- sim_data$y -mean(sim_data$y)

#define lambda
lambda <- 0
#find the solution
solve(t(X)%*%X + lambda*I)%*%t(X)%*%y
sum((solve(t(X)%*%X + lambda*I)%*%t(X)%*%y)^2)
```

```{r}
#define lambda
lambda <- 10
#find the solution
solve(t(X)%*%X + lambda*I)%*%t(X)%*%y
sum((solve(t(X)%*%X + lambda*I)%*%t(X)%*%y)^2)
```

```{r}
#define lambda
lambda <- 100
#find the solution
solve(t(X)%*%X + lambda*I)%*%t(X)%*%y
sum((solve(t(X)%*%X + lambda*I)%*%t(X)%*%y)^2)
```

```{r}
#define lambda
lambda <- 1000
#find the solution
solve(t(X)%*%X + lambda*I)%*%t(X)%*%y
sum((solve(t(X)%*%X + lambda*I)%*%t(X)%*%y)^2)
```

b. Do the coefficients always shrink towards zero for larger and larger values of $\lambda$? 
  
  c. Find the correlation matrix for the predictors. 

```{r}
cor(X)
```

c. Now standardize the predictor matrix first (that is, center and scale each column using that column's mean and standard deviation) and find the ridge regression estimates using the same four $\lambda$ values.

```{r}
std_X <- apply(X, MARGIN = 2, FUN = function(x){(x-mean(x))/sd(x)})
```

```{r}
#define lambda
lambda <- 0
#find the solution
solve(t(std_X)%*%std_X + lambda*I)%*%t(std_X)%*%y
sum((solve(t(std_X)%*%std_X + lambda*I)%*%t(std_X)%*%y)^2)
```

```{r}
#define lambda
lambda <- 10
#find the solution
solve(t(std_X)%*%std_X + lambda*I)%*%t(std_X)%*%y
sum((solve(t(std_X)%*%std_X + lambda*I)%*%t(std_X)%*%y)^2)
```

```{r}
#define lambda
lambda <- 100
#find the solution
solve(t(std_X)%*%std_X + lambda*I)%*%t(std_X)%*%y
sum((solve(t(std_X)%*%std_X + lambda*I)%*%t(std_X)%*%y)^2)
```

```{r}
#define lambda
lambda <- 1000
#find the solution
solve(t(std_X)%*%std_X + lambda*I)%*%t(std_X)%*%y
sum((solve(t(std_X)%*%std_X + lambda*I)%*%t(std_X)%*%y)^2)
```

d. Add Q here about takeaways?