# 1)

x <- c(68, 42, 51, 57, 56, 80, 45, 39, 36, 79)

x_bar <- mean(x)

std <- sd(x)

n <- length(x)

crit <- qt(0.05/2, n-1, lower.tail = FALSE) * std / sqrt(n)

c(x_bar - crit , x_bar + crit)

# 2)

c(2.364 - qt(0.1/2, 14,lower.tail = FALSE)* (0.9 / sqrt(15)),
    2.364 + qt(0.1/2, 14,lower.tail = FALSE)* (0.9 / sqrt(15)))

# 4)

z_o <- (9900 - 10000) / (120 / sqrt(30))

z_crit <- qnorm(0.05)

# 5)

t_o <- (22-25) / (1.5 / sqrt(10))

t_crit <- qt(0.05, 10-1)

p_t <- pt(t_o , 10-1)

p_t < 0.05

# 9)

mat <- matrix(c(200,250,150,300,50,50),byrow = TRUE, ncol = 2)

mat

u <- rowSums(mat) / sum(mat)

u

v <- colSums(mat) / sum(mat)

v

dim(u) <- c(3, 1)

u

dim(v) <- c(1,2)

v

u %*% v

expected <- (u %*% v) * sum(mat) 

expected

degree_of_freedom <- ( ncol(expected) - 1 ) * ( nrow(expected) -1 )

test_statistic <- sum((mat - expected)^2 / expected)

test_statistic

crit <- qchisq(0.05, degree_of_freedom, lower.tail = FALSE)

crit

chisq.test(mat)


# 10)

x1 <- c(86, 79, 81, 70,84)
x2 <- c(90,76,88, 82, 89)
x3 <- c(82, 68, 73, 71, 81)

x <- c(x1, x2, x3)

sst <- sum((x - mean(x))^2)

sst

m <- 3
n <- length(x1)

sst_df <- m * n - 1

ssw <- sum((x1 - mean(x1))^2) + sum((x2 - mean(x2))^2) + sum((x3 - mean(x3))^2)

ssw

ssw_df <- m * (n - 1)

ssb <- sst - ssw

ssb

ssb_df <- sst_df - ssw_df

ssb_df

f_statistic <- (ssb/ssb_df)/(ssw/ssw_df)

f_statistic

f_crit <- qf(0.05, ssb_df, ssw_df, lower.tail = FALSE)

f_crit > f_statistic

p_value_f <- pf(f_statistic, ssb_df, ssw_df, lower.tail = FALSE)

p_value_f

data <- data.frame(scores = c(86, 79,81,70,84,90,76,88,82,89,82,68,
                                 73,71,81),
      method = factor(rep(c("A1", "A2","A3"), c(5,5,5))))

model= aov(scores ~ method,data=data)

model

summary(model)

