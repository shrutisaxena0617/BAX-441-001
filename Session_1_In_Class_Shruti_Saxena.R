####### Exercise 1 ########
# A tech company has to complete a project no later than three months from now or there will be
# significant cost overruns. The manager of the company believes that there are four possible
# values for the random variable 𝑋, the number of months from now it will take to complete this
# project: 2, 2.5, 3, and 3.5. The manager currently thinks that the probabilities of these four
# possibilities are in the ratio 1 to 2 to 4 to 2. Find the probability distribution of 𝑋.
# a. What is the expected completion time (in months) of this project from now?
# b. How much variability (in months) exists around the expected value?


x <- c(2,2.5,3,3.5)
p <- c(1/9,2/9,4/9,2/9)
names(p) <- x

require(ggplot2)
ggplot(data = NULL) +aes(x=x, y=p) + geom_smooth()

# discrete probability distribution

E_x <- crossprod(x,p) #2.89 expected mean
Mean_vector <- rep(E_x,length(x))
Mean_vector
var <- (x - Mean_vector)^2 %*% p
var_1 <- crossprod((x - Mean_vector)^2,p)
var_1
var # 0.2098 variance in terms of months



####### Exercise 2 ########


# An investor has $100,000 to invest in the stock market. She is interested in developing a stock
# portfolio made up of stocks on the New York Exchange (NYSE), the Toronto Stock Exchange
# (TSX), and the NASDAQ. The stocks she has chosen to analyze are Home Depot (HD) and Nike
# (NKE) on the NYSE, Canadian National Railway (CNR) on the TSX, and Expedia (EXPE) on
# the NASDAQ. However, she does not know how much to invest in each one. She would like to
# maximize her return while minimizing her risk. She has compiled the monthly returns for all four
# stocks during a 60-month period (January 2011 to December 2015). The data are available on the
# CSV file Exercise2. After some consideration, she has narrowed her choices down to the
# following three. What should she do?
# a. Invest $25,000 in each stock
# b. Home Depot:$10,000, Nike:$20,000, Canadian National Railway:$30,000, Expedia:$40,000
# c. Home Depot: $30,000, Nike: $30,000, Canadian National Railway: $10,000, Expedia:
#   $30,000



exer2 <- read.csv("Exercise2.csv", sep = ",")
head(exer2)
nrow(exer2) #60
mean_vec <- c(mean(exer2$HD), mean(exer2$NKE), mean(exer2$CNR), mean(exer2$EXPE))
names(mean_vec) <- c("HD", "NKE", "CNR", "EXPE")
mean_vec
sigma_matrix <- cov(exer2)
sigma_matrix

#Option a -> 25000 in each stock, w1 = w2 = w3 = w4 = 0.25
w_a <- c(0.25,0.25,0.25,0.25)
names(w_a) <- c("w1", "w2", "w3", "w4")
ER_a <- crossprod(w_a, mean_vec)
ER_a #0.02105687
Var_a <- t(w_a) %*% sigma_matrix %*% w_a
Var_a #0.001758063
sd_a <- sqrt(Var_a)
sd_a
final_result_a <- c(ER_a,Var_a,sd_a)

#Option b -> HD = $10000, NKE = $20000, CNR = $30000, EXPE = $40000, w1 = 0.1, w2 = 0.2, w3 = 0.3, w4 = 0.4
w_b <- c(0.1,0.2,0.3,0.4)
names(w_b) <- c("w1", "w2", "w3", "w4")
ER_b <- crossprod(w_b, mean_vec)
ER_b #0.02069537
Var_b <- t(w_b) %*% sigma_matrix %*% w_b
Var_b #0.002692421
sd_b <- sqrt(Var_b)
sd_b
final_result_b <- c(ER_b,Var_b,sd_b)

#Option c -> HD = $30000, NKE = $30000, CNR = $10000, EXPE = $30000, w1 = 0.3, w2 = 0.3, w3 = 0.1, w4 = 0.3
w_c <- c(0.3,0.3,0.1,0.3)
names(w_c) <- c("w1", "w2", "w3", "w4")
ER_c <- crossprod(w_c, mean_vec)
ER_c #0.02208698
Var_c <- t(w_c) %*% sigma_matrix %*% w_c
Var_c #0.002048806
sd_c <- sqrt(Var_c)
sd_c
final_result_c <- c(ER_c,Var_c,sd_c)


final_result <- as.matrix(rbind(final_result_a, final_result_b, final_result_c))
row.names(final_result) <- c("A", "B", "C")
colnames(final_result) <- c("Expected Return", "Variance", "Standard Deviation")

final_result 
# Plan A is a safe option since it has second largest Expected Return (0.02106) and 
# lowest variance/ risk (0.00176). However, plan C could also be a choice since its Expected Return is 
# maximum (0.22087) and second highest variance/ risk (0.00205)



# Exercise 3

# PaperStock Company runs a manufacturing facility that produces a paper product. The fiber
# content of this product is supposed to be 20 pounds per 1000 square feet. (This is typical for the
# type of paper used in grocery bags, for example.) Because of random variations in the inputs to
# the process, however, the fiber content of a typical 1000-square-foot roll varies according to a
# 𝑁(𝜇, 𝜎) distribution. The mean fiber content (𝜇) can be controlled—that is, it can be set to any
# desired level by adjusting an instrument on the machine. The variability in fiber content, as
# measured by the standard deviation 𝜎, is 0.10 pound when the process is “good,” but it
# sometimes increases to 0.15 pound when the machine goes “bad.” A given roll of this product
# must be rejected if its actual fiber content is less than 19.8 pounds or greater than 20.3 pounds.
# Calculate the probability that a given roll is rejected, for a setting of 𝜇 = 20, when the machine
# is “good” and when it is “bad.” Run a sensitivity analysis to see how sensitive the rejection
# probability is to the mean and standard deviation.

data_good = rnorm(1000, mean = 20, sd = 0.1)

plot(density(data_good), main = "Normal Distribution for Good Machine") + 
  curve(dnorm(x, mean = 20, sd = 0.1), add = TRUE, col = 3, abline(h=NULL, v=c(19.8,19.9,20.1,20.2), lty = 3, col = "gray60"))

data_bad = rnorm(1000, mean = 20, sd = 0.15)

plot(density(data_bad), main = "Normal Distribution for Bad Machine") + 
  curve(dnorm(x, mean = 20, sd = 0.15), add = TRUE, col = 3, abline(h=NULL, v=c(19.85,20.15,20.3), lty = 3, col = "gray60"))

# Probability of rejection for good machine = 0.0241
# Probability of rejection for bad machine = 0.1146









