# Let's first import the data sourced from Wikipedia. Here I use use GitHub for the sake of simplicity.

# We need ggplot2 and dplyr for the purposes of this estimation, so if you don't have those, 

seats_vs_population <- read.csv("https://raw.githubusercontent.com/adriannikolov/predictingparliamentseats/master/Seats_vs_population.csv", header = TRUE, sep = ",")

# Several exploratory plots can give us a basic feel for the data. Ggplot2 will be used for all plots here.
# First for the seats in parliament.

library(ggplot2)
ggplot(seats_vs_population, aes(x = CombinedSeats)) +
  geom_histogram(binwidth = 20, col = "red", fill = "darkgreen") +
  labs(title = "Histogram of seats in Parliament") +
  labs(x = "Combined Seats", y = "Frequency")

# Then for the population

ggplot(seats_vs_population, aes(x = Population)) +
  geom_histogram(binwidth = 10000000, col = "red", fill = "darkgreen") +
  labs(title = "Histogram of population") +
  labs(x = "Population", y = "Frequency")

# Finally, an X-Y scatterplot for comparison

ggplot(seats_vs_population, aes(x = Population, y = CombinedSeats)) +
  geom_point() +
  labs(title = "Seats in Parliament vs Population") +
  labs(x = "Population", y = "Combined Seats on Parliament")

# The exploratory plots show that it is necessary to restructure the data. Due to the large variance 
# and presence of outliers (China, India) in the population scale, it would be worth it to plot it on a
# logarithmic one. We take the decimal log and attach it to the data

logPopulation <- log10(seats_vs_population$Population)
seats_vs_population <- cbind(seats_vs_population, logPopulation)

# Lets check the new data structure and remove the now obsolete logPopulation item.

head(seats_vs_population, n = 10)
rm(logPopulation)

# Lets redo the plots, but with the logged values.

ggplot(seats_vs_population, aes(x = logPopulation)) +
  geom_histogram(binwidth = 0.1, col = "red", fill = "darkgreen") +
  labs(title = "Histogram of logged population") +
  labs(x = " Logged Population", y = "Frequency")

ggplot(seats_vs_population, aes(x = logPopulation, y = CombinedSeats)) +
  geom_point() +
  labs(title = "Seats in Parliament vs Logged Population") +
  labs(x = "Logged Population", y = "Combined Seats on Parliament")

# From here we have two options: 1) attempt to fit a linear model, or
# 2) attempt to fit aa curved exponential model.
# The plot gives preference to the second option, but we will do both.
# First, the linear model.

linear_model <- lm(seats_vs_population$CombinedSeats ~ seats_vs_population$logPopulation)
summary(linear_model)

# This model is quite poor - an r2 fit of only 0.366, despite the high signifcance values.
# The negative intercept also points to absurd predictiions at very low values of the logged population.
# Dropping some of the outliers with extremely small of large parliaments could help improve it.

library(dplyr)
seats_vs_population2 <- filter(seats_vs_population, seats_vs_population$CombinedSeats >= 20 
                               & seats_vs_population$CombinedSeats <= 1000)

# We lost only 7 cases after we deined a narrower assembly size, between 20 and 1000 seats.
# Lets make a new model.

linear_model2 <- lm(seats_vs_population2$CombinedSeats ~ seats_vs_population2$logPopulation)
summary(linear_model2)

# Better. r2 of 53 explains more then half of the variance. Lets also plot the reult.

ggplot(seats_vs_population2, aes(x = logPopulation, y = CombinedSeats)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Seats in Parliament vs Logged Population") +
  labs(x = "Logged Population", y = "Combined Seats on Parliament")

# As cen be seen from the plot, however, the model still predicts absurdities for small
# populations and underpredicts their values. It is impossible, of course, that the
# assembly size be smaller than 0.

# Next lets try a nonlinear model, without the outliers

nonlinear_model <- nls(CombinedSeats ~ exp(a + b * logPopulation), data = seats_vs_population2, start = list(a = 1, b = 1))
summary(nonlinear_model)

# The results are significant. Lets plot them to check for consistancy.

ggplot(seats_vs_population2, aes(x = logPopulation, y = CombinedSeats)) +
  geom_point() +
  geom_smooth(method = "nls", formula = "y ~ exp(a + b * x) ", se = FALSE, method.args = list(start=c(a = 1, b = 1))) +
  labs(title = "Seats in Parliament vs Logged Population") +
  labs(x = "Logged Population", y = "Combined Seats on Parliament")

# The few outliers aside, the model looks pretty good. Lets see how it fares at predicting assembly sizes.
# Lets first take the logged populations of the first few countries.

logpops <- seats_vs_population2[, 4]

# ANd attempt to predict their assembly sizes and compare them to the actual sizes

predicted <- predict(nonlinear_model, newdata = logpops)
mean(seats_vs_population2$CombinedSeats) - mean(predicted)
median(seats_vs_population2$CombinedSeats) - median(predicted)

# As can be seen, on average the model tends to overpredict in most cases, especially when it comes to the most
# typical contries, judging from the median. In the second part of this attempt we wil attempt to use machine
# learning on the same set to minimize this issue.

# Since the exponential fit proved to be better then the log-linear model, we may as well attempt to use
# logarythms on both sides, as this effectivly "straightens" the curve.

logseats <- log10(seats_vs_population2$CombinedSeats)

seats_vs_population2 <- cbind(seats_vs_population2, logseats)

linear_model3 <- lm(seats_vs_population2$logseats ~ seats_vs_population2$logPopulation)

summary(linear_model3)


# The new linera model seems significantly better then the previous ones - r2 of 0.76, small error and
# high sigificance. Lets plot it:

ggplot(seats_vs_population2, aes(x = logPopulation, y = logseats)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Logged Seats in Parliament vs Logged Population") +
  labs(x = "Logged Population", y = "Logged Combined Seats on Parliament")

#Lets see how close its predictions are to the actual number of MP's.

predicted2 <- predict(linear_model3, interval = "confidence")

# As these are logarythms, we first need to transform them into number again.

predicted2 <- 10 ^ predicted2

# And then test with the real vaues

mean(seats_vs_population2$CombinedSeats) - mean(predicted2)
median(seats_vs_population2$CombinedSeats) - median(predicted2)

# The reulsts are quite similar to the nls - the difference in medians is much smaller,
# which makes this model better for mid-sized countries. However, the difference in means
# is larger, so overall the predictions are somewhat less acurate.

# A third possible approach is to use machine learning. As here we're dealing with a regression 
# problem, attmepting a Multivariate Adaptive Regression Splines would be a good start, in an
# attempt to build upon the single nls model.

library(earth)

# Lets split the data into a training and testing set, at a ratio 3/4 :1/4
train <- seats_vs_population3[1:135 ,]
test <- seats_vs_population3[136:180 ,]

# And then build a model based on the training set
set.seed(1234)
seats_vs_population2 <- seats_vs_population2[, 1:4]
mars_model <- earth(CombinedSeats ~., logPopulation, data = seats_vs_population2)
summary(mars_model)
evimp(mars_model)

# And check how well it fits data new to it. 
predicted_mars <- predict(mars_model, seats_vs_population2)
comparison1 <- as.data.frame(cbind(seats_vs_population2$CombinedSeats, predicted_mars))
colnames(comparison1) <- c("Seats", "Predicted")
ggplot(comparison1, aes(Seats, Predicted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Multivariate Adaptive Regression Splines Test")



# As we cannot directly compare the fit statistics of the MARS model with those of the 
# previous ones, we can compre the root mean squared error instead for the three models close to
# the goal, in order.

predicted3 <- predict(mars_model, seats_vs_population2)

rmse <- sqrt(mean((seats_vs_population2$CombinedSeats - predicted)^2))
rmse2 <- sqrt(mean((seats_vs_population2$CombinedSeats - predicted2)^2))
rmse3 <- sqrt(mean((seats_vs_population2$CombinedSeats - predicted3)^2))
rmse_m <- as.matrix(cbind(rmse, rmse2, rmse3))
rmse_m
rm(rmse, rmse2, rmse3)

# The RMSE metric shows that the nls model is sligtly better then the log-log one, but the MARS
# produces about twice as smaller error. The differences in the means and medians is also much smaller


mean(seats_vs_population2$CombinedSeats) - mean(predicted3)
median(seats_vs_population2$CombinedSeats) - median(predicted3)

# The MARS model appears to be very, very good. Still, it is worth exploring one more alternative;
# sometimes support vector machine can produce better results than OLS regressons as well as MARS. SO,

library(e1071)

svm_model <- svm(CombinedSeats ~ logPopulation, seats_vs_population2)
summary(svm_model)
predicted4<- predict(svm_model, seats_vs_population2)
rmse4 <-sqrt(mean((seats_vs_population2$CombinedSeats - predicted4)^2))
rmse_m <- cbind(rmse_m, rmse4)
rm(rmse4)

# Sofar, the SVM model seems to work a little better then the linear and nls one, but is still
# no match for the MARS. However, we can try to tune it a little bit:

tunedsvm <- tune(svm, CombinedSeats ~ logPopulation, data = seats_vs_population2,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)

tunedsvm
plot(tunedsvm)

# The plot shows in dark blue the epsolon value which works best. We can tune again, this time with 
# a smaller range

tunedsvm2 <- tune(svm, CombinedSeats ~ logPopulation, data = seats_vs_population2,
                 ranges = list(epsilon = seq(0, 0.4, 0.01), cost = 2^(2:9))
)

tunedsvm2
plot(tunedsvm2)

# It seems that an epsilon of 0.22 and cost around 100 will produce the best fitting model. Lets try to
# predict with these values

tunedSVMModel <- tunedsvm2$best.model
predicted5<- predict(tunedSVMModel, seats_vs_population2)
rmse5 <-sqrt(mean((seats_vs_population2$CombinedSeats - predicted5)^2))
rmse5
rmse_m <- cbind(rmse_m, rmse5)
rm(rmse5)

# So far, it appears that the MARS model predicts best. while the results from the other approaches
# are pretty comparabe. Finally, we arrived at a model that predicts the number of seats in the assembly
# pretty well.

