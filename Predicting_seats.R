# Let's first import the data sourced from Wikipedia. Here I use use GitHub for the sake of simplicity.

# We need ggplot2 and dplyr for the purposes of this estimation, so if you don't have those, 
install.packages("dplyr")
install.packages("ggplot2")

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

