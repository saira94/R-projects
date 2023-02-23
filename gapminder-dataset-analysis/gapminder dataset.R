install.packages("gapminder")
library(gapminder)
data("gapminder")

summary(gapminder) #summary of each of the variabe in dataset
mean(gapminder$gdpPercap) #mean of gdp per capita

attach(gapminder)
median(pop) #median of the population
hist(lifeExp)
hist(log(pop))

#lifeexpectancy data disaggregated by continent
boxplot(lifeExp ~ continent)
#scatter plot y axis = dependent variable (life expectancy) x axis = independent variable (gdppercapita)
plot(lifeExp ~ log(gdpPercap))

install.packages("dplyr")
library(dplyr)

#calculte the average life expectancy of Ireland and South Africa
gapminder %>%
  select(country, lifeExp) %>%
  filter(country == "South Africa" |
           country == "Ireland") %>%
  group_by(country) %>%
  summarise(Average_life = mean(lifeExp))

#output shows there is a 20 year life expectancy difference between this two countris
#is this different statistically significant? perform t-test
#t-test : null hypothesis is that there is no difference
#null hypothesis -> t-test -> p-value ( p < 0.05)
#p value is the probability that the null hypothesis is true
#the t-test also gives us a 95% CI confidence interval, that's the range i

df1 <- gapminder %>%
  select(country, lifeExp) %>%
  filter(country == "South Africa" |
           country == "Ireland")
t.test(data = df1, lifeExp ~ country)  

#output t = 10.067, df = 19.109, p-value = 4.466e-09, since the p value is close to 0, we can reject the null hypothesis

#lets do some cool visualisation

install.packages("ggplot2")
library(ggplot2)

gapminder %>%
  filter(gdpPercap < 50000) %>%
  ggplot(aes(x = log(gdpPercap), y = lifeExp, col = year, size = pop)) +
  geom_point(alpha = 0.3)+
  geom_smooth(method = lm)+
  facet_wrap(~continent)

#linear regression
lm(lifeExp ~ gdpPercap)
summary(lm(lifeExp ~ gdpPercap + pop)) 
