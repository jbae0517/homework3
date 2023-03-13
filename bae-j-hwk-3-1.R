#Jennifer Bae
#Homework 3-1


knitr::opts_chunk$set(echo = TRUE)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales,
               kableExtra, tinytex, modelsummary, fixest, AER)


#reading the data and saving as variables
tax_data <- readRDS("/Users/jenniferbae/Downloads/ECON 470 R Coding/homework3/data/output/TaxBurden_Data.rds")

# 1. Present a bar graph showing the proportion of states with a change in their cigarette tax in each year from 1970 to 1985.

data1 <- tax_data%>%
  filter(Year %in% 1970:1985) %>%
  group_by(state) %>%
  summarize(difference = diff(tax_state), year=c(1971:1985)) %>%
  mutate(change = difference!= 0) %>%
  group_by(year) %>%
  summarize(change_ct = sum(change), change_pct = change_ct/length(state)*100)

q1.graph <- data1 %>%
  ggplot(aes(x = year, y= change_pct)) + geom_col(fill = "lightblue") + 
  scale_x_discrete(breaks = seq(1970,1985,1)) +
  labs(x = "Year", y = "Proportion of the States (%)", Title = "Proportion of States with a Change in Their Cigarette Tax in Each Year from 1970 to 2019")+
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top")

q1.graph


# 2. Plot on a single graph the average tax (in 2012 dollars) on cigarettes and the average price of a pack of cigarettes from 1970 to 2018.

data2 <- tax_data%>%
  group_by(Year) %>%
  summarize(avg_tax = mean(tax_cpi), avg_price = mean(price_cpi)) %>%
  pivot_longer(!Year, names_to = "Average", values_to = "Value")

q2.graph <- data2 %>%
  ggplot(aes(x = Year, y = Value, color = Average)) + geom_point() + geom_line() +
  scale_color_manual(values = c("lightblue", "darkblue"), labels = c("Average Price", "Average Tax")) +
  scale_x_continuous(breaks = seq(1970,2020,5)) +
  labs(x = "Year", y = "Average Tax and Price (in 2012 dollars)", Title = "Average Tax in 2012 dollars on Cigarettes and Average Price of a Pack of Cigarettes from 1970 to 2018")+
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top")

q2.graph

# 3. Identify the 5 states with the highest increases in cigarette prices (in dollars) over the time period. Plot the average number of packs sold per capita for those states from 1970 to 2018.


cig_price <- tax_data %>%
  filter(Year == 1970 | Year == 2018) %>%
  group_by(state) %>%
  summarise(diff_tax_dollar = diff(tax_dollar), diff_price_cpi = diff(price_cpi))

high5states <- cig_price %>%
  select(-diff_tax_dollar) %>%
  arrange(desc(diff_price_cpi)) %>%
  head(5)
colnames(high5states) <- c("State", "Increases in Cigarette Prices")

data3 <- tax_data %>%
  filter(state %in% high5states$State) %>%
  group_by(Year, state) %>%
  summarise(avg_sales_per_cap = mean(sales_per_capita)) %>%
  mutate(state = factor(state, levels = high5states$State))

q3.graph <- data3 %>%
  ggplot(aes(x = Year, y = avg_sales_per_cap, color = state)) + 
  geom_point(alpha = 0.75) + geom_line(alpha = 0.75) +
  scale_color_manual(values = c("aquamarine4", "lightskyblue1", "slateblue", "cornflowerblue", "darkblue"), labels = levels(factor(data3$state))) +
  scale_x_continuous(breaks = seq(1970,2020,5)) +
  labs(x = "Year", y = "Average Pack Sales per Capita", Title = "Average Pack Sales per Capita in 5 States with the Highest Increases in Cigarette Prices")+
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top")


q3.graph


# 4. Identify the 5 states with the lowest increases in cigarette prices over the time period. Plot the average number of packs sold per capita for those states from 1970 to 2018.

low5states <- cig_price %>%
  select(-diff_tax_dollar) %>%
  arrange(diff_price_cpi) %>%
  head(5)
colnames(low5states) <- c("State", "Increases in Cigarette Prices")

data4 <- tax_data %>%
  filter(state %in% low5states$State) %>%
  group_by(Year, state) %>%
  summarise(avg_sales_per_cap = mean(sales_per_capita)) %>%
  mutate(state = factor(state, levels = low5states$State))

q4.graph <- data4 %>%
  ggplot(aes(x = Year, y = avg_sales_per_cap, color = state)) + 
  geom_point(alpha = 0.75) + geom_line(alpha = 0.75) +
  scale_color_manual(values = c("thistle1", "rosybrown1", "salmon1", "pink3", "orchid4"), labels = levels(factor(data4$state))) +
  scale_x_continuous(breaks = seq(1970,2020,5)) +
  labs(x = "Year", y = "Average Pack Sales per Capita", Title = "Average Pack Sales per Capita in 5 States with the Lowest Increases in Cigarette Prices")+
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top")

q4.graph


# 5. Compare the trends in sales from the 5 states with the highest price increases to those with the lowest price increases.


data5 <- tax_data %>%
  filter(state %in% low5states$State | state %in% high5states$State) %>%
  group_by(Year, state) %>%
  summarise(avg_sales_per_cap = mean(sales_per_capita)) %>%
  mutate(state = factor(state, levels = c(high5states$State, low5states$State))) %>%
  mutate(highestPI_lowestPI = ifelse(state %in% low5states$State, "Lowest 5 States", "Highest 5 States"))

q5.graph <- data5 %>%
  ggplot(aes(x = Year, y = avg_sales_per_cap, color = highestPI_lowestPI)) + 
  geom_point(alpha = 0.5) + geom_line(alpha = 0.5) +
  scale_color_manual(values = c("skyblue4", "salmon3"),
                     labels = levels(factor(data5$highestPI_lowestPI))) +
  scale_x_continuous(breaks = seq(1970,2020,5)) +
  labs(x = "Year", y = "Average Pack Sales per Capita", Title = "Average Pack Sales per Capita in 5 States with the Highest & Lowest Increases in Cigarette Prices from 1970 t0 2019")+
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top")

q5.graph


##Estimate the ATEs

# 6. Focusing only on the time period from 1970 to 1990, regress log sales on log prices to estimate the price elasticity of demand over that period. Interpret your results.

data_1970_1990 <- tax_data %>%
  mutate(ln_sales = log(sales_per_capita), ln_price = log(price_cpi)) %>%
  filter(Year %in% 1970:1990)

regress <- lm(formula = ln_sales ~ ln_price, data = data_1970_1990)


# 7. Again limiting to 1970 to 1990, regress log sales on log prices using the total (federal and state) cigarette tax (in dollars) as an instrument for log prices.

r.instrument <- feols(fml = ln_sales ~ 1 | ln_price ~ tax_cpi, data = data_1970_1990)
r.instrument.alt <- ivreg(formula = ln_sales ~ ln_price | tax_cpi, data = data_1970_1990)

summary(r.instrument)
# 8. Show the first stage and reduced-form results from the instrument.

step1 <- lm(formula = ln_price ~ tax_cpi, data = data_1970_1990)
step1.price <- predict(step1)
step2 <- lm(formula = ln_sales ~ step1.price, data = data_1970_1990)
rf <- lm(formula = ln_sales ~ tax_cpi, data = data_1970_1990)

summary(step1)

# 9. Repeat the questions 6-8 focusing on the period from 1991 to 2015

data_1991_2015 <- tax_data %>%
  mutate(ln_sales = log(sales_per_capita), ln_price = log(price_cpi)) %>%
  filter(Year %in% 1991:2015)

# 10. Compare your elasticity estimates from 1970-1990 versus those from 1991-2015. Are they different? If so, why?


save.image("Hwk3_workspace.Rdata")
