library(readxl)
library(tidyverse)
library(plotly)
library(ggpubr)

setwd("~/R")

eda_data = read_excel("eda_data.xlsx")


eda_data <- eda_data |> 
  mutate(year = factor(year)) |> 
  mutate_if(is.character,as.factor) |> 
  mutate(month =factor(month, levels = c ("Jan","Feb","March","Apr","May","Jun","July","Aug","Sept","Oct","Nov","Dec"))) |> 
  arrange(year,month,weekday) |> 
  dplyr::select('qty', 'year','month', 'weekday','product_name', 'state', "hmonth","temperature") |> 
  collect()

str(eda_data)

# Descriptive and Inferences Analysis

library(ggsci)
library(ggrain)
library(scales)

# descriptive statistics
library(dlookr)
library(EnvStats)
library(modeest)
library(MESS)
library(descriptr)
library(rstatix)
library(sjstats)

# data transformation
library(here)
#library(tidyverse)

# packages for reporting the results
library(gtsummary)
library(flextable)
library(report)
library(data.table)

#EDA-1: 
#A pie chart is a circular statistical graphic that is divided into slices to illustrate numerical proportions. 
#Each slice represents a proportionate part of the whole, and the total of all slices equals 100%. 
#Pie charts are commonly used to display categories of data and show the relationship of each category to the total.
# Pie charts are suitable for representing data with relatively few categories, where the emphasis is on comparing the proportions of the whole.

#Pie chart: by Calender month, % of qty
eda1 <- eda_data |> 
  group_by(month) |> 
  summarise(qty=sum(qty)) 

piepercent<- round(100 * eda1$qty / sum(eda1$qty), 2)

pie(eda1$qty, labels = paste0(piepercent, "% - ", eda1$month))

# As per pie chart, it shows there is average 8% selling on each month except for the month of August, September and November which 
# represent below slightly 8%.

#Pie chart: by weekdays, % of qty
eda_weeekday <- eda_data |> 
  group_by(weekday) |> 
  summarise(qty=sum(qty)) 

piepercent<- round(100 * eda_weeekday$qty / sum(eda_weeekday$qty), 2)

pie(eda_weeekday$qty, labels = paste0(piepercent, "% - ", eda_weeekday$weekday))
# As per above chart, it represents that each weekday Qty selling ratio is above 15% except for the day of Saturday and Sunday.
# we can understand due to weekend, its likely have low selling qty.


# Eda2: 
#A box plot is a statistical representation of the distribution of a dataset. 
#It provides a visual summary of the minimum, first quartile, median, third quartile, and maximum of a set of data. 
#The box represents the interquartile range (IQR), which is the range between the first quartile (Q1) and the third quartile (Q3). 
# The length of the box indicates the spread of the middle 50% of the data.
#Box Plot: qty by month
eda2 <- eda_data |> 
  ggplot(mapping = aes(x = month, y = qty, fill = month,.group = month))+
  geom_boxplot() +
  labs(x = NULL, y = "Qty", fill = "month") +
  coord_flip() +
  theme_classic()

ggplotly(eda2)
# as per above box chart, selling qty is above the far median value and evidence that distribution of qty is not normally distributed by month.
# In other words, Individual data points that fall outside the whiskers are considered outliers and are typically plotted individually.



# Due to not normally distributed of qty, here, we will transform the qty by log to check distribution of the qty by month.
#Box Plot: log(qty) by month
eda3 <- eda_data |> 
  ggplot(mapping = aes(x = month, y = log(qty), fill = month))+
  geom_boxplot() +
  labs(x = NULL, y = "Log(Qty)", fill = "month") +
  coord_flip() +
  theme_dark()

ggplotly(eda3)
# After transformed qty into log, we can see that qty are normally distributed among the all months. More on, there is no outline qty so far.



# Eda4: 
# Now, lets evaluate the distribution of qty by state to analyze the selling percentage of the qty. 
# % qty by state
eda4 <- eda_data |> 
  group_by(state) |> 
  summarise(sales=sum(qty),.groups = "drop") |> 
  mutate(sale_per = sales / sum(sales)*100) |> 
  arrange(-sales) |> 
  collect() |> 
  as.data.table()

print(eda4)

# As per table we can evaluate that, Maharashtra and Rajasthan state have highest selling ratio which cover 90% of the total selling qty.
# and following to them, Madhya Pradesh is on third stage which cover 7.74 %. 
#The table provides a breakdown of sales for different states in India, with Maharashtra (MAHARASHTRA) having the highest total sales of 102,617,538.
#The sales figures for other states vary considerably, with Rajasthan, Madhya Pradesh, and Jharkhand also contributing significant amounts.
#The data highlights a notable geographic disparity in sales, with a few states (particularly Maharashtra) dominating the overall sales figures. 
#While individually their contributions are small, collectively, they may still represent important markets.
#Further analysis could involve examining trends over time, understanding factors influencing sales in each state, and identifying opportunities for growth or improvement

# Eda5: 
# % qty by product Name
eda5 <- eda_data |> 
  group_by(product_name) |> 
  summarise(sales=sum(qty),.groups = "drop") |> 
  mutate(sale_per = (sales / sum(sales)*100)) |> 
  arrange(-sales) |> 
  as.data.table() |> 
  collect()

print(eda5)
# Afater evaluation of distribution of qty by state, distribution of qty by product name play a vital role to understand the product-wise 
# selling qty. As per table, Papdi Gathiya is the top most moving product following by Plain Ponga(poha), Tomato Cup , Udad Papad & Masal Cup.
# In addition to that, out of 56 products, 35 products are below 1 percent of the total selling qty means 21 products have more weights than rest
# the products.

# Eda 6: 
# Count by product with %
sales_by_product <- eda_data |> 
  group_by(product_name) |> 
  summarise(count = n(),
            Total_qty = sum(qty)) |>
  arrange(-Total_qty) |> 
  mutate(pct = count/sum(count)*100,
         cuml_perc = cumsum(pct)
  ) |> 
  collect()

#head(sales_by_product,n=10)
#tail(sales_by_product,n=10)
#dim(sales_by_product)

# Bar graph
#A bar graph is a graphical representation of data in which rectangular bars of varying lengths are used to 
# represent the values of different categories. Here are some key points about what a bar graph represents:

#The x-axis (horizontal axis) typically represents different categories or groups, such as items, or locations.
#The y-axis (vertical axis) represents the values or quantities or percentage associated with each category.

#The length of each bar is proportional to the value it represents. Longer bars indicate higher values, while shorter bars indicate lower values.

ggplot(sales_by_product, aes(x = product_name, y = pct)) +
  geom_col(width=0.65, fill = "steelblue4") +
  geom_text(aes(label=paste0(round(pct,2), "%")),
            vjust=-0.6, color = "black", size = 2) +
  labs(x = "Product Name", y = "Percentage",
       caption = "Number of Product: 56") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal(base_size = 20)+
  theme(legend.position="none",
        axis.text.x = element_text(angle = 65, hjust = 1,size = 8))


# As per bar chart, there are 30 products which has 2 or more than 2 percent contribution in the sales by product.


#-------------------------------------------------------------------
# statistical Analysis
#-------------------------------------------------------------------

#The decision about normality of data should be based on a careful consideration of all available 
# information such as graphs (histograms, Q-Q plots), summary and shape measures and statistical tests.

# check Descriptive summary of Qty by state
# summary table is valuable for understanding the central tendency, variability, and distribution characteristics of the quantity variable across different states.
eda_data |> 
  group_by(state) |> 
  describe(qty) |> 
  select(described_variables,state,n, na, mean, sd, p25, p50, p75, skewness, kurtosis) |> 
  ungroup()|> 
  collect() |> 
  print(width = 150)

# States like Madhya Pradesh, Maharashtra, and Rajasthan have relatively high mean quantity values.
# Jharkhand, Uttar Pradesh, and Telangana have high variability in quantity, as indicated by their standard deviations.
# skewness: Skewness is a measure of the asymmetry of the quantity distribution. 
# A positive skewness indicates a right-skewed distribution (tail on the right), and a negative skewness indicates a left-skewed distribution (tail on the left).
# # kurtosis: Kurtosis measures the "tailedness" of the distribution. 
# Positive kurtosis indicates heavy tails (more data in the tails than a normal distribution), while negative kurtosis indicates light tails.

#To check normality of Qty by QQ plot
ggqqplot(eda_data, "qty", conf.int = T ) +
  stat_qq_line(color="blue", na.rm = T)
# As per QQ plot, qty is not normally distributed and its shows extreme skewness at the end.Thus, we will transform the sales qty into log.


#To check normality of log(Qty) by QQ plot
ggqqplot(eda_data, "log(qty)", conf.int = T ) +
  stat_qq_line(color="blue", na.rm = T)


# Histogram of qty
ggplot(eda_data, aes(x = log(qty))) +
  geom_density(fill="steelblue4", color="#8fb4d9", 
               adjust = 1.5, alpha=0.6) +
  theme_minimal(base_size = 20) +
  labs(title = "Density Plot: log(qty)", y = "Density")
# After transformation of qty , density plot looks normally distributed.



# H0: There is normally distributed of the qty among the products
# H1: There is no normally distributed of the qty among the products
# let's take sample from product to test normality by Shapiro test
Stratified.Dataset <- eda_data |> 
  group_by(product_name) |> 
  sample_n(88, replace = F) 


Stratified.Dataset |> 
  group_by(product_name) |> 
  shapiro_test(qty) |>
  print(n=56)

# Interpretation: A small p-value (typically below a significance level like 0.05) suggests that there is enough evidence to reject the null hypothesis of normality. 
# In other words, if the p-value is low, you would conclude that the "qty" variable for that particular product does not follow a normal distribution.



library(nortest)
#Anderson-Darling test for normality. 
# The Anderson-Darling Test is a goodness-of-fit test that determines how well your data fits a given distribution.
ad.test(eda_data$qty)

#With a p-value less than 2.2e-16, you would reject the null hypothesis. This implies that the "qty" variable in your dataset does not follow a normal distribution.



#Check Levene’s test for equality of variances by product_name
# H0: the variances of data in two /all groups are equal
# H1: the variances of data in two /all groups are not equal

eda_data |> 
  levene_test(qty ~ product_name)

# p value is zero means reject the null hypotheses which suggest there is not equal variance among the products.



#Check Levene’s test for equality of variances by month
# H0: the variances of data in months are equal
# H1: the variances of data in months are not equal

eda_data |> 
  levene_test(qty ~ month)
# p value is far below than the significance level i.e. 0.05. Thus, reject the null hypotheses which suggest there is not equal variance among the months.


#Check Levene’s test for equality of variances by state
# H0: the variances of data in states are equal
# H1: the variances of data in states are not equal

eda_data |> 
  levene_test(qty ~ state)
# Looking at the p-values, if a states has a p-value significantly below 0.05, you might conclude that the quantity distribution for that states is not normal.



# One-way ANOVA test
# Now, we will perform an one-way ANOVA (with equal variances: Fisher’s classic ANOVA) to test 
# the null hypothesis that the mean Qty is the same for all the product_name / state / month / category

#by product name
eda_data %>% 
  anova_test(qty ~ product_name, detailed = T)

#Post-hoc tests
# Pairwise comparisons by state
pwc_Tukey_product <- eda_data %>% 
  tukey_hsd(qty ~ product_name) 

print(pwc_Tukey_product,n= 45)
#Post hoc tests are conducted after an ANOVA to explore specific group differences when the ANOVA indicates that there are significant differences among groups


#statistical analysis has revealed that the data is not normally distributed and there is no equal variance among products and states, 
#it indicates challenges related to the assumptions of normality and homogeneity of variances. 
#data is not normally distributed, data might be transformed (e.g., log transformation) to stabilize variances and normality. 
