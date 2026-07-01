# Exam Statistics 2026_1
Reuben Njue

``` {r}
pacman::p_load(conflicted,tidyverse,wrappedtools,broom,car,
               palmerpenguins,
               rlist, flextable,
               patchwork, ggbeeswarm, ggsignif, 
               rpart, rpart.plot, pROC)
conflicts_prefer(dplyr::filter, 
                 dplyr::select,
                 palmerpenguins::penguins)
set_flextable_defaults(big.mark = " ", 
                       font.size = 9, 
                       theme_fun = theme_vanilla,
                       padding.bottom = 1, 
                       padding.top = 3,
                       padding.left = 3,
                       padding.right = 4,
                       font.family = "sans",
                       background.color = "#FFFFFF",       # Forces a crisp white background
                       text.color = "#222222",             # Forces dark charcoal text 
                       border.color = "#CCCCCC"            # Gives clear light gray grid borders
)

theme_set(theme_bw())
theme_update(
  plot.title=element_text(family="sans"),
  plot.caption=element_text(family="sans"),
  axis.title=element_text(family="sans"),
  axis.text=element_text(family="sans",face = "bold"),
  strip.text=element_text(family="sans"))
```

``` {r}
#any packages needed? put them here!
pacman::p_load(conflicted, tidyverse, wrappedtools, flextable, dplyr, ggplot2, ggpubr, ggbeeswarm, multcomp, broom, stats)
```

Please give your answers underneath the question, referencing question
numbers.

Please format text answers as comments or put them in text parts.

# Q1: Please generate the following sequences of numbers: (5 pts)

## Q1a: Integers between 95 and 45 in descending order, including borders

``` {r}

(q1a <- 95:45)
(q1a <- seq(95,45, -1))
```

## Q1b: 100 random numbers from a uniform distribution with minimum=18 and maximum=65

``` {r}
(q1b <- runif(n = 100, min = 18, max = 65))
sample(18:65, 100, replace = TRUE)
```

## Q1c: 50 random numbers from a Normal distribution with mean=100 and SD=15

``` {r}
set.seed(42)
(q1c <- rnorm(n = 50, mean = 100, sd = sqrt(15)))
#AB 15 is already SD, not variance!!
```

## Q1d: 20 random numbers from a Poisson distribution with lambda=3

``` {r}
(q1d <- rpois(n= 20, lambda = 3))
```

## Q1e: 6 Lotto numbers (unique, uniform distribution) between 1 and 49 (“6 aus 49”).

``` {r}

(q1e <- sample(x = 1:49, size = 6, replace = FALSE))
```

## Q2: Test all the various numbers you generated in Q1 against the Normal distribution: Plot distributions and create a table structure with the p-values and your interpretation (3 pts)

``` {r}
set.seed(42)
#data. Not necessary to recrete. 
q1a <- 45:95
q1b <- runif(n = 100, min = 18, max = 65)
q1c <- rnorm(n = 50, mean = 100, sd = sqrt(15))
q1d <- rpois(n= 20, lambda = 3)
q1e <- sample(x = 1:49, size = 6, replace = FALSE)

#Shapiro-Wilk for Normality Tests
shp_q1a <- shapiro.test(q1a)
shp_q1b <- shapiro.test(q1b)
shp_q1c <- shapiro.test(q1c)
shp_q1d <- shapiro.test(q1d)
shp_q1e <- shapiro.test(q1e)



#PLOTTING 

#Histogram
# Q1a
par(mfrow = c(2, 5), mar = c(4, 4, 3, 1))
hist(q1a, main="Q1a: Descending", xlab="Value", col="lightblue", prob=TRUE)
lines(density(q1a), col="red", lwd=2)
# Q1b
hist(q1b, main="Q1b: Uniform", xlab="Value", col="lightgreen", prob=TRUE)
lines(density(q1b), col="red", lwd=2)
# Q1c
hist(q1c, main="Q1c: Normal", xlab="Value", col="lightpink", prob=TRUE)
lines(density(q1c), col="red", lwd=2)
# Q1d
hist(q1d, main="Q1d: Poisson", xlab="Value", col="lightyellow", prob=TRUE)
lines(density(q1d), col="red", lwd=2)
# Q1e
hist(q1e, main="Q1e: Lotto", xlab="Value", col="wheat", prob=TRUE)
lines(density(q1e), col="red", lwd=2)

# Q-Q Plot 
qqnorm(q1a, main="Q1a Q-Q Plot"); qqline(q1a, col="blue")
qqnorm(q1b, main="Q1b Q-Q Plot"); qqline(q1b, col="blue")
qqnorm(q1c, main="Q1c Q-Q Plot"); qqline(q1c, col="blue")
qqnorm(q1d, main="Q1d Q-Q Plot"); qqline(q1d, col="blue")
qqnorm(q1e, main="Q1e Q-Q Plot"); qqline(q1e, col="blue")
par(mfrow = c(1, 1))


#ALTERNATIVE
set.seed(42)
data_list <- list(
  `q1a: Descending` = 95:45,
  `q1b: Uniform`    = runif(n = 100, min = 18, max = 65),
  `q1c: Normal`     = rnorm(n = 50, mean = 100, sd = sqrt(15)),
  `q1d: Poisson`    = rpois(n= 20, lambda = 3),
  `q1e: Lotto`      = sample(x = 1:49, size = 6, replace = FALSE)
)
df_long <- enframe(
  data_list, 
  name = "Sequence",
  value = "Value") |>
  unnest(Value)

gg_plot <- ggplot(df_long, aes(x = Value)) +
  geom_density(fill = "steelblue", alpha = 0.6, color = "darkblue") +
  facet_wrap(~ Sequence, scales = "free", ncol = 3) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Density Distributions of Generated Sequences",
    x = "Value", 
    y = "Density"
  ) +
  theme(strip.background = element_rect(fill = "gray95", color = NA))
gg_plot

table_output <- df_long |>
  group_by(Sequence) |>
  summarise(
    Sample_Size = n(),
    p_value = shapiro.test(Value)$p.value
  ) |> 
  mutate(
    Interpretation = if_else(
      p_value < 0.05, 
      "Reject H0: Not Normally Distributed", 
      "Fail to reject H0: Normally Distributed"
    )
  ) |>
  flextable() |>
  set_header_labels(
    Sequence = "Sequence Group",
    Sample_Size = "Sample Size (n)",
    p_value = "Shapiro-Wilk (p-value)",
    Interpretation = "Normality Decision"
  ) |>
  colformat_double(j = "p_value", digits = 4) |> 
  autofit() |>
  theme_vanilla() |>
  bg(i = ~ p_value < 0.05, j = "Interpretation", bg = "#FFCCCC") |> 
  bg(i = ~ p_value >= 0.05, j = "Interpretation", bg = "#E2F0D9")

table_output
#AB your seed resulted in a non-normal sample from rnorm(), in such a case choose a different seed!
```

# Q3: Please explain the following terms in your own words (rather than just copy/pasting from wikipedia/chatbots) (12 pts)

## Q3a: standard deviation

a measure of how the actual individual data diviate from group mean
.e.i. the measuring the spreading of my values in a dataset around the
group mean.

it is a measure of variability of actual measurement (data) within a
group. meaning if I take the measurement of weight in a certain group
and the mean is 70kg with SD of + or - 1.5, my subjects are of almost
equal weight. if SD is + or - 16, it means the weight of my subject is
quite varying.

## Q3b: standard error of the mean

SEM measure how accurate the mean of the group is. The small the sample
the larger the SEM and the larger the sample the small the SEM. Meaning
large sample size mirror the true nature of the population. in other
word, true population mean.

## Q3c: confidence interval

It is a range of values that contains the true population parameter.
e.g. 95% CI means assuming a normal distribution of my data, the true
population mean should be found with this net.

## Q3d: statistical power

Statistical power is the probability that my study will produce the
expected effect if indeed that effect exist. In other word, it give
ability to avoid false negative. It measures likelihood of the study to
reject H0 when H0 is false. conventionally set at 0.80(80%).

## Q3e: working hypothesis (with example)

This is the outcome to expect from my study. E.g. knockout (KO) of gene
A reduces development of disease X in KO-mice compared to
control/wildtype mice (wt)

## Q3f: Null hypothesis (with example for Q3e)

This is what to measure in the study. Always assume there is no effect.
E.g. Knockout of gene A does not have effect on the development of
disease X, thus development of disease X is the same in KO as it is in
wildtype (Wt)

# Q4: Please name the class / function for test statistics in the following cases: (4 pts)

## Q4a: Comparison of mean values for 2 groups for a Gaussian measure; what descriptive statistic would be suitable together with the test?

test: student’s t-test

function_xy(): t.test()

description: mean() or meansd() and sd()

## Q4b: Comparison of central tendencies for ordinal data between two groups; what descriptive statistic would be suitable together with the test?

test: Wilcoxon rank-sum test / Mann-Whitney U test

function_xy(): `wilcox.test()`

description: Median: `median()` and interquartile range (IQR): `IQR()`

# Q5: Please import and analyze the dataset bordeaux.xlsx (6 pts)

Info on dataset:

- year: year of harvest

- temperature: sum of daily average temperatures (in Celsius degrees)

- sun: duration of insolation (in hours)

- heat: number of super-hot days

- rain: rain level (in millimeters)

- quality: wine quality: a factor with levels bad, good, and medium

## Q5a: Program the import into a variable and save (“pick” / address) the first 5 rows from columns rain and quality in a *separate* variable. Analyses with all rows!

``` {r}
rawdata <- readxl::read_xlsx("bordeaux.xlsx")
str(rawdata)


#First 5 rows from columns rain and quality 
rain_qlty <- rawdata[1:5, c ("rain", "quality")]
rain_qlty

#converting quality to Factor for Q5c
rawdata$quality <- factor(rawdata$quality, levels = c("bad", "medium", "good"))
```

## Q5b: Create a box plot for rain level, grouped by quality, using ggplot. Try to use a reasonable order of quality levels

``` {r}
ggplot(rawdata, aes(x = quality, y = rain, fill = quality)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 16) +
  scale_fill_manual(values = c("#FF9999", "#FFCC99", "#99FF99")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Wine Quality vs. Rain Level during Harvest",
    x = "Wine Quality Category",
    y = "Rain Level (in mm)"
  ) +
  theme(legend.position = "none")


#EVEN BETER 
my_comparisons <- list( 
  c("bad", "medium"), 
  c("medium", "good"), 
  c("bad", "good") 
)

ggplot(rawdata, aes(x = quality, y = rain, fill = quality)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, color = "grey30") +
  geom_beeswarm(aes(color = quality), cex =2, size = 2.5, alpha = 0.8, priority = "density") +
  scale_fill_manual(values = c("#FF9999", "#FFCC99", "#99FF99")) +
  scale_color_manual(values = c("#CC3333", "#CC6600", "#339933")) +
  stat_compare_means(
    comparisons = my_comparisons, 
    method = "t.test",       
    label = "p.signif",       
    label.y = c(700, 760, 820) 
  ) +
  
  stat_compare_means(method = "anova", label.y = 890) + 
  theme_minimal(base_size = 14) +
  labs(
    title = "Wine Quality vs. Rain Level",
    x = "Wine Quality Category",
    y = "Rain Level (in mm)"
  ) +
  theme(legend.position = "none")
```

## Q5c: Test, if (and which specifically) group differences you observe in Q5b could be a chance outcome, formulate the appropriate Null hypothesis. (Keep in mind that ‘*dependent variable*’ does not imply *causality*).

H0​: μ bad​ = μ medium​ = μ good​. The mean rain levels are the same for
wines of bad, medium and good quality. Any observed group differences is
entirely due to random chance.

``` {r}
anova_model <- lm(rain ~ quality, data = rawdata)
anova_out <- anova(anova_model)
print(anova_out)
anova_out$`Pr(>F)`


# post-hoc: Tukey
glht_out <- summary(glht(
  model = anova_model,
  linfct = mcp(quality = "Tukey")))

summary(glht_out)
tidy(glht_out) |>
  flextable()
```

\#Results from anaylsis

The global ANOVA give a p.value = 0.001185. Thus we reject the Null
hypothesis.

pairwise comparisons test: Tukey test show:

medium vs. bad p.adj.value = 0.0198 is significant thus bad quality
years gives more rain than medium quality years

good vs. bad p.adj.value = 0.0012 is significant thus bad guality year
gives more rainfall on average than good quality years.

good vs. medium p.adj.value = 0.5391 which mean no statistical
significant difference.  
Causality: This rain level correlation with poor wine quality does not
prove direct causality rather, there could be other associated factors
that accompany rainy weather.

## Q5d: Create a report table with descriptive statistics for temperature and rain for the levels of quality.

``` {r}
desc_stat <- rawdata |>
  group_by(quality) |>
  summarise(
    N_years = n(),
    MeanSD_Temp = meansd(temperature),
    MeanSD_Rain = meansd(rain),
  ) #|>
  #flextable()

print(desc_stat)
```

## Q5e: Export that table into a text file with a ‘;’ as separator

``` {r}
write.table(
  x = desc_stat, 
  file = "rawdata_quality_summary.txt", 
  sep = ";", 
  row.names = FALSE, 
  quote = FALSE
)
```

# Q6: Create a matrix named ‘participants’ with 3 columns and 20 rows. (2 pts)

- Fill column 1 with the words “Participant 1” to “Participant 20”

- Put your first name in row 2 column 2 and your family name in row 2
  column 3

``` {r}
participants <- matrix(data = NA, nrow = 20, ncol = 3, byrow = FALSE)
participants[, 1] <- paste("Participant", 1:20)
participants[2, 2] <- "Reuben Mukundi"
participants[2, 3] <- "Njue"
print(participants)
```

# Q7: Program a loop that runs along the rows of matrix from Q6 and that prints the run number and the first name; if there is no name, state “missing”. (2 pts)

``` {r}
for (i in 1:nrow(participants)) {
  first_name <- participants[i, 2]
  if (first_name == "" || is.na(first_name)) {
    display_name <- "missing"
  } else {
    display_name <- first_name
  }
  cat("Run number:", i, "-> First Name:", display_name, "\n")
}
```

# 

***Good luck!***

# 33/34 pts / 1
