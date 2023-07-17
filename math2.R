suppressMessages(library(tidyverse))        # data manipulation and plots
suppressMessages(library(funModeling))      # overview stats
library(magrittr)               # to use pipes
library(skimr)                  # to get a quick summary table
library(caret)                  # to create the partition for training/test datasets

options(scipen = 999)                              # turn off scientific notation for numbers
options(repr.plot.width=12, repr.plot.height=8)    # set universal plot size
# read the file in 
df <- read.csv(file.choose())
# denote factor variables
df$sex <- factor(df$sex)
df$smoker <- factor(df$smoker)
df$region <- factor(df$region)
df$children <- factor(df$children)
# check for missing values
sum(is.na(df))
# check data types
str(df)
skim(df)
figsize <- options(repr.plot.width=12, repr.plot.height=12) 
# set plot size for this plot 
# Smoker count plot
smoker <- df %>%
  ggplot(aes(x=smoker, fill=smoker)) +
  geom_bar(show.legend = FALSE) +
  # add percentages on top of bars
  geom_text(
    stat='count',
    aes(label=paste0(round(after_stat(prop*100), digits=1), "%"),group=1),
    vjust=-0.4,
    size=4
  ) +
  # add labels
  labs(
    x = "",
    y = "",
    title = "Number of policyholders by smoking"
  ) +
  # rename x-ticks
  scale_x_discrete(
    labels = c("no" = "Non-smoker", "yes" = "Smoker")
  ) +
  # adjust y-ticks
  scale_y_continuous(
    breaks=seq(0,2000,100)
  ) +
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)
  )
# Region count plot
region <- df %>%
  ggplot(aes(x=forcats::fct_infreq(region), fill=region)) +
  geom_bar(show.legend = FALSE) +
  # add percentages on top of bars
  geom_text(
    stat='count',
    aes(label = paste0(round(after_stat(prop*100), digits=1), "%"), group=1),
    vjust=-0.4,
    size=4
  ) +
  # add labels
  labs(
    x = "",
    y = "",
    title = "Number of policyholders by region"
  ) +
  # rename x-ticks
  scale_x_discrete(
    labels = c("northeast" = "North East", "northwest" = "North West",
               "southeast" = "South East", "southwest" = "South West")
  ) +
  # adjust ticks
  scale_y_continuous(
    breaks=seq(0,350,50)
  ) +
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)
  )

# Sex count plot
sex <- df %>%
  ggplot(aes(x=forcats::fct_infreq(sex), fill=sex)) +
  geom_bar(show.legend = FALSE) +
  # add percentages on top of bars
  geom_text(
    stat='count',
    aes(
      label=paste0(round(after_stat(prop*100), digits=1), "%"), group=1),
    vjust=-0.4,
    size=4
  ) +
  # add labels
  labs(
    x = "",
    y = "",
    title = "Number of policyholders by sex",
    fill = "Sex"
  ) +
  # rename x-ticks
  scale_x_discrete(
    labels = c("male" = "Male", "female" = "Female")
  ) +
  # adjust y-ticks
  scale_y_continuous(
    breaks=seq(0,700,100)
  ) +
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)
  )

# Children count plot
children <- df %>%
  ggplot(aes(x=forcats::fct_infreq(children), fill=children)) +
  geom_bar(show.legend = FALSE) +
  # add percentages
  geom_text(
    stat='count',
    aes(label=paste0(round(after_stat(prop*100), digits=1), "%"), group=1),
    vjust=-0.4,
    size=4
  ) +
  # add labels
  labs(
    x = "",
    y = "",
    title = "Number of dependents per policy"
  ) +
  # rename x-ticks
  scale_x_discrete(
    labels = c("0" = "None")
  ) +
  # adjust y-ticks
  scale_y_continuous(
    breaks=seq(0,600,50)
  ) +
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)
  )

# Plot grid
cowplot::plot_grid(
  smoker, region, sex, children,
  labels="AUTO",
  ncol = 2,
  nrow = 2
)

options(figsize)

figsize <- options(repr.plot.width=20, repr.plot.height=16)

# Age distribution
age_hist <- df %>%
  ggplot(aes(x=age))+
  geom_histogram(
    binwidth = 5,
    show.legend = FALSE,
    fill="#ff5733"
  )+
  labs(
    x = "Ages of policyholders",
    y = "Number of policyholders",
    title = "Distribution of ages"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

age_dens <- df %>%
  ggplot(aes(x=age)) +
  geom_density(
    alpha=.3,
    fill="#ff5733"
  )+
  labs(
    x = "Ages of policyholders",
    y = "Probability density",
    title = "Distribution of ages"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

age_box <- df %>%
  ggplot(aes(y=age)) +
  geom_boxplot(
    alpha=.5,
    fill="#ff5733"
  )+
  coord_flip() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs(
    y = "Ages of policyholders",
    x = "",
    title = "Distribution of ages"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

# BMI distribution
bmi_hist <- df %>%
  ggplot(aes(x=bmi))+
  geom_histogram(
    binwidth = 4,
    show.legend = FALSE,
    fill = "#55ab11"
  )+
  labs(
    x = "BMI scores of policyholders",
    y = "Number of policyholders",
    title = "Distribution of BMI scores"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

bmi_dens <- df %>%
  ggplot(aes(x=bmi)) +
  geom_density(
    alpha=.3,
    fill="#55ab11"
  )+
  labs(
    x = "BMI scores of policyholders",
    y = "Probability density",
    title = "Distribution of BMI scores"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

bmi_box <- df %>%
  ggplot(aes(y=bmi)) +
  geom_boxplot(
    alpha=.5,
    fill="#55ab11"
  )+
  coord_flip() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs(
    y = "BMI scores of policyholders",
    x = "",
    title = "Distribution of BMI scores"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

# Charges distribution
charges_hist <- df %>%
  ggplot(aes(x=charges)) +
  geom_histogram(
    binwidth = 2000,
    show.legend = FALSE,
    fill = "#FFC300"
  )+
  labs(
    x = "Charges to policyholders ($)",
    y = "Number of policyholders",
    title = "Distribution of medical charges"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

charges_dens <- df %>%
  ggplot(
    aes(x=charges)
  ) +
  geom_density(
    alpha=.3,
    fill="#FFC300"
  ) +
  labs(
    x = "Charges to policyholders ($)",
    y = "Probability density",
    title = "Distribution of medical charges"
  ) +
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

charges_box <- df %>%
  ggplot(aes(y=charges))+
  geom_boxplot(
    alpha=.5,
    fill="#FFC300"
  )+
  coord_flip()+
  # remove ticks from y-axis
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs(
    y = "Charges to policyholders ($)",
    x = "",
    title = "Distribution of medical charges"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

cowplot::plot_grid(
  age_hist, age_dens, age_box,
  bmi_hist, bmi_dens, bmi_box,
  charges_hist, charges_dens, charges_box,
  labels="AUTO",
  ncol = 3,
  nrow = 3
)

options(figsize)

figsize <- options(repr.plot.width=20, repr.plot.height=26)

# Boxplots
chargesBysmoker <- df %>%
  ggplot(
    aes(
      x=forcats::fct_reorder(smoker, charges, .fun=median, .desc=TRUE),
      y=charges,
      fill=smoker
    )
  ) +
  geom_boxplot(show.legend = FALSE) +
  coord_flip() +
  labs(
    x = "",
    y = "Charges to policyholders ($)",
    title = "Distribution of charges by smoking"
  )+
  scale_x_discrete(
    labels = c("no" = "Non-smoker", "yes" = "Smoker")
  )+
  # resize text
  theme(
    plot.title = element_text(size=18),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

chargesByregion <- df %>%
  ggplot(
    aes(
      x=forcats::fct_reorder(region, charges, .fun=median, .desc=TRUE),
      y=charges,
      fill=region
    )
  ) +
  geom_boxplot(show.legend = FALSE) +
  coord_flip() +
  labs(
    x = "",
    y = "Charges to policyholders ($)",
    title = "Distribution of charges by region"
  )+
  scale_x_discrete(
    labels = c("northeast" = "North East", "northwest" = "North West",
               "southeast" = "South East", "southwest" = "South West")
  )+
  # resize text
  theme(
    plot.title = element_text(size=18),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

chargesBychildren <- df %>%
  ggplot(
    aes(
      x=forcats::fct_reorder(children, charges, .fun=median, .desc=TRUE),
      y=charges,
      fill=children
    )
  ) +
  geom_boxplot(show.legend = FALSE) +
  coord_flip() +
  labs(
    x = "",
    y = "Charges to policyholders ($)",
    title = "Distribution of charges by dependents"
  )+
  scale_x_discrete(
    labels = c("0" = "None")
  )+
  # resize text
  theme(
    plot.title = element_text(size=18),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

chargesBysex <- df %>%
  ggplot(
    aes(
      x=forcats::fct_reorder(sex, charges, .fun=median, .desc=TRUE),
      y=charges,
      fill=sex
    )
  ) +
  geom_boxplot(show.legend = FALSE) +
  coord_flip() +
  labs(
    x = "",
    y = "Charges to policyholders ($)",
    title = "Distribution of charges by sex"
  )+
  scale_x_discrete(
    labels = c("male" = "Male", "female" = "Female")
  )+
  # resize text
  theme(
    plot.title = element_text(size=18),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )
# Plot grid of all plots
cowplot::plot_grid(
  chargesBysmoker,
  chargesByregion, 
  chargesBysex, 
  chargesBychildren, 
  labels="AUTO",
  ncol = 2,
  nrow = 4
)

options(figsize)

figsize <- options(repr.plot.width=12, repr.plot.height=8)

age_scatter <- df %>%
  ggplot(aes(x=age, y=charges)) +
  geom_point()+
  # add a linear regression line
  geom_smooth(method='lm')+
  labs(
    x = "Ages of policyholders",
    y = "Charges to policyholders ($)",
    title = "Medical Charges vs Policyholder Age"
  )+
  # resize text
  theme(
    plot.title = element_text(size=18),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

bmi_scatter <- df %>%
  ggplot(aes(x=bmi, y=charges)) +
  geom_point()+
  # add a linear regression line
  geom_smooth(method='lm')+
  labs(
    x = "BMI scores of policyholders",
    y = "Charges to policyholders ($)",
    title = "Medical Charges vs Policyholder BMI"
  )+
  # resize text
  theme(
    plot.title = element_text(size=18),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

cowplot::plot_grid(
  age_scatter, bmi_scatter,
  labels="AUTO",
  ncol = 2,
  nrow = 1
)

options(figsize)

df %>%
  group_by(smoker) %>%
  summarise(
    count = n(),
    min = min(charges),
    median = median(charges),
    max = max(charges),
    IQR = IQR(charges)
  ) %>%
  arrange(desc(median)) # sort by median in descending order
wilcox.test(df$charges ~ df$smoker)

df %>%
  group_by(region) %>%
  summarise(
    count = n(),
    min = min(charges),
    median = median(charges),
    max = max(charges),
    IQR = IQR(charges)
  ) %>%
  arrange(desc(median)) # sort by median in descending order
kruskal.test(charges ~ region, data = df)
df %>%
  group_by(children) %>%
  summarise(
    count = n(),
    min = min(charges),
    median = median(charges),
    max = max(charges),
    IQR = IQR(charges)
  ) %>%
  arrange(desc(median)) # sort by median in descending order
kruskal.test(charges ~ children, data = df)

pairwise.wilcox.test(df$charges, df$children, p.adj = "BH")
# Show which p-values are less than 0.05
pairwise.wilcox.test(df$charges, df$children, p.adj = "BH")$p.value < 0.05

charges_hist <- df %>%
  ggplot(
    aes(x=charges)
  ) +
  geom_histogram(
    binwidth = 2000,
    show.legend = FALSE,
    fill = "#FFC300"
  )+
  labs(
    x = "Charges to policyholders ($)",
    y = "Number of policyholders",
    title = "Distribution of medical charges"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14))

charges_hist_log10 <- df %>%
  ggplot(
    aes(x=log10(charges))
  ) +
  geom_histogram(
    show.legend = FALSE,
    fill = "#FFC300"
  )+
  labs(
    x = "Charges to policyholders log10 transformed",
    y = "Number of policyholders",
    title = "Distribution of medical charges after log10 transform"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14))

cowplot::plot_grid(
  charges_hist, charges_hist_log10,
  labels="AUTO",
  ncol = 2,
  nrow = 1)
# log10 transform of response variable 
df$logCharges<- log10(df$charges)

# Split the data into training and test sets
set.seed(122)                    # Set the seed to make the partition reproducible
training.samples <- df$logCharges %>%
  createDataPartition(p = 0.8, list = FALSE)
train  <- df[training.samples, ]
test <- df[-training.samples, ]

# Train the model on the training dataset
formula <- as.formula("logCharges ~ smoker + bmi + age + children + sex + region")
model <- lm(formula, data = train)
summary(model)
