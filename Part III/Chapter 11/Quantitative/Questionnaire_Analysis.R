# Preamble ====================================================================

install.packages("readxl")
install.packages("psych")
install.packages("robustlmm")
library("readxl")
library("psych")
library("robustlmm")
library("caret")
library("caTools") 
library("dplyr")
library("ggplot2")
library("tidyr")
library(hrbrthemes)
library(viridis)
library(gridExtra)



# Setup R to read current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(scipen=999)

#Setup a random seed for reproducability.
set.seed(2342) 

# Box Plots for Analysis (see Section X.X) =====================================

# SRIS BOX PLOT 
my_data = read_excel("Questionnaire_Data.xlsx", sheet = "data reduced")
sris_subset = my_data[, c("SRIS_Engagement_Score",	
                         "SRIS_Need_Score",	
                         "SRIS_Insight_Score",	
                         "SRIS_Score")]

# Set label size same for all box plots 
label_size = 11

# Reshape the data into long format
sris_subset_long <- gather(sris_subset, key = "variable", value = "value")
sris_subset_long

means <- sris_subset_long %>%
  group_by(variable) %>%
  summarise(mean_value = mean(value))

sris_plot <- sris_subset_long %>%
  ggplot(aes(x=variable, y=value, fill=variable)) +
    geom_boxplot(aes(colour = variable), fill="transparent", outlier.shape = 1) +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    scale_y_continuous(breaks = seq(1, 6, by = 1.0), limits=c(1,6)) +  # Set breaks for y-axis
    scale_x_discrete(labels = c(
      "SRIS_Engagement_Score" = "Engagement\n(Mean=4.5)\n(Med=4.5)\n(SD=0.7)",
      "SRIS_Need_Score" = "Need\n(Mean=4.6)\n(Med=4.5)\n(SD=0.7)",
      "SRIS_Insight_Score" = "Insight\n(Mean=3.9)\n(Med=4.0)\n(SD=0.4)",
      "SRIS_Score" = "SRIS Total\n(Mean=4.3)\n(Med=4.3)\n(SD=0.4)"
    )) +
    theme_minimal() +
  geom_point(data = means, aes(x = variable, y = mean_value, colour=variable), 
             shape = 4, size = 3) +
  #geom_point(data = sris_subset_long, aes(x = variable, y = value, colour=variable),
  #           position = position_jitter(width = 0.2), alpha = 0.3) +  # Add points for participants
  theme(legend.position="none",
        plot.title = element_text(size = label_size),
        axis.text.x = element_text(size = label_size),
        axis.text.y = element_text(size = label_size),
        axis.title.y = element_text(size = label_size)) +
  ylab("SRIS - Average Scores\n") + xlab("") 
sris_plot



# MSI BOX PLOTS
msi_subset = my_data[, c("MSI_Active_Engagement_mean",
                         "MSI_Perceptual_Abilities_mean",
                         "MSI_Musical_Training_mean",
                         "MSI_Singing_Abilities_mean")]

# Reshape the data into long format
msi_subset_long <- gather(msi_subset, key = "variable", value = "value")
msi_subset_long

means <- msi_subset_long %>%
  group_by(variable) %>%
  summarise(mean_value = mean(value))

msi_av_plot <- msi_subset_long %>%
  ggplot(aes(x=variable, y=value, fill=variable)) +
  geom_boxplot(aes(colour = variable), fill="transparent", outlier.shape = 1) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  scale_y_continuous(breaks = seq(1, 6, by = 1.0), limits=c(1,6)) +  # Set breaks for y-axis
  scale_x_discrete(labels = c(
    "MSI_Active_Engagement_mean" = "Active\nEngagement\n(Mean=4.0)\n(Med=4.0)\n(SD=1.5)",
    "MSI_Perceptual_Abilities_mean" = "Perceptual\nAbilities\n(Mean=4.2)\n(Med=4.0)\n(SD=1.2)",
    "MSI_Musical_Training_mean" = "Musical\nTraining\n(Mean=2.8)\n(Med=2.4)\n(SD=1.6)",
    "MSI_Singing_Abilities_mean" = "Singing\nAbilities\n(Mean=4.2)\n(Med=4.1)\n(SD=1.3)"
  )) +
  theme_minimal() +
  geom_point(data = means, aes(x = variable, y = mean_value, colour=variable), 
             shape = 4, size = 3) +
  #geom_point(data = msi_subset_long, aes(x = variable, y = value, colour=variable),
  #           position = position_jitter(width = 0.2), alpha = 0.3) +  # Add points for participants
  theme(legend.position="none",
        plot.title = element_text(size = label_size),
        axis.text.x = element_text(size = label_size),
        axis.text.y = element_text(size = label_size),
        axis.title.y = element_text(size = label_size)) +
  ylab("Goldsmith's MSI - Average Scores\n") + xlab("") 
msi_av_plot

gmssum_subset = my_data[, c("MSI_GMS_sum")]
gmssum_subset

# Reshape the data into long format
gmssum_subset_long <- gather(gmssum_subset, key = "variable", value = "value")
gmssum_subset_long

means <- gmssum_subset_long %>%
  group_by(variable) %>%
  summarise(mean_value = mean(value))

gmsum_plot <- gmssum_subset_long %>%
  ggplot(aes(x=variable, y=value, fill=variable)) +
  geom_boxplot(aes(colour = variable), fill="transparent", colour="navy", outlier.shape = 1) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  scale_y_continuous(breaks = seq(18, 126, by = 12.0), limits=c(18,126)) +  # Set breaks for y-axis
  scale_x_discrete(labels = c(
    "MSI_GMS_sum" = "MSI\nScore\n(Mean=67.7)\n(Med=63.5)\n(SD=21.5)"
  )) +
  theme_minimal() +
  geom_point(data = means, aes(x = variable, y = mean_value, colour="navy"), colour="navy", 
             shape = 4, size = 3) +
  #geom_point(data = gmssum_subset_long, aes(x = variable, y = value, colour="navy"), colour="navy",
  #           position = position_jitter(width = 0.2), alpha = 0.3) +  # Add points for participants
  theme(legend.position="none",
        plot.title = element_text(size = label_size),
        axis.text.x = element_text(size = label_size),
        axis.text.y = element_text(size = label_size),
        axis.title.y = element_text(size = label_size)) +
  ylab("Goldmiths's MSI - Total Score\n") + xlab("") 
gmsum_plot

# Put all MSI things on the same plot 
grid.arrange(gmsum_plot, msi_av_plot, nrow=1, widths=c(0.3,.7))




# UEQ BOX PLOT
my_data = read_excel("Questionnaire_Data.xlsx", sheet = "data reduced")
ueq_subset = my_data[, c("UEQ_Focused_Attention",
                          "UEQ_Aesthetic_Appeal",
                          "UEQ_Perceived_Usability",
                          "UEQ_Reward",	
                          "UEQ_Engagement")]

# Reshape the data into long format
ueq_subset_long <- gather(ueq_subset, key = "variable", value = "value")
ueq_subset_long$variable <- factor(ueq_subset_long$variable, levels = c(
  "UEQ_Focused_Attention",
  "UEQ_Aesthetic_Appeal",
  "UEQ_Perceived_Usability",
  "UEQ_Reward",
  "UEQ_Engagement"  # "test" should be last
))
ueq_subset_long

means <- ueq_subset_long %>%
  group_by(variable) %>%
  summarise(mean_value = mean(value))

ueq_plot <- ueq_subset_long %>%
  ggplot(aes(x=variable, y=value, fill=variable)) +
  geom_boxplot(aes(colour = variable), fill="transparent", outlier.shape = 1) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  scale_y_continuous(breaks = seq(1, 5, by = 1.0), limits=c(1,5)) +  # Set breaks for y-axis
  scale_x_discrete(labels = c(
    "UEQ_Focused_Attention" = "Focused\nAttention\n(Mean=4.1)\n(Med=4.0)\n(SD=0.6)",
    "UEQ_Aesthetic_Appeal" = "Aesthetic\nAppeal\n(Mean=3.8)\n(Med=3.7)\n(SD=0.6)",
    "UEQ_Perceived_Usability" = "Perceived\nUsability\n(Mean=3.7)\n(Med=3.7)\n(SD=0.6)",
    "UEQ_Reward" = "Reward\n\n(Mean=4.1)\n(Med=4.0)\n(SD=0.5)",
    "UEQ_Engagement" = "UEQ\n\n(Mean=3.9)\n(Med=3.9)\n(SD=0.4)"
  )) +
  theme_minimal() +
  geom_point(data = means, aes(x = variable, y = mean_value, colour=variable), 
             shape = 4, size = 3) +
  #geom_point(data = ueq_subset_long, aes(x = variable, y = value, colour=variable),
  #           position = position_jitter(width = 0.2), alpha = 0.3) +  # Add points for participants
  theme(legend.position="none",
        plot.title = element_text(size = label_size),
        axis.text.x = element_text(size = label_size),
        axis.text.y = element_text(size = label_size),
        axis.title.y = element_text(size = label_size)) +
  ylab("UEQ - Average Scores\n") + xlab("") 
ueq_plot


# RiCE BOX PLOT
my_data = read_excel("Questionnaire_Data.xlsx", sheet = "data reduced")
rice_subset = my_data[, c("RiCE-Cp",
                         "RiCE-Se",
                         "RiCE-Ex",
                         "RiCE")]

# Reshape the data into long format
rice_subset_long <- gather(rice_subset, key = "variable", value = "value")
rice_subset_long$variable <- factor(rice_subset_long$variable, levels = c(
  "RiCE-Cp",
  "RiCE-Se",
  "RiCE-Ex",
  "RiCE"
))

means <- rice_subset_long %>%
  group_by(variable) %>%
  summarise(mean_value = mean(value))

rice_plot <- rice_subset_long %>%
  ggplot(aes(x=variable, y=value, fill=variable)) +
  geom_boxplot(aes(colour = variable), fill="transparent", outlier.shape = 1) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  scale_y_continuous(breaks = seq(0, 10, by = 1.0), limits=c(0,10)) +  # Set breaks for y-axis
  scale_x_discrete(labels = c(
    "RiCE-Cp" = "RiCEv2-Cp\n(Mean=7.2)\n(Med=7.3)\n(SD=1.3)",
    "RiCE-Se" = "RiCEv2-Se\n(Mean=5.5)\n(Med=5.3)\n(SD=1.7)",
    "RiCE-Ex" = "RiCEv2-Ex\n(Mean=6.8)\n(Med=7.0)\n(SD=1.8)",
    "RiCE" = "RiCEv2\n(Mean=6.5)\n(Med=6.6)\n(SD=1.3)"
  )) +
  theme_minimal() +
  geom_point(data = means, aes(x = variable, y = mean_value, colour=variable), 
             shape = 4, size = 3) +
  #geom_point(data = rice_subset_long, aes(x = variable, y = value, colour=variable),
  #           position = position_jitter(width = 0.2), alpha = 0.3) +  # Add points for participants
  theme(legend.position="none",
        plot.title = element_text(size = label_size),
        axis.text.x = element_text(size = label_size),
        axis.text.y = element_text(size = label_size),
        axis.title.y = element_text(size = label_size)) +
  ylab("RiCEv2 - Average Scores\n") + xlab("") 
rice_plot



# Statistics for RiCEv2 Analysis (see Section X.X) =============================
# Load data 
my_data = read_excel("Questionnaire_Data.xlsx", sheet = "data cleaned")

#Reverse Rice-Ex-3
my_data$"RiCE-Ex-3-reversed" <- 10 - my_data$"RiCE-Ex-3"


# for whole scale
selectedItems = my_data[, c("RiCE-Cp-1", "RiCE-Cp-2", "RiCE-Cp-3", 
                              "RiCE-Se-1", "RiCE-Se-2", "RiCE-Se-3",
                              "RiCE-Ex-1", "RiCE-Ex-2", "RiCE-Ex-3-reversed")]
result = psych::alpha(selectedItems)$total$raw_alpha
signif(result,2)


# for reflection-on-current-process
selectedItems = my_data[, c("RiCE-Cp-1", "RiCE-Cp-2", "RiCE-Cp-3")]
result = psych::alpha(selectedItems)$total$raw_alpha
signif(result,2)
#cor.test(scale_dev$RICE_Q13, scale_dev$RICE_Q35, method=c("spearman"))

# for reflection-on-self
selectedItems = my_data[, c("RiCE-Se-1", "RiCE-Se-2", "RiCE-Se-3")]
result = psych::alpha(selectedItems)$total$raw_alpha
signif(result,2)

# for reflection-through-experimentation
selectedItems = my_data[, c("RiCE-Ex-1", "RiCE-Ex-2", "RiCE-Ex-3-reversed")]
result = psych::alpha(selectedItems)$total$raw_alpha
signif(result,2)


# Alpha for UEQ ================================================================
selectedItems = my_data[, c("AE-S.3",	"AE-S.1",	"AE-S.2",
                           "PU-S.2", "PU-S.3", "PU-S.1",
                           "FA-S.3",	"FA-S.2",	"FA-S.1",		
                           "RW-S.3", "RW-S.2", "RW-S.1")]
result = psych::alpha(selectedItems)$total$raw_alpha
signif(result,2)

selectedItems = my_data[, c("AE-S.3",	"AE-S.1",	"AE-S.2")]
result = psych::alpha(selectedItems)$total$raw_alpha
signif(result,2)

selectedItems = my_data[, c("PU-S.2", "PU-S.3", "PU-S.1")]
result = psych::alpha(selectedItems)$total$raw_alpha
signif(result,1)

selectedItems = my_data[, c("FA-S.3",	"FA-S.2",	"FA-S.1")]
result = psych::alpha(selectedItems)$total$raw_alpha
signif(result,1)

selectedItems = my_data[, c("RW-S.3", "RW-S.2", "RW-S.1")]
result = psych::alpha(selectedItems)$total$raw_alpha
signif(result,1)


# RiCE Correlates SRIS? ========================================================
my_data = read_excel("Questionnaire_Data.xlsx", sheet = "data reduced")
cor.test(my_data$RiCE, my_data$SRIS_Score, method=c("spearman"))


# Statistics for Confounding Variables (see Section X.X) =======================
# Load data 
my_data = read_excel("Questionnaire_Data.xlsx", sheet = "data reduced")

# Considers themselves as a Musician vs Non-Musician
# Mann Whitney U... although named wilcox by r studio
wilcox.test(my_data$UEQ_Engagement~my_data$Musician_Groups, correct = FALSE, exact=FALSE)
wilcox.test(my_data$RiCE~my_data$Musician_Groups, correct = FALSE, exact=FALSE)

# Year of study 
# Mann Whitney U... although named wilcox by r studio
wilcox.test(my_data$UEQ_Engagement~my_data$Year_of_Study, correct = FALSE, exact=FALSE)
wilcox.test(my_data$RiCE~my_data$Year_of_Study, correct = FALSE, exact=FALSE)

# Degree Programme
kruskal.test(my_data$UEQ_Engagement~my_data$Course_of_Study)
kruskal.test(my_data$RiCE~my_data$Course_of_Study)



# Statistics for Engagement & Reflection  (see Section 3.6.1)  =================
## Linear Regression Models ====================================================
# Split the datasets
my_data = read_excel("Questionnaire_Data.xlsx", sheet = "data reduced")

split_data = sample.split(my_data$Age,SplitRatio = 0.6) 
train_data = subset(my_data,split_data == TRUE) 
test_data = subset(my_data,split_data == FALSE) 
nrow(train_data) # Print sizes
nrow(test_data)

#For visualisations
# Custom color palette
colours <- c("UEQ_Focused_Attention" = "#1f78b4", 
             "UEQ_Reward" = "#e31a1c", 
             "UEQ_Aesthetic_Appeal" = "#33a02c",
             "UEQ_Percived_Usability" ="#ff7f00")

### RiCE : Model 1 ====
big_model <- lm(`RiCE` ~ UEQ_Focused_Attention
                + UEQ_Aesthetic_Appeal
                + UEQ_Perceived_Usability
                + UEQ_Reward,
                data=train_data)
summary(big_model)

step(big_model, direction = "backward")

found_model <- lm(formula = RiCE ~ UEQ_Focused_Attention + UEQ_Reward, data = train_data)
summary(found_model)

pred <- predict(found_model, test_data) 
postResample(pred, test_data$`RiCE`)

#### Visualise =====
x_seq <- seq(1, 5, length.out = 10) # Create a sequence of x values from 1 to 5
trend_data <- data.frame( # Create a dataframe for the trend line based on the found_model
  UEQ_Focused_Attention = x_seq,
  UEQ_Reward = x_seq
)
trend_data$predicted_RiCE <- predict(found_model, newdata = trend_data)

equation_text <- "RiCEv2=-3.99+(1.11)Focused_Attention+(1.41)Reward*"  # Replace with your actual equation

# Create the ggplot
p <- ggplot(train_data) +
  geom_point(aes(x = UEQ_Focused_Attention, y = RiCE, color = "UEQ_Focused_Attention", shape = "UEQ_Focused_Attention"), size = 5, stroke = 0.8, fill = "NA") +
  geom_point(aes(x = UEQ_Reward, y = RiCE, color = "UEQ_Reward", shape = "UEQ_Reward"), size = 5, stroke = 0.8, fill = "NA") +
  geom_smooth(data = trend_data, aes(x = (UEQ_Focused_Attention + UEQ_Reward) / 2, y = predicted_RiCE, color = "Model Fit"), method="lm", se=FALSE, color="black") +
  annotate("text", x = 1.45 , y = 0, label = equation_text, hjust = 0, color = "black", angle = 47, size = 2.5) +
  scale_x_continuous(limits = c(1, 5), breaks = seq(1, 5, by = 1)) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(x = "Engagement", y = "Reflection") +
  scale_color_manual(values = colours) +
  scale_shape_manual(values = c("UEQ_Focused_Attention" = 24, 
                                "UEQ_Reward" = 21, 
                                "UEQ_Aesthetic_Appeal" = 23, 
                                "UEQ_Percived_Usability" = 20)) +  # Hollow triangle and circle
  theme_classic() +  
  theme(legend.position = "bottom") +
  labs(color = NULL, shape = NULL) +
  guides(shape = guide_legend(override.aes = list(linetype = 0), nrow = 2, ncol = 2))  # Set linetype to 0 for points in legend
print(p)



### RiCE-Ex : Model 2 ====
big_model <- lm(`RiCE-Ex` ~ UEQ_Focused_Attention
                + UEQ_Aesthetic_Appeal
                + UEQ_Perceived_Usability
                + UEQ_Reward,
                data=train_data)
summary(big_model)

step(big_model, direction = "backward")

found_model <- lm(formula = `RiCE-Ex` ~ UEQ_Aesthetic_Appeal, data = train_data)
summary(found_model)

pred <- predict(found_model, test_data) 
postResample(pred, test_data$`RiCE-Ex`)

#### Visualise ====
x_seq <- seq(1, 5, length.out = 10)  # Create a sequence of x values from 1 to 5
trend_data <- data.frame(UEQ_Aesthetic_Appeal = x_seq)
trend_data$predicted_RiCE_Ex <- predict(found_model, newdata = trend_data)

equation_text <- "RiCEv2-Ex=-4.06+(2.70)Aesthetic_Appeal*"  # Replace with your actual equation

# Create the ggplot
p <- ggplot(train_data) +
  geom_point(aes(x = UEQ_Aesthetic_Appeal, y = `RiCE-Ex`, color = "UEQ_Aesthetic_Appeal", shape = "UEQ_Aesthetic_Appeal"), size = 5, stroke = 0.8, fill = "NA") +
  geom_smooth(data = trend_data, aes(x = UEQ_Aesthetic_Appeal, y = predicted_RiCE_Ex, color = "Model Fit"), method = "lm", se = FALSE, color = "black") +
  annotate("text", x = 1.5, y = 0.4, label = equation_text, hjust = 0, color = "black", angle = 49, size = 2.5) +
  scale_x_continuous(limits = c(1, 5), breaks = seq(1, 5, by = 1)) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(x = "Engagement", y = "Reflection-Through-Experimentation") +
  scale_color_manual(values = colours) +
  scale_shape_manual(values = c("UEQ_Focused_Attention" = 24, 
                                "UEQ_Reward" = 21, 
                                "UEQ_Aesthetic_Appeal" = 23, 
                                "UEQ_Percived_Usability" = 20)) +  # Hollow triangle and circle
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(color = NULL, shape = NULL) +
  guides(shape = guide_legend(override.aes = list(linetype = 0), nrow = 1))
print(p)


### RiCE-Cp : Model 3 ====
big_model <- lm(`RiCE-Cp` ~ UEQ_Focused_Attention
                + UEQ_Aesthetic_Appeal
                + UEQ_Perceived_Usability
                + UEQ_Reward,
                data=train_data)
summary(big_model)

step(big_model, direction = "backward")

found_model <- lm(formula = `RiCE-Cp` ~ UEQ_Focused_Attention + UEQ_Reward + UEQ_Aesthetic_Appeal, data = train_data)
summary(found_model)

pred <- predict(found_model, test_data) 
postResample(pred, test_data$`RiCE-Cp`)

#### Visualise ====
x_seq <- seq(1, 5, length.out = 10) # Create a sequence of x values from 1 to 5
trend_data <- data.frame( # Create a dataframe for the trend line based on the found_model
  UEQ_Focused_Attention = x_seq,
  UEQ_Reward = x_seq,
  UEQ_Aesthetic_Appeal = x_seq
)
trend_data$predicted_RiCE_Cp <- predict(found_model, newdata = trend_data)

equation_text <- "RiCEv2-Cp=-0.82\n                     +(2.10)Focused_Attention*\n                     +(1.07)Reward\n                     -(1.35)Aesthetic_Appeal"  # Replace with your actual equation

# Create the ggplot
p <- ggplot(train_data) +
  geom_point(aes(x = UEQ_Focused_Attention, y = `RiCE-Cp`, color = "UEQ_Focused_Attention", shape = "UEQ_Focused_Attention"), size = 5, stroke = 0.8, fill = "NA") +
  geom_point(aes(x = UEQ_Reward, y = `RiCE-Cp`, color = "UEQ_Reward", shape = "UEQ_Reward"), size = 5, stroke = 0.8, fill = "NA") +
  geom_point(aes(x = UEQ_Aesthetic_Appeal, y = `RiCE-Cp`, color = "UEQ_Aesthetic_Appeal", shape = "UEQ_Aesthetic_Appeal"), size = 5, stroke = 0.8, fill = "NA") +
  geom_smooth(data = trend_data, aes(x = (UEQ_Focused_Attention + UEQ_Reward + UEQ_Aesthetic_Appeal) / 3, y = predicted_RiCE_Cp, color = "Model Fit"), method="lm", se=FALSE, color="black") +
  annotate("text", x = 1.3, y = 0.4, label = equation_text, hjust = 0, color = "black", angle = 38, size = 2.5) +
  scale_x_continuous(limits = c(1, 5), breaks = seq(1, 5, by = 1)) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(x = "Engagement", y = "Reflection-on-Process") +
  scale_color_manual(values = colours) +
  scale_shape_manual(values = c("UEQ_Focused_Attention" = 24, 
                                "UEQ_Reward" = 21, 
                                "UEQ_Aesthetic_Appeal" = 23, 
                                "UEQ_Percived_Usability" = 20)) +  # Hollow triangle and circle
  theme_classic() +  
  theme(legend.position = "bottom", legend.text = element_text(size = 7)) +
  labs(color = NULL, shape = NULL) +
  guides(shape = guide_legend(override.aes = list(linetype = 0), nrow = 2, ncol = 2))  # Set linetype to 0 for points in legend
print(p)


### RiCE-Se : Model 4 ====
big_model <- lm(`RiCE-Se` ~ UEQ_Focused_Attention
                + UEQ_Aesthetic_Appeal
                + UEQ_Perceived_Usability
                + UEQ_Reward,
                data=train_data)
summary(big_model)

step(big_model, direction = "backward")

found_model <- lm(formula = `RiCE-Se` ~ UEQ_Reward, 
                  data = train_data)
summary(found_model)

pred <- predict(found_model, test_data) 
postResample(pred, test_data$`RiCE-Se`)

#### Visualise ====
x_seq <- seq(1, 5, length.out = 10)  # Create a sequence of x values from 1 to 5
trend_data <- data.frame(UEQ_Reward = x_seq)
trend_data$predicted_RiCE_Se <- predict(found_model, newdata = trend_data)

# Create the ggplot
p <- ggplot(train_data) +
  geom_point(aes(x = UEQ_Reward, y = `RiCE-Se`, color = "UEQ_Reward", shape = "UEQ_Reward"), size = 5, stroke = 0.8, fill = "NA") +
  geom_smooth(data = trend_data, aes(x = UEQ_Reward, y = predicted_RiCE_Se, color = "Model Fit"), method = "lm", se = FALSE, color = "black") +
  annotate("text", x = 2.55, y = 0.4, label = "RiCEv2-Se=-7.09+(1.94)Reward*", hjust = 0, color = "black", angle = 53, size = 2.5) +
  scale_x_continuous(limits = c(1, 5), breaks = seq(1, 5, by = 1)) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(x = "Engagement", y = "Reflection-on-Self") +
  scale_color_manual(values = colours) +
  scale_shape_manual(values = c("UEQ_Focused_Attention" = 24, 
                                "UEQ_Reward" = 21, 
                                "UEQ_Aesthetic_Appeal" = 23, 
                                "UEQ_Percived_Usability" = 20)) +  # Hollow triangle and circle  theme_classic() +
  theme_classic() +  
  theme(legend.position = "bottom") +
  labs(color = NULL, shape = NULL) +
  guides(shape = guide_legend(override.aes = list(linetype = 0), nrow = 1))
print(p)


