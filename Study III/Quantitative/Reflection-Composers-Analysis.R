# Preamble ====================================================================

install.packages("readxl")
install.packages("psych")
library("readxl")
library("reshape2")
library("ggplot2")
library("psych")


# Setup R to read current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(scipen=999)

#====================================================================================

my_data = read_excel("Autethnography Reflection Study Questionnaire Responses.xlsx",
                     sheet = "self", col_names = TRUE,  range = "G1:N8")

# Calculate the average for each session
my_data$Average <- rowMeans(my_data[, -1])
#my_data$Average <- apply(my_data[, -1], 1, median)

# Reshape data for ggplot2
df_melted <- melt(my_data, id.vars = c("Session", "Average"))
df_melted <- na.omit(df_melted)
#df_melted <- df_melted[!(df_melted$variable %in% c("P6", "P7")), ]


df_melted$group <- ifelse(df_melted$variable %in% c("P6", "P7"), 1, 0)
df_melted

colours <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#ff99ff", "#DAA520", "black", "blue")

# data for trend line
new_data <- data.frame(
  Session = 1:4,
  variable = rep("Trend D: P6 & P7 Average", times = 4),
  value = c(4.35, 6.00, 8.67, 5.67)
)
new_data2 <- data.frame(
  Session = 1:4,
  variable = rep("Trend C: P1 to P5 Average", times = 4),
  value = c(5.19, 6.47, 4.80, 6.25)
)


# Plotting
ggplot(df_melted, aes(x = Session, y = value, color = variable, shape = variable, group=variable)) +
  geom_point(size = 5, stroke=.8) +
  geom_line(aes(x = Session, y = value, color = variable), alpha = 0.5, size = 0.3, linetype="dotted") +  # Match line color to shapes
  geom_smooth(data = new_data, aes(x = Session, y = value, color = variable), method="loess", se=FALSE) +
  geom_smooth(data = new_data2, aes(x = Session, y = value, color = variable), method="loess", se=FALSE) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(x = "Session", y = "Reflection on Self",) +
  scale_color_manual(values = colours) +
  scale_shape_manual(values = c(7,2,9,4,5,8,1,0,0)) +
  theme_classic() +  
  theme(legend.position="bottom") +
  labs(color = NULL, shape = NULL) +
  guides(shape = guide_legend(override.aes = list(linetype = 0), nrow = 2, ncol=2))  # Set linetype to 0 for lines in legend

# Export 6 by 8

#===============================================================================

#PROCCESS


my_data = read_excel("Autethnography Reflection Study Questionnaire Responses.xlsx",
                     sheet = "process", col_names = TRUE,  range = "G1:N8")

# Calculate the average for each session
my_data$Average <- rowMeans(my_data[, -1])
#my_data$Average <- apply(my_data[, -1], 1, median)

# Reshape data for ggplot2
df_melted <- melt(my_data, id.vars = c("Session", "Average"))
df_melted <- na.omit(df_melted)
#df_melted <- df_melted[!(df_melted$variable %in% c("P6", "P7")), ]


df_melted$group <- ifelse(df_melted$variable %in% c("P6", "P7"), 1, 0)
df_melted

colours <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#ff99ff", "#DAA520", "black", "blue")

# data for trend line
new_data2 <- data.frame(
  Session = 1:4,
  variable = rep("Trend B: P2, P5 & P6 Average", times = 4),
  value = c(9.33, 9.33, 8.22, 6.77)
)
new_data <- data.frame(
  Session = 1:4,
  variable = rep("Trend A: P1, P3 & P4 Average", times = 4),
  value = c(8.00,
            6.56,
            8.11,
            7.22)
)


# Plotting
ggplot(df_melted, aes(x = Session, y = value, color = variable, shape = variable, group=variable)) +
  geom_point(size = 5.0, stroke=.8) +
  geom_line(aes(x = Session, y = value, color = variable), alpha = 0.5, size = 0.3, linetype="dotted") +  # Match line color to shapes
  geom_smooth(data = new_data, aes(x = Session, y = value, color = variable), method="loess", se=FALSE) +
  geom_smooth(data = new_data2, aes(x = Session, y = value, color = variable), method="loess", se=FALSE) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(x = "Session", y = "Reflection on Process",) +
  scale_color_manual(values = colours) +
  scale_shape_manual(values = c(7,2,9,4,5,8,1,0,0)) +
  theme_classic() +  
  theme(legend.position="bottom") +
  labs(color = NULL, shape = NULL) +
  guides(shape = guide_legend(override.aes = list(linetype = 0), nrow = 2, ncol=2))  # Set linetype to 0 for lines in legend

# Export 6 by 8


#===============================================================================


#PROCCESS


my_data = read_excel("Autethnography Reflection Study Questionnaire Responses.xlsx",
                     sheet = "experimentation", col_names = TRUE,  range = "G1:N8")

# Calculate the average for each session
my_data$Average <- rowMeans(my_data[, -1])
#my_data$Average <- apply(my_data[, -1], 1, median)

# Reshape data for ggplot2
df_melted <- melt(my_data, id.vars = c("Session", "Average"))
df_melted <- na.omit(df_melted)
#df_melted <- df_melted[!(df_melted$variable %in% c("P6", "P7")), ]


df_melted$group <- ifelse(df_melted$variable %in% c("P6", "P7"), 1, 0)
df_melted

colours <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#ff99ff", "#DAA520", "blue", "red")

# data for trend line
new_data <- data.frame(
  Session = 1:4,
  variable = rep("Trend E: All Composers Average", times = 4),
  value = c(8.04,7.29,5.95,5.04)
)
#new_data2 <- data.frame(
 # Session = 1:4,
  #variable = rep("Trend: P1 through P5 Average", times = 4),
  #value = c(5.19, 6.47, 4.80, 6.25)
#)


# Plotting
ggplot(df_melted, aes(x = Session, y = value, color = variable, shape = variable, group=variable)) +
  geom_point(size = 5, stroke=.8) +
  geom_line(aes(x = Session, y = value, color = variable), alpha = 0.5, size = 0.3, linetype="dotted") +  # Match line color to shapes
  geom_smooth(data = new_data, aes(x = Session, y = value, color = variable), method="lm", se=FALSE) +
  #geom_smooth(data = new_data2, aes(x = Session, y = value, color = variable), method="loess", se=FALSE) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(x = "Session", y = "Reflection through Experimentation",) +
  scale_color_manual(values = colours) +
  scale_shape_manual(values = c(7,2,9,4,5,8,1,0,0)) +
  theme_classic() +  
  theme(legend.position="bottom") +
  labs(color = NULL, shape = NULL) +
  guides(shape = guide_legend(override.aes = list(linetype = 0), nrow = 2, ncol=2))  # Set linetype to 0 for lines in legend

# Export 6 by 8

#===============================================================================

#Combined Patterns


my_data = read_excel("Autethnography Reflection Study Questionnaire Responses.xlsx",
                     sheet = "experimentation", col_names = TRUE,  range = "G1:N8")

# Calculate the average for each session
my_data$Average <- rowMeans(my_data[, -1])
#my_data$Average <- apply(my_data[, -1], 1, median)

# Reshape data for ggplot2
df_melted <- melt(my_data, id.vars = c("Session", "Average"))
df_melted <- na.omit(df_melted)
#df_melted <- df_melted[!(df_melted$variable %in% c("P6", "P7")), ]


df_melted$group <- ifelse(df_melted$variable %in% c("P6", "P7"), 1, 0)
df_melted

colours <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#ff99ff", "#DAA520", "blue", "red")

# data for trend line
new_data <- data.frame(
  Session = 1:4,
  variable = rep("Trend E: All Composers Average", times = 4),
  value = c(8.04,7.29,5.95,5.04)
)
#new_data2 <- data.frame(
# Session = 1:4,
#variable = rep("Trend: P1 through P5 Average", times = 4),
#value = c(5.19, 6.47, 4.80, 6.25)
#)


# Plotting
ggplot(df_melted, aes(x = Session, y = value, color = variable, shape = variable, group=variable)) +
  geom_point(size = 5, stroke=.8) +
  geom_line(aes(x = Session, y = value, color = variable), alpha = 0.5, size = 0.3, linetype="dotted") +  # Match line color to shapes
  geom_smooth(data = new_data, aes(x = Session, y = value, color = variable), method="lm", se=FALSE) +
  #geom_smooth(data = new_data2, aes(x = Session, y = value, color = variable), method="loess", se=FALSE) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(x = "Session", y = "Reflection through Experimentation",) +
  scale_color_manual(values = colours) +
  scale_shape_manual(values = c(7,2,9,4,5,8,1,0,0)) +
  theme_classic() +  
  theme(legend.position="bottom") +
  labs(color = NULL, shape = NULL) +
  guides(shape = guide_legend(override.aes = list(linetype = 0), nrow = 2, ncol=2))  # Set linetype to 0 for lines in legend

# Export 6 by 8


