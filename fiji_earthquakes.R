#Fiji Earthquake Analysis #####
#Daniel Nystrom - wwww.github.com/DanielANystrom
#This script was created to analyze the quakes
#data set to demonstrate various data analysis 
#techniques as well as the use of ggplot graphs.
#July 29, 2020

#Step 1 -- Import the necessary packages and data #####

#Load packages via the pacman p_load function
if(!require(pacman)) install.packages("pacman")
pacman::p_load(pacman, rio, psych, datasets, tidyverse,
               GGally, hexbin) #reduces endless lines of library()

#Background information on the dataset itself
?quakes
head(quakes)

#Import and export data
df <- quakes %>%
  as_tibble() %>%
  print()
write.table(df, file = "fiji.csv") #for use in Tableau

#Step 2 -- Descriptive statistics #####

#Run numeric summaries
summary(df)
describe(df)

#Although it IS possible to create individual
#histograms in ggplot (as below for "depth"):
hist <- ggplot(df, aes(depth)) + #call the variable
  geom_histogram(binwidth = 10, #specify the geometric type
                 fill="cyan3",
                 color="cyan4") +
  labs(title = "Histogram of 'Depth'", #add the labels
       caption = "Data credited to 'quakes' data from the R 'datasets' package") +
  theme(plot.title = element_text(face = "bold", color = "black", size = 17),
        panel.background = element_rect(fill = "grey90", color = "black",
                                         size = 1, linetype = "solid")) + #and edit text
  xlab("Depth (km)") + #change the x axis label
  ylab("Count") #and the y axis
print(hist) #call the figure

#And then run a correlation matrix separately:
matrix <- cor(df)
round(matrix,2)

#I'm finding that it's a bit tidier to run a scatterplot
#matrix in ggplot, with the correlations and density plot
#built in, which feels more succinct -- however, to change
#the column names, I had to declare another variable "label"
#as a vector of strings to get ggpairs() to run without errors:
label <- c("Latitude","Longitude","Depth","Magnitude","Stations") #vector for column names
scatter <- ggpairs(df, #initiate ggpairs for df
              columnLabels = label) + #and re-label columns
  labs(title="Scatterplot Matrix for Fiji Data", #add titles and caption
       subtitle="Scatterplots, density plot, and correlation coefficients",
       caption = "Data credited to 'quakes' data from the R 'datasets' package") +
  theme(plot.title = element_text(face = "bold", color = "black", size = 17), #customize the fonts
        plot.subtitle = element_text(face = "italic", color = "black", size = 14),
        panel.background = element_rect(fill="grey90", color = "black",
                                        size = 1, linetype = "solid")) #add block boundaries
print(scatter)
#Still looking for a way to change the color of the points

#Step 3 -- Statistical analysis ####

#First thing that pops out is the correlation between
#magnitude and stations, so let's explore that first.
#running a simple regression model:
lm1 <- lm(data = df, stations ~ mag)
extractAIC(lm1)
summary(lm1) %>%
  print()

#As well as a visualization of the regression line
viz1 <- ggplot(df, aes(stations, mag)) + #call variables
  geom_point(position="jitter", color = "cyan4", size = 3) + #specify geom type
  geom_smooth(method = "lm", level = 0.95, color = "red") + #add regression line
  labs(title = "Relationship Between Magnitude and Stations", #add titles
    subtitle = "Strong positive correlation with regression line and 95% confidence interval",
    caption = "Data credited to 'quakes' data from the R 'datasets' package") +
  theme(plot.title = element_text(face = "bold", color = "black", size = 17), #edit typeface
    plot.subtitle = element_text(face = "italic", color = "black", size = 14),
    panel.background = element_rect(fill = "grey90", color = "black",
                                     size = 1, linetype = "solid")) +
  xlab("Stations Recording the Earthquake") +  #x axis label
  ylab("Magnitude of Earthquake") #y axis label
viz1 <- viz1 + scale_x_continuous(breaks=seq(0,150,15)) #edit the graph lines
print(viz1)

#Let's feature of interest is the mag ~ depth relationship
#we saw from the density plot, depth seems to be bimodal
#and, indeed, for mag ~ depth and mag ~ stations there is
#only the slightest (though significant) negative correlation
#mag ~ depth linear model:
lm2 <- lm(data = df, mag ~ depth)
extractAIC(lm2)
summary(lm2) %>%
  print()

#And visualization
viz2 <- ggplot(df, aes(depth, mag)) +
  geom_point(position="jitter", color = "cyan4", size = 3) +
  geom_smooth(method = "lm", level = 0.95, color = "red") +
  theme(plot.title = element_text(face = "bold", color = "black", size = 17),
        plot.subtitle = element_text(face = "italic", color = "black", size = 14),
        panel.background = element_rect(fill = "grey90", color = "black",
                                         size = 1, linetype = "solid")) +
  labs(title = "Relationship Between Magnitude and Depth",
       subtitle = "Slight negative correlation with regreesion line and 95% confidence interval",
       caption = "Data credited to 'quakes' data from the R 'datasets' package") +
  xlab("Depth of the Earthquake (km)") + 
  ylab("Magnitude of Earthquake")
viz2 <- viz2 + scale_x_continuous(breaks=seq(0,700,75))
print(viz2)

#And the stations ~ depth linear model:
#visualization not include, but looks essentially
#identical to viz2
lm3 <- lm(data = df, stations ~ depth)
extractAIC(lm3)
summary(lm3) %>%
  print()

#But, when we look at how depth relates to longitude
#there seems to be a bit more of a relationship
#suggesting that the deepest earthquakes tend to
#be located in a similar longitude range
viz4 <- ggplot(df, aes(depth, long)) +
  geom_point(position="jitter", color = "cyan4", size = 3) +
  theme(plot.title = element_text(face = "bold", color = "black", size = 17),
        plot.subtitle = element_text(face = "italic", color = "black", size = 14),
        panel.background = element_rect(fill = "grey90", color = "black",
                                         size = 1, linetype = "solid")) +
  labs(title = "Relationship Between Longitude and Depth",
       subtitle = "Maybe a sign of two clusters for depth?",
       caption = "Data credited to 'quakes' data from the R 'datasets' package") +
  xlab("Depth of the Earthquake (km)") + 
  ylab("Longitude of Epicenter")
viz4 <- viz4 + scale_x_continuous(breaks=seq(0,700,75))
print(viz4)

#To explore the idea of two "clusters" for deep and shallow
#quakes I first create a new variable for the dataset
#for all quakes deeper than 311.4km (the mean)
df2 <- mutate(df, deep1 = ifelse(depth>311.4,1,0)) %>%
  as_tibble() %>%
  print() #add the variable, but not a factor
deep <- factor(df2$deep1) #make a factor out of it
df2$deep <- deep #add the factor variable
df2$deep1 <- NULL #drop the intermediary non-factor
print(df2) #there has to be a better way to do this

#Then you can run a simple t-test and find that
#the two groups have significantly different means
#An anova would work just as well.
t <- t.test(stations ~ deep, data = df2, var.equal=TRUE)
t

#And then the final visualization
bxplt1 <- ggplot(df2, aes(group = deep, y = stations, color = deep)) +
  scale_color_manual(values = c("cyan4","salmon4")) + #set the colors for the categories
  geom_boxplot(outlier.color = "black", outlier.shape = 8, #customize the boxplot
               outlier.size = 2.5, notch = TRUE, #I kind of like the notches for this
               notchwidth = 0.8, show.legend = TRUE, #default for show.legened is FALSE
               size = 1, fill = c("cyan3","salmon3")) + #fill is for the actual insidd
  theme(plot.title = element_text(face = "bold", color = "black", size = 17), #edit the text
         plot.subtitle = element_text(face = "italic", color = "black", size = 14),
         panel.background = element_rect(fill = "grey90", color = "black",
                                         size = 1, linetype = "solid"),
        legend.title = element_text(face = "bold", size = 15),
        legend.text = element_text(face = "bold", size = 12)) +
    labs(title = "Shallow Quakes more Noticeable than Deep", #change the headers
       subtitle = "Based on the number of stations recording each earthquake",
       caption = "Data credited to 'quakes' data from the R 'datasets' package") +
  ylab("Number of Stations Recording Earthquake") +
  xlab("Shallow versus Deep Quakes")
print(bxplt1)

#End of script