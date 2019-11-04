#==================================================#==================================================

# strip plot / a reproducible example of advanced data visualization 

#==================================================#==================================================

# load libraries
library(ggplot2)
library(gridExtra)
library(ggExtra)

#==================================================

# set wd
setwd() # change wd to the folder where the data is (csv-file)

# read data
t1 <- read.csv(file = "strip_plot_data.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", na.string ="99")
t1.df <- data.frame(t1)
is.na(t1.df)

# remove NAs
t1.df$class <- na.omit(t1.df$class) 
t1.df$school <- na.omit(t1.df$school) 

# recode variable class
oldvalues <- c("fünf", "sechs", "sieben", "acht", "neun", "zehn")
newvalues <- factor(c("05th","06th", "07th", "08th", "09th", "10th"))  # Make this a factor
t1.df$class <- newvalues[ match(t1.df$class, oldvalues) ]

# recode variable school
oldvalues <- c("gym", "real", "mittel")
newvalues <- factor(c("school type 1","school type 2", "school type 3"))  # Make this a factor
t1.df$school <- newvalues[ match(t1.df$school, oldvalues) ]

#==================================================

# print strip plot
setwd() # set working directory to the folder where you want to save the png-file

png(filename="strip_plot.png", width=1024, height=1024*0.50, units="px") # change size of plot
par(mfrow=c(1,1))

ggplot(data = t1.df, aes(x = stud, y = class, color = school, fill = school)) +
  
  # shape = point / change size or alpha level / data points are slightly jittered to avoid overplotting
  geom_point(size = 7, alpha = .7, shape = 21, position=position_jitter(width=.3,height=0), show.legend = TRUE) +
  
  # theme = classic
  theme_classic() +
  
  # modify titles
  labs(x = "number of students per class", y = "class level", title = "Distribution of students over class levels and school types") + # add subtitle with , subtitle = ""
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text = element_text(size=15),
        title = element_text(size=30)) +
 
   # modify legend elements
  theme(legend.position=c(0.1, 0.75), 
        legend.title = element_blank(),  
        legend.text = element_text(size=16)) +
  
  # modify axis elements
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line.y = element_blank()) +
  
  # modify theme elements
  theme(panel.grid.major.y = element_line(colour = "grey70")) +
  
  # modify colors
  scale_color_manual(values = c("tomato", "springgreen3", "royalblue2")) +
  scale_size(guide=FALSE) + # remove parts of legend
  scale_alpha(guide=FALSE) +
  
  xlim(0,35) # limit x axis from 0 to 35

dev.off()

#==================================================#==================================================
