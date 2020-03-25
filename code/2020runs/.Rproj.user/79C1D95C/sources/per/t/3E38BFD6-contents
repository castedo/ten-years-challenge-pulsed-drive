#---------------------------------------
# Clear the RStudio environment
#---------------------------------------

# clear workspace, console and plots if script is run in Rstudio
if (Sys.getenv("RSTUDIO") == "1") {
  rm(list = ls())     # clear workspace
  cat("\014")         # clear console
  dev.off()           # clear plots
}
#---------------------------------------


#---------------------------------------
# Load the required libraries
#---------------------------------------
## library(readxl)  # read excel files (much faster, needs full check!)

# library(stringr)    # common string operations
# 
# library(plyr)       # splitting and combining data
library(tidyr)      # reshape data sets
library(dplyr)      # data manipulation (for "glimpse")
# library(broom)      # convert objects into tidy data frames
library(ggplot2)    # further info needed?
# library(scales)     # for 'comma' option in ggplot2
# library(plotly)     # interactive plots
#---------------------------------------


#---------------------------------------
# Setup: user-defined parameters
#---------------------------------------
# Project directory (relative to source location)
#PROJECT <- "SINGLE"
#PROJECT <- "SINGLE2"
#PROJECT <- "SINGLE-BETA1"
#PROJECT <- "PULS0250"
#PROJECT <- "PULS0125"
#PROJECT <- "PULS0050"
PROJECT <- "PULS0050-BETA1"

# The data file contains the sizes of the steps observed in each calculated 
# IV curve, i.e. for each alpha_rf value;
#datafile <- "si.stp"
#datafile <- "pu0250.stp"
#datafile <- "pu0125.stp"
datafile <- "pu0050.stp"

#header <- 65        # number of header lines to skip,  50 data points
header <- 114        # number of header lines to skip, 100 data points

alpharf_step_min <- 0.0
alpharf_step_max <- 50.0
alpharf_step <- 0.5

plot_tag = "(b)"
#---------------------------------------


#---------------------------------------
# OS-independent constants
#---------------------------------------
SEPARATOR <- .Platform$file.sep
#---------------------------------------

#---------------------------------------
# Paths
#---------------------------------------
# Define the current directory
# (i.e., the directory that contains this script)
SRC  <- getwd()

# directory containing the data files (must already exist!)
INPUT <- file.path(SRC, PROJECT)

# directory for the output files
OUTPUT <- file.path(SRC, PROJECT)

# directories for reports and plots
REPORTS <- file.path(SRC, PROJECT)
PLOTS <- file.path(SRC, PROJECT)
#---------------------------------------


#---------------------------------------
# Read the data files containing precipitation and temperature data
#---------------------------------------
input_file <- file.path(INPUT, datafile)
df <- read.csv(file = input_file, sep = ",", dec = ".", as.is = TRUE,
               header = TRUE, skip = header)

head(df)

steps <- subset(df, select=X000:X004)
steps$alpharf <- (as.numeric(rownames(steps)) - 1) * alpharf_step
head(steps)

long_steps <- gather(steps, key = "stepnumber", value = "size", -alpharf)
head(long_steps)


step_nums <- c("n = 0", "n = 1", "n = 2", "n = 3", "n = 4")
names(step_nums) <- c("X000", "X001", "X002", "X003", "X004")
ggplot(data = long_steps, aes(x = alpharf, y = size)) +
  geom_point(colour = "black", size = 0.5) +
  scale_x_continuous(name = "alpha_rf", limits = c(alpharf_step_min, alpharf_step_max), breaks = seq(alpharf_step_min, alpharf_step_max, alpharf_step_max/5.0)) +
  scale_y_continuous(name = "step size", limits = c(0.0, 2.0), breaks = seq(0.0, 2.0, 1.0), minor_breaks = seq(0.0, 2.0, 0.5)) +
  facet_grid(stepnumber ~ ., labeller = labeller(stepnumber = step_nums)) +
  labs(tag = plot_tag) +
  theme_bw() + 
  theme(plot.tag.position = "topleft", 
        plot.tag = element_text(size = rel(2.0), face = "bold"), 
        axis.title = element_text(size = rel(1.75)),
        axis.text = element_text(size = rel(1.75)),
        strip.text.y = element_text(size = rel(1.0)),
        panel.spacing = unit(1.0, "lines"))

plotname <- paste(PROJECT, ".pdf", sep = "")
plotfile <- file.path(PLOTS, plotname)
ggsave(plotfile, device = "pdf", width = 15, height = 22, units = "cm", dpi = 300)



