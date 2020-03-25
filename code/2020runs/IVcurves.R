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
# library(latex2exp)  # LaTeX expressions
#---------------------------------------


#---------------------------------------
# Setup: user-defined parameters
#---------------------------------------
# Project directory (relative to source location)
#PROJECT <- "SINGLE"
#PROJECT <- "PULS0250"
#PROJECT <- "PULS0125"
PROJECT <- "PULS0050"

# The data file contains the sizes of the steps observed in each calculated 
# IV curve, i.e. for each alpha_rf value;
#datafile <- "SI010.out"
#datafile <- "PU010.out"
#datafile <- "PU010.out"
datafiles <- c("PU001.out", "PU011.out", "PU019.out", "PU029.out", "PU037.out", "PU047.out", "PU055.out")

header <- 32        # number of header lines to skip

omega <- 0.45
#---------------------------------------


#---------------------------------------
# OS-independent constants
#---------------------------------------
SEPARATOR <- .Platform$file.sep
#---------------------------------------


#---------------------------------------
# Functions
#---------------------------------------
rm_extension <- function(f){
  name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(f))
  return(name)
}
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

iv_curves <- data.frame()

for (f in datafiles) {
  colprefix <- rm_extension(f)

  input_file <- file.path(INPUT, f)
  df <- read.csv(file = input_file, sep = ",", dec = ".", as.is = TRUE,
                 header = TRUE, skip = header, col.names = paste0(colprefix, ".", c("eta", "alpha")))

  if (dim(iv_curves)[2] == 0) {
    iv_curves <- df
  } else {
    iv_curves <- cbind(iv_curves, df)
  }
}

datafiles <- c("PU001.out", "PU011.out", "PU019.out", "PU029.out", "PU037.out", "PU047.out", "PU055.out")
colfunc <- colorRampPalette(c("slategray", "darkslategray"))
cols <- c("royalblue", colfunc(6))

lsize = 0.75
psize = 1.25
ggplot(data = iv_curves) +
  geom_vline(xintercept = 0,  linetype = "11", size = lsize, colour = "black", alpha = 0.75) +
  geom_vline(xintercept = 4,  linetype = "11", size = lsize, colour = "black", alpha = 0.75) +
  geom_vline(xintercept = 8,  linetype = "11", size = lsize, colour = "black", alpha = 0.75) +
  geom_vline(xintercept = 12, linetype = "11", size = lsize, colour = "black", alpha = 0.75) +
  geom_vline(xintercept = 16, linetype = "11", size = lsize, colour = "black", alpha = 0.75) +
  geom_vline(xintercept = 20, linetype = "11", size = lsize, colour = "black", alpha = 0.75) +
  geom_vline(xintercept = 24, linetype = "11", size = lsize, colour = "black", alpha = 0.75) +
  geom_point(aes(x = PU001.eta / omega,      y = PU001.alpha), size = psize, colour = cols[1]) +
  geom_point(aes(x = PU011.eta / omega + 4,  y = PU011.alpha), size = psize, colour = cols[2]) +
  geom_point(aes(x = PU019.eta / omega + 8,  y = PU019.alpha), size = psize, colour = cols[3]) +
  geom_point(aes(x = PU029.eta / omega + 12, y = PU029.alpha), size = psize, colour = cols[4]) +
  geom_point(aes(x = PU037.eta / omega + 16, y = PU037.alpha), size = psize, colour = cols[5]) +
  geom_point(aes(x = PU047.eta / omega + 20, y = PU047.alpha), size = psize, colour = cols[6]) +
  geom_point(aes(x = PU055.eta / omega + 24, y = PU055.alpha), size = psize, colour = cols[7]) +
  # geom_segment(aes(x = 0.0, y = 5.0, xend = 0.0, yend = 4.0), colour = "red", size = 0.2, arrow = arrow(length = unit(0.5, "cm"), type = "open")) + 
  # geom_segment(aes(x = 4.0, y = 5.0, xend = 4.0, yend = 4.0), colour = "red", size = 0.2, arrow = arrow(length = unit(0.5, "cm"), type = "closed")) + 
  # geom_segment(aes(x = 8.0, y = -5.0, xend = 8.0, yend = -3.5), colour = "red", size = 0.2, arrow = arrow(length = unit(0.5, "cm"), type = "closed")) + 
  # geom_segment(aes(x = 0.0, y = -1.5, xend = 0.0, yend = 1.5), colour = "red", size = 0.2) + 
  scale_x_continuous(name = "eta / omega", limits = c(-10.0, 34.0), breaks = seq(0.0, 24.0, 4.0), minor_breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous(name = "alpha", limits = c(-5.0, 5.0), breaks = seq(-4.0, 4.0, 1.0), minor_breaks = NULL, expand = c(0, 0)) +
  geom_label(x =  8.5, y = 4,   label = "0",  size = rel(5), fontface = "bold", label.size = NA) +
  geom_label(x = 13.1, y = 4,   label = "5",  size = rel(5), fontface = "bold", label.size = NA) +
  geom_label(x = 17.5, y = 4,   label = "9",  size = rel(5), fontface = "bold", label.size = NA) +
  geom_label(x = 22.0, y = 4,   label = "14", size = rel(5), fontface = "bold", label.size = NA) +
  geom_label(x = 26.6, y = 4,   label = "18", size = rel(5), fontface = "bold", label.size = NA) +
  geom_label(x = 31.2, y = 4,   label = "23", size = rel(5), fontface = "bold", label.size = NA) +
  geom_label(x = 32.6, y = 2.7, label = "27", size = rel(5), fontface = "bold", label.size = NA) +
  theme_bw() +
  theme(#aspect.ratio = 3/4,
        plot.margin = unit(c(5, 5, 5, 5), "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.title = element_text(size = rel(1.75)),
        axis.text = element_text(size = rel(1.75), face = "bold", colour = "black"),
        axis.text.x = element_text(margin = unit(c(20, 20, 20, 20), "pt")), 
        axis.text.y = element_text(margin = unit(c(20, 20, 20, 20), "pt")),
        axis.ticks.length = unit(-10, "pt"),
  )


plotname <- paste(PROJECT, "-IV.pdf", sep = "")
plotfile <- file.path(PLOTS, plotname)
ggsave(plotfile, device = "pdf", width = 22, height = 15, units = "cm", dpi = 300)





# steps <- subset(df, select=X000:X004)
# steps$alpharf <- (as.numeric(rownames(steps)) - 1) * alpharf_step
# head(steps)
# 
# long_steps <- gather(steps, key = "stepnumber", value = "size", -alpharf)
# head(long_steps)
# 
# 
# step_nums <- c("n = 0", "n = 1", "n = 2", "n = 3", "n = 4")
# names(step_nums) <- c("X000", "X001", "X002", "X003", "X004")
# ggplot(data = long_steps, aes(x = alpharf, y = size)) +
#   geom_point(colour = "black", size = 1.0) +
#   scale_x_continuous(limits = c(0.0, 50.0), breaks = seq(0.0, 50.0, 10.0)) +
#   scale_y_continuous(limits = c(0.0, 2.0), minor_breaks = seq(0.0, 2.0, 0.5)) +
#   facet_grid(stepnumber ~ ., labeller = labeller(stepnumber = step_nums)) +
#   theme_bw()
# 
# 
