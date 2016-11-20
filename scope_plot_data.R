#!/usr/bin/Rscript

## Plot oscilloscope data saved by the Perl script scope_get_data

## Output is a HTML file with an embedded graphic, other formats will be added

## ------------------

## Copyright (C) 2016  Jon Meek

## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

## $Id: scope_plot_data.R,v 1.6 2016/11/20 19:11:13 meekj Exp $

## Example command line:
##  scope_plot_data.R --outdir /n2/r-reports --datadir ~/temp/scope --file test4-20161120035025.dat

## TODO:

# Plot mupliple files
# Output options: PDF, PNG, etc

suppressMessages(library(ggplot2)) # Install the tidyverse packege
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

suppressMessages(library(knitr))  # Install these packages
suppressMessages(library(docopt))


doc <- "Usage: scope_plot_data.R [--help --datadir <datadir> --outdir <outdir> --file <file> --note <note>]

-h --help           Show this help text
--datadir <datadir> Data directory
--outdir <outdir>   Report directory, default is data directory
--file <file>       Name of the test, filename
--note <note>       Optional note to describe the test
"
opt <- docopt(doc = doc, strict = FALSE, quoted_args = TRUE)

DataDir  <- opt[["datadir"]]
OutDir   <- opt[["outdir"]]
FileName <- opt[["file"]]
TestNote <- opt[["note"]]

if (is.null(opt[["outdir"]])) { # --outdir not specified, so put report in the data directory
    OutDir <- DataDir
}
OutFile <- paste(OutDir, '/', gsub('.dat', '', FileName), '.html', sep = '') # Don't include '.dat' in output filename

setwd(OutDir) # This will put the 'figure' directory with temporary image files in the same directory as the output
              # But be careful with running other programs that might write to the same directory at the same time


PointSize <- 4     # This needs to increase if Figure{Width, Height} are increased

FigureWidth  <- 20 # For knitr
FigureHeight <- 10

theme_jm1 <- theme_bw() + # Theme for decent HTML, especially when copying / pasting graphics into some word processors
    theme(
        plot.title  = element_text(size = rel(1.5), family = 'Helvetica', face = 'bold'),
        axis.title  = element_text(size = rel(1.5), colour = "black", face = 'bold'),
        axis.text.x = element_text(size = rel(1.5), lineheight = 0.9, colour = "black", vjust = 1, face = 'bold'),
        axis.text.y = element_text(size = rel(1.5), lineheight = 0.9, colour = "black", hjust = 1, face = 'bold'),
        strip.text.y = element_text(size = rel(1.7), colour = "black", face = 'bold'),
        legend.text = element_text(size = rel(1.3))
    )

Sys.setenv(TZ="UTC")

File <- paste(DataDir, FileName, sep = '/')

HeaderComment <- ''

HeaderLines <- 7                       # With comment line, including data header
hl <- readLines(File, n = HeaderLines) # Read header to determine if comment line is present

HeaderInstrumentID <- hl[1] # First five lines are the same with, or without, the comment
HeaderFilename     <- hl[2]
HeaderDateTime     <- hl[3]
HeaderChannelList  <- hl[4]
HeaderUnits        <- hl[5]

HeaderDateTime <- sub('T', ' ', HeaderDateTime) # Replace ISO separator with a space

if (regexpr("n t ", hl[HeaderLines]) == 1) {    # HeaderLines with comment
    HeaderComment  <- hl[6]
    Title <- paste(HeaderComment, HeaderDateTime, sep = ' - ')
    HeaderLines <- HeaderLines - 1              # Adjust for with comment

} else {                                        # HeaderLines without comment
    HeaderLines <- HeaderLines - 2              # Adjust for no comment
    Title <- paste(HeaderFilename, HeaderDateTime, sep = ' - ')
}

## Setup channel colors
# First four are for Keysight DSO
ChannelColorStandard <- c('gold2', 'green', 'blue', 'darkorchid1', 'purple', 'red', 'pink', 'cyan', 'orange', 'brown')
HeaderChannelList <- as.integer(unlist(strsplit(HeaderChannelList, ' '))) # Array of channel numbers from string

ChannelColors <- NULL
for (i in 1:length(HeaderChannelList)) {
    ChannelColors <- c(ChannelColors, ChannelColorStandard[HeaderChannelList[i]])
}

HeaderUnits <- unlist(strsplit(HeaderUnits, ' '))         # Form units label for Y axis, volts and/or amps
y_units <- tolower(unique(HeaderUnits[3:length(HeaderUnits)]))
y_units <- paste(y_units, 's', sep = '')                  # Make units plural
y_units <- paste(y_units, sep=' ', collapse=' ')          # In case mixed units, put them in a string

t1 <- read.table(File, skip = HeaderLines, header = TRUE) # Read the data

ChannelNames <- ordered(colnames(t1)[3:ncol(t1)])         # Get channel column order for color mapping
t1 <- t1 %>% gather('Channel', 'y',  3:ncol(t1))          # Expand data so that channel y values are on separate lines
t1$Channel <- factor(t1$Channel, ChannelNames)            # Make channel a factor in the column order - Maybe should use forcats

x_scale_factor <- 1                                       # Set defaults for time axis scaling
x_scale_label <- 'seconds'

log_diff <- floor( log10(max(t1$t) - min(t1$t)) )         # Find scale for time formatting to us, ns, etc

if (log_diff < 0) {
    log_diff <- abs(log_diff)
    
    if (log_diff >= 10) {
        x_scale_factor <- 1e12
        x_scale_label <- 'ps'
    }
    if (log_diff >= 7 & log_diff <= 9) {
        x_scale_factor <- 1e9
        x_scale_label <- 'ns'
    }

    if (log_diff >= 4 & log_diff <= 6) {
        x_scale_factor <- 1e6
        x_scale_label <- expression(paste(mu, 's', sep = ''))
    }

    if (log_diff >= 1 & log_diff <= 3) {
        x_scale_factor <- 1e3
        x_scale_label <- 'ms'
    }
}


## Build output data - Assuming HTML only for now
knitr_data <- "# Oscilloscope Data"

if (length(HeaderComment) > 0) {
    knitr_data <- c(knitr_data,  "## `r HeaderComment`")
}

knitr_data <- c(knitr_data,
    "## `r HeaderDateTime`",
    "### Instrument: `r HeaderInstrumentID`",
    "### Original file: `r HeaderFilename`"
)

knitr_data <- c(knitr_data, "```{r plot1, echo=FALSE, message=FALSE, fig.width = FigureWidth, fig.height = FigureHeight}")

## knitr_data <- NULL # For interactive development

knitr_data <- c(knitr_data, 
                "ggplot(t1) +",
                "geom_line(aes(x = x_scale_factor * t, y = y, colour = Channel)) +",
                "xlab(x_scale_label) +",
                "ylab(y_units) +",
                "ggtitle(Title) +",
                "scale_colour_manual(values=ChannelColors) +",
                "labs(colour='') +", # No legend header
                "theme_jm1"
                )

## eval(parse(text = knitr_data))       # For interactive development

knitr_data <- c(knitr_data, "```")

knitr_data <- c(knitr_data, # Append CSS data, need to do it at end to override defaults
    '<style type="text/css">',
    'body {',
    'max-width: 1400px;',   # Make the plots wider, default is 800
    'margin: auto;',
    'padding: 1em;',
    'line-height: 20px ; ',
    '}',
    'table, th {',          # Customize tables a bit
    '   max-width: 95%;',
    '   border: 1px solid #ccc;',
    '   border-spacing: 15px 3px;',
    '}',
    '</style>'
    )

writeLines(knit2html(text = knitr_data), OutFile)


##  writeLines(knit2pdf(text = knitr_data), OutFile) # For future

