## -----------------------------------------------------------
##
## Demonstration of Oscilloscope Control and Data Acquisition in Pure R
##

## Use IDE such as RStudio or ESS to run blocks of code, or just paste into R session

## Tested on Keysight DSO-X 3024T

# Copyright (C) 2016  Jon Meek

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

## $Id: scope_comm.R,v 1.1 2016/10/31 03:26:45 meekj Exp $

## The functions below will eventually be part of a R package, likely to be called oscilloscopeR
## A multiple channel read function will be added

scopeConnect <- function(addr, port) { # Returns connection object, be sure to close when finished
    socketConnection(host = addr, port, timeout=2, open = "r+b", blocking = FALSE)
}

scopeGetSingleWaveform <- function(connection, channel, points, Debug = FALSE) {

    loop_sleep <- 0.01 #  Sleep a few ms in read loops to give data time to arrive
    
    writeLines("WAVEFORM:FORMAT BYTE", connection)        # Later switch to WORD
    flush(connection)
    writeLines("WAVeform:POINts:MODE RAW", connection)    # Needed to get more than 62500 points
    flush(connection)

    num_points_string <- paste('WAVEFORM:POINTS' ,points)
    writeLines(num_points_string, connection)
    flush(connection)

    channel_string <- paste('WAVEFORM:SOURCE CHANNEL', channel, sep ='')
    writeLines(channel_string, connection)
    flush(connection)
    
    writeLines("WAVEFORM:PREAMBLE?", connection)
    flush(connection)

    wave_preamble <- NULL
    i <- 0
    while (length(wave_preamble) == 0) { # Handle non-blocking connection where result can be delayed
        i <- i + 1
        char_read <- readChar(connection, 200) # Usually about 90 characters
        wave_preamble <- append(wave_preamble, char_read)
        Sys.sleep(loop_sleep)
    }
    if (Debug) {cat(paste("final wave_preamble: ", i, wave_preamble), "\n")}

    wave_preamble_items <- unlist(strsplit(wave_preamble, '[,\n]')) # Possibly need to add \r on Windows?

    wave_format     <- as.integer(wave_preamble_items[1])
    wave_type       <- as.integer(wave_preamble_items[2])
    wave_num_points <- as.integer(wave_preamble_items[3])
    wave_count      <- as.integer(wave_preamble_items[4])
    wave_xincrement <- as.numeric(wave_preamble_items[5])
    wave_xorigin    <- as.numeric(wave_preamble_items[6])
    wave_xreference <- as.integer(wave_preamble_items[7])
    wave_yincrement <- as.numeric(wave_preamble_items[8])
    wave_yorigin    <- as.numeric(wave_preamble_items[9])
    wave_yreference <- as.integer(wave_preamble_items[10])

    writeLines("WAVEFORM:DATA?", connection)
    flush(connection)

    buffer_header <- NULL
    i <- 0
    while (length(buffer_header) == 0) {
        i <- i + 1
        buffer_header <- readBin(connection, "raw", 10)
        Sys.sleep(loop_sleep)
    }
    if (Debug) {cat(paste("buffer_header read loops: ", i, "\n\n"))}

    buffer <- NULL
    i <- 0
    while (length(buffer) < wave_num_points) {
        i <- i + 1
        charRead <- readBin(connection, "raw", wave_num_points + 1)
        buffer <- append(buffer, as.integer(charRead))
        if (Debug) {cat(paste("buffer read loop: ", i, length(charRead), length(buffer), "\n"))}
        Sys.sleep(loop_sleep)
    }
    if (Debug) {cat(paste("final buffer read loops: ", i, length(buffer), "\n\n"))}
    buffer <- buffer[-length(buffer)] # Drop last element, which seems to be LF

    data.frame(list(t = wave_xorigin + wave_xincrement * (1:wave_num_points - wave_xreference),
                    y = wave_yorigin + wave_yincrement * (buffer - wave_yreference) ))
}


##
## Sample usage
##

library(ggplot2) # The standard for plotting, IMHO. Now included in the tidyverse package.

ScopeIP <- '192.168.3.14'
ScopePort <- 5025

AcqPoints <- 10000


## Get Instrument ID and exit
##
con <- scopeConnect(ScopeIP, ScopePort)

writeLines("*IDN?", con)
flush(con)
ScopeID <- readLines(con)
close(con)
ScopeID


## Get Waveform Data from Channel 1 into a Data Frame and Exit
##
wf1 <- NULL
con <- scopeConnect(ScopeIP, ScopePort)
wf1 <- scopeGetSingleWaveform(con, 1, AcqPoints)
close(con)

str(wf1) # Look at what is in the data frame


## Get Waveform Data and Print Some Debug Info
##
wf1 <- NULL
con <- scopeConnect(ScopeIP, ScopePort)
wf1 <- scopeGetSingleWaveform(con, 1, AcqPoints, Debug = TRUE)
close(con)


## Plot the waveform
##
ggplot(wf1) + geom_line(aes(x = t * 1e9, y = y), color = 'blue')  + xlab('t, ns')




## PC Side FFT Loop with Frequency Domain Signal Averaging - Do higher resolution FFTs than the scope will do
## 

## Scope was set to 20 us/div and left in Single mode

library(dplyr)      # Data wrangling standard, included in tidyverse package
library(fftwtools)  # Interface to fftw3
library(e1071)      # For Hanning window generation


AcqPoints <- 500000 # 500k points for nice frequency domain resolution
Title <- 'Central NJ - FM Spectrum'

wf1 <- NULL
con <- scopeConnect(ScopeIP, ScopePort)

DisplayStart <-  87 # Mhz
DisplayEnd   <- 110 # Mhz

j <- 5

count <- 0
while (j > 0) {
    j <- j - 1
    count <- count + 1

    writeLines("DIGitize CHANnel1", con)           # Hardcoded channel for testing
    flush(con)

    wf1 <- scopeGetSingleWaveform(con, 1, AcqPoints, Debug = TRUE) # Get the waveform data

    str(wf1)
#    p <- ggplot(wf1) + geom_line(aes(x = t * 1e9, y = y), color = 'blue')  + xlab('t, ns') # Plot raw data
#    print(p)

    if (count == 1) { # Compute Hanning window data on the first pass
        Hanning <- hanning.window(length(wf1$y))
    }
    sfft <- fftw(wf1$y * Hanning, HermConj=0)
    pwr <- sqrt(Re(sfft) * Re(sfft) + Im(sfft) *  Im(sfft))
    
    str(pwr) # Look at data on each pass

    if (count == 1) { # Compute the x / frequency axis on first pass
        pwr_sum <- pwr
        TimePerChannel <- wf1$t[2] - wf1$t[1]
        NyquistFrequency <- (1 / (2*TimePerChannel))
        FrequencyPerChannel <- NyquistFrequency / (length(pwr_sum) - 1)
        f <- seq(from = 0, by = FrequencyPerChannel, length = length(pwr_sum))

        pwr_spectrum <- data.frame(list(F = f, Pwr = pwr_sum))
        pwr_spectrum$F <- pwr_spectrum$F / 1e6 # Convert to MHz for this example

    } else {
        pwr_sum <- pwr_sum + pwr    # Signal average
        pwr_spectrum$Pwr <- pwr_sum # Put current average into the data frame
    }

    pTitle <- paste(Title, ' -- ', count, 'averages')
    display_data <- pwr_spectrum %>% filter(F >= DisplayStart & F <= DisplayEnd)
    p <- ggplot(display_data) + geom_line(aes(x = F, y = Pwr), color = 'blue') +
      xlab('F, MHz') + ggtitle(pTitle) # + scale_y_log10()
    print(p)
    
    Sys.sleep(2) # Delay so that we can observe the spectrum on each loop
}

close(con)

str(pwr_spectrum)



## Peak Detection and Plotting Demonstation
##

library(wmtsa) # For peak detection (warning: uses MASS which overloads dplyr::select)

DisplayStart <-  87 # Mhz
DisplayEnd   <- 107 # Mhz

DisplayStart <-  87.5 # Mhz
DisplayEnd   <-  95 # Mhz

display_data <- pwr_spectrum %>% filter(F >= DisplayStart & F <= DisplayEnd)

SNRmin <- 1 # Peak detection value

W <- wavCWT(display_data$Pwr)
z <- wavCWTTree(W)

pk <- wavCWTPeaks(z, snr.min = SNRmin, length.min = 2, noise.fun = 'sd', noise.span = 2)
pk

str(pk)

f0 <- display_data$F[1]
df <- display_data$F[2] - display_data$F[1]

## Peaks <- f0 + df * pk$x

Peaks <- data.frame(list(F = f0 + df * pk$x, Height = pk$y, Chan = pk$x))
Peaks


HeightThreshold <- 2
Peaks <- Peaks %>% filter(Height > HeightThreshold)
Peaks

label_offset <- (1.01 * max(display_data$Pwr)) - max(display_data$Pwr)

ggplot(display_data) + geom_line(aes(x = F, y = Pwr), color = 'blue') +
    xlab('F, MHz') + ylab('Power') + ggtitle(Title) +
    geom_text(data = Peaks, aes(x = F, y = Height + label_offset, label = sprintf('%0.1f', F)), size = 2.5)


## Save plot to PDF
OutFile <- '/n2/r-reports/fm2.pdf'
ggsave(OutFile, dpi=150, height = 8.5, width = 11)

## Save plot to PNG
OutFile <- '/n2/r-reports/fm2.png'
ggsave(OutFile, dpi=150, height = 4, width = 8) # Labels too close to peak tops with 4 x 8



## ########################

## Write frequency domain data to a file

write.table(pwr_spectrum, 'fd-25avg-1.dat', quote=FALSE)

