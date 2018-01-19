# Build two time-series, each time series being a sinusoid with different frequencies
# Inspired by an example found online:: http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html 


xs <- seq(-2*pi,2*pi,pi/100) # create a sequence of numbers; this will be used to 'sample' the time series at those points
signal1 <- sin(3*xs) # signal1
signal2 <- sin(10*xs) # signal2
par(mfrow = c(1, 2)) # set graphical parameters; don't worry about it:: Basically I use that to have in a single figure both signals (see the following two lines of code)
plot(xs, signal1, type="l",ylim=c(-1,1)); abline(h=0,lty=3)
plot(xs, signal2, type="l",ylim=c(-1,1)); abline(h=0,lty=3)


# Bring both signals we have created together to create a new signal 
signal3 <- 0.5 * signal1 + 0.25 * signal2
plot(xs, signal3,type="l"); title("Combined signals 1 and 2 into a single signal"); abline(h=0,lty=3)

repeat.xs     <- seq(-2*pi,0,pi/100)
signal3.repeat <- 0.5*sin(3*repeat.xs) + 0.25*sin(10*repeat.xs)
plot(xs, signal3,type="l"); title("Repeating pattern")
points(repeat.xs, signal3.repeat,type="l",col="red"); abline(h=0,v=c(-2*pi,0),lty=3)

# ================================================================
#         ***********   Define function which will compute and plot the spectrum **********
plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

# ================================================================
#         ***********   Apply the Fourier transform  **********
# ================================================================

par(mfrow = c(1, 1))
signal3_FT_domain <- fft(signal3); plot.frequency.spectrum(signal3_FT_domain, xlimits=c(0,20))

par(mfrow = c(1, 2)) # set graphical parameters; don't worry about it:: Basically I use that to have in a single figure both signals (see the following two lines of code)
signal1_FT_domain <- fft(signal1); plot.frequency.spectrum(signal1_FT_domain, xlimits=c(0,30))
signal2_FT_domain <- fft(signal2); plot.frequency.spectrum(signal2_FT_domain, xlimits=c(0,30))


