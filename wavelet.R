# Example of Wavelet decomposition

library(wavethresh)

# Create base signal
x = (0:4095)/1000
y = sin(x)*cos((x+1)^2) 
par(mfrow=c(3,1))
plot(y, type='l', ylim=c(-3,3), 
	main="Original signal (white: original signal, black: with noise, red: low-freq component)")

# Add noise
set.seed(777)
ynoisy = y + rnorm(4096, 0.05)
lines(ynoisy)
lines(y, col='white')

# Add anomaly at timestep 2000
ynoisy[2000:2008] = ynoisy[2000:2008] + 1.5
lines(ynoisy)
lines(y, col='white')

# Use wavelet analysis to find lo, mid, hi freq components
ywd = wd(ynoisy)
ylo = threshold(ywd, levels=c(4:11), 
	policy="probability", value=1)
lines(wr(ylo), col='red')

yhi = threshold(ywd, levels=c(0:8), 
	policy="probability", value=1)
plot(wr(yhi), type='l', ylim=c(-3.5,3.5), ylab='y',
	main="High-frequency component of noisy signal")

ymid = threshold(ywd, levels=c(0:3, 9:11), 
	policy="probability", value=1)
plot(wr(ymid), type='l', ylim=c(-1.5,1.5), ylab='y',
	main="Mid-frequency component of noisy signal")
	
	
# Use wp data from wpfsec.csv
sa443 = wp[(wp$wp==443) & (wp$f==18)]
s = sa443$ct[order(sa443$sec)]
s2 = s[1600:2623]
par(mfrow=c(3,1))
plot(s2, type='l', 
	main="Syn/Ack packets on Port 443, 2014-04-10 18:09-18:33")

s2wd = wd(s2)
s2lo = threshold(s2wd, levels=c(4:9), 
	policy="probability", value=1)
lines(wr(s2lo), col='red')
s2hi = threshold(s2wd, levels=c(0:8), 
	policy="probability", value=1)
plot(wr(s2hi), type='l')
s2mid = threshold(s2wd, levels=c(0:3,9), 
	policy="probability", value=1)
plot(wr(s2mid), type='l')
