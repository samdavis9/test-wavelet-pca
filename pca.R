# Get data by port by second
wp = read.csv('wpfsec.csv')
wp$sec = factor(wp$sec)
wp$wp = factor(wp$wp)
m = tapply(wp$ct, list(wp$wp,wp$sec), sum)
m=m[,1:1453]
m[is.na(m)] = 0

# Plot raw data
par(mfrow=c(3,5))
plot(m[1,], xlab='time (sec)', ylab = 'packets',
	main="Packets/sec on other ports", type='l')
for (i in 2:15) plot(m[i,], xlab='time (sec)', ylab='packets',
	main=paste('Packets/sec on port', rownames(m)[i]), type='l')

# PCA analysis - In this example I have chosen scale=TRUE
# but that may or may not be a good idea
X = scale(t(m), center=TRUE, scale=TRUE)
pca = prcomp(X)
normxv = apply(pca$x, 2, function(x) sqrt(sum(x^2)))
u = scale(pca$x, center=FALSE, scale=normxv)

# Visualize components
par(mfrow=c(3,5))
for (i in 1:15) plot(u[,i], xlab='time (sec)',
	ylab=paste('Projection', i), type='l')

# Project on the first k components without anomalies
usd = apply(u, 2, function(x) max(abs(x))/sd(x))
k = which(usd[-1]>5)[1] # how many are not abnormal
Vk = pca$rotation[,1:k]
P = Vk %*% t(Vk)
modeled = X %*% P
resid = X - modeled
spe = apply(resid, 1, function(x) sum(x^2))
anom = which(spe>4)
anombox = c(anom-5, anom+5)

# Show modeled and residual traffic
for (i in 1:15) {
	par(mfrow=c(2,1))
	plot(X[,i], type='l', col='red', xlab='sec', 
		ylab='normalized rate', main=paste('Traffic on port',
		ifelse(i==1, 'other', rownames(m)[i]), 
		'(red=scaled raw data, black=modeled)'))
	lines(modeled[,i])
	abline(v=anombox, col='yellow')
	plot(resid[,i], type='l', xlab='sec', 
		ylab=paste('Port', rownames(m)[i]),
		main="Residuals")
	abline(v=anombox, col='yellow')
	readline()
	}