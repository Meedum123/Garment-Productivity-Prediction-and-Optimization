set.seed(123)
a <- rnorm(30)
b <- a * 0.5 + rnorm(30)
x <- cbind(a, b)  # Combine into a matrix
x=scale(x, center = TRUE, scale = FALSE)  

# Plot original data
plot(x[,1], x[,2], col="blue", pch=19, main="Original Data with Principal Components")
grid()

# Perform PCA
e=eigen(var(x))$vectors
sco=as.matrix(x)%*%(e)

# Draw PC1 and PC2 as arrows
arrows(0, 0, e[1,1], e[2,1], col="red", lwd=2)
arrows(0, 0, e[1,2], e[2,2], col="green", lwd=2)

legend("topleft", legend=c("PC1", "PC2"), col=c("red", "green"), lwd=2)

plot(sco[,1], sco[,2], col="blue", pch=19, main="Data in Principal Component Space")
grid()
abline(v=0, h=0, lty=2, col="gray")

x_e=sco[,1] %*% t(e[,1])

par(mfrow=c(1,2))
plot(x[,1],x[,2],col="gray")
abline(a=0,b=e[2,1]/e[1,1])
plot(x_e[,1],x_e[,2],col="gray")
abline(a=0,b=e[2,1]/e[1,1])
par(mfrow=c(1,1))


################# PLS understanding
set.seed(123)
a <- rnorm(30)
b <- a * 0.5 + rnorm(30)
x <- cbind(a, b)  # Combine into a matrix
x=scale(x, center = TRUE, scale = FALSE)  
y=2*a+3*b+rnorm(30,0,1)
y=scale(y, center = TRUE, scale = FALSE)  

ex=eigen(var(x))$vectors
scox=as.matrix(x)%*%(ex)

ey=eigen(var(y))$vectors
scoy=as.matrix(y)%*%(ey)

# scoy=r*scox

r=solve(t(scox)%*%scox)%*%t(scox)%*%scoy

E_scory=scox%*%r

E_y=E_scory%*%ey

par(mfrow = c(1, 2))
plot(y)
plot(E_y)
par(mfrow = c(1, 1))
plot(y-E_y)


### prediction
set.seed(100)
n=5
a <- rnorm(n)
b <- a * 0.5 + rnorm(n)
x_t <- cbind(a, b) # Combine into a matrix
x_t=scale(x_t, center = TRUE, scale = FALSE)  
y_t=2*a+3*b+rnorm(1,0,n)
y_t=scale(y_t, center = TRUE, scale = FALSE)  

ex_t=eigen(var(x_t))$vectors
scox_t=as.matrix(x_t)%*%(ex_t)

E_scory_t=scox_t%*%r

E_y_t=E_scory_t%*%ey

y_t
E_y_t

par(mfrow = c(1, 2))
plot(y_t)
plot(E_y_t)
par(mfrow = c(1, 1))
plot(y_t-E_y_t)
