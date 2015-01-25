# Основные обозначения: 
#   x, P, F, f  - значения дискретных и непрерывных с.в., 
#                 а также их вероятности, функции распределения
#                 и плотности вероятности; 
#   ltext, cpal - подписи к графикам и цвет графиков; 
#   kk, k       - вспомогательные переменные.


# Графики распределений вероятностей дискретных с.в.
dgraph <- function(x, P, ltext) { 
  windows()
  kk <- seq(dim(P)[2])
  cpal <- rainbow(max(kk))
  plot(x, P[,1], col=cpal[1], type="o", pch=1, lwd=1, xlim=c(0.9*min(x), 1.1*max(x)), ylim=c(0, 1.1*max(P)), xlab="", ylab="", main="")
  for(k in kk[-1]) lines(x, P[,k],col=cpal[k], type="o", pch=k, lwd=1)
  
  legend("topright", col=cpal, pch=kk, lwd=1, legend=ltext)
}

# Графики функций распределения дискретных с.в.
pgraph <- function(x, F, ltext) { 
  windows()
  kk <- seq(dim(F)[2]) 
  cpal <- rainbow(max(kk))
  
  plot(stepfun(x, c(0, F[,1])), col=cpal[1], pch=1, lwd=1,
   xlim=c(0.9*min(x), 1.1*max(x)), ylim=c(0, 1.1*max(F)),
   xlab="", ylab="", main="")
  
   for(k in kk[-1]) lines(stepfun(x, c(0, F[,k])),
   col=cpal[k], pch=k, lwd=1)
  
  legend("bottomright", col=cpal, pch=kk, lwd=1, legend=ltext)
  }
# Графики плотностей вероятностей непрерывных с.в.

cgraph <- function(x, f, ltext) { windows()
 kk <- seq(dim(f)[2]); cpal <- rainbow(max(kk))
 plot(x, f[,1], col=cpal[1], type="l", lwd=1,
 xlim=c(0.9*min(x), 1.1*max(x)), ylim=c(0, 1.1*max(f)),
 xlab="", ylab="", main="")
 for(k in kk[-1]) lines(x, f[,k], col=cpal[k], lwd=1)
 legend("topright", col=cpal, lwd=1, legend=ltext)
}
# Графики функций распределения непрерывных с.в.

fgraph <- function(x, F, ltext) { windows()
 kk <- seq(dim(F)[2]); cpal <- rainbow(max(kk))
 plot(x, F[,1], col=cpal[1], type="l", lwd=1,
 xlim=c(0.9*min(x), 1.1*max(x)), ylim=c(0, 1.1*max(F)),
 xlab="", ylab="", main="")
 for(k in kk[-1]) lines(x, F[,k], col=cpal[k], lwd=1)
 legend("bottomright", col=cpal, lwd=1, legend=ltext)
}
