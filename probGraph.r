# �������� �����������: 
#   x, P, F, f  - �������� ���������� � ����������� �.�., 
#                 � ����� �� �����������, ������� �������������
#                 � ��������� �����������; 
#   ltext, cpal - ������� � �������� � ���� ��������; 
#   kk, k       - ��������������� ����������.


# ������� ������������� ������������ ���������� �.�.
dgraph <- function(x, P, ltext) { 
  windows()
  kk <- seq(dim(P)[2])
  cpal <- rainbow(max(kk))
  plot(x, P[,1], col=cpal[1], type="o", pch=1, lwd=1, xlim=c(0.9*min(x), 1.1*max(x)), ylim=c(0, 1.1*max(P)), xlab="", ylab="", main="")
  for(k in kk[-1]) lines(x, P[,k],col=cpal[k], type="o", pch=k, lwd=1)
  
  legend("topright", col=cpal, pch=kk, lwd=1, legend=ltext)
}

# ������� ������� ������������� ���������� �.�.
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
# ������� ���������� ������������ ����������� �.�.

cgraph <- function(x, f, ltext) { windows()
 kk <- seq(dim(f)[2]); cpal <- rainbow(max(kk))
 plot(x, f[,1], col=cpal[1], type="l", lwd=1,
 xlim=c(0.9*min(x), 1.1*max(x)), ylim=c(0, 1.1*max(f)),
 xlab="", ylab="", main="")
 for(k in kk[-1]) lines(x, f[,k], col=cpal[k], lwd=1)
 legend("topright", col=cpal, lwd=1, legend=ltext)
}
# ������� ������� ������������� ����������� �.�.

fgraph <- function(x, F, ltext) { windows()
 kk <- seq(dim(F)[2]); cpal <- rainbow(max(kk))
 plot(x, F[,1], col=cpal[1], type="l", lwd=1,
 xlim=c(0.9*min(x), 1.1*max(x)), ylim=c(0, 1.1*max(F)),
 xlab="", ylab="", main="")
 for(k in kk[-1]) lines(x, F[,k], col=cpal[k], lwd=1)
 legend("bottomright", col=cpal, lwd=1, legend=ltext)
}
