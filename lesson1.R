x <- c(1,2,3,4); 
y <- c(4,3,2,1);
z <- c(1,3,4,2); 
t <- c(1,4,2,3);

a <- c(1,-2,3,-4);

d_transpose <- matrix(c(x,y,z,t), nrow=length(x))

d <- matrix(c(x,y,z,t), nrow=length(x), byrow=TRUE);

if(det(d) != 0) solve(d,a) else stop("������� ������� ��������!")

o <- c(0,0,0,0)

matrix(round(runif(9, min=-9, max=9)), nrow=3) -> A; A
matrix(0, nrow=3, ncol=3) -> O
all(A+B == B+A)


source("probGraph.r")

#������������ �������������
p <- seq(1, 7, 2)/10; # ����������� ������
n <- 12;# ����� ���������  
x <- seq(0, n)
binomProbDist <- sapply(p, function(pp) dbinom(x, n, pp))
binomCDF      <- sapply(p, function(pp) pbinom(x, n, pp))

l <- sapply(p, function(pp) sprintf("B(%.0f, %.3g)", n, pp))

discreteDistGraph(x, binomProbDist, l) 

discreteCDFGraph(x, binomCDF, l)



# ������� ��������� ������������ � ������� ��������� ������������� �.�.:
# a, s - ��������� ����������� �������������; 
# x - ��������� �������� �.�.; 
# normalProbDensity, normalCDF - �������� ��������� ������������ � ������� ������������� �.�. 
# l - ������� � ��������.
a <- 0; 
s <- c(1/4, 1/2, 1, 2); 
x <- seq(-6, 6, len=300)
normalProbDensity <- sapply(s, function(ss) dnorm(x, a, ss))
normalCDF <- sapply(s, function(ss) pnorm(x, a, ss))
l <- sapply(s, function(ss) sprintf("N(%.3g, %.3g)", a, ss))

continuousDistGraph(x, normalProbDensity, l); 
continuousCDFGraph(x, normalCDF, l)
