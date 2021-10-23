##############################################

#Author: Renan Barbieri Segamarchi
#11882707
#email for contact: renan.barbieri.s@usp.br
#FMRP-USP
#Code made for analyzing and calculating the match of two lines

##############################################
#Reading the raw data of X and y
arq <- read.csv("Planilha_aux.csv",sep=",")
arq
#Calculating the initial values such as mr1 column
mr11 <- (arq[1,4]-arq[1,2])/(arq[1,3]-arq[1,1])
mr12 <- (arq[2,4]-arq[2,2])/(arq[2,3]-arq[2,1])
mr13 <- (arq[3,4]-arq[3,2])/(arq[3,3]-arq[3,1])
mr1 <- c(mr11,mr12,mr13)
mr1

#Calculating the initial values alike mr2 column
mr21 <- (arq[1,8]-arq[1,6])/(arq[1,7]-arq[1,5])
mr22 <- (arq[2,8]-arq[2,6])/(arq[2,7]-arq[2,5])
mr23 <- (arq[3,8]-arq[3,6])/(arq[3,7]-arq[3,5])
mr2 <- c(mr21,mr22,mr23)
mr2
####Creates a dataframe for outputting like a table
mat <- as.data.frame(mr1)
mat$mr2 <- mr2
mat

#Calculates the br1 column
br11 <- -1*(mr11*arq[1,1]-arq[1,2])
br12 <- -1*(mr12*arq[2,1]-arq[2,2])
br13 <- -1*(mr13*arq[3,1]-arq[3,2])
br1 <- c(br11,br12,br13)
br1

#Now br2
br21 <- -1*(mr21*arq[1,5]-arq[1,6])
br22 <- -1*(mr22*arq[2,5]-arq[2,6])
br23 <- -1*(mr23*arq[3,5]-arq[3,6])
br2 <- c(br21,br22,br23)
br2

####Creates a dataframe for outputting like a table
matbr <- as.data.frame(br1)
matbr$br2 <- br2
matbr

#Going on to the final calculations, of X and Y
X1 <- (-1* (mr21*arq[1,5]) + arq[1,6] + (mr11*arq[1,1])-arq[1,2]) / (mr11-mr21)
X2 <- (-1* (mr22*arq[2,5]) + arq[2,6] + (mr12*arq[2,1])-arq[2,2]) / (mr12-mr22)
X3 <- (-1* (mr23*arq[3,5]) + arq[3,6] + (mr13*arq[3,1])-arq[3,2]) / (mr13-mr23)
X <- c(X1,X2,X3)
X

Y1 <- mr11*X1 - mr11*arq[1,1] + arq[1,2]
Y2 <- mr12*X2 - mr12*arq[2,1] + arq[2,2]
Y3 <- mr13*X3 - mr13*arq[3,1] + arq[3,2]
Y <- c(Y1,Y2,Y3)


matXY <- as.data.frame(X)
matXY$Y2 <- Y
matXY

#FOr the final values:
rec3DX <- mean(X1,X2,X3)
rec3DX
rec3DY <- mean(Y1,Y2,Y3)
rec3DY
rec3DZ <- mean(Y2,X3)

rec <- c(rec3DX,rec3DY,rec3DZ)
rec
erroabsX <- abs(159.3 - rec3DX)
erroabsY <- abs(168.2 - rec3DY)
erroabsZ <- abs(74.9 - rec3DZ)
erroabs <- c(erroabsX,erroabsY,erroabsZ)
erroabs
ERROTOTAL <- mean(erroabs)
ERROTOTAL
