A <- matrix(c(1,2,3,1), ncol=2, byrow=TRUE)
B <- matrix(c(-1,3,3,4), ncol=2, byrow=TRUE)
C <- matrix(c(5,0,0,1), ncol=2, byrow=TRUE)

diag_of_A <- diag(A)
diag_of_B <- diag(B)
diag_of_C <- diag(C)

#1d:
diag_of_A + diag_of_B + diag_of_C
