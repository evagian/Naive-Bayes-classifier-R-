#Question	1
d1	<- c(1,	0,	0,	1,	0,	0,	0,	1)
d2	<- c(0,	1,	0,	1,	1,	0,	0,	0)
d3	<- c(0,	0,	1,	0,	0,	0,	1,	0)
d4	<- c(0,	0,	0,	1,	0,	0,	0,	0)
d5	<- c(0,	0,	0,	0,	0,	0,	1,	0)
d6	<- c(0,	0,	0,	1,	0,	1,	0,	1)
d7	<- c(0,	0,	1,	0,	0,	1,	0,	1)
d8	<- c(1,	0,	0,	0,	0,	0,	0,	1)
d9	<- c(0,	0,	0,	0,	0,	1,	0,	1)
d10	<- c(1,	1,	0,	0,	0,	1,	0,	1)
nb_df	<- as.data.frame(rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10))
names(nb_df)	<- c("BadCredit",	"HasStableJob",	"OwnsHouse",	"BigLoan",	
                  "HasLargeBankAccount",	"HasPriorLoans",	"HasDependents",	"Decision")
df <- nb_df



#question 2 
#1	when	we	reject	the	loan	and	0	when	we	accept
#reject 1 : P(reject)=6/10 
#accept 0: P(accept)=4/10

priors <- data.frame(Decision = c(1, 0), prob = c(6/10, 4/10))

#question 3 P(Fi = 1|Class = 0)|P(Fi = 1|Class = 1)


p1 <- unname(subset(aggregate(BadCredit ~ Decision, df, sum), select=c(BadCredit)))
p2 <- unname(subset(aggregate(HasStableJob ~ Decision, df, sum), select=c(HasStableJob)))
p3 <- unname(subset(aggregate(OwnsHouse ~ Decision, df, sum), select=c(OwnsHouse)))
p4 <- unname(subset(aggregate(BigLoan ~ Decision, df, sum), select=c(BigLoan)))
p5 <- unname(subset(aggregate(HasLargeBankAccount ~ Decision, df, sum), select=c(HasLargeBankAccount)))
p6 <- unname(subset(aggregate(HasPriorLoans ~ Decision, df, sum), select=c(HasPriorLoans)))
p7 <- unname(subset(aggregate(HasDependents ~ Decision, df, sum), select=c(HasDependents)))

probs	<- as.data.frame(rbind(p1[,1],p2[,1],p3[,1],p4[,1],p5[,1],p6[,1],p7[,1]))
names(probs)	<- c("class0",	"class1")
probs$class0 <- probs$class0/4
probs$class1 <- probs$class1/6

#Question 4
Decision 1 | 0
BadCredit 
1:          3/6     0/4 | P(BadCredit=1|Decision=1) = 3/6, P(BadCredit=1|Decision=0) = 0/4
0:          3/6     4/4 | P(BadCredit=0|Decision=1) = 3/6, P(BadCredit=0|Decision=0) = 4/4

Decision 1 | 0
HasStableJob 
1:          1/6     1/4 | P(HasStableJob=1|Decision=1) = 1/6, P(HasStableJobt=1|Decision=0) = 1/4
0:          5/6     3/4 | P(HasStableJob=0|Decision=1) = 5/6, P(HasStableJob=0|Decision=0) = 3/4

Decision 1 | 0
OwnsHouse 
1:          1/6     1/4 | P(OwnsHouse=1|Decision=1) = 1/6, P(OwnsHouse=1|Decision=0) = 1/4
0:          5/6     3/4 | P(OwnsHouse=0|Decision=1) = 5/6, P(OwnsHouse=0|Decision=0) = 3/4

Decision 1 | 0
BigLoan 
1:          2/6     2/4 | P(BigLoan=1|Decision=1) = 2/6, P(BigLoan=1|Decision=0) = 2/4
0:          4/6     2/4 | P(BigLoan=0|Decision=1) = 4/6, P(BigLoan=0|Decision=0) = 2/4

Decision 1 | 0
HasLargeBankAccount 
1:          0/6     1/4 | P(HasLargeBankAccount=1|Decision=1) = 0/6, P(HasLargeBankAccount=1|Decision=0) = 1/4
0:          6/6     3/4 | P(HasLargeBankAccount=0|Decision=1) = 6/6, P(HasLargeBankAccount=0|Decision=0) = 3/4

Decision 1 | 0
HasPriorLoans 
1:          4/6     0/4 | P(HasPriorLoans=1|Decision=1) = 4/6, P(HasPriorLoans=1|Decision=0) = 0/4
0:          2/6     4/4 | P(HasPriorLoans=0|Decision=1) = 2/6, P(HasPriorLoans=0|Decision=0) = 4/4

Decision 1 | 0
HasDependents 
1:          0/6     2/4 | P(HasDependents=1|Decision=1) = 0/6, P(HasDependents=1|Decision=0) = 2/4
0:          6/6     2/4 | P(HasDependents=0|Decision=1) = 6/6, P(HasDependents=0|Decision=0) = 2/4

# In the "probs" dataframe we calculated the P(Fi = 1|Class = 0)'s and P(Fi = 1|Class = 1)'s.
# We can use these to calculate the P(Fi = 0|Class = 0)'s and P(Fi = 0|Class = 1)'s
# as P(Fi = 0|Class = 0) = 1 - P(Fi = 1|Class = 0) and P(Fi = 0|Class = 1) = 1 - P(Fi = 1|Class = 1).
# Concluding P(Fi = 1|Class = 0) + P(Fi = 1|Class = 1) != 1
# but P(Fi = 0|Class = 0) + P(Fi = 1|Class = 0) = 1
# and P(Fi = 0|Class = 1) + P(Fi = 1|Class = 1) = 1

p1lC <- probs
p0lC <- 1-probs

#Question 6
# P(Decision = 0 | BadCredit = 0) = p0lC[1,1] = 4/4 = 1
# The (Decision = 0 | BadCredit = 1) combination is not present in our training data but
# this might be caused by the fact that we only have 10 observation. It is possible that in a
# larger dataset the (Decision = 0 | BadCredit = 1) combination might be frequently encountered.

P(F1=0|C0) = 4 + 1 / 4 + 2
P(F1=0|C1) = 0 + 1 / 6 + 2

P(F1=1|C0) = 0 + 1 / 4 + 2
P(F1=1|C1) = 3 + 1 / 6 + 2

p1 <- unname(subset(aggregate(BadCredit ~ Decision, df, sum), select=c(BadCredit)))
p2 <- unname(subset(aggregate(HasStableJob ~ Decision, df, sum), select=c(HasStableJob)))
p3 <- unname(subset(aggregate(OwnsHouse ~ Decision, df, sum), select=c(OwnsHouse)))
p4 <- unname(subset(aggregate(BigLoan ~ Decision, df, sum), select=c(BigLoan)))
p5 <- unname(subset(aggregate(HasLargeBankAccount ~ Decision, df, sum), select=c(HasLargeBankAccount)))
p6 <- unname(subset(aggregate(HasPriorLoans ~ Decision, df, sum), select=c(HasPriorLoans)))
p7 <- unname(subset(aggregate(HasDependents ~ Decision, df, sum), select=c(HasDependents)))

probs2	<- as.data.frame(rbind(p1[,1],p2[,1],p3[,1],p4[,1],p5[,1],p6[,1],p7[,1]))
names(probs2)	<- c("class0",	"class1")
probs3 <- probs2
probs3$class0 <- (probs2$class0+1)/(4+2)
probs3$class1 <- (probs2$class1+1)/(6+2)
p1lC2 <- probs3

probs4 <- probs2
probs4$class0 <- (4-probs4$class0)
probs4$class1 <- (6-probs4$class1)
probs4$class0 <- (probs4$class0+1)/(4+2)
probs4$class1 <- (probs4$class1+1)/(6+2)
p0lC2 <- probs4


names(p1lC)	<- c("value1class0",	"value1class1")
names(p0lC)	<- c("value0class0",	"value0class1")
basicprobs <- p1lC
basicprobs$value0class0 <- p0lC$value0class0
basicprobs$value0class1 <- p0lC$value0class1


names(p1lC2)	<- c("value1class0smooth",	"value1class1smooth")
names(p0lC2)	<- c("value0class0smooth",	"value0class1smooth")
smoothprobs <- p1lC2
smoothprobs$value0class0smooth <- p0lC2$value0class0smooth
smoothprobs$value0class1smooth <- p0lC2$value0class1smooth

prob_matrix <- smoothprobs
test_df <- df
test_df <- subset(test_df, select = -c(pred))

#Question 8
test_df[, ] <- sapply(test_df[, ], as.numeric)
priors[, ] <- sapply(priors[, ], as.numeric)
prob_matrix[, ] <- sapply(prob_matrix[, ], as.numeric)


predict_nb <- function(test_df, priors, prob_matrix) {

temp <- test_df
temp$class1y <- 1
temp$class1x <- 1
temp$class0y <- 1
temp$class0x <- 1

temp$p1 <- 0
temp$p0 <- 0
test_df$pred <- -1

for(i in 1:nrow(temp)){
  
for(j in 1:7){
  
  temp[i,10]<-ifelse(temp[i,j]==1, prob_matrix[j,2], prob_matrix[j,4])
  temp[i,12]<-ifelse(temp[i,j]==1, prob_matrix[j,1], prob_matrix[j,3])
  
  temp[i,9] <- temp[i,9]*temp[i,10]
  temp[i,11] <- temp[i,11]*temp[i,12]
  
  
}

  temp[i,13] <- temp[i,9]*priors[1,2] 
  temp[i,14] <- temp[i,11]*priors[2,2] 
  
  test_df[i,9] <-ifelse(temp[i,13]>temp[i,14] , 1, 0)
 
}
print(test_df)
}


predict_nb(test_df, priors, prob_matrix)

# Verify posterior probabilities
priors <- data.frame(Decision = c(1, 0), prob = c(6/10, 4/10))

#d1:	1 0 0 1 0 0 0 1
P(Decision=1|d1) ≈ P(BadCredit=1|Decision=1)*P(HasStableJob=0|Decision=1)*P(OwnsHouse=0|Decision=1)*P(BigLoan=1|Decision=1)
*P(HasLargeBankAccount=0|Decision=1)*P(HasPriorLoans=0|Decision=1)*P(HasDependents=0|Decision=1)*P(Decision=1)  
p1lC[1,2]*p0lC[2,2]*p0lC[3,2]*p1lC[4,2]*p0lC[5,2]*p0lC[6,2]*p0lC[7,2]*priors[1,2] 
#or
3/6 * 5/6 * 5/6 * 2/6 * 6/6 * 2/6 * 6/6 * 6/10 

P(Decision=0|d1) ≈ P(BadCredit=1|Decision=0)*P(HasStableJob=0|Decision=0)*P(OwnsHouse=0|Decision=0)*P(BigLoan=1|Decision=0)
*P(HasLargeBankAccount=0|Decision=0)*P(HasPriorLoans=0|Decision=0)*P(HasDependents=0|Decision=0)*P(Decision=0)  
p1lC[1,1]*p0lC[2,1]*p0lC[3,1]*p1lC[4,1]*p0lC[5,1]*p0lC[6,1]*p0lC[7,1]*priors[2,2] 
# or
0/4 * 3/4 * 3/4 * 2/4 * 3/4 * 4/4 * 2/4 * 4/10 

#d1: after add-one smoothing
#P(Decision=1|d1)
p1lC2[1,2]*p0lC2[2,2]*p0lC2[3,2]*p1lC2[4,2]*p0lC2[5,2]*p0lC2[6,2]*p0lC2[7,2]*priors[1,2]
#P(Decision=0|d1)
p1lC2[1,1]*p0lC2[2,1]*p0lC2[3,1]*p1lC2[4,1]*p0lC2[5,1]*p0lC2[6,1]*p0lC2[7,1]*priors[2,2]

#d2: 0 1 0 1 1 0 0 0
P(Decision=1|d2) ≈ P(BadCredit=0|Decision=1)*P(HasStableJob=1|Decision=1)*P(OwnsHouse=0|Decision=1)*P(BigLoan=1|Decision=1)
*P(HasLargeBankAccount=1|Decision=1)*P(HasPriorLoans=0|Decision=1)*P(HasDependents=0|Decision=1)*P(Decision=1)  
p0lC[1,2]*p1lC[2,2]*p0lC[3,2]*p1lC[4,2]*p1lC[5,2]*p0lC[6,2]*p0lC[7,2]*priors[1,2] 
#or
3/6 * 1/6 * 5/6 * 2/6 * 0/6 * 2/6 * 6/6 * 6/10

P(Decision=0|d2) ≈ P(BadCredit=0|Decision=0)*P(HasStableJob=1|Decision=0)*P(OwnsHouse=0|Decision=0)*P(BigLoan=1|Decision=0)
*P(HasLargeBankAccount=1|Decision=0)*P(HasPriorLoans=0|Decision=0)*P(HasDependents=0|Decision=0)*P(Decision=0)  
p0lC[1,1]*p1lC[2,1]*p0lC[3,1]*p1lC[4,1]*p1lC[5,1]*p0lC[6,1]*p0lC[7,1]*priors[2,2] 
# or
4/4 * 1/4 * 3/4 * 2/4 * 1/4 * 4/4 * 2/4 * 4/10

#d2: after add-one smoothing
#P(Decision=1|d2)
p0lC2[1,2]*p1lC2[2,2]*p0lC2[3,2]*p1lC2[4,2]*p1lC2[5,2]*p0lC2[6,2]*p0lC2[7,2]*priors[1,2] 
#P(Decision=0|d2)
p0lC2[1,1]*p1lC2[2,1]*p0lC2[3,1]*p1lC2[4,1]*p1lC2[5,1]*p0lC2[6,1]*p0lC2[7,1]*priors[2,2] 


#d3:	0 0 1 0 0 0 1 0
P(Decision=1|d3) ≈ P(BadCredit=0|Decision=1)*P(HasStableJob=0|Decision=1)*P(OwnsHouse=1|Decision=1)*P(BigLoan=0|Decision=1)
*P(HasLargeBankAccount=0|Decision=1)*P(HasPriorLoans=0|Decision=1)*P(HasDependents=1|Decision=1)*P(Decision=1)  
p0lC[1,2]*p0lC[2,2]*p1lC[3,2]*p0lC[4,2]*p0lC[5,2]*p0lC[6,2]*p1lC[7,2]*priors[1,2] 
#or
3/6 * 5/6 * 1/6 * 4/6 * 6/6 * 2/6 * 0/6 * 6/10

P(Decision=0|d3) ≈ P(BadCredit=0|Decision=0)*P(HasStableJob=0|Decision=0)*P(OwnsHouse=1|Decision=0)*P(BigLoan=0|Decision=0)
*P(HasLargeBankAccount=0|Decision=0)*P(HasPriorLoans=0|Decision=0)*P(HasDependents=1|Decision=0)*P(Decision=0)  
p0lC[1,1]*p0lC[2,1]*p1lC[3,1]*p0lC[4,1]*p0lC[5,1]*p0lC[6,1]*p1lC[7,1]*priors[2,2] 
#or
4/4 * 3/4 * 1/4 * 2/4 * 3/4 * 4/4 * 2/4 * 4/10 
#d3: after add-one smoothing
#P(Decision=1|d3)
p0lC2[1,2]*p0lC2[2,2]*p1lC2[3,2]*p0lC2[4,2]*p0lC2[5,2]*p0lC2[6,2]*p1lC2[7,2]*priors[1,2] 
#P(Decision=0|d3)
p0lC2[1,1]*p0lC2[2,1]*p1lC2[3,1]*p0lC2[4,1]*p0lC2[5,1]*p0lC2[6,1]*p1lC2[7,1]*priors[2,2] 


#d4: 0 0 0 1 0 0 0 0
P(Decision=1|d4) ≈ P(BadCredit=0|Decision=1)*P(HasStableJob=0|Decision=1)*P(OwnsHouse=0|Decision=1)*P(BigLoan=1|Decision=1)
*P(HasLargeBankAccount=0|Decision=1)*P(HasPriorLoans=0|Decision=1)*P(HasDependents=0|Decision=1)*P(Decision=1)  
p0lC[1,2]*p0lC[2,2]*p0lC[3,2]*p1lC[4,2]*p0lC[5,2]*p0lC[6,2]*p0lC[7,2]*priors[1,2] 
#or
3/6 * 5/6 * 5/6 * 2/6 * 6/6 * 2/6 * 6/6 * 6/10 

P(Decision=0|d4) ≈ P(BadCredit=0|Decision=0)*P(HasStableJob=0|Decision=0)*P(OwnsHouse=0|Decision=0)*P(BigLoan=1|Decision=0)
*P(HasLargeBankAccount=0|Decision=0)*P(HasPriorLoans=0|Decision=0)*P(HasDependents=0|Decision=0)*P(Decision=0)  
p0lC[1,1]*p0lC[2,1]*p0lC[3,1]*p1lC[4,1]*p0lC[5,1]*p0lC[6,1]*p0lC[7,1]*priors[2,2]
#or
4/4 * 3/4 * 3/4 * 2/4 * 3/4 * 4/4 * 2/4 * 4/10 
#d4: after add-one smoothing
#P(Decision=1|d4)
p0lC2[1,2]*p0lC2[2,2]*p0lC2[3,2]*p1lC2[4,2]*p0lC2[5,2]*p0lC2[6,2]*p0lC2[7,2]*priors[1,2] 
#P(Decision=0|d4)
p0lC2[1,1]*p0lC2[2,1]*p0lC2[3,1]*p1lC2[4,1]*p0lC2[5,1]*p0lC2[6,1]*p0lC2[7,1]*priors[2,2] 


#d5: 0 0 0 0 0 0 1 0
P(Decision=1|d5) ≈ P(BadCredit=0|Decision=1)*P(HasStableJob=0|Decision=1)*P(OwnsHouse=0|Decision=1)*P(BigLoan=0|Decision=1)
*P(HasLargeBankAccount=0|Decision=1)*P(HasPriorLoans=0|Decision=1)*P(HasDependents=1|Decision=1)*P(Decision=1)  
p0lC[1,2]*p0lC[2,2]*p0lC[3,2]*p0lC[4,2]*p0lC[5,2]*p0lC[6,2]*p1lC[7,2]*priors[1,2] 
#or
3/6 * 5/6 * 5/6 * 4/6 * 6/6 * 2/6 * 0/6 * 6/10 

P(Decision=0|d5) ≈ P(BadCredit=0|Decision=0)*P(HasStableJob=0|Decision=0)*P(OwnsHouse=0|Decision=0)*P(BigLoan=0|Decision=0)
*P(HasLargeBankAccount=0|Decision=0)*P(HasPriorLoans=0|Decision=0)*P(HasDependents=1|Decision=0)*P(Decision=0)  
p0lC[1,1]*p0lC[2,1]*p0lC[3,1]*p0lC[4,1]*p0lC[5,1]*p0lC[6,1]*p1lC[7,1]*priors[2,2] 
#or
4/4 * 3/4 * 3/4 * 2/4 * 3/4 * 4/4 * 2/4 * 4/10
#d5: after add-one smoothing
#P(Decision=1|d5)
p0lC2[1,2]*p0lC2[2,2]*p0lC2[3,2]*p0lC2[4,2]*p0lC2[5,2]*p0lC2[6,2]*p1lC2[7,2]*priors[1,2] 
#P(Decision=0|d5)
p0lC2[1,1]*p0lC2[2,1]*p0lC2[3,1]*p0lC2[4,1]*p0lC2[5,1]*p0lC2[6,1]*p1lC2[7,1]*priors[2,2] 


#d6: 0 0 0 1 0 1 0 1
P(Decision=1|d6) ≈ P(BadCredit=0|Decision=1)*P(HasStableJob=0|Decision=1)*P(OwnsHouse=0|Decision=1)*P(BigLoan=1|Decision=1)
*P(HasLargeBankAccount=0|Decision=1)*P(HasPriorLoans=1|Decision=1)*P(HasDependents=0|Decision=1)*P(Decision=1)  
p0lC[1,2]*p0lC[2,2]*p0lC[3,2]*p1lC[4,2]*p0lC[5,2]*p1lC[6,2]*p0lC[7,2]*priors[1,2] 
#or
3/6 * 5/6 * 5/6 * 2/6 * 6/6 * 4/6 * 6/6 * 6/10 

P(Decision=0|d6) ≈ P(BadCredit=0|Decision=0)*P(HasStableJob=0|Decision=0)*P(OwnsHouse=0|Decision=0)*P(BigLoan=1|Decision=0)
*P(HasLargeBankAccount=0|Decision=0)*P(HasPriorLoans=1|Decision=0)*P(HasDependents=0|Decision=0)*P(Decision=0)  
= p0lC[1,1]*p0lC[2,1]*p0lC[3,1]*p1lC[4,1]*p0lC[5,1]*p1lC[6,1]*p0lC[7,1]*priors[2,2] 
#or
4/4 * 3/4 * 3/4 * 2/4 * 3/4 * 0/4 * 2/4 * 4/10 
#d6: after add-one smoothing
#P(Decision=1|d6)
p0lC2[1,2]*p0lC2[2,2]*p0lC2[3,2]*p1lC2[4,2]*p0lC2[5,2]*p1lC2[6,2]*p0lC2[7,2]*priors[1,2] 
#P(Decision=0|d6)
p0lC2[1,1]*p0lC2[2,1]*p0lC2[3,1]*p1lC2[4,1]*p0lC2[5,1]*p1lC2[6,1]*p0lC2[7,1]*priors[2,2] 

#d7: 0 0 1 0 0 1 0 1
P(Decision=1|d7) ≈ P(BadCredit=0|Decision=1)*P(HasStableJob=0|Decision=1)*P(OwnsHouse=1|Decision=1)*P(BigLoan=0|Decision=1)
*P(HasLargeBankAccount=0|Decision=1)*P(HasPriorLoans=1|Decision=1)*P(HasDependents=0|Decision=1)*P(Decision=1)  
p0lC[1,2]*p0lC[2,2]*p1lC[3,2]*p0lC[4,2]*p0lC[5,2]*p1lC[6,2]*p0lC[7,2]*priors[1,2] 
#or
3/6 * 5/6 * 1/6 * 4/6 * 6/6 * 4/6 * 6/6 * 6/10 

P(Decision=0|d7) ≈ P(BadCredit=0|Decision=0)*P(HasStableJob=0|Decision=0)*P(OwnsHouse=1|Decision=0)*P(BigLoan=0|Decision=0)
*P(HasLargeBankAccount=0|Decision=0)*P(HasPriorLoans=1|Decision=0)*P(HasDependents=0|Decision=0)*P(Decision=0)  
p0lC[1,1]*p0lC[2,1]*p1lC[3,1]*p0lC[4,1]*p0lC[5,1]*p1lC[6,1]*p0lC[7,1]*priors[2,2] 
#or
4/4 * 3/4 * 1/4 * 2/4 * 3/4 * 0/4 * 2/4 * 4/10 
#d7: after add-one smoothing
#P(Decision=1|d7)
p0lC2[1,2]*p0lC2[2,2]*p1lC2[3,2]*p0lC2[4,2]*p0lC2[5,2]*p1lC2[6,2]*p0lC2[7,2]*priors[1,2] 
#P(Decision=0|d7)
p0lC2[1,1]*p0lC2[2,1]*p1lC2[3,1]*p0lC2[4,1]*p0lC2[5,1]*p1lC2[6,1]*p0lC2[7,1]*priors[2,2] 

#d8: 1 0 0 0 0 0 0 1
P(Decision=1|d8) ≈ P(BadCredit=1|Decision=1)*P(HasStableJob=0|Decision=1)*P(OwnsHouse=0|Decision=1)*P(BigLoan=0|Decision=1)
*P(HasLargeBankAccount=0|Decision=1)*P(HasPriorLoans=0|Decision=1)*P(HasDependents=0|Decision=1)*P(Decision=1)  
p1lC[1,2]*p0lC[2,2]*p0lC[3,2]*p0lC[4,2]*p0lC[5,2]*p0lC[6,2]*p0lC[7,2]*priors[1,2] 
#or
3/6 * 5/6 * 5/6 * 4/6 * 6/6 * 2/6 * 6/6 * 6/10 

P(Decision=0|d8) ≈ P(BadCredit=1|Decision=0)*P(HasStableJob=0|Decision=0)*P(OwnsHouse=0|Decision=0)*P(BigLoan=0|Decision=0)
*P(HasLargeBankAccount=0|Decision=0)*P(HasPriorLoans=0|Decision=0)*P(HasDependents=0|Decision=0)*P(Decision=0)  
p1lC[1,1]*p0lC[2,1]*p0lC[3,1]*p0lC[4,1]*p0lC[5,1]*p0lC[6,1]*p0lC[7,1]*priors[2,2]
#or
0/4 * 3/4 * 3/4 * 2/4 * 3/4 * 4/4 * 2/4 * 4/10
#d8: after add-one smoothing
#P(Decision=1|d8)
p1lC2[1,2]*p0lC2[2,2]*p0lC2[3,2]*p0lC2[4,2]*p0lC2[5,2]*p0lC2[6,2]*p0lC2[7,2]*priors[1,2] 
#P(Decision=0|d8)
p1lC2[1,1]*p0lC2[2,1]*p0lC2[3,1]*p0lC2[4,1]*p0lC2[5,1]*p0lC2[6,1]*p0lC2[7,1]*priors[2,2]

#d9: 0 0 0 0 0 1 0 1
P(Decision=1|d9) ≈ P(BadCredit=0|Decision=1)*P(HasStableJob=0|Decision=1)*P(OwnsHouse=0|Decision=1)*P(BigLoan=0|Decision=1)
*P(HasLargeBankAccount=0|Decision=1)*P(HasPriorLoans=1|Decision=1)*P(HasDependents=0|Decision=1)*P(Decision=1)  
p0lC[1,2]*p0lC[2,2]*p0lC[3,2]*p0lC[4,2]*p0lC[5,2]*p1lC[6,2]*p0lC[7,2]*priors[1,2] 
#or
3/6 * 5/6 * 5/6 * 4/6 * 6/6 * 4/6 * 6/6 * 6/10

P(Decision=0|d9) ≈ P(BadCredit=0|Decision=0)*P(HasStableJob=0|Decision=0)*P(OwnsHouse=0|Decision=0)*P(BigLoan=0|Decision=0)
*P(HasLargeBankAccount=0|Decision=0)*P(HasPriorLoans=1|Decision=0)*P(HasDependents=0|Decision=0)*P(Decision=0)  
= p0lC[1,1]*p0lC[2,1]*p0lC[3,1]*p0lC[4,1]*p0lC[5,1]*p1lC[6,1]*p0lC[7,1]*priors[2,2] 
#or
4/4 * 3/4 * 3/4 * 2/4 * 3/4 * 0/4 * 2/4 * 4/10 
#d9: after add-one smoothing
#P(Decision=1|d9)
p0lC2[1,2]*p0lC2[2,2]*p0lC2[3,2]*p0lC2[4,2]*p0lC2[5,2]*p1lC2[6,2]*p0lC2[7,2]*priors[1,2] 
#P(Decision=0|d9)
p0lC2[1,1]*p0lC2[2,1]*p0lC2[3,1]*p0lC2[4,1]*p0lC2[5,1]*p1lC2[6,1]*p0lC2[7,1]*priors[2,2] 

#d10:	1 1 0 0 0 1 0 1
P(Decision=1|d10) ≈ P(BadCredit=1|Decision=1)*P(HasStableJob=1|Decision=1)*P(OwnsHouse=0|Decision=1)*P(BigLoan=0|Decision=1)
*P(HasLargeBankAccount=0|Decision=1)*P(HasPriorLoans=1|Decision=1)*P(HasDependents=0|Decision=1)*P(Decision=1) 
p1lC[1,2]*p1lC[2,2]*p0lC[3,2]*p0lC[4,2]*p0lC[5,2]*p1lC[6,2]*p0lC[7,2]*priors[1,2] 
#or
3/6 * 1/6 * 5/6 * 4/6 * 6/6 * 4/6 * 6/6 * 6/10 

P(Decision=0|d10) ≈ P(BadCredit=1|Decision=0)*P(HasStableJob=1|Decision=0)*P(OwnsHouse=0|Decision=0)*P(BigLoan=0|Decision=0)
*P(HasLargeBankAccount=0|Decision=0)*P(HasPriorLoans=1|Decision=0)*P(HasDependents=0|Decision=0)*P(Decision=0) 
= p1lC[1,1]*p1lC[2,1]*p0lC[3,1]*p0lC[4,1]*p0lC[5,1]*p1lC[6,1]*p0lC[7,1]*priors[2,2] 
#or
0/4 * 1/4 * 3/4 * 2/4 * 3/4 * 0/4 * 2/4 * 4/10 
#d10: after add-one smoothing
#P(Decision=1|d10)
p1lC2[1,2]*p1lC2[2,2]*p0lC2[3,2]*p0lC2[4,2]*p0lC2[5,2]*p1lC2[6,2]*p0lC2[7,2]*priors[1,2] 
#P(Decision=0|d10)
p1lC2[1,1]*p1lC2[2,1]*p0lC2[3,1]*p0lC2[4,1]*p0lC2[5,1]*p1lC2[6,1]*p0lC2[7,1]*priors[2,2] 

#question 9

predict_nb <- function(test_df, priors, prob_matrix) {
  
  temp <- test_df
  temp$class1y <- 1
  temp$class1x <- 1
  temp$class0y <- 1
  temp$class0x <- 1
  
  temp$p1 <- 0
  temp$p0 <- 0
  test_df$pred <- -1
  
  for(i in 1:nrow(temp)){
    
    for(j in 1:7){
      
      temp[i,10]<-ifelse(temp[i,j]==1, prob_matrix[j,2], prob_matrix[j,4])
      temp[i,12]<-ifelse(temp[i,j]==1, prob_matrix[j,1], prob_matrix[j,3])
      
      temp[i,9] <- temp[i,9]*temp[i,10]
      temp[i,11] <- temp[i,11]*temp[i,12]
      
      
    }
    
    temp[i,13] <- temp[i,9]*priors[1,2] 
    temp[i,14] <- temp[i,11]*priors[2,2] 
    
    test_df[i,9] <-ifelse(temp[i,13]>temp[i,14] , 1, 0)
    
  }
  print(test_df)
  print(table(test_df$Decision, test_df$pred)	)
  misClasificError <- mean(test_df$Decision !=  test_df$pred) 
  print(paste('Accuracy',1-misClasificError))
}


predict_nb(test_df, priors, prob_matrix)

