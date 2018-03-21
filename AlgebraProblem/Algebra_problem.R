library(ggplot2)


Icor<-function(p1,p2,a){
  # Conditional probabilities
  ## A through B can be estimated from the data
  ## Use these equations to try to solve for p1, p2 & a
  ## For example, B-C or C-B will isolate p1 and p2 only
  A<-p1*p2*(1-2*a)
  B<-p1*(1-p2)+p1*p2*a
  C<-(1-p1)*p2+p1*p2*a # could I remove the interaction term with alpha here?
  D<-(1-p1)*(1-p2)
  
  # Equations for calculating (co)variances and correlation coefficient  
  V1<-(A+B)*(1-(A+B))
  V2<-(A+C)*(1-(A+C))
  COV12<-A-(A+B)*(A+C)
  
  COR12<-COV12/(sqrt(V1)*sqrt(V2))
  
  # Insert equations for A through B here
  ## For example, if you wanted to solve for p1 & p2 only
  ## Combine A through B to isolate each of these terms
  a<-(2*p1*p2-1-A+B+C+D)/(4*p1*p2) # Insert equation for estimating alpha (a)
  p1<- -((-2+C-B+sqrt((C^2)-2*B*C+4*D+(B^2)))/(2))# Insert equation for p1
  p2<- -((-2+B-C+sqrt((B^2)-2*B*C+4*D+(C^2)))/(2))# insert equation for p2
  
  return(cbind(a,p1,p2))
}

# Test your model
SimDat<-data.frame(P1=runif(5000),P2=runif(5000),a=runif(5000))
Parm<-Icor(p1=SimDat$P1,p2=SimDat$P2,a=SimDat$a)


#Each of these 3 plots will be a 1:1 line if your calculations are correct"
## Test p1
qplot(SimDat$P1,Parm[,2])
## Test p2
qplot(SimDat$P2,Parm[,3])
## Test alpha
qplot(SimDat$a,Parm[,1])


#### Solving for p2####
A<-p1*p2*(1-2*a)
B<-p1*(1-p2)+p1*p2*a
C<-(1-p1)*p2+p1*p2*a 
D<-(1-p1)*(1-p2)

p1<-B-C+p2 
p2<-C-B+p1

p1<-1 - (D/(1-p2))

p2 = C -B + (1 - (D/(1-p2)))
p(1-p)=C(1-p)-B(1-p)+1*(1-p)-(D/(1-p))*(1-p)
p(1-p)=C(1-p)-B(1-p)+1-p-D
p - (p^2) = C - Cp - B + Bp + 1 - p - D
-(p^2) + (2-B+C)*p + D - 1 + B - C = 0


# USE THE QHADRADIC EQUATION

p2 =-( (-2 + B - C + sqrt((B^2) - 2*B*C + 4*D + (C^2))) / (2))
&&
p2 =-( (-2 + B - C - sqrt((B^2) - 2*B*C + 4*D + (C^2))) / (2))

#### Solving for p1####

A<-p1*p2*(1-2*a)
B<-p1*(1-p2)+p1*p2*a
C<-(1-p1)*p2+p1*p2*a 
D<-(1-p1)*(1-p2)

p2 = (1 - D/(1-p1))

p1<-B-C+p2 

p1 = B - C + (1 - D/(1-p1)) 

# Same but a little different... (on paper)
p1 =-( (-2 + C - B + sqrt((C^2) - 2*B*C + 4*D + (B^2))) / (2))
&&
p1 =-( (-2 + C - B - sqrt((C^2) - 2*B*C + 4*D + (B^2))) / (2))


####Solving for alpha####

# Theres abouT  million ways to solve for alpha especially if you have p1 and p2














