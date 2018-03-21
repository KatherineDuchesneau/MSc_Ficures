####Simulating data####

# Hypothesis 1: Mycorrhizal colonization protects roots from lesions at the point of colonization,
# resulting in a negative correlation between the two.


NXsec<-100 # Number of cross-sections per plant
NPlants<-1000 # Number of plants
Pm<-0.2 # Probability of Mycorrhizae colonizing any given x-section
Pp<-Pm # Probability of Pathogens colonizing any give x-section 
Pc<-0.5 # Probability of finding Pathogens given Mycorrhizae have colonized at the same cross section. 
# Interference if Pc < 1, facilitation if Pc > 1. The value of 0.5 means Pathogens have only half the expected colonization rate when Mycorrhizae are present.

Root<-data.frame(Myc=rep(NA,NPlants),Path=rep(NA,NPlants)) # Data for saving output

for(Pl in 1:NPlants){
  tempMyc<-sample(1:0,NXsec,replace=T,prob=c(Pm,1-Pm)) # Generate Myc colonization... we've weighted the probability of sampling!
  tempPath<-rep(NA,NXsec) # Empty vector of pathogen infection
  
  # Simulate root cross-sections. 
  if(sum(tempMyc==0)>0){
    tempPath[tempMyc==0]<-sample(1:0,length(tempPath[tempMyc==0]),replace=T,prob=c(Pp,1-Pp))  
  }
  if(sum(tempMyc==1)>0){
    tempPath[tempMyc==1]<-sample(1:0,length(tempPath[tempMyc==1]),replace=T,prob=c(Pp*Pc,1-Pp*Pc))
  }
  
  # Calculate % colonization
  Root$Myc[Pl]<-sum(tempMyc)
  Root$Path[Pl]<-sum(tempPath)
  
  # Clear temporary vectors to avoid errors in future iterations
  tempMyc<-tempPath<-NA
}
head(Root)


mean(Root$Myc)
mean(Root$Path)
summary(lm(Path~Myc,data=Root))
qplot(Myc,Path,data=Root,alpha=I(0.3))+
  theme_bw()+
  geom_smooth(method="lm")




####Add GM####
#make a function for it! 
GMsim<-function(NXsec=100,NPlants=1000,Pm=0.2,Pp=0.2,Pc=0.5){
  Root<-data.frame(Myc=rep(NA,NPlants),Path=rep(NA,NPlants)) # Data for saving output
  for(i in 1:NPlants){
    tempMyc<-sample(1:0,NXsec,replace=T,prob=c(Pm,1-Pm)) # Generate Myc colonization
    tempPath<-rep(NA,NXsec) # Empty vector of pathogen infection
    
    # Simulate root cross-sections. 
    if(length(tempPath[tempMyc==0])>0){
      tempPath[tempMyc==0]<-sample(1:0,length(tempPath[tempMyc==0]),replace=T,prob=c(Pp,1-Pp))  
    }
    if(length(tempPath[tempMyc==1])>0){
      tempPath[tempMyc==1]<-sample(1:0,length(tempPath[tempMyc==1]),replace=T,prob=c(Pp*Pc,1-Pp*Pc))
    }
    
    # Calculate % colonization
    Root$Myc[i]<-sum(tempMyc)
    Root$Path[i]<-sum(tempPath)
    
    # Clear temporary vectors to avoid errors in future iterations
    tempMyc<-tempPath<-NA
  }
  return(Root) ## Don't forget to return this to the user!
}



GMlow<-GMsim()
GMhigh<-GMsim(Pm=0.1)
# Add labels and combine for analysis & visualization
GMhigh$Dens<-"High"
GMlow$Dens<-"Low"
simDat<-rbind(GMlow,GMhigh)
head(simDat)
summary(lm(Path~Myc*Dens,data=simDat))

qplot(Myc,Path,data=simDat,colour=Dens,alpha=I(0.3))+
  theme_bw()+
  geom_smooth(method="lm")


# what if myc protection changes?
GMlow<-GMsim(Pm=0.2,Pp=0.2,Pc=0.5)
GMhigh<-GMsim(Pm=0.1,Pc=1)
# Add labels and combine for analysis & visualization
GMhigh$Dens<-"High"
GMlow$Dens<-"Low"
simDat<-rbind(GMlow,GMhigh)
head(simDat)
summary(lm(Path~Myc*Dens,data=simDat))

qplot(Myc,Path,data=simDat,colour=Dens,alpha=I(0.3))+
  theme_bw()+
  geom_smooth(method="lm")

# we could test the performance of the statistical model (the type 1 and 2 error of the statistical model!!)


Loops<-100
Pout<-data.frame(Myc=rep(NA,Loops),Dens=rep(NA,Loops),Int=rep(NA,Loops))
for(i in 1:Loops){
  GMlow<-GMsim(Pm=0.2,Pc=0.5)
  GMhigh<-GMsim(Pm=0.1,Pc=1)
  # Add labels and combine for analysis & visualization
  GMhigh$Dens<-"High"
  GMlow$Dens<-"Low"
  simDat<-rbind(GMlow,GMhigh)
  # Analyze and save
  Pout[i,]<-summary(lm(Path~Myc*Dens,data=simDat))$coefficients[14:16]
}
head(Pout)

sum(Pout$Myc>0.05)/length(Pout$Myc) # Mycorrhizae effect
sum(Pout$Dens>0.05)/length(Pout$Dens) # GM Density Effect
sum(Pout$Int>0.05)/length(Pout$Int) # Interaction Effect

qplot(Myc,data=Pout)+
  geom_vline(aes(xintercept=0.05),colour="red")+
  theme_bw() # Distribution of P-values for Myc slope

qplot(Dens,data=Pout)+
  geom_vline(aes(xintercept=0.05),colour="red")+
  theme_bw() # Distribution of P-values for GM density effect

qplot(Int,data=Pout)+
  geom_vline(aes(xintercept=0.05),colour="red")+
  theme_bw() # Distribution of P-values for density*Myc interaction


# Our Type II error rates tell us the lm() model is failing to reject the null 
# for the interaction term about 50% of the time and Myc and Dens main effects 
# about 90% the time! Without any parameter differences we should get a P-value 
# greater than 0.05 about 95% of the time. The lm() model is essentially garbage 
# for detecting the effect that we are simulating.


# What is our type 1 error?
Loops<-100
Pout<-data.frame(Myc=rep(NA,Loops),Dens=rep(NA,Loops),Int=rep(NA,Loops))
for(i in 1:Loops){
  GMhigh<-GMsim(Pm=0.2,Pc=0.5)
  GMlow<-GMsim(Pm=0.1,Pc=1)
  # Add labels and combine for analysis & visualization
  GMhigh$Dens<-"High"
  GMlow$Dens<-"Low"
  simDat<-rbind(GMlow,GMhigh)
  # Analyze and save
  Pout[i,]<-summary(lm(Path~Myc*Dens,data=simDat))$coefficients[14:16]
}
head(Pout)

sum(Pout$Myc<0.05)/length(Pout$Myc) # Mycorrhizae effect
sum(Pout$Dens<0.05)/length(Pout$Dens) # GM Density Effect
sum(Pout$Int<0.05)/length(Pout$Int) # Interaction Effect


#### Nested(Nested(Sim))####

Loops<-100
PoutNoInt<-data.frame(Myc=rep(NA,Loops),Dens=rep(NA,Loops),Int=rep(NA,Loops))
for(i in 1:Loops){
  GMlow<-GMsim(Pm=0.2,Pc=0.5)
  GMhigh<-GMsim(Pm=0.1,Pc=0.5)
  # Add labels and combine for analysis & visualization
  GMhigh$Dens<-"High"
  GMlow$Dens<-"Low"
  simDat<-rbind(GMlow,GMhigh)
  # Analyze and save
  PoutNoInt[i,]<-summary(lm(Path~Myc*Dens,data=simDat))$coefficients[14:16]
}
Loops<-100
PoutInt<-data.frame(Myc=rep(NA,Loops),Dens=rep(NA,Loops),Int=rep(NA,Loops))
for(i in 1:Loops){
  GMlow<-GMsim(Pm=0.2,Pc=0.5)
  GMhigh<-GMsim(Pm=0.1,Pc=1)
  # Add labels and combine for analysis & visualization
  GMhigh$Dens<-"High"
  GMlow$Dens<-"Low"
  simDat<-rbind(GMlow,GMhigh)
  # Analyze and save
  PoutInt[i,]<-summary(lm(Path~Myc*Dens,data=simDat))$coefficients[14:16]
}
cat("Error rates for No-Interaction Model\n",
    "Myc:",sum(PoutNoInt$Myc>0.05)/length(PoutNoInt$Myc),"\n",
    "Dens:",sum(PoutNoInt$Dens>0.05)/length(PoutNoInt$Dens),"\n",
    "Error rates for Interaction Model\n",
    "I-Myc:",sum(PoutInt$Myc>0.05)/length(PoutInt$Myc),"\n",
    "I-Dens:",sum(PoutInt$Dens>0.05)/length(PoutInt$Dens),"\n",
    "I-Int:",sum(PoutInt$Int>0.05)/length(PoutInt$Int),"\n")


#### Better Statistics ####


GMIsim<-function(NXsec=100,NPlants=1000,Pm=0.2,Pp=0.2,Pc=0.5){
  Ivals<-data.frame(I=rep(NA,NXsec)) # Vector for saving output
  for(i in 1:NPlants){
    tempMyc<-sample(1:0,NXsec,replace=T,prob=c(Pm,1-Pm)) # Generate Myc colonization
    tempPath<-rep(NA,NXsec) # Empty vector of pathogen infection
    
    # Simulate root cross-sections. 
    if(length(tempPath[tempMyc==0])>0){
      tempPath[tempMyc==0]<-sample(1:0,length(tempPath[tempMyc==0]),replace=T,prob=c(Pp,1-Pp))  
    }
    if(length(tempPath[tempMyc==1])>0){
      tempPath[tempMyc==1]<-sample(1:0,length(tempPath[tempMyc==1]),replace=T,prob=c(Pp*Pc,1-Pp*Pc))
    }
    
    # Count # categories
    tempMPobs<-sum(tempMyc==1 & tempPath==1) # Count of observed co-occurrences
    # Number of prediced co-occrrences is a bit tricky. 
    ## 1. Sum each sole occurrence and divide by total observations to calculate individual probabilities
    ## 2. Multiply probabilities together to calculate predicted joint probability
    ## 3. Multiply by total N observations to calculate predicted number of co-occurrences
    tempMPpred<-sum(tempMyc==1 & tempPath==0)*sum(tempMyc==0 & tempPath==1)/NXsec 
    # Now calculate I as the deviation of observed from predicted
    
    # Calculate I
    if(tempMPpred > 0){ # Avoid errors when tempMPpred == 0
      Ivals[i,]<-tempMPobs/tempMPpred  
    }
    
    
    # Clear temporary vectors to avoid errors in future iterations
    tempMyc<-tempPath<-tempMPobs<-tempMPpred<-NA
  }
  return(Ivals)
}

Loops<-100
Pout<-data.frame(Int0=rep(NA,Loops),Dens=rep(NA,Loops))
for(i in 1:Loops){
  GMhigh<-GMIsim(Pm=0.1,Pc=0.5)
  GMlow<-GMIsim(Pm=0.2,Pc=1)
  # Add labels and combine for analysis & visualization
  GMhigh$Dens<-"High"
  GMlow$Dens<-"Low"
  simDat<-rbind(GMlow,GMhigh)
  # Analyze and save
  Pout[i,]<-summary(lm(I~Dens,data=simDat))$coefficients[7:8]
}
head(Pout)
sum(Pout$Int0>0.05)/length(Pout$Int0) # Test of whether I is different from zero
sum(Pout$Dens>0.05)/length(Pout$Dens) # Test if I differs for high vs low GM


qplot(Int0,data=Pout)+geom_vline(aes(xintercept=0.05),colour="red")+theme_bw() # Distribution of P-values
qplot(Dens,data=Pout)+geom_vline(aes(xintercept=0.05),colour="red")+theme_bw() # Distribution of P-values

