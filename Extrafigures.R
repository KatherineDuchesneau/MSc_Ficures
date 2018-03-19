#### Soil abiotic qualities and location####

formulaLDA <- lda(var$Location ~ Agg_stability + Nutrient_PC1 + pH, data=Y)
summary(formulaLDA)
formulaLDA$means
scalvec<-data.frame(formulaLDA$scaling)
LDAval <- data.frame(predict(formulaLDA)$x)
anova(lm(LDAval$LD1~var$Location))
DF<-cbind(LDAval$LD1,data.frame(var$Location))
colnames(DF)<-c("LDAval","Location")
SoilLDAplot<-ggplot(DF, aes(LDAval, fill = Location)) + geom_density(alpha = 0.2)+
  ggtitle("Linear Discriminant analysis of Soil characteristics \n in and out of A. petiolata invasion")+
  theme_bw()+
  theme(legend.title=element_blank(),plot.title = element_text(hjust = 0.5,face="bold", size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),axis.text.y = element_text(size=10),panel.border = element_rect(colour = "black"),strip.background = element_rect(colour = "black",fill="white"),strip.text.x = element_text(size = 12))+scale_fill_manual(values=c("red3","blue3"))
SoilLDAplot
ggsave(file="SoilLDAplot.eps", plot=SoilLDAplot, width=10, height=8)

#### Garlic mustard on Mycorrhiza####

modX<-glmmadmb(Mycorrhiza ~ species + species:location +
                 (1|pop)+ (1|indiv),
               data=ndata, family="nbinom1") 

modY<-glmmadmb(location ~ species + species:location +
                 (1|pop)+ (1|indiv),
               data=ndata, family="binomial") 

X1<-as.factor(round(residuals(modY),5))
X2<-residuals(modX)
df<-as.data.frame(cbind(X2,data.frame(ndata$location)))
colnames(df)<-c("X2","X1")
AVplotMycINOUT<-ggplot(df, aes(X1,X2, group=X1,fill=X1)) + geom_boxplot()+
  ggtitle("Added Variable plot of\nMycorrhiza inside and outside of invasion")+
  labs(x="Location", y="Residuals of Mycorrhiza")+
  theme_bw()+
  theme(legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5,face="bold", size=20),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        axis.text.y = element_text(size=10),
        panel.border = element_rect(colour = "black"),
        strip.background = element_rect(colour = "black",fill="white"),
        strip.text.x = element_text(size = 12))+
        scale_fill_manual(values=c("red3","blue3"))
AVplotMycINOUT
ggsave(file="AVplotMycINOUT.eps", plot=AVplotMycINOUT, width=10, height=8)



#### Garlic mustard on Lesions####

modX<-glmmadmb(Lesions ~ species + species:location +
                 (1|pop)+ (1|indiv),
               data=ndata, family="nbinom1") 

modY<-glmmadmb(location ~ species + species:location +
                 (1|pop)+ (1|indiv),
               data=ndata, family="binomial") 

X1<-as.factor(round(residuals(modY),5))
X2<-residuals(modX)
df<-as.data.frame(cbind(X2,data.frame(ndata$location)))
colnames(df)<-c("X2","X1")
AVplotLesionsINOUT<-ggplot(df, aes(X1,X2, group=X1,fill=X1)) + geom_boxplot()+
  ggtitle("Added Variable plot of\nLesions inside and outside of invasion")+
  labs(x="Location", y="Residuals of Lesions")+
  theme_bw()+
  theme(legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5,face="bold", size=20),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        axis.text.y = element_text(size=10),
        panel.border = element_rect(colour = "black"),
        strip.background = element_rect(colour = "black",fill="white"),
        strip.text.x = element_text(size = 12))+
  scale_fill_manual(values=c("red3","blue3"))
AVplotMycINOUT
ggsave(file="AVplotLesionsINOUT.eps", plot=AVplotLesionsINOUT, width=10, height=8)



