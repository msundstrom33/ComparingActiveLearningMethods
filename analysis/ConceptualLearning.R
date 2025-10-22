library(ggplot2)
library(tidyselect)
library(tidyverse) 
library(dplyr)
library(effsize)
library(metafor)
library(ggpubr)
library(ggsignif)

### Function to calculate Hedge's g for one course
analyze.concept.inventory<- function(rosterfile,method,instructor){
  data<-read.csv(rosterfile)
  data$PreScore<-as.numeric(data$PreScore)
  data$PostScore<-as.numeric(data$PostScore)
  n.enrolled<-length(data$ID)
  data<-na.omit(data)
  n.matched.respondents<-length(data$ID)
  percent.matched.respondents<-length(data$ID)/n.enrolled
  df<-(2*n.matched.respondents)-2
  d<-effsize::cohen.d(data$PostScore,data$PreScore,paired=TRUE,hedges.correction = TRUE)$estimate
  d.lower<-effsize::cohen.d(data$PostScore,data$PreScore,paired=TRUE,hedges.correction = TRUE)$conf.int[1]
  d.upper<-effsize::cohen.d(data$PostScore,data$PreScore,paired=TRUE,hedges.correction = TRUE)$conf.int[2]
  firstterm<-(n.matched.respondents*2/(n.matched.respondents^2))+(d^2/(2*df))
  secondterm<-n.matched.respondents*2/df
  var<-firstterm*secondterm
  
  return(c(method,instructor,n.matched.respondents,percent.matched.respondents,d,d.lower,d.upper,var))
}


# Run function on all 25 courses with sufficient data
mat = matrix(ncol = 8, nrow = 0)
hedgesg = data.frame(mat)

hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_ISLE_Course1.csv","ISLE","1"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_ISLE_Course2.csv","ISLE","2"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_ISLE_Course3.csv","ISLE","3"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_ISLE_Course4.csv","ISLE","4"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_ISLE_Course5.csv","ISLE","5"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_PeerInstruction_Course1.csv","PeerInstruction","1"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_PeerInstruction_Course2.csv","PeerInstruction","2"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_PeerInstruction_Course3.csv","PeerInstruction","3"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_PeerInstruction_Course4.csv","PeerInstruction","4"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_PeerInstruction_Course5.csv","PeerInstruction","5"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_PeerInstruction_Course6.csv","PeerInstruction","6"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_PeerInstruction_Course7.csv","PeerInstruction","7"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_PeerInstruction_Course8.csv","PeerInstruction","8"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_PeerInstruction_Course9.csv","PeerInstruction","9"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_SCALEUP_Course3.csv","SCALEUP","3"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_SCALEUP_Course4.csv","SCALEUP","4"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_SCALEUP_Course5.csv","SCALEUP","5"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_SCALEUP_Course6.csv","SCALEUP","6"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_SCALEUP_Course7.csv","SCALEUP","7"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_Tutorials_Course2.csv","Tutorials","2"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_Tutorials_Course3.csv","Tutorials","3"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_Tutorials_Course4.csv","Tutorials","4"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_Tutorials_Course5.csv","Tutorials","5"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_Tutorials_Course6.csv","Tutorials","6"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_Tutorials_Course7.csv","Tutorials","7"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_Tutorials_Course8.csv","Tutorials","8"))

colnames(hedgesg)<-c("Method","Course","NMatched","Percent.Matched","HedgesG","LowerCI","UpperCI","Variance")

###  Conduct heterogeneity test

hedgesg$Method<-factor(hedgesg$Method,levels=c("SCALEUP","ISLE","PeerInstruction","Tutorials"))
rma(yi = as.numeric(HedgesG), vi = as.numeric(Variance), mods = ~ Method, data = hedgesg, method="REML")

### Calculate and plot pooled effect sizes for each method

pooled_eff <- rma(yi = as.numeric(HedgesG), vi = as.numeric(Variance), mods = ~ Method-1, data = hedgesg, method="REML")

pooled_plot<-as.data.frame(pooled_eff$beta,row.names = FALSE)
pooled_plot<-cbind(pooled_plot,pooled_eff$ci.lb,pooled_eff$ci.ub,pooled_eff$zval,pooled_eff$pval)
pooled_plot<-cbind(pooled_plot,c("SCALEUP","ISLE","PeerInstruction","Tutorials"))
colnames(pooled_plot)<-c("g","g_lower","g_upper","zvalue","pvalue","method")

pooled_plot$method[pooled_plot$method=="SCALEUP"]<-"SCALE-UP\n(n=289)"
pooled_plot$method[pooled_plot$method=="PeerInstruction"]<-"Peer Instruction\n(n=920)"
pooled_plot$method[pooled_plot$method=="ISLE"]<-"ISLE\n(n=202)"
pooled_plot$method[pooled_plot$method=="Tutorials"]<-"Tutorials\n(n=231)"

pooled_plot$method<-factor(pooled_plot$method,levels=c("Peer Instruction\n(n=920)","ISLE\n(n=202)","Tutorials\n(n=231)","SCALE-UP\n(n=289)"))

SignificanceDF <- data.frame(start = c("ISLE\n(n=202)","Peer Instruction\n(n=920)"), end = c("SCALE-UP\n(n=289)","SCALE-UP\n(n=289)"),
                             y = c(2.1,1.8),
                             label = c("p = 0.025","p = 0.005")) #values are from "zval" column of heterogeneity test above

ci<-ggplot() +
  geom_hline(yintercept=0,color = "black")+
  geom_pointrange(data=pooled_plot,aes(x=method, y=g,ymin = g_lower, ymax = g_upper,colour=method),size=0.65)+
  theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.ticks.y = element_line(color = "black", linewidth = 0.5),axis.ticks.x=element_line(color = "black", linewidth = 0.5),axis.title = element_text(size=9,color='black'),axis.text = element_text(size=8,color='black')) + theme(strip.background = element_rect(colour='gray20', linewidth=1),strip.text=element_text(size=8,color='black'))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5)) + theme(legend.position = "none",legend.title=element_blank(),legend.text=element_text(size=8,color='black'))+ylab("Effect Size (Hedges' g)")+scale_colour_manual(values=c("#cc79a7","#56b3e9","#e69f00","#009e74"))+xlab("Active Learning Method")+scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+ylim(-0.1,2.2)+geom_signif(
    data = SignificanceDF,
    aes(xmin = start, xmax = end, annotations = label, y_position = y),
    textsize = 2.5, vjust = -0.2,size=0.2,
    manual = TRUE,orientation="x")

ci
ggsave(ci,filename = "CI.pdf",width=3.5,height=2.5,units="in",device = cairo_pdf)


# Plot individual effect sizes

hedgesg$HedgesG<-as.numeric(hedgesg$HedgesG)
hedgesg$LowerCI<-as.numeric(hedgesg$LowerCI)
hedgesg$UpperCI<-as.numeric(hedgesg$UpperCI)

hedgesg$Method<-as.character(hedgesg$Method)
hedgesg$Method[hedgesg$Method=="SCALEUP"]<-"SCALE-UP"
hedgesg$Method[hedgesg$Method=="PeerInstruction"]<-"Peer Instruction"
hedgesg$Method<-factor(hedgesg$Method,levels=c("Peer Instruction","ISLE","Tutorials","SCALE-UP"))


ci_ind<-ggplot() +
  geom_hline(yintercept=0,color = "black")+
  geom_pointrange(data=hedgesg,aes(x=as.factor(HedgesG), y=HedgesG,ymin = LowerCI, ymax = UpperCI,colour=Method),size=0.65)+
  theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.ticks.y = element_line(color = "black", linewidth = 0.5),axis.ticks.x=element_line(color = "black", linewidth = 0.5),axis.title = element_text(size=9,color='black'),axis.text = element_text(size=8,color='black')) + theme(strip.background = element_rect(colour='gray20', linewidth=1),strip.text=element_text(size=8,color='black'))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),axis.text.x = element_blank()) + theme(legend.position = "none",legend.title=element_blank(),legend.text=element_text(size=8,color='black'))+ylab("Effect Size (Hedges' g)")+scale_colour_manual(values=c("#cc79a7","#56b3e9","#e69f00","#009e74"))+xlab("Course")+scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + facet_wrap(~Method,scales="free_x",nrow=1)

ci_ind
ggsave(ci_ind,filename = "CI_Ind.pdf",width=7,height=2.5,units="in",device = cairo_pdf)


### Repeat analysis only using courses that used the FCI

mat = matrix(ncol = 8, nrow = 0)
hedgesg = data.frame(mat)

hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_ISLE_Course1.csv","ISLE","1"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_ISLE_Course2.csv","ISLE","2"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_ISLE_Course3.csv","ISLE","3"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_ISLE_Course4.csv","ISLE","4"))

hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_PeerInstruction_Course1.csv","PeerInstruction","1"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_PeerInstruction_Course6.csv","PeerInstruction","6"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_PeerInstruction_Course9.csv","PeerInstruction","9"))

hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_SCALEUP_Course3.csv","SCALEUP","3"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_SCALEUP_Course4.csv","SCALEUP","4"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_SCALEUP_Course5.csv","SCALEUP","5"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_SCALEUP_Course6.csv","SCALEUP","6"))

hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_Tutorials_Course5.csv","Tutorials","5"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_Tutorials_Course7.csv","Tutorials","7"))
hedgesg<-rbind(hedgesg,analyze.concept.inventory("Roster_Tutorials_Course8.csv","Tutorials","8"))

colnames(hedgesg)<-c("Method","Course","NMatched","Percent.Matched","HedgesG","LowerCI","UpperCI","Variance")


###  Conduct heterogeneity test

hedgesg$Method<-factor(hedgesg$Method,levels=c("SCALEUP","ISLE","PeerInstruction","Tutorials"))

rma(yi = as.numeric(HedgesG), vi = as.numeric(Variance), mods = ~ Method, data = hedgesg, method="REML")

### Calculate pooled effect sizes for each method

rma(yi = as.numeric(HedgesG), vi = as.numeric(Variance), mods = ~ Method-1, data = hedgesg, method="REML")

pooled_eff <- rma(yi = as.numeric(HedgesG), vi = as.numeric(Variance), mods = ~ Method-1, data = hedgesg, method="REML")

pooled_plot<-as.data.frame(pooled_eff$beta,row.names = FALSE)
pooled_plot<-cbind(pooled_plot,pooled_eff$ci.lb,pooled_eff$ci.ub,pooled_eff$zval,pooled_eff$pval)
pooled_plot<-cbind(pooled_plot,c("SCALEUP","ISLE","PeerInstruction","Tutorials"))
colnames(pooled_plot)<-c("g","g_lower","g_upper","zvalue","pvalue","method")

pooled_plot$method[pooled_plot$method=="SCALEUP"]<-"SCALE-UP\n(n=253)"
pooled_plot$method[pooled_plot$method=="PeerInstruction"]<-"Peer Instruction\n(n=87)"
pooled_plot$method[pooled_plot$method=="ISLE"]<-"ISLE\n(n=184)"
pooled_plot$method[pooled_plot$method=="Tutorials"]<-"Tutorials\n(n=75)"

pooled_plot$method<-factor(pooled_plot$method,levels=c("Peer Instruction\n(n=87)","ISLE\n(n=184)","Tutorials\n(n=75)","SCALE-UP\n(n=253)"))

SignificanceDF <- data.frame(start = c("Tutorials\n(n=75)","ISLE\n(n=184)"), end = c("SCALE-UP\n(n=253)","SCALE-UP\n(n=253)"),
                             y = c(2.1,1.8),
                             label = c("p = 0.046","p = 0.019")) #values are from "zval" column of heterogeneity test above

ci_fci<-ggplot() +
  geom_hline(yintercept=0,color = "black")+
  geom_pointrange(data=pooled_plot,aes(x=method, y=g,ymin = g_lower, ymax = g_upper,colour=method),size=0.65)+
  theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.ticks.y = element_line(color = "black", linewidth = 0.5),axis.ticks.x=element_line(color = "black", linewidth = 0.5),axis.title = element_text(size=9,color='black'),axis.text = element_text(size=8,color='black')) + theme(strip.background = element_rect(colour='gray20', linewidth=1),strip.text=element_text(size=8,color='black'))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5)) + theme(legend.position = "none",legend.title=element_blank(),legend.text=element_text(size=8,color='black'))+ylab("Effect Size (Hedges' g)")+scale_colour_manual(values=c("#cc79a7","#56b3e9","#e69f00","#009e74"))+xlab("Active Learning Method")+scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+ylim(-0.1,2.2)+geom_signif(
    data = SignificanceDF,
    aes(xmin = start, xmax = end, annotations = label, y_position = y),
    textsize = 2.5, vjust = -0.2,size=0.2,
    manual = TRUE,orientation="x")

ci_fci
ggsave(ci_fci,filename = "CI_FCI.pdf",width=3.5,height=2.5,units="in",device = cairo_pdf)



