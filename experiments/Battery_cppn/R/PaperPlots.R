#Read imported data frames
library(dplyr)
library(ggplot2)
library(gridExtra)
library(plotly)
library(ggbeeswarm)
setwd("~/Battery-Robot/experiments/Battery_cppn/R")
savedir<-"/Users/tarigarma/PhD/1st_paperSUB/Rebo21a/Journal/EnergyEfficiency/Figures/"
#See initial short experiments to check for initial battery charge
setinit_level<-readRDS("Getlevel__generationInfo.RDS")
setinit_level<-setinit_level[complete.cases(setinit_level),]
(length(which(setinit_level$Battery<0))*100)/nrow(setinit_level)
#mean battery
mean(setinit_level$Battery)
median(setinit_level$Battery)
#mean battery with max size
at<-setinit_level%>%filter(absolute_size==15)
mean(at$Battery)
median(at$Battery)
threshold<-quantile(setinit_level$Battery,probs = 0.50)
ggplot(setinit_level,aes(y=Battery,x=robot_id))+geom_point()+geom_hline(yintercept = threshold)
at<-setinit_level%>%filter(run==1)%>%group_by(Generation)
ggplot(at,aes(x=Generation,y=Battery))+geom_point()

##
##Read and prepare Baseline Experiments
baseline<-readRDS("Baseline_generationInfo.RDS")
baseline<-baseline[complete.cases(baseline),] #73 NAs removed
baseline$Fitness<-baseline$Fitness * 100
#Modify for plotting
baseline<-baseline%>%group_by(Generation)%>%
  mutate(Balance_med = summary(Balance)[[3]])%>%
  mutate(Balance_up=summary(Balance)[[5]])%>%
  mutate(Balance_down=summary(Balance)[[2]])%>%
  mutate(Fitness_med=summary(Fitness)[[3]])%>%
  mutate(Fitness_up=summary(Fitness)[[5]])%>%
  mutate(Fitness_down=summary(Fitness)[[2]])%>%
  mutate(Battery_med=summary(Battery)[[3]])%>%
  mutate(Battery_up=summary(Battery)[[5]])%>%
  mutate(Battery_down=summary(Battery)[[2]])

##
##Read and prepare NSGA-II  Experiments
nsga<-readRDS("managerNSGA_generationInfo.RDS")
nsga<-nsga[complete.cases(nsga),] #48 NAs removed
nsga$Fitness<-nsga$Fitness * 100
#Modify for plotting
nsga<-nsga%>%group_by(Generation)%>%
  mutate(Balance_med = summary(Balance)[[3]])%>%
  mutate(Balance_up=summary(Balance)[[5]])%>%
  mutate(Balance_down=summary(Balance)[[2]])%>%
  mutate(Fitness_med=summary(Fitness)[[3]])%>%
  mutate(Fitness_up=summary(Fitness)[[5]])%>%
  mutate(Fitness_down=summary(Fitness)[[2]])%>%
  mutate(Battery_med=summary(Battery)[[3]])%>%
  mutate(Battery_up=summary(Battery)[[5]])%>%
  mutate(Battery_down=summary(Battery)[[2]])

###########################
###Plots
########################
#Compare Speed in one graph
toPlot<-bind_rows("Baseline"=baseline,"NSGA-II"= nsga, .id="Experiment")
p1<-ggplot(toPlot, aes(y=Fitness_med, x=Generation, colour=Experiment))+
  geom_line()+geom_ribbon(aes(ymin=Fitness_down, ymax=Fitness_up,fill=Experiment),alpha=0.2,color=NA)+
  ylab("Speed (cm/s)")+ggtitle("B.")+
  theme(text = element_text(size=20),legend.position = "bottom")
#Compare Battery in one graph
toPlot<-bind_rows("Baseline"=baseline,"NSGA-II"= nsga, .id="Experiment")
p2<-ggplot(toPlot, aes(y=Battery_med, x=Generation, colour=Experiment))+
  geom_line()+geom_ribbon(aes(ymin=Battery_down, ymax=Battery_up,fill=Experiment),alpha=0.2,color=NA)+
  ylab("Remaining Energy (W)")+ggtitle("A.")+
  theme(text = element_text(size=20),legend.position = "bottom")
#Compare Balance in one graph
toPlot<-bind_rows("Baseline"=baseline,"NSGA-II"= nsga, .id="Experiment")
p3<-ggplot(toPlot, aes(y=Balance_med, x=Generation, colour=Experiment))+
  geom_line()+geom_ribbon(aes(ymin=Balance_down, ymax=Balance_up,fill=Experiment),alpha=0.2,color=NA)+
  ylab("Balance")+ggtitle("D.")+
  theme(text = element_text(size=20), legend.position = "bottom")
##Compare the max value
maxBase<-baseline%>%group_by(run,Generation)%>%
  slice(which.max(Fitness))%>%ungroup()%>%
  group_by(Generation)%>%
  mutate(Fitness_med=summary(Fitness)[[3]])%>%
  mutate(Fitness_up=summary(Fitness)[[5]])%>%
  mutate(Fitness_down=summary(Fitness)[[2]])
maxnsga<-nsga%>%group_by(run,Generation)%>%
  slice(which.max(Fitness))%>%ungroup()%>%
  group_by(Generation)%>%
  mutate(Fitness_med=summary(Fitness)[[3]])%>%
  mutate(Fitness_up=summary(Fitness)[[5]])%>%
  mutate(Fitness_down=summary(Fitness)[[2]])
toPlot<-bind_rows("Baseline"=maxBase,"NSGA-II"= maxnsga, .id="Experiment")
p4<-ggplot(toPlot, aes(y=Fitness_med, x=Generation, colour=Experiment))+
  geom_line()+geom_ribbon(aes(ymin=Fitness_down, ymax=Fitness_up,fill=Experiment),alpha=0.2,color=NA)+
  ylab("Speed (cm/s)")+ggtitle("C.")+
  theme(text = element_text(size=20),legend.position = "bottom")


#Arrange in one plot
plist<-list(p1,p2,p3,p4)
lay<-rbind(c(2,1),
           c(4,3))
pdf(file=paste0(savedir,"Fig1.pdf"), width = 14,height = 10)
grid.arrange(grobs=plist,layout_matrix =lay)
dev.off()

###########################
###size vs speed
########################
nsga99<-nsga%>%filter(Generation==99)%>%
  select(c("robot_id","Fitness","Battery","run","absolute_size","hinge_count","Balance"))
baseline99<-baseline%>%filter(Generation==99)%>%
  select(c("robot_id","Fitness","Battery","run","absolute_size","hinge_count", "Balance"))
toPlotdf<-bind_rows("Baseline"=baseline99,"NSGA-II"= nsga99, .id="Experiment")
toPlotdf$hinge_count<-as.factor(toPlotdf$hinge_count)
toPlotdf$absolute_size<-as.factor(toPlotdf$absolute_size)
pdf(file=paste0(savedir,"hinge_speed.pdf"), width = 14,height = 7)
ggplot(toPlotdf, aes(y=Fitness, x=hinge_count, colour=Experiment))+
  geom_beeswarm()+geom_hline(yintercept = 6,linetype="dashed")+
  ylab("Speed (cm/s)")+xlab("Number of joints")+
  theme(text=element_text(size=22))
dev.off()

###########################
###Descriptors
########################
descriptors<-c("coverage", "hinge_count", 
               "brick_count",
               "proportion", "height",
               "symmetry", "avg_period", "dev_period", "avg_phase_offset", "dev_phase_offset", 
               "avg_amplitude", "dev_amplitude", "avg_intra_dev_params", "avg_inter_dev_params")
p<-list()
for(i in 1:length(descriptors))
{
  at<-nsga%>%select(c("Generation","run",descriptors[i]))
  names(at)[3]<-"value"
  ot<-baseline%>%select(c("Generation","run",descriptors[i]))
  names(ot)[3]<-"value"
  at<-bind_rows("Baseline"=ot,"NSGA-II"= at, .id="Experiment")
  at<-at%>%group_by(Experiment,Generation)%>%
    mutate(value_med = summary(value)[[3]])%>%
    mutate(value_up=summary(value)[[5]])%>%
    mutate(value_down=summary(value)[[2]])
  
  p[[i]]<-ggplot(at,aes(x=Generation,y=value_med,colour=Experiment))+geom_line()+
    geom_ribbon(aes(ymin=value_down, ymax=value_up,fill=Experiment),alpha=0.2,color=NA)+
    ylab(descriptors[i])+
    theme(text = element_text(size=20),legend.position = "bottom")
  #print(p)
  #print(descriptors[i])
  #print("\n")
  #print(wilcox.test(at$value,ot$value, alternative = "two.sided"))
  
  #readline(prompt="Press [enter] to continue")
}
pdf(file=paste0(savedir,"descriptor_body.pdf"), width = 14,height = 7)
do.call(grid.arrange,p[c(1,3,4,6)])
dev.off()
pdf(file=paste0(savedir,"descriptor_controller.pdf"), width = 14,height = 10)
do.call(grid.arrange,p[7:12])
dev.off()


###########################
###Get top ten robots
########################
##Only robots of the last generation
top_baseline<-baseline99[order(baseline99$Fitness,decreasing=TRUE),]
top_baseline<-top_baseline[1:10,]
top_nsga<-nsga99[order(nsga99$Fitness,decreasing=TRUE),]
top_nsga<-top_nsga[1:10,]
#Robots of all generations (and pareto NSGA)
top_baseline<-baseline[order(baseline$Fitness,decreasing=TRUE),c(1:5,6,15,24)]
top_baseline<-top_baseline[!duplicated(top_baseline$robot_id),]
top_baseline<-top_baseline[1:10,]
orderedDf<-nsga[order(nsga$Fitness,nsga$Battery,decreasing=T),c(1:4,6,15,24)]
front_nsga<-orderedDf[which(!duplicated(cummax(orderedDf$Battery))),]
top_nsga<-front_nsga[1:10,]

getRobotCommand<-function(at,exp_base_name,option=T) #(top_baseline,"Baseline")
{
  filecon<-"getCommand.txt"
  for (i in 1:nrow(at))
  {
    if(option==T){
    write(paste0("get ","../Data/",exp_base_name,"_",at$run[i],
                 "/data_fullevolution/plane/phenotype_images/body_robot_",
                 at$robot_id[i],".png ",exp_base_name,i,".png"),filecon,sep="\n",append=TRUE)
    }else{
    write(paste0("get ","../Data/",exp_base_name,"_",at$run[i],
                 "/data_fullevolution/plane/individuals/individual_robot_",
                 at$robot_id[i],".pkl ",exp_base_name,i,".pkl"),filecon,sep="\n",append=TRUE)
    }
  }
}

getRobotCommand(top_baseline,"Baseline",T)
getRobotCommand(top_nsga,"managerNSGA",T)
getRobotCommand(top_baseline,"Baseline",F)
getRobotCommand(top_nsga,"managerNSGA",F)



#Third quartile baseline
baseline_95<-baseline%>%ungroup()%>%
  select(-c(Fitness_down,Fitness_med,Fitness_up))%>%
  group_by(run,Generation)%>%
  filter(Fitness >quantile(Fitness,0.75))
baseline_95<-baseline_95%>%group_by(Generation)%>%
  mutate(Fitness_med=summary(Fitness)[[3]])%>%
  mutate(Fitness_up=summary(Fitness)[[5]])%>%
  mutate(Fitness_down=summary(Fitness)[[2]])%>%
  mutate(Battery_med=summary(Battery)[[3]])%>%
  mutate(Battery_up=summary(Battery)[[5]])%>%
  mutate(Battery_down=summary(Battery)[[2]])%>%
  mutate(Balance_med = summary(Balance)[[3]])%>%
  mutate(Balance_up=summary(Balance)[[5]])%>%
  mutate(Balance_down=summary(Balance)[[2]])
p8<-ggplot(baseline_95,aes(x=Generation,y=Fitness_med))+geom_line()+
  ggtitle("Fitness: Locomotion")
#Third quartile NSGA
nsga_95<-nsga%>%ungroup()%>%
  select(-c(Fitness_down,Fitness_med,Fitness_up))%>%
  group_by(run,Generation)%>%
  filter(Fitness >quantile(Fitness,0.75))
nsga_95<-nsga_95%>%group_by(Generation)%>%
  mutate(Fitness_med=summary(Fitness)[[3]])%>%
  mutate(Fitness_up=summary(Fitness)[[5]])%>%
  mutate(Fitness_down=summary(Fitness)[[2]])%>%
  mutate(Battery_med=summary(Battery)[[3]])%>%
  mutate(Battery_up=summary(Battery)[[5]])%>%
  mutate(Battery_down=summary(Battery)[[2]])%>%
  mutate(Balance_med = summary(Balance)[[3]])%>%
  mutate(Balance_up=summary(Balance)[[5]])%>%
  mutate(Balance_down=summary(Balance)[[2]])
p9<-ggplot(nsga_95,aes(x=Generation,y=Fitness_med))+geom_line()+
  ggtitle("Fitness: Locomotion & Energy consumption")
grid.arrange(p8,p9,nrow=2)
#in one graph
pdf(file=paste0(savedir,"speed3dr.pdf"), width = 14,height = 7)
toPlot<-bind_rows("Baseline"=baseline_95,"NSGA"= nsga_95, .id="Experiment")
ggplot(toPlot, aes(y=Fitness_med, x=Generation, colour=Experiment))+
  geom_line()+geom_ribbon(aes(ymin=Fitness_down, ymax=Fitness_up,fill=Experiment),alpha=0.2,color=NA)+
  ylab("Speed (cm/s)")+theme(text = element_text(size=20))
dev.off()
ggplot(toPlot, aes(y=Battery_med, x=Generation, colour=Experiment))+
  geom_line()+geom_ribbon(aes(ymin=Battery_down, ymax=Battery_up,fill=Experiment),alpha=0.2,color=NA)+
  ylab("Speed (cm/s)")+theme(text = element_text(size=20))
ggplot(toPlot, aes(y=Balance_med, x=Generation, colour=Experiment))+
  geom_line()+geom_ribbon(aes(ymin=Balance_down, ymax=Balance_up,fill=Experiment),alpha=0.2,color=NA)+
  ylab("Speed (cm/s)")+theme(text = element_text(size=20))




  
    