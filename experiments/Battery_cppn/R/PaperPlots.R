#Read imported data frames
library(dplyr)
library(ggplot2)
library(gridExtra)
library(plotly)
library(ggbeeswarm)
setwd("~/Battery-Robot/experiments/Battery_cppn/Data")
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
dbaseline<-readRDS("DirectedBaseline_generationInfo.RDS") #16 robots failed
baseline<-baseline[complete.cases(baseline),] #73 NAs removed
baseline$Fitness<-baseline$Fitness * 100
dbaseline<-dbaseline[complete.cases(dbaseline),] #60 NAs removed
dbaseline$Fitness<-dbaseline$Fitness * 100
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

dbaseline<-dbaseline%>%group_by(Generation)%>%
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
dnsga<-readRDS("DirectedmanagerNSGA_generationInfo.RDS")
nsga<-nsga[complete.cases(nsga),] #48 NAs removed
nsga$Fitness<-nsga$Fitness * 100
dnsga<-dnsga[complete.cases(dnsga),] #133 NAs removed
dnsga$Fitness<-dnsga$Fitness * 100
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

dnsga<-dnsga%>%group_by(Generation)%>%
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
##For baseline locomotion
toPlot<-bind_rows("Baseline"=baseline,"Battery"= nsga, .id="Case")
g1<-subset(toPlot, Case=="Baseline" & Generation==99)
g2<-subset(toPlot, Case=="Battery" & Generation==99)
#Compare Speed in one graph
print("Is the different significant in the last generation? Wilcoxon Rank Test with p=")
print(wilcox.test(g1$Fitness_med,g2$Fitness_med))
p1<-ggplot(toPlot, aes(y=Fitness_med, x=Generation, colour=Case))+
  geom_line()+geom_ribbon(aes(ymin=Fitness_down, ymax=Fitness_up,fill=Case),alpha=0.2,color=NA)+
  ylab("Speed (cm/s)")+#ggtitle("B.")+
  theme(text = element_text(size=18),legend.position = "bottom")+
  annotate("text", x=97,y=3,label="p<0.001",colour="red")
#Compare Battery in one graph
print("Is the different significant in the last generation? Wilcoxon Rank Test with p=")
print(wilcox.test(g1$Battery_med,g2$Battery_med))
p2<-ggplot(toPlot, aes(y=Battery_med, x=Generation, colour=Case))+
  geom_line()+geom_ribbon(aes(ymin=Battery_down, ymax=Battery_up,fill=Case),alpha=0.2,color=NA)+
  ylab("Remaining Energy (W)")+#ggtitle("Locomotion")+
  theme(text = element_text(size=18),legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
  annotate("text", x=97,y=10,label="p<0.001",colour="red")
#Compare Balance in one graph
print("Is the different significant in the last generation? Wilcoxon Rank Test with p=")
print(wilcox.test(g1$Balance_med,g2$Balance_med))
p3<-ggplot(toPlot, aes(y=Balance_med, x=Generation, colour=Case))+
  geom_line()+geom_ribbon(aes(ymin=Balance_down, ymax=Balance_up,fill=Case),alpha=0.2,color=NA)+
  ylab("Balance")+#ggtitle("D.")+
  theme(text = element_text(size=18), legend.position = "bottom")+
  annotate("text", x=97,y=1,label="p<0.001",colour="red")
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
toPlot<-bind_rows("Baseline"=maxBase,"Battery"= maxnsga, .id="Case")
g1<-subset(toPlot, Case=="Baseline" & Generation==99)
g2<-subset(toPlot, Case=="Battery" & Generation==99)
print("Is the different significant in the last generation? Wilcoxon Rank Test with p=")
print(wilcox.test(g1$Fitness_med,g2$Fitness_med))
p4<-ggplot(toPlot, aes(y=Fitness_med, x=Generation, colour=Case))+
  geom_line()+geom_ribbon(aes(ymin=Fitness_down, ymax=Fitness_up,fill=Case),alpha=0.2,color=NA)+
  ylab("Speed (cm/s)")+#ggtitle("C.")+
  theme(text = element_text(size=18),legend.position = "bottom")+
  annotate("text", x=97,y=8,label="p<0.001",colour="red")


##For Baseline Directed locomotion
toPlot<-bind_rows("Baseline"=dbaseline,"Battery"= dnsga, .id="Case")
g1<-subset(toPlot, Case=="Baseline" & Generation==99)
g2<-subset(toPlot, Case=="Battery" & Generation==99)
#Compare Speed in one graph
print("Is the different significant in the last generation? Wilcoxon Rank Test with p=")
print(wilcox.test(g1$Fitness_med,g2$Fitness_med))
p1_2<-ggplot(toPlot, aes(y=Fitness_med, x=Generation, colour=Case))+
  geom_line()+geom_ribbon(aes(ymin=Fitness_down, ymax=Fitness_up,fill=Case),alpha=0.2,color=NA)+
  ylab("Speed (cm/s)")+#ggtitle("B. directed")+
  theme(text = element_text(size=18),legend.position = "bottom")+
  annotate("text", x=97,y=1.55,label="p<0.001",colour="red")
#Compare Battery in one graph
print("Is the different significant in the last generation? Wilcoxon Rank Test with p=")
print(wilcox.test(g1$Battery_med,g2$Battery_med))
p2_2<-ggplot(toPlot, aes(y=Battery_med, x=Generation, colour=Case))+
  geom_line()+geom_ribbon(aes(ymin=Battery_down, ymax=Battery_up,fill=Case),alpha=0.2,color=NA)+
  ylab("Remaining Energy (W)")+#ggtitle("Directed locomotion")+
  theme(text = element_text(size=18),legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
  annotate("text", x=97,y=10,label="p<0.001",colour="red")
#Compare Balance in one graph
print("Is the different significant in the last generation? Wilcoxon Rank Test with p=")
print(wilcox.test(g1$Balance_med,g2$Balance_med))
p3_2<-ggplot(toPlot, aes(y=Balance_med, x=Generation, colour=Case))+
  geom_line()+geom_ribbon(aes(ymin=Balance_down, ymax=Balance_up,fill=Case),alpha=0.2,color=NA)+
  ylab("Balance")+#ggtitle("D. directed")+
  theme(text = element_text(size=18), legend.position = "bottom")+
  annotate("text", x=97,y=0.998,label="p<0.001",colour="red")
##Compare the max value
maxBase<-dbaseline%>%group_by(run,Generation)%>%
  slice(which.max(Fitness))%>%ungroup()%>%
  group_by(Generation)%>%
  mutate(Fitness_med=summary(Fitness)[[3]])%>%
  mutate(Fitness_up=summary(Fitness)[[5]])%>%
  mutate(Fitness_down=summary(Fitness)[[2]])
maxnsga<-dnsga%>%group_by(run,Generation)%>%
  slice(which.max(Fitness))%>%ungroup()%>%
  group_by(Generation)%>%
  mutate(Fitness_med=summary(Fitness)[[3]])%>%
  mutate(Fitness_up=summary(Fitness)[[5]])%>%
  mutate(Fitness_down=summary(Fitness)[[2]])
toPlot<-bind_rows("Baseline"=maxBase,"Battery"= maxnsga, .id="Case")
g1<-subset(toPlot, Case=="Baseline" & Generation==99)
g2<-subset(toPlot, Case=="Battery" & Generation==99)
print("Is the different significant in the last generation? Wilcoxon Rank Test with p=")
print(wilcox.test(g1$Fitness_med,g2$Fitness_med))
p4_2<-ggplot(toPlot, aes(y=Fitness_med, x=Generation, colour=Case))+
  geom_line()+geom_ribbon(aes(ymin=Fitness_down, ymax=Fitness_up,fill=Case),alpha=0.2,color=NA)+
  ylab("Speed (cm/s)")+#ggtitle("C. directed")+
  theme(text = element_text(size=18),legend.position = "bottom")+
  annotate("text", x=97,y=3.5,label="p<0.001",colour="red")


#Arrange in one plot
plist<-list(p1,p2,p3,p4,p1_2,p2_2,p3_2,p4_2)
lay<-rbind(c(2,6),
           c(1,5),
           c(4,8),
           c(3,7))
pdf(file=paste0(savedir,"Fig1_2.pdf"), width = 10,height = 14)
grid.arrange(grobs=plist,layout_matrix =lay)
dev.off()

###########################
###size vs speed
########################
#Simple locomotion
nsga99<-nsga%>%filter(Generation==99)%>%
  select(c("robot_id","Fitness","Battery","run","absolute_size","hinge_count","Balance"))
baseline99<-baseline%>%filter(Generation==99)%>%
  select(c("robot_id","Fitness","Battery","run","absolute_size","hinge_count", "Balance"))
#Directed Locomotion
dnsga99<-dnsga%>%filter(Generation==99)%>%
  select(c("robot_id","Fitness","Battery","run","absolute_size","hinge_count","Balance"))
dbaseline99<-dbaseline%>%filter(Generation==99)%>%
  select(c("robot_id","Fitness","Battery","run","absolute_size","hinge_count", "Balance"))
#plot locomotion
toPlotdf<-bind_rows("Baseline"=baseline99,"Battery"= nsga99, .id="Experiment")
#Compare same size robots
sub_battery<-toPlotdf%>%filter(Experiment == "Battery",
                               hinge_count == 14)
sub_baseline<-toPlotdf%>%filter(Experiment == "Baseline",
                                hinge_count == 14)
sub_battery%>%summarise(mean(Fitness))
sub_baseline%>%summarise(mean(Fitness))
t.test(sub_baseline$Fitness,sub_battery$Fitness)

toPlotdf$hinge_count<-as.factor(toPlotdf$hinge_count)
toPlotdf$absolute_size<-as.factor(toPlotdf$absolute_size)
#pdf(file=paste0(savedir,"hinge_speed.pdf"), width = 14,height = 7)
p5<-ggplot(toPlotdf, aes(y=Fitness, x=hinge_count, colour=Experiment))+
  geom_beeswarm()+geom_hline(yintercept = 6,linetype="dashed")+
  ylab("Speed (cm/s)")+xlab("Number of joints")+ggtitle("Simple locomotion")+
  theme(text=element_text(size=19),legend.position = "bottom",plot.title = element_text(hjust = 0.5))
#dev.off()
#plot directed locomotion
toPlotdf<-bind_rows("Baseline"=dbaseline99,"Battery"= dnsga99, .id="Experiment")
#Compare same size robots
sub_battery<-toPlotdf%>%filter(Experiment == "Battery",
                               hinge_count == 14)
sub_baseline<-toPlotdf%>%filter(Experiment == "Baseline",
                                hinge_count == 14)
sub_battery%>%summarise(mean(Fitness))
sub_baseline%>%summarise(mean(Fitness))
t.test(sub_baseline$Fitness,sub_battery$Fitness)

toPlotdf$hinge_count<-as.factor(toPlotdf$hinge_count)
toPlotdf$absolute_size<-as.factor(toPlotdf$absolute_size)
#pdf(file=paste0(savedir,"Directed_hinge_speed.pdf"), width = 14,height = 7)
p5_2<-ggplot(toPlotdf, aes(y=Fitness, x=hinge_count, colour=Experiment))+
  geom_beeswarm()+geom_hline(yintercept = 3,linetype="dashed")+
  ylab("Speed (cm/s)")+xlab("Number of joints")+ggtitle("Directed locomotion")+
  theme(text=element_text(size=19),legend.position = "bottom",plot.title = element_text(hjust = 0.5))
#dev.off()

pdf(file=paste0(savedir,"hinge_speed_2.pdf"), width = 14,height = 7)
grid.arrange(p5,p5_2, nrow=1)
dev.off()
###########################
###Descriptors
########################
descriptors<-c("coverage", "hinge_count", 
               "limbs",
               "proportion", "height",
               "symmetry", "avg_period", "dev_period", "avg_phase_offset", "dev_phase_offset", 
               "avg_amplitude", "dev_amplitude", "avg_intra_dev_params", "avg_inter_dev_params")
#For locomotion
p<-list()
for(i in 1:length(descriptors))
{
  print(descriptors[i])
  at<-nsga%>%select(c("Generation","run",descriptors[i]))
  names(at)[3]<-"value"
  ot<-baseline%>%select(c("Generation","run",descriptors[i]))
  names(ot)[3]<-"value"
  at<-bind_rows("Baseline"=ot,"Battery"= at, .id="Experiment")
  at<-at%>%group_by(Experiment,Generation)%>%
    mutate(value_med = summary(value)[[3]])%>%
    mutate(value_up=summary(value)[[5]])%>%
    mutate(value_down=summary(value)[[2]])
  g1<-subset(at, Experiment == "Baseline" & Generation == 99 )
  g2<-subset(at, Experiment == "Battery" & Generation == 99)
  print("Wilcoxon test p-value is:")
  wiltest<-wilcox.test(g1$value_med,g2$value_med)$p.value
  print(wiltest)
  anntext<-ifelse(!is.nan(wiltest),ifelse(wiltest<0.05,"p<0.001","p>0.05"),"NS")
  ypos<-max(at$value_up)
  p[[i]]<-ggplot(at,aes(x=Generation,y=value_med,colour=Experiment))+geom_line()+
    geom_ribbon(aes(ymin=value_down, ymax=value_up,fill=Experiment),alpha=0.2,color=NA)+
    ylab(descriptors[i])+
    theme(text = element_text(size=19),legend.position = "bottom")+
    annotate("text",x=97,y=ypos,label=anntext,colour="red")

  # print(descriptors[i])
  # print("\n")
  # print(wilcox.test(at$value,ot$value, alternative = "greater"))
  # 
  # readline(prompt="Press [enter] to continue")
}

#For directed locomotion
p_directed<-list()
for(i in 1:length(descriptors))
{
  print(descriptors[i])
  at<-dnsga%>%select(c("Generation","run",descriptors[i]))
  names(at)[3]<-"value"
  ot<-dbaseline%>%select(c("Generation","run",descriptors[i]))
  names(ot)[3]<-"value"
  at<-bind_rows("Baseline"=ot,"Battery"= at, .id="Experiment")
  at<-at%>%group_by(Experiment,Generation)%>%
    mutate(value_med = summary(value)[[3]])%>%
    mutate(value_up=summary(value)[[5]])%>%
    mutate(value_down=summary(value)[[2]])
  g1<-subset(at, Experiment == "Baseline" & Generation == 99 )
  g2<-subset(at, Experiment == "Battery" & Generation == 99)
  print("Wilcoxon test p-value is:")
  wiltest<-wilcox.test(g1$value_med,g2$value_med)$p.value
  print(wiltest)
  anntext<-ifelse(!is.nan(wiltest),ifelse(wiltest<0.05,"p<0.001","p>0.05"),"NS")
  ypos<-max(at$value_up)
  p_directed[[i]]<-ggplot(at,aes(x=Generation,y=value_med,colour=Experiment))+geom_line()+
    geom_ribbon(aes(ymin=value_down, ymax=value_up,fill=Experiment),alpha=0.2,color=NA)+
    ylab(descriptors[i])+#ggtitle("Directed")
    theme(text = element_text(size=19),legend.position = "bottom")+
    annotate("text",x=97,y=ypos,label=anntext,colour="red")

  # print(descriptors[i])
  # print("\n")
  # print(wilcox.test(at$value,ot$value, alternative = "greater"))
  # 
  # readline(prompt="Press [enter] to continue")
}

plist<-list(p[[1]],p[[3]],p[[4]],p[[6]],
            p_directed[[1]],p_directed[[3]],p_directed[[4]],p_directed[[6]])
lay<-rbind(c(1,5),
           c(2,6),
           c(3,7),
           c(4,8))
pdf(file=paste0(savedir,"descriptor_body_2.pdf"), width = 10,height = 14)
grid.arrange(grobs=plist,layout_matrix =lay)
dev.off()


# pdf(file=paste0(savedir,"descriptor_controller.pdf"), width = 14,height = 10)
# do.call(grid.arrange,p[7:12])
# dev.off()

plist<-list(p[[7]],p[[11]],p[[12]],
            p_directed[[7]],p_directed[[11]],p_directed[[12]])
lay<-rbind(c(1,4),
           c(2,5),
           c(3,6))
pdf(file=paste0(savedir,"descriptor_controller_2.pdf"), width = 14,height = 10)
grid.arrange(grobs=plist,layout_matrix =lay)
dev.off()

plist<-list(p[[8]],p[[9]],p[[10]],
            p_directed[[8]],p_directed[[9]],p_directed[[10]])
lay<-rbind(c(1,4),
           c(2,5),
           c(3,6))
pdf(file=paste0(savedir,"Extra_descriptor_controller.pdf"), width = 14,height = 10)
grid.arrange(grobs=plist,layout_matrix =lay)
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

###########################
###Get top ten robots for Directed locomotion
########################
##Only robots of the last generation
top_baseline<-dbaseline99[order(dbaseline99$Fitness,decreasing=TRUE),]
top_baseline<-top_baseline[1:10,]
top_nsga<-dnsga99[order(dnsga99$Fitness,decreasing=TRUE),]
top_nsga<-top_nsga[1:10,]

getRobotCommand(top_baseline,"DirectedBaseline",T)
getRobotCommand(top_nsga,"DirectedmanagerNSGA",T)
getRobotCommand(top_baseline,"DirectedBaseline",F)
getRobotCommand(top_nsga,"DirectedmanagerNSGA",F)






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




  
    