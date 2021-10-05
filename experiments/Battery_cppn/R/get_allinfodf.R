##Get the complete data frames with the info of the resulting experiments

#setwd()
setwd("~/Battery-Robot/experiments/Battery_cppn/R/")
source("ReadFunctions.R")


args = commandArgs(trailingOnly=TRUE)  #input for experiment_base_name
#args = "Baseline"  #example
experiment_base_name<-args[1]
cat(experiment_base_name,"\n")
exp_dir<-paste0("~/Battery-Robot/experiments/Battery_cppn/Data/")
runs_dir<-list.dirs(path = exp_dir, full.names = F, recursive = F)
runs_dir<-runs_dir[ grepl(experiment_base_name, runs_dir) ]
#for Info
runs<-length(runs_dir)
cat("The experiment has ",runs," runs \n")
#loop to read for each run folder
allinfo_list<-list()
index_counter<-0
for (r in runs_dir){
  index_counter<-index_counter+1
  cat("index is: ", index_counter, " \n")
  cat("Current experiment forlder: ",r, " \n")
  measures_fitness<-readFitnessComponents(experiment_dir=exp_dir, experiment_name=r, population_size=10)$genSelected  #genSelected or allRobots
  run_number<-as.numeric(gsub(".*?([0-9]+).*", "\\1",r))
  measures_fitness$run<-rep(run_number,nrow(measures_fitness))
  descriptors<-readDescriptors(experiment_dir=exp_dir, experiment_name=r)
  all_info<-merge(measures_fitness,descriptors, by="robot_id")
  allinfo_list[[index_counter]]<-all_info
}

allinfo_df<-do.call(rbind,allinfo_list)
##Fila with the info of all the runs of experiment_base_name
saveRDS(allinfo_df, file=paste0(experiment_base_name,"_generationInfo.RDS"))