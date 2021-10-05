#libraries
library(stringr)

# ##Function inputs
# experiment_dir<-"~/Battery-Robot/experiments/Battery_cppn/Data"
# experiment_base_name<-"Baseline"
# environment<-"plane"
# population_size<-10
# ## + select the experiment run to read (extra for)
# dirs<-list.dirs(path = experiment_dir, full.names = F,recursive = F)
# exp_dirs<-dirs[ grepl(experiment_base_name, dirs) ]
# experiment_name<-exp_dirs[1]


#'readDescriptors
#'
#'Function to read the phenotype descriptors of the robots generated at
#'the end of an evolution round
#'
#'@param experiment_dir The path where the data of the experiment is stored on the form "exp_name/"
#'@param experiment_name Name of the experiment 
#'@param environment As of now constant. Environment where robot is evaluated
#'@return data frame with the read descriptors and the robot_id

readDescriptors<-function(experiment_dir,experiment_name,environment="plane")
{
  #Input Format example
  #experiment_dir: "/Users/tarigarma/Battery-Robot/experiments/Battery_cppn/Data"
  #experiment_name: "Baseline1"
  #environment:"plane"
  
  descriptors<-c("branching", "branching_modules_count", "limbs", "extremities", 
                 "length_of_limbs", "extensiveness", "coverage", "joints", "hinge_count", 
                 "active_hinges_count", "brick_count", "touch_sensor_count", "brick_sensor_count", 
                 "proportion", "width", "height", "size", "absolute_size", "sensors", 
                 "symmetry", "avg_period", "dev_period", "avg_phase_offset", "dev_phase_offset", 
                 "avg_amplitude", "dev_amplitude", "avg_intra_dev_params", "avg_inter_dev_params", 
                 "sensors_reach", "recurrence", "synaptic_reception")
  
  descriptor_dir<-paste0(experiment_name,"/data_fullevolution/",environment,"/descriptors/")
  
  descriptor<-matrix(ncol=length(descriptors)+1,nrow=0)
  descriptor_files<-list.files(path=paste0(experiment_dir,"/",descriptor_dir), pattern = "phenotype_desc_robot_")
  for(i in 1:length(descriptor_files))
  {
    
    f_read<-read.table(file=paste0(experiment_dir,"/",descriptor_dir,descriptor_files[i]),
                       na.strings = "None",
                       colClasses = c("character","numeric"),
                       col.names = c("Descriptor","Value"),
                       sep=" ")
    rownames(f_read)<-f_read$Descriptor
    f_read<-f_read[descriptors,2]
    
    
    robot_id<-as.numeric(gsub(".*?([0-9]+).*", "\\1", descriptor_files[i]))
    descriptor<-rbind(descriptor,c(robot_id,f_read))
  }
  descriptor<-as.data.frame(descriptor)
  names(descriptor)<-c("robot_id",descriptors)
  
  
  return(descriptor)
}


#'readFitnessComponents
#'Function to read the results of the experiments in terms of 
#'Fitness: Displacement_Velocity as read in fitness folder
#'Batterry: Used battery charge as read in fitness folder
#'Balance: Core component as read in descriptors folder
#'
#'@param experiment_dir The path where the data of the experiment is stored on the form "exp_name/"
#'@param experiment_name Name of the experiment 
#'@param environment As of now constant. Environment where robot is evaluated
#'@param population_size Size of experiment population
#'@return List with all_robots: data frame with the read measures of all evaluated robots
#'                  selected_robots: data frame with only the selected robot population in each generation
readFitnessComponents<-function(experiment_dir, experiment_name, environment="plane", population_size)
{
  #Input Format example
  #experiment_dir: "/Users/tarigarma/Battery-Robot/experiments/Battery_cppn/Data"
  #experiment_name: "Baseline1"
  #environment:"plane"
  #population_size=10
  
  ##Read Gens and Robot_id
  all_dir<-list.dirs(path = paste0(experiment_dir,"/",experiment_name,"/"), full.names = F)
  generationsDF<-vector()
  for(i in all_dir){
    if(str_detect(i, paste0("selectedpop_",environment,"/selectedpop_"))){
      gen<-as.numeric(str_split(i, fixed(paste0("selectedpop_",environment,"/selectedpop_")))[[1]][2])
      gen_files<-list.files(paste0(experiment_dir,"/",experiment_name,"/selectedpop_",
                                   environment,"/selectedpop_",gen), pattern = "body_robot_*")
      robot_id<-as.numeric(gsub(".*?([0-9]+).*", "\\1", gen_files))
      generationsDF<-rbind(generationsDF,cbind(rep(gen,population_size),robot_id))
    }
  }
  generationsDF<-as.data.frame(generationsDF)
  names(generationsDF)[1]<-"Generation"
  
  fitness_dir<-paste0(experiment_name,"/data_fullevolution/",environment,"/fitness/")
  battery_dir<-paste0(experiment_name,"/data_fullevolution/",environment,"/battery/")
  balance_dir<-paste0(experiment_name,"/data_fullevolution/",environment,"/descriptors/")
  
  ##Read Fitness
  fitness<-vector()
  fitness_files<-list.files(path=paste0(experiment_dir,"/",fitness_dir), pattern = "fitness_")
  for(i in 1:length(fitness_files))
  {
    f_read<-read.table(file=paste0(experiment_dir,"/",fitness_dir,fitness_files[i]),
                       na.strings = "None",
                       colClasses = c("numeric"),
                       col.names = c("Fitness"))
    info_robot<-str_extract_all(sub('\\.','_',fitness_files[i]),pattern ="[^_]+")
    Generation<-info_robot[[1]][2]
    robot_id<-info_robot[[1]][4]
    fitness<-rbind(fitness,cbind(Generation,robot_id,f_read))
  }
  
  ##Read Battery
  battery<-vector()
  battery_files<-list.files(path=paste0(experiment_dir,"/",battery_dir), pattern = "battery_")
  for(i in 1:length(battery_files))
  {
    f_read<-read.table(file=paste0(experiment_dir,"/",battery_dir,battery_files[i]),
                       na.strings = "None",
                       colClasses = c("numeric"),
                       col.names = c("Battery"))
    info_robot<-str_extract_all(sub('\\.','_',battery_files[i]),pattern ="[^_]+")
    Generation<-info_robot[[1]][2]
    robot_id<-info_robot[[1]][4]
    battery<-rbind(battery,cbind(Generation,robot_id,f_read))
  }
  
  ##Read Balance
  balance<-vector()
  balance_files<-list.files(path=paste0(experiment_dir,"/",balance_dir), pattern = "behavior_desc_robot_")
  for(i in 1:length(balance_files))
  {
    countLines<-readLines(con = paste0(experiment_dir,"/",balance_dir,balance_files[i]))
    if(length(countLines)==1){ #None case
      f_read<-data.frame("Descriptor"=NA,"Value"=NA)
      HBalance<-subset(f_read, select = Value)
      names(HBalance)<-"Balance"
    }else {
      f_read<-read.table(file=paste0(experiment_dir,"/",balance_dir,balance_files[i]),
                         na.strings = "None",
                         colClasses = c("character","numeric"),
                         col.names = c("Descriptor","Value"),
                         sep=" ")
      HBalance <- subset(f_read, Descriptor =="head_balance",select = Value)
      names(HBalance)<-"Balance"
    }
    robot_id<-as.numeric(gsub(".*?([0-9]+).*", "\\1", balance_files[i]))
    balance<-rbind(balance,cbind(robot_id,HBalance))
  }
  
  ##Bring everything together in a big DF
  all_robots <- merge(fitness,battery, by = c("Generation","robot_id")) # displacement_velocity + battery
  all_robots <- merge(all_robots,balance, by="robot_id") #Balance
  selected_robots <- merge(generationsDF,all_robots) 
  selected_robots <- selected_robots[order(selected_robots$Generation),]
  return(list("allRobots"=all_robots, "genSelected"=selected_robots))
}
