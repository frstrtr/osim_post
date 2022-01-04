library(R.utils)
library(logging)
library("XML")
library("methods")
library(ggplot2)
library(reshape2)
library (scales) 

logReset();
basicConfig('DEBUG');
addHandler(writeToConsole);
addHandler(writeToFile, file=paste("output-",System$getHostname(),'-',Sys.Date(),".log",sep=''), level='DEBUG');
save_to_file = TRUE
#------------------------------------------------------------------#
sargs <- commandArgs(TRUE)
path <- args[1]
athlete <- args[2]
#------------------------------------------------------------------#
files <- list.files(paste(path,'/data/',athlete,sep=''), all.files = TRUE, pattern = ".tsv")

types = c("jo_", "j_", "jo100_1", "jo100_2", "jo300_1", "jo300_2", "jo500_1", "jo500_2", "js_")
jump_list <- list()
for(i in types)
{
  for (j in files)
  {
    if((length(grep(i, j)) != 0)&&(length(grep('_f_', j)) == 0)&&(length(grep('_a', j)) == 0))
    {
      key = i
      value <- gsub(".tsv", "", j) 
      jump_list[[ key ]] <- value
    }
  }	
}

pos_source <- read.table(paste(path,'/data/',athlete,'/', jump_list["j_"], '_model_BodyKinematics_pos_global.sto',sep=''), skip=18, header = TRUE)
COM_1 = mean(pos_source$center_of_mass_Y[seq(1:20)] )

  
#------------------------------------------------------------------#
model_xml_file = ""
for (jump in jump_list)
{
  experiment = jump
  #------------------------------------------------------------------#
  model_xml_file = paste(path,'/data/',athlete,'/', experiment, '_gait_with_arms_scaled.osim',sep='')
  #------------------------------------------------------------------#
  logdebug('loading antropometry...');
  # antropometry = scan(file = paste(path,'/data/',athlete,'/antrop.txt',sep=''), what = character())
  antropometry = scan(file = paste('antrop.txt',sep=''), what = character())
  sex <- antropometry[2]
  weight <- as.numeric(antropometry[4])
  height <- as.numeric(antropometry[6])
  #------------------------------------------------------------------#
  logdebug('loading source data files (pos, vel, acc, body_forces, inverse_dynamics, mot)...');
  pos_source <- read.table(paste(path,'/data/',athlete,'/', experiment, '_model_BodyKinematics_pos_global.sto',sep=''), skip=18, header = TRUE)
  vel_source <- read.table(paste(path,'/data/',athlete,'/', experiment, '_model_BodyKinematics_vel_global.sto',sep=''), skip=18, header = TRUE)
  acc_source <- read.table(paste(path,'/data/',athlete,'/', experiment, '_model_BodyKinematics_acc_global.sto',sep=''), skip=18, header = TRUE)
  body_forces_source <- read.table(paste(path,'/data/',athlete,'/', experiment, '_body_forces_at_joints.sto',sep=''), skip=6, header = TRUE)
  inverse_dynamics_source <- read.table(paste(path,'/data/',athlete,'/', experiment, '_inverse_dynamics.sto',sep=''), skip=6, header = TRUE)
  mot_source <- read.table(paste(path,'/data/',athlete,'/', experiment, '.mot',sep=''), skip=10, header = TRUE)
  ext_forces_source <- read.table(paste(path,'/data/',athlete,'/', experiment, '_ext_forces.mot',sep=''), skip=10, header = TRUE)
  frames <- length(mot_source$time)
  logdebug(paste('loaded', frames, 'frames'))
  #------------------------------------------------------------------#
  logdebug('determining the phases of the jump...')
  center_of_mass_Y_delta = c(pos_source$center_of_mass_Y, pos_source$center_of_mass_Y[frames]) - c(pos_source$center_of_mass_Y[1], pos_source$center_of_mass_Y) 
  center_of_mass_Y_delta = center_of_mass_Y_delta[2:(frames-1)]
  center_of_mass_X_delta = c(pos_source$center_of_mass_X, pos_source$center_of_mass_X[frames]) - c(pos_source$center_of_mass_X[1], pos_source$center_of_mass_X) 
  center_of_mass_X_delta = center_of_mass_X_delta[2:(frames-1)]
  center_of_mass_Z_delta = c(pos_source$center_of_mass_Z, pos_source$center_of_mass_Z[frames]) - c(pos_source$center_of_mass_Z[1], pos_source$center_of_mass_Z) 
  center_of_mass_Z_delta = center_of_mass_Z_delta[2:(frames-1)]
  #------------------------------------------------------------------#
  logdebug('detecting ground release point...')
  combined_vertical_force = ext_forces_source$ground_force_vy + ext_forces_source$ground_force_vy
  zero_force = combined_vertical_force[length(combined_vertical_force)]
  force_tolerance = 0.1
  force_max_time = min(ext_forces_source$time[combined_vertical_force>=max(combined_vertical_force)])
  force_max_frame = frames-sum(mot_source$time>=force_max_time)-2
  fitting_times = ext_forces_source$time[(combined_vertical_force<=zero_force*(1+ force_tolerance))]
  eject_time = min(fitting_times[fitting_times >= force_max_time])
  eject_frame = frames-sum(mot_source$time>=eject_time)-2
  logdebug(paste("calculated platform eject time (sec): ", eject_time))
  logdebug(paste("calculated platform eject frame: ", eject_frame))
  #------------------------------------------------------------------#
  #SMOTHING
  center_of_mass_Y_delta_smoothed = smooth.spline(center_of_mass_Y_delta, spar=0.4)$y
  center_of_mass_X_delta_smoothed = smooth.spline(center_of_mass_X_delta, spar=0.4)$y
  center_of_mass_Z_delta_smoothed = smooth.spline(center_of_mass_Z_delta, spar=0.4)$y
  center_of_mass_Y_smoothed = smooth.spline(pos_source$center_of_mass_Y, spar=0.4)$y
  #------------------------------------------------------------------#
  logdebug('detecting movement point (COM goes down)...')
  ext_frames = length(ext_forces_source$time)
  movement_tolerance = 1e-3
  movement_force = 50
  firstframe = 3
  frames_array = seq(firstframe, ext_frames)
  if((length(grep("jump_jo_", experiment)) == 0)&&(length(grep("jump_j_", experiment)) == 0))
  firstframe = min(frames_array[abs(ext_forces_source$ground_force_vy)> movement_force])
  frames_array = seq(firstframe, ext_frames)
  counter = 1
  for(delta_move in center_of_mass_Y_delta_smoothed)
  {
    if(abs(delta_move)>movement_tolerance)
      break  
    counter = counter + 1
  }
  movement_frame = counter
  if((length(grep("jump_jo_", experiment)) == 0)&&(length(grep("jump_j_", experiment)) == 0)&&(length(grep("jump_js_", experiment)) == 0))
  {movement_frame = firstframe
    movement_time = ext_forces_source$time[movement_frame]
    c=0
    for(i in pos_source$time)
    {
      if(i >= movement_time)
        break
      c=c+1
    }
    movement_frame = c
  }
  movement_time = pos_source$time[movement_frame]
  logdebug(paste("calculated movement start time (sec): ", movement_time))
  logdebug(paste("calculated movement start frame: ", movement_frame))
  #------------------------------------------------------------------#
  logdebug('detecting lowest COM point...')
  lowest_time = min(pos_source$time[center_of_mass_Y_smoothed <= min(center_of_mass_Y_smoothed)])
  lowest_frame = min(seq(1, frames)[center_of_mass_Y_smoothed <= min(center_of_mass_Y_smoothed)])
  logdebug(paste("calculated lowest COM time (sec): ", lowest_time))
  logdebug(paste("calculated lowest COM frame: ", lowest_frame))
  #------------------------------------------------------------------#
  logdebug('detecting highest COM point...')
  highest_frame = min(seq(1,frames)[pos_source$center_of_mass_Y==max(pos_source$center_of_mass_Y[seq(firstframe, frames)])])
  highest_time = mot_source$time[highest_frame]
  logdebug(paste("calculated highest COM time (sec): ", highest_time))
  logdebug(paste("calculated highest COM frame: ", highest_frame))
  #------------------------------------------------------------------#
  logdebug('loading XML model to get segment weights')
  model_xml = xmlParse(file = model_xml_file)
  get_segment_mass <- function(segment)
  {
    #OpenSimDocument -> Model -> BodySet -> objects -> Body name = "tibia_r"
    seg = getNodeSet(model_xml, paste("//Body[@name='",segment,"']/mass",sep=''))
    value = xmlValue(seg[[1]])
    as.numeric(value)
  }
  #------------------------------------------------------------------#
  #support_time
  Separation_time = eject_time - movement_time
  Sep_amort_time = lowest_time - movement_time
  Sep_repul_time = eject_time - lowest_time
  #------------------------------------------------------------------#
  #Inertia
  mass_foot = get_segment_mass("toes_r")+ get_segment_mass("calcn_r") + get_segment_mass("talus_r")
  mass_shank = get_segment_mass("tibia_r")
  mass_thigh = get_segment_mass("femur_r")
  I_foot = ((get_segment_mass("toes_r") + get_segment_mass("calcn_r") + get_segment_mass("talus_r"))/660.49)+((get_segment_mass("toes_r") + get_segment_mass("calcn_r") + get_segment_mass("talus_r"))/4716100)
  I_shank = ((get_segment_mass("tibia_r"))/789.61)+((get_segment_mass("tibia_r"))/278784)
  I_thigh = ((get_segment_mass("femur_r"))/712.89)+((get_segment_mass("femur_r"))/291600)
  #------------------------------------------------------------------#
  #ANGLES
  logdebug('ANGLES calculating')
  angle_ankle_r = mot_source$ankle_angle_r +90
  angle_ankle_r = smooth.spline(angle_ankle_r, spar = 0.5)$y
  angle_knee_r = 180 - abs(mot_source$knee_angle_r)
  angle_knee_r = smooth.spline(angle_knee_r, spar = 0.5)$y
  angle_hip_r = 180 - abs(mot_source$hip_flexion_r)
  angle_hip_r = smooth.spline(angle_hip_r, spar = 0.5)$y
  
  angles_file = paste(path,'/data/',athlete,'/',experiment,'_angles.csv',sep='')
  columns = c(
    'angle_ankle_r', 
    'angle_knee_r',
    'angle_hip_r'
  )
  target_array = matrix(nrow=(frames),ncol=length(columns))
  target_array[,1] = angle_ankle_r
  target_array[,2] = angle_knee_r
  target_array[,3] = angle_hip_r
  
  write.table(target_array, file = angles_file, append = FALSE, col.names = columns, row.names=FALSE, dec=',', sep=';')
  #------------------------------------------------------------------#
  #ANGULAR_VELOCITES
  logdebug('ANGULAR_VELOCITES calculating')
  angular_velocity_ankle_source = (c(angle_ankle_r, angle_ankle_r[frames]) - c(angle_ankle_r[1], angle_ankle_r)) * 400
  angular_velocity_ankle_source = angular_velocity_ankle_source[1:(frames-1)]
  angular_velocity_knee_source =(c(angle_knee_r,angle_knee_r[frames]) - c(angle_knee_r[1], angle_knee_r)) * 400
  angular_velocity_knee_source = angular_velocity_knee_source[1:(frames-1)]
  angular_velocity_hip_source =(c(angle_hip_r,angle_hip_r[frames]) - c(angle_hip_r[1],angle_hip_r)) * 400
  angular_velocity_hip_source = angular_velocity_hip_source[1:(frames-1)]
  
  angular_velocity_file = paste(path,'/data/',athlete,'/',experiment,'_angular_velocity.csv',sep='')
  columns = c(
    'angular_velocity_ankle', 
    'angular_velocity_knee',
    'angular_velocity_hip'
  )
  target_array = matrix(nrow=(frames-1),ncol=length(columns))
  target_array[,1] = angular_velocity_ankle_source
  target_array[,2] = angular_velocity_knee_source
  target_array[,3] = angular_velocity_hip_source
  
  write.table(target_array, file = angular_velocity_file, append = FALSE, col.names = columns, row.names=FALSE, dec=',', sep=';')
  
  #------------------------------------------------------------------#
  #time_source
  time_source <- inverse_dynamics_source$time
  #------------------------------------------------------------------#
  #ENERGY_Ekin
  logdebug('ENERGY calculating')
  
  Ekin_pelvis = get_segment_mass("pelvis") * ((vel_source$pelvis_X)^2 + (vel_source$pelvis_Y)^2 + (vel_source$pelvis_Z)^2)*0.5
  Ekin_pelvis= smooth.spline(Ekin_pelvis, spar= 0.7)$y
  Ekin_femur = get_segment_mass("femur_r") * ((vel_source$femur_r_X)^2 + (vel_source$femur_r_Y)^2 + (vel_source$femur_r_Z)^2)*0.5
  Ekin_femur= smooth.spline(Ekin_femur, spar= 0.7)$y
  Ekin_tibia  = get_segment_mass("tibia_r") * ((vel_source$tibia_r_X)^2 + (vel_source$tibia_r_Y)^2 + (vel_source$tibia_r_Z)^2)*0.5
  Ekin_tibia= smooth.spline(Ekin_tibia, spar= 0.7)$y
  Ekin_talus  = get_segment_mass("talus_r") * ((vel_source$talus_r_X)^2 + (vel_source$talus_r_Y)^2 + (vel_source$talus_r_Z)^2)*0.5
  Ekin_talus= smooth.spline(Ekin_talus, spar= 0.7)$y
  Ekin_calcn  = get_segment_mass("calcn_r") * ((vel_source$calcn_r_X)^2 + (vel_source$calcn_r_Y)^2 + (vel_source$calcn_r_Z)^2)*0.5
  Ekin_calcn= smooth.spline(Ekin_calcn, spar= 0.7)$y
  Ekin_toes  = get_segment_mass("toes_r") * ((vel_source$toes_r_X)^2 + (vel_source$toes_r_Y)^2 + (vel_source$toes_r_Z)^2)*0.5
  Ekin_toes= smooth.spline(Ekin_toes, spar= 0.7)$y
  Ekin_torso  = get_segment_mass("torso") * ((vel_source$torso_X)^2 + (vel_source$torso_Y)^2 + (vel_source$torso_Z)^2)*0.5
  Ekin_torso= smooth.spline(Ekin_torso, spar= 0.7)$y
  Ekin_humerus  = get_segment_mass("humerus_r") * ((vel_source$humerus_r_X)^2 + (vel_source$humerus_r_Y)^2 + (vel_source$humerus_r_Z)^2)*0.5
  Ekin_humerus= smooth.spline(Ekin_humerus, spar= 0.7)$y
  Ekin_ulna  = get_segment_mass("ulna_r") * ((vel_source$ulna_r_X)^2 + (vel_source$ulna_r_Y)^2 + (vel_source$ulna_r_Z)^2)*0.5
  Ekin_ulna= smooth.spline(Ekin_ulna, spar= 0.7)$y
  Ekin_radius  = get_segment_mass("radius_r") * ((vel_source$radius_r_X)^2 + (vel_source$radius_r_Y)^2 + (vel_source$radius_r_Z)^2)*0.5
  Ekin_radius= smooth.spline(Ekin_radius, spar= 0.7)$y
  Ekin_hand  = get_segment_mass("hand_r") * ((vel_source$hand_r_X)^2 + (vel_source$hand_r_Y)^2 + (vel_source$hand_r_Z)^2)*0.5
  Ekin_hand= smooth.spline(Ekin_hand, spar= 0.7)$y
  
  Ekin1 = Ekin_pelvis + 2*Ekin_femur + 2*Ekin_tibia + 2*Ekin_talus + 2*Ekin_calcn + 2*Ekin_toes +2*Ekin_humerus + 2*Ekin_ulna +2*Ekin_radius +2*Ekin_hand
  Ekin1 = Ekin1[2:frames]
  
  Ekin_foot_right = I_foot * (((angular_velocity_ankle_source)*3.14/180)^2)*0.5
  Ekin_shank_right = I_shank * (((angular_velocity_knee_source)*3.14/180)^2)*0.5
  Ekin_thigh_right = I_thigh * (((angular_velocity_hip_source)*3.14/180)^2)*0.5
  Ekin2 = Ekin_foot_right + Ekin_shank_right + Ekin_thigh_right
  Ekin = Ekin1 + (2*Ekin2)
  Ekin_table = Ekin[seq(movement_frame, highest_frame)]
  Ekin_amort_start = Ekin [1]
  Ekin_amort_finish = Ekin [lowest_frame]
  Ekin_amort = Ekin_amort_finish - Ekin_amort_start
  Ekin_amort = round(Ekin_amort,2)
  #------------------------------------------------------------------#
  #EPOT
  Epot_pelvis = get_segment_mass("pelvis") *9.81 * (pos_source$pelvis_Y)^2 
  Epot_femur = get_segment_mass("femur_r") * 9.81 * (pos_source$femur_r_Y)^2 
  Epot_tibia  = get_segment_mass("tibia_r") * 9.81 *  (pos_source$tibia_r_Y)^2 
  Epot_talus  = get_segment_mass("talus_r") * 9.81 *  (pos_source$talus_r_Y)^2 
  Epot_calcn  = get_segment_mass("calcn_r") *9.81 *  (pos_source$calcn_r_Y)^2 
  Epot_toes  = get_segment_mass("toes_r") * 9.81 * (pos_source$toes_r_Y)^2 
  Epot_torso  = get_segment_mass("torso") * 9.81 * (pos_source$torso_Z)^2
  Epot_humerus  = get_segment_mass("humerus_r") * 9.81* (pos_source$humerus_r_Y)^2 
  Epot_ulna  = get_segment_mass("ulna_r") * 9.81 * (pos_source$ulna_r_Y)^2
  Epot_radius  = get_segment_mass("radius_r") * 9.81 * (pos_source$radius_r_Y)^2
  Epot_hand  = get_segment_mass("hand_r") * 9.81 * (pos_source$hand_r_Y)^2 

  Epot = Epot_pelvis + 2*Epot_femur + 2*Epot_tibia + 2*Epot_talus + 2*Epot_calcn + 2*Epot_toes + 2*Epot_torso + 2*Epot_humerus + 2*Epot_ulna + 2*Epot_radius + 2*Epot_hand
  Epot_table = Epot[seq(movement_frame, highest_frame)]
  #------------------------------------------------------------------#
  #EMECH
  Emech = Ekin + Epot[seq(1, frames-1)]
  Emech_table =Emech[seq(movement_frame, highest_frame)]
  f = highest_frame - movement_frame +1
  enrgy_time = time_source[seq(movement_frame, highest_frame)]
  Emech_max = Emech [eject_frame]
  Emech_min = Emech [lowest_frame]
  Emech_SUM = Emech_max - Emech_min
  
  #------------------------------------------------------------------#
  energy_file = paste(path,'/data/',athlete,'/',experiment,'_energy.csv',sep='')
  columns = c(
    'time',
    'Ekin', 
    'Epot',
    'Emech'
    
  )
  target_array = matrix(nrow=(f),ncol=length(columns))
  target_array[,1] = enrgy_time
  target_array[,2] = Ekin_table
  target_array[,3] = Epot_table
  target_array[,4] = Emech_table
  
  write.table(target_array, file = energy_file, append = FALSE, col.names = columns, row.names=FALSE, dec=',', sep=';')
  
  #------------------------------------------------------------------#
 
  #MOMENTs
  logdebug('MOMENTs calculating')
  
  ankle_angle_r_moment = inverse_dynamics_source$ankle_angle_r_moment
  ankle_angle_r_moment =ankle_angle_r_moment[1:frames]
  knee_angle_r_moment = inverse_dynamics_source$knee_angle_r_moment
  knee_angle_r_moment = knee_angle_r_moment[1:frames]
  hip_flexion_r_moment = inverse_dynamics_source$hip_flexion_r_moment
  hip_flexion_r_moment = hip_flexion_r_moment[1:frames]
  
  Moment_ankle_right_source = (ankle_angle_r_moment)
  Moment_ankle_right_source = smooth.spline(Moment_ankle_right_source, spar = 0.7)$y
  Moment_knee_right_source = (knee_angle_r_moment)
  Moment_knee_right_source = smooth.spline(Moment_knee_right_source, spar = 0.7)$y
  Moment_hip_right_source = -(hip_flexion_r_moment)
  Moment_hip_right_source = smooth.spline(Moment_hip_right_source, spar = 0.8)$y
  Moment_right_sum_source = Moment_ankle_right_source+Moment_knee_right_source+Moment_hip_right_source
  
  Moment_ankle_right = Moment_ankle_right_source[seq(movement_frame, highest_frame)]
  Moment_knee_right = Moment_knee_right_source[seq(movement_frame, highest_frame)]
  Moment_hip_right = Moment_hip_right_source[seq(movement_frame, highest_frame)]
  Moment_right_sum = Moment_right_sum_source[seq(movement_frame, highest_frame)]
  
  time_source_moment = time_source[seq(movement_frame, highest_frame)]
  
  moments_file = paste(path,'/data/',athlete,'/',experiment,'_moments.csv',sep='')
  columns = c(
    'time',
    'Moment_ankle_right', 
    'Moment_knee_right',
    'Moment_hip_right',
    'Moment_right_sum'
  )
  target_array = matrix(nrow=(f),ncol=length(columns))
  target_array[,1] = time_source_moment
  target_array[,2] = Moment_ankle_right
  target_array[,3] = Moment_knee_right
  target_array[,4] = Moment_hip_right
  target_array[,5] = Moment_right_sum
  
  write.table(target_array, file = moments_file, append = FALSE, col.names = columns, row.names=FALSE, dec=',', sep=';')
  #------------------------------------------------------------------#
  #POWERs
  logdebug('POWERs calculating')
  
  Power_ankle_right_source = (Moment_ankle_right_source[seq(2, frames)]) * ((angular_velocity_ankle_source) *pi/180/weight)
  Power_ankle_right_source = smooth.spline(Power_ankle_right_source, spar = 0.8)$y
  Power_knee_right_source = (Moment_knee_right_source[seq(2, frames)]) * ((angular_velocity_knee_source)*pi/180/weight)
  Power_knee_right_source = smooth.spline(Power_knee_right_source, spar = 0.5)$y
  Power_hip_right_source = (Moment_hip_right_source[seq(2, frames)]) * ((angular_velocity_hip_source)*pi/180/weight)
  Power_hip_right_source = smooth.spline(Power_hip_right_source, spar = 0.5)$y
  Power_sum_source = ((Power_knee_right_source) + (Power_ankle_right_source) + (Power_hip_right_source))
  time_source_power = time_source [seq(movement_frame, highest_frame)]
  
  Power_ankle_right = Power_ankle_right_source[seq(movement_frame, highest_frame)]
  Power_knee_right = Power_knee_right_source[seq(movement_frame, highest_frame)]
  Power_hip_right = Power_hip_right_source[seq(movement_frame, highest_frame)]
  Power_sum = Power_sum_source[seq(movement_frame, highest_frame)]
    
  powers_file = paste(path,'/data/',athlete,'/',experiment,'_powers.csv',sep='')
  columns = c(
    'time',
    'Power_ankle_right', 
    'Power_knee_right',
    'Power_hip_right',
    'Power_sum'
  )
  target_array = matrix(nrow=(f),ncol=length(columns))
  target_array[,1] = time_source_power
  target_array[,2] = Power_ankle_right
  target_array[,3] = Power_knee_right
  target_array[,4] = Power_hip_right
  target_array[,5] = Power_sum
  
  write.table(target_array, file = powers_file, append = FALSE, col.names = columns, row.names=FALSE, dec=',', sep=';')
  Power_ankle_right_max= max(Power_ankle_right)
  Power_knee_right_max= max(Power_knee_right)
  Power_hip_right_max= max(Power_hip_right)
  Power_sum_max= max(Power_sum[seq(1,f-1)])
  #------------------------------------------------------------------#
  #Work

  Work_ankle = (Moment_ankle_right_source[seq(1, frames-1)])*angular_velocity_ankle_source*0.0025*pi/180
  Work_ankle = smooth.spline(Work_ankle,spar = 0.8)$y
  Work_knee = (Moment_knee_right_source[seq(1, frames-1)])*angular_velocity_knee_source*0.0025*pi/180
  Work_knee = smooth.spline(Work_knee,spar = 0.4)$y
  Work_hip = (Moment_hip_right_source[seq(1, frames-1)])*angular_velocity_hip_source*0.0025*pi/180
  Work_hip = smooth.spline(Work_hip,spar = 0.7)$y
  Work = 2*(Work_ankle+Work_knee+Work_hip)
  Work = smooth.spline(Work,spar = 0.7)$y
  
  Work_ankle = Work_ankle[seq(movement_frame, highest_frame)]
  Work_knee = Work_knee[seq(movement_frame, highest_frame)]
  Work_hip = Work_hip[seq(movement_frame, highest_frame)]
  Work = Work[seq(movement_frame, highest_frame)]
  
  time_source_work = time_source[seq(movement_frame, highest_frame)]
  
  W = Work [seq(lowest_frame, eject_frame)]
  W_SUM = sum (W)
  
  sum_file = paste(path,'/data/',athlete,'/',experiment,'_sum_file.csv',sep='')
  columns = c(
    'Energy_summ',
    'Work_summ'
  )
  target_array = matrix(nrow=(1),ncol=length(columns))
  target_array[,1] = Emech_SUM
  target_array[,2] = W_SUM
  
  write.table(target_array, file = sum_file, append = FALSE, col.names = columns, row.names=FALSE, dec=',', sep=';')
  
  work_file = paste(path,'/data/',athlete,'/',experiment,'_works.csv',sep='')
  columns = c(
    'time',
    'Work_ankle', 
    'Work_knee',
    'Work_hip',
    'Work'
  )
  target_array = matrix(nrow=(f),ncol=length(columns))
  target_array[,1] = time_source_work
  target_array[,2] = Work_ankle
  target_array[,3] = Work_knee
  target_array[,4] = Work_hip
  target_array[,5] = Work
 
  write.table(target_array, file = work_file, append = FALSE, col.names = columns, row.names=FALSE, dec=',', sep=';')
  #------------------------------------------------------------------#
  #STIFFNESS
  logdebug('STIFFNESS calculating')
  amort_com_max = max(pos_source$center_of_mass_Y[seq(1, eject_frame)])
  amort_com_min = min(pos_source$center_of_mass_Y[seq(1, eject_frame)])
  max_sum_of_left_and_right_force_Y = max(ext_forces_source$ground_force_vy + ext_forces_source$X1_ground_force_vy)
  max_avg_of_left_and_right_force_Y = max((ext_forces_source$ground_force_vy + ext_forces_source$X1_ground_force_vy)/2)
  amort_stiffness_K_sum = max_sum_of_left_and_right_force_Y / (amort_com_max - amort_com_min) 
  amort_stiffness_K_avg = max_avg_of_left_and_right_force_Y / (amort_com_max - amort_com_min)
  
  max_moment_ankle = max(abs(Moment_ankle_right_source[seq(1, eject_frame)]))
  max_moment_knee = max(abs(Moment_knee_right_source[seq(1,  eject_frame)]))
  max_moment_hip = max(abs(Moment_hip_right_source[seq(1, eject_frame)]))
  max_angular_velociry_ankle = max(abs(angular_velocity_ankle_source[seq(1, eject_frame)]))
  max_angular_velociry_knee = max(abs(angular_velocity_knee_source[seq(1, eject_frame)]))
  max_angular_velociry_hip = max(abs(angular_velocity_hip_source[seq(1, eject_frame)]))
  
  Stiffness_a = max_moment_ankle/max_angular_velociry_ankle * 100
  Stiffness_k = max_moment_knee/max_angular_velociry_knee * 100
  Stiffness_h = max_moment_hip/max_angular_velociry_hip * 100
  
  Stiffness_sum = Stiffness_a+Stiffness_k+Stiffness_h
  
  Stiffness_ankle = Stiffness_a*100/Stiffness_sum
  Stiffness_knee = Stiffness_k*100/Stiffness_sum
  Stiffness_hip = Stiffness_h*100/Stiffness_sum
  #------------------------------------------------------------------#
  #START_FORCE
  P_athlet = weight * 9.8
  start_force_source <- ext_forces_source$ground_force_vy
  l = length(start_force_source)
  start_force_source = smooth.spline(start_force_source,spar = 0.7)$y
  max_start_force = max(start_force_source)
  c=0
  for(i in start_force_source)
  {
    if(i>= max_start_force)
      break
    c= c+1
  }
  start_force_source = start_force_source [seq(c, l)]
  count = 0  
  for (i in start_force_source)
  {
    if(i <= P_athlet)
      break
    count = count+1
  }
  count = count + c
  start_force_time = ext_forces_source[count,1]
  phase_split_time = ext_forces_source[c,1]
  start_force_time = start_force_time - phase_split_time
  start_force = P_athlet/start_force_time*0.001
  #------------------------------------------------------------------#
  #COM
  
  COM_2 = pos_source$center_of_mass_Y[highest_frame]
  COM = COM_2 - COM_1
  COM = round(COM, 2)
  
  COM_Y <- pos_source$center_of_mass_Y
  if (save_to_file)
  {
    png_file = paste(path,'/data/',athlete,'/',experiment, '_plot_2_jump_phases.png', sep = "")
    png(png_file, width=1600, height=1000, units="px", res=106)
  }
  plot(COM_Y, ann=FALSE, type="l", col="red", lwd=2)
  lines(x=c(-10000,10000), y=c(0,0), col='black')
  title(main=paste("Jump Phases Breakdown (Using smoothed Center of Mass + Force Reactions)", sep=''))
  title(ylab="Delta (m)")
  title(xlab=paste("Frames", sep="")) 
  legend("topleft", c("center_of_mass VERTICAL delta smoothed", "center_of_mass TRANSVERSAL delta smoothed","center_of_mass LONGITUDINAL delta smoothed",
                      paste("Frame ", movement_frame, " (" , movement_time, " sec): start of movement (Phase 1 begins)", sep=""), paste("Frame ", lowest_frame, " (" , lowest_time, " sec): center of mass it at lowest point (Phase 1 ends, 2 begins)",sep=''), paste("Frame ", eject_frame, " (" , eject_time, " sec): ejecting from the platform (Phase 2 ends, Phase 3 begins)", sep=''), paste("Frame ", highest_frame, " (" , highest_time, " sec): center of mass is at highest point (Phase 3 ends)", sep='')), 
         col=c('red', "darkgreen","blue", "orange", "brown", "purple", "darkgreen"), lty=c(1,1,1,2,2,2,2), lwd=c(2,1,1,2,2,2,2));
  top = max(COM_Y)  
  bottom = min(COM_Y)  
  lines(x=c(eject_frame, eject_frame), y=c(bottom, top), lty=2, lwd=2, col="purple") 
  lines(x=c(movement_frame, movement_frame), y=c(bottom, top), lty=2, lwd=2, col="orange") 
  lines(x=c(lowest_frame, lowest_frame), y=c(bottom, top), lty=2, lwd=2, col="brown") 
  lines(x=c(highest_frame, highest_frame), y=c(bottom, top), lty=2, lwd=2, col="darkgreen") 
  if(save_to_file) dev.off();
  
  #------------------------------------------------------------------#
  #EFF
  Ekin_sum = mean(Ekin[seq(eject_frame+1, frames-1)])
  Epot_sum = mean(Epot[seq(1, eject_frame)])
  Eff = (Ekin_sum)/(Epot_sum) *100
  #------------------------------------------------------------------#
  #TABLE
  summary_file = paste(path,'/data/',athlete,'/',experiment,'_table.csv',sep='')
  target_array = matrix(nrow=2,ncol=15)
  target_array[1,1] = 'COM'
  target_array[1,2] = 'Separation_time'
  target_array[1,3] = 'Amort_time'
  target_array[1,4] = 'Push_off_time'
  target_array[1,5] = 'Effectiv'
  target_array[1,6] = 'Stiffness'
  target_array[1,7] = 'start_force'
  target_array[1,8] = 'Stiffness_ankle'
  target_array[1,9] = 'Stiffness_knee'
  target_array[1,10] = 'Stiffness_hip'
  target_array[1,11] = 'Power_ankle_right_max' 
  target_array[1,12] = 'Power_knee_right_max' 
  target_array[1,13] = 'Power_hip_right_max' 
  target_array[1,14] = 'Power_sum_max' 
  target_array[1,15] = 'Ekin_amort'
  
  target_array[2,1] = COM
  target_array[2,2] = Separation_time
  target_array[2,3] = Sep_amort_time
  target_array[2,4] = Sep_repul_time
  target_array[2,5] = Eff
  target_array[2,6] = amort_stiffness_K_avg
  target_array[2,7] = start_force
  target_array[2,8] = Stiffness_ankle
  target_array[2,9] = Stiffness_knee
  target_array[2,10] = Stiffness_hip
  target_array[2,11] = Power_ankle_right_max
  target_array[2,12] = Power_knee_right_max
  target_array[2,13] = Power_hip_right_max
  target_array[2,14] = Power_sum_max
  target_array[2,15] = Ekin_amort
  
  write.table(target_array, file = summary_file, append = FALSE, col.names = FALSE, row.names=FALSE, dec=',', sep=';')
  
}

#------------------------------------------------------------------#
#GRAPHICS

tables <- list.files(paste(path,'/data/',athlete, sep=''), all.files = TRUE, pattern = ".csv")
types = c("jo_", "j_", "jo100_1", "jo100_2", "jo300_1", "jo300_2", "jo500_1", "jo500_2", "js_")
table_list <- list()
for(i in types)
{
  for (j in tables)
  {
    if((length(grep(i, j)) != 0)&&(length(grep('_table', j)) != 0))
    {
      key = i
      value <- gsub(".csv", "", j) 
      table_list[[ key ]] <- value
    }
  }	
}
#------------------------------------------------------------------#
#EFFECTIVNOST
Eff <- list()
n=1
for (table in table_list)
{ 
  normalized_file = table
  file = paste(path,"/data/",athlete,"/",normalized_file,".csv", sep = '')
  Eff_jump_source <- read.csv(file=file, sep=";")
  Eff_jump = Eff_jump_source$Eff
  Eff[n] <- Eff_jump
  n = n+1
} 
x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
y <- c(Eff)
if (save_to_file)
{
  png_file = paste(path,'/data/',athlete,'/','Jumps_effectiv.png', sep = '')
  png(png_file, width=1600, height=1000, units="px", res=106)
}
plot (x, y, type = "l",col="red")
axis(1, at=1:8, lab=c("Jump_jo","Jump_j","Jump_jo100","Jump_j100","Jump_jo300", "Jump_j300", "Jump_jo500", "Jump_j500", "Jump_js"))
title(main=paste("Jump_effectiv", sep=''))
title(ylab="Effectiv")
title(xlab=paste("Jumps", sep="")) 
if (save_to_file) dev.off();
#------------------------------------------------------------------#  
#JOINT_STIFFNESS
J_Stiff_ankle <- list()
J_Stiff_knee <- list()
J_Stiff_hip <- list()
t=1
target_array = matrix(nrow=9,ncol=4)
target_array[1,1] = 'jump_jo'
target_array[2,1] = 'jump_j'
target_array[3,1] = 'jump_jo100'
target_array[4,1] = 'jump_j100'
target_array[5,1] = 'jump_jo300'
target_array[6,1] = 'jump_j300'
target_array[7,1] = 'jump_jo500'
target_array[8,1] = 'jump_j500'
target_array[9,1] = 'jump_js'
for (table in table_list)
{ 
  normalized_file = table
  file = paste(path,"/data/",athlete,"/",normalized_file,".csv", sep = '')
  J_Stiff_source <- read.csv(file=file, sep=";")
  J_Stiffness_ankle = J_Stiff_source$Stiffness_ankle
  J_Stiffness_knee = J_Stiff_source$Stiffness_knee
  J_Stiffness_hip = J_Stiff_source$Stiffness_hip
  J_Stiff_ankle [t] <- J_Stiffness_ankle
  J_Stiff_knee [t] <- J_Stiffness_knee
  J_Stiff_hip [t] <- J_Stiffness_hip
  summary_file = paste(path,'/data/',athlete,'/', 'j_stiff.csv',sep='')
  target_array[t,2] = J_Stiff_ankle[[t]]
  target_array[t,3] = J_Stiff_knee[[t]]
  target_array[t,4] = J_Stiff_hip[[t]]
  t=t+1
}
write.table(target_array, file = summary_file, append = FALSE, col.names = FALSE, row.names=FALSE, dec=',', sep=';')

if (save_to_file)
{
  png_file = paste(path,'/data/',athlete,'/','Joint_Stiffness.png', sep = '')
  png(png_file, width=1600, height=1000, units="px", res=106)
}
Stiff_data <- read.table(paste(path,'/data/',athlete,'/','j_stiff.csv',sep=''), sep=';')
names.arg=c("jo", "j", "jo100", "j100", "jo300", "j300", "jo500", "j500")
x = c(1, 2, 3, 4, 5, 6 ,7, 8)
Ankle_Stiffness = c(Stiff_data$V2)
Knee_Stiffness = c(Stiff_data$V3)
Hip_Stiffness = c(Stiff_data$V4)
to_plot <- data.frame(x=names.arg,Hip_Stiffness=Hip_Stiffness, Knee_Stiffness=Knee_Stiffness, Ankle_Stiffness=Ankle_Stiffness)
melted<-melt(to_plot, id="x")
ggplot(melted,aes(x=x,y=value,fill=variable)) + geom_bar(stat="identity",position = "fill", alpha=.3) + scale_y_continuous(labels = percent_format())+ theme_bw() 
if (save_to_file) dev.off();
#------------------------------------------------------------------#
#Report

l=2
COM_source <- list()
Separation_time_source <- list()
Sep_amort_time_source <- list()
Sep_repul_time_source <- list()
summ_stiff_source <- list()
eff_source <- list()
start_f_source <- list()
power_sum_source <- list()

target_array = matrix(nrow=10, ncol=9)
target_array[1,1] = ' '
target_array[2,1] = 'jump_jo'
target_array[3,1] = 'jump_j'
target_array[4,1] = 'jump_jo100_1'
target_array[5,1] = 'jump_jo100_2'
target_array[6,1] = 'jump_jo300_1'
target_array[7,1] = 'jump_jo300_2'
target_array[8,1] = 'jump_jo500_1'
target_array[9,1] = 'jump_jo500_2'
target_array[10,1] = 'jump_js'

target_array[1,2] = 'COM'
target_array[1,3] = 'Separation_time'
target_array[1,4] = 'Amort_time'
target_array[1,5] = 'Push_off_time'
target_array[1,6] = 'Effectiv'
target_array[1,7] = 'Stiffness'
target_array[1,8] = 'Start_force'
target_array[1,9] = 'Power_sum_max'
#target_array[1,10] = 'Ekin_amort'
#target_array[1,11] = 'KPD'


for (table in table_list)
{ 
  normalized_file = table
  file = paste(path,"/data/",athlete,"/",normalized_file,".csv", sep = '')
  table_source <- read.csv(file=file, sep=";")
  summ_stiff_source <- table_source$Stiffness
  eff_source <- table_source$Eff
  COM_source <- table_source$COM
  Separation_time_source <- table_source$Separation_time
  Sep_amort_time_source <- table_source$Amort_time
  Sep_repul_time_source <- table_source$Push_off_time
  start_f_source <- table_source$start_force
  power_sum_source <- table_source$Power_sum_max
  
  summ_stiff_source [l] <- summ_stiff_source
  eff_source [l] <- eff_source
  COM_source [l] <- COM_source
  Separation_time_source [l] <- Separation_time_source
  Sep_amort_time_source [l] <- Sep_amort_time_source
  Sep_repul_time_source [l] <- Sep_repul_time_source
  start_f_source [l] <- start_f_source
  power_sum_source [l] <- power_sum_source
  
  summary_file = paste(path,'/data/',athlete,'/', 'Report.csv',sep='')
  
  target_array[l,2] = COM_source[[l]]
  target_array[l,3] = Separation_time_source[[l]]
  target_array[l,4] = Sep_amort_time_source[[l]]
  target_array[l,5] = Sep_repul_time_source[[l]]
  target_array[l,6] = eff_source[[l]]
  target_array[l,7] = summ_stiff_source[[l]]
  target_array[l,8] = start_f_source[[l]]
  target_array[l,9] = power_sum_source[[l]]
  l=l+1
}
write.table(target_array, file = summary_file, append = FALSE, col.names = FALSE, row.names=FALSE, dec=',', sep=';')
#------------------------------------------------------------------#
df<-read.table(paste(path,'/data/',athlete,'/Report.csv',sep=''), header = TRUE, sep=";", dec=',')
df <- data.frame(df)
df$COM <- gsub("\\.", ",", df$COM)
df$Separation_time <- gsub("\\.", ",", df$Separation_time)
df$Amort_time <- gsub("\\.", ",", df$Amort_time)
df$Push_off_time <- gsub("\\.", ",", df$Push_off_time)
df$Effectiv <- gsub("\\.", ",", df$Effectiv)
df$Stiffness <- gsub("\\.", ",", df$Stiffness)
df$Start_force <- gsub("\\.", ",", df$Start_force)
df$Power_sum_max <- gsub("\\.", ",", df$Power_sum_max)
write.table(df, file = summary_file, append = FALSE, col.names = TRUE, row.names=FALSE, dec=',', sep=';')
#------------------------------------------------------------------#
my.file.rename <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)}

old_way = paste(path,'/data/',athlete, sep="")

folder_files <- list.files(paste(path,'/data/',athlete,sep=''), all.files = TRUE)
types = c("_energy.csv","_moments.csv","_powers.csv","_works.csv", "Report.csv", "_sum_file.csv")
for(i in types)
{
  for (j in folder_files)
  {
    if((length(grep(i, j)) != 0))
    {
      old_way_file = paste(old_way, '/', j, sep = "")
      new_way = paste(old_way,'/', athlete, '/', j, sep = "")
      my.file.rename(from = old_way_file, to = new_way)
    }
  }	
}
#------------------------------------------------------------------#
