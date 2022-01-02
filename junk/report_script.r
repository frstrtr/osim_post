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
path = 'C:/Users/IEUser/Desktop/Smishlyaev_15.10.2015_attempt_report_with_energy_transfer_full' 
#------------------------------------------------------------------#
files <- list.files(paste(path, sep=''), all.files = TRUE)
pos_source <- read.table(paste(path,'/jump_jo','/', 'Run_50002_analyze__BodyKinematics_pos_global.sto',sep=''), skip=18, header = TRUE)
COM_1 = mean(pos_source$center_of_mass_Y[seq(1:20)] )

#types = c("jo_", "jo100_", "jo300_", "jo500_", "j_", "j100_", "j300_", "j500_", "js_")
types = c("jump_jo", "jump_jo100", "jump_jo300", "jump_jo500", "jump_js")

for (folder in types)
{
  model_xml_file = paste(path,'/subject01_RRA_adjusted_Run_50002_cycle1_v191_with_corrected_mass_probed_0_muscles_changed_0.osim',sep='')
  #------------------------------------------------------------------#
  logdebug('loading antropometry...'); 
  antropometry = scan(file = paste(path,'/antrop.txt',sep=''), what = character())
  sex <- antropometry[2]
  weight <- as.numeric(antropometry[4])
  height <- as.numeric(antropometry[6])
  #------------------------------------------------------------------#
  logdebug('loading source data files (pos, vel, acc, body_forces, inverse_dynamics, mot)...');
  pos_source <- read.table(paste(path,'/', folder,'/Run_50002_analyze__BodyKinematics_pos_global.sto',sep=''), skip=18, header = TRUE)
  vel_source <- read.table(paste(path,'/', folder,'/Run_50002_analyze__BodyKinematics_vel_global.sto',sep=''), skip=18, header = TRUE)
  acc_source <- read.table(paste(path,'/', folder,'/Run_50002_analyze__BodyKinematics_acc_global.sto',sep=''), skip=18, header = TRUE)
  body_forces_source <- read.table(paste(path,'/', folder, '/Run_50002_body_forces_at_joints.sto',sep=''), skip=6, header = TRUE)
  inverse_dynamics_source <- read.table(paste(path,'/', folder, '/Run_50002_inverse_dynamics.sto',sep=''), skip=6, header = TRUE)
  mot_source <- read.table(paste(path,'/', folder, '/', folder, '.mot',sep=''), skip=10, header = TRUE)
  ext_forces_source <- read.table(paste(path,'/', folder, '/', folder, '_ext_forces.mot',sep=''), skip=10, header = TRUE)
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
  firstframe = 1
  frames_array = seq(firstframe, ext_frames)
  if((length(grep("jump_jo", folder)) == 0)&&(length(grep("jump_j", folder)) == 0))
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
  if((length(grep("jump_jo", folder)) == 0)&&(length(grep("jump_j", folder)) == 0)&&(length(grep("jump_js", folder)) == 0))
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
  phase_split = round((lowest_frame - movement_frame) / (eject_frame - movement_frame) * 400, digits=0)
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
  
  angles_file = paste(path, '/', folder, '/', folder, '_angles.csv',sep='')
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
  
  angular_velocity_file = paste(path,'/',folder, '/', folder, '_angular_velocity.csv',sep='')
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
  energy_file = paste(path,'/',folder,'/', folder, '_energy.csv',sep='')
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
  
  moments_file = paste(path,'/',folder,'/', folder, '_moments.csv',sep='')
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
  Power_sum_max = max (Power_sum)  
  powers_file = paste(path,'/',folder,'/', folder, '_powers.csv',sep='')
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
  
  sum_file = paste(path,'/',folder, '/', folder, '_sum_file.csv',sep='')
  columns = c(
    'Energy_summ',
    'Work_summ'
  )
  target_array = matrix(nrow=(1),ncol=length(columns))
  target_array[,1] = Emech_SUM
  target_array[,2] = W_SUM
  
  write.table(target_array, file = sum_file, append = FALSE, col.names = columns, row.names=FALSE, dec=',', sep=';')
  
  work_file = paste(path,'/',folder,'/', folder, '_works.csv',sep='')
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
  
  #------------------------------------------------------------------#
  #EFF
  Ekin_sum = mean(Ekin[seq(eject_frame+1, frames-1)])
  Epot_sum = mean(Epot[seq(1, eject_frame)])
  Eff = (Ekin_sum)/(Epot_sum) *100
  #------------------------------------------------------------------#
  #TABLE
  summary_file = paste(path,'/', folder, '_table.csv',sep='')
  target_array = matrix(nrow=2,ncol=8)
  target_array[1,1] = 'COM'
  target_array[1,2] = 'Separation_time'
  target_array[1,3] = 'Amort_time'
  target_array[1,4] = 'Push_off_time'
  target_array[1,5] = 'Effectiv'
  target_array[1,6] = 'start_force'
  target_array[1,7] = 'Ekin_amort'
  target_array[1,8] = 'Power_summ_max'
  
  
  target_array[2,1] = COM
  target_array[2,2] = Separation_time
  target_array[2,3] = Sep_amort_time
  target_array[2,4] = Sep_repul_time
  target_array[2,5] = Eff
  target_array[2,6] = start_force
  target_array[2,7] = Ekin_amort
  target_array[2,8] = Power_sum_max
  
  write.table(target_array, file = summary_file, append = FALSE, col.names = FALSE, row.names=FALSE, dec=',', sep=';')
  
}

tables <- list.files(paste(path, sep=''), all.files = TRUE, pattern = "_table.csv")
types = c("jo_", "jo100_", "jo300_", "jo500_", "j_", "j100_", "j300_", "j500_", "js_")
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
#Report

l=2
COM_source <- list()
Separation_time_source <- list()
Sep_amort_time_source <- list()
Sep_repul_time_source <- list()
eff_source <- list()
start_f_source <- list()
power_sum_source <- list()

target_array = matrix(nrow=10, ncol=8)
target_array[1,1] = ' '
target_array[2,1] = 'jump_jo'
target_array[3,1] = 'jump_j'
target_array[4,1] = 'jump_jo100'
target_array[5,1] = 'jump_j100'
target_array[6,1] = 'jump_jo300'
target_array[7,1] = 'jump_j300'
target_array[8,1] = 'jump_jo500'
target_array[9,1] = 'jump_j500'
target_array[10,1] = 'jump_js'

target_array[1,2] = 'COM'
target_array[1,3] = 'Separation_time'
target_array[1,4] = 'Amort_time'
target_array[1,5] = 'Push_off_time'
target_array[1,6] = 'Effectiv'
target_array[1,7] = 'Start_force'
target_array[1,8] = 'Power_sum_max'


for (table in table_list)
{ 
  normalized_file = table
  file = paste(path,"/",normalized_file,".csv", sep = '')
  table_source <- read.csv(file=file, sep=";")
  eff_source <- table_source$Effectiv
  COM_source <- table_source$COM
  Separation_time_source <- table_source$Separation_time
  Sep_amort_time_source <- table_source$Amort_time
  Sep_repul_time_source <- table_source$Push_off_time
  start_f_source <- table_source$start_force
  power_sum_source <- table_source$Power_summ_max
  
  eff_source [l] <- eff_source
  COM_source [l] <- COM_source
  Separation_time_source [l] <- Separation_time_source
  Sep_amort_time_source [l] <- Sep_amort_time_source
  Sep_repul_time_source [l] <- Sep_repul_time_source
  start_f_source [l] <- start_f_source
  power_sum_source [l] <- power_sum_source
  
  summary_file = paste(path,'/Report.csv',sep='')
  
  target_array[l,2] = COM_source[[l]]
  target_array[l,3] = Separation_time_source[[l]]
  target_array[l,4] = Sep_amort_time_source[[l]]
  target_array[l,5] = Sep_repul_time_source[[l]]
  target_array[l,6] = eff_source[[l]]
  target_array[l,7] = start_f_source[[l]]
  target_array[l,8] = power_sum_source[[l]]
  l=l+1
}
write.table(target_array, file = summary_file, append = FALSE, col.names = FALSE, row.names=FALSE, dec=',', sep=';')
#------------------------------------------------------------------#
df<-read.table(paste(path,'/', 'Report.csv',sep=''), header = TRUE, sep=";", dec=',')
df <- data.frame(df)
df$COM <- gsub("\\.", ",", df$COM)
df$Separation_time <- gsub("\\.", ",", df$Separation_time)
df$Amort_time <- gsub("\\.", ",", df$Amort_time)
df$Push_off_time <- gsub("\\.", ",", df$Push_off_time)
df$Effectiv <- gsub("\\.", ",", df$Effectiv)
df$Start_force <- gsub("\\.", ",", df$Start_force)
df$Power_sum_max <- gsub("\\.", ",", df$Power_sum_max)
write.table(df, file = summary_file, append = FALSE, col.names = TRUE, row.names=FALSE, dec=',', sep=';')
#------------------------------------------------------------------#
my.file.rename <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)}

old_way = paste(path, sep="")

