
library(R.utils)
library(logging)
library("XML")
library("methods")
library(ggplot2)
library(reshape2)
library (scales) 
#library(plotly)
#library(xlsx)
#library(WriteXLS)
#------------------------------------------------------------------#
logReset();
basicConfig('DEBUG');
addHandler(writeToConsole);
addHandler(writeToFile, file=paste("output-",System$getHostname(),'-',Sys.Date(),".log",sep=''), level='DEBUG');
save_to_file = TRUE
#------------------------------------------------------------------#
path = 'C:/Users/IEUser/Desktop/Smishlyaev_15.10.2015_attempt_report_with_energy_transfer_full/'
#path = '/Users/proton/Google Диск/Science/OpensimModeling/Storage/jump_CMC_Horoshilov_jump_j_1/data/'
athlete = ''
experiment = ''
#------------------------------------------------------------------#

#------------------------------------------------------------------#
model_xml_file = paste(path,'//',athlete,'/', experiment, 'subject01_RRA_adjusted_Run_50002_cycle1_v191_with_corrected_mass_probed_0_muscles_changed_0.osim',sep='')
#------------------------------------------------------------------#
logdebug('loading antropometry...');
antropometry = scan(file = paste(path,'//',athlete,'/antrop.txt',sep=''), what = character())
sex <- antropometry[2]
weight <- as.numeric(antropometry[4])
height <- as.numeric(antropometry[6])

logdebug('loading experiment name...');
jumpname_file = scan(file = paste(path,'//',athlete,'/jumpfile.txt',sep=''), what = character())
jumpname <- jumpname_file[1]



#------------------------------------------------------------------#
logdebug('loading source data files (pos, vel, acc, body_forces, inverse_dynamics, mot)...');
pos_source <- read.table(paste(path,'//',athlete,'/', experiment, '/Run_50002_analyze__BodyKinematics_pos_global.sto',sep=''), skip=18, header = TRUE)
vel_source <- read.table(paste(path,'//',athlete,'/', experiment, '/Run_50002_analyze__BodyKinematics_vel_global.sto',sep=''), skip=18, header = TRUE)
acc_source <- read.table(paste(path,'//',athlete,'/', experiment, '/Run_50002_analyze__BodyKinematics_acc_global.sto',sep=''), skip=18, header = TRUE)
body_forces_source <- read.table(paste(path,'//',athlete,'/', experiment, '/Run_50002_body_forces_at_joints.sto',sep=''), skip=6, header = TRUE)
inverse_dynamics_source <- read.table(paste(path,'//',athlete,'/', experiment, '/Run_50002_inverse_dynamics.sto',sep=''), skip=6, header = TRUE)
mot_source <- read.table(paste(path,'//',athlete,'/', experiment, '/Run_50002.mot',sep=''), skip=10, header = TRUE)
ext_forces_source <- read.table(paste(path,'//',athlete,'/', experiment, '/Run_500 02_newCOP3.mot',sep=''), skip=10, header = TRUE)
frames <- length(mot_source$time)
logdebug(paste('loaded', frames, 'frames'))

#------------------------------------------------------------------#
# CMC sources

metabolic_probes_source <- read.table(paste(path,'//',athlete,'/', experiment, '/CMC_Results_v240_Run_50002_cycle1_probed_0_muscles_changed_0/subject01_Run_50002_cycle1_probed_0_muscles_changed_0_MetabolicsReporter_probes.sto',sep=''), skip=12, header = TRUE)

controls_source <- read.table(paste(path,'//',athlete,'/', experiment, '/CMC_Results_v240_Run_50002_cycle1_probed_0_muscles_changed_0/subject01_Run_50002_cycle1_probed_0_muscles_changed_0_controls.sto',sep=''), skip=6, header = TRUE)

kinematics_q_source <- read.table(paste(path,'//',athlete,'/', experiment, '/CMC_Results_v240_Run_50002_cycle1_probed_0_muscles_changed_0/subject01_Run_50002_cycle1_probed_0_muscles_changed_0_Kinematics_q.sto',sep=''), skip=10, header = TRUE)

kinematics_u_source <- read.table(paste(path,'//',athlete,'/', experiment, '/CMC_Results_v240_Run_50002_cycle1_probed_0_muscles_changed_0/subject01_Run_50002_cycle1_probed_0_muscles_changed_0_Kinematics_u.sto',sep=''), skip=10, header = TRUE)

actuation_speed_source  <- read.table(paste(path,'//',athlete,'/', experiment, '/CMC_Results_v240_Run_50002_cycle1_probed_0_muscles_changed_0/subject01_Run_50002_cycle1_probed_0_muscles_changed_0_Actuation_speed.sto',sep=''), skip=22, header = TRUE)

actuation_power_source  <- read.table(paste(path,'//',athlete,'/', experiment, '/CMC_Results_v240_Run_50002_cycle1_probed_0_muscles_changed_0/subject01_Run_50002_cycle1_probed_0_muscles_changed_0_Actuation_power.sto',sep=''), skip=22, header = TRUE)

actuation_force_source  <- read.table(paste(path,'//',athlete,'/', experiment, '/CMC_Results_v240_Run_50002_cycle1_probed_0_muscles_changed_0/subject01_Run_50002_cycle1_probed_0_muscles_changed_0_Actuation_force.sto',sep=''), skip=22, header = TRUE)



ActiveFiberForce <- read.table(paste(path,'//',athlete,'/', experiment, '/CMC_Results_v240_Run_50002_cycle1_probed_0_muscles_changed_0/subject01_Run_50002_cycle1_probed_0_muscles_changed_0_MuscleAnalysis_ActiveFiberForce.sto',sep=''), skip=11, header = TRUE)
FiberForce<- read.table(paste(path,'//',athlete,'/', experiment, '/CMC_Results_v240_Run_50002_cycle1_probed_0_muscles_changed_0/subject01_Run_50002_cycle1_probed_0_muscles_changed_0_MuscleAnalysis_FiberForce.sto',sep=''), skip=11, header = TRUE)
FiberLength<- read.table(paste(path,'//',athlete,'/', experiment, '/CMC_Results_v240_Run_50002_cycle1_probed_0_muscles_changed_0/subject01_Run_50002_cycle1_probed_0_muscles_changed_0_MuscleAnalysis_FiberLength.sto',sep=''), skip=11, header = TRUE)
FiberVelocity<- read.table(paste(path,'//',athlete,'/', experiment, '/CMC_Results_v240_Run_50002_cycle1_probed_0_muscles_changed_0/subject01_Run_50002_cycle1_probed_0_muscles_changed_0_MuscleAnalysis_FiberVelocity.sto',sep=''), skip=11, header = TRUE)
PassiveFiberForce<- read.table(paste(path,'//',athlete,'/', experiment, '/CMC_Results_v240_Run_50002_cycle1_probed_0_muscles_changed_0/subject01_Run_50002_cycle1_probed_0_muscles_changed_0_MuscleAnalysis_PassiveFiberForce.sto',sep=''), skip=11, header = TRUE)
TendonForce<- read.table(paste(path,'//',athlete,'/', experiment, '/CMC_Results_v240_Run_50002_cycle1_probed_0_muscles_changed_0/subject01_Run_50002_cycle1_probed_0_muscles_changed_0_MuscleAnalysis_TendonForce.sto',sep=''), skip=11, header = TRUE)
NormalizedFiberLength<- read.table(paste(path,'//',athlete,'/', experiment, '/CMC_Results_v240_Run_50002_cycle1_probed_0_muscles_changed_0/subject01_Run_50002_cycle1_probed_0_muscles_changed_0_MuscleAnalysis_NormalizedFiberLength.sto',sep=''), skip=11, header = TRUE)
NormFiberVelocity<- read.table(paste(path,'//',athlete,'/', experiment, '/CMC_Results_v240_Run_50002_cycle1_probed_0_muscles_changed_0/subject01_Run_50002_cycle1_probed_0_muscles_changed_0_MuscleAnalysis_NormFiberVelocity.sto',sep=''), skip=11, header = TRUE)

controls <- read.table(paste(path,'//',athlete,'/', experiment, '/CMC_Results_v240_Run_50002_cycle1_probed_0_muscles_changed_0/subject01_Run_50002_cycle1_probed_0_muscles_changed_0_controls.sto',sep=''), skip=6, header = TRUE)


#------------------------------------------------------------------#



 #------------------------------------------------------------------#
  logdebug('determining the phases of the jump...')
  center_of_mass_Y_delta = c(pos_source$center_of_mass_Y, pos_source$center_of_mass_Y[frames]) - c(pos_source$center_of_mass_Y[1], pos_source$center_of_mass_Y) 
  center_of_mass_Y_delta = center_of_mass_Y_delta[2:(frames-1)]
  center_of_mass_X_delta = c(pos_source$center_of_mass_X, pos_source$center_of_mass_X[frames]) - c(pos_source$center_of_mass_X[1], pos_source$center_of_mass_X) 
  center_of_mass_X_delta = center_of_mass_X_delta[2:(frames-1)]
  center_of_mass_Z_delta = c(pos_source$center_of_mass_Z, pos_source$center_of_mass_Z[frames]) - c(pos_source$center_of_mass_Z[1], pos_source$center_of_mass_Z) 
  center_of_mass_Z_delta = center_of_mass_Z_delta[2:(frames-1)]

  #------------------------------------------------------------------#
  #SMOTHING
  center_of_mass_Y_delta_smoothed = smooth.spline(center_of_mass_Y_delta, spar=0.4)$y
  center_of_mass_X_delta_smoothed = smooth.spline(center_of_mass_X_delta, spar=0.4)$y
  center_of_mass_Z_delta_smoothed = smooth.spline(center_of_mass_Z_delta, spar=0.4)$y
  center_of_mass_Y_smoothed = smooth.spline(pos_source$center_of_mass_Y, spar=0.4)$y
  #------------------------------------------------------------------#

  logdebug('detecting movement point (COM goes down)...')
  movement_tolerance = 1e-3
  movement_force = 50
  firstframe = 1
  frames_array = seq(firstframe, frames)
  if((length(grep("jump_jo_", jumpname)) == 0)&&(length(grep("jump_j_", jumpname)) == 0))
  firstframe = min(frames_array[abs(ext_forces_source$ground_force_vy)> movement_force])
  frames_array = seq(firstframe, frames)
  counter = 1
  for(delta_move in center_of_mass_Y_delta_smoothed)
  {
    if(abs(delta_move)>movement_tolerance)
      break  
    counter = counter + 1
  }
  movement_frame = min(frames_array[counter])
  if((length(grep("jump_jo_", jumpname)) == 0)&&(length(grep("jump_j_", jumpname)) == 0))
    movement_frame = firstframe
  movement_time = ext_forces_source$time[movement_frame]
  a = 1
  for(i in pos_source$time)
  {
    if(i>=movement_time)
      break  
    a = a + 1
  }
  movement_frame = a
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
  
  max_ext = max(ext_forces_source$ground_force_vy)
  c=0
  for (i in ext_forces_source$ground_force_vy)
  {
    if(i>= max_ext)
      break
    c= c+1
  }
  f = length(ext_forces_source$ground_force_vy)
  z = ext_forces_source$ground_force_vy[c:f]
  eject_frame =0
  for (i in z)
  {
    if(i<=0)
      break
    eject_frame = eject_frame+1
  }
  eject_frame = eject_frame + c
  eject_time = ext_forces_source$time[eject_frame]


start_frame = movement_frame
end_frame = highest_frame
mid_frame = lowest_frame
mid_time = lowest_time
start_time = movement_time
end_time = highest_time

global_time = ext_forces_source$time[start_frame:end_frame]
global_frame_count = length(global_time)
global_columns_limit = 102

# for everyting that is in .mot resolution
new_frames_for_mot = seq(start_frame, end_frame, length.out = global_frame_count)


target_file_names = matrix(nrow= 1, ncol=global_columns_limit)
target_column_names = matrix(nrow= 1, ncol=global_columns_limit)
target_column_data = matrix(nrow= global_frame_count, ncol=global_columns_limit)


target_file_names[, 1] = '-'
target_column_names[, 1] = 'time'
target_column_data[, 1] = global_time

target_file_names[, 2] = '-'
target_column_names[, 2] = 'phase_sep_time'
target_column_data[, 2] = mid_time
 
current_col_to_fill = 3





######################################################
logdebug(paste('normalizing force  - ground_force_vy', sep=''))

data_before = ext_forces_source$ground_force_vy
array = data_before
array_spl = smooth.spline(array)

target_file_names[, current_col_to_fill] = 'ext_forces'
target_column_names[, current_col_to_fill] = 'ground_force_vy'
target_column_data[, current_col_to_fill] = predict(array_spl, new_frames_for_mot)$y

current_col_to_fill = current_col_to_fill + 1;





######################################################
logdebug(paste('normalizing COM - center_of_mass_Y_smoothed', sep=''))

data_before = center_of_mass_Y_smoothed
array = data_before
array_spl = smooth.spline(array)

target_file_names[, current_col_to_fill] = 'pos'
target_column_names[, current_col_to_fill] = 'center_of_mass_vertical_smoothed'
target_column_data[, current_col_to_fill] = predict(array_spl, new_frames_for_mot)$y

current_col_to_fill = current_col_to_fill + 1;








######################################################
#logdebug(paste('normalizing .mot source ', sep=''))

#filtered_columns = c(
#'hip_flexion_r',
#'knee_angle_r',
#'ankle_angle_r'
#);

#data = mot_source
#columns = attributes(data)[1]$names
#n_columns = length(columns)
#source_array = slot(data, '.Data')

#for (i in seq(1, n_columns))
#{
#	column = columns[i]
#	
#	if(column %in% filtered_columns)
#	{
#	array = source_array[i][[1]]
#	array_spl = smooth.spline(array)
#	
#	target_file_names[, current_col_to_fill] = 'mot'
#	target_column_names[, current_col_to_fill] = column
#	target_column_data[, current_col_to_fill] = predict(array_spl, new_frames_for_mot)$y
#
#	current_col_to_fill = current_col_to_fill + 1;
#	
#	}
#}




######################################################
logdebug(paste('normalizing inverse_dynamics source ', sep=''))

filtered_columns = c(
'hip_flexion_r_moment',
'knee_angle_r_moment',
'ankle_angle_r_moment'
);

data = inverse_dynamics_source
columns = attributes(data)[1]$names
n_columns = length(columns)
source_array = slot(data, '.Data')

for (i in seq(1, n_columns))
{
	column = columns[i]
	
	if(column %in% filtered_columns)
	{
	array = source_array[i][[1]]
	array_spl = smooth.spline(array)
	
	target_file_names[, current_col_to_fill] = 'inverse_dynamics'
	target_column_names[, current_col_to_fill] = column
	target_column_data[, current_col_to_fill] = predict(array_spl, new_frames_for_mot)$y

	current_col_to_fill = current_col_to_fill + 1;
	
	}
}







######################################################
logdebug(paste('normalizing metabolic reporter ', sep=''))


filtered_columns = c(
'metabolics_TOTAL',
'metabolics_semimem_r',
'metabolics_bifemsh_r',
'metabolics_glut_max1_r',
'metabolics_glut_max2_r',
'metabolics_glut_max3_r',
'metabolics_psoas_r',
#'metabolics_quad_fem_r',
'metabolics_rect_fem_r',
'metabolics_vas_lat_r',
'metabolics_lat_gas_r',
'metabolics_soleus_r',
'metabolics_tib_ant_r'
);

data = metabolic_probes_source
columns = attributes(data)[1]$names
n_columns = length(columns)
source_array = slot(data, '.Data')


start_local_frame = sum(metabolic_probes_source$time < start_time)
end_local_frame = sum(metabolic_probes_source$time <= end_time)

# for everyting that is in .mot resolution
new_frames_for_metabolics = seq(start_local_frame, end_local_frame, length.out = global_frame_count)

for (i in seq(1, n_columns))
{
	column = columns[i]
	
	if(column %in% filtered_columns)
	{
	array = source_array[i][[1]]
	array_spl = smooth.spline(array)
	
	target_file_names[, current_col_to_fill] = 'metabolic_probes'
	target_column_names[, current_col_to_fill] = column
	target_column_data[, current_col_to_fill] = predict(array_spl, new_frames_for_metabolics)$y

	current_col_to_fill = current_col_to_fill + 1;
	
	}
}







######################################################
logdebug(paste('normalizing kinematics_q ', sep=''))

filtered_columns = c(
'hip_flexion_r', 
'knee_angle_r', 
'ankle_angle_r'
);

data = kinematics_q_source
columns = attributes(data)[1]$names
n_columns = length(columns)
source_array = slot(data, '.Data')


start_local_frame = sum(kinematics_q_source$time < start_time)
end_local_frame = sum(kinematics_q_source$time <= end_time)

# for everyting that is in .mot resolution
new_frames_for_kinematics_q = seq(start_local_frame, end_local_frame, length.out = global_frame_count)

for (i in seq(1, n_columns))
{
	column = columns[i]
	
	if(column %in% filtered_columns)
	{
	array = source_array[i][[1]]
	array_spl = smooth.spline(array)
	
	target_file_names[, current_col_to_fill] = 'kinematics_q (positions, angles)'
	target_column_names[, current_col_to_fill] = column
	target_column_data[, current_col_to_fill] = predict(array_spl, new_frames_for_kinematics_q)$y

	current_col_to_fill = current_col_to_fill + 1;
	
	}
}



######################################################
logdebug(paste('normalizing kinematics_u ', sep=''))

filtered_columns = c(
'hip_flexion_r', 
'knee_angle_r', 
'ankle_angle_r'
);

data = kinematics_u_source
columns = attributes(data)[1]$names
n_columns = length(columns)
source_array = slot(data, '.Data')


start_local_frame = sum(kinematics_u_source$time < start_time)
end_local_frame = sum(kinematics_u_source$time <= end_time)

# for everyting that is in .mot resolution
new_frames_for_kinematics_u = seq(start_local_frame, end_local_frame, length.out = global_frame_count)

for (i in seq(1, n_columns))
{
	column = columns[i]
	
	if(column %in% filtered_columns)
	{
	array = source_array[i][[1]]
	array_spl = smooth.spline(array)
	
	target_file_names[, current_col_to_fill] = 'kinematics_u (speeds)'
	target_column_names[, current_col_to_fill] = column
	target_column_data[, current_col_to_fill] = predict(array_spl, new_frames_for_kinematics_u)$y

	current_col_to_fill = current_col_to_fill + 1;
	
	}
}




######################################################
logdebug(paste('normalizing actuation files and other CMC stuff ', sep=''))


filtered_columns = c(
'semimem_r',
'bifemsh_r',
'glut_max1_r',
'glut_max2_r',
'glut_max3_r',
'psoas_r',
#'quad_fem_r',
'rect_fem_r',
'vas_lat_r',
'lat_gas_r',
'soleus_r',
'tib_ant_r'
);

source_names = c(
#'ActiveFiberForce', 
'FiberForce',
#'FiberLength',
'FiberVelocity',
#'PassiveFiberForce',
#'TendonForce',
'NormalizedFiberLength',
'NormFiberVelocity',
'controls',
'actuation_power_source'
)


n_sources = length(source_names)

for (src in seq(1, n_sources))
{

data = eval(parse(text= source_names[src]))

columns = attributes(data)[1]$names
n_columns = length(columns)
source_array = slot(data, '.Data')

start_local_frame = sum(data$time < start_time)
end_local_frame = sum(data$time <= end_time)

# for everyting that is in .mot resolution
new_frames_for_local = seq(start_local_frame, end_local_frame, length.out = global_frame_count)

for (i in seq(1, n_columns))
{
	column = columns[i]
	
	if(column %in% filtered_columns)
	{
	array = source_array[i][[1]]
	array_spl = smooth.spline(array, spar = 0.5)
	
	target_file_names[, current_col_to_fill] = source_names[src]
	target_column_names[, current_col_to_fill] = column
	target_column_data[, current_col_to_fill] = predict(array_spl, new_frames_for_local)$y

	current_col_to_fill = current_col_to_fill + 1;
	
	}
}
}

# extracting max isometric force
model_xml = xmlParse(file = model_xml_file)

get_max_isometric_force <- function(muscle)
{
	#OpenSimDocument -> Model -> BodySet -> objects -> Body name = "tibia_r"
   seg = getNodeSet(model_xml, paste("//Thelen2003Muscle[@name='", muscle,"']/max_isometric_force",sep=''))
	value = xmlValue(seg[[1]])
	as.numeric(value)
}

for (i in seq(1, length(filtered_columns)))
{
	muscle = filtered_columns[i]
		
	target_file_names[, current_col_to_fill] = 'model'
	target_column_names[, current_col_to_fill] = 'max isometric force'
	target_column_data[, current_col_to_fill] = get_max_isometric_force(muscle)

	current_col_to_fill = current_col_to_fill + 1;
}




normalized_file = paste(path,'//',athlete,'/',experiment,'/normalized_CMC.csv',sep='')
write.table(target_file_names, file = normalized_file, append = FALSE, col.names = FALSE, row.names=FALSE, dec=',', sep=';')
write.table(target_column_names, file = normalized_file, append = TRUE, col.names = FALSE, row.names=FALSE, dec=',', sep=';')
write.table(target_column_data, file = normalized_file, append = TRUE, col.names = FALSE, row.names=FALSE, dec=',', sep=';')
