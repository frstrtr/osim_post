library(R.utils) # Utility functions useful when programming and developing R packages.
library(logging) # Pure R implementation of the ubiquitous log4j package. It offers hierarchic loggers, multiple handlers per log
#library(xlsx)
#library(WriteXLS)
library("XML")
library("methods")
logReset();
basicConfig('DEBUG');
addHandler(writeToConsole);
addHandler(writeToFile, file=paste("output-",System$getHostname(),'-',Sys.Date(),".log",sep=''), level='DEBUG');

path = '.'

athlete <- "jump_jo_3"
experiment <- "jump_jo_3"
save_to_file = TRUE

model_xml_file = paste(path,'/data/',athlete,'/gait_with_arms_scaled.osim',sep='')

#------------------------------------------------------------------#
logdebug('loading antropometry...');
# antropometry = scan(file = paste(path,'/data/antrop.txt',sep=''), what = character())
antropometry = scan(file = paste('antrop.txt',sep=''), what = character())

sex <- antropometry[2]
weight <- as.numeric(antropometry[4])
height <- as.numeric(antropometry[6])

#------------------------------------------------------------------#
logdebug('loading source data files (pos, vel, acc, body_forces, inverse_dynamics, mot)...');
pos_source <- read.table(paste('jump_js_1_model_BodyKinematics_pos_global.sto',sep=''), skip=18, header = TRUE)
vel_source <- read.table(paste('jump_js_1_model_BodyKinematics_vel_global.sto',sep=''), skip=18, header = TRUE)
acc_source <- read.table(paste('jump_js_1_model_BodyKinematics_acc_global.sto',sep=''), skip=18, header = TRUE)
body_forces_source <- read.table(paste('jump_js_1_body_forces_at_joints.sto',sep=''), skip=6, header = TRUE)
inverse_dynamics_source <- read.table(paste('jump_js_1_inverse_dynamics.sto',sep=''), skip=6, header = TRUE)
mot_source <- read.table(paste('jump_js_1.mot',sep=''), skip=10, header = TRUE)
ext_forces_source <- read.table(paste('jump_js_1_ext_forces.mot',sep=''), skip=10, header = TRUE)
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

if (save_to_file)
{
	png_file = paste(path,'/data/',athlete,'/', experiment, '_plot_1_center_of_mass_delta_raw.png', sep = "")
	png(png_file, width=1600, height=1000, units="px", res=106)
}
plot(center_of_mass_Y_delta, ann=FALSE, type="l", col="red", lwd=2)
lines(x=seq(1,frames-2),center_of_mass_X_delta, col='darkgreen', lwd=1)
lines(x=seq(1,frames-2),center_of_mass_Z_delta, col='blue', lwd=1)
lines(x=c(-10000,10000), y=c(0,0), col='black')
title(main=paste("Center of Mass movement (delta) over time", sep=''))
title(ylab="Delta (m)")
title(xlab=paste("Frames", sep="")) 
legend("topleft", c("pos_source$center_of_mass Y delta", "pos_source$center_of_mass X delta","pos_source$center_of_mass Z delta"), 
   col=c('red', "darkgreen","blue"), lty=c(1,1,1), lwd=c(2,1,1));
if(save_to_file) dev.off();


#if (save_to_file)
#{
#	png_file = paste(path,'/data/',athlete,'/', experiment, '_plot_1_center_of_mass_raw.png', sep = "")
#	png(png_file, width=1600, height=1000, units="px", res=106)
#}
#plot(pos_source$center_of_mass_Y, ann=FALSE, type="l", col="red", lwd=2)
#title(main=paste("Center of Mass movement over time", sep=''))
#title(ylab="Hieght (m)")
#title(xlab=paste("Frames", sep="")) 
#if(save_to_file) dev.off();



logdebug('detecting ground release point...')
combined_vertical_force = ext_forces_source$X1_Force_Y + ext_forces_source$X2_Force_Y
zero_force = combined_vertical_force[length(combined_vertical_force)]
force_tolerance = 0.1
force_max_time = min(ext_forces_source$time[combined_vertical_force>=max(combined_vertical_force)])
force_max_frame = frames-sum(mot_source$time>=force_max_time)-2
fitting_times = ext_forces_source$time[(combined_vertical_force<=zero_force*(1+ force_tolerance))]
eject_time = min(fitting_times[fitting_times >= force_max_time])
eject_frame = frames-sum(mot_source$time>=eject_time)-2
logdebug(paste("calculated platform eject time (sec): ", eject_time))
logdebug(paste("calculated platform eject frame: ", eject_frame))

#smoothing
center_of_mass_Y_delta_smoothed = smooth.spline(center_of_mass_Y_delta, spar=0.4)$y
center_of_mass_X_delta_smoothed = smooth.spline(center_of_mass_X_delta, spar=0.4)$y
center_of_mass_Z_delta_smoothed = smooth.spline(center_of_mass_Z_delta, spar=0.4)$y
center_of_mass_Y_smoothed = smooth.spline(pos_source$center_of_mass_Y, spar=0.4)$y


logdebug('detecting movement point (COM goes down)...')
movement_tolerance = 7e-5
frames_array = seq(1, frames)
movement_frame = min(frames_array[abs(center_of_mass_Y_delta_smoothed)> movement_tolerance]) - 2
movement_time = mot_source$time[movement_frame+1]
logdebug(paste("calculated movement start time (sec): ", movement_time))
logdebug(paste("calculated movement start frame: ", movement_frame))

logdebug('detecting lowest COM point...')
lowest_time = min(pos_source$time[center_of_mass_Y_smoothed <= min(center_of_mass_Y_smoothed)])
lowest_frame = min(frames_array[center_of_mass_Y_smoothed <= min(center_of_mass_Y_smoothed)])
logdebug(paste("calculated lowest COM time (sec): ", lowest_time))
logdebug(paste("calculated lowest COM frame: ", lowest_frame))

logdebug('detecting highest COM point...')
highest_frame = min(frames_array[pos_source$center_of_mass_Y==max(pos_source$center_of_mass_Y)])
highest_time = mot_source$time[highest_frame]
logdebug(paste("calculated highest COM time (sec): ", highest_time))
logdebug(paste("calculated highest COM frame: ", highest_frame))




logdebug('normalizing files (pos, vel, acc, body_forces, inverse_dynamics, mot)...')

normalize_file <- function(data, out_name)
{
	logdebug(paste('normalizing: ', out_name, sep=''))
		
	columns = attributes(data)[1]$names
	n_columns = length(columns)
	
	source_array = slot(data, '.Data')
	target_array = matrix(nrow=400, ncol=n_columns)
	
	start_frame = movement_frame
	end_frame = eject_frame
	
	new_frames = seq(start_frame, end_frame, length.out = 400)

	for (i in seq(1, n_columns))
	{
		column = columns[i]
		
		#logdebug(paste('processing column ',i,' - ', column, sep=''))
		
		array = source_array[i][[1]]
				
		array_spl = smooth.spline(array)
		target_array[,i] = predict(array_spl, new_frames)$y
	}

	write.table(target_array, file = out_name, append = FALSE, col.names = columns, row.names=FALSE, dec=',', sep=';')
	#WriteXLS(as.data.frame(target_array), ExcelFileName = out_name, col.names = columns, row.names=FALSE, AdjWidth=TRUE, BoldHeaderRow=TRUE, FreezeRow=1)
}


normalize_force_file <- function(data, out_name)
{
	logdebug(paste('normalizing force: ', out_name, sep=''))
		
	columns = attributes(data)[1]$names
	n_columns = length(columns)
	
	source_array = slot(data, '.Data')
	target_array = matrix(nrow=400, ncol=n_columns)
	
	
	frames_force = seq(1, length(data$time))
	start_frame = min(frames_force[data$time >= movement_time])
	end_frame = min(frames_force[data$time >= eject_time])
	
	new_frames = seq(start_frame, end_frame, length.out = 400)

	for (i in seq(1, n_columns))
	{
		column = columns[i]
		
		#logdebug(paste('processing column ',i,' - ', column, sep=''))
		
		array = source_array[i][[1]]
				
		array_spl = smooth.spline(array)
		target_array[,i] = predict(array_spl, new_frames)$y
	}

	write.table(target_array, file = out_name, append = FALSE, col.names = columns, row.names=FALSE, dec=',', sep=';')
	#WriteXLS(as.data.frame(target_array), ExcelFileName = out_name, col.names = columns, row.names=FALSE, AdjWidth=TRUE, BoldHeaderRow=TRUE, FreezeRow=1)
}

normalize_file(pos_source, paste(path,'/data/',athlete,'/',experiment,'_normalized_1_pos.csv',sep=''))
normalize_file(vel_source, paste(path,'/data/',athlete,'/',experiment,'_normalized_2_vel.csv',sep=''))
normalize_file(acc_source, paste(path,'/data/',athlete,'/',experiment,'_normalized_3_acc.csv',sep=''))
normalize_file(body_forces_source, paste(path,'/data/',athlete,'/',experiment,'_normalized_4_body_forces.csv',sep=''))
normalize_file(inverse_dynamics_source, paste(path,'/data/',athlete,'/',experiment,'_normalized_5_inverse_dynamics.csv',sep=''))
normalize_file(mot_source, paste(path,'/data/',athlete,'/',experiment,'_normalized_6_mot.csv',sep=''))
normalize_force_file(ext_forces_source, paste(path,'/data/',athlete,'/',experiment,'_normalized_7_ext_forces.csv',sep=''))

#TODO - ext forces normalize as well, but needs to be filtered differently as it has different timescale
logdebug('normalization done.')



logdebug('loading XML model to get segment weights')
model_xml = xmlParse(file = model_xml_file)

get_segment_mass <- function(segment)
{
	#OpenSimDocument -> Model -> BodySet -> objects -> Body name = "tibia_r"
   seg = getNodeSet(model_xml, paste("//Body[@name='",segment,"']/mass",sep=''))
	value = xmlValue(seg[[1]])
	as.numeric(value)
}


logdebug('loading normalized files')
pos_smoothed <- read.table(paste(path,'/data/',athlete,'/',experiment,'_normalized_1_pos.csv',sep=''), header = TRUE, sep=";", dec=',')
vel_smoothed <- read.table(paste(path,'/data/',athlete,'/',experiment,'_normalized_2_vel.csv',sep=''), header = TRUE, sep=";", dec=',')
acc_smoothed <- read.table(paste(path,'/data/',athlete,'/',experiment,'_normalized_3_acc.csv',sep=''), header = TRUE, sep=";", dec=',')
body_forces_smoothed <- read.table(paste(path,'/data/',athlete,'/',experiment,'_normalized_4_body_forces.csv',sep=''), header = TRUE, sep=";", dec=',')
inverse_dynamics_smoothed <- read.table(paste(path,'/data/',athlete,'/',experiment,'_normalized_5_inverse_dynamics.csv',sep=''), header = TRUE, sep=";", dec=',')
mot_smoothed <- read.table(paste(path,'/data/',athlete,'/',experiment,'_normalized_6_mot.csv',sep=''), header = TRUE, sep=";", dec=',')
ext_forces_smoothed <- read.table(paste(path,'/data/',athlete,'/',experiment,'_normalized_7_ext_forces.csv',sep=''), header = TRUE, sep=";", dec=',')

# 1 frame - beginning of ammortization phase, 400 - end of push-off
phase_split = round((lowest_frame - movement_frame) / (eject_frame - movement_frame) * 400, digits=0)


# Жесткость

amort_com_max = max(pos_smoothed$center_of_mass_Y[seq(1, phase_split)])
amort_com_min = min(pos_smoothed$center_of_mass_Y[seq(1, phase_split)])

max_sum_of_left_and_right_force_Y = max(ext_forces_smoothed$X1_Force_Y + ext_forces_smoothed$X2_Force_Y)
max_avg_of_left_and_right_force_Y = max((ext_forces_smoothed$X1_Force_Y + ext_forces_smoothed$X2_Force_Y)/2)

amort_stiffness_K_sum = max_sum_of_left_and_right_force_Y / (amort_com_max - amort_com_min) 
amort_stiffness_K_avg = max_avg_of_left_and_right_force_Y / (amort_com_max - amort_com_min)

# Энергии

Ekin1 = weight * (((vel_smoothed$center_of_mass_X*vel_smoothed$center_of_mass_X + vel_smoothed$center_of_mass_Y*vel_smoothed$center_of_mass_Y + vel_smoothed$center_of_mass_Z*vel_smoothed$center_of_mass_Z)*pi/180)^2) * 0.5

I_foot = ((get_segment_mass("toes_r") + get_segment_mass("calcn_r") + get_segment_mass("talus_r"))/660.49)+((get_segment_mass("toes_r") + get_segment_mass("calcn_r") + get_segment_mass("talus_r"))/4716100)
I_shank = ((get_segment_mass("tibia_r"))/789.61)+((get_segment_mass("tibia_r"))/278784)
I_thigh = ((get_segment_mass("femur_r"))/712.89)+((get_segment_mass("femur_r"))/291600)

Ekin_foot_right = I_foot * (((vel_smoothed$calcn_r_Oz)*pi/180)^2)*0.5
Ekin_shank_right = I_shank * (((vel_smoothed$tibia_r_Oz)*pi/180)^2)*0.5
Ekin_thigh_right = I_thigh * (((vel_smoothed$femur_r_Oz)*pi/180)^2)*0.5

Ekin2 = Ekin_foot_right + Ekin_shank_right + Ekin_thigh_right
Ekin = Ekin1 + (2 * Ekin2)

Epot = weight * 9.81 * (pos_smoothed$center_of_mass_Y + 0.398)

Emech = Ekin + Epot

# Моменты

Moment_knee_right = inverse_dynamics_smoothed$knee_angle_r_moment 
Moment_ankle_right = inverse_dynamics_smoothed$ankle_angle_r_moment 
Moment_hip_right = inverse_dynamics_smoothed$hip_flexion_r_moment 
Moment_right_sum = Moment_knee_right + Moment_ankle_right + Moment_hip_right

# Мощности

Knee_angular_velocity_right = (c(mot_smoothed$knee_angle_r) - c(mot_smoothed$knee_angle_r[1],mot_smoothed$knee_angle_r[seq(1,399)])) * pi / 180 / ((eject_time-movement_time)/400)
Power_knee_right = Moment_knee_right * Knee_angular_velocity_right

Ankle_angular_velocity_right = (c(mot_smoothed$ankle_angle_r) - c(mot_smoothed$ankle_angle_r[1],mot_smoothed$ankle_angle_r[seq(1,399)])) * pi / 180 / ((eject_time-movement_time)/400)
Power_ankle_right = Moment_ankle_right * Ankle_angular_velocity_right

Hip_angular_velocity_right = (c(mot_smoothed$hip_flexion_r) - c(mot_smoothed$hip_flexion_r[1],mot_smoothed$hip_flexion_r[seq(1,399)])) * pi / 180 / ((eject_time-movement_time)/400)
Power_hip_right = Moment_hip_right * Hip_angular_velocity_right

Power_sum = abs(Power_knee_right) + abs(Power_ankle_right) + abs(Power_hip_right)

#фаза амортизации

Emech_max = max(Emech[seq(1, phase_split)])
Emech_min = min(Emech[seq(1, phase_split)])
delta_E_amort = Emech_max - Emech_min

Moment_max = max(Moment_right_sum[seq(1, phase_split)])
Moment_min = min(Moment_right_sum[seq(1, phase_split)])
delta_moment_amort = Moment_max - Moment_min

Power_sum_max = max(Power_sum[seq(1, phase_split)])
Power_sum_min = min(Power_sum[seq(1, phase_split)])
delta_power_amort = Power_sum_max - Power_sum_min

#фаза отталкивания

Emech_max = max(Emech[seq(phase_split+1, 400)])
Emech_min = min(Emech[seq(phase_split+1, 400)])
delta_E_ott = Emech_max - Emech_min

Moment_max = max(Moment_right_sum[seq(phase_split+1, 400)])
Moment_min = min(Moment_right_sum[seq(phase_split+1, 400)])
delta_moment_ott = Moment_max - Moment_min

Power_sum_max = max(Power_sum[seq(phase_split+1, 400)])
Power_sum_min = min(Power_sum[seq(phase_split+1, 400)])
delta_power_ott = Power_sum_max - Power_sum_min

# TODO save to file

power_and_moments_file = paste(path,'/data/',athlete,'/',experiment,'_normalized_8_RCALC_power_and_moments.csv',sep='')
columns = c(
'Ekin1', 
'Ekin_foot_right', 
'Ekin_shank_right', 
'Ekin_thigh_right', 
'Ekin2', 
'Emech',
'Moment_knee_right', 
'Moment_ankle_right', 
'Moment_hip_right', 
'Moment_right_sum',
'Knee_angular_velocity_right',
'Power_knee_right',
'Ankle_angular_velocity_right',
'Power_ankle_right',
'Hip_angular_velocity_right',
'Power_hip_right',
'Power_sum'
)
target_array = matrix(nrow=400,ncol=length(columns))
target_array[,1] = Ekin1
target_array[,2] = Ekin_foot_right
target_array[,3] = Ekin_shank_right
target_array[,4] = Ekin_thigh_right
target_array[,5] = Ekin2
target_array[,6] = Emech
target_array[,7] = Moment_knee_right
target_array[,8] = Moment_ankle_right
target_array[,9] = Moment_hip_right
target_array[,10] = Moment_right_sum
target_array[,11] = Knee_angular_velocity_right
target_array[,12] = Power_knee_right
target_array[,13] = Ankle_angular_velocity_right
target_array[,14] = Power_ankle_right
target_array[,15] = Hip_angular_velocity_right
target_array[,16] = Power_hip_right
target_array[,17] = Power_sum
write.table(target_array, file = power_and_moments_file, append = FALSE, col.names = columns, row.names=FALSE, dec=',', sep=';')



summary_file = paste(path,'/data/',athlete,'/',experiment,'_normalized_9_RCALC_summary.csv',sep='')
target_array = matrix(nrow=2,ncol=8)
target_array[1,1] = 'Энергия_фаза амортизации'
target_array[1,2] = 'Момент_фаза амортизации'
target_array[1,3] = 'Мощность_фаза амортизации'
target_array[1,4] = 'Показатель жесткости'
target_array[1,5] = 'Показатель жесткости_для одной ноги'
target_array[1,6] = 'Энергия_фаза отталкивания'
target_array[1,7] = 'Момент_фаза отталкивания'
target_array[1,8] = 'Мощность_фаза отталкивания'
target_array[2,1] = delta_E_amort
target_array[2,2] = delta_moment_amort
target_array[2,3] = delta_power_amort
target_array[2,4] = amort_stiffness_K_sum
target_array[2,5] = amort_stiffness_K_avg
target_array[2,6] = delta_E_ott
target_array[2,7] = delta_moment_ott
target_array[2,8] = delta_power_ott
write.table(target_array, file = summary_file, append = FALSE, col.names = FALSE, row.names=FALSE, dec=',', sep=';')








if (save_to_file)
{
	png_file = paste(path,'/data/',athlete,'/', experiment, '_plot_2_jump_phases.png', sep = "")
	png(png_file, width=1600, height=1000, units="px", res=106)
}
plot(center_of_mass_Y_delta_smoothed, ann=FALSE, type="l", col="red", lwd=2)
lines(x=seq(1,frames-2),center_of_mass_X_delta_smoothed, col='darkgreen', lwd=1)
lines(x=seq(1,frames-2),center_of_mass_Z_delta_smoothed, col='blue', lwd=1)
lines(x=c(-10000,10000), y=c(0,0), col='black')
title(main=paste("Jump Phases Breakdown (Using smoothed Center of Mass + Force Reactions)", sep=''))
title(ylab="Delta (m)")
title(xlab=paste("Frames", sep="")) 
legend("topleft", c("center_of_mass VERTICAL delta smoothed", "center_of_mass TRANSVERSAL delta smoothed","center_of_mass LONGITUDINAL delta smoothed",
paste("Frame ", movement_frame, " (" , movement_time, " sec): start of movement (Phase 1 begins)", sep=""), paste("Frame ", lowest_frame, " (" , lowest_time, " sec): center of mass it at lowest point (Phase 1 ends, 2 begins)",sep=''), paste("Frame ", eject_frame, " (" , eject_time, " sec): ejecting from the platform (Phase 2 ends, Phase 3 begins)", sep=''), paste("Frame ", highest_frame, " (" , highest_time, " sec): center of mass is at highest point (Phase 3 ends)", sep='')), 
   col=c('red', "darkgreen","blue", "orange", "brown", "purple", "darkgreen"), lty=c(1,1,1,2,2,2,2), lwd=c(2,1,1,2,2,2,2));
   
top = max(center_of_mass_Y_delta_smoothed)  
bottom = min(center_of_mass_Y_delta_smoothed)  
lines(x=c(eject_frame, eject_frame), y=c(bottom, top), lty=2, lwd=2, col="purple") 
lines(x=c(movement_frame, movement_frame), y=c(bottom, top*0.6), lty=2, lwd=2, col="orange") 
lines(x=c(lowest_frame, lowest_frame), y=c(bottom, top), lty=2, lwd=2, col="brown") 
lines(x=c(highest_frame, highest_frame), y=c(bottom, top), lty=2, lwd=2, col="darkgreen") 
if(save_to_file) dev.off();


#------------------------------------------------------------------#
logdebug('plotting knee joint moment/velocity angle...')
knee_angle_r_moment = inverse_dynamics_source$knee_angle_r_moment
angular_velocity_knee_Oz = vel_source$femur_r_Oz - vel_source$tibia_r_Oz
knee_angle_r_moment_smoothed = smooth.spline(knee_angle_r_moment)$y
angular_velocity_knee_Oz_smoothed = smooth.spline(angular_velocity_knee_Oz)$y

if (save_to_file)
{
	png_file = paste(path,'/data/',athlete,'/', experiment, '_plot_3_knee_moment_vs_angular_velocity.png', sep = "")
	png(png_file, width=1600, height=1000, units="px", res=106)
}
plot(x=knee_angle_r_moment_smoothed, y=angular_velocity_knee_Oz_smoothed, ann=FALSE, type="l", col="black", lwd=2)
points(x= knee_angle_r_moment_smoothed[1], y = angular_velocity_knee_Oz_smoothed[1], pch=17, col="grey", cex=3)
points(x= knee_angle_r_moment_smoothed[movement_frame], y = angular_velocity_knee_Oz_smoothed[movement_frame], pch=18, col="orange", cex=3)
points(x= knee_angle_r_moment_smoothed[lowest_frame], y = angular_velocity_knee_Oz_smoothed[lowest_frame], pch=18, col="brown", cex=3)
points(x= knee_angle_r_moment_smoothed[eject_frame], y = angular_velocity_knee_Oz_smoothed[eject_frame], pch=18, col="purple", cex=3)
points(x= knee_angle_r_moment_smoothed[frames], y = angular_velocity_knee_Oz_smoothed[frames], pch=19, col="red", cex=3)
points(x= knee_angle_r_moment_smoothed[highest_frame], y = angular_velocity_knee_Oz_smoothed[highest_frame], pch=17, col="darkgreen", cex=3)


title(main=paste("Knee Angle Moment vs Angular Velocity", sep=''))
title(ylab="Angular Velocity")
title(xlab=paste("Angle Moment", sep="")) 
legend("bottomleft", c("start of experiment", 
paste("Frame ", movement_frame, " (" , movement_time, " sec): start of movement (Phase 1 begins)", sep=""), paste("Frame ", lowest_frame, " (" , lowest_time, " sec): center of mass has lowest Y (Phase 1 ends, 2 begins)",sep=''), paste("Frame ", eject_frame, " (" , eject_time, " sec): ejecting from the platform (Phase 2 ends)",sep=''), paste("Frame ", highest_frame, " (" , highest_time, " sec): center of mass is at highest point (Phase 3 ends)", sep=''), "end of experiment"), 
   col=c("grey", "orange", "brown", "purple", "darkgreen", "red"), lty=c(1,1,1,1,1,1), pch=c(17,18,18,18,17,19));
if(save_to_file) dev.off();


