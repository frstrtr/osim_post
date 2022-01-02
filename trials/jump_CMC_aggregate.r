Sys.setlocale(category="LC_CTYPE", locale="ru_RU.UTF-8")

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

#------------------------------------------------------------------#
path = 'C:/Users/IEUser/Desktop/Smishlyaev_15.10.2015_attempt_report_with_energy_transfer_full' 
athlete = ''
experiment = ''
#------------------------------------------------------------------#

files <- Sys.glob(file.path(path, "*normalized_CMC.csv"))

jumps = 
c('jump_jo100',
'jump_jo300',
'jump_jo',
'jump_js');


jump_j100 <- FALSE
jump_j300 <- FALSE
jump_j500  <- FALSE
jump_j  <- FALSE
jump_jo100 <- FALSE
jump_jo300 <- FALSE
jump_jo500 <- FALSE
jump_jo  <- FALSE
jump_js    <- FALSE

for (i in 1:length(files))
{
	filename = files[i];
	
		  if (grepl("jump_j100_", filename)) { jump_j100 = read.table(filename, header = FALSE, sep=";", dec=','); }
	else  if (grepl("jump_j300_", filename)) { jump_j300 = read.table(filename, header = FALSE, sep=";", dec=','); }
	else  if (grepl("jump_j500_", filename)) { jump_j500 = read.table(filename, header = FALSE, sep=";", dec=','); }
	else  if (grepl("jump_j_", filename)) { jump_j = read.table(filename, header = FALSE, sep=";", dec=','); }
	else  if (grepl("jump_jo100_", filename)) { jump_jo100 = read.table(filename, header = FALSE, sep=";", dec=','); }
	else  if (grepl("jump_jo300_", filename)) { jump_jo300 = read.table(filename, header = FALSE, sep=";", dec=','); }
	else  if (grepl("jump_jo500_", filename)) { jump_jo500 = read.table(filename, header = FALSE, sep=";", dec=','); }
	else  if (grepl("jump_jo_", filename)) { jump_jo = read.table(filename, header = FALSE, sep=";", dec=','); }
	else  if (grepl("jump_js_", filename)) { jump_js = read.table(filename, header = FALSE, sep=";", dec=','); } 
}

muscles = c(
'semimem_r',
'bifemsh_r',
'glut_max1_r',
'glut_max2_r',
'glut_max3_r',
'psoas_r' ,
'rect_fem_r', 
'vas_lat_r' ,
'lat_gas_r', 
'soleus_r',
'tib_ant_r');

global_frame_count = length(muscles) * 7
global_columns_limit = 3+9

target_column_names = matrix(nrow= 1, ncol=global_columns_limit)
target_column_data = matrix(nrow= global_frame_count, ncol=global_columns_limit)

target_column_names[, 1] = 'muslce';
target_column_names[, 2] = 'parameter';
target_column_names[, 3] = 'calculation rule';

for (jump_idx in 1:length(jumps)) target_column_names[, 3+jump_idx] = jumps[jump_idx];

current_row_to_fill = 1;

for (m_idx in 1:length(muscles))
{
	muscle = muscles[m_idx];
	
	for (jump_idx in 1:length(jumps)) 
	{
		logdebug(paste('processing muscle: ', muscle, ' for jump: ', jumps[jump_idx], sep=''));
		
		jump = jumps[jump_idx];
		target_column_names[, 3 + jump_idx] = jump;
		
		jump = eval(parse(text= jump));
		
		row_counter = current_row_to_fill - 1;
		
		# ---------------------------------------------------------------------------
		row_counter = row_counter + 1;
		target_column_data[row_counter, 1] = muscle;
		target_column_data[row_counter, 2] = 'Pik norm sily';
		target_column_data[row_counter, 3] = 'Max(FiberForce)/MaxIsometricForce';
				
		FiberForce = jump[ 3:length(jump[,1]), ((jump[2,] == muscle) & (jump[1,] == 'FiberForce'))];
		FiberForce = as.numeric(gsub(",", "\\.", FiberForce));
			
		filtered_columns = c(
		'semimem_r',
		'bifemsh_r',
		'glut_max1_r',
		'glut_max2_r',
		'glut_max3_r',
		'psoas_r',
		'rect_fem_r',
		'vas_lat_r',
		'lat_gas_r',
		'soleus_r',
		'tib_ant_r'
		);
			
		muscle_no_in_model = c(1:length(filtered_columns));
		muscle_no_in_model = min(muscle_no_in_model[filtered_columns==muscle]);
			
		model_first_colum = c(1:length(jump[1,]));
		model_first_colum = min(model_first_colum[jump[2,]=='max isometric force']);
			
		MaxIsometricForce = jump[ 3, model_first_colum + muscle_no_in_model - 1];		
		MaxIsometricForce = as.numeric(gsub(",", "\\.", MaxIsometricForce));
			
		target_column_data[row_counter, 3+jump_idx] = max(FiberForce) / MaxIsometricForce;




	
		# ---------------------------------------------------------------------------
		row_counter = row_counter + 1;
		target_column_data[row_counter, 1] = muscle;
		target_column_data[row_counter, 2] = 'Pik norm skorosti';
		target_column_data[row_counter, 3] = 'Max(NormFiberVelocity)';
				
		NormFiberVelocity = jump[ 3:length(jump[,1]), ((jump[2,] == muscle) & (jump[1,] == 'NormFiberVelocity'))];
		NormFiberVelocity = as.numeric(gsub(",", "\\.", NormFiberVelocity));
		
		target_column_data[row_counter, 3+jump_idx] = max(NormFiberVelocity);
		
		
		
		
		
		
		
		# ---------------------------------------------------------------------------
		row_counter = row_counter + 1;
		target_column_data[row_counter, 1] = muscle;
		target_column_data[row_counter, 2] = 'Pik moshnosti';
		target_column_data[row_counter, 3] = 'Max(FiberForce/MaxIsometricForce*FiberVelocity)';
				
		FiberVelocity = jump[ 3:length(jump[,1]), ((jump[2,] == muscle) & (jump[1,] == 'FiberVelocity'))];
		FiberVelocity = as.numeric(gsub(",", "\\.", FiberVelocity));
	
		target_column_data[row_counter, 3+jump_idx] = max(FiberVelocity/MaxIsometricForce*FiberForce);







		# ---------------------------------------------------------------------------
		row_counter = row_counter + 1;
		target_column_data[row_counter, 1] = muscle;
		target_column_data[row_counter, 2] = 'Maks. rastyazhenie';
		target_column_data[row_counter, 3] = 'Max(NormalizedFiberLength)';
	
		NormalizedFiberLength = jump[ 3:length(jump[,1]), ((jump[2,] == muscle) & (jump[1,] == 'NormalizedFiberLength'))];
		NormalizedFiberLength = as.numeric(gsub(",", "\\.", NormalizedFiberLength));
		
		target_column_data[row_counter, 3+jump_idx] = max(NormalizedFiberLength);
		
		






		# ---------------------------------------------------------------------------
		row_counter = row_counter + 1;
		target_column_data[row_counter, 1] = muscle;
		target_column_data[row_counter, 2] = 'Min. sokrashenie';
		target_column_data[row_counter, 3] = 'Min(NormalizedFiberLength)  ';
				
	
		target_column_data[row_counter, 3+jump_idx] = min(NormalizedFiberLength);








		# ---------------------------------------------------------------------------
		#row_counter = row_counter + 1;
		#target_column_data[row_counter, 1] = muscle;
		#target_column_data[row_counter, 2] = 'Skorost pri maks. krutyashego momenta v 1 faze';
		#target_column_data[row_counter, 3] = 'FiberVelocity v moment, kogda NormalizedFiberLength*FiberForce max v 1 faze';
		
		#time = jump[ 3:length(jump[,1]), 1];
		#time = as.numeric(gsub(",", "\\.", time));
		
		#phase_split_time = jump[3, 2];
		#phase_split_time = as.numeric(gsub(",", "\\.", phase_split_time));
				
		#Torque = NormalizedFiberLength*FiberForce;
		#TorqueInPhase1 = Torque[time<=phase_split_time];
		#timeInPhase1 = time[time<=phase_split_time];
		#MaxTorqueInPhase1 = max(TorqueInPhase1);
		#MaxTorqueTimeInPhase1 = min(timeInPhase1[TorqueInPhase1 == MaxTorqueInPhase1]);

		#target_column_data[row_counter, 3+jump_idx] = FiberVelocity[time == MaxTorqueTimeInPhase1];








		# ---------------------------------------------------------------------------
		#row_counter = row_counter + 1;
		#target_column_data[row_counter, 1] = muscle;
		#target_column_data[row_counter, 2] = 'Skorost pri maks. krutyashego momenta vo 2 faze';
		#target_column_data[row_counter, 3] = 'FiberVelocity v moment, kogda NormalizedFiberLength*FiberForce max vo 2 faze';
				
		#TorqueInPhase2 = Torque[time>phase_split_time];
		#timeInPhase2 = time[time>phase_split_time];
		#MaxTorqueInPhase2 = max(TorqueInPhase2);
		#MaxTorqueTimeInPhase2 = min(timeInPhase2[TorqueInPhase2 == MaxTorqueInPhase2]);

		#target_column_data[row_counter, 3+jump_idx] = FiberVelocity[time == MaxTorqueTimeInPhase2];


		
		
		
		# ---------------------------------------------------------------------------
		#row_counter = row_counter + 1;
		#target_column_data[row_counter, 1] = muscle;
		#target_column_data[row_counter, 2] = 'Maks krutyashiy moment';
		#target_column_data[row_counter, 3] = 'NormalizedFiberLength*FiberForce max';
		
		#Torque = NormalizedFiberLength*FiberForce;
		#MaxTorque = max(Torque);
		#MaxTorqueTime = min(time[Torque == MaxTorque]);

		#target_column_data[row_counter, 3+jump_idx] = MaxTorque;








		# ---------------------------------------------------------------------------
		#row_counter = row_counter + 1;
		#target_column_data[row_counter, 1] = muscle;
		#target_column_data[row_counter, 2] = 'Sila pri max. krutyashego momenta';
		#target_column_data[row_counter, 3] = 'FiberForce/MaxIsometricForce v moment, kogda NormalizedFiberLength*FiberForce max';
		

		#target_column_data[row_counter, 3+jump_idx] = FiberForce[time == MaxTorqueTime] / MaxIsometricForce;








		# ---------------------------------------------------------------------------
		#row_counter = row_counter + 1;
		#target_column_data[row_counter, 1] = muscle;
		#target_column_data[row_counter, 2] = 'Moshnost pri max. krutyashego momenta';
		#target_column_data[row_counter, 3] = 'FiberForce*FiberVelocity v moment, kogda NormalizedFiberLength*FiberForce max';
				
		#target_column_data[row_counter, 3+jump_idx] = FiberForce[time == MaxTorqueTime] * FiberVelocity[time == MaxTorqueTime];
		







		# ---------------------------------------------------------------------------
		row_counter = row_counter + 1;
		target_column_data[row_counter, 1] = muscle;
		target_column_data[row_counter, 2] = 'Srednee metabolizma';
		target_column_data[row_counter, 3] = 'Average(metabolic_probes)';
		
		metabolic_probes = jump[ 3:length(jump[,1]), ((jump[2,] == paste('metabolics_', muscle, sep='')) & (jump[1,] == 'metabolic_probes'))];
		metabolic_probes = as.numeric(gsub(",", "\\.", metabolic_probes));
	
		target_column_data[row_counter, 3+jump_idx] = mean(metabolic_probes);






		# ---------------------------------------------------------------------------
		row_counter = row_counter + 1;
		target_column_data[row_counter, 1] = muscle;
		target_column_data[row_counter, 2] = 'Find';
		target_column_data[row_counter, 3] = 'Max(FiberForce / MaxIsometricForce * controls)';
		
		controls = jump[ 3:length(jump[,1]), ((jump[2,] == muscle) & (jump[1,] == 'controls'))];
		controls = as.numeric(gsub(",", "\\.", controls));
	
		target_column_data[row_counter, 3+jump_idx] = max(controls * FiberForce / MaxIsometricForce);
		
	}
	
	current_row_to_fill = current_row_to_fill + 7;
}


normalized_file = paste(path,'/aggregated_CMC_muscles_table.csv',sep='')
write.table(target_column_names, file = normalized_file, append = FALSE, col.names = FALSE, row.names=FALSE, dec=',', sep=';')
write.table(target_column_data, file = normalized_file, append = TRUE, col.names = FALSE, row.names=FALSE, dec=',', sep=';')
