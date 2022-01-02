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

#------------------------------------------------------------------#
logdebug('loading antropometry...');
antropometry = scan(file = paste(path,'//',athlete,'/antrop.txt',sep=''), what = character())
sex <- antropometry[2]
weight <- as.numeric(antropometry[4])
height <- as.numeric(antropometry[6])

files <- Sys.glob(file.path(path, "*normalized_CMC.csv"))
joint_power_files <- Sys.glob(file.path(path, "*_powers.csv"))
platform_force_files <- Sys.glob(file.path(path, "*_ext_forces.mot"))



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


jump_j100_joint_powers <- FALSE
jump_j300_joint_powers <- FALSE
jump_j500_joint_powers  <- FALSE
jump_j_joint_powers  <- FALSE
jump_jo100_joint_powers <- FALSE
jump_jo300_joint_powers <- FALSE
jump_jo500_joint_powers <- FALSE
jump_jo_joint_powers  <- FALSE
jump_js_joint_powers    <- FALSE

for (i in 1:length(joint_power_files))
{
	filename = joint_power_files[i];
	
		  if (grepl("jump_j100_", filename)) { jump_j100_joint_powers = read.table(filename, header = TRUE, sep=";", dec=','); }
	else  if (grepl("jump_j300_", filename)) { jump_j300_joint_powers = read.table(filename, header = TRUE, sep=";", dec=','); }
	else  if (grepl("jump_j500_", filename)) { jump_j500_joint_powers = read.table(filename, header = TRUE, sep=";", dec=','); }
	else  if (grepl("jump_j_", filename)) { jump_j_joint_powers = read.table(filename, header = TRUE, sep=";", dec=','); }
	else  if (grepl("jump_jo100_", filename)) { jump_jo100_joint_powers = read.table(filename, header = TRUE, sep=";", dec=','); }
	else  if (grepl("jump_jo300_", filename)) { jump_jo300_joint_powers = read.table(filename, header = TRUE, sep=";", dec=','); }
	else  if (grepl("jump_jo500_", filename)) { jump_jo500_joint_powers = read.table(filename, header = TRUE, sep=";", dec=','); }
	else  if (grepl("jump_jo_", filename)) { jump_jo_joint_powers = read.table(filename, header = TRUE, sep=";", dec=','); }
	else  if (grepl("jump_js_", filename)) { jump_js_joint_powers = read.table(filename, header = TRUE, sep=";", dec=','); } 
}


# load the final report file
final_report = read.table(paste(path,'/Report.csv', sep=''), header = TRUE, sep=";", dec=',')

jumps_in_final_report = length(final_report[,2])
final_clean_report_column_names = matrix(nrow=1, ncol=7)
final_clean_report_data = matrix(nrow=jumps_in_final_report, ncol=7)

final_clean_report_column_names[, 1] = 'JUMP';
final_clean_report_column_names[, 2] = 'Amplituda COM';
final_clean_report_column_names[, 3] = 'Vremya Na Platforme (sec)';
final_clean_report_column_names[, 4] = 'Amortizaciya (sec)';
final_clean_report_column_names[, 5] = 'Ottalkivanie (sec)';
final_clean_report_column_names[, 6] = 'Start force (Nm)';
final_clean_report_column_names[, 7] = 'Power Sum Max';

final_clean_report_data[,1] = as.character(final_report[,1])
final_clean_report_data[,2] = as.double(gsub(",",".",as.character(final_report[,2])))
final_clean_report_data[,3] = as.double(gsub(",",".",as.character(final_report[,3])))
final_clean_report_data[,4] = as.double(gsub(",",".",as.character(final_report[,4])))
final_clean_report_data[,5] = as.double(gsub(",",".",as.character(final_report[,5])))

t__ammortizaciya = final_clean_report_data[,4]
t__ottalkivanie = final_clean_report_data[,5]

#final_clean_report_data[,6] = as.double(gsub(",",".",as.character(final_report[,6])))
final_clean_report_data[,7] = as.double(gsub(",",".",as.character(final_report[,8])))

normalized_report_file = paste(path,'/aggregated_Final_Report.csv',sep='')
write.table(final_clean_report_column_names, file = normalized_report_file, append = FALSE, col.names = FALSE, row.names=FALSE, dec=',', sep=';')
write.table(final_clean_report_data, file = normalized_report_file, append = TRUE, col.names = FALSE, row.names=FALSE, dec=',', sep=';')


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




output_column_names = matrix(nrow=1, ncol=19)
output_column_names[, 1] = 'jumpname';
output_column_names[, 2] = 'first_phase_integrated_knee_joint_power';
output_column_names[, 3] = 'second_phase_integrated_knee_joint_power';
output_column_names[, 4] = 'first_phase_knee_energy_transfer';
output_column_names[, 5] = 'second_phase_knee_energy_transfer';
output_column_names[, 6] = 'first_phase_knee_transfer_percent';
output_column_names[, 7] = 'second_phase_knee_transfer_percent';
output_column_names[, 8] = 'first_phase_integrated_ankle_joint_power';
output_column_names[, 9] = 'second_phase_integrated_ankle_joint_power';
output_column_names[, 10] = 'first_phase_ankle_energy_transfer';
output_column_names[, 11] = 'second_phase_ankle_energy_transfer';
output_column_names[, 12] = 'first_phase_ankle_transfer_percent';
output_column_names[, 13] = 'second_phase_ankle_transfer_percent';
output_column_names[, 14] = 'first_phase_integrated_hip_joint_power';
output_column_names[, 15] = 'second_phase_integrated_hip_joint_power';
output_column_names[, 16] = 'first_phase_hip_energy_transfer';
output_column_names[, 17] = 'second_phase_hip_energy_transfer';
output_column_names[, 18] = 'first_phase_hip_transfer_percent';
output_column_names[, 19] = 'second_phase_hip_transfer_percent';
normalized_file = paste(path,'/Energy_transfer.csv',sep='')
write.table(output_column_names, file = normalized_file, append = FALSE, col.names = FALSE, row.names=FALSE, dec=',', sep=';')


for (jump_idx in 1:length(jumps)) 
{

jumpname = jumps[jump_idx];

logdebug(paste('working with jump: ', jumpname, sep=''));

jump_cmc = eval(parse(text= jumpname));
jump_joint_powers = eval(parse(text= paste(jumpname,'_joint_powers', sep='')));

jump_joint_time = jump_joint_powers[,1]
jump_cmc_time = as.double(gsub(",",".",as.character(jump_cmc[3:length(jump_cmc[,1]),1])))

global_start_time = max(min(jump_joint_time), min(jump_cmc_time))
global_stop_time = min(max(jump_joint_time), max(jump_cmc_time))

jump_joint_mask = (jump_joint_time >= global_start_time) & (jump_joint_time <= global_stop_time)
jump_cmc_mask = (jump_cmc_time >= global_start_time) & (jump_cmc_time <= global_stop_time)
global_time = jump_joint_time[jump_joint_mask]

# find the necessary columns from CMC, build the power
semimem_r  = as.double(gsub(",",".",as.character(jump_cmc[3:length(jump_cmc[,1]),81])))
bifemsh_r  = as.double(gsub(",",".",as.character(jump_cmc[3:length(jump_cmc[,1]),82])))
glut_max1_r  = as.double(gsub(",",".",as.character(jump_cmc[3:length(jump_cmc[,1]),83])))
glut_max2_r  = as.double(gsub(",",".",as.character(jump_cmc[3:length(jump_cmc[,1]),84])))
glut_max3_r  = as.double(gsub(",",".",as.character(jump_cmc[3:length(jump_cmc[,1]),85])))
psoas_r  = as.double(gsub(",",".",as.character(jump_cmc[3:length(jump_cmc[,1]),86])))
rect_fem_r  = as.double(gsub(",",".",as.character(jump_cmc[3:length(jump_cmc[,1]),87])))
vas_lat_r  = as.double(gsub(",",".",as.character(jump_cmc[3:length(jump_cmc[,1]),88])))
lat_gas_r = as.double(gsub(",",".",as.character(jump_cmc[3:length(jump_cmc[,1]),89])))
soleus_r  = as.double(gsub(",",".",as.character(jump_cmc[3:length(jump_cmc[,1]),90])))
tib_ant_r = as.double(gsub(",",".",as.character(jump_cmc[3:length(jump_cmc[,1]),91])))

ext_force = as.double(gsub(",",".",as.character(jump_cmc[3:length(jump_cmc[,1]),3])))

knee_cmc_power = glut_max1_r+ glut_max2_r+glut_max3_r+ semimem_r+psoas_r+rect_fem_r;
hip_cmc_power = lat_gas_r+ vas_lat_r+ bifemsh_r+ semimem_r+ rect_fem_r; 
ankle_cmc_power = lat_gas_r + soleus_r + tib_ant_r;

knee_cmc_power = knee_cmc_power[jump_cmc_mask]
ankle_cmc_power = ankle_cmc_power[jump_cmc_mask]
hip_cmc_power = hip_cmc_power[jump_cmc_mask]

ext_force = ext_force[jump_cmc_mask]

ankle_joint_power = jump_joint_powers[,2]
knee_joint_power = jump_joint_powers[,3]
hip_joint_power =  - jump_joint_powers[,4]

ankle_joint_power = ankle_joint_power[jump_joint_mask] * weight / 10.0
knee_joint_power = knee_joint_power[jump_joint_mask] * weight / 10.0
hip_joint_power = hip_joint_power[jump_joint_mask] * weight / 10.0

final_clean_report_column_names[, 4] = 'Amortizaciya (sec)';
final_clean_report_column_names[, 5] = 'Ottalkivanie (sec)';

t_amortizaciya = as.double(final_clean_report_data[final_clean_report_data[,1] == jumpname, 4])
t_ottalkivanie = as.double(final_clean_report_data[final_clean_report_data[,1] == jumpname, 5])
t_platformzero = min(global_time[(global_time>t_ottalkivanie) & (ext_force < 1)])

# ENERGY TRANSFERS
first_phase_integrated_ankle_joint_power = sum(ankle_joint_power[(global_time < t_ottalkivanie)])*0.025
second_phase_integrated_ankle_joint_power = sum(ankle_joint_power[(global_time >= t_ottalkivanie) & (global_time < t_platformzero)])*0.025
first_phase_ankle_energy_transfer = sum(ankle_joint_power[(global_time < t_ottalkivanie)] - ankle_cmc_power[(global_time < t_ottalkivanie)])*0.025
second_phase_ankle_energy_transfer = sum(ankle_joint_power[(global_time >= t_ottalkivanie) & (global_time < t_platformzero)] - ankle_cmc_power[(global_time >= t_ottalkivanie) & (global_time < t_platformzero)])*0.025
first_phase_ankle_transfer_percent = first_phase_ankle_energy_transfer / first_phase_integrated_ankle_joint_power
second_phase_ankle_transfer_percent = second_phase_ankle_energy_transfer / second_phase_integrated_ankle_joint_power

first_phase_integrated_knee_joint_power = sum(knee_joint_power[(global_time < t_ottalkivanie)])*0.025
second_phase_integrated_knee_joint_power = sum(knee_joint_power[(global_time >= t_ottalkivanie) & (global_time < t_platformzero)])*0.025
first_phase_knee_energy_transfer = sum(knee_joint_power[(global_time < t_ottalkivanie)] - knee_cmc_power[(global_time < t_ottalkivanie)])*0.025
second_phase_knee_energy_transfer = sum(knee_joint_power[(global_time >= t_ottalkivanie) & (global_time < t_platformzero)] - knee_cmc_power[(global_time >= t_ottalkivanie) & (global_time < t_platformzero)])*0.025
first_phase_knee_transfer_percent = first_phase_knee_energy_transfer / first_phase_integrated_knee_joint_power
second_phase_knee_transfer_percent = second_phase_knee_energy_transfer / second_phase_integrated_knee_joint_power

first_phase_integrated_hip_joint_power = sum(hip_joint_power[(global_time < t_ottalkivanie)])*0.025
second_phase_integrated_hip_joint_power = sum(hip_joint_power[(global_time >= t_ottalkivanie) & (global_time < t_platformzero)])*0.025
first_phase_hip_energy_transfer = sum(hip_joint_power[(global_time < t_ottalkivanie)] - hip_cmc_power[(global_time < t_ottalkivanie)])*0.025
second_phase_hip_energy_transfer = sum(hip_joint_power[(global_time >= t_ottalkivanie) & (global_time < t_platformzero)] - hip_cmc_power[(global_time >= t_ottalkivanie) & (global_time < t_platformzero)])*0.025
first_phase_hip_transfer_percent = first_phase_hip_energy_transfer / first_phase_integrated_hip_joint_power
second_phase_hip_transfer_percent = second_phase_hip_energy_transfer / second_phase_integrated_hip_joint_power


output_data = matrix(nrow=1, ncol=19)
output_data[, 1] = jumpname;
output_data[, 2] = first_phase_integrated_knee_joint_power;
output_data[, 3] = second_phase_integrated_knee_joint_power;
output_data[, 4] = first_phase_knee_energy_transfer;
output_data[, 5] = second_phase_knee_energy_transfer;
output_data[, 6] = first_phase_knee_transfer_percent;
output_data[, 7] = second_phase_knee_transfer_percent;
output_data[, 8] = first_phase_integrated_ankle_joint_power;
output_data[, 9] = second_phase_integrated_ankle_joint_power;
output_data[, 10] = first_phase_ankle_energy_transfer;
output_data[, 11] = second_phase_ankle_energy_transfer;
output_data[, 12] = first_phase_ankle_transfer_percent;
output_data[, 13] = second_phase_ankle_transfer_percent;
output_data[, 14] = first_phase_integrated_hip_joint_power;
output_data[, 15] = second_phase_integrated_hip_joint_power;
output_data[, 16] = first_phase_hip_energy_transfer;
output_data[, 17] = second_phase_hip_energy_transfer;
output_data[, 18] = first_phase_hip_transfer_percent;
output_data[, 19] = second_phase_hip_transfer_percent;
normalized_file = paste(path,'/Energy_transfer.csv',sep='')
write.table(output_data, file = normalized_file, append = TRUE, col.names = FALSE, row.names=FALSE, dec=',', sep=';')








output_column_names = matrix(nrow=1, ncol=7)
output_data = matrix(nrow=length(global_time), ncol=7)

output_column_names[, 1] = 'time';
output_column_names[, 2] = 'knee_cmc_power';
output_column_names[, 3] = 'ankle_cmc_power';
output_column_names[, 4] = 'hip_cmc_power';
output_column_names[, 5] = 'knee_joint_power';
output_column_names[, 6] = 'ankle_joint_power';
output_column_names[, 7] = 'hip_joint_power';

output_data[, 1] = global_time;
output_data[, 2] = knee_cmc_power;
output_data[, 3] = ankle_cmc_power;
output_data[, 4] = hip_cmc_power;
output_data[, 5] = knee_joint_power;
output_data[, 6] = ankle_joint_power;
output_data[, 7] = hip_joint_power;

normalized_file = paste(path,'/', jumpname, '_CMC_and_joint_power.csv',sep='')
write.table(output_column_names, file = normalized_file, append = FALSE, col.names = FALSE, row.names=FALSE, dec=',', sep=';')
write.table(output_data, file = normalized_file, append = TRUE, col.names = FALSE, row.names=FALSE, dec=',', sep=';')










save_to_file = TRUE

# ----------------------------
if (save_to_file)
{
	png_file = paste(jumpname,'_power_knee.png', sep = "")
	png(png_file, width=1000, height=700, units="px", res=106)
}
plot(x = global_time, y = knee_joint_power, col = 'white', ylim = c(min(knee_joint_power, knee_cmc_power), max(knee_joint_power, knee_cmc_power)), ann=FALSE)

lines(x = c(min(global_time), max(global_time)), y=c(0,0), col="black")
lines(x = global_time, y = knee_cmc_power, col = "red", lwd=2, lty='solid')
lines(x = global_time, y = knee_joint_power, col = "blue", lwd=2, lty='solid')

lines(x = c(t_amortizaciya, t_amortizaciya), y = c(-10000, 10000), col = "purple", lwd=1, lty='solid')
lines(x = c(t_ottalkivanie, t_ottalkivanie), y = c(-10000, 10000), col = "darkgreen", lwd=1, lty='solid')

title(main=paste(jumpname, " - Knee - CMC power vs joint power", sep=''))
title(ylab="Power (Watts)")
title(xlab=paste("Time (seconds)", sep="")) 

legend(x = "topright", c(paste("CMC power",sep=''), paste("Joint power",sep=''), 
paste("Ammortizaciya (", t_amortizaciya, ' sec)', sep=''), paste("Ottalkivanie (", t_ottalkivanie, ' sec)', sep='')), 
   col=c('red', "blue", "purple", 'darkgreen'), lty=c(1,1,1,1), lwd=c(2,2,1,1));

if(save_to_file) dev.off();
# ----------------------------


# ----------------------------
if (save_to_file)
{
	png_file = paste(jumpname,'_power_ankle.png', sep = "")
	png(png_file, width=1000, height=700, units="px", res=106)
}
plot(x = global_time, y = ankle_joint_power, col = 'white', ylim = c(min(ankle_joint_power, ankle_cmc_power), max(ankle_joint_power, ankle_cmc_power)), ann=FALSE)

lines(x = c(min(global_time), max(global_time)), y=c(0,0), col="black")
lines(x = global_time, y = ankle_cmc_power, col = "red", lwd=2, lty='solid')
lines(x = global_time, y = ankle_joint_power, col = "blue", lwd=2, lty='solid')

lines(x = c(t_amortizaciya, t_amortizaciya), y = c(-10000, 10000), col = "purple", lwd=1, lty='solid')
lines(x = c(t_ottalkivanie, t_ottalkivanie), y = c(-10000, 10000), col = "darkgreen", lwd=1, lty='solid')

title(main=paste(jumpname, " - ankle - CMC power vs joint power", sep=''))
title(ylab="Power (Watts)")
title(xlab=paste("Time (seconds)", sep="")) 

legend(x = "topright", c(paste("CMC power",sep=''), paste("Joint power",sep=''), 
paste("Ammortizaciya (", t_amortizaciya, ' sec)', sep=''), paste("Ottalkivanie (", t_ottalkivanie, ' sec)', sep='')), 
   col=c('red', "blue", "purple", 'darkgreen'), lty=c(1,1,1,1), lwd=c(2,2,1,1));

if(save_to_file) dev.off();
# ----------------------------ankle


# ----------------------------
if (save_to_file)
{
	png_file = paste(jumpname,'_power_hip.png', sep = "")
	png(png_file, width=1000, height=700, units="px", res=106)
}
plot(x = global_time, y = hip_joint_power, col = 'white', ylim = c(min(hip_joint_power, hip_cmc_power), max(hip_joint_power, hip_cmc_power)), ann=FALSE)

lines(x = c(min(global_time), max(global_time)), y=c(0,0), col="black")
lines(x = global_time, y = hip_cmc_power, col = "red", lwd=2, lty='solid')
lines(x = global_time, y = hip_joint_power, col = "blue", lwd=2, lty='solid')

lines(x = c(t_amortizaciya, t_amortizaciya), y = c(-10000, 10000), col = "purple", lwd=1, lty='solid')
lines(x = c(t_ottalkivanie, t_ottalkivanie), y = c(-10000, 10000), col = "darkgreen", lwd=1, lty='solid')

title(main=paste(jumpname, " - hip - CMC power vs joint power", sep=''))
title(ylab="Power (Watts)")
title(xlab=paste("Time (seconds)", sep="")) 

legend(x = "topright", c(paste("CMC power",sep=''), paste("Joint power",sep=''), 
paste("Ammortizaciya (", t_amortizaciya, ' sec)', sep=''), paste("Ottalkivanie (", t_ottalkivanie, ' sec)', sep='')), 
   col=c('red', "blue", "purple", 'darkgreen'), lty=c(1,1,1,1), lwd=c(2,2,1,1));

if(save_to_file) dev.off();
# ----------------------------hip



# ----------------------------
if (save_to_file)
{
	png_file = paste(jumpname,'_platform_time.png', sep = "")
	png(png_file, width=1000, height=700, units="px", res=106)
}
plot(x = global_time, y = hip_joint_power, col = 'white', ylim = c(0, max(ext_force)), ann=FALSE)

lines(x = c(min(global_time), max(global_time)), y=c(0,0), col="black")
lines(x = global_time, y = ext_force, col = "darkgreen", lwd=2, lty='solid')

lines(x = c(t_amortizaciya, t_amortizaciya), y = c(-10000, 10000), col = "purple", lwd=1, lty='solid')
lines(x = c(t_ottalkivanie, t_ottalkivanie), y = c(-10000, 10000), col = "darkgreen", lwd=1, lty='solid')

title(main=paste(jumpname, " - platform force", sep=''))
title(ylab="Force (Newtons)")
title(xlab=paste("Time (seconds)", sep="")) 

legend(x = "topright", c(paste("Force",sep=''), 
paste("Ammortizaciya (", t_amortizaciya, ' sec)', sep=''), paste("Ottalkivanie (", t_ottalkivanie, ' sec)', sep='')), 
   col=c('darkgreen', "purple", 'darkgreen'), lty=c(1,1,1), lwd=c(2,1,1));

if(save_to_file) dev.off();
# ----------------------------hip



}

