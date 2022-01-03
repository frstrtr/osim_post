import csv
import array
import shutil
import sys
import os
from os import walk

MODEL_FILE = "<model_file>"
SLASH_MODEL_FILE = "</model_file>"
XML_MODEL_FILE = "<model_file>gait_with_arms_scaled.osim</model_file>"

#athlete = '000_test'
#experiment = 'jump_jo 1'
#athlete = '001_lomakina'
#experiment = 'jump_jo 2'
#athlete = '002_anufriev'
#experiment = 'jump_j 2'
#athlete = '003_khoroshilov'
#experiment = 'jump_jo 1'
#athlete = '004_rakhimova'
#experiment = 'jump_j300 2'

if (len(sys.argv)!=3):
	print("Usage: python osim_processing_prep.py athlete experiment")
	print("Example: python osim_processing_prep.py 001_lomakina \"jump_jo 2\"")	
	exit(0)

#athlete = 'denshikov' #sys.argv[1]
path = sys.argv[1]
athlete = sys.argv[2]

DATA_DIR = '/data/'

f0 = []
# for (dirpath, dirnames, filenames) in walk(athlete+DATA_DIR):
for (dirpath, dirnames, filenames) in walk(path+athlete+DATA_DIR):
    f0.extend(filenames)
    break
	
for k in f0:
	if(k.find(' ') != -1):
		old_k = k
		new_k = k.replace(' ','_')
		os.rename(path+athlete+DATA_DIR+ '/' + old_k, path+athlete+DATA_DIR+ '/' + new_k)
					
# jumps = dict.fromkeys(['_jo_', '_j_', '_jo100_', '_j100_', '_jo300_', '_j300_', '_jo500_', '_j500_'])
jumps = dict.fromkeys(['_js_','_jo_', '_j_', '_jo100_', '_j100_', '_jo300_', '_j300_', '_jo500_', '_j500_'])

f = []
for (dirpath, dirnames, filenames) in walk(path+athlete+DATA_DIR):
    f.extend(filenames)
    break

# passing only last try in each kind of jump to the jumps list - why?
for i in jumps:
	for j in f:
		if(j.find(i) != -1) & (j.find('.tsv') != -1) & (j.find('_f_')==-1) & (j.find('_a')==-1):
			jumps[i] = j.replace('.tsv','')
	
batch_final = path+athlete+DATA_DIR+'run_r.bat'
	
for value in jumps.values():
	experiment = str(value)
	# no need to change variables below
	data_source = path+athlete+DATA_DIR+experiment+'.tsv'
	data_target = path+athlete+DATA_DIR+experiment+'.trc'
	
	antropometry = path+athlete+DATA_DIR+'antrop.txt'

	scale_config_template = 'templates/1_scale_config_template.xml'
	ik_config_template = 'templates/2_ik_config_template.xml'
	id_config_template = 'templates/3_id_config_template.xml'
	external_forces_template = 'templates/external_forces_template.xml'
	analyze_config_template = 'templates/4_analyze_config_template.xml'
	
	EXTERNAL_FORCES_XML = '_external_forces.xml'
	scale_config = path+athlete+DATA_DIR+experiment+'_1_scale_config.xml'
	ik_config = path+athlete+DATA_DIR+experiment+'_2_ik_config.xml'
	id_config = path+athlete+DATA_DIR+experiment+'_3_id_config.xml'
	external_forces = path+athlete+DATA_DIR+experiment+EXTERNAL_FORCES_XML
	analyze_config = path+athlete+DATA_DIR+experiment+'_4_analyze_config.xml'

	model_file = 'gait_with_arms.osim'
	model_file_path = './models/'
	model_target_file = path+athlete+DATA_DIR+experiment+'_'+model_file
	model_file_scaled = 'gait_with_arms_scaled.osim'

	left_force_source = path+athlete+DATA_DIR+experiment+'_f_1.tsv'
	right_force_source = path+athlete+DATA_DIR+experiment+'_f_2.tsv'
	forces_target = path+athlete+DATA_DIR+experiment+'_ext_forces.mot'

	#batch_target = 'data/'+athlete+'/run_opensim.bat'
	BATCH_TARGET = path+athlete+DATA_DIR+experiment+'_run_opensim.log'
	
	with open(data_source, 'r') as infile, open(data_target, 'w') as outfile, open(antropometry, 'r') as antropometry_file, open(scale_config_template, 'r') as scale_config_template_file, open(ik_config_template, 'r') as ik_config_template_file, open(analyze_config_template, 'r') as analyze_config_template_file, open(id_config_template, 'r') as id_config_template_file, open(analyze_config, 'w') as analyze_config_file, open(scale_config, 'w') as scale_config_file, open(ik_config, 'w') as ik_config_file, open(id_config, 'w') as id_config_file, open(left_force_source, 'r') as left_force_infile,  open(right_force_source, 'r') as right_force_infile, open(forces_target, 'w') as forces_outfile, open(external_forces_template, 'r') as external_forces_template_file, open(external_forces, 'w') as external_forces_file:
		
		# copying the source model (.osim file)
		shutil.copy(model_file_path+model_file, model_target_file)
		
		# generating .xml configs
		SEX = ''
		WEIGHT = ''
		HEIGHT = ''

		STOP_TIME = ''

		antrop_reader = csv.reader(antropometry_file, delimiter=',', quotechar='"', skipinitialspace=True)
		for row in antrop_reader:
			if row[0].lower() == 'sex':
				SEX = row[1]
			if row[0].lower() == 'weight':
				WEIGHT = row[1]
			if row[0].lower() == 'height':
				HEIGHT = row[1]

		# generating the tsv -> trc files
		reader = csv.reader(infile, delimiter='\t', quotechar='"', skipinitialspace=True)
		writer = csv.writer(outfile, delimiter='\t', lineterminator='\n')

		i = 1
		NO_OF_FRAMES = 0
		NO_OF_CAMERAS = 0
		NO_OF_MARKERS = 0
		FREQUENCY = 0
		MARKER_NAMES = ''
		EXTRA_COLUMNS_TO_BE_REMOVED = 0

		for row in reader:
			if i <= 10:
				print(row)
			if i == 1:
				NO_OF_FRAMES = row[1]
			if i == 2:
				NO_OF_CAMERAS = row[1]
			if i == 3:
				NO_OF_MARKERS = row[1]
			if i == 4:
				FREQUENCY = row[1]
			
			if i == 10:
				MARKER_NAMES = row

				print("writing header to output")
				
				writer.writerow(['PathFileType', 4,	'(X/Y/Z)', data_target])
				writer.writerow(['DataRate','CameraRate','NumFrames','NumMarkers','Units','OrigDataRate','OrigDataStartFrame','OrigNumFrames'])
				writer.writerow([FREQUENCY, FREQUENCY, NO_OF_FRAMES, NO_OF_MARKERS, "mm", FREQUENCY, "1", NO_OF_FRAMES])
				
				print(MARKER_NAMES)

				MARKERS_LINE_1 = ['Frame#', 'Time']
				MARKERS_LINE_2 = ['', '']
				
				for x in range(1,int(NO_OF_MARKERS)+1):
					MARKERS_LINE_1.append(MARKER_NAMES[x].lower())
					MARKERS_LINE_1.append('')
					MARKERS_LINE_1.append('')
					MARKERS_LINE_2.append('X'+str(x))
					MARKERS_LINE_2.append('Y'+str(x))
					MARKERS_LINE_2.append('Z'+str(x))

				writer.writerow(MARKERS_LINE_1)
				writer.writerow(MARKERS_LINE_2)

			if i == 11:
				if row[1] == "Frame":
					EXTRA_COLUMNS_TO_BE_REMOVED = EXTRA_COLUMNS_TO_BE_REMOVED + 1
				if row[1] == "Time":
					EXTRA_COLUMNS_TO_BE_REMOVED = EXTRA_COLUMNS_TO_BE_REMOVED + 1
				if row[2] == "Frame":
					EXTRA_COLUMNS_TO_BE_REMOVED = EXTRA_COLUMNS_TO_BE_REMOVED + 1
				if row[2] == "Time":
					EXTRA_COLUMNS_TO_BE_REMOVED = EXTRA_COLUMNS_TO_BE_REMOVED + 1
				if row[0] == "Frame":
					EXTRA_COLUMNS_TO_BE_REMOVED = EXTRA_COLUMNS_TO_BE_REMOVED + 1
				if row[0] == "Time":
					EXTRA_COLUMNS_TO_BE_REMOVED = EXTRA_COLUMNS_TO_BE_REMOVED + 1
					
				print("EXTRA_COLUMNS_TO_BE_REMOVED:")
				print(EXTRA_COLUMNS_TO_BE_REMOVED)

			if i >= 12:
				STOP_TIME = "{0:.4f}".format(1.00/float(FREQUENCY)*(i-12.00))
				VALUES = [i-11, "{0:.4f}".format(1.00/float(FREQUENCY)*(i-12.00))]
				for x in range(1,int(NO_OF_MARKERS)+1):
					#VALUES.append("{0:.3f}".format(- float(row[(x-1)*3 + EXTRA_COLUMNS_TO_BE_REMOVED]) + 370.0)) # X_opensim = -X_QTM
					#VALUES.append("{0:.3f}".format(float(row[(x-1)*3+2 + EXTRA_COLUMNS_TO_BE_REMOVED]) - 398.0)) # Y_opensim = Z_QTM
					#VALUES.append("{0:.3f}".format(float(row[(x-1)*3+1 + EXTRA_COLUMNS_TO_BE_REMOVED]) - 972.0)) # Z_opensim = Y_QTM
					VALUES.append("{0:.3f}".format(- float(row[(x-1)*3 + EXTRA_COLUMNS_TO_BE_REMOVED]))) # X_opensim = -X_QTM
					VALUES.append("{0:.3f}".format(float(row[(x-1)*3+2 + EXTRA_COLUMNS_TO_BE_REMOVED]))) # Y_opensim = Z_QTM
					VALUES.append("{0:.3f}".format(float(row[(x-1)*3+1 + EXTRA_COLUMNS_TO_BE_REMOVED]))) # Z_opensim = Y_QTM
				writer.writerow(VALUES)	
			i = i+1


		# now generating the forces
		# generating the tsv -> trc files
		
		force_left_reader = csv.reader(left_force_infile, delimiter='\t', quotechar='"', skipinitialspace=True)
		force_right_reader = csv.reader(right_force_infile, delimiter='\t', quotechar='"', skipinitialspace=True)
		force_writer = csv.writer(forces_outfile, delimiter='\t', lineterminator='\n')
		
		i = 1
		FORCE_NO_OF_SAMPLES = 0
		FORCE_FREQUENCY = 0
		FORCE_EXTRA_COLUMNS_TO_BE_REMOVED = 0
		
		FORCE_PLATE_CORNER_POSX_POSY_X = 0
		FORCE_PLATE_CORNER_POSX_POSY_Y = 0
		RIGHT_FORCE_PLATE_CORNER_POSX_POSY_X = 0
		RIGHT_FORCE_PLATE_CORNER_POSX_POSY_Y = 0
		
		PLATFORM_CALIB_LEFT_X = - 0.793
		PLATFORM_CALIB_LEFT_Y = + 6.550
		PLATFORM_CALIB_LEFT_Z = + 0.730
		
		PLATFORM_CALIB_RIGHT_X = + 15.833
		PLATFORM_CALIB_RIGHT_Y = - 59.609
		PLATFORM_CALIB_RIGHT_Z = - 15.948
		
		
		
		
		while True:
			try:
				row = next(force_left_reader)
				row_right = next(force_right_reader)
				
				if i <= 10:
					print(row)
				if i == 1:
					FORCE_NO_OF_SAMPLES = row[1]
				if i == 2:
					FORCE_FREQUENCY = row[1]
				if i == 10:
					FORCE_PLATE_CORNER_POSX_POSY_X = row[1]
					RIGHT_FORCE_PLATE_CORNER_POSX_POSY_X = row_right[1]
				if i == 11:
					FORCE_PLATE_CORNER_POSX_POSY_Y = row[1]
					RIGHT_FORCE_PLATE_CORNER_POSX_POSY_Y = row_right[1]
				if i == 24:
					print("writing header to force output")
					print(row)
					force_writer.writerow(['Coordinates'])
					force_writer.writerow(['version=1'])
					force_writer.writerow(['nRows='+str(int(FORCE_NO_OF_SAMPLES))])
					force_writer.writerow(['nColumns=19'])
					force_writer.writerow(['inDegrees=yes'])
					force_writer.writerow([])
					force_writer.writerow(['Units are S.I. units (second, meters, Newtons, ...)'])
					force_writer.writerow(['Angles are in degrees.'])
					force_writer.writerow([])
					force_writer.writerow(['endheader'])
					
					force_writer.writerow(['time','ground_force_vx','ground_force_vy','ground_force_vz','ground_force_px','ground_force_py','ground_force_pz','1_ground_force_vx','1_ground_force_vy','1_ground_force_vz','1_ground_force_px','1_ground_force_py','1_ground_force_pz','ground_torque_x','ground_torque_y','ground_torque_z','1_ground_torque_x','1_ground_torque_y','1_ground_torque_z'])
					
					if row[0] == "SAMPLE":
						FORCE_EXTRA_COLUMNS_TO_BE_REMOVED = FORCE_EXTRA_COLUMNS_TO_BE_REMOVED + 1
					if row[1] == "SAMPLE":
						FORCE_EXTRA_COLUMNS_TO_BE_REMOVED = FORCE_EXTRA_COLUMNS_TO_BE_REMOVED + 1
					if row[0] == "TIME":
						FORCE_EXTRA_COLUMNS_TO_BE_REMOVED = FORCE_EXTRA_COLUMNS_TO_BE_REMOVED + 1
					if row[1] == "TIME":
						FORCE_EXTRA_COLUMNS_TO_BE_REMOVED = FORCE_EXTRA_COLUMNS_TO_BE_REMOVED + 1
					
					print("FORCE_EXTRA_COLUMNS_TO_BE_REMOVED:")
					print(FORCE_EXTRA_COLUMNS_TO_BE_REMOVED)
					
				if i >= 28:
					VALUES = ["{0:.4f}".format(1.00/float(FORCE_FREQUENCY)*(i-25.00))]
					
					#calibrating ground forces
					row[(1-1)*3+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED] = float(row[(1-1)*3+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) - PLATFORM_CALIB_LEFT_X
					row[(1-1)*3+2+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED] = float(row[(1-1)*3+2+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) + PLATFORM_CALIB_LEFT_Y
					row[(1-1)*3+1+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED] = float(row[(1-1)*3+1+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) + PLATFORM_CALIB_LEFT_Z
					row_right[(1-1)*3+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED] = float(row_right[(1-1)*3+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) - PLATFORM_CALIB_RIGHT_X
					row_right[(1-1)*3+2+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED] = float(row_right[(1-1)*3+2+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) + PLATFORM_CALIB_RIGHT_Y
					row_right[(1-1)*3+1+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED] = float(row_right[(1-1)*3+1+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) + PLATFORM_CALIB_RIGHT_Z
					
					#ground_force_vx ground_force_vy ground_force_vz	
					VALUES.append("{0:.3f}".format(- float(row[(1-1)*3+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED])))
					VALUES.append("{0:.3f}".format(float(row[(1-1)*3+2+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED])))
					VALUES.append("{0:.3f}".format(float(row[(1-1)*3+1+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED])))
					
					#ground_force_px ground_force_py ground_force_pz
					VALUES.append("{0:.3f}".format(-(float(row[6+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) + float(FORCE_PLATE_CORNER_POSX_POSY_X) - 250)/1000)) # X_opensim = -X_QTM
					VALUES.append("{0:.3f}".format(float(row[8+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED])/1000)) # Y_opensim = Z_QTM
					VALUES.append("{0:.3f}".format((float(row[7+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) + float(FORCE_PLATE_CORNER_POSX_POSY_Y) - 250)/1000)) # Z_opensim = Y_QTM
					
					#now the other file 1_ground_force_vx	1_ground_force_vy	1_ground_force_vz	1_ground_force_px	1_ground_force_py	1_ground_force_pz
					VALUES.append("{0:.3f}".format(- float(row_right[(1-1)*3+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED])))
					VALUES.append("{0:.3f}".format(float(row_right[(1-1)*3+2+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED])))
					VALUES.append("{0:.3f}".format(float(row_right[(1-1)*3+1+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED])))
					
					#1_ground_force_px	1_ground_force_py	1_ground_force_pz
					VALUES.append("{0:.3f}".format(-(float(row_right[6+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) + float(RIGHT_FORCE_PLATE_CORNER_POSX_POSY_X) - 250)/1000)) # X_opensim = -X_QTM
					VALUES.append("{0:.3f}".format(float(row_right[8+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED])/1000)) # Y_opensim = Z_QTM
					VALUES.append("{0:.3f}".format((float(row_right[7+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) + float(RIGHT_FORCE_PLATE_CORNER_POSX_POSY_Y) - 250)/1000)) # Z_opensim = Y_QTM
					
					#now the moments ground_torque_x	ground_torque_y	ground_torque_z	1_ground_torque_x	1_ground_torque_y	1_ground_torque_z
					torque_X = 0
					torque_Y = float(row[(2-1)*3+2+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) - ( - float(row[(2-1)*3+1+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) / float(row[(1-1)*3+2+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) * float(row[(1-1)*3+1+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) - float(row[(2-1)*3+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) / float(row[(1-1)*3+2+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) * float(row[(1-1)*3+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) )
					torque_Z = 0
					VALUES.append("{0:.5f}".format(float(torque_X)))
					VALUES.append("{0:.5f}".format(float(torque_Y)))
					VALUES.append("{0:.5f}".format(float(torque_Z)))
					
					torque_X = 0
					torque_Y = float(row_right[(2-1)*3+2+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) - ( - float(row_right[(2-1)*3+1+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) / float(row_right[(1-1)*3+2+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) * float(row_right[(1-1)*3+1+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) - float(row_right[(2-1)*3+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) / float(row_right[(1-1)*3+2+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) * float(row_right[(1-1)*3+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) )
					torque_Z = 0
					VALUES.append("{0:.5f}".format(float(torque_X)))
					VALUES.append("{0:.5f}".format(float(torque_Y)))
					VALUES.append("{0:.5f}".format(float(torque_Z)))
					
					
					force_writer.writerow(VALUES)
				i = i+1
			except StopIteration:
				break

		template = scale_config_template_file.read()
		content = template.replace("<mass></mass>","<mass>"+WEIGHT+"</mass>").replace("<marker_file></marker_file>", "<marker_file>"+experiment+'.trc'+"</marker_file>").replace("<model_file>gait_with_arms.osim</model_file>",MODEL_FILE+experiment+'_'+model_file+SLASH_MODEL_FILE).replace("<output_model_file>gait_with_arms_scaled.osim</output_model_file>","<output_model_file>"+experiment+'_'+model_file_scaled+"</output_model_file>")
		scale_config_file.write(content)
		
		template = ik_config_template_file.read()
		content = template.replace("<marker_file></marker_file>", "<marker_file>"+experiment+'.trc'+"</marker_file>").replace("<output_motion_file></output_motion_file>", "<output_motion_file>"+experiment+'.mot'+"</output_motion_file>").replace("STOP_TIME", STOP_TIME).replace(XML_MODEL_FILE,MODEL_FILE+experiment+'_'+model_file_scaled+SLASH_MODEL_FILE)
		ik_config_file.write(content)
		
		template =id_config_template_file.read()
		content = template.replace("<coordinates_file></coordinates_file>","<coordinates_file>"+experiment+'.mot'+"</coordinates_file>").replace("<output_gen_force_file></output_gen_force_file>","<output_gen_force_file>"+experiment+'_inverse_dynamics.sto'+"</output_gen_force_file>").replace("<output_body_forces_file></output_body_forces_file>","<output_body_forces_file>"+experiment+'_body_forces_at_joints.sto'+"</output_body_forces_file>").replace("STOP_TIME", STOP_TIME).replace(XML_MODEL_FILE,MODEL_FILE+experiment+'_'+model_file_scaled+SLASH_MODEL_FILE).replace("<external_loads_file>external_forces.xml</external_loads_file>", "<external_loads_file>"+experiment+EXTERNAL_FORCES_XML+"</external_loads_file>")
		id_config_file.write(content)

		template =external_forces_template_file.read()
		content = template.replace("<datafile></datafile>","<datafile>"+experiment+'_ext_forces.mot'+"</datafile>").replace("<external_loads_model_kinematics_file></external_loads_model_kinematics_file>", "<external_loads_model_kinematics_file>"+experiment+'.mot'+"</external_loads_model_kinematics_file>")
		external_forces_file.write(content)
			
		template =analyze_config_template_file.read()
		content = template.replace("<coordinates_file></coordinates_file>","<coordinates_file>"+experiment+'.mot'+"</coordinates_file>").replace("STOP_TIME", STOP_TIME).replace(XML_MODEL_FILE,MODEL_FILE+experiment+'_'+model_file_scaled+SLASH_MODEL_FILE).replace("<external_loads_file>external_forces.xml</external_loads_file>", "<external_loads_file>"+experiment+EXTERNAL_FORCES_XML+"</external_loads_file>").replace("<AnalyzeTool name=\"gait_reduced_nonscaled-scaled\">","<AnalyzeTool name=\""+experiment+"_model\">")
		analyze_config_file.write(content)

	print(" ")
	print("complete preprocessing.")


	with open(BATCH_TARGET, 'w') as batch_file:
		batch_file.write("cd data\\"+athlete+"\n")
		batch_file.write("scale -S "+experiment+"_1_scale_config.xml\n")
		batch_file.write("ik -S "+experiment+"_2_ik_config.xml\n")
		batch_file.write("id -S "+experiment+"_3_id_config.xml\n")
		batch_file.write("analyze -S "+experiment+"_4_analyze_config.xml\n")
		#batch_file.write("pause\n")
		
	# os.startfile(batch_target)
	# print("create bat.")
	#input("Press Enter to continue...")
	
	'''
	from os import walk
	f1 = []
	for (dirpath, dirnames, filenames) in walk(athlete+DATA_DIR):
		f1.extend(filenames)
	fn = ['gait_with_arms_scaled']
	for i in f1:
		for j in fn:        
			if(i.find(j) != -1) & (i.find('.osim') != -1):
				os.rename(athlete+DATA_DIR + '\\'  + i, athlete+DATA_DIR + '\\'  + experiment + '_' + j + '.osim')
				
	'''
	print("complete bat.")

print("copy osim file.")
shutil.copyfile('./scripts/osim_post_processing.r', './trials/'+athlete+DATA_DIR+'osim_post_processing.r')

input("Press Enter to continue...")
	
with open(batch_final, 'w') as batch_file:
	batch_file.write("Rscript.exe osim_post_processing.r " + path + " " + athlete + "\n")
	batch_file.write("pause\n")
	
# os.startfile(batch_final)	
print("executing batchfile")
#os.system("cmd /k " + batch_target)
