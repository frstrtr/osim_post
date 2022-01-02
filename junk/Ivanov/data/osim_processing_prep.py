import csv
import array
import shutil
import sys
import os

# athlete = '000_test'
# experiment = 'jump_jo 1'
# athlete = '001_lomakina'
# experiment = 'jump_jo 2'
# athlete = '002_anufriev'
# experiment = 'jump_j 2'
# athlete = '003_khoroshilov'
# experiment = 'jump_jo 1'
# athlete = '004_rakhimova'
# experiment = 'jump_j300 2'

# if (len(sys.argv)!=3):
# 	print("Usage: python osim_processing_prep.py athlete experiment")
# 	print("Example: python osim_processing_prep.py 001_lomakina \"jump_jo 2\"")
# 	exit(0)

# athlete = sys.argv[1]
athlete = 'alekhin'
measurement_date='07-08-2020'

experiment_set = set()

for experiment_iter in os.listdir('.\\data\\'+athlete+'\\'):
    f_name, f_type = experiment_iter.split('.')
    if f_type == 'tsv':
        name_parts = f_name.split(' ')
        experiment_set.add(name_parts[0]+' ' + name_parts[1][:1])

print([e_set for e_set in experiment_set])

# exit()
# experiment = sys.argv[2]

# experiment = 'jump_j 2'

for experiment in experiment_set:
    # experiment = 'jump_j100 2'

    # no need to change variables below
    data_source = 'data/'+athlete+'/'+experiment+'.tsv'
    data_target = 'data/'+athlete+'/'+experiment+'.trc'

    antropometry = 'data/'+athlete+'/antrop.txt'

    scale_config_template = '1_scale_config_template.xml'
    ik_config_template = '2_ik_config_template.xml'
    id_config_template = '3_id_config_template.xml'
    external_forces_template = 'external_forces_template.xml'
    analyze_config_template = '4_analyze_config_template.xml'
    scale_config = 'data/'+athlete+'/'+'1_scale_config.xml'
    ik_config = 'data/'+athlete+'/'+'2_ik_config.xml'
    id_config = 'data/'+athlete+'/'+'3_id_config.xml'

    # experiment added
    external_forces = 'data/'+athlete+'/'+'external_forces.xml'

    analyze_config = 'data/'+athlete+'/'+'4_analyze_config.xml'

    model_file = 'gait_with_arms.osim'
    model_target_file = 'data/'+athlete+'/'+model_file

    left_force_source = 'data/'+athlete+'/'+experiment+'_f_1.tsv'
    right_force_source = 'data/'+athlete+'/'+experiment+'_f_2.tsv'

    # experiment added
    forces_target = 'data/'+athlete+'/'+experiment+'_ext_forces.mot'

    print(model_target_file, left_force_source,
          right_force_source, forces_target)

    # exit()

    with open(data_source, 'r') as infile,    open(data_target, 'w') as outfile, open(antropometry, 'r') as antropometry_file,    open(scale_config_template, 'r') as scale_config_template_file,    open(ik_config_template, 'r') as ik_config_template_file,    open(analyze_config_template, 'r') as analyze_config_template_file,    open(id_config_template, 'r') as id_config_template_file,    open(analyze_config, 'w') as analyze_config_file,    open(scale_config, 'w') as scale_config_file,    open(ik_config, 'w') as ik_config_file,    open(id_config, 'w') as id_config_file,    open(left_force_source, 'r') as left_force_infile,    open(right_force_source, 'r') as right_force_infile,    open(forces_target, 'w') as forces_outfile,    open(external_forces_template, 'r') as external_forces_template_file,    open(external_forces, 'w') as external_forces_file:

        # copying the source model (.osim file)
        shutil.copy(model_file, model_target_file)

        # generating .xml configs
        SEX = ''
        WEIGHT = ''
        HEIGHT = ''

        STOP_TIME = ''

        antrop_reader = csv.reader(
            antropometry_file, delimiter=',', quotechar='"', skipinitialspace=True)
        for row in antrop_reader:
            if row[0].lower() == 'sex':
                SEX = row[1]
            if row[0].lower() == 'weight':
                WEIGHT = row[1]
            if row[0].lower() == 'height':
                HEIGHT = row[1]

        # generating the tsv -> trc files
        reader = csv.reader(infile, delimiter='\t',
                            quotechar='"', skipinitialspace=True)
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
                writer.writerow(['DataRate', 'CameraRate', 'NumFrames', 'NumMarkers',
                                 'Units', 'OrigDataRate', 'OrigDataStartFrame', 'OrigNumFrames'])
                writer.writerow([FREQUENCY, FREQUENCY, NO_OF_FRAMES,
                                 NO_OF_MARKERS, "mm", FREQUENCY, "1", NO_OF_FRAMES])

                print(MARKER_NAMES)

                MARKERS_LINE_1 = ['Frame#', 'Time']
                MARKERS_LINE_2 = ['', '']

                for x in range(1, int(NO_OF_MARKERS)+1):
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
                VALUES = [
                    i-11, "{0:.4f}".format(1.00/float(FREQUENCY)*(i-12.00))]
                for x in range(1, int(NO_OF_MARKERS)+1):
                    # VALUES.append("{0:.3f}".format(- float(row[(x-1)*3 + EXTRA_COLUMNS_TO_BE_REMOVED]) + 370.0)) # X_opensim = -X_QTM
                    # VALUES.append("{0:.3f}".format(float(row[(x-1)*3+2 + EXTRA_COLUMNS_TO_BE_REMOVED]) - 398.0)) # Y_opensim = Z_QTM
                    # VALUES.append("{0:.3f}".format(float(row[(x-1)*3+1 + EXTRA_COLUMNS_TO_BE_REMOVED]) - 972.0)) # Z_opensim = Y_QTM
                    # X_opensim = -X_QTM
                    VALUES.append(
                        "{0:.3f}".format(- float(row[(x-1)*3 + EXTRA_COLUMNS_TO_BE_REMOVED])))
                    VALUES.append("{0:.3f}".format(
                        float(row[(x-1)*3+2 + EXTRA_COLUMNS_TO_BE_REMOVED])))  # Y_opensim = Z_QTM
                    VALUES.append("{0:.3f}".format(
                        float(row[(x-1)*3+1 + EXTRA_COLUMNS_TO_BE_REMOVED])))  # Z_opensim = Y_QTM
                writer.writerow(VALUES)
            i = i+1

        # now generating the forces
        # generating the tsv -> trc files

        force_left_reader = csv.reader(
            left_force_infile, delimiter='\t', quotechar='"', skipinitialspace=True)
        force_right_reader = csv.reader(
            right_force_infile, delimiter='\t', quotechar='"', skipinitialspace=True)
        force_writer = csv.writer(
            forces_outfile, delimiter='\t', lineterminator='\n')

        i = 1
        FORCE_NO_OF_SAMPLES = 0
        FORCE_FREQUENCY = 0
        FORCE_EXTRA_COLUMNS_TO_BE_REMOVED = 0

        while True:
            try:
                row = next(force_left_reader)
                row_right = next(force_right_reader)

                if i <= 27:
                    print(row)
                if i == 1:
                    FORCE_NO_OF_SAMPLES = row[1]
                if i == 2:
                    FORCE_FREQUENCY = row[1]
                if i == 3:  # TIME_STAMP	2020-08-07, 12:48:47.992	3065.10169880
                    NO_OF_MARKERS = row[1]
                if i == 24:
                    print("writing header to force output")
                    print(row)
                    force_writer.writerow(['Coordinates'])
                    force_writer.writerow(['version=1'])
                    force_writer.writerow(
                        ['nRows='+str(int(FORCE_NO_OF_SAMPLES))])
                    force_writer.writerow(['nColumns=19'])
                    force_writer.writerow(['inDegrees=yes'])
                    force_writer.writerow([])
                    force_writer.writerow(
                        ['Units are S.I. units (second, meters, Newtons, ...)'])
                    force_writer.writerow(['Angles are in degrees.'])
                    force_writer.writerow([])
                    force_writer.writerow(['endheader'])

                    force_writer.writerow(['time', '1_Force_X', '1_Force_Y', '1_Force_Z', '1_Moment_X', '1_Moment_Y', '1_Moment_Z', '1_COP_X', '1_COP_Y',
                                           '1_COP_Z', '2_Force_X', '2_Force_Y', '2_Force_Z', '2_Moment_X', '2_Moment_Y', '2_Moment_Z', '2_COP_X', '2_COP_Y', '2_COP_Z'])

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
                    VALUES = ["{0:.4f}".format(
                        1.00/float(FORCE_FREQUENCY)*(i-25.00))]
                    for x in range(1, 3):
                        VALUES.append(
                            "{0:.3f}".format(- float(row[(x-1)*3+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED])))
                        VALUES.append("{0:.3f}".format(
                            float(row[(x-1)*3+2+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED])))
                        VALUES.append("{0:.3f}".format(
                            float(row[(x-1)*3+1+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED])))

                    # VALUES.append("{0:.3f}".format(- float(row[6+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) + 370.0)) # X_opensim = -X_QTM
                    # VALUES.append("{0:.3f}".format(float(row[8+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) - 398.0)) # Y_opensim = Z_QTM
                    # VALUES.append("{0:.3f}".format(float(row[7+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) - 972.0)) # Z_opensim = Y_QTM
                    # X_opensim = -X_QTM
                    VALUES.append(
                        "{0:.3f}".format(- float(row[6+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED])))
                    VALUES.append("{0:.3f}".format(
                        float(row[8+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED])))  # Y_opensim = Z_QTM
                    VALUES.append("{0:.3f}".format(
                        float(row[7+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED])))  # Z_opensim = Y_QTM
                    # now the other file
                    for x in range(1, 3):
                        VALUES.append(
                            "{0:.3f}".format(- float(row_right[(x-1)*3+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED])))
                        VALUES.append("{0:.3f}".format(
                            float(row_right[(x-1)*3+2+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED])))
                        VALUES.append("{0:.3f}".format(
                            float(row_right[(x-1)*3+1+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED])))

                    # VALUES.append("{0:.3f}".format(- float(row_right[6+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) + 370.0)) # X_opensim = -X_QTM
                    # VALUES.append("{0:.3f}".format(float(row_right[8+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) - 398.0)) # Y_opensim = Z_QTM
                    # VALUES.append("{0:.3f}".format(float(row_right[7+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED]) - 972.0)) # Z_opensim = Y_QTM
                    # X_opensim = -X_QTM
                    VALUES.append(
                        "{0:.3f}".format(- float(row_right[6+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED])))
                    VALUES.append("{0:.3f}".format(
                        float(row_right[8+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED])))  # Y_opensim = Z_QTM
                    VALUES.append("{0:.3f}".format(
                        float(row_right[7+FORCE_EXTRA_COLUMNS_TO_BE_REMOVED])))  # Z_opensim = Y_QTM
                    force_writer.writerow(VALUES)
                i = i+1
            except StopIteration:
                break

        template = scale_config_template_file.read()
        content = template.replace("<mass></mass>", "<mass>"+WEIGHT+"</mass>").replace(
            "<marker_file></marker_file>", "<marker_file>"+experiment+'.trc'+"</marker_file>")
        scale_config_file.write(content)

        template = ik_config_template_file.read()
        content = template.replace("<marker_file></marker_file>", "<marker_file>"+experiment+'.trc'+"</marker_file>").replace(
            "<output_motion_file></output_motion_file>", "<output_motion_file>"+experiment+'.mot'+"</output_motion_file>").replace("STOP_TIME", STOP_TIME)
        ik_config_file.write(content)

        template = id_config_template_file.read()
        
        content = template.replace("<coordinates_file></coordinates_file>", "<coordinates_file>"+experiment+'.mot'+"</coordinates_file>").replace("<output_gen_force_file></output_gen_force_file>", "<output_gen_force_file>"+experiment+'_inverse_dynamics.sto' + "</output_gen_force_file>").replace("<output_body_forces_file></output_body_forces_file>", "<output_body_forces_file>"+experiment+'_body_forces_at_joints.sto'+"</output_body_forces_file>").replace("STOP_TIME", STOP_TIME)
        
        id_config_file.write(content)

        template = external_forces_template_file.read()
        content = template.replace(
            "<datafile></datafile>", "<datafile>"+experiment+'_ext_forces.mot'+"</datafile>")
        external_forces_file.write(content)

        template = analyze_config_template_file.read()
        content = template.replace("<coordinates_file></coordinates_file>", "<coordinates_file>" +
                                   experiment+'.mot'+"</coordinates_file>").replace("STOP_TIME", STOP_TIME)
        analyze_config_file.write(content)

    print(experiment)
    print("complete preprocessing.")

    experiment_dir = experiment.split(' ')[0]+'_'+experiment.split(' ')[1]+'\\'
    
    os.system('mkdir .\\data\\'+athlete+'\\'+experiment_dir+"\n")
    os.system('move .\\data\\'+athlete+'\\*.xml '+'.\\data\\'+athlete+'\\'+experiment_dir+"\n")
    os.system('move .\\data\\'+athlete+'\\*.osim '+'.\\data\\'+athlete+'\\'+experiment_dir+"\n")
    # _ = input('Press Enter to start next file')

    os.system('move .\\data\\'+athlete+'\\*.trc '+'.\\\n')
    os.system('move .\\data\\'+athlete+'\\*.mot '+'.\\\n')
    os.system('opensim-cmd run-tool .\\data\\'+athlete+'\\'+experiment_dir+'1_scale_config.xml\n')
    # _ = input('Press Enter to start next file')

    os.system('move .\\*.trc .\\data\\'+athlete+'\\'+experiment_dir+'\n')
    os.system('opensim-cmd run-tool .\\data\\'+athlete+'\\'+experiment_dir+'2_ik_config.xml\n')
    # _ = input('Press Enter to start next file')

    os.system('move .\\*.mot .\\data\\'+athlete+'\\'+experiment_dir+'\n')
    os.system('opensim-cmd run-tool .\\data\\'+athlete+'\\'+experiment_dir+'3_id_config.xml\n')
    # _ = input('Press Enter to start next file')

    os.system('move .\\*.sto .\\data\\'+athlete+'\\'+experiment_dir+'\n')
    os.system('move .\\*.osim .\\data\\'+athlete+'\\'+experiment_dir+'\n')
    os.system('opensim-cmd run-tool .\\data\\'+athlete+'\\'+experiment_dir+'4_analyze_config.xml\n')
    # _ = input('Press Enter to start next file')

    os.system('move .\\*.log '+'.\\data\\'+athlete+'\\'+experiment_dir+"\n")
    os.system('move '+'.\\data\\'+athlete+'\\'+experiment_dir+"gait_with_arms.osim .\\")
    
    # _ = input('Press Enter to start next file')

    exp=experiment.split('_')[1].split()[0]+'_'+experiment.split('_')[1].split()[1] # 'jump_j500 2' -> 'j500_2

    os.system('ren '+'.\\data\\'+athlete+'\\'+experiment_dir+'*dynamics.sto '+athlete+'_'+measurement_date+'_'+exp+'_ID.sto\n')
    os.system('ren '+'.\\data\\'+athlete+'\\'+experiment_dir+'*acc_global.sto '+athlete+'_'+measurement_date+'_'+exp+'_acc.sto\n')
    os.system('ren '+'.\\data\\'+athlete+'\\'+experiment_dir+'*pos_global.sto '+athlete+'_'+measurement_date+'_'+exp+'_pos.sto\n')
    os.system('ren '+'.\\data\\'+athlete+'\\'+experiment_dir+'*vel_global.sto '+athlete+'_'+measurement_date+'_'+exp+'_vel.sto\n')
    os.system('ren '+'.\\data\\'+athlete+'\\'+experiment_dir+'*Loads.sto '+athlete+'_'+measurement_date+'_'+exp+'_RL.sto\n')
    os.system('ren '+'.\\data\\'+athlete+'\\'+experiment_dir+'*joints.sto '+athlete+'_'+measurement_date+'_'+exp+'_BFJ.sto\n')
    os.system('ren '+'.\\data\\'+athlete+'\\'+experiment_dir+'*.trc '+athlete+'_'+measurement_date+'_'+exp+'.trc\n')
    os.system('ren '+'.\\data\\'+athlete+'\\'+experiment_dir+'*forces.mot '+athlete+'_'+measurement_date+'_'+exp+'_EF.mot\n')
    os.system('ren '+'.\\data\\'+athlete+'\\'+experiment_dir+'*'+experiment.split('_')[1].split()[1]+'.mot '+athlete+'_'+measurement_date+'_'+exp+'.mot\n')

    os.system('mkdir .\\data\\'+athlete+'\\'+experiment_dir+"temp\n")

    os.system('move .\\data\\'+athlete+'\\'+experiment_dir+athlete+'*.* .\\data\\'+athlete+'\\'+experiment_dir+'temp\\\n')

    os.system('mkdir .\\data\\'+athlete+'\\'+experiment_dir+"option\n")
    
    os.system('move .\\data\\'+athlete+'\\'+experiment_dir+'*.* .\\data\\'+athlete+'\\'+experiment_dir+'option\\\n')
    os.system('move .\\data\\'+athlete+'\\'+experiment_dir+'temp\\*.* .\\data\\'+athlete+'\\'+experiment_dir+'\n')
    os.system('rmdir .\\data\\'+athlete+'\\'+experiment_dir+'temp\n')
    os.system('rmdir /S /Q .\\data\\'+athlete+'\\'+experiment_dir+'option\n')

    # _ = input('Press Enter to start next file')