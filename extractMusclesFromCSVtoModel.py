from xml.dom import minidom
import csv

xmldoc = minidom.parse('subject01_RRA_adjusted_Run_50002_cycle1_v191_with_corrected_mass_probed_0.osim')


parameters = [
'max_isometric_force', 
'optimal_fiber_length', 
'tendon_slack_length', 
'pennation_angle', 
'activation_time_constant', 
'deactivation_time_constant', 
'Vmax', 
'Vmax0', 
'FmaxTendonStrain', 
'FmaxMuscleStrain', 
'KshapeActive', 
'KshapePassive', 
'damping', 
'Af', 
'Flen']

with open('subject01_scaled_v191_Muscles_sorted_0.csv', newline='') as csvfile:
	csvr = csv.reader(csvfile, delimiter=',', quotechar='"')
	for row in csvr:
		if row[0] != 'muscle':
			itemlist = xmldoc.getElementsByTagName('Thelen2003Muscle')
			for s in itemlist:
				mucle_name = s.attributes['name'].value
				if mucle_name == row[0]:
					for i in range (0, 2):
						s.getElementsByTagName(parameters[i])[0].firstChild.nodeValue = row[i+1]
						#print(parameters[i])
						#print(row[i+1])

	
#itemlist = xmldoc.getElementsByTagName('Thelen2003Muscle')
#for s in itemlist:
#	res = s.attributes['name'].value
#	for param in parameters:
#		res = res + ',' + s.getElementsByTagName(param)[0].firstChild.nodeValue 
#	print(res)
#	f.write(res+"\n")
	
csvfile.close()

file_handle = open("subject01_RRA_adjusted_Run_50002_cycle1_v191_with_corrected_mass_probed_0_muscles_changed_0.osim","w")
xmldoc.writexml(file_handle)
file_handle.close()
