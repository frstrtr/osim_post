from xml.dom import minidom

xmldoc = minidom.parse('subject01_scaled_v191.osim')
f = open('subject01_scaled_v191_Muscles.csv', 'w')

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

header = "muscle"
for param in parameters:
	header = header + ", " + param
print(header)
f.write(header+"\n")

	
itemlist = xmldoc.getElementsByTagName('Thelen2003Muscle')
for s in itemlist:
	res = s.attributes['name'].value
	for param in parameters:
		res = res + ',' + s.getElementsByTagName(param)[0].firstChild.nodeValue 
	print(res)
	f.write(res+"\n")
	
f.close()
	
#import xml.etree.ElementTree

#e = xml.etree.ElementTree.parse('subject01_scaled_v191.osim').getroot()

#for muscle in e.findall('Thelen2003Muscle'):
#    print('found:')
#    print(muscle.get('name'))