#!/usr/bin/env python

#####################
## Get Version details
######################
import commands
status, version = commands.getstatusoutput ("git describe --tags --long")
if not status: 
	print ("Version: " + version)
else: 
	print "git describe return bad status!"
	version = "NA"
	# Need to exit gracefully
	# Now it continues!!! 

####################
## build elm code 
####################
fileContent = \
'''
module Version where \n\
{-| This file is auto-generated by the Python script version.py. -} \n\
\n\
\n\
version : String \n\
'''

fileContent = fileContent + 'version = "' + version + '"\n\n'

#print (fileContent)

##################
## Create file after backing up previous one, if it exists
##################
fo = open ("Version.elm", "w+")
#print "Name of file opened ", fo.name 

fo.write (fileContent)
fo.close()