#!/usr/bin/env python

# Inspired by 
# https://gitfu.wordpress.com/2008/05/25/git-describe-great-another-way-to-refer-to-commits/
# http://gitready.com/beginner/2009/02/03/tagging.html

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
module Version exposing (..) -- where \n\
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
fo = open ("Version.elm", "rw+")
#print "Name of file opened ", fo.name 

######
# Is it really necessary to update? 
######
previous = fo.read() 
#print (previous)

if previous.find (version) != -1:
	print ("Version.elm already up-to-date!")
else: 
	print ("Version.elm updated with " + version)
	fo.write (fileContent)

fo.close()