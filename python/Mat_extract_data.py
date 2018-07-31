
import xlrd
import glob
import re
import csv
import sys


#Command line needs to be cd in current directory of all the xls files
#Py file also needs to be stored there
#python Mat_extract_data.py 'Diff26_c2_data.csv'


#############################

# the star means any character

channels = ['*C0.xls', '*C1.xls','*C2.xls']
chanId = ["0","1","2"]
n = 0

maxInt = [28, 29, 30] #Columns
meanInt = [33, 34, 35]

# list
output = []
# this the column header
heading = ['channel', 'day', 'image', 'cell.id','cell.area', 'spot.id', 'x', 'y', 'area', 'max.int', 'mean.int']
output.append([heading])


for channel in channels:

    files = glob.glob(channel)

    for image in files:

        day = re.findall(r'_D(\d+)_', image)[0]
        #print file
        pos = re.findall(r'Pos(\d+).', image)[0]
        #print "Pos", pos
            
        workbook = xlrd.open_workbook(image)
        byroi_sheet = workbook.sheet_by_name('sort by ROI')

        byroi_sheet.cell(0, 0).value

        # find all rows with 'ROI Id' in the first column starting from row number 30
        row_id = 5
        while row_id < byroi_sheet.nrows:
            row = byroi_sheet.row_values(row_id)
            # prefix u for unicode
            if row[0] == u"ROI Id":
                # print 'Found ROI Id in row %d column 0' % row_id
                # switch to the next row
                row_id += 1
                extracted_roi_id = int(byroi_sheet.row_values(row_id)[0])
                extracted_roi_area = int(byroi_sheet.row_values(row_id)[2])
                #print 'The ROID Id is %d' % extracted_roi_id
                #print 'ROID', extracted_roi_id

                # ignore the colum title
                row_id += 2
                if not row_id < byroi_sheet.nrows:
                    values = [chanId[n], day, pos, extracted_roi_id, extracted_roi_area, 0, 0, 0, 0, 0, 0]
                    output.append([values])
                    break
                            
                row = byroi_sheet.row_values(row_id)

                if row[0] == u'--------':
                    #This assigns zero values when a cell does not contain any spot
                    values = [chanId[n], day, pos, extracted_roi_id, extracted_roi_area, 0, 0, 0, 0, 0, 0]
                    output.append([values])

                row = byroi_sheet.row_values(row_id)
                # extract the columns
                while row[0] != u'--------':
                    # this print correspond to the previous print for header titles
                    # print the two columns 6 and 33
                    values = [chanId[n], day, pos, extracted_roi_id, extracted_roi_area, float(row[1 - 1]), float(row[3 - 1]), float(row[4 - 1]), float(row[6 - 1]), float(row[maxInt[n] - 1]), float(row[meanInt[n] - 1])]
                    output.append([values])
                    row_id += 1
                    if not row_id < byroi_sheet.nrows:
                        break
                    row = byroi_sheet.row_values(row_id)

            row_id += 1

        #print output

    n +=1


# write it
filename = sys.argv[1]
with open(filename, 'w') as csvfile:
    writer = csv.writer(csvfile)
    [writer.writerow(r[0]) for r in output]

