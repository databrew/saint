import ezsheets
import os
import shutil
os.chdir('../credentials')

s = ezsheets.Spreadsheet('https://docs.google.com/spreadsheets/d/1buZrczeSuU_B9Nv7aLDia8wUnsa4sbNXiv6oALm2Hr8/edit#gid=0')
s.downloadAsExcel()

## Convert to xml
os.system('xls2xform saintperu.xlsx saintperu.xml ')

# Move
shutil.move('saintperu.xlsx', '../forms/saintperu.xlsx')
shutil.move('saintperu.xml',  '../forms/saintperu.xml')
# shutil.move('itemsets.csv', '../forms/itemsets.csv')

print('Done. Docs in forms/.')
