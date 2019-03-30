import datetime
from docxtpl import DocxTemplate
import os

def gendoc(CsvList, TerminalList, TerminalDifCsvList, CsvDifTerminalList, NaklNum, BlankNum, FileName, Mark, LicensePlate, Driver, Company, CreatedDate, StartWeight, UpdatedDate, EndWeight, ItemCount, ItemWeight, PackageWeight, ExcessWeight):
    if not os.path.exists("tmp/"):
        os.mkdir("tmp")
    doc = DocxTemplate("priv/templates/blank_template.docx")
    now = datetime.datetime.now()
    if int(ItemCount) != 0:
        srVal = float(ExcessWeight) / float(ItemCount)
    else:
        srVal = 0
    dVal = abs(srVal - 50)
    srVal = format(srVal, '.2f')
    dVal = format(dVal, '.2f')
    context = { 'terminal_dif_csv_list' : TerminalDifCsvList.decode('utf-8'), 'csv_dif_terminal_list' : CsvDifTerminalList.decode('utf-8'),'terminal_list' : TerminalList.decode('utf-8'), 'csv_list' : CsvList.decode('utf-8'), 'nakl_num' : NaklNum.decode('utf-8'), 'blank_num' : str(2*BlankNum).decode('utf-8'), 'blank_num2' : str(2*BlankNum+1).decode('utf-8'), 'date' : now.strftime("%d-%m-%Y"), 'mark' : Mark.decode('utf-8'), 'license_plate' : LicensePlate.decode('utf-8'), 'company' : Company.decode('utf-8'), 'driver' : Driver.decode('utf-8'),  'date_start' : CreatedDate.decode('utf-8'), 'weight_start' : StartWeight.decode('utf-8'), 'date_end' : UpdatedDate.decode('utf-8'),  'weight_end' : EndWeight.decode('utf-8'),  'item_weight' : str(ItemWeight).decode('utf-8'),  'package_weight' : str(PackageWeight).decode('utf-8'),  'excess_weight' : ExcessWeight.decode('utf-8'),  'excess_one' : str(srVal), 'excess_val' : str(dVal) }
    doc.render(context)
    doc.save(FileName)

