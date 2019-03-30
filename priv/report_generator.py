import xlsxwriter

from xlsxwriter.workbook import Workbook
from xlsxwriter.worksheet import Worksheet
from xlsxwriter.worksheet import convert_cell_args

def excel_string_width(str):
    string_width = len(str)

    if string_width == 0:
        return 0
    else:
        return string_width * 1.1

class MyWorksheet(Worksheet):

    @convert_cell_args
    def write_string(self, row, col, string, cell_format=None):
        if self._check_dimensions(row, col):
            return -1

        min_width = 0

        string_width = excel_string_width(string)
        if string_width > min_width:
            max_width = self.max_column_widths.get(col, min_width)
            if string_width > max_width:
                self.max_column_widths[col] = string_width

        return super(MyWorksheet, self).write_string(row, col, string,
                                                     cell_format)

class MyWorkbook(Workbook):
    def add_worksheet(self, name=None):
        worksheet = super(MyWorkbook, self).add_worksheet(name, MyWorksheet)
        worksheet.max_column_widths = {}
        return worksheet
    def close(self):
        for worksheet in self.worksheets():
            for column, width in worksheet.max_column_widths.items():
                worksheet.set_column(column, column, width)
        return super(MyWorkbook, self).close()

def gen_report(FileName, ColumnTitle, Transfers):
    workbook = MyWorkbook(FileName)
    worksheet = workbook.add_worksheet()
    worksheet.freeze_panes(1, 0)
    title_format = workbook.add_format({'bold': True})
    bottom_border = workbook.add_format()
    bottom_border.set_bottom(2)
    columnCnt = len(ColumnTitle)
    worksheet.conditional_format(0,0,0,columnCnt-1, {'type':'no_errors', 'format':bottom_border})
    for col_ind in range(0, len(ColumnTitle)):
        worksheet.write(0, col_ind, ColumnTitle[col_ind].decode('utf-8'), title_format)
    cur_row = 1
    for t_ind in range(0, len(Transfers)):
        for r_ind in range(0, len(Transfers[t_ind])):
            for c_ind in range(0, len(Transfers[t_ind][r_ind])):
                worksheet.write(cur_row, c_ind, Transfers[t_ind][r_ind][c_ind].decode('utf-8'))
            cur_row = cur_row + 1
        worksheet.conditional_format(cur_row - 3,0,cur_row - 3,columnCnt-2, {'type':'no_errors', 'format':bottom_border})
        worksheet.conditional_format(cur_row - 1,0,cur_row - 1,columnCnt-1, {'type':'no_errors', 'format':bottom_border})

    workbook.close()