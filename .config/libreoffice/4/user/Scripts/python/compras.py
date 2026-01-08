from uno import getComponentContext, createUnoStruct

CTX = getComponentContext()
SM = CTX.ServiceManager

def create_instance(name, with_context=False):
    if with_context:
        instance = SM.createInstanceWithContext(name, CTX)
    else:
        instance = SM.createInstance(name)

    return instance

def _current_LO_date():
    """
    Returns the current date in LibreOffice Calc date format.
    LibreOffice Calc uses a date system where 30th December 1899 is day 0.
    """
    from datetime import date

    base_date = date(1899, 12, 30)  # LibreOffice date base
    today = date.today()
    lo_date = (today - base_date).days

    return float(lo_date)

def insert_row():
    """
    Inserts a new row below the currently selected one.
    """

    # Get the current document and selection
    desktop = create_instance('com.sun.star.frame.Desktop', True)
    doc = desktop.getCurrentComponent()
    controller = doc.CurrentController
    frame = controller.getFrame()
    sel = controller.getSelection()
    sheet = controller.getActiveSheet()

    # Insert a new row below the selected row
    old_row = sel.RangeAddress.StartRow
    new_row = old_row + 1
    sheet.Rows.insertByIndex(new_row, 1)

    # Enter default values, such as the date and formulas
    sheet.getCellByPosition(0, new_row).Value = _current_LO_date()
    sheet.getCellByPosition(4, new_row).Formula = f"=(C{new_row+1}-D{new_row+1})/2"
    sheet.getCellByPosition(6, new_row).Formula = f"=G{old_row+1}+F{new_row+1}-E{new_row+1}"

    # Move the cursor to the first non-date cell of the newly inserted row
    dispatcher = create_instance('com.sun.star.frame.DispatchHelper', True)
    args = (
        createUnoStruct('com.sun.star.beans.PropertyValue'),
    )

    args[0].Name = 'ToPoint'
    args[0].Value = f"$B${new_row + 1}"

    dispatcher.executeDispatch(frame, '.uno:GoToCell', '', 0, args)

    return

g_exportedScripts = (insert_row,)