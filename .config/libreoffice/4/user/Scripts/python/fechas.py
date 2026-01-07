from uno import getComponentContext, createUnoStruct

CTX = getComponentContext()
SM = CTX.ServiceManager

def current_LO_date():
    """
    Returns the current date in LibreOffice Calc date format.
    LibreOffice Calc uses a date system where 30th December 1899 is day 0.
    """
    from datetime import date

    base_date = date(1899, 12, 30)  # LibreOffice date base
    today = date.today()
    lo_date = (today - base_date).days

    return float(lo_date)

def create_instance(name, with_context=False):
    if with_context:
        instance = SM.createInstanceWithContext(name, CTX)
    else:
        instance = SM.createInstance(name)

    return instance

def insert_current_date():
    """
    Inserts the current date into the selected cell of a LibreOffice Calc sheet.
    """
    from datetime import date

    desktop = create_instance('com.sun.star.frame.Desktop', True)
    doc = desktop.getCurrentComponent()
    sel = doc.CurrentController.getSelection()
    cell = sel.getCellByPosition(0, 0)  # Top-left cell of the selection

    cell.Value = current_LO_date()

    return

def insert_row():
    """
    Inserts a new row below the currently selected row in a LibreOffice Calc sheet.
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
    sheet.Rows.insertByIndex(old_row + 1, 1)

    # Copy formulas from the old row to the new row
    for col in range(sheet.Columns.getCount()):
        source = sheet.getCellByPosition(col, old_row)
        target = sheet.getCellByPosition(col, old_row + 1)

        if source.Formula.startswith("="):  # Formula
            target.Formula = source.Formula

    # Move the cursor to the first cell of the newly inserted row
    dispatcher = create_instance('com.sun.star.frame.DispatchHelper', True)
    args = (
        createUnoStruct('com.sun.star.beans.PropertyValue'),
    )

    args[0].Name = 'ToPoint'
    args[0].Value = f"$A${old_row + 2}"

    dispatcher.executeDispatch(frame, '.uno:GoToCell', '', 0, args)

    return

g_exportedScripts = (insert_current_date, insert_row)