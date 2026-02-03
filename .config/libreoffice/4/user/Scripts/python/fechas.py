from uno import getComponentContext

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

def insert_current_date():
    """
    Inserts the current date into the selected cell of a LibreOffice Calc sheet.
    """
    from datetime import date

    desktop = create_instance('com.sun.star.frame.Desktop', True)
    doc = desktop.getCurrentComponent()
    sel = doc.CurrentController.getSelection()
    cell = sel.getCellByPosition(0, 0)  # Top-left cell of the selection

    cell.Value = _current_LO_date()

    return

g_exportedScripts = (insert_current_date,)