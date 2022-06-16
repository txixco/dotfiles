from wintools import WinTools

months = [ "Enero", "Febrero", "Marzo", "Abril", 
           "Mayo", "Junio", "Julio", "Agosto", 
           "Septiembre", "Octubre", "Noviembre", "Diciembre"]
           
day = system.exec_command("date +'%-d'")
month = months[int(system.exec_command("date +'%-m'")) - 1]
url = f"http://www.spurgeon.com.mx/chequera/{day}{month}.html"

WinTools(window.mediator).open_url(url, "meditaciones diarias")