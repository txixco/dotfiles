from datetime import datetime

today = datetime.today()
keyboard.send_keys(f"{today:%I/%m/%Y %H:%M:%S}")