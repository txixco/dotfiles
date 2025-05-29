from datetime import datetime

today = datetime.today()
keyboard.send_keys(f"{today:%d/%m/%Y}")