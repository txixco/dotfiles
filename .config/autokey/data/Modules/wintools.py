from autokey.scripting import System, Window
from autokey.scripting.dialog_gtk import GtkDialog

# Constants

BROWSER = "qutebrowser --target window {url}"
#BROWSER = "surf -k {url}"
#BROWSER = "firefox --new-window {url}"
#BROWSER = "uzbl -u {url}"

# Class
class WinTools:
    """Class for the tools"""
    
    def __init__(self, mediator):
        self.system = System()
        self.dialog = GtkDialog()
        self.window = Window(mediator)

    def center_window(self, title: str = None, width_percent: int = 50, shift_percent: int = 0) -> None:
        """Center a window in the screen"""
        
        if not title:
            title = self.window.get_active_title()
                
        # Calculate the geometry
        command = "xrandr | grep '*' | awk '{print $1}'"
        (scr_width, scr_height) = map(int, (self.system.exec_command(command)).split("x"))
        width = int(scr_width * width_percent / 100)
        height = int(scr_height * 0.80)

        shift = scr_width * shift_percent / 100
        x = int((scr_width / 2) - (width / 2) - shift)
        y = int((scr_height / 2) - (height / 2))
        
        # Wait and resize/move
        self.window.wait_for_exist(title, 10)
        self.window.resize_move(title, x, y, width, height)

    def open_url(self, url: str, title: str, width_percent: int = 50) -> None:
        """Open an url with the configured browser"""

        self.system.exec_command(BROWSER.format(url=url), False)
        self.center_window(title, width_percent)
