from autokey.scripting import System, Window, GtkDialog

# Constants

BROWSER = "surf -k {url}"
#BROWSER = "firefox --new-window {url}"
#BROWSER = "uzbl -u {url}"

# Class
class WinTools:
    """Class for the tools"""
    
    def __init__(self, mediator):
        self.system = System()
        self.dialog = GtkDialog()
        self.window = Window(mediator)

    def center_window(self, title: str, width_percent: int = 50, shift_percent: int = 0) -> None:
        """Center a window in the screen"""
        
        # Calculate the geometry
        command = "xrandr | grep '*' | awk '{print $1}'"
        (scrwidth, scrheight) = map(int, (self.system.exec_command(command)).split("x"))
        width = scrwidth * width_percent / 100
        height = scrheight * 0.90

        shift = scrwidth * shift_percent / 100
        x = (scrwidth / 2) - (width / 2) - shift
        y = (scrheight / 2) - (height / 2)
        
        # Wait and resize/move
        self.window.wait_for_exist(title, 10)
        self.window.resize_move(title, 10, 10, width, height)

    def open_url(self, url: str, title: str, width_percent: int = 50) -> None:
        """Open an url with the configured browser"""

        self.system.exec_command(BROWSER.format(url=url), False)
        self.center_window(title, width_percent)
