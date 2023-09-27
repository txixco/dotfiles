import re

contents = clipboard.get_selection()
contents = re.sub('\s', '', contents)
clipboard.fill_clipboard(contents)