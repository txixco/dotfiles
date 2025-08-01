alias config="/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
alias wincfg="/usr/bin/git --git-dir=$HOME/wincfg/windotfiles/ --work-tree=$HOME"
alias ec="$EDITOR $HOME/.zshrc"
alias sc="source $HOME/.zshrc"
alias scrum="python3 -c 'import random; persons = [ \"Bo\", \"Chantal\", \"Raúl\", \"Anthony\" ]; random.shuffle(persons); print(\"\\n\".join(persons))'"
alias upgrade="sudo dnf upgrade -y"
alias install="sudo dnf install -y"
alias shutdown="sudo shutdown -h now"
