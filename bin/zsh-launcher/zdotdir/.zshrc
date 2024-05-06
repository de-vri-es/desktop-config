ZDOTDIR="${1-$HOME}"
if [[ -f "$ZDOTDIR/.zshrc" ]]; then
	source "$ZDOTDIR/.zshrc"
fi

preexec() {
	echo "$!"
	eval "$1 &|";
	exit 0
}
