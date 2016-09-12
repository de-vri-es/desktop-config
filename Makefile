PREFIX     ?= "${HOME}"
CONFIG_DIR ?= "${PREFIX}/.config"

.PHONY: install

recompile: install

restart: install

install: | ${CONFIG_DIR}
	cp -a config/*        ${CONFIG_DIR}/
	cp -a hgaiser/qtile   ${CONFIG_DIR}/
	cp -a home/xinitrc    ${PREFIX}/.xinitrc
	cp -a home/gtkrc-2.0  ${PREFIX}/.gtkrc-2.0

${CONFIG_DIR}:
	-mkdir -p "${CONFIG_DIR}" >/dev/null 2>&1
