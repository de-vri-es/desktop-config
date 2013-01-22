PREFIX     ?= "${HOME}"
CONFIG_DIR ?= "${PREFIX}/.config"
XMONAD_DIR ?= "${PREFIX}/.xmonad"

.PHONY: install

recompile: install
	xmonad --recompile
	xmonad --restart

restart: install
	xmonad --restart

install: | ${CONFIG_DIR} ${XMONAD_DIR}
	cp -a config/*        ${CONFIG_DIR}/
	cp -a home/xinitrc    ${PREFIX}/.xinitrc
	cp -a home/gtkrc-2.0  ${PREFIX}/.gtkrc-2.0
	cp -a xmonad/*        ${XMONAD_DIR}/
	
${CONFIG_DIR}:
	-mkdir -p "${CONFIG_DIR}" >/dev/null 2>&1
	
${XMONAD_DIR}:
	-mkdir -p "${XMONAD_DIR}" >/dev/null 2>&1
