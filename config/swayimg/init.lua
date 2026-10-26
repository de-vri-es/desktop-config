function SHELL_ESCAPE(s)
	return "'" .. s:gsub("'", "'\\''") .. "'"
end

swayimg.exif_orientation = true
swayimg.decoration = false
swayimg.overlay = false

swayimg.imagelist.adjacent = true
swayimg.imagelist.order = "alpha"

swayimg.viewer.default_scale = "optimal"
swayimg.viewer.text = {
	topleft = {},
	topright = {},
	bottomleft = {},
	bottomright = {},
}

swayimg.viewer.bind_reset()

swayimg.viewer.on_key("Escape", function()
	swayimg.exit()
end)
swayimg.viewer.on_key("q", function()
	swayimg.exit()
end)
swayimg.viewer.on_key("Return", function()
	swayimg.mode = "gallery"
end)
swayimg.viewer.on_key("Shift+Return", function()
	os.execute("swayimg " .. SHELL_ESCAPE(swayimg.viewer.get_image().path) .. " & disown")
end)

swayimg.gallery.on_key("Escape", function()
	swayimg.exit()
end)
swayimg.gallery.on_key("q", function()
	swayimg.exit()
end)
swayimg.gallery.on_key("Return", function()
	swayimg.mode = "viewer"
end)
swayimg.gallery.on_key("Shift+Return", function()
	os.execute("swayimg " .. SHELL_ESCAPE(swayimg.gallery.get_image().path) .. " & disown")
end)

swayimg.viewer.on_key("Left", function()
	swayimg.viewer.open("prev")
end)

swayimg.viewer.on_key("Right", function()
	swayimg.viewer.open("next")
end)

swayimg.viewer.on_mouse("MouseLeft", function()
end)

swayimg.viewer.on_mouse("MouseRight", function()
end)

swayimg.viewer.on_mouse("MouseMiddle", function()
end)

swayimg.viewer.on_mouse("MouseSide", function()
	swayimg.viewer.open("prev")
end)

swayimg.viewer.on_mouse("MouseExtra", function()
	swayimg.viewer.open("next")
end)

swayimg.viewer.on_key("R", function()
	swayimg.viewer.rotate(270)
end)
swayimg.viewer.on_key("Shift+R", function()
	swayimg.viewer.rotate(90)
end)

swayimg.viewer.on_key("V", function()
	swayimg.viewer.flip_vertical()
end)
swayimg.viewer.on_key("H", function()
	swayimg.viewer.flip_horizontal()
end)


_G.manually_resized = false
_G.scale = 1.0;
_G.center_x = nil;
_G.center_y = nil;

swayimg.viewer.on_image_change(function()
	RESET_SCALE()
end)

swayimg.on_window_resize(function()
	if not _G.manually_resized then
		RESET_SCALE()
	else
		local window_size = swayimg.get_window_size();
		local center_x = math.floor(_G.center_x * window_size.width);
		local center_y = math.floor(_G.center_y * window_size.height);
		swayimg.viewer.set_abs_scale(_G.scale, center_x, center_y)
	end
end)

function RESET_SCALE()
	_G.manually_resized = false
	swayimg.viewer.set_fix_scale("optimal")
end
function ADJUST_SCALE(factor)
	local center = swayimg.get_mouse_pos()
	local scale = swayimg.viewer.scale * factor
	local window_size = swayimg.get_window_size();
	scale = math.max(scale, 0.01)
	_G.scale = scale;
	_G.center_x = (center.x + 0.0) / window_size.width;
	_G.center_y = (center.y + 0.0) / window_size.height;
	_G.manually_resized = true
	swayimg.viewer.set_abs_scale(scale, center.x, center.y)
end

swayimg.viewer.on_mouse("ScrollUp", function()
	ADJUST_SCALE(1 * 1.04)
end)
swayimg.viewer.on_mouse("ScrollDown", function()
	ADJUST_SCALE(1 / 1.04)
end)

swayimg.viewer.on_key("Backspace", function()
	RESET_SCALE()
end)
swayimg.viewer.on_key("Equal", function()
	ADJUST_SCALE(1 * 1.04)
end)
swayimg.viewer.on_key("Plus", function()
	ADJUST_SCALE(1 * 1.04)
end)
swayimg.viewer.on_key("Shift+Plus", function()
	ADJUST_SCALE(1 * 1.04)
end)
swayimg.viewer.on_key("Minus", function()
	ADJUST_SCALE(1 / 1.04)
end)

-- function dirname(str)
--     return str:match("(.*/)")
-- end

-- swayimg.viewer.on_file_drop(function(paths)
-- 	swayimg.imagelist.add(dirname(paths[1]))
-- 	swayimg.viewer.open(paths[1])
-- end)
