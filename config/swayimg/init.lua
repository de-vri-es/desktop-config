swayimg.enable_exif_orientation(true)
swayimg.enable_decoration(false)
swayimg.enable_overlay(false)

swayimg.imagelist.enable_adjacent(true)
swayimg.imagelist.set_order("alpha")

swayimg.viewer.set_default_scale("optimal")
swayimg.viewer.set_text("topleft", {})
swayimg.viewer.set_text("topright", {})
swayimg.viewer.set_text("bottomleft", {})
swayimg.viewer.set_text("bottomright", {})

swayimg.viewer.bind_reset()

swayimg.viewer.on_key("Escape", function()
	swayimg.exit()
end)
swayimg.viewer.on_key("q", function()
	swayimg.exit()
end)

swayimg.viewer.on_key("Left", function()
	swayimg.viewer.switch_image("prev")
end)

swayimg.viewer.on_key("Right", function()
	swayimg.viewer.switch_image("next")
end)

swayimg.viewer.on_mouse("MouseSide", function()
	swayimg.viewer.switch_image("prev")
end)

swayimg.viewer.on_mouse("MouseExtra", function()
	swayimg.viewer.switch_image("next")
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

swayimg.viewer.on_image_change(function()
	reset_scale(swayimg.viewer)
end)

swayimg.on_window_resize(function()
	if not _G.manually_resized then
		reset_scale(swayimg.viewer)
	end
end)

function reset_scale(viewer, factor)
	_G.manually_resized = false
	swayimg.viewer.set_fix_scale("optimal")
end
function adjust_scale(viewer, factor)
	_G.manually_resized = true
	center = swayimg.get_mouse_pos()
	scale = swayimg.viewer.get_scale() * factor
	scale = math.max(scale, 0.01)
	viewer.set_abs_scale(scale, center.x, center.y)
end

swayimg.viewer.on_mouse("ScrollUp", function()
	adjust_scale(swayimg.viewer, 1 * 1.04)
end)
swayimg.viewer.on_mouse("ScrollDown", function()
	adjust_scale(swayimg.viewer, 1 / 1.04)
end)

swayimg.viewer.on_key("Backspace", function()
	reset_scale(swayimg.viewer)
end)
swayimg.viewer.on_key("Equal", function()
	adjust_scale(swayimg.viewer, 1 * 1.04)
end)
swayimg.viewer.on_key("Plus", function()
	adjust_scale(swayimg.viewer, 1 * 1.04)
end)
swayimg.viewer.on_key("Shift+Plus", function()
	adjust_scale(swayimg.viewer, 1 * 1.04)
end)
swayimg.viewer.on_key("Minus", function()
	adjust_scale(swayimg.viewer, 1 / 1.04)
end)

function dirname(str)
    return str:match("(.*/)")
end

swayimg.viewer.on_file_drop(function(paths)
	swayimg.imagelist.add(dirname(paths[1]))
	swayimg.viewer.open(paths[1])
end)
