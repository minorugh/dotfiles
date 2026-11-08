if get_window_class() == "Gnome-terminal-server" then
  local p = io.popen("xrandr --listmonitors")
  local out = p:read("*a")
  p:close()
  if string.find(out, "DP-1-2", 1, true) then
    local maximized = get_window_is_maximized()
    if maximized then
      unmaximize()
    end
    local x, y, w, h = get_window_geometry()
    set_window_position(1920 + (1920 - w) / 2, (1200 - h) / 2)
    if maximized then
      maximize()
    end
  end
end
