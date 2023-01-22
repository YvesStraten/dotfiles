local whichkey_setup, whichkey = pcall(require, "whichkey")
if not whichkey_setup then
	return
end

whichkey.setup()
