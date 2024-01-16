local notify = require("notify")
vim.notify = notify
require("notify").setup({
	render = "compact",
})

vim.notify("Config loaded")
