local orgmode = require("orgmode")
orgmode.setup({
	org_agenda_files = { "~/org/*" },
})

orgmode.setup_ts_grammar()
