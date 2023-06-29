local dap = require("dap")
dap.adapters.lldb = {
  type = "executable",
  command = "~/.nix-profile/bin/lldb-vscode",
  name = "lldb",
}

require("dapui").setup()
require("neodev").setup({
  library = {
    plugins = {
      "nvim-dap-ui",
      types = true
    },
  }
})
