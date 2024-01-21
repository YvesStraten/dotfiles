{inputs, ...}: final: prev: {
  vimPlugins =
    prev.vimPlugins
    // {
      vim-snippets = prev.vimUtils.buildVimPlugin {
        name = "vim-snippets";
        src = inputs.vim-snippets;
      };

      ouroboros = prev.vimUtils.buildVimPlugin {
        name = "ouroboros";
        src = inputs.ouroboros;
      };
    };
}
