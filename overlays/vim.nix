{ inputs, ... }:
final: prev: {
  vimPlugins = prev.vimPlugins // {
    vim-snippets = prev.vimUtils.buildVimPlugin {
      name = "vim-snippets";
      dontBuild = true;
      src = inputs.vim-snippets;
    };

    ouroboros = prev.vimUtils.buildVimPlugin {
      name = "ouroboros";
      dontBuild = true;
      src = inputs.ouroboros;
    };

    tiny-code-action = prev.vimUtils.buildVimPlugin {
      name = "tiny-code-action";
      dontBuild = true;
      src = inputs.tiny-code-action;
    };
  };
}
