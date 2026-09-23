{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.file.".vim/colors/flexoki_dark.vim".source = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/kepano/flexoki/8d723bac4a9ac46adfdf99d42155286977aac72a/vim/flexoki_dark.vim";
    sha256 = "0hgfcmmywwvh89vzrn5638wn4c19sl2w908jcf4vsvbay35wdl08";
  };
  home.file.".vim/colors/flexoki_light.vim".source = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/kepano/flexoki/8d723bac4a9ac46adfdf99d42155286977aac72a/vim/flexoki_light.vim";
    sha256 = "0k8f2lvi4h2cci2gis3cbc13i86b1lvlm8w8dw45v30iw0mz6qq3";
  };

  programs.vim = {
    plugins = with pkgs.vimPlugins; [
      vim-wakatime
    ];

    enable = true;
    extraConfig = ''
      set number
      set linebreak
      set showbreak=+++
      set textwidth=100
      set showmatch
      set visualbell
      set hlsearch
      set smartcase
      set ignorecase
      set incsearch
      set autoindent
      set shiftwidth=4
      set smartindent
      set smarttab
      set softtabstop=4
      set ruler
      set undolevels=1000
      set backspace=indent,eol,start
      set background=dark
      colorscheme flexoki_dark
    '';
  };
}
