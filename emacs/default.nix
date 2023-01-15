{
  buildEnv,
  emacsWithPackagesFromUsePackage,
  emacsPgtk,
  emacs-all-the-icons-fonts,
}:

buildEnv {
  name = "emacs-env";
  paths = [
    (emacsWithPackagesFromUsePackage {
      package = emacsPgtk;
      config = ./setup.el;
      defaultInitFile = true;
    })
    emacs-all-the-icons-fonts
  ];
}
