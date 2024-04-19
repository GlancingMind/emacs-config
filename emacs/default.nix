{
  buildEnv,
  emacsWithPackagesFromUsePackage,
  emacs29-pgtk,
  emacs-all-the-icons-fonts,
}:

buildEnv {
  name = "emacs-env";
  paths = [
    (emacsWithPackagesFromUsePackage {
      package = emacs29-pgtk;
      config = ./setup.el;
      defaultInitFile = true;
    })
    emacs-all-the-icons-fonts
  ];
}
