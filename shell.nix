{
  mkShellNoCC,
  emacsPgtkNativeComp,
  emacs-all-the-icons-fonts
}:

mkShellNoCC {
  packages = [
    # put packages here
    emacs-all-the-icons-fonts
  ];

  shellHook = ''
  '';
}
