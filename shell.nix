{
  mkShellNoCC,
  emacsPgtkNativeComp
}:

mkShellNoCC {
  packages = [
    # put packages here
    emacsPgtkNativeComp
  ];

  shellHook = ''
  '';
}
