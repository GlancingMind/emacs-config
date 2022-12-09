{
  emacsWithPackagesFromUsePackage,
  emacsPgtkNativeComp,
}:

emacsWithPackagesFromUsePackage {
  package = emacsPgtkNativeComp;
  config = ./tangeled/init.el;
  defaultInitFile = true;
  alwaysTangle = true;
}
