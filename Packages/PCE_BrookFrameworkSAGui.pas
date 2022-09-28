{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PCE_BrookFrameworkSAGui;

{$warn 5023 off : no warning about unused units}
interface

uses
  libsagui, SAGui, SAGuiBasedTypes, SAGuiGlobal, SAGuiClasses, SAGuiUtilities, 
  SAGuiInternalUtilities, SAGuiRessources, SAGuiConsts, SAGuiExceptions, 
  LibSAGuiAPI, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PCE_BrookFrameworkSAGui', @Register);
end.
