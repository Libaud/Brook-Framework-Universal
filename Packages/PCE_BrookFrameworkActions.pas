{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PCE_BrookFrameworkActions;

{$warn 5023 off : no warning about unused units}
interface

uses
  BrookActions, BrookGActions, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PCE_BrookFrameworkActions', @Register);
end.
