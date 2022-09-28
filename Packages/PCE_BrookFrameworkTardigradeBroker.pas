{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PCE_BrookFrameworkTardigradeBroker;

{$warn 5023 off : no warning about unused units}
interface

uses
  BrookTardigradeBroker, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PCE_BrookFrameworkTardigradeBroker', @Register);
end.
