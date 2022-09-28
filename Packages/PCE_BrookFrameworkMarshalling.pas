{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PCE_BrookFrameworkMarshalling;

{$warn 5023 off : no warning about unused units}
interface

uses
  Marshalling, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PCE_BrookFrameworkMarshalling', @Register);
end.
