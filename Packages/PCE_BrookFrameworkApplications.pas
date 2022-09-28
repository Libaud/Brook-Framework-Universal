{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PCE_BrookFrameworkApplications;

{$warn 5023 off : no warning about unused units}
interface

uses
  BrookApplication, BrookApplicationInterfaces, BrookApplicationExceptions, 
  BrookHTTPApplication, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PCE_BrookFrameworkApplications', @Register);
end.
