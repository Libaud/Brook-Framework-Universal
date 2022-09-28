{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PCE_BrookFrameworkIDEIntegration;

{$warn 5023 off : no warning about unused units}
interface

uses
  RegisterPCE_BrookFrameworkIDEIntegration, BrookIDEIntegration, BrookIDEIntf, 
  BrookPropEdits, frmBrookActEdit, frmBrookChooseProjectType, 
  frmBrookNewBroker, frmBrookNewProject, TyphonPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RegisterPCE_BrookFrameworkIDEIntegration', 
    @RegisterPCE_BrookFrameworkIDEIntegration.Register);
end;

initialization
  RegisterPackage('PCE_BrookFrameworkIDEIntegration', @Register);
end.
