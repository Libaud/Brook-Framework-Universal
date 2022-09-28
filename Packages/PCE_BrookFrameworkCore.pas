{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PCE_BrookFrameworkCore;

{$warn 5023 off : no warning about unused units}
interface

uses
  RegisterPCE_BrookFramework, BrookExtra, BrookHandledClasses, 
  BrookMediaTypes, BrookReader, BrookString, BrookStringMap, BrookUtility, 
  BrookClasses, BrookConsts, BrookException, BrookMessages, BrookBasedTypes, 
  BrookInterfaces, BrookLocker, BrookTools, BrookUtils, TyphonPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RegisterPCE_BrookFramework', 
    @RegisterPCE_BrookFramework.Register);
end;

initialization
  RegisterPackage('PCE_BrookFrameworkCore', @Register);
end.
