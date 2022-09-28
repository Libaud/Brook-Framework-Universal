{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PCE_BrookFrameworkLogging;

{$warn 5023 off : no warning about unused units}
interface

uses
  BrookLog, BrookLogger, BrookLoggingConstants, BrookLoggingRessources, 
  BrookLoggingOutputs, BrookLoggingLevels, BrookLoggingExceptions, 
  BrookLoggingBasedTypes, BrookLoggingServices, BrookLoggingApplications, 
  TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PCE_BrookFrameworkLogging', @Register);
end.
