{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PCE_BrookFrameworkBrokers;

{$warn 5023 off : no warning about unused units}
interface

uses
  BrookFCLCGIBroker, BrookFCLEventLogBroker, BrookFCLFCGIBroker, 
  BrookFCLHttpAppBroker, BrookFCLHttpClientBroker, BrookFCLHttpDaemonBroker, 
  BrookHttPDefsBroker, BrookMessages_ptBR, BrookStaticFileBroker, 
  TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PCE_BrookFrameworkBrokers', @Register);
end.
