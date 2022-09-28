{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PCE_BrookFrameworkHandlers;

{$warn 5023 off : no warning about unused units}
interface

uses
  BrookConfiguratorHandler, BrookFCLeventLogHandler, 
  BrookMessagesHandler_Ptbr, BrookMiddlewareHandler, BrookRouterHandler, 
  BrookSessionHandler, BrookHandlersExceptions, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PCE_BrookFrameworkHandlers', @Register);
end.
