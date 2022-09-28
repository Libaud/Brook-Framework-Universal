{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PCE_BrookFramework;

{$warn 5023 off : no warning about unused units}
interface

uses
  BrookExtra, BrookHandledClasses, BrookHTTPAuthentication, BrookHTTPCookies, 
  BrookHTTPRequest, BrookHTTPResponse, BrookHTTPServer, BrookHTTPUploads, 
  BrookLibraryLoader, BrookLogger, BrookMathExpression, BrookMediaTypes, 
  BrookReader, BrookString, BrookStringMap, BrookURLEntryPoints, 
  BrookURLRouter, BrookUtility, libsagui, Marshalling, Platform, 
  RegisterPCE_BrookFramework, TyphonPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RegisterPCE_BrookFramework', 
    @RegisterPCE_BrookFramework.Register);
end;

initialization
  RegisterPackage('PCE_BrookFramework', @Register);
end.
