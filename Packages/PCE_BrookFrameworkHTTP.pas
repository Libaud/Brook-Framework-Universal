{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PCE_BrookFrameworkHTTP;

{$warn 5023 off : no warning about unused units}
interface

uses
  BrookHTTPAuthentication, BrookHTTPClient, BrookHTTPConstants, BrookHTTPCookies,
  BrookHTTPDefs, BrookHTTPRequest, BrookHTTPResponse, BrookHTTPServer, 
  BrookHTTPUploads, BrookHTTPRequestHelper, BrookHTTPBasedTypes, 
  BrookHttpUtils, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PCE_BrookFrameworkHTTP', @Register);
end.
