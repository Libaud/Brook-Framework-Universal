{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PCE_BrookFrameworkHTTPCore;

{$warn 5023 off : no warning about unused units}
interface

uses
  BrookHTTPAuthentication, BrookHTTPClient, BrookHTTPConstants, 
  BrookHTTPCookies, BrookHTTPRequest, BrookHTTPResponse, BrookHTTPServer, 
  BrookHTTPUploads, BrookHTTPRequestHelper, BrookHTTPBasedTypes, 
  BrookHTTPUtils, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PCE_BrookFrameworkHTTPCore', @Register);
end.
