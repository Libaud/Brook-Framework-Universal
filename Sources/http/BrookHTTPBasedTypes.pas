unit BrookHTTPBasedTypes;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils;

type
  { HTTP verbs enumeration. }
  TBrookHTTPRequestMethod = (rmUnknown, rmGET, rmPOST, rmPUT, rmDELETE, rmPATCH, rmOPTIONS, rmHEAD);

  { Set of HTTP verbs. }
  TBrookHTTPRequestMethods = set of TBrookHTTPRequestMethod;

implementation

end.

