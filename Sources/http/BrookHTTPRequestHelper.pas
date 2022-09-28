unit BrookHTTPRequestHelper;

{$ifdef FPC}
  {$mode objfpc}{$H+}
  {$modeswitch TypeHelpers}
{$endif}

interface

uses
  Classes, SysUtils, BrookHTTPBasedTypes;

type
  { Type helper for HTTP verb conversion. }
  //TBrookHTTPRequestMethodHelper = {$ifdef DELPHI}record helper for TBrookHTTPRequestMethod{$else}class{$endif}
  TBrookHTTPRequestMethodHelper = type helper for TBrookHTTPRequestMethod
  public const
    { Holds the name of HTTP verbs. }
    METHODS: array[TBrookHTTPRequestMethod] of string = ('Unknown', 'GET', 'POST', 'PUT',
                                                         'DELETE', 'PATCH', 'OPTIONS', 'HEAD');
  public
    { Converts a @code(TBrookHTTPRequestMethod) to string. }
    function ToString: string; inline;
    { Returns a @code(TBrookHTTPRequestMethod) from a string. }
    function FromString(const AMethod: string): TBrookHTTPRequestMethod; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

implementation

{ TBrookHTTPRequestMethodHelper }

function TBrookHTTPRequestMethodHelper.ToString: string;
begin
  Result:= METHODS[Self];
end;

function TBrookHTTPRequestMethodHelper.FromString(const AMethod: string): TBrookHTTPRequestMethod;
var
  M: string;
  I: TBrookHTTPRequestMethod;
begin
  M := AMethod.ToUpper;
  for I := Low(METHODS) to High(METHODS) do
    if SameStr(M, METHODS[I]) then
      Exit(I);
  Result := rmUnknown;
end;

end.

