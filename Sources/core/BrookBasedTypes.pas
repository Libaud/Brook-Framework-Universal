unit BrookBasedTypes;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils;

{$I BrookDefines.inc}

type
  TStringArray = specialize TArray<string>;

  { Defines an array of strings. }
  TBrookArrayOfString = array of string;

  { Defines an enumerator to represent the HTTP request methods. }
  TBrookRequestMethod = (rmUnknown, rmAll, rmGet, rmPost, rmPut,
                         rmDelete, rmHead, rmOptions, rmTrace
                        );

  { Defines a set to represent the AcceptEncoding HTTP header. }
  TBrookAcceptEncodingSet = set of (aeDeflate, aeGzip, aeSdch, aeXGzip);

  { Event signature used by stuff that handles errors.
    @param(ASender[in] Sender object.)
    @param(AException[in] Exception object.) }
  TBrookErrorEvent = procedure(ASender: TObject; AException: Exception) of object;


implementation

end.

