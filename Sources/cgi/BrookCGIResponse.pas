unit BrookCGIResponse;

{$mode objfpc}{$H+}

interface

uses
  BrookClasses, BrookApplication, BrookLog, BrookMessages, BrookConsts,
  BrookHTTPConstants, {BrookRouter, BrookUtils, BrookHttpDefsBroker,} HttpDefs,
  {CustWeb,} CustCGI, Classes, SysUtils;

type
  { TBrookCGIResponse }

  TBrookCGIResponse = class(TCGIResponse)
      protected
        procedure CollectHeaders(AHeaders: TStrings); override;
    end;


implementation

uses
  BrookGlobal;

{ TBrookCGIResponse }

procedure TBrookCGIResponse.CollectHeaders(AHeaders: TStrings);
begin
  AHeaders.Add(BROOK_HTTP_HEADER_X_POWERED_BY + HS +
    'Brook for Free Pascal and FCL-Web.');
  inherited CollectHeaders(AHeaders);
end;


end.

