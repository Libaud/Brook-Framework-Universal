unit BrookCGIApplications;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

{$I BrookDefines.inc}

interface

uses
  BrookClasses, BrookApplication, BrookLog, BrookMessages, BrookConsts,
  BrookHTTPConstants, BrookRouter, BrookUtils, {BrookHttpDefsBroker,} HttpDefs,
  CustWeb, CustCGI, Classes, SysUtils;

type
  TBrookCGIApplication = class;

  { TBrookCGIApplication }

  TBrookCGIApplication = class(TCustomCGIApplication)
  protected
    function InitializeWebHandler: TWebHandler; override;
  end;

implementation

uses
  BrookCGIHandler;

{ TBrookCGIApplication }

function TBrookCGIApplication.InitializeWebHandler: TWebHandler;
begin
  Result := TBrookCGIHandler.Create(Self);
end;

end.

