unit BrookCGIHandler;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

{$I BrookDefines.inc}

interface

uses
  BrookClasses, BrookApplication, BrookLog, BrookMessages, BrookConsts,
  BrookHTTPConstants, {BrookRouter, BrookUtils, BrookHttpDefsBroker,} HttpDefs,
  CustWeb, CustCGI, Classes, SysUtils;

type
  { TBrookCGIHandler }

  TBrookCGIHandler = class(TCGIHandler)
  protected
    function CreateRequest: TCGIRequest; override;
    function CreateResponse(AOutput: TStream): TCGIResponse; override;
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
    procedure ShowRequestException(R: TResponse; E: Exception); override;
  end;

implementation

uses
  BrookRouter, BrookGlobal, BrookCGIRequest, BrookCGIResponse,
  BrookLoggingServices, BrookServersAndDaemonsUtils;

{ TBrookCGIHandler }

function TBrookCGIHandler.CreateRequest: TCGIRequest;
begin
  Result := TBrookCGIRequest.CreateCGI(Self);
  if ApplicationURL = ES then
    ApplicationURL := TBrookRouter.RootUrl;
end;

function TBrookCGIHandler.CreateResponse(AOutput: TStream): TCGIResponse;
begin
  Result := TBrookCGIResponse.CreateCGI(Self, AOutput);
end;

procedure TBrookCGIHandler.HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  VLog: string;
begin
  AResponse.ContentType := BrookFormatContentType;
  if BrookSettingsInstance.LogActive then
  begin
    VLog := LineEnding;
    if ARequest.PathInfo <> ES then
      VLog += '<PathInfo>' + LineEnding + ARequest.PathInfo + LineEnding +
        '</PathInfo>' + LineEnding;
    if ARequest.CookieFields.Count > 0 then
      VLog += '<Cookies>' + LineEnding + ARequest.CookieFields.Text +
        '</Cookies>' + LineEnding;
    if ARequest.ContentFields.Count > 0 then
      VLog += '<Fields>' + LineEnding + ARequest.ContentFields.Text +
        '</Fields>' + LineEnding;
    if ARequest.QueryFields.Count > 0 then
      VLog += '<Params>' + LineEnding + ARequest.QueryFields.Text +
        '</Params>' + LineEnding;
  end;
  try
    TBrookRouter.Service.Route(ARequest, AResponse);
    TBrookCGIRequest(ARequest).DeleteTempUploadedFiles;
    if BrookSettingsInstance.LogActive and (AResponse.Contents.Count > 0) then
    begin
      VLog += '<Content>' + LineEnding + AResponse.Contents.Text +
        '</Content>';
      TBrookServiceLogger.Service.Info(VLog);
    end;
  except
    on E: Exception do
    begin
      if BrookSettingsInstance.LogActive then
        TBrookServiceLogger.Service.Error(VLog, E);
      ShowRequestException(AResponse, E);
    end;
  end;
end;

procedure TBrookCGIHandler.ShowRequestException(R: TResponse; E: Exception);
begin
  BrookShowRequestException(Self, R, E);
end;

end.

