unit BrookServersAndDaemonsUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustWeb, HTTPDefs, BrookHTTPConstants;

function BrookFormatContentType: string;
procedure BrookShowRequestException(AHandler: TWebHandler; aResponse: TResponse; aException: Exception);

implementation

uses
  StrUtils, BrookGlobal, BrookConsts, BrookException;

function BrookFormatContentType: string;
begin
  if BrookSettingsInstance.Charset <> ES then
    Result := BrookSettingsInstance.ContentType + BROOK_HTTP_HEADER_CHARSET +
      BrookSettingsInstance.Charset
  else
    Result := BrookSettingsInstance.ContentType;
end;

procedure BrookShowRequestException(AHandler: TWebHandler; aResponse: TResponse; aException: Exception);
var
  VHandled: Boolean = False;

  procedure HandleHttp404;
  begin
    if not aResponse.HeadersSent then
    begin
      aResponse.Code := BROOK_HTTP_STATUS_CODE_NOT_FOUND;
      aResponse.CodeText := BROOK_HTTP_REASON_PHRASE_NOT_FOUND;
      aResponse.ContentType := BrookFormatContentType;
    end;
    if (BrookSettingsInstance.Page404File <> ES) and
      FileExists(BrookSettingsInstance.Page404File) then
      aResponse.Contents.LoadFromFile(BrookSettingsInstance.Page404File)
    else
      aResponse.Content := BrookSettingsInstance.Page404;
    aResponse.Content := StringsReplace(aResponse.Content, ['@root', '@path'],
      [BrookSettingsInstance.RootUrl, aException.Message], [rfIgnoreCase, rfReplaceAll]);
    aResponse.SendContent;
    VHandled := True;
  end;

  procedure HandleHttp500;
  begin
    if not aResponse.HeadersSent then
    begin
      aResponse.Code := BROOK_HTTP_STATUS_CODE_INTERNAL_SERVER_ERROR;
      aResponse.CodeText := BROOK_HTTP_REASON_PHRASE_INTERNAL_SERVER_ERROR;
      aResponse.ContentType := BrookFormatContentType;
    end;
    if (BrookSettingsInstance.Page500File <> ES) and
      FileExists(BrookSettingsInstance.Page500File) then
    begin
      aResponse.Contents.LoadFromFile(BrookSettingsInstance.Page500File);
      aResponse.Content := StringReplace(aResponse.Content, '@error', aException.Message,
        [rfIgnoreCase, rfReplaceAll]);
    end
    else
    begin
      aResponse.Content := BrookSettingsInstance.Page500;
      aResponse.Content := StringReplace(BrookSettingsInstance.Page500, '@error', aException.Message,
        [rfIgnoreCase, rfReplaceAll]);
    end;
    aResponse.SendContent;
    VHandled := True;
  end;

begin
  if aResponse.ContentSent then
    Exit;
  if Assigned(BrookSettingsInstance.OnError) then
  begin
    BrookSettingsInstance.OnError(aResponse, aException, VHandled);
    if VHandled then
      Exit;
  end;
  if Assigned(AHandler.OnShowRequestException) then
  begin
    AHandler.OnShowRequestException(aResponse, aException, VHandled);
    if VHandled then
      Exit;
  end;
  if AHandler.RedirectOnError and not aResponse.HeadersSent then
  begin
    aResponse.SendRedirect(Format(AHandler.RedirectOnErrorURL, [HttpEncode(aException.Message)]));
    aResponse.SendContent;
    Exit;
  end;
  if aException is EBrookHttp404 then
    HandleHttp404
  else
    HandleHttp500;
end;
end.

