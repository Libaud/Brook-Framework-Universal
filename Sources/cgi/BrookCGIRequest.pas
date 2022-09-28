unit BrookCGIRequest;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

{$I BrookDefines.inc}

interface

uses
  BrookClasses, BrookApplication, BrookLog, BrookMessages, BrookConsts,
  BrookHTTPConstants, {BrookRouter, BrookUtils, BrookHttpDefsBroker,} HttpDefs,
  {CustWeb,} CustCGI, Classes, SysUtils;

type
  { TBrookCGIRequest }

  TBrookCGIRequest = class(TCGIRequest)
      protected
        function GetTempUploadFileName(const AName, AFileName: string;
          ASize: Int64): string; override;
        function RequestUploadDir: string; override;
        procedure InitRequestVars; override;
      public
        procedure DeleteTempUploadedFiles; override;
    end;


implementation

uses
  BrookGlobal;

{ TBrookCGIRequest }

procedure TBrookCGIRequest.DeleteTempUploadedFiles;
begin
  if BrookSettingsInstance.DeleteUploadedFiles then
    inherited;
end;

function TBrookCGIRequest.GetTempUploadFileName(
  const AName, AFileName: string; ASize: Int64): string;
begin
  if BrookSettingsInstance.KeepUploadedNames then
    Result := RequestUploadDir + AFileName
  else
    Result := inherited GetTempUploadFileName(AName, AFileName, ASize);
end;

function TBrookCGIRequest.RequestUploadDir: string;
begin
  Result := BrookSettingsInstance.DirectoryForUploads;
  if Result = '' then
    Result := GetTempDir;
  Result := IncludeTrailingPathDelimiter(Result);
end;

procedure TBrookCGIRequest.InitRequestVars;
var
  VMethod: string;
begin
{$IFDEF BROOK_DEBUG}
  try
{$ENDIF}
    VMethod := Method;
    if VMethod = ES then
      raise Exception.Create(SBrookNoRequestMethodError);
    case VMethod of
      BROOK_HTTP_REQUEST_METHOD_DELETE, BROOK_HTTP_REQUEST_METHOD_PUT,
        BROOK_HTTP_REQUEST_METHOD_PATCH:
        begin
          InitPostVars;
          if HandleGetOnPost then
            InitGetVars;
        end;
    else
      inherited;
    end;
{$IFDEF BROOK_DEBUG}
  except
    on E: Exception do
    begin
      WriteLn('Content-Type: text/plain');
      WriteLn;
      WriteLn('Catastrophic error: ', E.Message);
      raise;
    end;
  end;
{$ENDIF}
end;

end.

