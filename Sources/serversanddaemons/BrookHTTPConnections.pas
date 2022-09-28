unit BrookHTTPConnections;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpHTTPServer;

type
  { TBrookHttpConnectionRequest }

  TBrookHttpConnectionRequest = class(TFPHttpConnectionRequest)
      protected
        function GetTempUploadFileName(const AName, AFileName: string;
          ASize: Int64): string; override;
        function RequestUploadDir: string; override;
        procedure InitRequestVars; override;
      public
        procedure DeleteTempUploadedFiles; override;
    end;

  { TBrookHttpConnectionResponse }

  TBrookHttpConnectionResponse = class(TFPHttpConnectionResponse)
  protected
    procedure CollectHeaders(AHeaders: TStrings); override;
  end;


implementation

uses
  BrookGlobal, BrookConsts, BrookMessages, BrookHTTPConstants;

{ TBrookHttpConnectionRequest }

// Protected Method's

function TBrookHttpConnectionRequest.GetTempUploadFileName(const AName,
  AFileName: string; ASize: Int64): string;
begin
  if BrookSettingsInstance.KeepUploadedNames then
    Result := RequestUploadDir + AFileName
  else
    Result := inherited GetTempUploadFileName(AName, AFileName, ASize);
end;

function TBrookHttpConnectionRequest.RequestUploadDir: string;
begin
  Result := BrookSettingsInstance.DirectoryForUploads;
  if Result = '' then
    Result := GetTempDir;
  Result := IncludeTrailingPathDelimiter(Result);
end;

procedure TBrookHttpConnectionRequest.InitRequestVars;
var
  VMethod: string;
begin
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
end;

// Public Method's

procedure TBrookHttpConnectionRequest.DeleteTempUploadedFiles;
begin
  if BrookSettingsInstance.DeleteUploadedFiles then
    inherited;
end;

{ TBrookHttpConnectionResponse }

procedure TBrookHttpConnectionResponse.CollectHeaders(AHeaders: TStrings);
begin
  AHeaders.Add(BROOK_HTTP_HEADER_X_POWERED_BY + HS +
               'Brook for Free Pascal and FCL-Web.');
  inherited CollectHeaders(AHeaders);
end;

end.

