unit BrookLoggingOutputs;

{$mode objfpc}{$H+}

interface

uses
  RTLConsts,
{$IFDEF FPC}
  Math,
{$ELSE}
  Types,
  IOUtils,
{$ENDIF}
  SysUtils,
  DateUtils,
  Classes,
  BrookExtra;

type
  { Abstract class for logger output. }
  TBrookLoggerOutput = class abstract(TPersistent)
  private
    FFilters: TStringList;
    FOptions: TStringList;
    class function InternalFormat(const ALevel,
      AMessage: string): string; {$IFNDEF DEBUG}inline;{$ENDIF}
  protected
    class function FormatLog(const ALevel, AMessage: string): string; virtual;
    class function FormatFail(const ALevel: string;
      AException: Exception): string; virtual;
  public
    { Creates an instance of @code(TBrookLoggerOutput).
      @param(AFilters[in] Filters to be assigned to the logger instance.)
      @param(AOptions[in] Options to be assigned to the logger instance.) }
    constructor Create(AFilters, AOptions: TStringList); virtual;
    { Returns the alias name for output source.
      @returns(Output source alias.) }
    class function GetRegisterAlias: string; virtual;
    { Returns the name for output source.
      @returns(Output source name.) }
    class function GetName: string; virtual; abstract;
    { Returns @True if a certain log level is filtered.
      @param(ALevel[in] Log level.) }
    function IsFiltered(const ALevel: string): Boolean; virtual;
    { Appends a message to the output log.
      @param(ALevel[in] Log level.)
      @param(AMessage[in] Log message.) }
    procedure Log(const ALevel, AMessage: string); virtual; abstract;
    { Appends an exception message to the output log.
      @param(ALevel[in] Log level.)
      @param(AException[in] Log exception.) }
    procedure Fail(const ALevel: string;
      AException: Exception); virtual; abstract;
    { List containing the filtered log levels. }
    property Filters: TStringList read FFilters;
    { List containing additional options to the output. }
    property Options: TStringList read FOptions;
  end;

  { Class-reference for @code(TBrookLoggerOutput). }
  TBrookLoggerOutputClass = class of TBrookLoggerOutput;

  { Class for console logger output. }
  TBrookLoggerOutputConsole = class(TBrookLoggerOutput)
  public
    { Returns the name for output source.
      @returns(Output source name.) }
    class function GetName: string; override;
    { Appends a message to the output log.
      @param(ALevel[in] Log level.)
      @param(AMessage[in] Log message.) }
    procedure Log(const ALevel, AMessage: string); override;
    { Appends an exception message to the output log.
      @param(ALevel[in] Log level.)
      @param(AException[in] Log exception.) }
    procedure Fail(const ALevel: string; AException: Exception); override;
  end;

  { Class for file logger output. }
  TBrookLoggerOutputFile = class(TBrookLoggerOutput)
  private
    FHandle: TFileStream;
    FEncoding: TEncoding;
    FLastDate: TDate;
    FDirectory: string;
    FFileName: TFileName;
    procedure SetDirectory(const AValue: string);
  protected
    function CreateFile(AEncoding: TEncoding;
      const AFileName: TFileName): TFileStream; overload; virtual;
    function CreateFile(
      const AFileName: TFileName): TFileStream; overload; virtual;
    function RecreateFile(const AFileName: TFileName): TFileStream; virtual;
    procedure UpgradeFileName; virtual;
    procedure UpgradeFile; virtual;
    procedure WriteLog(const AMsg: string); {$IFNDEF DEBUG}inline;{$ENDIF}
    property LastDate: TDate read FLastDate;
    property Handle: TFileStream read FHandle;
  public
    { Method triggered after the constructor is called. }
    procedure AfterConstruction; override;
    { Destroys an instance of @code(TBrookLoggerOutputFile). }
    destructor Destroy; override;
    { Returns the name for output source.
      @returns(Output source name.) }
    class function GetName: string; override;
    { Appends a message to the output log.
      @param(ALevel[in] Log level.)
      @param(AMessage[in] Log message.) }
    procedure Log(const ALevel, AMessage: string); override;
    { Appends an exception message to the output log.
      @param(ALevel[in] Log level.)
      @param(AException[in] Log exception.) }
    procedure Fail(const ALevel: string; AException: Exception); override;
    { Specifies the output directory containing the logs. }
    property Directory: string read FDirectory write SetDirectory;
    { Generated absolute filename for the log. }
    property FileName: TFileName read FFileName;
  end;


implementation

uses
  BrookLoggingConstants, BrookLoggingRessources;

{ TBrookLoggerOutput }

constructor TBrookLoggerOutput.Create(AFilters, AOptions: TStringList);
begin
  inherited Create;
  if not Assigned(AFilters) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['AFilters']);
  if not Assigned(AOptions) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['AOptions']);
  FFilters := AFilters;
  FOptions := AOptions;
end;

class function TBrookLoggerOutput.GetRegisterAlias: string;
begin
  Result := Concat(BROOK_LOGGER_TAG, GetName);
end;

class function TBrookLoggerOutput.InternalFormat(const ALevel,
  AMessage: string): string;
begin
  Result := Concat(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now), ' ', ALevel,
    ': ', AMessage.TrimRight);
end;

class function TBrookLoggerOutput.FormatLog(const ALevel,
  AMessage: string): string;
begin
  Result := InternalFormat(ALevel, AMessage);
end;

class function TBrookLoggerOutput.FormatFail(const ALevel: string;
  AException: Exception): string;
begin
  if not Assigned(AException) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['AException']);
  Result := FormatLog(ALevel, Concat(AException.ClassName, ': ',
    AException.Message));
end;

function TBrookLoggerOutput.IsFiltered(const ALevel: string): Boolean;
begin
  Result := FFilters.IndexOf(ALevel) > -1;
end;

{ TBrookLoggerOutputConsole }

class function TBrookLoggerOutputConsole.GetName: string;
begin
  Result := 'Console';
end;

procedure TBrookLoggerOutputConsole.Log(const ALevel, AMessage: string);
begin
  if IsConsole and not IsFiltered(ALevel) then
    WriteLn(Output, FormatLog(ALevel, AMessage));
end;

procedure TBrookLoggerOutputConsole.Fail(const ALevel: string;
  AException: Exception);
begin
  if IsConsole and not IsFiltered(ALevel) then
    WriteLn(ErrOutput, FormatFail(ALevel, AException));
end;

{ TBrookLoggerOutputFile }

procedure TBrookLoggerOutputFile.AfterConstruction;
begin
  inherited AfterConstruction;
  if not FDirectory.IsEmpty then
    Exit;
  FDirectory := FOptions.Values['Directory'];
  if FDirectory.IsEmpty then
    FDirectory :=
  {$IFDEF FPC}
      GetUserDir
  {$ELSE}
      TPath.GetHomePath
  {$ENDIF};
  UpgradeFileName;
end;

destructor TBrookLoggerOutputFile.Destroy;
begin
  FHandle.Free;
  inherited Destroy;
end;

function TBrookLoggerOutputFile.CreateFile(AEncoding: TEncoding;
  const AFileName: TFileName): TFileStream;
var
  VMode: Word;
begin
  FEncoding := AEncoding;
  if not Assigned(FEncoding) then
    FEncoding := TEncoding.UTF8;
  if FileExists(AFileName) then
    VMode := fmOpenReadWrite
  else
    VMode := fmCreate;
  VMode := VMode or fmShareDenyWrite;
  Result := TFileStream.Create(AFileName, VMode, BROOK_FILE_RIGHTS);
end;

function TBrookLoggerOutputFile.CreateFile(
  const AFileName: TFileName): TFileStream;
begin
  Result := CreateFile(TEncoding.UTF8, AFileName);
end;

function TBrookLoggerOutputFile.RecreateFile(
  const AFileName: TFileName): TFileStream;
begin
  FHandle.Free;
  Result := CreateFile(FEncoding, AFileName);
end;

procedure TBrookLoggerOutputFile.UpgradeFileName;
var
  VDate: TDate;
begin
  VDate := Date;
  if CompareDateTime(IncDay(FLastDate), VDate) <> LessThanValue then
    Exit;
  FLastDate := VDate;
  FFileName := Concat(IncludeTrailingPathDelimiter(FDirectory),
    ChangeFileExt(ExtractFileName(ParamStr(0)), ''), '_',
    FormatDateTime('yyyymmdd', FLastDate), '.log');
end;

procedure TBrookLoggerOutputFile.UpgradeFile;
begin
  if not FDirectory.IsEmpty then
    ForceDirectories(FDirectory);
  UpgradeFileName;
  FHandle := RecreateFile(FFileName);
end;

procedure TBrookLoggerOutputFile.SetDirectory(const AValue: string);
begin
  if AValue = FDirectory then
    Exit;
  FDirectory := AValue;
  FLastDate := 0;
end;

procedure TBrookLoggerOutputFile.WriteLog(const AMsg: string);
var
  VBuffer: TBytes;
begin
  if not Assigned(FHandle) then
    Exit;
  VBuffer := FEncoding.GetBytes(
{$IFDEF FPC}UnicodeString({$ENDIF}Concat(AMsg, sLineBreak){$IFDEF FPC}){$ENDIF});
  FHandle.Seek(0, TSeekOrigin.soEnd);
  FHandle.WriteBuffer(VBuffer[0], FEncoding.GetCharCount(VBuffer));
end;

class function TBrookLoggerOutputFile.GetName: string;
begin
  Result := 'File';
end;

procedure TBrookLoggerOutputFile.Log(const ALevel, AMessage: string);
begin
  if IsFiltered(ALevel) then
    Exit;
  UpgradeFile;
  WriteLog(FormatLog(ALevel, AMessage));
end;

procedure TBrookLoggerOutputFile.Fail(const ALevel: string;
  AException: Exception);
begin
  if IsFiltered(ALevel) then
    Exit;
  UpgradeFile;
  WriteLog(FormatFail(ALevel, AException));
end;

end.

