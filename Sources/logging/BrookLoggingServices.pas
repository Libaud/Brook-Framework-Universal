unit BrookLoggingServices;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BrookLogger, BrookLoggingBasedTypes;

type
  { Is a metaclass for @link(TBrookLogger) class. }
  TBrookServiceLoggerClass = class of TBrookServiceLogger;

  { Provides features for the application logging. }
  TBrookServiceLogger = class(TBrookLogger)
  private
    FActive: Boolean;
    FAfterLog: TBrookLogEvent;
    FBeforeLog: TBrookLogEvent;
    FFileName: TFileName;
    FOutput: TBrookLogOutput;
    FPrepared: Boolean;
    FTypes: TBrookLogTypes;
  protected
    function GetActive: Boolean; virtual;
    procedure SetActive(const AValue: Boolean); virtual;
    function GetFileName: TFileName; virtual;
    procedure SetFileName(const AValue: TFileName); virtual;
    procedure SetOutput(const AValue: TBrookLogOutput); virtual;
    function GetOutput: TBrookLogOutput; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    { Return the service class provided by this class. }
    class function GetServiceClass: TBrookLoggerClass;
    { Registers the service provided by this class. }
    class procedure RegisterService;
    { Unregisters the service  provided by this class. }
    class procedure UnregisterService;
    { Return an instance of this class. }
    class function Service: TBrookServiceLogger;
    { Prepare the logger broker. }
    procedure Prepare; virtual;
    { Unprepare the logger broker. }
    procedure Unprepare; virtual;
    { Writes a log. }
    procedure Log(const AType: TBrookLogType; const S: string;
      const ACode: Word; const E: Exception = nil); virtual; abstract;
    { Writes a log triggering the @code(AfterLog) and @(BeforeLog) events. }
    procedure DoLog(const AType: TBrookLogType; const S: string;
      const ACode: Word; const E: Exception = nil); virtual;
    { Writes a custom log. }
    procedure Custom(const S: string; const ACode: Word); virtual;
    { Writes an information log. }
    procedure Info(const S: string); virtual;
    { Writes a warning log. }
    procedure Warn(const S: string); virtual;
    { Writes a debug log. }
    procedure Debug(const S: string); virtual;
    { Writes an error log. }
    procedure Error(const S: string; E: Exception = nil); virtual;
    { Enables or disables the logger. }
    property Active: Boolean read GetActive write SetActive;
    { Defines the name of the log file. }
    property FileName: TFileName read GetFileName write SetFileName;
    { The logger output types. }
    property Types: TBrookLogTypes read FTypes write FTypes;
    { The logger output mode. }
    property Output: TBrookLogOutput read GetOutput write SetOutput;
    { Return @code(True) if broker is prepared. }
    property Prepared: Boolean read FPrepared;
    { Is triggered after the logger writes a log. }
    property AfterLog: TBrookLogEvent read FAfterLog write FAfterLog;
    { Is triggered before the logger writes a log. }
    property BeforeLog: TBrookLogEvent read FBeforeLog write FBeforeLog;
  end;


implementation

uses
  BrookLoggingExceptions, BrookLoggingConstants, BrookLoggingRessources,
  BrookMessages;

var
  _BrookLoggerService: TBrookServiceLogger = nil;
  _BrookLoggerServiceClass: TBrookServiceLoggerClass = nil;

{ TBrookServiceLogger }

constructor TBrookServiceLogger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTypes := [ltCustom, ltInfo, ltWarning, ltError, ltDebug];
end;

class function TBrookServiceLogger.GetServiceClass: TBrookLoggerClass;
begin
  Result := _BrookLoggerServiceClass;
end;

class procedure TBrookServiceLogger.RegisterService;
begin
  if Assigned(_BrookLoggerServiceClass) then
    raise
      EBrookLogger.Create(Self, SBrookLoggerServiceAlreadyRegisteredError);
  _BrookLoggerServiceClass := Self;
end;

class procedure TBrookServiceLogger.UnregisterService;
begin
  FreeAndNil(_BrookLoggerService);
  _BrookLoggerServiceClass := nil;
end;

class function TBrookServiceLogger.Service: TBrookServiceLogger;
begin
  if not Assigned(_BrookLoggerService) then
  begin
    if not Assigned(_BrookLoggerServiceClass) then
      raise EBrookLogger.Create(Self, SBrookNoLoggerServiceRegisteredError);
    _BrookLoggerService := _BrookLoggerServiceClass.Create(nil);
  end;
  Result := _BrookLoggerService;
end;

procedure TBrookServiceLogger.Prepare;
begin
  FPrepared := True;
end;

procedure TBrookServiceLogger.Unprepare;
begin
  FPrepared := False;
end;

procedure TBrookServiceLogger.DoLog(const AType: TBrookLogType; const S: string;
  const ACode: Word; const E: Exception);
var
  VHandled: Boolean = False;
begin
  try
    if Assigned(FBeforeLog) then
      FBeforeLog(Self, AType, S, ACode, E, VHandled);
    if VHandled then
      Exit;
    if not FPrepared then
      Prepare;
    Log(AType, S, ACode, E);
  finally
    if Assigned(FAfterLog) then
      FAfterLog(Self, AType, S, ACode, E, VHandled);
  end;
end;

procedure TBrookServiceLogger.Custom(const S: string; const ACode: Word);
begin
  DoLog(ltCustom, S, ACode, nil);
end;

procedure TBrookServiceLogger.Info(const S: string);
begin
  DoLog(ltInfo, S, 0, nil);
end;

procedure TBrookServiceLogger.Warn(const S: string);
begin
  DoLog(ltWarning, S, 0, nil);
end;

procedure TBrookServiceLogger.Debug(const S: string);
begin
  DoLog(ltDebug, S, 0, nil);
end;

procedure TBrookServiceLogger.Error(const S: string; E: Exception);
begin
  DoLog(ltError, S, 0, E);
end;

function TBrookServiceLogger.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TBrookServiceLogger.SetActive(const AValue: Boolean);
begin
  if AValue <> FActive then
  begin
    FActive := AValue;
    Unprepare;
  end;
end;

function TBrookServiceLogger.GetFileName: TFileName;
begin
  Result := FFileName;
end;

procedure TBrookServiceLogger.SetFileName(const AValue: TFileName);
begin
  if AValue <> FFileName then
  begin
    FFileName := AValue;
    Unprepare;
  end;
end;

procedure TBrookServiceLogger.SetOutput(const AValue: TBrookLogOutput);
begin
  if AValue <> FOutput then
  begin
    FOutput := AValue;
    Unprepare;
  end;
end;

function TBrookServiceLogger.GetOutput: TBrookLogOutput;
begin
  Result := FOutput;
end;

end.

