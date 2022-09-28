unit BrookLoggingApplications;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BrookLogger, BrookLoggingOutputs, BrookLoggingLevels;

type
  TBrookApplicationLoggerClass = class of TBrookApplicationLogger;

  { Component that writes log to a predefined output type. }
  TBrookApplicationLogger = class(TBrookLogger)
  private
    FOutput: TBrookLoggerOutput;
    FFilters: TStringList;
    FOptions: TStringList;
    FLevels: TBrookLoggerLevels;
    FOutputName: string;
    FStreamedActive: Boolean;
    FActive: Boolean;
    function GetOutput: TBrookLoggerOutput;
    procedure SetActive(AValue: Boolean);
    procedure SetOutputName(const AValue: string);
    function IsActiveStored: Boolean;
    function IsOutputNameStored: Boolean;
    procedure SetFilters(AValue: TStringList);
    procedure SetOptions(AValue: TStringList);
  protected
    procedure Loaded; override;
    function CreateFilters: TStringList; virtual;
    function CreateOptions: TStringList; virtual;
    function CreateOutput(AFilters, AOptions: TStringList): TBrookLoggerOutput; virtual;
    function CreateLevels: TBrookLoggerLevels; virtual;
    procedure DoOpen; virtual;
    procedure DoClose; virtual;
    procedure CheckActive; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure CheckInactive; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure CheckOutputName; {$IFNDEF DEBUG}inline;{$ENDIF}
  public
    { Creates an instance of @code(TBrookApplicationLogger).
      @param(AOwner[in] Owner component.) }
    constructor Create(AOwner: TComponent); override;
    { Destroys an instance of @code(TBrookApplicationLogger). }
    destructor Destroy; override;
    { Gets an output log class from the classes register. }
    function GetOutputClass: TBrookLoggerOutputClass;
{$IFNDEF DEBUG}inline;{$ENDIF}
    { Enabled the logger component. }
    procedure Open;
    { Disables the logger component. }
    procedure Close;
    { Appends a message to the output log.
      @param(ALevel[in] Log level.)
      @param(AMessage[in] Log message.) }
    procedure Log(const ALevel, AMessage: string);
{$IFNDEF DEBUG}inline;{$ENDIF}
    { Appends an exception message to the output log.
      @param(ALevel[in] Log level.)
      @param(AException[in] Log exception.) }
    procedure Fail(const ALevel: string; AException: Exception);
{$IFNDEF DEBUG}inline;{$ENDIF}
    { Appends a message to the output log as information level.
      @param(AMessage[in] Log message.) }
    procedure Info(const AMessage: string); {$IFNDEF DEBUG}inline;{$ENDIF}
    { Appends a message to the output log as hint level.
      @param(AMessage[in] Log message.) }
    procedure Hint(const AMessage: string); {$IFNDEF DEBUG}inline;{$ENDIF}
    { Appends a message to the output log as warning level.
      @param(AMessage[in] Log message.) }
    procedure Warn(const AMessage: string); {$IFNDEF DEBUG}inline;{$ENDIF}
    { Appends a message to the output log as debug level.
      @param(AMessage[in] Log message.) }
    procedure Debug(const AMessage: string); {$IFNDEF DEBUG}inline;{$ENDIF}
    { Appends a message to the output log as error level.
      @param(AMessage[in] Log message.) }
    procedure Error(AException: Exception); {$IFNDEF DEBUG}inline;{$ENDIF}
    { Current active log output. }
    property Output: TBrookLoggerOutput read GetOutput;
  published
    { Activates the logger component. }
    property Active: Boolean read FActive write SetActive stored IsActiveStored;
    { Retains the log levels. }
    property Levels: TBrookLoggerLevels read FLevels write FLevels;
    { Name of the chosen output type. }
    property OutputName: string read FOutputName write SetOutputName
      stored IsOutputNameStored;
    { List containing the filtered log levels. }
    property Filters: TStringList read FFilters write SetFilters;
    { List containing additional options to the chosen output. }
    property Options: TStringList read FOptions write SetOptions;
  end;

implementation

uses
  BrookLoggingConstants, BrookLoggingRessources;

{ TBrookApplicationLogger }

constructor TBrookApplicationLogger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLevels := CreateLevels;
  FFilters := CreateFilters;
  FOptions := CreateOptions;
  FOutputName := BROOK_LOGGER_OUTPUT_NAME;
end;

destructor TBrookApplicationLogger.Destroy;
begin
  SetActive(False);
  FLevels.Free;
  FOptions.Free;
  FFilters.Free;
  inherited Destroy;
end;

function TBrookApplicationLogger.CreateFilters: TStringList;
begin
  Result := TStringList.Create;
  Result.Add(FLevels.Info);
  Result.Add(FLevels.Hint);
  Result.Add(FLevels.Debug);
end;

function TBrookApplicationLogger.CreateOptions: TStringList;
begin
  Result := TStringList.Create;
end;

function TBrookApplicationLogger.GetOutputClass: TBrookLoggerOutputClass;
var
  C: TPersistentClass;
  N: string;
begin
  N := Concat(BROOK_LOGGER_TAG, FOutputName);
  C := Classes.GetClass(N);
  if not Assigned(C) then
    raise EClassNotFound.CreateFmt(SBrookUnknownOutputName, [FOutputName]);
  if not C.InheritsFrom(TBrookLoggerOutput) then
    raise EInvalidCast.CreateFmt(SBrookInvalidOutputClass, [C.ClassName]);
  Result := TBrookLoggerOutputClass(C);
end;

function TBrookApplicationLogger.CreateOutput(AFilters,
  AOptions: TStringList): TBrookLoggerOutput;
begin
  Result := GetOutputClass.Create(AFilters, AOptions);
end;

function TBrookApplicationLogger.CreateLevels: TBrookLoggerLevels;
begin
  Result := TBrookLoggerLevels.Create;
end;

procedure TBrookApplicationLogger.CheckOutputName;
begin
  if FOutputName.IsEmpty then
    raise EArgumentException.Create(SBrookEmptyOutputName);
end;

procedure TBrookApplicationLogger.CheckActive;
begin
  if not Active then
    raise EInvalidOpException.Create(SBrookInactiveOutput);
end;

procedure TBrookApplicationLogger.CheckInactive;
begin
  if (not (csLoading in ComponentState)) and Active then
    raise EInvalidOpException.Create(SBrookActiveOutput);
end;

procedure TBrookApplicationLogger.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedActive then
      SetActive(True);
  except
    if csDesigning in ComponentState then
    begin
      if Assigned(ApplicationHandleException) then
        ApplicationHandleException(ExceptObject)
      else
        ShowException(ExceptObject, ExceptAddr);
    end
    else
      raise;
  end;
end;

procedure TBrookApplicationLogger.DoOpen;
begin
  if Assigned(FOutput) then
    Exit;
  CheckOutputName;
  FOutput := CreateOutput(FFilters, FOptions);
  FActive := True;
end;

procedure TBrookApplicationLogger.DoClose;
begin
  if not Assigned(FOutput) then
    Exit;
  FOutput.Destroy;
  FOutput := nil;
  FActive := False;
end;

function TBrookApplicationLogger.IsActiveStored: Boolean;
begin
  Result := FActive;
end;

function TBrookApplicationLogger.GetOutput: TBrookLoggerOutput;
begin
  CheckActive;
  Result := FOutput;
end;

function TBrookApplicationLogger.IsOutputNameStored: Boolean;
begin
  Result := FOutputName <> BROOK_LOGGER_OUTPUT_NAME;
end;

procedure TBrookApplicationLogger.SetActive(AValue: Boolean);
begin
  if AValue = FActive then
    Exit;
  if csDesigning in ComponentState then
    FActive := AValue
  else
    if AValue then
    begin
      if csReading in ComponentState then
        FStreamedActive := True
      else
        DoOpen;
    end
    else
      DoClose;
end;

procedure TBrookApplicationLogger.SetFilters(AValue: TStringList);
begin
  FFilters.Assign(AValue);
end;

procedure TBrookApplicationLogger.SetOptions(AValue: TStringList);
begin
  FOptions.Assign(AValue);
end;

procedure TBrookApplicationLogger.SetOutputName(const AValue: string);
begin
  if FOutputName = AValue then
    Exit;
  if not FStreamedActive then
    CheckInactive;
  FOutputName := AValue;
  if FOutputName.IsEmpty then
    FOutputName := BROOK_LOGGER_OUTPUT_NAME;
end;

procedure TBrookApplicationLogger.Open;
begin
  SetActive(True);
end;

procedure TBrookApplicationLogger.Close;
begin
  SetActive(False);
end;

procedure TBrookApplicationLogger.Log(const ALevel, AMessage: string);
begin
  if Active then
    Output.Log(ALevel, AMessage);
end;

procedure TBrookApplicationLogger.Fail(const ALevel: string; AException: Exception);
begin
  if Active then
    Output.Fail(ALevel, AException);
end;

procedure TBrookApplicationLogger.Info(const AMessage: string);
begin
  Log(FLevels.Info, AMessage);
end;

procedure TBrookApplicationLogger.Hint(const AMessage: string);
begin
  Log(FLevels.Hint, AMessage);
end;

procedure TBrookApplicationLogger.Warn(const AMessage: string);
begin
  Log(FLevels.Warn, AMessage);
end;

procedure TBrookApplicationLogger.Debug(const AMessage: string);
begin
  Log(FLevels.Debug, AMessage);
end;

procedure TBrookApplicationLogger.Error(AException: Exception);
begin
  Fail(FLevels.Error, AException);
end;

end.

