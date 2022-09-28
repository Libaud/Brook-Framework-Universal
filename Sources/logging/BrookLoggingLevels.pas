unit BrookLoggingLevels;

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
  { Class that retains the log levels. }
  TBrookLoggerLevels = class(TPersistent)
  private
    FInfo: string;
    FHint: string;
    FWarn: string;
    FDebug: string;
    FError: string;
    function IsInfoStored: Boolean;
    function IsHintStored: Boolean;
    function IsWarnStored: Boolean;
    function IsDebugStored: Boolean;
    function IsErrorStored: Boolean;
  public
    { Creates an instance of @code(TBrookLoggerLevels). }
    constructor Create; virtual;
    { Copies the properties of the source levels.
      @param(ASource[in] Levels source to be copied.) }
    procedure Assign(ASource: TPersistent); override;
  published
    { Level message for information log. }
    property Info: string read FInfo write FInfo stored IsInfoStored;
    { Level message for hint log. }
    property Hint: string read FHint write FHint stored IsHintStored;
    { Level message for warning log. }
    property Warn: string read FWarn write FWarn stored IsWarnStored;
    { Level message for debug log. }
    property Debug: string read FDebug write FDebug stored IsDebugStored;
    { Level message for error log. }
    property Error: string read FError write FError stored IsErrorStored;
  end;


implementation

uses
  BrookLoggingConstants, BrookLoggingRessources;

{ TBrookLoggerLevels }

constructor TBrookLoggerLevels.Create;
begin
  inherited Create;
  FInfo := SBrookLevelInfo;
  FHint := SBrookLevelHint;
  FWarn := SBrookLevelWarn;
  FDebug := SBrookLevelDebug;
  FError := SBrookLevelError;
end;

procedure TBrookLoggerLevels.Assign(ASource: TPersistent);
var
  VSrc: TBrookLoggerLevels;
begin
  if ASource is TBrookLoggerLevels then
  begin
    VSrc := ASource as TBrookLoggerLevels;
    FInfo := VSrc.Info;
    FHint := VSrc.Hint;
    FWarn := VSrc.Warn;
    FDebug := VSrc.Debug;
    FError := VSrc.Error;
  end
  else
    inherited Assign(ASource);
end;

function TBrookLoggerLevels.IsInfoStored: Boolean;
begin
  Result := FInfo <> SBrookLevelInfo;
end;

function TBrookLoggerLevels.IsHintStored: Boolean;
begin
  Result := FHint <> SBrookLevelHint;
end;

function TBrookLoggerLevels.IsWarnStored: Boolean;
begin
  Result := FWarn <> SBrookLevelWarn;
end;

function TBrookLoggerLevels.IsDebugStored: Boolean;
begin
  Result := FDebug <> SBrookLevelDebug;
end;

function TBrookLoggerLevels.IsErrorStored: Boolean;
begin
  Result := FError <> SBrookLevelError;
end;

end.

