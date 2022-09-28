unit BrookLocker;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  { Allows to lock other threads from accessing a block of code. }
  TBrookLocker = class(TPersistent)
  private
    FMutex: TCriticalSection;
    FActive: Boolean;
    procedure SetActive(AValue: Boolean);
    function IsActiveStored: Boolean;
  protected
    property Mutex: TCriticalSection read FMutex;
    function CreateMutex: TCriticalSection; virtual;
  public
    { Creates an instance of @code(TBrookLocker). }
    constructor Create; virtual;
    { Frees an instance of @code(TBrookLocker). }
    destructor Destroy; override;
    { Locks all other threads. }
    procedure Lock; virtual;
    { Unlocks all other threads. }
    procedure Unlock; virtual;
    { Tries to lock all other threads. }
    function  TryLock: Boolean; virtual;
  published
    { Activates the locker. (Default: @True) }
    property Active: Boolean read FActive
                             write SetActive
                             stored IsActiveStored;
  end;

implementation

{ TBrookLocker }

constructor TBrookLocker.Create;
begin
  inherited Create;
  FMutex := CreateMutex;
  FActive := True;
end;

destructor TBrookLocker.Destroy;
begin
  FMutex.Free;
  inherited Destroy;
end;

function TBrookLocker.CreateMutex: TCriticalSection;
begin
  Result := TCriticalSection.Create;
end;

procedure TBrookLocker.SetActive(AValue: Boolean);
begin
  if FActive = AValue then
    Exit;
  FMutex.Acquire;
  try
    FActive := AValue;
  finally
    FMutex.Release;
  end;
end;

function TBrookLocker.IsActiveStored: Boolean;
begin
  Result := not FActive;
end;

procedure TBrookLocker.Lock;
begin
  if FActive then
    FMutex.Acquire;
end;

procedure TBrookLocker.Unlock;
begin
  if FActive then
    FMutex.Release;
end;

function TBrookLocker.TryLock: Boolean;
begin
  Result := FActive and FMutex.TryEnter;
end;

end.

