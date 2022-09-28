unit BrookRoutes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BrookBasedTypes, BrookClasses, BrookRouterBasedTypes, BrookActionBasedClasses, BrookActionInterfaces;

type
  { Is a metaclass for @link(TBrookRoutes) class. }
  TBrookRoutesClass = class of TBrookRoutes;


  { Defines a list of routes. }
  TBrookRoutes = class(TBrookPersistent)
  private
    FList: TFPList;
    function GetItems(const AIndex: Integer): PBrookRoute;
    procedure SetItems(const AIndex: Integer; const AValue: PBrookRoute);
  protected
    procedure FreeRoutes; virtual;
  public
    { Creates an instance of a @link(TBrookRoutes) class. }
    constructor Create; virtual;
    { Frees an instance of @link(TBrookRoutes) class. }
    destructor Destroy; override;
    { Clears all routes. }
    procedure Clear;
    { Returns the number of registered routes. }
    function Count: Integer;
    { Adds a route item. }
    function Add(AActionClass: TBrookActionClass; const APattern: string;
      const AMethod: TBrookRequestMethod; const ADefault: Boolean): Integer;
    { Get the default action class. }
    procedure GetDefaultActionClass(out AClass: TBrookActionClass;
      out AIndex: Integer);
    { Get the action class with empty pattern. }
    procedure GetEmptyPatternActionClass(out AClass: TBrookActionClass;
      out AIndex: Integer);
    { Get the registered pattern of a class. }
    function PatternByActionClass(AClass: TBrookActionClass): string;
    { Get the action class from a patter. }
    function ActionClassByPattern(const APattern: string): TBrookActionClass;
    { Get an action class from its class name. }
    function ActionClassByClassName(const AName: string): TBrookActionClass;
    { The list of routes. }
    property Items[const AIndex: Integer]: PBrookRoute read GetItems
                                                       write SetItems; default;
    property List: TFPList read FList;
  end;

implementation

uses
  BrookRouterExceptions, BrookGlobal, BrookMessages, BrookConsts;

{ TBrookRoutes }

constructor TBrookRoutes.Create;
begin
  inherited Create;
  FList := TFPList.Create;
end;

destructor TBrookRoutes.Destroy;
begin
  FreeRoutes;
  FList.Free;
  inherited Destroy;
end;

procedure TBrookRoutes.Clear;
begin
  FreeRoutes;
  FList.Clear;
end;

function TBrookRoutes.Count: Integer;
begin
  Result := FList.Count;
end;

function TBrookRoutes.GetItems(const AIndex: Integer): PBrookRoute;
begin
  Result := FList.Items[AIndex];
end;

procedure TBrookRoutes.SetItems(const AIndex: Integer; const AValue: PBrookRoute);
begin
  FList.Items[AIndex] := AValue;
end;

procedure TBrookRoutes.FreeRoutes;
var
  P: PBrookRoute;
begin
  for P in FList do
    Dispose(P);
end;

function TBrookRoutes.Add(AActionClass: TBrookActionClass;
                          const APattern: string; const AMethod: TBrookRequestMethod;
                          const ADefault: Boolean): Integer;
var
  PRoute: PBrookRoute;
begin
  for PRoute in FList do
  begin
    if BrookSettingsInstance.Mapped then
    begin
      if (PRoute^.ActionClass = AActionClass) and
        (PRoute^.Pattern = APattern) and (PRoute^.Method = AMethod) then
        raise EBrookRoutes.CreateFmt(Self, SBrookActionAlreadyRegisteredError,
          [AActionClass.ClassName]);
      if (PRoute^.Pattern = APattern) and (PRoute^.Method = AMethod) then
        raise EBrookRoutes.CreateFmt(Self, SBrookPatternAlreadyRegisteredError,
          [APattern]);
    end
    else
    begin
      if (PRoute^.ActionClass = AActionClass) and
        (PRoute^.Pattern = APattern) then
        raise EBrookRoutes.CreateFmt(Self, SBrookActionAlreadyRegisteredError,
          [AActionClass.ClassName]);
      if PRoute^.Pattern = APattern then
        raise EBrookRoutes.CreateFmt(Self, SBrookPatternAlreadyRegisteredError,
          [APattern]);
    end;
    if ADefault and PRoute^.Default and (PRoute^.ActionClass <> AActionClass) then
      raise EBrookRoutes.Create(Self, SBrookDefaultActionAlreadyRegisteredError);
  end;
  New(PRoute);
  PRoute^.ActionClass := AActionClass;
  PRoute^.Default := ADefault;
  PRoute^.Method := AMethod;
  PRoute^.Pattern := APattern;
  Result := FList.Add(PRoute);
end;

procedure TBrookRoutes.GetDefaultActionClass(out AClass: TBrookActionClass;
  out AIndex: Integer);
var
  I: Integer;
  PRoute: PBrookRoute;
begin
  for I := 0 to Pred(FList.Count) do
  begin
    PRoute := FList[I];
    if PRoute^.Default then
    begin
      AIndex := I;
      AClass := PRoute^.ActionClass;
      Exit;
    end;
  end;
  AIndex := -1;
  AClass := nil;
end;

procedure TBrookRoutes.GetEmptyPatternActionClass(out
  AClass: TBrookActionClass; out AIndex: Integer);
var
  I: Integer;
  PRoute: PBrookRoute;
begin
  for I := 0 to Pred(FList.Count) do
  begin
    PRoute := FList[I];
    if PRoute^.Pattern = ES then
    begin
      AIndex := I;
      AClass := PRoute^.ActionClass;
      Exit;
    end;
  end;
  AIndex := -1;
  AClass := nil;
end;

function TBrookRoutes.PatternByActionClass(AClass: TBrookActionClass): string;
var
  PRoute: PBrookRoute;
begin
  for PRoute in FList do
    if PRoute^.ActionClass = AClass then
    begin
      Result := PRoute^.Pattern;
      Exit;
    end;
  Result := ES;
end;

function TBrookRoutes.ActionClassByPattern(
  const APattern: string): TBrookActionClass;
var
  PRoute: PBrookRoute;
begin
  for PRoute in FList do
    if PRoute^.Pattern = APattern then
    begin
      Result := PRoute^.ActionClass;
      Exit;
    end;
  Result := nil;
end;

function TBrookRoutes.ActionClassByClassName(
  const AName: string): TBrookActionClass;
var
  PRoute: PBrookRoute;
begin
  for PRoute in FList do
    if SameText(PRoute^.ActionClass.ClassName, AName) then
    begin
      Result := PRoute^.ActionClass;
      Exit;
    end;
  Result := nil;
end;

end.

