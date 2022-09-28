(*
  Brook for Free Pascal

  Copyright (C) 2014-2019 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

{ Application interface. }
unit BrookApplication;

{$I BrookDefines.inc}

interface

uses
  CustWeb, BrookConsts, BrookClasses, BrookMessages, Classes,
  BrookApplicationInterfaces;

type
    { TBrookApplication }

  TBrookApplication = class(TBrookInterfacedObject, IBrookApplication)
  private
    //FApp: TBrookCGIApplication;
    FApp: TCustomWebApplication;
    function GetTerminated: Boolean;
    function GetTitle: string;
    procedure SetTitle(const aTitle: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure CreateForm(AInstanceClass: TComponentClass; out AReference);
    function Instance: TObject;
    procedure Run;
    procedure Terminate;
    property Terminated: Boolean read GetTerminated;
  end;


implementation

uses
  BrookGlobal;

{ TBrookApplication }

function TBrookApplication.GetTerminated: Boolean;
begin
  Result := FApp.Terminated;
end;

function TBrookApplication.GetTitle: string;
begin

end;

procedure TBrookApplication.SetTitle(const aTitle: string);
begin

end;

constructor TBrookApplication.Create;
begin
  //FApp := TBrookCGIApplication.Create(nil);
  FApp.Initialize;
end;

destructor TBrookApplication.Destroy;
begin
  FApp.Free;
  inherited Destroy;
end;

procedure TBrookApplication.CreateForm(AInstanceClass: TComponentClass;
  out AReference);
var
  VReference: TComponent;
begin
  VReference := AInstanceClass.Create(nil);
  TComponent(AReference) := VReference;
  FApp.InsertComponent(VReference);
end;

function TBrookApplication.Instance: TObject;
begin
  Result := FApp;
end;

procedure TBrookApplication.Run;
begin
  if BrookSettingsInstance.RootUrl <> '' then
    FApp.ApplicationURL := BrookSettingsInstance.RootUrl;
  FApp.Run;
end;

procedure TBrookApplication.Terminate;
begin
  FApp.Terminate;
end;

initialization
  BrookRegisterApp(TBrookApplication.Create);

end.
