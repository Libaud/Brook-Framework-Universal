(*
  Brook for Free Pascal

  Copyright (C) 2014-2019 Mario Ray Mahardhika

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

{ FCL HTTPApp broker. }

unit BrookFCLHttpAppBroker;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  BrookGlobal, BrookClasses, BrookApplication, BrookLog, BrookRouter, BrookUtils,
  BrookConsts, BrookHTTPConstants, BrookHttpDefsBroker, BrookMessages, HttpDefs,
  CustWeb, CustHttpApp, FPHttpServer, Classes, SysUtils, BrookApplicationInterfaces;

//type
  //TBrookHttpApplication = class;

  { TBrookApplication }

  {TBrookApplication = class(TBrookInterfacedObject, IBrookApplication)
  private
    FApp: TBrookHttpApplication;
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
  end;}

  { TBrookHttpApplication }

  {TBrookHttpApplication = class(TCustomHttpApplication)
  private
    FShowTermMsg: Boolean;
  protected
    function InitializeWebHandler: TWebHandler; override;
  public
    property ShowTermMsg: Boolean read FShowTermMsg write FShowTermMsg;
  end;}

 { TBrookEmbeddedHttpServer }

  {TBrookEmbeddedHttpServer = class(TEmbeddedHttpServer)
  protected
    function CreateRequest: TFPHttpConnectionRequest; override;
    function CreateResponse(
      ARequest: TFPHttpConnectionRequest): TFPHttpConnectionResponse; override;
  end;}


var
  SBrookHttpServerTerminalMsg: string = 'Open the ''%s'' URL in your browser.'
                                        {$IFDEF UNIX} + LineEnding + LineEnding + 'Use [Ctrl+C] to quit ...'{$ENDIF};

function BrookHttpServerTerminalMsg: string;

implementation

{uses
  BrookCGIUtils;}

function BrookHttpServerTerminalMsg: string;
var
  VUrl: string;
begin
  if BrookSettingsInstance.RootUrl = '' then
    VUrl := 'http://localhost'
  else
    VUrl := BrookSettingsInstance.RootUrl;
  if VUrl[Length(VUrl)] = US then
    System.Delete(VUrl, Length(VUrl), 1);
  if not (BrookSettingsInstance.Port in [0, 80]) then
    VUrl += ':' + IntToStr(BrookSettingsInstance.Port);
  Result := Format(SBrookHttpServerTerminalMsg, [VUrl]);
end;

{ TBrookApplication }

{function TBrookApplication.GetTerminated: Boolean;
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
  FApp := TBrookHttpApplication.Create(nil);
  FApp.Initialize;
  FApp.ShowTermMsg := System.IsConsole;
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
  if BrookSettingsInstance.Port <> 0 then
    FApp.Port := BrookSettingsInstance.Port;
  if BrookSettingsInstance.RootUrl <> '' then
    FApp.ApplicationURL := BrookSettingsInstance.RootUrl;
  if FApp.ShowTermMsg then
    WriteLn(BrookHttpServerTerminalMsg);
  FApp.Run;
end;

procedure TBrookApplication.Terminate;
begin
  FApp.Terminate;
end;}

{ TBrookHttpApplication }

{function TBrookHttpApplication.InitializeWebHandler: TWebHandler;
begin
  Result := TBrookHttpServerHandler.Create(Self);
end;}

{ TBrookEmbeddedHttpServer }

{function TBrookEmbeddedHttpServer.CreateRequest: TFPHttpConnectionRequest;
begin
  Result := TBrookHttpConnectionRequest.Create;
end;

function TBrookEmbeddedHttpServer.CreateResponse(
  ARequest: TFPHttpConnectionRequest): TFPHttpConnectionResponse;
begin
  Result := TBrookHttpConnectionResponse.Create(ARequest);
end;}

initialization
  BrookRegisterApp(TBrookApplication.Create);

finalization
  BrookUnregisterApp;

end.
