unit BrookEmbeddedHTTPServers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {BrookGlobal, BrookClasses, BrookApplication, BrookLog, BrookRouter, BrookUtils,
  BrookConstants, BrookHTTPConstants, BrookHttpDefsBroker, BrookMessages, HttpDefs,}
  CustHttpApp, FPHttpServer, BrookApplicationInterfaces;

type
 { TBrookEmbeddedHttpServer }

  TBrookEmbeddedHttpServer = class(TEmbeddedHttpServer)
  protected
    function CreateRequest: TFPHttpConnectionRequest; override;
    function CreateResponse(
      ARequest: TFPHttpConnectionRequest): TFPHttpConnectionResponse; override;
  end;


implementation

uses
  BrookHTTPConnections;

{ TBrookEmbeddedHttpServer }

function TBrookEmbeddedHttpServer.CreateRequest: TFPHttpConnectionRequest;
begin
  Result := TBrookHttpConnectionRequest.Create;
end;

function TBrookEmbeddedHttpServer.CreateResponse(
  ARequest: TFPHttpConnectionRequest): TFPHttpConnectionResponse;
begin
  Result := TBrookHttpConnectionResponse.Create(ARequest);
end;

end.

