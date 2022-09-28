unit BrookGActions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BrookActions, BrookactionInterfaces, BrookBasedTypes, BrookHTTPDefs, BrookException, BrookMessages,
  BrookConsts, BrookHTTPConstants;

type
  { Provides features to handle HTTP requests and responses mapping URIs to
    object. }
  generic TBrookGAction<T> = class(TBrookAction)
  private
    FEntity: T;
  protected
    procedure FillEntity; virtual;
    function CreateEntity: T; virtual;
    procedure FreeEntity; virtual;
  public
    { Creates an instance of a @link(TBrookGAction) class. }
    constructor Create(aOwner: TComponent); overload; override;
    { Frees an instance of @link(TBrookGAction) class. }
    destructor Destroy; override;
    { Is triggered by a request of any HTTP method. }
    procedure Request(ARequest: TBrookRequest; AResponse: TBrookResponse); override;
    { Maps URI to object. }
    property Entity: T read FEntity write FEntity;
  end;


implementation

{ TBrookGAction }

constructor TBrookGAction.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FEntity := CreateEntity;
end;

destructor TBrookGAction.Destroy;
begin
  FreeEntity;
  inherited Destroy;
end;

function TBrookGAction.CreateEntity: T;
begin
  Result := T.Create;
end;

procedure TBrookGAction.FreeEntity;
begin
  FreeAndNil(FEntity);
end;

procedure TBrookGAction.Request(ARequest: TBrookRequest;
  AResponse: TBrookResponse);
begin
  case ARequest.Method of
    BROOK_HTTP_REQUEST_METHOD_GET: Get;
    BROOK_HTTP_REQUEST_METHOD_POST:
      begin
        FillEntity;
        Post;
      end;
    BROOK_HTTP_REQUEST_METHOD_PUT:
      begin
        FillEntity;
        Put;
      end;
    BROOK_HTTP_REQUEST_METHOD_PATCH:
      begin
        FillEntity;
        Patch;
      end;
    BROOK_HTTP_REQUEST_METHOD_DELETE:
      begin
        FillEntity;
        Delete;
      end;
    BROOK_HTTP_REQUEST_METHOD_HEAD: Head;
    BROOK_HTTP_REQUEST_METHOD_OPTIONS: Options;
  end;
end;

procedure TBrookGAction.FillEntity;
begin
  GetFields(FEntity);
end;

end.

