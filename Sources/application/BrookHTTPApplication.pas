unit BrookHTTPApplication;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustHTTPApp, CustWeb;

type
  { TBrookHttpApplication }

  TBrookHTTPApplication = class(TCustomHTTPApplication)
  private
    FShowTermMsg: Boolean;
  protected
    function InitializeWebHandler: TWebHandler; override;
  public
    property ShowTermMsg: Boolean read FShowTermMsg write FShowTermMsg;
  end;

implementation

{ TBrookHttpApplication }

function TBrookHTTPApplication.InitializeWebHandler: TWebHandler;
begin
  { Todo -o Frédéric : Implementation }
  {Result := TBrookHttpServerHandler.Create(Self);}
end;

end.

