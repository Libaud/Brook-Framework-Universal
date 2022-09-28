(*
  Brook for Free Pascal

  Copyright (C) 2014-2019 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

{ Action classes. }

unit BrookActions;

{$I BrookDefines.inc}

interface

uses
  Classes, SysUtils, BrookActionBasedClasses, BrookActionInterfaces, BrookBasedTypes, BrookHTTPDefs, BrookException, BrookMessages,
  BrookConsts, BrookHTTPConstants;

type

  { Provides features to handle HTTP requests and responses. }
  //TBrookAction = class(TBrookPersistent)
  TBrookAction = class(TBrookActionBased)
  private
    {FFields: TStrings;
    FFiles: TBrookUploadedFiles;
    FParams: TStrings;
    FHttpRequest: TBrookRequest;
    FHttpResponse: TBrookResponse;
    FVariables: TStrings;
    function GetField(const AName: string): string;
    function GetMethod: string;
    function GetParam(const AName: string): string;
    function GetVariable(const AName: string): string;
    procedure SetField(const AName: string; const AValue: string);
    procedure SetParam(const AName: string; const AValue: string);
    procedure SetVariable(const AName: string; const AValue: string);}
  public
    { Creates an instance of a @link(TBrookAction) class. }
    //constructor Create(aOwner: TComponent); override;
    { Creates an instance of a @link(TBrookAction) class passing params to
      request/response. }
    //constructor Create(aOwner: TComponent; ARequest: TBrookRequest; AResponse: TBrookResponse); overload; virtual;
    { Frees an instance of @link(TBrookAction) class. }
    //destructor Destroy; override;
    { Registers an action.

      @param(APattern Is an expression defining which URLs is used to call
       an action. It is possible to use variables inside URLs:

       @definitionList(
        @itemLabel(@bold(:name) -- Represents a variable that spans single URL
         component between slashes.)
        @item(Examples:

         @code(TMyAction.Register('/foo/:myvar');)

         Value of a variable @code("myvar") can be read from the property
         @link(Variables) or @link(Variable), e.g.:

         @code(Write(Variables.Values['myvar']);)

         @code(Write(Variable['myvar']);)

         Any number of variables can be combined:

         @code(TMyAction.Register('/foo/:cat/:id');)
        )
        @itemLabel(@bold(*name) -- Represents a variable that spans one or more
         levels between slashes in the current URL.)
        @item(Examples:

         @code(TMyAction.Register('/home/*path');)

         Any of the following URLs will match:

         http://localhost/cgi-bin/cgi1/home/file @br
         http://localhost/cgi-bin/cgi1/home/dir/file @br
         http://localhost/cgi-bin/cgi1/home/dir/subdir/file etc.

         Variable @code(Variables.Values['path']) will receive @code('file'),
         @code('dir/file') or @code('dir/subdir/file') correspondingly.

         You can also add static text after variable part:

         @code(TMyAction.Register('/home/*path/download');)

         http://localhost/cgi-bin/cgi1/home/dir/file/download -- This will match, @br
         http://localhost/cgi-bin/cgi1/home/dir/file/info -- but not this, because ending is different.

         Multi-level variable can be combined with any number of single-level
         variables in any order:

         @code(TMyAction.Register('/home/user/:uid/file/*fpath/version/:vid/info');)

         @bold(@italic(NOTE:)) Only one multi-level variable can be specified per URL.
        )
        @itemLabel(@bold(url/) -- Adds a slash to the end of the URL if does not exist.)
        @item(Example:

         @code(TMyAction.Register('/foo/');)

         An action can be accessed as
         http://localhost/cgi-bin/cgi1/foo or http://localhost/cgi-bin/cgi1/foo/.
         When called as http://localhost/cgi-bin/cgi1/foo, it will be automatically
         redirected to http://localhost/cgi-bin/cgi1/foo/.
         If the pathinfo is different from @code(/foo) a 404 page is returned;
        )
       )
        @bold(@italic(NOTE:)) Two actions can't be registered with the same
        pattern except when they are called by means of different HTTP methods.
      )

      @param(ADefault A action registered as @italic(Default) will be called
        automatically if the URL does not match with @italic(Pattern) of any
        registered actions. It is not allowed to register more than one action
        as default. A typical example of use is:

        @code(TMyAction.Register('*', True);)) }
    //class procedure Register(const APattern: string; const ADefault: Boolean = False); overload;
    { Registers an action specifying the HTTP request method.

      @param(AMethod Informs the HTTP request method being valid the following
       options: @code(rmAll, rmGet, rmHead, rmOptions, rmPost, rmPut) or
       @code(rmDelete). The only way to register two actions with the same
       pattern is differentiating the value of this parameter.
       If at least one action has this parameter changed, the route mapping is
       enabled in @link(TBrookSettings.Mapped).
       A typical example of use is:

       @longCode(
    procedure TMyAction1.Get;
    begin
      Write('GET');
    end;

    procedure TMyAction1.Put;
    begin
      Write('PUT');
    end;

    procedure TMyAction2.Post;
    begin
      Write('POST');
    end;

  initialization
    TMyAction1.Register('/foo1', rmGet);
    TMyAction1.Register('/foo1', rmPut);
    TMyAction2.Register('/foo1', rmPost);)) }
    class procedure Register(const APattern: string; const AMethod: TBrookRequestMethod; const ADefault: Boolean = False); overload;
    { Returns the path of action. Exemple:

      @code(/cgi-bin/cgi1/myaction). }
    class function GetPath: string;
    { Fills the @link(Variables) with the registered variables passed through
      the URL. }
    //procedure DoFillVariables(const ANames, AValues: TBrookArrayOfString); virtual;
    { Fills the @link(Variables) with the registered variables passed one by one
      through the URL. }
    //procedure DoFillingVariables(const AIndex: Integer; const ANames, AValues: TBrookArrayOfString); virtual;
    { Creates a cookie. }
    {procedure SetCookie(const AName, AValue: string; const AExpires: TDateTime = NullDate; const APath: string = ES;
                        const ADomain: string = ES; const ASecure: Boolean = False; const AHttpOnly: Boolean = False);}
    { Get a cookie value. }
    //function GetCookie(const AName: string): string;
    { Deletes a cookie. }
    //procedure DeleteCookie(const AName: string; const APath: string = ES; const ADomain: string = ES);
    { Calls the method @link(TBrookAction.Request). }
    //procedure DoRequest(ARequest: TBrookRequest; AResponse: TBrookResponse); virtual;
    { Is triggered by a request of any HTTP method. }
    //procedure Request(ARequest: TBrookRequest; AResponse: TBrookResponse); virtual;
    { Get an object with the fields coming from a
        @code(x-www-form-urlencoded) form. }
    //procedure GetFields(AObject: TObject);
    { Get an object with the params coming from a @code(QUERY_STRING). }
    //procedure GetParams(AObject: TObject);
    { Get an object with the variables coming from an URL. }
    //procedure GetVariables(AObject: TObject);
    { Creates an URL for action. }
    //function UrlFor(AActionClass: TBrookActionClass): string; overload;
    { Creates an URL for an action informing an array of parameters. Exemple:

      @longCode(
      procedure TMyAction.Get;
      begin
        // When calling with http://localhost/cgi-bin/cgi1/foo/myvalue
        // the output will be /cgi-bin/cgi1/foo/myvalue
        Write(UrlFor(TMyAction, ['myvalue']));
      end;

      initialization
        TMyAction.Register('/foo/:myvar');) }
    function UrlFor(AActionClass: TBrookActionClass; const AParams: array of string): string; overload;
    { Creates an URL for an action passing an array of parameters however
      informing the class name as string. }
    function UrlFor(AClassName: string;
      const AParams: array of string): string; overload;
    { Creates an URL for an action informing the class name as string. }
    //function UrlFor(AClassName: string): string; overload;
    { Is triggered by a GET HTTP request method. }
    procedure Get; virtual;
    { Is triggered by a POST HTTP request method. }
    procedure Post; virtual;
    { Is triggered by a PUT HTTP request method. }
    procedure Put; virtual;
    { Is triggered by a PATCH HTTP request method. }
    procedure Patch; virtual;
    { Is triggered by a DELETE HTTP request method. }
    procedure Delete; virtual;
    { Is triggered by a HEAD HTTP request method. }
    procedure Head; virtual;
    { Is triggered by an OPTIONS HTTP request method. }
    procedure Options; virtual;
    { Redirects to an URL. }
    //procedure Redirect(const AUrl: string); overload;
    { Redirects to an URL informing the (302, 307) status code. }
    //procedure Redirect(const AUrl: string; const AStatusCode: Word); overload;
    { Redirects to an URL informing the root URL. }
    //procedure Redirect(const AUrl: string; const AUseRootUrl: Boolean); overload;
    { Redirects to an URL informing the (302, 307) status code and the
      @code(ScriptName). }
    procedure Redirect(const AUrl: string; const AUseRootUrl: Boolean;
      const AStatusCode: Word); overload;
    { Raises a message for action exceptions. }
    //procedure Error(const AMsg: string); overload;
    { Raises a formated message for action exceptions. }
    //procedure Error(const AMsg: string; const AArgs: array of const); overload;
    { Stops the action showing an exception message. }
    //procedure Stop(const AMsg: string); overload;
    { Stops the action showing a formatted exception message. }
    //procedure Stop(const AMsg: string; const AArgs: array of const); overload;
    { Writes the content of a file. }
    //procedure Render(const AFileName: TFileName); overload; virtual;
    { Writes the content of a file passing parameters to the output. }
    //procedure Render(const AFileName: TFileName; const AArgs: array of const); overload; virtual;
    { Clears all written content with @code(Write(), WriteLn(), Render()) etc. }
    //procedure Clear;
    { Checks if a name exists in fields. }
    //function Exists(const AName: string): Boolean;
    { Writes a string. }
    //procedure Write(const AString: string); overload;
    { Writes a boolean. }
    //procedure Write(const ABoolean: Boolean); overload;
    { Writes an integer. }
    //procedure Write(const AInteger: Integer); overload;
    { Writes a float. }
    //procedure Write(const AFloat: Double); overload;
    { Writes an object. }
    //procedure Write(AObject: TObject); overload;
    { Writes an object allowing to ignore properties via an array of strings. }
    ///procedure Write(AObject: TObject; const AIgnoredProps: TStrings); overload;
    { Writes an object allowing to ignore properties via a list of strings. }
    //procedure Write(AObject: TObject; const AIgnoredProps: array of string); overload;
    { Writes a content of stream. }
    //procedure Write(AStream: TStream); overload;
    { Writes a formatted string. }
    //procedure Write(const AFmt: string; const AArgs: array of const); overload;
    // Properties
    { Handles the fields of a form. }
    {property Field[const AName: string]: string read GetField
                                                     write SetField;}
    { Handles the Query_String parameters of a URL. }
    {property Param[const AName: string]: string read GetParam write SetParam;}
    { Handles variables from a parametrized URL. }
    {property Variable[const AName: string]: string read GetVariable
      write SetVariable;}
    { Handles a file list of fields of a form. }
    {property Files: TBrookUploadedFiles read FFiles;}
    { Handles a string list of fields of a form. }
    //property Fields: TStrings read FFields;
    { Handles a string list of the Query_String parameters of a URL. }
    //property Params: TStrings read FParams;
    { Handles a string list of variables from a parametrized URL. }
    //property Variables: TStrings read FVariables;
    { Returns the HTTP request method. }
    //property Method: string read GetMethod;
    { Provides services related to HTTP requests drived to a webserver. }
    //property HttpRequest: TBrookRequest read FHttpRequest;
    { Provides services related to the HTTP responses comming back from a webserver. }
    //property HttpResponse: TBrookResponse read FHttpResponse;
  end;

implementation

uses
  BrookGlobal, BrookRouter, BrookActionExceptions, BrookUtils, BrookHTTPUtils;

{ TBrookAction }

class procedure TBrookAction.Register(const APattern: string; const AMethod: TBrookRequestMethod; const ADefault: Boolean);
begin
  if Self = TBrookAction then
    raise
      EBrookAction.Create(Self, SBrookRegiterTBrookActionError);
  if AMethod <> rmAll then
    BrookSettingsInstance.Mapped := True;
  TBrookRouter.Service.Routes.Add(Self, LowerCase(APattern), AMethod, ADefault);
end;

class function TBrookAction.GetPath: string;
begin
  Result := BrookIncludeTrailingUrlDelimiter(TBrookRouter.RootUrl) + LowerCase(Copy(ClassName, 2, MaxInt));
end;

function TBrookAction.UrlFor(AActionClass: TBrookActionClass;
  const AParams: array of string): string;
begin
  Result := TBrookRouter.Service.UrlFor(AActionClass, AParams);
end;

function TBrookAction.UrlFor(AClassName: string;
  const AParams: array of string): string;
begin
  Result := TBrookRouter.Service.UrlFor(AClassName, AParams);
end;

procedure TBrookAction.Get;
begin
  TBrookRouter.MethodNotAllowed(HTTPResponse);
end;

procedure TBrookAction.Post;
begin
  TBrookRouter.MethodNotAllowed(HTTPResponse);
end;

procedure TBrookAction.Put;
begin
  TBrookRouter.MethodNotAllowed(HTTPResponse);
end;

procedure TBrookAction.Patch;
begin
  TBrookRouter.MethodNotAllowed(HTTPResponse);
end;

procedure TBrookAction.Delete;
begin
  TBrookRouter.MethodNotAllowed(HTTPResponse);
end;

procedure TBrookAction.Head;
begin
  TBrookRouter.MethodNotAllowed(HTTPResponse);
end;

procedure TBrookAction.Options;
begin
  TBrookRouter.MethodNotAllowed(HTTPResponse);
end;

procedure TBrookAction.Redirect(const AUrl: string; const AUseRootUrl: Boolean; const AStatusCode: Word);
begin
  HTTPResponse.Code := AStatusCode;
  HTTPResponse.CodeText := BrookStatusCodeToReasonPhrase(AStatusCode);
  if AUseRootUrl then
    HTTPResponse.SetCustomHeader('Location', TBrookRouter.RootUrl(HTTPRequest) + AUrl)
  else
    HTTPResponse.SetCustomHeader('Location', AUrl);
end;

end.
