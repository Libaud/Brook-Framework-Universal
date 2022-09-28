unit BrookActionInterfaces;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

{$I BrookDefines.inc}

interface

uses
  Classes, SysUtils, BrookBasedTypes, BrookHTTPDefs
  {, BrookException, BrookMessages,
  BrookConsts, BrookHTTPConstants};

type
  IBrookAction = interface
      // Method's
      { Fills the @link(Variables) with the registered variables passed through
        the URL. }
      procedure DoFillVariables(const ANames, AValues: TBrookArrayOfString); virtual;
      { Fills the @link(Variables) with the registered variables passed one by one
        through the URL. }
      procedure DoFillingVariables(const AIndex: Integer; const ANames, AValues: TBrookArrayOfString); virtual;
      { Creates a cookie. }
      { Calls the method @link(TBrookAction.Request). }
      procedure DoRequest(ARequest: TBrookRequest; AResponse: TBrookResponse); virtual;
      // Access Method's
      function GetFiles: TBrookUploadedFiles;
      function GetFields: TStrings;
      function GetField(const AName: string): string;
      function GetMethod: string;
      function GetParams: TStrings;
      function GetParam(const AName: string): string;
      function GetVariables: TStrings;
      function GetVariable(const AName: string): string;
      procedure SetField(const AName: string; const AValue: string);
      procedure SetParam(const AName: string; const AValue: string);
      procedure SetVariable(const AName: string; const AValue: string);
      function GetHTTPRequest: TBrookRequest;
      function GetHTTPResponse: TBrookResponse;
      // Properties
      { Handles the fields of a form. }
      property Field[const AName: string]: string read GetField
                                                  write SetField;
      { Handles the Query_String parameters of a URL. }
      property Param[const AName: string]: string read GetParam
                                                       write SetParam;
      { Handles variables from a parametrized URL. }
      property Variable[const AName: string]: string read GetVariable
                                                     write SetVariable;
      { Handles a file list of fields of a form. }
      property Files: TBrookUploadedFiles read GetFiles;
      { Handles a string list of fields of a form. }
      property Fields: TStrings read GetFields;
      { Handles a string list of the Query_String parameters of a URL. }
      property Params: TStrings read GetParams;
      { Handles a string list of variables from a parametrized URL. }
      property Variables: TStrings read GetVariables;
      { Returns the HTTP request method. }
      property Method: string read GetMethod;
      { Provides services related to HTTP requests drived to a webserver. }
      property HTTPRequest: TBrookRequest read GetHTTPRequest;
      { Provides services related to the HTTP responses comming back from a
        webserver. }
      property HTTPResponse: TBrookResponse read GetHTTPResponse;
    end;

implementation

end.

