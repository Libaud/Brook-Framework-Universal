unit BrookGlobal;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, BrookSettings, BrookConsts, BrookHTTPConstants;

var
  { Global variable to store Brook settings. }
  BrookSettingsInstance: TBrookSettings = (
    Mapped: False;
    Charset: BROOK_HTTP_CHARSET_UTF_8;
    ContentType: BROOK_HTTP_CONTENT_TYPE_TEXT_HTML;
    Page404: BROOK_HTTP_RESPONSE_TEMPLATE_NOT_FOUND;
    Page404File: ES;
    Page500: BROOK_HTTP_RESPONSE_TEMPLATE_INTERNAL_SERVER_ERROR;
    Page500File: ES;
    DirectoryForUploads: ES;
    DeleteUploadedFiles: False;
    KeepUploadedNames: True;
    Configuration: ES;
    RootUrl: ES;
    Port: 0;
    LogActive: False;
    LogFile: ES;
    OnError: nil;
  );

implementation

end.

