unit BrookSettings;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, HttpDefs;

type
  { Stores the Brook settings. }

  TOnShowRequestException = procedure (aResponse: TResponse; aException: Exception; aHandled: Boolean) of object;

  TBrookSettings = record
    { Enables the mapping of HTTP request methods. }
    Mapped: Boolean;
    { Set the default application Charset. }
    Charset: ShortString;
    { Set the default application Content-Type. }
    ContentType: ShortString;
    { Set the 404 HTML page. The string will be sent as is. }
    Page404: string;
    { Set the 404 HTML page file. The file content will be sent.
      This has higher precedence than @code(TBrookSettings.Page404)
      so when both are set, this will be processed first and only
      if the file is not found or cannot be read the system will
      fallback to @code(TBrookSettings.Page404) }
    Page404File: string;
    { Set the 500 HTML page. The string will be sent as is. }
    Page500: string;
    { Set the 500 HTML page file. The file content will be sent.
      This has higher precedence than @code(TBrookSettings.Page500)
      so when both are set, this will be processed first and only
      if the file is not found or cannot be read the system will
      fallback to @code(TBrookSettings.Page500) }
    Page500File: string;
    { Set the default directory for uploads. }
    DirectoryForUploads: string;
    { Defines if the temporary uploaded files will be deleted. }
    DeleteUploadedFiles: Boolean;
    { Keeps the original name of the uploaded files. }
    KeepUploadedNames: Boolean;
    { Set a configuration for the application or for its object members. }
    Configuration: string;
    { Set the default root URL. This is used by methods such as
      @code(TBrookAction.UrlFor), @code(TBrookActionHelper.LinkTo),
      @code(TBrookActionHelper.ButtonTo) etc. By default, Brook assumes
      @code(SCRIPT_NAME) as root URL. }
    RootUrl: string;
    { Set the default application port. }
    Port: Word;
    { Enables the application log. }
    LogActive: Boolean;
    { Set a name for the application log file. }
    LogFile: TFileName;
    { Handles the application exceptions. }
    OnError: TOnShowRequestException;
  end;

implementation

end.

