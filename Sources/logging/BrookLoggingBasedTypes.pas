unit BrookLoggingBasedTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { Defines an enumerator to represent the logger output kind. }
  TBrookLogOutput = (loFile, loSystem);

  { Defines an enumerator to represent the logger event types. }
  TBrookLogType = (ltCustom, ltInfo, ltWarning, ltError, ltDebug);

  { Defines a set to represent the logger event types. }
  TBrookLogTypes = set of TBrookLogType;

  { Is a type to the log event. }
  TBrookLogEvent = procedure(ASender: TObject; const AType: TBrookLogType;
                             const S: string; const ACode: Word; const E: Exception;
                             var AHandled: Boolean) of object;
  { Defines a pointer to the log event.}
  PBrookLogEvent = ^TBrookLogEvent;


implementation

end.

