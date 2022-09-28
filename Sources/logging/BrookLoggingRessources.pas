unit BrookLoggingRessources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  { Error message @code('Empty output name.'). }
  SBrookEmptyOutputName = 'Empty output name.';
  { Error message @code('Active output log.'). }
  SBrookActiveOutput = 'Active output log.';
  { Error message @code('Inactive output log.'). }
  SBrookInactiveOutput = 'Inactive output log.';
  { Error message @code('Invalid output class: <class-name>.'). }
  SBrookInvalidOutputClass = 'Invalid output class: %s.';
  { Error message @code('Unknown output name: <output-name>.'). }
  SBrookUnknownOutputName = 'Unknown output name: %s.';
  { Name for information log level. }
  SBrookLevelInfo = 'INFO';
  { Name for hint log level. }
  SBrookLevelHint = 'HINT';
  { Name for warning log level. }
  SBrookLevelWarn = 'WARN';
  { Name for debug log level. }
  SBrookLevelDebug = 'DEBUG';
  { Name for error log level. }
  SBrookLevelError = 'ERROR';

implementation

end.

