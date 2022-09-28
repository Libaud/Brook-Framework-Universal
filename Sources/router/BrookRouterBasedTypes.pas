unit BrookRouterBasedTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BrookBasedTypes, BrookActionBasedClasses, BrookActionInterfaces, BrookHTTPDefs;

type
  { Defines a route item. }
  TBrookRoute = record
    { Specifies the class of the action to be called. }
    ActionClass: TBrookActionClass;
    { Checks if the action is default. }
    Default: Boolean;
    { Specifies a HTTP request method of the action to be called. }
    Method: TBrookRequestMethod;
    { Specifies the patter of the action to be called. }
    Pattern: string;
  end;
  { Defines a pointer to the route item.}
  PBrookRoute = ^TBrookRoute;

  { Is a type to @code(*MatchPattern) event. }
  TBrookMatchPatternEvent = function(ASender: TObject; APattern, APathInfo: string; out ARedirect: Boolean;
                                     out ANames, AValues: TBrookArrayOfString; var AHandled: Boolean): Boolean of object;

  { Defines a pointer to the match pattern event.}
  PBrookMatchPatternEvent = ^TBrookMatchPatternEvent;

  { Is a type to @code(*Route) event. }
  TBrookRouteEvent = procedure(ASender: TObject; ARequest: TBrookRequest;
                               AResponse: TBrookResponse; var AHandled: Boolean) of object;
  { Defines a pointer to the route event.}
  PBrookRouteEvent = ^TBrookRouteEvent;

  { Is a type to @code(*ExecuteAction) event. }
  TBrookExecuteActionEvent = procedure(ASender: TObject; AAction: IBrookAction;
    ARequest: TBrookRequest; AResponse: TBrookResponse; const ANames,
    AValues: TBrookArrayOfString; ARoute: TBrookRoute;
    var AHandled: Boolean) of object;
  { Defines a pointer to the execute action event.}
  PBrookExecuteActionEvent = ^TBrookExecuteActionEvent;


implementation

end.

