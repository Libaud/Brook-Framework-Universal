unit BrookHandlersExceptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BrookException, BrookLoggerExceptions;

type
  { Handles exceptions for @link(TBrookConfiguratorHandler). }
  EBrookConfiguratorHandler = class(EBrookConfigurator);

  { Handles exceptions for @link(TBrookFCLEventLogHandler). }
  EBrookFCLEventLogHandler = class(EBrookLogger);

  { Handles exceptions for @link(TBrookMessagesHandler_ptBR). }
  EBrookMessagesHandler_ptBR = class(EBrook);

  { Handles exceptions for @link(TBrookMiddlewareHandler). }
  EBrookMiddlewareHandler = class(EBrookMiddleware);

  { Handles exceptions for @link(TBrookRouterHandler). }
  EBrookRouterHandler = class(EBrookRouter);

  { Handles exceptions for @link(TBrookSession). }
  EBrookSessionHandler = class(EBrookSession);


implementation

end.

