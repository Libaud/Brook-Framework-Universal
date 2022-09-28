(*
  Brook for Free Pascal

  Copyright (C) 2014-2019 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

{ Base classes. }

unit BrookClasses;

{$I BrookDefines.inc}

interface

uses
  Classes;

type
  { Is the main object for Brook. }
  TBrookObject = class(TObject)
  end;

  { Is the main class for Brook. }
  TBrookClass = class of TBrookObject;

  { Is the main interfaced object for Brook. }
  TBrookInterfacedObject = class(TInterfacedObject)
  end;

  { Is the main persistent for Brook. }
  TBrookPersistent = class(TPersistent)
  end;

  { Is the main component for Brook. }
  TBrookComponent = class(TComponent)
  end;

implementation

end.
