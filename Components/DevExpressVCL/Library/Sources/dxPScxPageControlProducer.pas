{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxPScxPageControlProducer;

interface

{$I cxVer.inc}

uses
  Classes, Controls, dxPSContainerLnk, cxPC;

type
  TdxPScxTabControlProducer = class(TdxPSRootContainerProducer)
  public
    function Control: TcxTabControl; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPScxPageControlIterator = class(TdxPSWinControlIterator)
  private
    function GetPageControl: TcxPageControl;
  protected
    function GetControl(Index: Integer): TControl; override;
    function GetControlCount: Integer; override;
  public
    property PageControl: TcxPageControl read GetPageControl;
  end;

  TdxPScxTabSheetProducer = class(TdxPSRootContainerProducer)
  public
    function Control: TcxTabSheet; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPScxPageControlProducer = class(TdxPSRootContainerProducer)
  protected
    function CanProcessChild(AChildControl: TControl): Boolean; override;
    class function IteratorClass: TdxPSWinControlIteratorClass; override;
  public
    function Control: TcxPageControl; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

implementation

{ TdxPScxTabControlProducer }

function TdxPScxTabControlProducer.Control: TcxTabControl;
begin
  Result := inherited Control as TcxTabControl;
end;

class function TdxPScxTabControlProducer.ControlClass: TControlClass;
begin
  Result := TcxTabControl;
end;

{ TdxPScxPageControlIterator }

function TdxPScxPageControlIterator.GetControl(Index: Integer): TControl;
begin
  Result := PageControl.ActivePage;
end;

function TdxPScxPageControlIterator.GetControlCount: Integer;
begin
  Result := Ord(PageControl.PageCount > 0);
end;

function TdxPScxPageControlIterator.GetPageControl: TcxPageControl;
begin
  Result := TcxPageControl(Control);
end;

{ TdxPScxTabSheetProducer }

function TdxPScxTabSheetProducer.Control: TcxTabSheet;
begin
  Result := inherited Control as TcxTabSheet;
end;

class function TdxPScxTabSheetProducer.ControlClass: TControlClass;
begin
  Result := TcxTabSheet;
end;

{ TdxPScxPageControlProducer }

function TdxPScxPageControlProducer.Control: TcxPageControl;
begin
  Result := inherited Control as TcxPageControl;
end;

class function TdxPScxPageControlProducer.ControlClass: TControlClass;
begin
  Result := TcxPageControl;
end;

function TdxPScxPageControlProducer.CanProcessChild(AChildControl: TControl): Boolean;
begin
  Result := AChildControl = Control.ActivePage;
end;

class function TdxPScxPageControlProducer.IteratorClass: TdxPSWinControlIteratorClass;
begin
  Result := TdxPScxPageControlIterator;
end;

procedure RegisterAssistants;
begin
  TdxPScxTabControlProducer.Register;
  TdxPScxTabSheetProducer.Register;
  TdxPScxPageControlProducer.Register;
end;

procedure UnregisterAssistants;
begin
  TdxPScxPageControlProducer.Unregister;
  TdxPScxTabSheetProducer.Unregister;
  TdxPScxTabControlProducer.Unregister;
end;

initialization
  RegisterAssistants;
  dxPSContainerLnk.dxPSRegisterContainers([TcxTabSheet, TcxTabControl, TcxPageControl]);

finalization
  dxPSContainerLnk.dxPSUnregisterContainers([TcxTabSheet, TcxTabControl, TcxPageControl]);
  UnregisterAssistants;

end.
