{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit cxGridLayoutViewReg;

{$I cxVer.inc}

interface

procedure Register;

implementation

uses
  ImgList, DesignEditors, DesignIntf, DesignMenus, VCLEditors,
  dxCoreReg, cxLibraryReg, cxGridReg, Classes, Controls, cxStyles, cxGridLayoutView,
  cxLayoutViewEditor, cxPropEditors
  {$IFNDEF NONDB}, cxGridDBLayoutView {$ENDIF};

{ TcxGridLayoutViewSelectionEditor }

type
  TcxGridLayoutViewSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure TcxGridLayoutViewSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('cxStyles');
  Proc('cxCustomData');
  Proc('cxGraphics');
end;

{$IFNDEF NONDB}

{ TcxGridDBLayoutViewSelectionEditor }

type
  TcxGridDBLayoutViewSelectionEditor = class(TcxGridLayoutViewSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure TcxGridDBLayoutViewSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
{$IFDEF DELPHI16}
  Proc('Data.DB');
{$ELSE}
  Proc('DB');
{$ENDIF}
  Proc('cxDBData');
end;

{$ENDIF}

procedure Register;
begin
  ForceDemandLoadState(dlDisable);

  RegisterNoIcon([
    TcxGridLayoutItem,
    TcxGridLayoutViewItem,
    TcxGridLayoutView
  {$IFNDEF NONDB},
    TcxGridDBLayoutView,
    TcxGridDBLayoutViewItem
  {$ENDIF}
  ]);

  RegisterPropertyEditor(TypeInfo(TcxGridLayoutItem), TcxGridLayoutViewItem, 'LayoutItem', TClassProperty);
  RegisterSelectionEditor(TcxGridLayoutView, TcxGridLayoutViewSelectionEditor);
{$IFNDEF NONDB}
  RegisterSelectionEditor(TcxGridDBLayoutView, TcxGridDBLayoutViewSelectionEditor);
{$ENDIF}
  HideClassProperties(TcxGridLayoutItem, ['LayoutLookAndFeel', 'Name']);
end;

initialization
  GroupDescendentsWith(TcxGridLayoutItem, TControl);

  RegisterStyleSheetClass(TcxGridLayoutViewStyleSheet);

finalization
  UnregisterStyleSheetClass(TcxGridLayoutViewStyleSheet);

end.
