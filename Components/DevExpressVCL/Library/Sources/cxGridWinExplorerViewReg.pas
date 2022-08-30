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

unit cxGridWinExplorerViewReg;

{$I cxVer.inc}

interface

procedure Register;

implementation

uses
  ImgList, DesignEditors, DesignIntf, DesignMenus, VCLEditors,
  dxCoreReg, cxLibraryReg, cxGridReg, Classes, Controls, cxStyles,
  cxGridWinExplorerView, cxPropEditors, cxGridCustomView
  {$IFNDEF NONDB}, cxGridDBWinExplorerView {$ENDIF};

{ TcxGridWinExplorerViewItemSetItemEditor }

type
  TcxGridWinExplorerViewItemSetItemProperty = class(TcxCustomGridTableItemProperty)
  protected
    function InternalGetGridView(APersistent: TPersistent): TcxCustomGridView; override;
  end;

function TcxGridWinExplorerViewItemSetItemProperty.InternalGetGridView(APersistent: TPersistent): TcxCustomGridView;
begin
  Result := TcxGridWinExplorerViewItemSet(APersistent).GridView;
end;

{ TcxGridWinExplorerViewSelectionEditor }

type
  TcxGridWinExplorerViewSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure TcxGridWinExplorerViewSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('cxCustomData');
  Proc('cxGraphics');
end;

{$IFNDEF NONDB}

{ TcxGridDBLayoutViewSelectionEditor }

type
  TcxGridDBLayoutViewSelectionEditor = class(TcxGridWinExplorerViewSelectionEditor)
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
    TcxGridWinExplorerViewItem,
    TcxGridWinExplorerView
  {$IFNDEF NONDB},
    TcxGridDBWinExplorerViewItem,
    TcxGridDBWinExplorerView
  {$ENDIF}
  ]);

  RegisterSelectionEditor(TcxGridWinExplorerView, TcxGridWinExplorerViewSelectionEditor);

  //ItemSet register Item property editor
  RegisterPropertyEditor(TypeInfo(TcxGridWinExplorerViewItem), TcxGridWinExplorerViewItemSet,
    'CheckBoxItem', TcxGridWinExplorerViewItemSetItemProperty);
  RegisterPropertyEditor(TypeInfo(TcxGridWinExplorerViewItem), TcxGridWinExplorerViewItemSet,
    'DescriptionItem', TcxGridWinExplorerViewItemSetItemProperty);
  RegisterPropertyEditor(TypeInfo(TcxGridWinExplorerViewItem), TcxGridWinExplorerViewItemSet,
    'ExtraLargeImageItem', TcxGridWinExplorerViewItemSetItemProperty);
  RegisterPropertyEditor(TypeInfo(TcxGridWinExplorerViewItem), TcxGridWinExplorerViewItemSet,
    'ExtraLargeImageIndexItem', TcxGridWinExplorerViewItemSetItemProperty);
  RegisterPropertyEditor(TypeInfo(TcxGridWinExplorerViewItem), TcxGridWinExplorerViewItemSet,
    'GroupItem', TcxGridWinExplorerViewItemSetItemProperty);
  RegisterPropertyEditor(TypeInfo(TcxGridWinExplorerViewItem), TcxGridWinExplorerViewItemSet,
    'LargeImageItem', TcxGridWinExplorerViewItemSetItemProperty);
  RegisterPropertyEditor(TypeInfo(TcxGridWinExplorerViewItem), TcxGridWinExplorerViewItemSet,
    'LargeImageIndexItem', TcxGridWinExplorerViewItemSetItemProperty);
  RegisterPropertyEditor(TypeInfo(TcxGridWinExplorerViewItem), TcxGridWinExplorerViewItemSet,
    'MediumImageIndexItem', TcxGridWinExplorerViewItemSetItemProperty);
  RegisterPropertyEditor(TypeInfo(TcxGridWinExplorerViewItem), TcxGridWinExplorerViewItemSet,
    'MediumImageItem', TcxGridWinExplorerViewItemSetItemProperty);
  RegisterPropertyEditor(TypeInfo(TcxGridWinExplorerViewItem), TcxGridWinExplorerViewItemSet,
    'SmallImageItem', TcxGridWinExplorerViewItemSetItemProperty);
  RegisterPropertyEditor(TypeInfo(TcxGridWinExplorerViewItem), TcxGridWinExplorerViewItemSet,
    'SmallImageIndexItem', TcxGridWinExplorerViewItemSetItemProperty);
  RegisterPropertyEditor(TypeInfo(TcxGridWinExplorerViewItem), TcxGridWinExplorerViewItemSet,
    'TextItem', TcxGridWinExplorerViewItemSetItemProperty);
end;

end.
