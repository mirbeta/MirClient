{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid Utils                                 }
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

unit cxGridPopupMenuReg;

{$I cxVer.inc}

interface

procedure Register;

implementation

uses
  DesignIntf, DesignEditors, Types, Windows, Classes, Messages, SysUtils, TypInfo, Dialogs,
  cxGridPopupMenu, cxGridCustomPopupMenu, dxCoreReg, cxGridReg, dxBuiltInPopupMenu, cxClasses;

type

  { TcxGridPopupMenuEditor }

  TcxGridPopupMenuEditor = class(TcxCustomGridEditor);

  { TcxGridPopupMenuProperty }

  TcxGridPopupMenuProperty = class(TComponentProperty)
  private
    FGetValuesStrProc: TGetStrProc;
  protected
    procedure ReceiveComponentNames(const S: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TcxGridPopupMenuSelectionEditor }

  TcxGridPopupMenuSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TComponent), TcxPopupMenuInfo, 'PopupMenu', TcxGridPopupMenuProperty);
  RegisterComponentEditor(TcxGridPopupMenu, TcxGridPopupMenuEditor);
  RegisterComponents(dxCoreLibraryProductPage, [TcxGridPopupMenu]);
  RegisterSelectionEditor(TcxGridPopupMenu, TcxGridPopupMenuSelectionEditor);
end;

{ TcxGridPopupMenuSelectionEditor }

procedure TcxGridPopupMenuSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);
  if not TdxBuiltInPopupMenuAdapterManager.IsActualAdapterStandard then
    Proc(cxGetUnitName(TdxBuiltInPopupMenuAdapterManager.GetActualAdapterClass));
end;

{ TcxGridPopupMenuProperty }

procedure TcxGridPopupMenuProperty.GetValues(Proc: TGetStrProc);
begin
  FGetValuesStrProc := Proc;
  try
    Designer.GetComponentNames(GetTypeData(GetPropType), ReceiveComponentNames);
  finally
    FGetValuesStrProc := nil;
  end;
end;

procedure TcxGridPopupMenuProperty.ReceiveComponentNames(const S: string);
var
  AComponent: TComponent;
begin
  AComponent := Designer.GetComponent(S);
  if Assigned(FGetValuesStrProc) and Assigned(AComponent) and
    (Supports(AComponent, IDoPopup) or (AComponent.ClassName = 'TPopupMenu') or
    (AComponent.ClassName = 'TdxBarPopupMenu')) then
      FGetValuesStrProc(S);
end;

end.
