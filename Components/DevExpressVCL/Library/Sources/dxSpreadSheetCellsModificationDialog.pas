{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetCellsModificationDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutLookAndFeels, cxClasses, dxLayoutContainer, dxLayoutControl,
  dxLayoutControlAdapters, cxButtons, dxLayoutcxEditAdapters, cxContainer, cxEdit, cxLabel, cxRadioGroup,
  dxSpreadSheetTypes, dxSpreadSheetCore, dxForms;

type

  { TdxSpreadSheetCellsModificationDialogForm }

  TdxSpreadSheetCellsModificationDialogFormClass = class of TdxSpreadSheetCellsModificationDialogForm;
  TdxSpreadSheetCellsModificationDialogForm = class(TdxForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    lbCaption: TcxLabel;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMainGroup1: TdxLayoutGroup;
    lcMainItem1: TdxLayoutItem;
    lcMainItem2: TdxLayoutItem;
    lcMainItem3: TdxLayoutItem;
    lcMainItem4: TdxLayoutItem;
    lcMainItem5: TdxLayoutItem;
    lcMainItem6: TdxLayoutItem;
    lcMainItem7: TdxLayoutItem;
    rbShiftColumn: TcxRadioButton;
    rbShiftHorizontally: TcxRadioButton;
    rbShiftRow: TcxRadioButton;
    rbShiftVertically: TcxRadioButton;
  protected
    procedure Initialize(AIsDeletingMode: Boolean); virtual;
    procedure InitializeDialogPosition(ASpreadSheet: TdxCustomSpreadSheet); virtual;
    function GetCellsModification: TdxSpreadSheetCellsModification;
  public
    property CellsModification: TdxSpreadSheetCellsModification read GetCellsModification;
  end;

var
  dxSpreadSheetCellsModificationDialogFormClass: TdxSpreadSheetCellsModificationDialogFormClass = TdxSpreadSheetCellsModificationDialogForm;

function ShowCellsModificationDialog(out ACellsModification: TdxSpreadSheetCellsModification;
  AIsDeletingMode: Boolean; ASpreadSheet: TdxCustomSpreadSheet = nil): Boolean;
implementation

uses
  dxSpreadSheetDialogStrs, Math, cxGeometry;

{$R *.dfm}

function ShowCellsModificationDialog(out ACellsModification: TdxSpreadSheetCellsModification;
  AIsDeletingMode: Boolean; ASpreadSheet: TdxCustomSpreadSheet = nil): Boolean;
var
  ADialogForm: TdxSpreadSheetCellsModificationDialogForm;
begin
  ADialogForm := TdxSpreadSheetCellsModificationDialogFormClass.Create(nil);
  try
    if ASpreadSheet <> nil then
      SetControlLookAndFeel(ADialogForm, ASpreadSheet.DialogsLookAndFeel);
    ADialogForm.Initialize(AIsDeletingMode);
    ADialogForm.InitializeDialogPosition(ASpreadSheet);
    Result := ADialogForm.ShowModal = mrOk;
    if Result then
      ACellsModification := ADialogForm.CellsModification;
  finally
    ADialogForm.Free;
  end;
end;

{ TdxSpreadSheetCellsModificationDialogForm }

procedure TdxSpreadSheetCellsModificationDialogForm.Initialize(AIsDeletingMode: Boolean);
const
  CaptionMap: array[Boolean] of Pointer = (
    @sdxCellsModificationDialogInsertCaption,
    @sdxCellsModificationDialogDeleteCaption
  );
  ShiftHorizontallyMap: array[Boolean] of Pointer = (@sdxShiftCellsRight, @sdxShiftCellsLeft);
  ShiftVerticallyMap: array[Boolean] of Pointer = (@sdxShiftCellsDown, @sdxShiftCellsUp);
begin
  Caption := cxGetResourceString(CaptionMap[AIsDeletingMode]);
  lbCaption.Caption := cxGetResourceString(CaptionMap[AIsDeletingMode]);
  rbShiftColumn.Caption := cxGetResourceString(@sdxShiftColumn);
  rbShiftRow.Caption := cxGetResourceString(@sdxShiftRow);
  rbShiftHorizontally.Caption := cxGetResourceString(ShiftHorizontallyMap[AIsDeletingMode]);
  rbShiftVertically.Caption := cxGetResourceString(ShiftVerticallyMap[AIsDeletingMode]);
  btnCancel.Caption := cxGetResourceString(@sdxCellsModificationDialogButtonCancel);
  btnOk.Caption := cxGetResourceString(@sdxCellsModificationDialogButtonOK);
end;

procedure TdxSpreadSheetCellsModificationDialogForm.InitializeDialogPosition(ASpreadSheet: TdxCustomSpreadSheet);
var
  AMonitor: TMonitor;
  AMonitorBounds: TRect;
  APosition: TPoint;
  ASpreadSheetPositionOnScreen: TRect;
begin
  APosition := GetMouseCursorPos;
  if ASpreadSheet <> nil then
  begin
    ASpreadSheetPositionOnScreen := dxMapWindowRect(ASpreadSheet.Handle, 0, ASpreadSheet.ClientBounds);
    if not PtInRect(ASpreadSheetPositionOnScreen, APosition) then
      APosition := cxRectCenter(ASpreadSheetPositionOnScreen);
  end;

  Dec(APosition.Y, Height div 2);
  Dec(APosition.X, Width div 2);

  AMonitor := Screen.MonitorFromPoint(APosition);
  if AMonitor <> nil then
  begin
    AMonitorBounds := AMonitor.WorkAreaRect;
    APosition.X := Min(AMonitorBounds.Right - Width, Max(AMonitorBounds.Left, APosition.X));
    APosition.Y := Min(AMonitorBounds.Bottom - Height, Max(AMonitorBounds.Top, APosition.Y));
  end;

  SetBounds(APosition.X, APosition.Y, Width, Height);
  Position := poDesigned;
end;

function TdxSpreadSheetCellsModificationDialogForm.GetCellsModification: TdxSpreadSheetCellsModification;
begin
  if rbShiftColumn.Checked then
    Result := cmShiftColumns
  else if rbShiftRow.Checked then
    Result := cmShiftRows
  else if rbShiftVertically.Checked then
    Result := cmShiftCellsVertically
  else
    Result := cmShiftCellsHorizontally;
end;

end.
