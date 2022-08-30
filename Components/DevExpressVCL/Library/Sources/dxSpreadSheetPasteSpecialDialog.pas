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

unit dxSpreadSheetPasteSpecialDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxClasses, dxLayoutContainer, dxLayoutControl, dxLayoutLookAndFeels, Menus,
  dxLayoutControlAdapters, StdCtrls, cxButtons, dxSpreadSheetCore, dxSpreadSheetClipboard, dxSpreadSheetClipboardFormats,
  dxSpreadSheetTypes, cxContainer, cxEdit, cxListBox, dxLayoutcxEditAdapters, cxCheckBox, cxLabel, cxRadioGroup, dxForms;

type

  { TdxSpreadSheetPasteSpecialDialogForm }

  TdxSpreadSheetPasteSpecialDialogForm = class(TdxForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    cbColumnWidths: TcxCheckBox;
    cbComments: TcxCheckBox;
    cbFormulas: TcxCheckBox;
    cbSkipBlanks: TcxCheckBox;
    cbStyles: TcxCheckBox;
    cbValues: TcxCheckBox;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    lbFormats: TcxListBox;
    lbPasteOptions: TcxLabel;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    lgPasteFormats: TdxLayoutGroup;
    lgPasteOptions: TdxLayoutGroup;
    liPasteFormat: TdxLayoutItem;
    rbAll: TcxRadioButton;
    rbNumberFormatting: TcxRadioButton;

    procedure lbFormatsDblClick(Sender: TObject);
    procedure PasteOptionsChanged(Sender: TObject);
  strict private
    FSpreadSheet: TdxCustomSpreadSheet;

    function CanPaste: Boolean;
    function GetSelectedFormat: TdxSpreadSheetCustomClipboardFormatClass;
    function GetSelectedPasteOptions: TdxSpreadSheetClipboardPasteOptions;
    function GetTable: TdxSpreadSheetTableView; inline;
  protected
    procedure ApplyLocalizations;
    procedure Initialize(ASpreadSheet: TdxCustomSpreadSheet);
    procedure Paste; overload;
    procedure Paste(AFormat: TdxSpreadSheetCustomClipboardFormatClass; APasteOptions: TdxSpreadSheetClipboardPasteOptions); overload;
    procedure PopulatePasteFormats;
    procedure UpdateControlsState;
    //
    property SelectedFormat: TdxSpreadSheetCustomClipboardFormatClass read GetSelectedFormat;
    property SelectedPasteOptions: TdxSpreadSheetClipboardPasteOptions read GetSelectedPasteOptions;
    property SpreadSheet: TdxCustomSpreadSheet read FSpreadSheet;
    property Table: TdxSpreadSheetTableView read GetTable;
  end;

procedure ShowPasteSpecialDialog(ASpreadSheet: TdxCustomSpreadSheet);
implementation

uses
  cxGeometry, dxSpreadSheetDialogStrs, dxCore;

{$R *.dfm}

type
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);

procedure ShowPasteSpecialDialog(ASpreadSheet: TdxCustomSpreadSheet);
begin
  with TdxSpreadSheetPasteSpecialDialogForm.Create(nil) do
  try
    Initialize(ASpreadSheet);
    if ShowModal = mrOk then
      Paste;
  finally
    Free;
  end;
end;

{ TdxSpreadSheetPasteSpecialDialogForm }

procedure TdxSpreadSheetPasteSpecialDialogForm.ApplyLocalizations;
begin
  Caption := cxGetResourceString(@sdxPasteSpecialDialogCaption);

  btnCancel.Caption := cxGetResourceString(@sdxPasteSpecialDialogButtonCancel);
  btnOk.Caption := cxGetResourceString(@sdxPasteSpecialDialogButtonOK);

  liPasteFormat.Caption := cxGetResourceString(@sdxPasteSpecialDialogPasteFormat);
  lbPasteOptions.Caption := cxGetResourceString(@sdxPasteSpecialDialogPasteOptions);

  cbColumnWidths.Caption := cxGetResourceString(@sdxPasteSpecialDialogPasteColumnWidths);
  cbComments.Caption := cxGetResourceString(@sdxPasteSpecialDialogPasteComments);
  cbFormulas.Caption := cxGetResourceString(@sdxPasteSpecialDialogPasteFormulas);
  cbSkipBlanks.Caption := cxGetResourceString(@sdxPasteSpecialDialogPasteSkinBlanks);
  cbStyles.Caption := cxGetResourceString(@sdxPasteSpecialDialogPasteStyles);
  cbValues.Caption := cxGetResourceString(@sdxPasteSpecialDialogPasteValues);
  rbAll.Caption := cxGetResourceString(@sdxPasteSpecialDialogPasteStylesAll);
  rbNumberFormatting.Caption := cxGetResourceString(@sdxPasteSpecialDialogPasteStylesNumberFormatting);
end;

procedure TdxSpreadSheetPasteSpecialDialogForm.Initialize(ASpreadSheet: TdxCustomSpreadSheet);
begin
  FSpreadSheet := ASpreadSheet;
  lgPasteOptions.Visible := Table.CanPasteFromClipboard([]);
  lgPasteFormats.Visible := not lgPasteOptions.Visible;
  PopulatePasteFormats;
  ApplyLocalizations;
  UpdateControlsState;
end;

procedure TdxSpreadSheetPasteSpecialDialogForm.Paste;
begin
  Paste(SelectedFormat, SelectedPasteOptions);
end;

procedure TdxSpreadSheetPasteSpecialDialogForm.Paste(
  AFormat: TdxSpreadSheetCustomClipboardFormatClass; APasteOptions: TdxSpreadSheetClipboardPasteOptions);
var
  AClipboardArea: TRect;
  ACopyMode: TdxSpreadSheetClipboardCopyMode;
  AViewGUID: string;
begin
  if (AFormat <> nil) and (APasteOptions <> []) then
  begin
    if AFormat.InheritsFrom(TdxSpreadSheetBinaryClipboardFormat) then
    begin
      TdxSpreadSheetBinaryClipboardFormatClass(AFormat).GetInfoFromClipboard(AViewGUID, ACopyMode, AClipboardArea);
      TdxSpreadSheetTableViewAccess(Table).CheckPasteSelection(AClipboardArea, ACopyMode);
    end
    else
    begin
      AClipboardArea := cxInvalidRect;
      ACopyMode := ccmNone;
      AViewGUID := '';
    end;

    ShowHourglassCursor;
    SpreadSheet.BeginUpdate;
    try
      SpreadSheet.History.BeginAction(TdxSpreadSheetHistoryPasteFromClipboardAction);
      try
        TdxSpreadSheetTableViewClipboardHelper.PasteData(Table,
          AFormat.LoadFromClipboard, AClipboardArea, ACopyMode, AViewGUID, APasteOptions);
      finally
        SpreadSheet.History.EndAction;
      end;
    finally
      SpreadSheet.EndUpdate;
      HideHourglassCursor;
    end;
  end;
end;

procedure TdxSpreadSheetPasteSpecialDialogForm.PopulatePasteFormats;
begin
  lbFormats.Items.BeginUpdate;
  try
    TdxSpreadSheetTableViewClipboardHelper.EnumFormatsForPaste(
      procedure (AFormat: TdxSpreadSheetCustomClipboardFormatClass)
      begin
        if AFormat.CanLoadFromClipboard then
          lbFormats.Items.AddObject(AFormat.GetDescription, TObject(AFormat));
      end,
      lgPasteOptions.Visible);
    lbFormats.ItemIndex := 0;
  finally
    lbFormats.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetPasteSpecialDialogForm.UpdateControlsState;
begin
  rbAll.Enabled := cbStyles.Checked;
  rbNumberFormatting.Enabled := cbStyles.Checked;
  cbFormulas.Enabled := cbValues.Checked;
  btnOk.Enabled := CanPaste;
end;

function TdxSpreadSheetPasteSpecialDialogForm.GetSelectedFormat: TdxSpreadSheetCustomClipboardFormatClass;
begin
  Result := TdxSpreadSheetCustomClipboardFormatClass(lbFormats.ItemObject);
end;

function TdxSpreadSheetPasteSpecialDialogForm.GetSelectedPasteOptions: TdxSpreadSheetClipboardPasteOptions;
begin
  if lgPasteFormats.Visible then
    Exit(dxSpreadSheetDefaultPasteOptions);

  Result := [];
  if cbComments.Checked then
    Include(Result, cpoComments);
  if cbValues.Checked then
    Include(Result, cpoValues);
  if cbValues.Checked and cbFormulas.Checked then
    Include(Result, cpoFormulas);
  if cbSkipBlanks.Checked then
    Include(Result, cpoSkipBlanks);
  if cbColumnWidths.Checked then
    Include(Result, cpoColumnWidths);
  if cbStyles.Checked then
  begin
    if rbAll.Checked then
      Include(Result, cpoStyles)
    else
      Include(Result, cpoNumberFormatting);
  end;
end;

function TdxSpreadSheetPasteSpecialDialogForm.GetTable: TdxSpreadSheetTableView;
begin
  Result := SpreadSheet.ActiveSheetAsTable;
end;

function TdxSpreadSheetPasteSpecialDialogForm.CanPaste: Boolean;
begin
  Result := (SelectedFormat <> nil) and (SelectedPasteOptions - [cpoSkipBlanks] <> []);
end;

procedure TdxSpreadSheetPasteSpecialDialogForm.lbFormatsDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TdxSpreadSheetPasteSpecialDialogForm.PasteOptionsChanged(Sender: TObject);
begin
  UpdateControlsState;
end;

end.
