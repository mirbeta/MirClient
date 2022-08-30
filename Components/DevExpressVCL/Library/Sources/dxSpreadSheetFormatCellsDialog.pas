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

unit dxSpreadSheetFormatCellsDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, Controls, Graphics, dxCoreClasses, cxLookAndFeelPainters,
  // SpreadSheet
  dxSpreadSheetCore,
  dxSpreadSheetCoreStyles,
  dxSpreadSheetCellStyleEditDialog,
  dxSpreadSheetCellStyleEditDialogController,
  dxSpreadSheetCellStyleEditDialogHelpers,
  dxSpreadSheetCoreHelpers,
  dxSpreadSheetGraphics,
  dxSpreadSheetNumberFormat,
  dxSpreadSheetStrs,
  dxSpreadSheetStyles,
  dxSpreadSheetTypes;

const
  sknfGeneral    = 0;
  sknfNumber00   = 1;
  sknfTime       = 2;
  sknfDate       = 3;
  sknfCurrency   = 4;
  sknfPercent    = 5;
  sknfScientific = 6;

type

  { TdxSpreadSheetFormatCellsDialogController }

  TdxSpreadSheetFormatCellsDialogController = class(TdxSpreadSheetCellStyleEditDialogCustomController)
  strict private
    FAreas: TdxRectList;
    FSheet: TdxSpreadSheetTableView;
  protected
    function GetCellStyles: TdxSpreadSheetCellStyles; override;
  public
    constructor Create(ASheet: TdxSpreadSheetTableView); virtual;
    destructor Destroy; override;
    procedure EnumCellStyles(AProcRef: TdxSpreadSheetCellStyleEnumProcRef; AEnumDefaultStyles: Boolean); override;
    function FormatFocusedCellValue(const AFormatCode: string): TdxSpreadSheetNumberFormatResult; override;
    function HasMultipleCellsAreaAtHorz: Boolean; override;
    function HasMultipleCellsAreaAtVert: Boolean; override;
    // Merging
    function CanMergeAreas: Boolean; override;
    function GetMergeAreasInfo: TcxCheckBoxState; override;
    procedure MergeAreas; override;
    procedure UnmergeAreas; override;
    // Saving
    procedure BeginSaving; override;
    procedure EndSaving; override;
    procedure PrepareToSave; override;
    //
    property Areas: TdxRectList read FAreas;
    property Sheet: TdxSpreadSheetTableView read FSheet;
  end;

  { TdxSpreadSheetCellStylesHelper }

  TdxSpreadSheetCellStylesHelper = class
  strict private
    class procedure SetBorderAroundLine(ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect);
    class procedure SetBorderNone(ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect);
  protected
    class procedure SaveCellStyles(ASheet: TdxSpreadSheetTableView; AProcRef: TdxSpreadSheetCellStyleEnumProcRef);
  public
    class procedure SaveBorder(ASheet: TdxSpreadSheetTableView; AIsBorder: Boolean);
    class procedure SaveFontStyle(ASheet: TdxSpreadSheetTableView; AFontStyle: TFontStyle);
    class procedure SaveNumberFormat(ASheet: TdxSpreadSheetTableView; AFormat: Integer);
  end;

procedure ShowFormatCellsDialog(ASheet: TdxSpreadSheetTableView; const AActivePage: Integer = -1);
implementation

uses
  Math, Forms, Variants, SysUtils, dxTypeHelpers, cxGeometry, cxGraphics, dxHashUtils, dxSpreadSheetCoreStrs;

type
  TdxSpreadSheetHistoryAccess = class(TdxSpreadSheetHistory);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);

procedure ShowFormatCellsDialog(ASheet: TdxSpreadSheetTableView; const AActivePage: Integer = -1);
var
  ADialog: TdxSpreadSheetCellStyleEditDialogForm;
begin
  ADialog := TdxSpreadSheetCellStyleEditDialogForm.Create(ASheet.SpreadSheet, AActivePage);
  try
    ADialog.Initialize(TdxSpreadSheetFormatCellsDialogController.Create(ASheet));
    ADialog.Load;
    if ADialog.ShowModal = mrOk then
      ADialog.Save;
  finally
    ADialog.Free;
  end;
end;

{ TdxSpreadSheetFormatCellsDialogController }

constructor TdxSpreadSheetFormatCellsDialogController.Create(ASheet: TdxSpreadSheetTableView);
var
  I: Integer;
begin
  inherited Create;
  FSheet := ASheet;
  FAreas := TdxRectList.Create;

  if ASheet.Selection.Count > 1 then
  begin
    for I := 0 to ASheet.Selection.Count - 1 do
      Areas.Add(ASheet.Selection.Items[I].Rect);
  end
  else
    Areas.Add(ASheet.Selection.Area);

  if Sheet.OptionsProtection.&Protected then
    Exclude(FCapabilities, csecProtection);
end;

destructor TdxSpreadSheetFormatCellsDialogController.Destroy;
begin
  FreeAndNil(FAreas);
  inherited Destroy;
end;

procedure TdxSpreadSheetFormatCellsDialogController.EnumCellStyles(
  AProcRef: TdxSpreadSheetCellStyleEnumProcRef; AEnumDefaultStyles: Boolean);
var
  AHelper: TdxSpreadSheetTableViewEnumCellStylesHelper;
  I: Integer;
begin
  AHelper := TdxSpreadSheetTableViewEnumCellStylesHelper.Create(Sheet);
  try
    AHelper.EnumDefaultStyles := AEnumDefaultStyles;
    for I := 0 to Areas.Count - 1 do
      AHelper.ProcessArea(Areas[I], AProcRef);
  finally
    AHelper.Free;
  end;
end;

function TdxSpreadSheetFormatCellsDialogController.FormatFocusedCellValue(const AFormatCode: string): TdxSpreadSheetNumberFormatResult;
var
  ACell: TdxSpreadSheetCell;
  ANumberFormatter: TdxSpreadSheetNumberFormat;
begin
  ANumberFormatter := TdxSpreadSheetNumberFormat.Create(AFormatCode, PredefinedFormats.GetIDByFormatCode(AFormatCode));
  try
    ACell := Sheet.Cells[Sheet.Selection.FocusedRow, Sheet.Selection.FocusedColumn];
    if ACell <> nil then
      ANumberFormatter.Format(ACell.AsVariant, ACell.DataType, Sheet.SpreadSheet.FormulaController.FormatSettings, Result)
    else
      ANumberFormatter.Format(Null, cdtBlank, Sheet.SpreadSheet.FormulaController.FormatSettings, Result);
  finally
    ANumberFormatter.Free;
  end;
end;

function TdxSpreadSheetFormatCellsDialogController.CanMergeAreas: Boolean;
begin
  Result := not Sheet.Options.Protected;
end;

function TdxSpreadSheetFormatCellsDialogController.GetMergeAreasInfo: TcxCheckBoxState;
var
  ACell: TdxSpreadSheetMergedCell;
  AHasMerged: Boolean;
  AHasUnmerged: Boolean;
  I: Integer;
begin
  AHasMerged := False;
  AHasUnmerged := False;
  for I := 0 to Areas.Count - 1 do
  begin
    ACell := Sheet.MergedCells.Last;
    while ACell <> nil do
    begin
      if ACell.Intersects(Areas[I]) then
      begin
        AHasMerged := True;
        if not cxRectIsEqual(ACell.Area, Areas[I]) then
          AHasUnmerged := True;
      end;
      if AHasMerged and AHasUnmerged then
        Break;
      ACell := ACell.Prev;
    end;
  end;

  if AHasMerged and AHasUnmerged then
    Result := cbsGrayed
  else
    if AHasMerged then
      Result := cbsChecked
    else
      Result := cbsUnchecked;
end;

function TdxSpreadSheetFormatCellsDialogController.HasMultipleCellsAreaAtHorz: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Areas.Count - 1 do
    if Areas[I].Width > 0 then
    begin
      Result := True;
      Break;
    end;
end;

function TdxSpreadSheetFormatCellsDialogController.HasMultipleCellsAreaAtVert: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Areas.Count - 1 do
    if Areas[I].Height > 0 then
    begin
      Result := True;
      Break;
    end;
end;

procedure TdxSpreadSheetFormatCellsDialogController.MergeAreas;
var
  I: Integer;
begin
  UnmergeAreas;
  for I := 0 to Areas.Count - 1 do
    Sheet.MergedCells.Add(Areas[I]);
end;

procedure TdxSpreadSheetFormatCellsDialogController.BeginSaving;
begin
  inherited;
  TdxSpreadSheetTableViewAccess(Sheet).History.BeginAction(TdxSpreadSheetHistoryFormatCellAction);
  TdxSpreadSheetTableViewAccess(Sheet).BeginUpdate;
end;

procedure TdxSpreadSheetFormatCellsDialogController.EndSaving;
begin
  inherited;
  TdxSpreadSheetTableViewAccess(Sheet).Pack;
  TdxSpreadSheetTableViewAccess(Sheet).EndUpdate;
  TdxSpreadSheetTableViewAccess(Sheet).History.EndAction;
end;

procedure TdxSpreadSheetFormatCellsDialogController.PrepareToSave;
var
  AHelper: TdxSpreadSheetTableViewEnumCellStylesHelper;
  I: Integer;
begin
  AHelper := TdxSpreadSheetTableViewEnumCellStylesHelper.Create(Sheet);
  try
    for I := 0 to Areas.Count - 1 do
      AHelper.PrepareToSave(Areas[I]);
  finally
    AHelper.Free;
  end;
end;

procedure TdxSpreadSheetFormatCellsDialogController.UnmergeAreas;
var
  I: Integer;
begin
  for I := 0 to Areas.Count - 1 do
    Sheet.MergedCells.DeleteItemsInArea(Areas[I]);
end;

function TdxSpreadSheetFormatCellsDialogController.GetCellStyles: TdxSpreadSheetCellStyles;
begin
  Result := Sheet.SpreadSheet.CellStyles;
end;

{ TdxSpreadSheetCellStylesHelper }

class procedure TdxSpreadSheetCellStylesHelper.SaveCellStyles(
  ASheet: TdxSpreadSheetTableView; AProcRef: TdxSpreadSheetCellStyleEnumProcRef);
var
  AController: TdxSpreadSheetFormatCellsDialogController;
begin
  ASheet.BeginUpdate;
  try
    AController := TdxSpreadSheetFormatCellsDialogController.Create(ASheet);
    try
      TdxSpreadSheetHistoryAccess(TdxSpreadSheetTableViewAccess(ASheet).History).BeginAction(TdxSpreadSheetHistoryFormatCellAction);
      try
        AController.PrepareToSave;
        AController.EnumCellStyles(AProcRef, False);
      finally
        FreeAndNil(AController);
      end;
    finally
      TdxSpreadSheetHistoryAccess(TdxSpreadSheetTableViewAccess(ASheet).History).EndAction;
    end;
  finally
    ASheet.EndUpdate;
  end;
end;

class procedure TdxSpreadSheetCellStylesHelper.SetBorderAroundLine(
  ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect);
var
  ABorder: TcxBorder;
  AStyle: TdxSpreadSheetCellBorderStyle;
begin
  for ABorder := Low(TcxBorder) to High(TcxBorder) do
  begin
    AStyle := sscbsDefault;
    case ABorder of
      bLeft:
        if AArea.Left = AColumn then
          AStyle := sscbsThin;
      bTop:
        if AArea.Top = ARow then
          AStyle := sscbsThin;
      bRight:
        if AArea.Right = AColumn then
          AStyle := sscbsThin;
      bBottom:
        if AArea.Bottom = ARow then
          AStyle := sscbsThin;
    end;
    ACellStyle.Borders[ABorder].Style := AStyle;
    ACellStyle.Borders[ABorder].Color := clDefault;
  end;
end;

class procedure TdxSpreadSheetCellStylesHelper.SetBorderNone(
  ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect);
var
  ABorder: TcxBorder;
begin
  for ABorder := Low(TcxBorder) to High(TcxBorder) do
    ACellStyle.Borders[ABorder].Style := sscbsDefault;
end;

class procedure TdxSpreadSheetCellStylesHelper.SaveBorder(ASheet: TdxSpreadSheetTableView; AIsBorder: Boolean);
begin
  if AIsBorder then
    SaveCellStyles(ASheet, SetBorderAroundLine)
  else
    SaveCellStyles(ASheet, SetBorderNone);
end;

class procedure TdxSpreadSheetCellStylesHelper.SaveFontStyle(ASheet: TdxSpreadSheetTableView; AFontStyle: TFontStyle);
var
  AFocusedStyle: TdxSpreadSheetCellStyle;
begin
  AFocusedStyle := TdxSpreadSheetTableViewAccess(ASheet).GetFocusedCellStyle;
  if AFontStyle in AFocusedStyle.Font.Style then
  begin
    SaveCellStyles(ASheet,
      procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
      begin
        ACellStyle.Font.Style := ACellStyle.Font.Style - [AFontStyle];
      end);
  end
  else
    SaveCellStyles(ASheet,
      procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
      begin
        ACellStyle.Font.Style := ACellStyle.Font.Style + [AFontStyle];
      end);
end;

class procedure TdxSpreadSheetCellStylesHelper.SaveNumberFormat(ASheet: TdxSpreadSheetTableView; AFormat: Integer);
begin
  SaveCellStyles(ASheet,
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    begin
      case AFormat of
        sknfGeneral:
          ACellStyle.DataFormat.FormatCode := dxSpreadSheetGeneralNumberFormat;
        sknfNumber00:
          ACellStyle.DataFormat.FormatCodeID := $02;
        sknfTime:
          ACellStyle.DataFormat.FormatCodeID := $12;
        sknfDate:
          ACellStyle.DataFormat.FormatCodeID := $0E;
        sknfCurrency:
          ACellStyle.DataFormat.FormatCodeID := $05;
        sknfPercent:
          ACellStyle.DataFormat.FormatCodeID := $09;
        sknfScientific:
          ACellStyle.DataFormat.FormatCodeID := $0B;
      end;
    end);
end;

end.
