{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.DocumentLayout.BottomTextIndentCalculator;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Contnrs, Graphics, Generics.Defaults, Generics.Collections,
  dxRichEdit.Utils.Types,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Core;

type

  { TdxBottomTextIndentCalculatorBase }

  TdxBottomTextIndentCalculatorBase = class abstract
  private
    FRowIndex: Integer;
    FRowSeparatorIndex: Integer;
    FState: TdxTablesControllerTableState;
    FTable: TdxTable;
    function GetBorderCalculator: TdxTableBorderCalculator;
    function GetRow: TdxTableRow;
  strict protected
    constructor Create(AState: TdxTablesControllerTableState; ATable: TdxTable; ARowSeparatorIndex: Integer); overload;
  protected
    function CalculateBottomTextIndentCore(out AHorizontalCellBordersInfo: TdxHorizontalCellBordersInfoList): TdxModelUnit; virtual; abstract;

    property State: TdxTablesControllerTableState read FState;
    property BorderCalculator: TdxTableBorderCalculator read GetBorderCalculator;
    property Table: TdxTable read FTable;
    property Row: TdxTableRow read GetRow;
    property RowIndex: Integer read FRowIndex;
    property RowSeparatorIndex: Integer read FRowSeparatorIndex;
  public
    class function CreateCalculator(AState: TdxTablesControllerTableState; ATable: TdxTable; ARowSeparatorIndex: Integer): TdxBottomTextIndentCalculatorBase; overload;
    function CalculateBottomTextIndent(out AHorizontalCellBordersInfo: TdxHorizontalCellBordersInfoList): TdxLayoutUnit;
  end;

  { TdxNoSpacingBottomTextIndentCalculator }

  TdxNoSpacingBottomTextIndentCalculator = class(TdxBottomTextIndentCalculatorBase)
  protected
    function CalculateBottomTextIndentCore(out AHorizontalCellBordersInfo: TdxHorizontalCellBordersInfoList): TdxModelUnit; override;
    function GetBordersInfo: TdxHorizontalCellBordersInfoList; virtual;
    function GetBottomTextIndentForCell(ABorderInfo: TdxHorizontalCellBordersInfo): TdxModelUnit; virtual;
  end;

  { TdxNoSpacingLastAnchorBottomTextIndentCalculator }

  TdxNoSpacingLastAnchorBottomTextIndentCalculator = class(TdxNoSpacingBottomTextIndentCalculator)
  protected
    function GetBottomTextIndentForCell(ABorderInfo: TdxHorizontalCellBordersInfo): TdxModelUnit; override;
    function GetBordersInfo: TdxHorizontalCellBordersInfoList; override;
  end;

  { TdxSpacingBottomTextIndentCalculator }

  TdxSpacingBottomTextIndentCalculator = class(TdxBottomTextIndentCalculatorBase)
  protected
    function CalculateBottomTextIndentCore(out AHorizontalCellBordersInfo: TdxHorizontalCellBordersInfoList): TdxModelUnit; override;
    function GetMaxBottomBorderCell(ARowIndex: Integer): TdxModelUnit; virtual;
    function GetMaxTopBorderCell(ARowIndex: Integer): TdxModelUnit; virtual;
  end;

implementation

uses
  Math, dxRichEdit.DocumentModel.Borders;

type
  TdxTablesControllerTableStateAccess = class(TdxTablesControllerTableState);

{ TdxBottomTextIndentCalculatorBase }

constructor TdxBottomTextIndentCalculatorBase.Create(AState: TdxTablesControllerTableState; ATable: TdxTable;
  ARowSeparatorIndex: Integer);
begin
  inherited Create;
  FTable := ATable;
  FState := AState;
  FRowSeparatorIndex := ARowSeparatorIndex;
  FRowIndex := Math.Min(ARowSeparatorIndex, ATable.Rows.Count - 1);
end;

class function TdxBottomTextIndentCalculatorBase.CreateCalculator(AState: TdxTablesControllerTableState; ATable: TdxTable;
  ARowSeparatorIndex: Integer): TdxBottomTextIndentCalculatorBase;
var
  ARow: TdxTableRow;
  AHasCellSpacig: Boolean;
  ALastSeparatorIndex, ARowIndex, ACellSpacing: Integer;
begin
  ALastSeparatorIndex := ATable.Rows.Count;
  if ARowSeparatorIndex < ALastSeparatorIndex then
    ARowIndex := ARowSeparatorIndex
  else
    ARowIndex := ALastSeparatorIndex - 1;
  ARow := ATable.Rows[ARowIndex];
  ACellSpacing := TdxTablesControllerTableState.GetActualCellSpacing(ARow);
  AHasCellSpacig := ACellSpacing > 0;
  if AHasCellSpacig then
    Result := TdxSpacingBottomTextIndentCalculator.Create(AState, ATable, ARowSeparatorIndex)
  else
  begin
    if ARowSeparatorIndex < ALastSeparatorIndex then
      Result := TdxNoSpacingBottomTextIndentCalculator.Create(AState, ATable, ARowSeparatorIndex)
    else
      Result := TdxNoSpacingLastAnchorBottomTextIndentCalculator.Create(AState, ATable, ARowSeparatorIndex);
  end;
end;

function TdxBottomTextIndentCalculatorBase.CalculateBottomTextIndent(
  out AHorizontalCellBordersInfo: TdxHorizontalCellBordersInfoList): TdxLayoutUnit;
var
  AModelUnit: TdxModelUnit;
begin
  AModelUnit := CalculateBottomTextIndentCore(AHorizontalCellBordersInfo);
  Result := State.UnitConverter.ToLayoutUnits(AModelUnit);
end;

function TdxBottomTextIndentCalculatorBase.GetBorderCalculator: TdxTableBorderCalculator;
begin
  Result := TdxTablesControllerTableStateAccess(State).FBorderCalculator;
end;

function TdxBottomTextIndentCalculatorBase.GetRow: TdxTableRow;
begin
  Result := Table.Rows[RowIndex];
end;

{ TdxNoSpacingBottomTextIndentCalculator }

function TdxNoSpacingBottomTextIndentCalculator.CalculateBottomTextIndentCore(
  out AHorizontalCellBordersInfo: TdxHorizontalCellBordersInfoList): TdxModelUnit;
var
  I: Integer;
  AMaxIndent, ABottomTextIndent: TdxModelUnit;
  ABorderInfo: TdxHorizontalCellBordersInfo;
  ABordersInfo: TdxHorizontalCellBordersInfoList;
begin
  AMaxIndent := 0;
  ABordersInfo := GetBordersInfo;
  AHorizontalCellBordersInfo := ABordersInfo;
  for I := 0 to ABordersInfo.Count - 1 do
  begin
    ABorderInfo := ABordersInfo[I];
    if ABorderInfo.Border = nil then
      Continue;
    ABottomTextIndent := GetBottomTextIndentForCell(ABorderInfo);
    AMaxIndent := Math.Max(ABottomTextIndent, AMaxIndent);
  end;
  Result := AMaxIndent;
end;

function TdxNoSpacingBottomTextIndentCalculator.GetBordersInfo: TdxHorizontalCellBordersInfoList;
begin
  Result := TdxTablesControllerTableStateAccess(State).FHorizontalBordersCalculator.GetTopBorders(Row);
end;

function TdxNoSpacingBottomTextIndentCalculator.GetBottomTextIndentForCell(
  ABorderInfo: TdxHorizontalCellBordersInfo): TdxModelUnit;
var
  ACell: TdxTableCell;
  ABorder: TdxBorderBase;
  ABorderHeight, ACellTopMargin: TdxModelUnit;
begin
  ACell := ABorderInfo.BelowCell;
  ABorder := ABorderInfo.Border;

  ABorderHeight := BorderCalculator.GetActualWidth(ABorder);

  ACellTopMargin := State.GetActualCellTopMargin(ACell);
  Result := ABorderHeight + ACellTopMargin;
end;

{ TdxNoSpacingLastAnchorBottomTextIndentCalculator }

function TdxNoSpacingLastAnchorBottomTextIndentCalculator.GetBordersInfo: TdxHorizontalCellBordersInfoList;
begin
  Result := TdxTablesControllerTableStateAccess(State).FHorizontalBordersCalculator.GetBottomBorders(Row);
end;

function TdxNoSpacingLastAnchorBottomTextIndentCalculator.GetBottomTextIndentForCell(
  ABorderInfo: TdxHorizontalCellBordersInfo): TdxModelUnit;
var
  ACell: TdxTableCell;
begin
  ACell := ABorderInfo.AboveCell;
  Result := BorderCalculator.GetActualWidth(ACell.GetActualBottomCellBorder);
end;

{ TdxSpacingBottomTextIndentCalculator }

function TdxSpacingBottomTextIndentCalculator.CalculateBottomTextIndentCore(
  out AHorizontalCellBordersInfo: TdxHorizontalCellBordersInfoList): TdxModelUnit;
var
  AMaxBottomBorderCell, AMaxTopBorderCell, ACellSpacing: TdxModelUnit;
begin
  AMaxBottomBorderCell := GetMaxBottomBorderCell(RowSeparatorIndex - 1);
  AMaxTopBorderCell := GetMaxTopBorderCell(RowSeparatorIndex);
  ACellSpacing := TdxTablesControllerTableState.GetActualCellSpacing(Row);
  AHorizontalCellBordersInfo := nil;
  Result := AMaxBottomBorderCell + AMaxTopBorderCell + ACellSpacing;
end;

function TdxSpacingBottomTextIndentCalculator.GetMaxBottomBorderCell(ARowIndex: Integer): TdxModelUnit;
var
  I: Integer;
  ARow: TdxTableRow;
  ACell: TdxTableCell;
  ACells: TdxTableCellCollection;
  ABorderStyle: TdxBorderLineStyle;
  AMaxBorder, ABottomTextIndent: TdxModelUnit;
begin
  if ARowIndex < 0 then
  begin
    ABorderStyle := Table.GetActualTopBorder.Style;
    if (ABorderStyle = TdxBorderLineStyle.None) or (ABorderStyle = TdxBorderLineStyle.&Nil) or (ABorderStyle = TdxBorderLineStyle.Disabled) then
      Exit(0)
    else
      Exit(BorderCalculator.GetActualWidth(Table.GetActualTopBorder));
  end;
  Assert(ARowIndex < Table.Rows.Count);
  ARow := Table.Rows[ARowIndex];
  ACells := ARow.Cells;
  AMaxBorder := 0;
  for I := 0 to ACells.Count - 1 do
  begin
    ACell := ACells[I];
    ABottomTextIndent := BorderCalculator.GetActualWidth(ACell.GetActualBottomCellBorder);
    AMaxBorder := Math.Max(ABottomTextIndent, AMaxBorder);
  end;
  Result := AMaxBorder;
end;

function TdxSpacingBottomTextIndentCalculator.GetMaxTopBorderCell(ARowIndex: Integer): TdxModelUnit;
var
  I: Integer;
  ARow: TdxTableRow;
  ACell: TdxTableCell;
  ACells: TdxTableCellCollection;
  ABorderStyle: TdxBorderLineStyle;
  AMaxBorder, ABorderWidth, ATopMargin: TdxModelUnit;
begin
  if ARowIndex >= Table.Rows.Count then
  begin
    ABorderStyle := Table.GetActualBottomBorder.Style;
    if (ABorderStyle = TdxBorderLineStyle.None) or (ABorderStyle = TdxBorderLineStyle.&Nil) or (ABorderStyle = TdxBorderLineStyle.Disabled) then
      Exit(0)
    else
      Exit(BorderCalculator.GetActualWidth(Table.GetActualBottomBorder));
  end;
  Assert(ARowIndex >= 0);
  ARow := Table.Rows[ARowIndex];
  ACells := ARow.Cells;
  AMaxBorder := 0;
  for I := 0 to ACells.Count - 1 do
  begin
    ACell := ACells[I];
    ABorderWidth := BorderCalculator.GetActualWidth(ACell.GetActualTopCellBorder);
    ATopMargin := State.GetActualCellTopMargin(ACell);
    AMaxBorder := Math.Max(ABorderWidth + ATopMargin, AMaxBorder);
  end;
  Result := AMaxBorder;
end;

end.
