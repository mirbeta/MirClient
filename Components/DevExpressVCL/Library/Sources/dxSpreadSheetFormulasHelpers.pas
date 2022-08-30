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

unit dxSpreadSheetFormulasHelpers;

{$I cxVer.Inc}

interface

uses
  Types, SysUtils, dxSpreadSheetCore, cxEdit;

type

  { TdxModifyFormulasHelper }

  TdxModifyFormulasHelper = class
  protected
    class function ApplyToColumns(const AFormula: string; const AArea: TRect; ASheet: TdxSpreadSheetTableView): Integer;
    class function ApplyToRows(const AFormula: string; const AArea: TRect; ASheet: TdxSpreadSheetTableView): Integer;
    class function IsCellEmpty(const ARowIndex, AColumnIndex: Integer; ASheet: TdxSpreadSheetTableView): Boolean; inline;
    class function IsNonEmptyCellWithoutCorrespondingFormula(const AFormula: string;
      const ARowIndex, AColumnIndex: Integer; ASheet: TdxSpreadSheetTableView): Boolean;
    class function IsNumericDataInCell(const ARowIndex, AColumnIndex: Integer; ASheet: TdxSpreadSheetTableView): Boolean;
  public
    class procedure ApplyToSelection(const AFormula: string; ASheet: TdxSpreadSheetTableView);
    class function GetBracketsBalance(const AFormula: string): Integer;
    class procedure Insert(const AFormula: string; AEdit: TcxCustomEdit);
  end;

function dxIsOperandSeparator(const AChar: Char): Boolean;
implementation

uses
  Math, dxSpreadSheetTypes, dxSpreadSheetUtils, cxMemo, cxRichEdit, cxTextEdit, dxSpreadSheetCoreStrs, StrUtils,
  cxGeometry, dxHashUtils, cxFormats;

type
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);

function dxIsOperandSeparator(const AChar: Char): Boolean;
var
  ALength: Integer;
  AOperation: TdxSpreadSheetFormulaOperation;
begin
  if (AChar = dxLeftParenthesis) or (AChar = dxLeftArrayParenthesis) or (AChar = dxFormatSettings.ListSeparator) then
    Exit(True);
  for AOperation := Low(AOperation) to High(AOperation) do
  begin
    ALength := Length(dxDefaultOperations[AOperation]);
    if (ALength > 0) and (AChar = dxDefaultOperations[AOperation][ALength]) then
      Exit(True);
  end;
  Result := False;
end;

{ TdxModifyFormulasHelper }

class procedure TdxModifyFormulasHelper.ApplyToSelection(const AFormula: string; ASheet: TdxSpreadSheetTableView);

  function NeedFormulaForColumns(const AArea: TRect): Boolean;
  begin
    Result := AArea.Top < AArea.Bottom;
  end;

  function NeedFormulaForRows(const AArea: TRect): Boolean;
  var
    R: Integer;
  begin
    Result := False;
    for R := AArea.Top to Min(AArea.Bottom - 1, ASheet.Rows.LastIndex) do
    begin
      Result := IsCellEmpty(R, AArea.Right, ASheet);
      if not Result then
        Break;
    end;
    Result := (AArea.Left < AArea.Right) and (Result or (AArea.Top = AArea.Bottom));
  end;

var
  AArea: TRect;
  ACounter: Integer;
  I: Integer;
begin
  ACounter := 0;
  for I := 0 to ASheet.Selection.Count - 1 do
  begin
    AArea := ASheet.Selection.Items[I].Rect;
    if NeedFormulaForColumns(AArea) then
      Inc(ACounter, ApplyToColumns(AFormula, AArea, ASheet));
    if NeedFormulaForRows(AArea) then
      Inc(ACounter, ApplyToRows(AFormula, AArea, ASheet));
  end;
  if ACounter = 0 then
    Insert(AFormula, TdxSpreadSheetTableViewAccess(ASheet).EditingController.Edit);
end;

class function TdxModifyFormulasHelper.GetBracketsBalance(const AFormula: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(AFormula) do
    case AFormula[I] of
      '(': Inc(Result);
      ')': Dec(Result);
    end;
end;

class procedure TdxModifyFormulasHelper.Insert(const AFormula: string; AEdit: TcxCustomEdit);
var
  ACursorPosition: Integer;
  AText: string;
begin
  if AEdit is TcxCustomTextEdit then
  begin
    ACursorPosition := TcxCustomTextEdit(AEdit).SelStart;

    if AEdit is TcxCustomMemo then
      AText := TcxCustomMemo(AEdit).Lines.Text
    else
      AText := TcxCustomTextEdit(AEdit).Text;

    if AEdit is TcxCustomRichEdit then
      TcxCustomRichEdit(AEdit).Properties.PlainText := True;

    if ACursorPosition = 0 then
    begin
      AText := dxDefaultOperations[opEQ];
      ACursorPosition := Length(AText);
    end;

    if not dxIsOperandSeparator(AText[ACursorPosition]) then
    begin
      System.Insert(dxDefaultOperations[opAdd], AText, ACursorPosition + 1);
      Inc(ACursorPosition, Length(dxDefaultOperations[opAdd]));
    end;

    System.Insert(AFormula + dxLeftParenthesis + dxRightParenthesis, AText, ACursorPosition + 1);
    Inc(ACursorPosition, Length(AFormula));
    Inc(ACursorPosition, Length(dxLeftParenthesis));

    if AEdit is TcxCustomMemo then
      TcxCustomMemo(AEdit).Lines.Text := AText
    else
      TcxCustomTextEdit(AEdit).Text := AText;

    TcxCustomTextEdit(AEdit).SelStart := ACursorPosition;
  end;
end;

class function TdxModifyFormulasHelper.ApplyToColumns(
  const AFormula: string; const AArea: TRect; ASheet: TdxSpreadSheetTableView): Integer;

  procedure ExcludeEmptyLeftCells(var AArea: TRect);
  var
    ANeedExclude: Boolean;
    R, C: Integer;
  begin
    for R := AArea.Top to Min(AArea.Bottom, ASheet.Rows.LastIndex) do
    begin
      ANeedExclude := True;
      for C := AArea.Left to Min(AArea.Right, ASheet.Columns.LastIndex) do
        if IsNumericDataInCell(R, C, ASheet) then
        begin
          ANeedExclude := False;
          Break;
        end;

      if ANeedExclude then
        AArea.Top := AArea.Top + 1
      else
        Break;
    end;
  end;

  function BuildFormulaForColumn(AColumnIndex: Integer; const AArea: TRect): string;
  var
    I: Integer;
  begin
    for I := AArea.Top to Min(AArea.Bottom, ASheet.Rows.LastIndex) do
    begin
      if IsNumericDataInCell(I, AColumnIndex, ASheet) then
        Exit(dxDefaultOperations[opEQ] + AFormula + dxLeftParenthesis +
          dxReferenceToString(cxRect(AColumnIndex, AArea.Top, AColumnIndex, Min(AArea.Bottom, dxSpreadSheetMaxRowIndex - 1))) +
          dxRightParenthesis);
    end;
    Result := '';
  end;

  function GetFormulaRowIndex(var AArea: TRect): Integer;
  var
    ANext: Boolean;
    C: Integer;
  begin
    Result := AArea.Bottom;
    repeat
      ANext := False;
      for C := AArea.Left to Min(AArea.Right, ASheet.Columns.LastIndex) do
        if IsNonEmptyCellWithoutCorrespondingFormula(AFormula, Result, C, ASheet) then
        begin
          ANext := True;
          Inc(Result);
          Break;
        end;
    until not ANext;
  end;

var
  ASumArea: TRect;
  ATempFormula: string;
  R, C: Integer;
begin
  Result := 0;
  ASumArea := AArea;
  ExcludeEmptyLeftCells(ASumArea);
  R := GetFormulaRowIndex(ASumArea);
  ASumArea.Bottom := Min(AArea.Bottom, R - 1);
  for C := ASumArea.Left to Min(ASumArea.Right, ASheet.Columns.LastIndex) do
  begin
    ATempFormula := BuildFormulaForColumn(C, ASumArea);
    if ATempFormula <> '' then
    begin
      Inc(Result);
      ASheet.CreateCell(Min(R, dxSpreadSheetMaxRowIndex), C).SetText(ATempFormula, True);
    end;
  end;
end;

class function TdxModifyFormulasHelper.ApplyToRows(
  const AFormula: string; const AArea: TRect; ASheet: TdxSpreadSheetTableView): Integer;

  function BuildFormulaForRow(ARowIndex: Integer; const AArea: TRect): string;
  var
    I: Integer;
  begin
    for I := AArea.Left to Min(AArea.Right, ASheet.Columns.LastIndex) do
    begin
      if IsNumericDataInCell(ARowIndex, I, ASheet) then
        Exit(dxDefaultOperations[opEQ] + AFormula + dxLeftParenthesis +
          dxReferenceToString(cxRect(AArea.Left, ARowIndex, Min(AArea.Right, dxSpreadSheetMaxColumnIndex - 1), ARowIndex)) +
          dxRightParenthesis);
    end;
    Result := '';
  end;

  procedure ExcludeEmptyTopCells(var AARea: TRect);
  var
    ANeedRemove: Boolean;
    R, C: Integer;
  begin
    for C := AArea.Left to Min(AArea.Right, ASheet.Columns.LastIndex) do
    begin
      ANeedRemove := True;
      for R := AArea.Top to Min(AArea.Bottom, ASheet.Rows.LastIndex) do
        if IsNumericDataInCell(R, C, ASheet) then
        begin
          ANeedRemove := False;
          Break;
        end;

      if ANeedRemove then
        AArea.Left := AArea.Left + 1
      else
        Break;
    end;
  end;

  function GetFormulaColumnIndex(var AArea: TRect): Integer;
  var
    ANext: Boolean;
    R: Integer;
  begin
    Result := AArea.Right;
    repeat
      ANext := False;
      for R := AArea.Top to Min(AArea.Bottom, ASheet.Rows.LastIndex) do
        if IsNonEmptyCellWithoutCorrespondingFormula(AFormula, R, Result, ASheet) then
        begin
          ANext := True;
          Inc(Result);
          Break;
        end;
    until not ANext;
  end;

var
  ASumArea: TRect;
  ATempFormula: string;
  R, C: Integer;
begin
  Result := 0;
  ASumArea := AArea;
  ExcludeEmptyTopCells(ASumArea);
  C := GetFormulaColumnIndex(ASumArea);
  ASumArea.Right := Min(AArea.Right, C - 1);
  ASumArea.Bottom := AArea.Bottom;
  for R := ASumArea.Top to Min(ASumArea.Bottom, ASheet.Rows.LastIndex) do
  begin
    ATempFormula := BuildFormulaForRow(R, ASumArea);
    if ATempFormula <> '' then
    begin
      Inc(Result);
      ASheet.CreateCell(R, Min(C, dxSpreadSheetMaxColumnIndex)).SetText(ATempFormula, True);
    end;
  end;
end;

class function TdxModifyFormulasHelper.IsCellEmpty(const ARowIndex, AColumnIndex: Integer;
  ASheet: TdxSpreadSheetTableView): Boolean;
var
  ACell: TdxSpreadSheetCell;
begin
  ACell := ASheet.Cells[ARowIndex, AColumnIndex];
  Result := (ACell = nil) or (ACell.DataType = cdtBlank);
end;

class function TdxModifyFormulasHelper.IsNonEmptyCellWithoutCorrespondingFormula(const AFormula: string;
  const ARowIndex, AColumnIndex: Integer; ASheet: TdxSpreadSheetTableView): Boolean;
var
  ACell: TdxSpreadSheetCell;
begin
  ACell := ASheet.Cells[ARowIndex, AColumnIndex];
  Result := (ACell <> nil) and (ACell.DataType <> cdtBlank) and not
    (ACell.IsFormula and StartsText(dxDefaultOperations[opEQ] + AFormula, ACell.AsFormula.AsText));
end;

class function TdxModifyFormulasHelper.IsNumericDataInCell(const ARowIndex, AColumnIndex: Integer;
  ASheet: TdxSpreadSheetTableView): Boolean;
var
  ACell: TdxSpreadSheetCell;
begin
  ACell := ASheet.Cells[ARowIndex, AColumnIndex];
  Result := (ACell <> nil) and (ACell.DataType in [cdtCurrency, cdtFloat, cdtInteger, cdtFormula]);
end;

end.
