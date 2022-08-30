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

unit dxRichEdit.DocumentModel.VisibleTextFilter.Core;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils,
  dxCore, dxCoreClasses,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Fields.Core;

type
  { IdxVisibleTextFilter }

  IdxVisibleTextFilter = interface
  ['{A992CFEC-50AE-4FBF-B5E0-F5673B9E2A85}']
    function Clone(APieceTable: TdxCustomPieceTable): IdxVisibleTextFilter;
    function IsRunVisible(const ARunIndex: TdxRunIndex): Boolean;
    function GetPrevVisibleRunIndex(ARunIndex: TdxRunIndex): TdxRunIndex;
    function GetRunVisibilityInField(ARunIndex: TdxRunIndex): TdxRunVisibility;
    function GetNextVisibleRunIndex(ARunIndex: TdxRunIndex): TdxRunIndex;
    function FindVisibleRunForward(ARunIndex: TdxRunIndex): TdxRunIndex;
    function FindVisibleParagraphRunForward(ARunIndex: TdxRunIndex): TdxRunIndex;
    function GetNextVisibleLogPosition(const APosition: TdxDocumentModelPosition; ASkipFieldBoundaryRanges: Boolean): TdxDocumentLogPosition; overload;
    function GetNextVisibleLogPosition(const APosition: TdxDocumentModelPosition; ASkipFieldBoundaryRanges, AInsureNextLogPositionVisible: Boolean): TdxDocumentLogPosition; overload;
    function GetPrevVisibleLogPosition(const APosition: TdxDocumentModelPosition; ASkipFieldBoundaryRanges: Boolean): TdxDocumentLogPosition; overload;
    function GetVisibleLogPosition(const APosition: TdxDocumentModelPosition; ASkipFieldBoundaryRanges: Boolean): TdxDocumentLogPosition; overload;
    function GetNextVisibleLogPosition(ALogPosition: TdxDocumentLogPosition; ASkipFieldResultEnd: Boolean): TdxDocumentLogPosition; overload;
    function GetNextVisibleLogPosition(ALogPosition: TdxDocumentLogPosition; ASkipFieldResultEnd, AInsureNextLogPositionVisible: Boolean): TdxDocumentLogPosition; overload;
    function GetPrevVisibleLogPosition(ALogPosition: TdxDocumentLogPosition; ASkipFieldBoundaryRanges: Boolean): TdxDocumentLogPosition; overload;
    function GetVisibleLogPosition(const APosition: TdxDocumentModelPosition): TdxDocumentLogPosition; overload;
  end;

  { TdxVisibleTextFilterBase }

  TdxVisibleTextFilterBase = class(TcxIUnknownObject, IdxVisibleTextFilter)
  strict private
    FPieceTable: TdxCustomPieceTable;
  public
    constructor Create(APieceTable: TdxCustomPieceTable);

    function Clone(APieceTable: TdxCustomPieceTable): IdxVisibleTextFilter; virtual; abstract;

    function IsRunVisible(const ARunIndex: TdxRunIndex): Boolean; virtual;
    function IsRunVisibleCore(const ARunIndex: TdxRunIndex): TdxRunVisibility; virtual;
    function HasVisibleRunInParagraph(ALastParagraphRun: TdxRunIndex): Boolean;
    function IsFieldRunVisibleCore(ARunIndex: TdxRunIndex; AField: TdxField): Boolean;
    function IsFieldRunVisible(ARunIndex: TdxRunIndex; AField: TdxField): TdxRunVisibility; virtual;
    function FindVisibleParagraphRunForward(ARunIndex: TdxRunIndex): TdxRunIndex; virtual;
    function GetNextVisibleRunIndex(ARunIndex: TdxRunIndex): TdxRunIndex; virtual;
    function GetPrevVisibleRunIndex(ARunIndex: TdxRunIndex): TdxRunIndex; virtual;
    function GetRunVisibilityInField(ARunIndex: TdxRunIndex): TdxRunVisibility;
    function FindVisibleRunForward(ARunIndex: TdxRunIndex): TdxRunIndex; virtual;
    function GetNextVisibleLogPosition(const APosition: TdxDocumentModelPosition; ASkipFieldBoundaryRanges: Boolean): TdxDocumentLogPosition; overload;
    function GetNextVisibleLogPosition(const APosition: TdxDocumentModelPosition;
      ASkipFieldBoundaryRanges, AInsureNextLogPositionVisible: Boolean): TdxDocumentLogPosition; overload;
    function IsPositionOnInvisibleCellParagraph(const APosition: TdxDocumentModelPosition): Boolean;
    function IsPositionAfterFieldCodeEndRun(const APosition: TdxDocumentModelPosition): Boolean; virtual;
    function IsPositionBeforeInvisibleFieldCodeEndRun(const APosition: TdxDocumentModelPosition): Boolean; virtual;
    function IsPositionVisible(AFieldCodeStartRunIndex: TdxRunIndex): Boolean; virtual;
    function IsPositionBeforeInvisibleFieldCodeStartRun(const APosition: TdxDocumentModelPosition): Boolean; virtual;
    function IsPositionBeforeFieldResultEndRun(const APosition: TdxDocumentModelPosition): Boolean; virtual;
    function GetNextVisibleLogPositionCore(const APosition: TdxDocumentModelPosition; ABreakAfterFieldResultEndRun: Boolean;
      out AVisibleCharSkipped: Boolean): TdxDocumentModelPosition;
    function GetPrevVisibleLogPosition(const APosition: TdxDocumentModelPosition; ASkipFieldBoundaryRanges: Boolean): TdxDocumentLogPosition; overload; virtual;
    function GetVisibleLogPosition(const APosition: TdxDocumentModelPosition; ASkipFieldBoundaryRanges: Boolean): TdxDocumentLogPosition; overload; virtual;
    function GetPrevVisiblePositionCore(const APosition: TdxDocumentModelPosition; ABreakOnFieldCodeStart: Boolean;
      out AVisibleCharSkipped: Boolean): TdxDocumentModelPosition;
    function GetVisibleLogPosition(const APosition: TdxDocumentModelPosition): TdxDocumentLogPosition; overload; virtual;
    function GetNextVisibleLogPosition(ALogPosition: TdxDocumentLogPosition; ASkipFieldResultEnd: Boolean): TdxDocumentLogPosition; overload; virtual;
    function GetNextVisibleLogPosition(ALogPosition: TdxDocumentLogPosition; ASkipFieldResultEnd, AInsureNextLogPositionVisible: Boolean): TdxDocumentLogPosition; overload;
    function GetPrevVisibleLogPosition(ALogPosition: TdxDocumentLogPosition; ASkipFieldBoundaryRanges: Boolean): TdxDocumentLogPosition; overload; virtual;
    function GetModelPosition(ALogPosition: TdxDocumentLogPosition): TdxDocumentModelPosition; virtual;

    property PieceTable: TdxCustomPieceTable read FPieceTable;
  end;

implementation

uses
  Math,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Tables.Core,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.ParagraphRange;

{ TdxVisibleTextFilterBase }

constructor TdxVisibleTextFilterBase.Create(APieceTable: TdxCustomPieceTable);
begin
  Assert(APieceTable <> nil);
  inherited Create;
  FPieceTable := APieceTable;
end;

function TdxVisibleTextFilterBase.FindVisibleParagraphRunForward(ARunIndex: TdxRunIndex): TdxRunIndex;
var
  ARuns: TdxRunCollection;
  AMaxRunIndex: TdxRunIndex;
begin
  ARuns := PieceTable.Runs;
  AMaxRunIndex := ARuns.Count - 1;
  while (ARunIndex < AMaxRunIndex) and not (IsRunVisible(ARunIndex) and (ARuns[ARunIndex] is TdxParagraphRun)) do
    Inc(ARunIndex);
  Result := ARunIndex;
end;

function TdxVisibleTextFilterBase.FindVisibleRunForward(ARunIndex: TdxRunIndex): TdxRunIndex;
var
  ARuns: TdxRunCollection;
  AMaxRunIndex: TdxRunIndex;
begin
  ARuns := PieceTable.Runs;
  AMaxRunIndex := ARuns.Count - 1;
  while (ARunIndex < AMaxRunIndex) and not IsRunVisible(ARunIndex) do
    Inc(ARunIndex);
  Result := ARunIndex;
end;

function TdxVisibleTextFilterBase.GetNextVisibleLogPosition(const APosition: TdxDocumentModelPosition; ASkipFieldBoundaryRanges: Boolean): TdxDocumentLogPosition;
begin
  Result := GetNextVisibleLogPosition(APosition, ASkipFieldBoundaryRanges, False);
end;

function TdxVisibleTextFilterBase.GetNextVisibleLogPosition(const APosition: TdxDocumentModelPosition;
  ASkipFieldBoundaryRanges, AInsureNextLogPositionVisible: Boolean): TdxDocumentLogPosition;
var
  APos: TdxDocumentModelPosition;
  ACharSkipped: Boolean;
  AVisibleCharSkipped: Boolean;
begin
  APos := APosition;
  ACharSkipped := False;
  repeat
    APos := GetNextVisibleLogPositionCore(APos, ASkipFieldBoundaryRanges, AVisibleCharSkipped);
    ACharSkipped := ACharSkipped or AVisibleCharSkipped;
  until not (((ASkipFieldBoundaryRanges and (IsPositionBeforeFieldResultEndRun(APos) or not ACharSkipped or IsPositionAfterFieldCodeEndRun(APos))) or
    IsPositionOnInvisibleCellParagraph(APos)));
  if AInsureNextLogPositionVisible then
    Result := GetVisibleLogPosition(APos)
  else
    Result := APos.LogPosition;
end;

function TdxVisibleTextFilterBase.GetModelPosition(ALogPosition: TdxDocumentLogPosition): TdxDocumentModelPosition;
var
  ARunIndex: TdxRunIndex;
begin
  Result := TdxDocumentModelPosition.Create(PieceTable);
  Result.ParagraphIndex := PieceTable.FindParagraphIndex(ALogPosition);
  Result.RunStartLogPosition := PieceTable.FindRunStartLogPosition(PieceTable.Paragraphs[Result.ParagraphIndex], ALogPosition,
    ARunIndex);
  Result.RunIndex := ARunIndex;
  Result.LogPosition := ALogPosition;
end;

function TdxVisibleTextFilterBase.GetNextVisibleLogPosition(ALogPosition: TdxDocumentLogPosition;
  ASkipFieldResultEnd: Boolean): TdxDocumentLogPosition;
begin
  Result := GetNextVisibleLogPosition(ALogPosition, ASkipFieldResultEnd, False);
end;

function TdxVisibleTextFilterBase.GetNextVisibleLogPosition(ALogPosition: TdxDocumentLogPosition; ASkipFieldResultEnd, AInsureNextLogPositionVisible: Boolean): TdxDocumentLogPosition;
var
  AModelPosition: TdxDocumentModelPosition;
begin
  AModelPosition := GetModelPosition(ALogPosition);
  Result := GetNextVisibleLogPosition(AModelPosition, ASkipFieldResultEnd, AInsureNextLogPositionVisible);
end;

function TdxVisibleTextFilterBase.GetNextVisibleLogPositionCore(const APosition: TdxDocumentModelPosition;
  ABreakAfterFieldResultEndRun: Boolean; out AVisibleCharSkipped: Boolean): TdxDocumentModelPosition;
var
  ARuns: TdxRunCollection;
  ARunIndex, AMaxIndex: TdxRunIndex;
  APos, ARunStartPosition: TdxDocumentLogPosition;
begin
  ARuns := PieceTable.Runs;
  ARunIndex := APosition.RunIndex;
  APos := APosition.LogPosition;
  AMaxIndex := PieceTable.Runs.Count - 1;
  AVisibleCharSkipped := False;
  if APosition.RunOffset < ARuns[ARunIndex].Length then
  begin
    if IsRunVisible(ARunIndex) then
    begin
      APosition.LogPosition := APos + 1;
      if (APosition.LogPosition > APosition.RunEndLogPosition) and (ARuns.Count - 1 <> ARunIndex) then
      begin
        APosition.RunIndex := APosition.RunIndex + 1;
        APosition.RunStartLogPosition := APosition.LogPosition;
      end;
      AVisibleCharSkipped := True;
      Exit(APosition);
    end
    else
      APos := APos + ARuns[ARunIndex].Length - APosition.RunOffset;
  end;
  ARunStartPosition := APos;
  while ARunIndex < AMaxIndex do
  begin
    ARunStartPosition := APos;
    Inc(ARunIndex);
    if IsRunVisible(ARunIndex) then
      Break;
    APos := APos + ARuns[ARunIndex].Length;
    if ABreakAfterFieldResultEndRun and (ARuns[ARunIndex] is TdxFieldResultEndRun) then
      Break;
  end;
  APosition.LogPosition := APos;
  APosition.RunIndex := ARunIndex;
  APosition.RunStartLogPosition := ARunStartPosition;
  Result := APosition;
end;

function TdxVisibleTextFilterBase.GetNextVisibleRunIndex(ARunIndex: TdxRunIndex): TdxRunIndex;
var
  AMaxIndex: TdxRunIndex;
begin
  AMaxIndex := PieceTable.Runs.Count - 1;
  Result := ARunIndex;
  while Result < AMaxIndex do
  begin
    Inc(Result);
    if IsRunVisible(Result) then
      Break;
  end;
end;

function TdxVisibleTextFilterBase.GetPrevVisibleLogPosition(const APosition: TdxDocumentModelPosition;
  ASkipFieldBoundaryRanges: Boolean): TdxDocumentLogPosition;
var
  APos: TdxDocumentModelPosition;
  ACharSkipped: Boolean;
  AVisibleCharSkipped: Boolean;
begin
  APos := APosition;
  ACharSkipped := False;
  repeat
    APos := GetPrevVisiblePositionCore(APos, ASkipFieldBoundaryRanges, AVisibleCharSkipped);
    ACharSkipped := ACharSkipped or AVisibleCharSkipped;
    if APos.LogPosition = 0 then
      Break;
  until not ((ASkipFieldBoundaryRanges and (IsPositionBeforeFieldResultEndRun(APos) or not ACharSkipped or
    IsPositionAfterFieldCodeEndRun(APos) or IsPositionBeforeInvisibleFieldCodeEndRun(APos))) or
    IsPositionOnInvisibleCellParagraph(APos));
  Result := APos.LogPosition;
end;

function TdxVisibleTextFilterBase.GetPrevVisibleLogPosition(ALogPosition: TdxDocumentLogPosition;
  ASkipFieldBoundaryRanges: Boolean): TdxDocumentLogPosition;
var
  AModelPosition: TdxDocumentModelPosition;
begin
  AModelPosition := GetModelPosition(ALogPosition);
  Result := GetPrevVisibleLogPosition(AModelPosition, ASkipFieldBoundaryRanges);
end;

function TdxVisibleTextFilterBase.GetPrevVisiblePositionCore(const APosition: TdxDocumentModelPosition;
  ABreakOnFieldCodeStart: Boolean; out AVisibleCharSkipped: Boolean): TdxDocumentModelPosition;
var
  APos, ARunStartLogPosition: TdxDocumentLogPosition;
  ARuns: TdxRunCollection;
  ARunIndex: TdxRunIndex;
begin
  APos := APosition.LogPosition;
  ARuns := PieceTable.Runs;
  ARunIndex := APosition.RunIndex;
  AVisibleCharSkipped := False;
  if APosition.RunOffset > 0 then
  begin
    if IsRunVisible(ARunIndex) then
    begin
      APosition.LogPosition := APos - 1;
      AVisibleCharSkipped := True;
      Exit(APosition);
    end
    else
      APos := APosition.RunStartLogPosition;
  end;
  ARunStartLogPosition := APos;
  while ARunIndex > 0 do
  begin
    Dec(ARunIndex);
    ARunStartLogPosition := APos - ARuns[ARunIndex].Length;
    if IsRunVisible(ARunIndex) then
      Break;
    APos := APos - ARuns[ARunIndex].Length;
    if ABreakOnFieldCodeStart and (ARuns[ARunIndex] is TdxFieldCodeStartRun) then
      Break;
  end;
  APosition.LogPosition := APos;
  APosition.RunIndex := ARunIndex;
  APosition.RunStartLogPosition := ARunStartLogPosition;
  Result := APosition;
end;

function TdxVisibleTextFilterBase.GetPrevVisibleRunIndex(ARunIndex: TdxRunIndex): TdxRunIndex;
begin
  Result := ARunIndex;
  while Result > 0 do
  begin
    Dec(Result);
    if IsRunVisible(Result) then
      Break;
  end;
end;

function TdxVisibleTextFilterBase.GetRunVisibilityInField(
  ARunIndex: TdxRunIndex): TdxRunVisibility;
var
  AField: TdxField;
begin
  AField := TdxSimplePieceTable(PieceTable).FindFieldByRunIndex(ARunIndex);
  if AField <> nil then
    Result := IsFieldRunVisible(ARunIndex, AField)
  else
    Result := TdxRunVisibility.Visible;
end;

function TdxVisibleTextFilterBase.GetVisibleLogPosition(const APosition: TdxDocumentModelPosition): TdxDocumentLogPosition;
var
  ARuns: TdxRunCollection;
  ARunIndex, AMaxIndex: TdxRunIndex;
begin
  ARuns := PieceTable.Runs;
  ARunIndex := APosition.RunIndex;
  Result := APosition.LogPosition;
  if IsRunVisible(ARunIndex) then
    Exit
  else
    Inc(Result, ARuns[ARunIndex].Length - APosition.RunOffset);
  AMaxIndex := PieceTable.Runs.Count - 1;
  while ARunIndex < AMaxIndex do
  begin
    Inc(ARunIndex);
    if IsRunVisible(ARunIndex) then
      Break;
    Inc(Result, ARuns[ARunIndex].Length);
  end;
end;

function TdxVisibleTextFilterBase.GetVisibleLogPosition(const APosition: TdxDocumentModelPosition;
  ASkipFieldBoundaryRanges: Boolean): TdxDocumentLogPosition;
var
  AVisibleCharSkipped: Boolean;
  APos: TdxDocumentModelPosition;
begin
  APos := APosition;
  while (ASkipFieldBoundaryRanges and
    (IsPositionBeforeFieldResultEndRun(APos) or IsPositionAfterFieldCodeEndRun(APos) or
    IsPositionBeforeInvisibleFieldCodeStartRun(APos))) or
    IsPositionOnInvisibleCellParagraph(APos) do
  begin
    APos := GetPrevVisiblePositionCore(APos, ASkipFieldBoundaryRanges, AVisibleCharSkipped);
    if APos.LogPosition = 0 then
      Break;
  end;
  Result := APos.LogPosition;
end;

function TdxVisibleTextFilterBase.HasVisibleRunInParagraph(ALastParagraphRun: TdxRunIndex): Boolean;
var
  ARunIndex: TdxRunIndex;
begin
  ARunIndex := ALastParagraphRun - 1;
  while ARunIndex >= 0 do
  begin
    if IsRunVisible(ARunIndex) then
      Exit(not (PieceTable.Runs[ARunIndex] is TdxParagraphRun));
    Dec(ARunIndex);
  end;
  Result := False;
end;

function TdxVisibleTextFilterBase.IsFieldRunVisible(ARunIndex: TdxRunIndex; AField: TdxField): TdxRunVisibility;
begin
  if IsFieldRunVisibleCore(ARunIndex, AField) then
  begin
    if AField.Parent <> nil then
      Result := IsFieldRunVisible(ARunIndex, AField.Parent)
    else
      Result := TdxRunVisibility.Visible;
  end
  else
    Result := TdxRunVisibility.Hidden;
end;

function TdxVisibleTextFilterBase.IsFieldRunVisibleCore(ARunIndex: TdxRunIndex; AField: TdxField): Boolean;
begin
  Result := AField.IsCodeView and AField.Code.Contains(ARunIndex) or
    not AField.IsCodeView and AField.Result.Contains(ARunIndex);
end;

function TdxVisibleTextFilterBase.IsPositionAfterFieldCodeEndRun(const APosition: TdxDocumentModelPosition): Boolean;
var
  ARunIndex: TdxRunIndex;
  ARuns: TdxRunCollection;
begin
  Result := False;
  ARunIndex := APosition.RunIndex;
  ARuns := PieceTable.Runs;
  if (ARuns[ARunIndex] is TdxFieldCodeEndRun) and (APosition.RunOffset = ARuns[ARunIndex].Length) then
    Result := True
  else
    if (ARunIndex > 0) and (APosition.RunStartLogPosition = APosition.LogPosition) and
        (ARuns[ARunIndex - 1] is TdxFieldCodeEndRun) then
      Result := True;
end;

function TdxVisibleTextFilterBase.IsPositionBeforeFieldResultEndRun(const APosition: TdxDocumentModelPosition): Boolean;
var
  ARuns: TdxRunCollection;
begin
  Result := False;
  ARuns := PieceTable.Runs;
  if (APosition.LogPosition = APosition.RunStartLogPosition) and
      (ARuns[APosition.RunIndex] is TdxFieldResultEndRun) then
    Result := True
  else
    if (APosition.RunIndex + 1 < ARuns.Count) and
        (APosition.LogPosition = APosition.RunEndLogPosition + 1) and
        (ARuns[APosition.RunIndex + 1] is TdxFieldResultEndRun) then
      Result := True;
end;

function TdxVisibleTextFilterBase.IsPositionBeforeInvisibleFieldCodeEndRun(
  const APosition: TdxDocumentModelPosition): Boolean;
var
  ARuns: TdxRunCollection;
begin
  Result := False;
  ARuns := PieceTable.Runs;
  if (APosition.LogPosition = APosition.RunStartLogPosition) and
      (ARuns[APosition.RunIndex] is TdxFieldCodeStartRun) then
    Result := not IsPositionVisible(APosition.RunIndex)
  else
    if (APosition.RunIndex + 1 < ARuns.Count) and
        (APosition.LogPosition = APosition.RunEndLogPosition + 1) and
        (ARuns[APosition.RunIndex + 1] is TdxFieldCodeStartRun) then
      Result := not IsPositionVisible(APosition.RunIndex + 1);
end;

function TdxVisibleTextFilterBase.IsPositionBeforeInvisibleFieldCodeStartRun(
  const APosition: TdxDocumentModelPosition): Boolean;
var
  ARuns: TdxRunCollection;
begin
  ARuns := PieceTable.Runs;
  if (APosition.LogPosition = APosition.RunStartLogPosition) and
      (ARuns[APosition.RunIndex] is TdxFieldCodeStartRun) then
    Result :=  not IsPositionVisible(APosition.RunIndex)
  else
    if (APosition.RunIndex + 1 < ARuns.Count) and
        (APosition.LogPosition = APosition.RunEndLogPosition + 1) and
        (ARuns[APosition.RunIndex + 1] is TdxFieldCodeStartRun) then
      Result := not IsPositionVisible(APosition.RunIndex + 1)
    else
      Result := False;
end;

function TdxVisibleTextFilterBase.IsPositionOnInvisibleCellParagraph(const APosition: TdxDocumentModelPosition): Boolean;
var
  ARunOffset: Integer;
  ARunLength: Integer;
  ARunIndex: TdxRunIndex;
  ARuns: TdxRunCollection;
  ARun: TdxParagraphRun;
  ACell: TdxCustomTableCell;
begin
  Result := False;
  ARunOffset := APosition.RunOffset;
  ARunLength := APosition.RunEndLogPosition - APosition.RunStartLogPosition + 1;
  if (ARunOffset <> 0) and (ARunOffset <> ARunLength) then
    Exit;
  ARunIndex := APosition.RunIndex;
  if ARunOffset = ARunLength then
    Inc(ARunIndex);
  ARuns := PieceTable.Runs;
  if ARunIndex >= ARuns.Count then
    Exit;
  if ARuns[ARunIndex] is TdxParagraphRun then
  begin
    ARun := TdxParagraphRun(ARuns[ARunIndex]);
    ACell := ARun.Paragraph.GetCellCore;
    if (ACell <> nil) then
    begin
      if ACell.IsContinueVerticalMerging then
        Result := True;
    end;
  end;
end;

function TdxVisibleTextFilterBase.IsPositionVisible(AFieldCodeStartRunIndex: TdxRunIndex): Boolean;
var
  AField, AParent: TdxField;
begin
  AField := TdxSimplePieceTable(PieceTable).FindFieldByRunIndex(AFieldCodeStartRunIndex);
  AParent := AField.Parent;
  if AParent = nil then
    Exit(True);
  if AParent.Code.Contains(AField) then
    Result := AParent.IsCodeView
  else
    Result := not AParent.IsCodeView;
end;

function TdxVisibleTextFilterBase.IsRunVisible(const ARunIndex: TdxRunIndex): Boolean;
begin
  Result := IsRunVisibleCore(ARunIndex) <> TdxRunVisibility.Hidden;
end;

function TdxVisibleTextFilterBase.IsRunVisibleCore(const ARunIndex: TdxRunIndex): TdxRunVisibility;
var
  ARun: TdxTextRunBase;
  AParagraphRun: TdxParagraphRun;
  ACell: TdxCustomTableCell;
  AParagraph: TdxSimpleParagraph;
  ANextParagraphIndex: TdxParagraphIndex;
  AFieldRunVisibility: TdxRunVisibility;
  APieceTable: TdxSimplePieceTable;
begin
  APieceTable := TdxSimplePieceTable(PieceTable);
  ARun := APieceTable.Runs[ARunIndex];
  if ARun is TdxFieldResultEndRun then
    Exit(TdxRunVisibility.Hidden);
  if (ARun is TdxSeparatorTextRun) and ARun.Hidden then
    Exit(TdxRunVisibility.Hidden);
  AFieldRunVisibility := GetRunVisibilityInField(ARunIndex);
  if AFieldRunVisibility <> TdxRunVisibility.Visible then
    Exit(AFieldRunVisibility);
  if ARun is TdxParagraphRun then
  begin
    AParagraphRun := TdxParagraphRun(ARun);
    ACell := AParagraphRun.Paragraph.GetCellCore;
    if ACell <> nil then
    begin
      if ACell.IsContinueVerticalMerging then
      begin
        Result := TdxRunVisibility.Hidden;
        Exit;
      end;
      if ACell.GetEndParagraphIndexCore = AParagraphRun.Paragraph.Index then
      begin
        Result := TdxRunVisibility.ForceVisible;
        Exit;
      end;
    end;
    AParagraph := AParagraphRun.Paragraph;
    ANextParagraphIndex := AParagraph.Index + 1;
    if ANextParagraphIndex < FPieceTable.Paragraphs.Count then
    begin
      if APieceTable.Paragraphs[ANextParagraphIndex].GetCellCore <> ACell then
      begin
        if HasVisibleRunInParagraph(Min(ARunIndex, AParagraph.LastRunIndex)) then
          Exit(TdxRunVisibility.ForceVisible)
        else
          Exit(TdxRunVisibility.Visible);
      end;
    end;
  end;
  Result := TdxRunVisibility.Visible;
end;

end.
