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

unit dxRichEdit.DocumentModel.CopyManager.Simple;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.CopyManager.Core;

type
  { TdxSelectionBasedOperation }

  TdxSelectionBasedOperation = class abstract
  strict private
    FBackspacePressed: Boolean;
    FPieceTable: TdxSimplePieceTable;
  private
    function GetDocumentModel: TdxSimpleDocumentModel;
  protected
    procedure AfterExecute; virtual;
    procedure BeforeExecute; virtual;

    function ShouldProcessRunParent(AInfo: TdxRunInfo): Boolean; virtual; abstract;
    function ShouldProcessContentInSameParent(AInfo: TdxRunInfo): Boolean; virtual; abstract;
    procedure ProcessRunParent(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean); virtual; abstract;
    procedure ProcessContentInsideParent(AInfo: TdxRunInfo; AAllowMergeWithNextParagraph, ADocumentLastParagraphSelected: Boolean); virtual; abstract;
    function ProcessContentSameParent(AInfo: TdxRunInfo; AAllowMergeWithNextParagraph, ADocumentLastParagraphSelected: Boolean): Boolean; virtual;
    procedure ProcessContentCrossParent(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean); virtual;
    function ProcessHead(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Integer; virtual; abstract;
    function ProcessMiddle(AInfo: TdxRunInfo; AParagraphCount: Integer; ADocumentLastParagraphSelected: Boolean): Boolean; virtual; abstract;
    function ProcessTail(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Integer; virtual; abstract;

    property BackspacePressed: Boolean read FBackspacePressed write FBackspacePressed;
  public
    constructor Create(APieceTable: TdxSimplePieceTable);

    function Execute(AStartLogPosition: TdxDocumentLogPosition;
      ALength: Integer; ADocumentLastParagraphSelected: Boolean): Boolean;
    function ExecuteCore(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Boolean; virtual;

    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
    property PieceTable: TdxSimplePieceTable read FPieceTable;
  end;

  { TdxDeleteContentOperationBase }

  TdxDeleteContentOperationBase = class abstract(TdxCustomDeleteContentOperation)
  private
    FDeletingResult: Boolean;
    function GetDocumentModel: TdxSimpleDocumentModel;
    function GetPieceTable: TdxSimplePieceTable;
  protected
    procedure AfterExecute; virtual;
    procedure BeforeExecute(ARunInfo: TdxRunInfo); virtual;
    function CreateDeleteContentOperation: TdxSelectionBasedOperation; virtual; abstract;
    procedure DeleteContent(const AStart, AEnd: TdxDocumentModelPosition; ADocumentLastParagraphSelected: Boolean); overload; virtual;
    procedure DeleteContent(AStartPosition: TdxDocumentLogPosition; ALength: Integer; ADocumentLastParagraphSelected: Boolean); overload; virtual;
    procedure ExecuteCore(ARunInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean); virtual; abstract;
  public
    function Execute(AStartLogPosition: TdxDocumentLogPosition; ALength: Integer;
      ADocumentLastParagraphSelected: Boolean): Boolean; override;

    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
    property PieceTable: TdxSimplePieceTable read GetPieceTable;
  end;

  { TdxSimpleDeleteContentOperation }

  TdxSimpleDeleteContentOperation = class(TdxDeleteContentOperationBase)
  end;

  { TdxSimpleDocumentModelCopyManager }

  TdxSimpleDocumentModelCopyManager = class(TdxCustomDocumentModelCopyManager)
  strict private
    function GetSourceModel: TdxSimpleDocumentModel;
    function GetSourcePieceTable: TdxSimplePieceTable;
    function GetTargetModel: TdxSimpleDocumentModel;
    function GetTargetPieceTable: TdxSimplePieceTable;
  protected
    procedure ApplySourceParagraphFormatting(ASourceParagraph, ATargetParagraph: TdxSimpleParagraph);
    procedure ApplySourceCharacterFormatting(ASourceRun, ATargetRun: TdxTextRunBase);
  public
    procedure OnTargetParagraphInserted(ASourceParagraph, ATargetParagraph: TdxSimpleParagraph;
      ASourceRun, ATargetRun: TdxTextRunBase);
    procedure OnTargetRunInserted(ASourceRun, ATargetRun: TdxTextRunBase);
    property SourceModel: TdxSimpleDocumentModel read GetSourceModel;
    property SourcePieceTable: TdxSimplePieceTable read GetSourcePieceTable;
    property TargetModel: TdxSimpleDocumentModel read GetTargetModel;
    property TargetPieceTable: TdxSimplePieceTable read GetTargetPieceTable;
  end;


implementation

{ TdxSelectionBasedOperation }

constructor TdxSelectionBasedOperation.Create(APieceTable: TdxSimplePieceTable);
begin
  inherited Create;
  FPieceTable := APieceTable;
end;

function TdxSelectionBasedOperation.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := PieceTable.DocumentModel;
end;

function TdxSelectionBasedOperation.Execute(AStartLogPosition: TdxDocumentLogPosition;
  ALength: Integer; ADocumentLastParagraphSelected: Boolean): Boolean;
var
  ATransaction: TdxHistoryTransaction;
  AIsSelectionChanged: Boolean;
  AInfo: TdxRunInfo;
begin
  if ALength <= 0 then
    Exit(False);
  DocumentModel.BeginUpdate;
  try
    AIsSelectionChanged := DocumentModel.Selection.IsSelectionChanged;
    ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
    try
      BeforeExecute;
      AInfo := PieceTable.ObtainAffectedRunInfo(AStartLogPosition, ALength);
      try
        Result := ExecuteCore(AInfo, ADocumentLastParagraphSelected);
      finally
        AInfo.Free;
      end;
      AfterExecute;
    finally
      if not AIsSelectionChanged then
        DocumentModel.Selection.IsSelectionChanged := False;
      ATransaction.Free;
    end;
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxSelectionBasedOperation.AfterExecute;
begin
end;

procedure TdxSelectionBasedOperation.BeforeExecute;
begin
end;

function TdxSelectionBasedOperation.ExecuteCore(AInfo: TdxRunInfo;
  ADocumentLastParagraphSelected: Boolean): Boolean;
begin
  if ShouldProcessContentInSameParent(AInfo) then
    Result := ProcessContentSameParent(AInfo, True, ADocumentLastParagraphSelected)
  else
  begin
    ProcessContentCrossParent(AInfo, ADocumentLastParagraphSelected);
    Result := True;
  end;
end;

function TdxSelectionBasedOperation.ProcessContentSameParent(AInfo: TdxRunInfo;
  AAllowMergeWithNextParagraph, ADocumentLastParagraphSelected: Boolean): Boolean;
begin
  Result := ShouldProcessRunParent(AInfo);
  if Result then
    ProcessRunParent(AInfo, ADocumentLastParagraphSelected)
  else
    ProcessContentInsideParent(AInfo, AAllowMergeWithNextParagraph, ADocumentLastParagraphSelected)
end;

procedure TdxSelectionBasedOperation.ProcessContentCrossParent(
  AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean);
var
  ASectionCount: Integer;
begin
  ASectionCount := ProcessHead(AInfo, ADocumentLastParagraphSelected);
  if ProcessMiddle(AInfo, ASectionCount, ADocumentLastParagraphSelected) then
    ProcessTail(AInfo, ADocumentLastParagraphSelected);
end;

{ TdxDeleteContentOperationBase }

procedure TdxDeleteContentOperationBase.AfterExecute;
begin
end;

procedure TdxDeleteContentOperationBase.BeforeExecute(ARunInfo: TdxRunInfo);
begin
end;

procedure TdxDeleteContentOperationBase.DeleteContent(const AStart,
  AEnd: TdxDocumentModelPosition; ADocumentLastParagraphSelected: Boolean);
var
  AStartPosition: TdxDocumentLogPosition;
  ALength: Integer;
begin
  AStartPosition := AStart.LogPosition;
  ALength := AEnd.LogPosition - AStartPosition + 1;
  DeleteContent(AStartPosition, ALength, ADocumentLastParagraphSelected);
end;

procedure TdxDeleteContentOperationBase.DeleteContent(AStartPosition: TdxDocumentLogPosition;
  ALength: Integer; ADocumentLastParagraphSelected: Boolean);
var
  AOperation: TdxSelectionBasedOperation;
begin
  AOperation := CreateDeleteContentOperation;
  try
    FDeletingResult := AOperation.Execute(AStartPosition, ALength, ADocumentLastParagraphSelected) or FDeletingResult;
  finally
    AOperation.Free;
  end;
end;

function TdxDeleteContentOperationBase.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := PieceTable.DocumentModel;
end;

function TdxDeleteContentOperationBase.GetPieceTable: TdxSimplePieceTable;
begin
  Result := TdxSimplePieceTable(inherited PieceTable);
end;

function TdxDeleteContentOperationBase.Execute(AStartLogPosition: TdxDocumentLogPosition;
  ALength: Integer; ADocumentLastParagraphSelected: Boolean): Boolean;
var
  ARunInfo: TdxRunInfo;
  ATransaction: TdxHistoryTransaction;
begin
  if ALength <= 0 then
    Exit(False);
  ARunInfo := PieceTable.ObtainAffectedRunInfo(AStartLogPosition, ALength);
  try
    FDeletingResult := False;
    ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
    try
      BeforeExecute(ARunInfo);
      ExecuteCore(ARunInfo, ADocumentLastParagraphSelected);
      AfterExecute;
    finally
      ATransaction.Free;
    end;
    Result := FDeletingResult;
  finally
    ARunInfo.Free;
  end;
end;

{ TdxSimpleDocumentModelCopyManager }

procedure TdxSimpleDocumentModelCopyManager.OnTargetParagraphInserted(
  ASourceParagraph, ATargetParagraph: TdxSimpleParagraph;
  ASourceRun, ATargetRun: TdxTextRunBase);
begin
  TargetPosition.ParagraphIndex := TargetPosition.ParagraphIndex + 1;
  OnTargetRunInserted(ASourceRun, ATargetRun);
  if FormattingCopyOptions = TdxFormattingCopyOptions.KeepSourceFormatting then
    ApplySourceParagraphFormatting(ASourceParagraph, ATargetParagraph);
end;

procedure TdxSimpleDocumentModelCopyManager.OnTargetRunInserted(ASourceRun, ATargetRun: TdxTextRunBase);
var
  ARunLength: Integer;
begin
  ARunLength := ASourceRun.Length;
  TargetPosition.RunStartLogPosition := TargetPosition.RunStartLogPosition + ARunLength;
  TargetPosition.LogPosition := TargetPosition.LogPosition + ARunLength;
  TargetPosition.RunIndex := TargetPosition.RunIndex + 1;
  if TargetPieceTable = TargetModel.MainPieceTable then
    Assert(TargetPosition.RunIndex = TdxPositionConverter.ToDocumentModelPosition(TargetModel.MainPieceTable, TargetPosition.LogPosition).RunIndex);
  if FormattingCopyOptions = TdxFormattingCopyOptions.KeepSourceFormatting then
    ApplySourceCharacterFormatting(ASourceRun, ATargetRun);
end;

procedure TdxSimpleDocumentModelCopyManager.ApplySourceParagraphFormatting(ASourceParagraph, ATargetParagraph: TdxSimpleParagraph);
var
  ATargetFormatting: TdxRunParagraphFormatting;
  ASourceFormatting: TdxRunParagraphFormattingKey;
begin
  ASourceFormatting := TdxRunParagraphFormattingKey.Create(ASourceParagraph.ParagraphProperties.Index,
    ASourceParagraph.ParagraphStyleIndex, ASourceParagraph.GetOwnNumberingListIndex, ASourceParagraph.GetOwnListLevelIndex);
  if MapSourceToTargetParagraphFormatting.TryGetValue(ASourceFormatting, ATargetFormatting) then
  begin
    ATargetParagraph.ParagraphStyleIndex := ATargetFormatting.ParagraphStyleIndex;
    ATargetParagraph.ParagraphProperties.ChangeIndexCore(ATargetFormatting.ParagraphPropertiesIndex, []);
  end
  else
  begin
    ATargetParagraph.ParagraphProperties.BeginUpdate;
    try
      if ASourceParagraph.Alignment <> ATargetParagraph.Alignment then
        ATargetParagraph.Alignment := ASourceParagraph.Alignment;

      if ASourceParagraph.LeftIndent <> ATargetParagraph.LeftIndent then
        ATargetParagraph.LeftIndent := ASourceParagraph.LeftIndent;

      if ASourceParagraph.RightIndent <> ATargetParagraph.RightIndent then
        ATargetParagraph.RightIndent := ASourceParagraph.RightIndent;

      if ASourceParagraph.SpacingBefore <> ATargetParagraph.SpacingBefore then
        ATargetParagraph.SpacingBefore := ASourceParagraph.SpacingBefore;

      if ASourceParagraph.SpacingAfter <> ATargetParagraph.SpacingAfter then
        ATargetParagraph.SpacingAfter := ASourceParagraph.SpacingAfter;

      if ASourceParagraph.LineSpacingType <> ATargetParagraph.LineSpacingType then
        ATargetParagraph.LineSpacingType := ASourceParagraph.LineSpacingType;

      if ASourceParagraph.LineSpacing <> ATargetParagraph.LineSpacing then
        ATargetParagraph.LineSpacing := ASourceParagraph.LineSpacing;

      if ASourceParagraph.FirstLineIndentType <> ATargetParagraph.FirstLineIndentType then
        ATargetParagraph.FirstLineIndentType := ASourceParagraph.FirstLineIndentType;

      if ASourceParagraph.FirstLineIndent <> ATargetParagraph.FirstLineIndent then
        ATargetParagraph.FirstLineIndent := ASourceParagraph.FirstLineIndent;

      if ASourceParagraph.SuppressHyphenation <> ATargetParagraph.SuppressHyphenation then
        ATargetParagraph.SuppressHyphenation := ASourceParagraph.SuppressHyphenation;

      if ASourceParagraph.SuppressLineNumbers <> ATargetParagraph.SuppressLineNumbers then
        ATargetParagraph.SuppressLineNumbers := ASourceParagraph.SuppressLineNumbers;

      if ASourceParagraph.ContextualSpacing <> ATargetParagraph.ContextualSpacing then
        ATargetParagraph.ContextualSpacing := ASourceParagraph.ContextualSpacing;

      if ASourceParagraph.PageBreakBefore <> ATargetParagraph.PageBreakBefore then
        ATargetParagraph.PageBreakBefore := ASourceParagraph.PageBreakBefore;

      if ASourceParagraph.BeforeAutoSpacing <> ATargetParagraph.BeforeAutoSpacing then
        ATargetParagraph.BeforeAutoSpacing := ASourceParagraph.BeforeAutoSpacing;

      if ASourceParagraph.AfterAutoSpacing <> ATargetParagraph.AfterAutoSpacing then
        ATargetParagraph.AfterAutoSpacing := ASourceParagraph.AfterAutoSpacing;

      if ASourceParagraph.KeepWithNext <> ATargetParagraph.KeepWithNext then
        ATargetParagraph.KeepWithNext := ASourceParagraph.KeepWithNext;

      if ASourceParagraph.KeepLinesTogether <> ATargetParagraph.KeepLinesTogether then
        ATargetParagraph.KeepLinesTogether := ASourceParagraph.KeepLinesTogether;

      if ASourceParagraph.WidowOrphanControl <> ATargetParagraph.WidowOrphanControl then
        ATargetParagraph.WidowOrphanControl := ASourceParagraph.WidowOrphanControl;

      if ASourceParagraph.OutlineLevel <> ATargetParagraph.OutlineLevel then
        ATargetParagraph.OutlineLevel := ASourceParagraph.OutlineLevel;

      if ASourceParagraph.BackColor <> ATargetParagraph.BackColor then
        ATargetParagraph.BackColor := ASourceParagraph.BackColor;
    finally
      ATargetParagraph.ParagraphProperties.EndUpdate;
    end;
    MapSourceToTargetParagraphFormatting.Add(ASourceFormatting, TdxRunParagraphFormatting.Create(ATargetParagraph.ParagraphStyleIndex, ATargetParagraph.ParagraphProperties.Index));
  end;
end;

procedure TdxSimpleDocumentModelCopyManager.ApplySourceCharacterFormatting(ASourceRun, ATargetRun: TdxTextRunBase);
var
  ATargetFormatting, ASourceFormatting: TdxRunCharacterFormatting;
begin
  ASourceFormatting := TdxRunCharacterFormatting.Create(ASourceRun.CharacterProperties.Index, ASourceRun.CharacterStyleIndex);
  if MapSourceToTargetCharacterFormatting.TryGetValue(ASourceFormatting, ATargetFormatting) then
  begin
    ATargetRun.CharacterStyleIndex := ATargetFormatting.CharacterStyleIndex;
    ATargetRun.CharacterProperties.ChangeIndexCore(ATargetFormatting.CharacterPropertiesIndex, []);
  end
  else
  begin
    ATargetRun.CharacterProperties.BeginUpdate;
    try
      if ASourceRun.FontName <> ATargetRun.FontName then
        ATargetRun.FontName := ASourceRun.FontName;
      if ASourceRun.DoubleFontSize <> ATargetRun.DoubleFontSize then
        ATargetRun.DoubleFontSize := ASourceRun.DoubleFontSize;
      if ASourceRun.FontBold <> ATargetRun.FontBold then
        ATargetRun.FontBold := ASourceRun.FontBold;
      if ASourceRun.FontItalic <> ATargetRun.FontItalic then
        ATargetRun.FontItalic := ASourceRun.FontItalic;
      if ASourceRun.FontStrikeoutType <> ATargetRun.FontStrikeoutType then
        ATargetRun.FontStrikeoutType := ASourceRun.FontStrikeoutType;
      if ASourceRun.FontUnderlineType <> ATargetRun.FontUnderlineType then
        ATargetRun.FontUnderlineType := ASourceRun.FontUnderlineType;
      if ASourceRun.AllCaps <> ATargetRun.AllCaps then
        ATargetRun.AllCaps := ASourceRun.AllCaps;
      if ASourceRun.UnderlineWordsOnly <> ATargetRun.UnderlineWordsOnly then
        ATargetRun.UnderlineWordsOnly := ASourceRun.UnderlineWordsOnly;
      if ASourceRun.StrikeoutWordsOnly <> ATargetRun.StrikeoutWordsOnly then
        ATargetRun.StrikeoutWordsOnly := ASourceRun.StrikeoutWordsOnly;
      if ASourceRun.ForeColor <> ATargetRun.ForeColor then
        ATargetRun.ForeColor := ASourceRun.ForeColor;
      if ASourceRun.BackColor <> ATargetRun.BackColor then
        ATargetRun.BackColor := ASourceRun.BackColor;
      if ASourceRun.UnderlineColor <> ATargetRun.UnderlineColor then
        ATargetRun.UnderlineColor := ASourceRun.UnderlineColor;
      if ASourceRun.StrikeoutColor <> ATargetRun.StrikeoutColor then
        ATargetRun.StrikeoutColor := ASourceRun.StrikeoutColor;
      if ASourceRun.Script <> ATargetRun.Script then
        ATargetRun.Script := ASourceRun.Script;
      if ASourceRun.Hidden <> ATargetRun.Hidden then
        ATargetRun.Hidden := ASourceRun.Hidden;
    finally
      ATargetRun.CharacterProperties.EndUpdate;
    end;
    MapSourceToTargetCharacterFormatting.Add(ASourceFormatting, TdxRunCharacterFormatting.Create(ATargetRun.CharacterStyleIndex, ATargetRun.CharacterProperties.Index));
  end;
end;

function TdxSimpleDocumentModelCopyManager.GetSourceModel: TdxSimpleDocumentModel;
begin
  Result := TdxSimpleDocumentModel(inherited SourceModel);
end;

function TdxSimpleDocumentModelCopyManager.GetSourcePieceTable: TdxSimplePieceTable;
begin
  Result := TdxSimplePieceTable(inherited SourcePieceTable);
end;

function TdxSimpleDocumentModelCopyManager.GetTargetModel: TdxSimpleDocumentModel;
begin
  Result := TdxSimpleDocumentModel(inherited TargetModel);
end;

function TdxSimpleDocumentModelCopyManager.GetTargetPieceTable: TdxSimplePieceTable;
begin
  Result := TdxSimplePieceTable(inherited TargetPieceTable);
end;

end.
