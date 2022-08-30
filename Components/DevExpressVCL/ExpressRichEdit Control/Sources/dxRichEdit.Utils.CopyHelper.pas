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

unit dxRichEdit.Utils.CopyHelper;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes,
  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields;

type
  { TdxCopyHelper }

  TdxCopyHelper = class
  public
    class procedure CopyDocumentModel(ASource: TdxDocumentModel; ATarget: TdxDocumentModel;
      AIncludeUnreferenced: Boolean); static;
    class procedure CopyDocumentModelProperties(ASource: TdxDocumentModel; ATarget: TdxDocumentModel); static;
    class procedure CopySections(ASource: TdxSectionCollection; ATarget: TdxSectionCollection); static;
    class procedure CopySectionCore(ASourceSection: TdxSection; ATargetSection: TdxSection); static;
    class procedure CopyUnreferencedNotes(ASource: TdxDocumentModel; ATarget: TdxDocumentModel); static;
    class function CopyAndWrapToField(ASourcePieceTable: TdxPieceTable; ATargetPieceTable: TdxPieceTable;
      ASourceInterval: TdxDocumentLogInterval; ALogPosition: TdxDocumentLogPosition): TdxField; static;
    class procedure CopyQuoted(ASourcePieceTable: TdxPieceTable; ATargetPieceTable: TdxPieceTable;
      ASourceInterval: TdxDocumentLogInterval; ALogPosition: TdxDocumentLogPosition); static;
    class procedure CopyCore(ASourcePieceTable, ATargetPieceTable: TdxCustomPieceTable); overload; static;
    class procedure CopyCore(ASourcePieceTable: TdxPieceTable; ATargetPieceTable: TdxPieceTable;
      ASourceInterval: TdxDocumentLogInterval; ALogPosition: TdxDocumentLogPosition); overload; static;
    class procedure CopyCore(ASourcePieceTable: TdxPieceTable; ATargetPieceTable: TdxPieceTable;
      ASourceInterval: TdxDocumentLogInterval; ALogPosition: TdxDocumentLogPosition; ASuppressJoinTables: Boolean); overload; static;
    class procedure CopyCore(ASourcePieceTable: TdxPieceTable; ATargetPieceTable: TdxPieceTable;
      ASourceInterval: TdxDocumentLogInterval; ALogPosition: TdxDocumentLogPosition; ASuppressJoinTables: Boolean;
      ASuppressFieldsUpdate: Boolean; AUpdateFieldOperationType: TdxUpdateFieldOperationType); overload; static;
  end;

implementation

uses
  Contnrs,
  dxRichEdit.DocumentModel.CopyManager,
  dxRichEdit.DocumentModel.Notes;

{ TdxCopyHelper }

class procedure TdxCopyHelper.CopyDocumentModel(ASource: TdxDocumentModel; ATarget: TdxDocumentModel;
  AIncludeUnreferenced: Boolean);
begin
  CopyDocumentModelProperties(ASource, ATarget);
  CopyCore(ASource.MainPieceTable, ATarget.MainPieceTable);
  CopySections(ASource.Sections, ATarget.Sections);
  if AIncludeUnreferenced then
    CopyUnreferencedNotes(ASource, ATarget);
end;

class procedure TdxCopyHelper.CopyDocumentModelProperties(ASource: TdxDocumentModel; ATarget: TdxDocumentModel);
begin
  ATarget.DefaultCharacterProperties.CopyFrom(ASource.DefaultCharacterProperties.Info);
  ATarget.DefaultParagraphProperties.CopyFrom(ASource.DefaultParagraphProperties.Info);
  ATarget.DefaultTableCellProperties.CopyFrom(ASource.DefaultTableCellProperties.Info);
  ATarget.DefaultTableProperties.CopyFrom(ASource.DefaultTableProperties.Info);
  ATarget.DefaultTableRowProperties.CopyFrom(ASource.DefaultTableRowProperties.Info);
end;

class procedure TdxCopyHelper.CopySections(ASource: TdxSectionCollection; ATarget: TdxSectionCollection);
var
  ACount, I: TdxSectionIndex;
begin
  Assert(ASource.Count = ATarget.Count);
  ACount := ASource.Count;
  for I := 0 to ACount - 1 do
    CopySectionCore(ASource[I], ATarget[I]);
end;

class procedure TdxCopyHelper.CopySectionCore(ASourceSection: TdxSection; ATargetSection: TdxSection);
begin
  if ASourceSection.InnerFirstPageHeader = nil then
    ATargetSection.Headers.LinkToPrevious(TdxHeaderFooterType.First)
  else
  begin
    ATargetSection.Headers.Add(TdxHeaderFooterType.First);
    CopyCore(ASourceSection.InnerFirstPageHeader.PieceTable, ATargetSection.InnerFirstPageHeader.PieceTable);
  end;
  if ASourceSection.InnerOddPageHeader = nil then
    ATargetSection.Headers.LinkToPrevious(TdxHeaderFooterType.Odd)
  else
  begin
    ATargetSection.Headers.Add(TdxHeaderFooterType.Odd);
    CopyCore(ASourceSection.InnerOddPageHeader.PieceTable, ATargetSection.InnerOddPageHeader.PieceTable);
  end;
  if ASourceSection.InnerEvenPageHeader = nil then
    ATargetSection.Headers.LinkToPrevious(TdxHeaderFooterType.Even)
  else
  begin
    ATargetSection.Headers.Add(TdxHeaderFooterType.Even);
    CopyCore(ASourceSection.InnerEvenPageHeader.PieceTable, ATargetSection.InnerEvenPageHeader.PieceTable);
  end;

  if ASourceSection.InnerFirstPageFooter = nil then
    ATargetSection.Footers.LinkToPrevious(TdxHeaderFooterType.First)
  else
  begin
    ATargetSection.Footers.Add(TdxHeaderFooterType.First);
    CopyCore(ASourceSection.InnerFirstPageFooter.PieceTable, ATargetSection.InnerFirstPageFooter.PieceTable);
  end;
  if ASourceSection.InnerOddPageFooter = nil then
    ATargetSection.Footers.LinkToPrevious(TdxHeaderFooterType.Odd)
  else
  begin
    ATargetSection.Footers.Add(TdxHeaderFooterType.Odd);
    CopyCore(ASourceSection.InnerOddPageFooter.PieceTable, ATargetSection.InnerOddPageFooter.PieceTable);
  end;
  if ASourceSection.InnerEvenPageFooter = nil then
    ATargetSection.Footers.LinkToPrevious(TdxHeaderFooterType.Even)
  else
  begin
    ATargetSection.Footers.Add(TdxHeaderFooterType.Even);
    CopyCore(ASourceSection.InnerEvenPageFooter.PieceTable, ATargetSection.InnerEvenPageFooter.PieceTable);
  end;
  ATargetSection.CopyFromCore(ASourceSection);
end;

class procedure TdxCopyHelper.CopyUnreferencedNotes(ASource: TdxDocumentModel; ATarget: TdxDocumentModel);
var
  ACount, I: Integer;
  AFootNote: TdxFootNote;
  AEndNote: TdxEndNote;
begin
  ACount := ASource.FootNotes.Count;
  for I := 0 to ACount - 1 do
    if not ASource.FootNotes[I].IsReferenced then
    begin
      AFootNote := TdxFootNote.Create(ATarget);
      ATarget.UnsafeEditor.InsertFirstParagraph(TdxPieceTable(AFootNote.PieceTable));
      CopyCore(ASource.FootNotes[I].PieceTable, AFootNote.PieceTable);
      ATarget.FootNotes.Add(AFootNote);
    end;

  ACount := ASource.EndNotes.Count;
  for I := 0 to ACount - 1 do
    if not ASource.EndNotes[I].IsReferenced then
    begin
      AEndNote := TdxEndNote.Create(ATarget);
      ATarget.UnsafeEditor.InsertFirstParagraph(TdxPieceTable(AEndNote.PieceTable));
      CopyCore(ASource.EndNotes[I].PieceTable, AEndNote.PieceTable);
      ATarget.EndNotes.Add(AEndNote);
    end;
end;

class function TdxCopyHelper.CopyAndWrapToField(ASourcePieceTable: TdxPieceTable; ATargetPieceTable: TdxPieceTable;
  ASourceInterval: TdxDocumentLogInterval; ALogPosition: TdxDocumentLogPosition): TdxField;
var
  AField: TdxField;
begin
  AField := ATargetPieceTable.CreateField(ALogPosition, 0);
  CopyCore(ASourcePieceTable, ATargetPieceTable, ASourceInterval, ALogPosition + 1);
  Result := AField;
end;

class procedure TdxCopyHelper.CopyQuoted(ASourcePieceTable: TdxPieceTable; ATargetPieceTable: TdxPieceTable;
  ASourceInterval: TdxDocumentLogInterval; ALogPosition: TdxDocumentLogPosition);
begin
  ATargetPieceTable.InsertText(ALogPosition, '""');
  CopyCore(ASourcePieceTable, ATargetPieceTable, ASourceInterval, ALogPosition + 1);
end;

class procedure TdxCopyHelper.CopyCore(ASourcePieceTable, ATargetPieceTable: TdxCustomPieceTable);
begin
  CopyCore(TdxPieceTable(ASourcePieceTable), TdxPieceTable(ATargetPieceTable),
    TdxDocumentLogInterval.Create(0, TdxPieceTable(ASourcePieceTable).DocumentEndLogPosition - TdxPieceTable(ASourcePieceTable).DocumentStartLogPosition),
    0, False);
end;

class procedure TdxCopyHelper.CopyCore(ASourcePieceTable: TdxPieceTable; ATargetPieceTable: TdxPieceTable;
  ASourceInterval: TdxDocumentLogInterval; ALogPosition: TdxDocumentLogPosition);
begin
  CopyCore(ASourcePieceTable, ATargetPieceTable, ASourceInterval, ALogPosition, False);
end;

class procedure TdxCopyHelper.CopyCore(ASourcePieceTable: TdxPieceTable; ATargetPieceTable: TdxPieceTable;
  ASourceInterval: TdxDocumentLogInterval; ALogPosition: TdxDocumentLogPosition; ASuppressJoinTables: Boolean);
begin
  CopyCore(ASourcePieceTable, ATargetPieceTable, ASourceInterval, ALogPosition, ASuppressJoinTables, True,
    TdxUpdateFieldOperationType.Copy);
end;

class procedure TdxCopyHelper.CopyCore(ASourcePieceTable: TdxPieceTable; ATargetPieceTable: TdxPieceTable;
  ASourceInterval: TdxDocumentLogInterval; ALogPosition: TdxDocumentLogPosition; ASuppressJoinTables: Boolean;
  ASuppressFieldsUpdate: Boolean; AUpdateFieldOperationType: TdxUpdateFieldOperationType);
var
  ACopyManager: TdxDocumentModelCopyManager;
  ARunIndex: TdxRunIndex;
  ARangeStartLogPosition: TdxDocumentLogPosition;
  AOperation: TdxCopySectionOperation;
begin
  ACopyManager := TdxDocumentModelCopyManager.Create(ASourcePieceTable, ATargetPieceTable,
    TdxParagraphNumerationCopyOptions.CopyAlways);
  ACopyManager.TargetPosition.LogPosition := ALogPosition;
  ACopyManager.TargetPosition.ParagraphIndex := ATargetPieceTable.FindParagraphIndex(ALogPosition);
  ARangeStartLogPosition := ATargetPieceTable.FindRunStartLogPosition(ATargetPieceTable.Paragraphs[ACopyManager.TargetPosition.ParagraphIndex],
    ALogPosition, ARunIndex);
  if ARangeStartLogPosition <> ALogPosition then
  begin
    ATargetPieceTable.SplitTextRun(ACopyManager.TargetPosition.ParagraphIndex, ARunIndex, ALogPosition - ARangeStartLogPosition);
    ARangeStartLogPosition := ALogPosition;
    Inc(ARunIndex);
  end;
  ACopyManager.TargetPosition.RunStartLogPosition := ARangeStartLogPosition;
  ACopyManager.TargetPosition.RunIndex := ARunIndex;

  AOperation := TdxCopySectionOperation(ASourcePieceTable.DocumentModel.CreateCopySectionOperation(ACopyManager));
  AOperation.SuppressFieldsUpdate := ASuppressFieldsUpdate;
  AOperation.SuppressJoinTables := ASuppressJoinTables;
  AOperation.UpdateFieldOperationType := AUpdateFieldOperationType;
  AOperation.FixLastParagraph := False;
  AOperation.Execute(ASourceInterval.Start, ASourceInterval.Length, False);
end;

end.
