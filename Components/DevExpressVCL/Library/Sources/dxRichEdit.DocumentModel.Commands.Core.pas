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

unit dxRichEdit.DocumentModel.Commands.Core;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core;

type
  { TdxCustomPieceTableCommand }

  TdxCustomPieceTableCommand = class abstract
  private
    FPieceTable: TdxCustomPieceTable;
    FResult: TObject;
    FTransaction: TdxHistoryTransaction;
    function GetDocumentModel: TdxCustomDocumentModel;
  protected
    procedure BeginExecute; virtual;
    procedure EndExecute; virtual;

		procedure ExecuteCore; virtual; abstract;
    procedure CalculateExecutionParameters; virtual; abstract;
    procedure CalculateApplyChangesParameters; virtual; abstract;
    procedure ApplyChanges; virtual; abstract;
  public
    constructor Create(APieceTable: TdxCustomPieceTable);
    procedure Execute; virtual;

    property DocumentModel: TdxCustomDocumentModel read GetDocumentModel;
    property PieceTable: TdxCustomPieceTable read FPieceTable;
    property &Result: TObject read FResult write FResult;
    property Transaction: TdxHistoryTransaction read FTransaction;
  end;

  { TdxPieceTableInsertObjectCommand }

  TdxPieceTableInsertObjectCommand = class abstract (TdxCustomPieceTableCommand)
  private
    FParagraphIndex: TdxParagraphIndex;
    FApplyChangesParagraphIndex: TdxParagraphIndex;
  protected
    function GetChangeType: TdxDocumentModelChangeType; virtual; abstract;
    procedure CalculateExecutionParameters; override;
    procedure CalculateApplyChangesParameters; override;
    procedure ApplyChanges; override;
    function CalculateInsertionParagraphIndex: TdxParagraphIndex; virtual; abstract;
  public
    property ParagraphIndex: TdxParagraphIndex read FParagraphIndex;
    property ChangeType: TdxDocumentModelChangeType read GetChangeType;
  end;

  { TdxPieceTableInsertObjectAtLogPositionCommand }

  TdxPieceTableInsertObjectAtLogPositionCommand = class abstract (TdxPieceTableInsertObjectCommand)
  private
    FForceVisible: Boolean;
    FLogPosition: TdxDocumentLogPosition;
  protected
    function CalculateInsertionParagraphIndex: TdxParagraphIndex; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean); reintroduce;

    property ForceVisible: Boolean read FForceVisible;
    property LogPosition: TdxDocumentLogPosition read FLogPosition;
  end;

implementation

uses
  dxRichEdit.Utils.Exceptions;

{ TdxCustomPieceTableCommand }

constructor TdxCustomPieceTableCommand.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create;
  Assert(APieceTable <> nil);
  FPieceTable := APieceTable;
end;

procedure TdxCustomPieceTableCommand.BeginExecute;
begin
  DocumentModel.BeginUpdate;
  FTransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  CalculateExecutionParameters;
  CalculateApplyChangesParameters;
end;

procedure TdxCustomPieceTableCommand.EndExecute;
begin
  ApplyChanges;
  FreeAndNil(FTransaction);
  DocumentModel.EndUpdate;
end;

procedure TdxCustomPieceTableCommand.Execute;
begin
  BeginExecute;
  try
    ExecuteCore;
  finally
    EndExecute;
  end;
end;

function TdxCustomPieceTableCommand.GetDocumentModel: TdxCustomDocumentModel;
begin
  Result := FPieceTable.DocumentModel;
end;

{ TdxPieceTableInsertObjectCommand }

procedure TdxPieceTableInsertObjectCommand.ApplyChanges;
var
  AFirstParagraph, ALastParagraph: TdxParagraphBase;
begin
  AFirstParagraph := PieceTable.Paragraphs[FApplyChangesParagraphIndex];
  ALastParagraph := PieceTable.Paragraphs[ParagraphIndex];
  PieceTable.ApplyChanges(ChangeType, AFirstParagraph.FirstRunIndex, ALastParagraph.LastRunIndex);
end;

procedure TdxPieceTableInsertObjectCommand.CalculateApplyChangesParameters;
var
  ASection: TdxCustomSection;
  ASectionIndex: TdxSectionIndex;
  AFirstParagraph, ALastParagraph: TdxParagraphBase;
begin
  ASectionIndex := PieceTable.LookupSectionIndexByParagraphIndex(ParagraphIndex);
  if ASectionIndex > 0 then
  begin
    ASection := DocumentModel.Sections[ASectionIndex];
    if (ASection.LastParagraphIndex = ParagraphIndex) and (ParagraphIndex > 0) then
      FApplyChangesParagraphIndex := ParagraphIndex - 1
    else
      FApplyChangesParagraphIndex := ParagraphIndex;
  end
  else
    FApplyChangesParagraphIndex := ParagraphIndex;
  AFirstParagraph := PieceTable.Paragraphs[FApplyChangesParagraphIndex];
  ALastParagraph := PieceTable.Paragraphs[ParagraphIndex];
  PieceTable.ApplyChanges(ChangeType, AFirstParagraph.FirstRunIndex, ALastParagraph.LastRunIndex + 1);
end;

procedure TdxPieceTableInsertObjectCommand.CalculateExecutionParameters;
begin
  FParagraphIndex := CalculateInsertionParagraphIndex;
end;

{ TdxPieceTableInsertObjectAtLogPositionCommand }

function TdxPieceTableInsertObjectAtLogPositionCommand.CalculateInsertionParagraphIndex: TdxParagraphIndex;
begin
  Result := PieceTable.FindParagraphIndex(LogPosition);
end;

constructor TdxPieceTableInsertObjectAtLogPositionCommand.Create(APieceTable: TdxCustomPieceTable;
  ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean);
begin
  inherited Create(APieceTable);
  if ALogPosition < 0 then
    TdxRichEditExceptions.ThrowArgumentException('logPosition', ALogPosition);
  FLogPosition := ALogPosition;
  FForceVisible := AForceVisible;
end;

end.
