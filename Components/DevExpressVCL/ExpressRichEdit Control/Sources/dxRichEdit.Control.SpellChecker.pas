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

unit dxRichEdit.Control.SpellChecker;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Controls,
  dxSpellCheckerCore,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.PieceTableIterators,
  dxRichEdit.Control,
  dxRichEdit.InnerControl,
  dxRichEdit.InnerControl.SpellCheckerController,
  dxRichEdit.View.Core;

type

  { TdxSpellCheckerRichEditControlTextController }

  TdxSpellCheckerRichEditControlTextController = class(TdxRichEditTextController)
  strict private
    FWordIterator: TdxSpellCheckerWordIterator;
  protected
    function GetWordIterator(APieceTable: TdxCustomPieceTable): TdxSpellCheckerWordIterator; override;
  public
    destructor Destroy; override;
    procedure Reset;

    function GetSentenceFinishPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition; override;
    function GetSentenceStartPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition; override;
  end;

  { TdxSpellCheckerRichEditControlAdapter }

  TdxSpellCheckerRichEditControlAdapter = class(TInterfacedObject, IdxSpellCheckerAdapter)
  strict private
    FController: TdxSpellCheckerRichEditControlTextController;
    FEdit: TdxCustomRichEditControl;

    function GetDocumentModel: TdxDocumentModel;
    function GetInnerControl: TdxInnerRichEditControl;
    function GetPieceTable: TdxPieceTable;
    function GetInnerSelection: TdxSelection; overload;
  protected
    function CreatePositionFromLogPosition(ALogPosition: Integer): IdxSpellCheckerPosition;

    // IdxSpellCheckerAdapter
    function CreateController: IdxSpellCheckTextController; virtual;
    function GetEdit: TWinControl; virtual;
    function GetEditorHandle: THandle; virtual;
    function GetHideSelection: Boolean; virtual;
    function GetReadOnly: Boolean; virtual;
    procedure GetSelection(out AStart, AFinish: IdxSpellCheckerPosition); overload; virtual;
    procedure GetSpellingBounds(out AStart, AFinish: IdxSpellCheckerPosition); virtual;
    procedure Post(AUpdateValue: Boolean = True); virtual;
    procedure Replace(var AStart, AFinish: IdxSpellCheckerPosition; const AWord: string;
      var ASpellingStart, ASpellingFinish: IdxSpellCheckerPosition); virtual;
    procedure SetHideSelection(AValue: Boolean); virtual;
    procedure SetSelection(const AStart, AFinish: IdxSpellCheckerPosition); virtual;
    procedure UpdateController(AController: IdxSpellCheckTextController); virtual;
  public
    constructor Create(AEdit: TdxCustomRichEditControl);
    destructor Destroy; override;
    class function CreateAdapter(AObject: TObject): IdxSpellCheckerAdapter;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property Edit: TdxCustomRichEditControl read FEdit;
    property InnerControl: TdxInnerRichEditControl read GetInnerControl;
    property PieceTable: TdxPieceTable read GetPieceTable;
    property Selection: TdxSelection read GetInnerSelection;
  end;

implementation

uses
  Math;

{ TdxSpellCheckerRichEditControlTextController }

destructor TdxSpellCheckerRichEditControlTextController.Destroy;
begin
  FreeAndNil(FWordIterator);
  inherited Destroy;
end;

function TdxSpellCheckerRichEditControlTextController.GetWordIterator(APieceTable: TdxCustomPieceTable): TdxSpellCheckerWordIterator;
begin
  if (FWordIterator = nil) or (FWordIterator.PieceTable <> APieceTable) then
  begin
    FreeAndNil(FWordIterator);
    FWordIterator := TdxSpellCheckerWordIterator.Create(TdxPieceTable(APieceTable));
  end;
  Result := FWordIterator;
end;

procedure TdxSpellCheckerRichEditControlTextController.Reset;
begin
  FreeAndNil(FWordIterator);
end;

function TdxSpellCheckerRichEditControlTextController.GetSentenceFinishPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
begin
  Result := inherited GetSentenceFinishPosition(APos);
  Reset;
end;

function TdxSpellCheckerRichEditControlTextController.GetSentenceStartPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
begin
  Result := inherited GetSentenceStartPosition(APos);
  Reset;
end;

{ TdxSpellCheckerRichEditControlAdapter }

constructor TdxSpellCheckerRichEditControlAdapter.Create(AEdit: TdxCustomRichEditControl);
begin
  inherited Create;
  FEdit := AEdit;
  FController := TdxSpellCheckerRichEditControlTextController.Create(InnerControl);
end;

destructor TdxSpellCheckerRichEditControlAdapter.Destroy;
begin
  FreeAndNil(FController);
  inherited Destroy;
end;

class function TdxSpellCheckerRichEditControlAdapter.CreateAdapter(AObject: TObject): IdxSpellCheckerAdapter;
begin
  Result := Create(AObject as TdxCustomRichEditControl);
end;

function TdxSpellCheckerRichEditControlAdapter.CreatePositionFromLogPosition(ALogPosition: Integer): IdxSpellCheckerPosition;
var
  APosition: TdxDocumentModelPosition;
begin
  APosition := TdxDocumentModelPosition.Create(PieceTable);
  APosition.LogPosition := ALogPosition;
  APosition.Update;
  Result := TdxDocumentPosition.Create(APosition);
end;

function TdxSpellCheckerRichEditControlAdapter.CreateController: IdxSpellCheckTextController;
begin
  Result := FController;
end;

function TdxSpellCheckerRichEditControlAdapter.GetEdit: TWinControl;
begin
  Result := Edit;
end;

function TdxSpellCheckerRichEditControlAdapter.GetEditorHandle: THandle;
begin
  if Edit.HandleAllocated then
    Result := Edit.Handle
  else
    Result := 0;
end;

function TdxSpellCheckerRichEditControlAdapter.GetHideSelection: Boolean;
begin
  Result := False;
end;

function TdxSpellCheckerRichEditControlAdapter.GetReadOnly: Boolean;
begin
  Result := InnerControl.ReadOnly;
end;

procedure TdxSpellCheckerRichEditControlAdapter.GetSelection(out AStart, AFinish: IdxSpellCheckerPosition);
begin
  AFinish := CreatePositionFromLogPosition(Max(0, Min(PieceTable.DocumentEndLogPosition, Selection.&End)));
  AStart := CreatePositionFromLogPosition(Max(0, Min(PieceTable.DocumentEndLogPosition, Selection.Start)));
end;

procedure TdxSpellCheckerRichEditControlAdapter.GetSpellingBounds(out AStart, AFinish: IdxSpellCheckerPosition);
begin
  AFinish := TdxDocumentPosition.Create(TdxDocumentModelPosition.FromDocumentEnd(PieceTable));
  AStart := TdxDocumentPosition.Create(TdxDocumentModelPosition.Create(PieceTable));
end;

procedure TdxSpellCheckerRichEditControlAdapter.Post(AUpdateValue: Boolean);
begin
  // do nothing
end;

procedure TdxSpellCheckerRichEditControlAdapter.Replace(var AStart, AFinish: IdxSpellCheckerPosition;
  const AWord: string; var ASpellingStart, ASpellingFinish: IdxSpellCheckerPosition);
var
  ASpellingFinishLogPosition: Integer;
  ASpellingStartLogPosition: Integer;
  AStartLogPosition: Integer;
  ADelta: Integer;
begin
  AStartLogPosition := TdxDocumentPosition(AStart).Position.LogPosition;
  ASpellingStartLogPosition := TdxDocumentPosition(ASpellingStart).Position.LogPosition;
  ASpellingFinishLogPosition := TdxDocumentPosition(ASpellingFinish).Position.LogPosition;
  ADelta := Length(AWord) - (TdxDocumentPosition(AFinish).Position.LogPosition - AStartLogPosition);

  if FController.ReplaceWord(AStart, AFinish, AWord) then
  begin
    if AStartLogPosition < ASpellingStartLogPosition then
      Inc(ASpellingStartLogPosition, ADelta);
    if AStartLogPosition < ASpellingFinishLogPosition then
      Inc(ASpellingFinishLogPosition, ADelta);

    ASpellingStart := CreatePositionFromLogPosition(ASpellingStartLogPosition);
    ASpellingFinish := CreatePositionFromLogPosition(ASpellingFinishLogPosition);
    AFinish := CreatePositionFromLogPosition(AStartLogPosition + Length(AWord));
    FController.Reset;
  end;
end;

procedure TdxSpellCheckerRichEditControlAdapter.SetHideSelection(AValue: Boolean);
begin
  // do nothing
end;

procedure TdxSpellCheckerRichEditControlAdapter.SetSelection(const AStart, AFinish: IdxSpellCheckerPosition);
var
  ALogPosition: Integer;
begin
  DocumentModel.BeginUpdate;
  try
    Selection.BeginUpdate;
    try
      Selection.ClearMultiSelection;

      ALogPosition := TdxDocumentPosition(AStart).Position.LogPosition;
      Selection.SetStartCell(ALogPosition);
      Selection.Start := ALogPosition;

      ALogPosition := TdxDocumentPosition(AFinish).Position.LogPosition;
      Selection.&End := ALogPosition;
      Selection.UpdateTableSelectionEnd(ALogPosition);
    finally
      Selection.EndUpdate;
    end;
    Edit.EnsureCaretVisible(False);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxSpellCheckerRichEditControlAdapter.UpdateController(AController: IdxSpellCheckTextController);
begin
  // do nothing
end;

function TdxSpellCheckerRichEditControlAdapter.GetDocumentModel: TdxDocumentModel;
begin
  Result := Edit.DocumentModel;
end;

function TdxSpellCheckerRichEditControlAdapter.GetInnerControl: TdxInnerRichEditControl;
begin
  Result := Edit.InnerControl;
end;

function TdxSpellCheckerRichEditControlAdapter.GetInnerSelection: TdxSelection;
begin
  Result := DocumentModel.Selection;
end;

function TdxSpellCheckerRichEditControlAdapter.GetPieceTable: TdxPieceTable;
begin
  Result := DocumentModel.ActivePieceTable;
end;

initialization
  TdxSpellCheckerAdapters.Register(TdxCustomRichEditControl, TdxSpellCheckerRichEditControlAdapter.CreateAdapter);

finalization
  TdxSpellCheckerAdapters.Unregister(TdxCustomRichEditControl);
end.
