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

unit dxRichEdit.DocumentModel.NotesRange;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.ChunkedStringBuilder,
  dxGenerics,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.CopyManager.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.CopyManager.Simple,
  dxRichEdit.DocumentModel.CopyManager,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.Notes,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.LayoutEngine.Formatter;

type
  { TdxLayoutDependentTextRun }

  TdxLayoutDependentTextRun = class(TdxTextRun)
  private
    FFieldResultFormatting: TdxFieldResultFormatting;
  protected
    procedure CopyContentCore(ACopyManager: TdxSimpleDocumentModelCopyManager); override;
    function CalculateRowProcessingFlags: TdxRowProcessingFlags; override;
    procedure InheritRowProcessgFlags(ARun: TdxTextRunBase); override;
  public
    destructor Destroy; override;
    function CreateRun(AParagraph: TdxSimpleParagraph; AStartIndex, ALength: Integer): TdxTextRun; override;
    function CanPlaceCaretBefore: Boolean; override;
    function CanJoinWith(ARun: TdxTextRunBase): Boolean; override;
    function GetText(ABuffer: TdxChunkedStringBuilder; AFrom: Integer; ATo: Integer): string; override;
    procedure Export(const AExporter: IdxSimpleDocumentModelExporter); override;
    procedure Measure(ABoxInfo: TdxBoxInfo; const AMeasurer: IdxObjectMeasurer); override;
    function TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean; override;

    property FieldResultFormatting: TdxFieldResultFormatting read FFieldResultFormatting
      write FFieldResultFormatting;
  end;

  { TdxFootNoteRunBase }

  TdxFootNoteRunBase = class abstract(TdxLayoutDependentTextRun)
  strict private
    FNoteIndex: Integer;
    function GetNote: TdxFootNoteBase;
    function InternalGetNoteCollection: TdxFootNoteBaseCollection;
  protected
    function GetCounterId: string; virtual; abstract;
    function CreateNote(ADocumentModel: TdxCustomDocumentModel): TdxFootNoteBase; virtual; abstract;
    function GetNoteCollection(ADocumentModel: TdxCustomDocumentModel): TdxFootNoteBaseCollection; virtual; abstract;
    function CreateNoteCopy(ACopyManager: TdxCustomDocumentModelCopyManager): Integer;
    procedure CopyContentCore(ACopyManager: TdxSimpleDocumentModelCopyManager); override;
    procedure InsertFootNoteEndNoteRun(ACopyManager: TdxCustomDocumentModelCopyManager; AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; ANoteIndex: Integer); virtual; abstract;

    property CounterId: string read GetCounterId;
  public
    property NoteIndex: Integer read FNoteIndex write FNoteIndex;
    property Note: TdxFootNoteBase read GetNote;
    property NoteCollection: TdxFootNoteBaseCollection read InternalGetNoteCollection;
  end;
  TdxFootNoteRunBaseList = class(TdxList<TdxFootNoteRunBase>);

  { TdxFootNoteRun }

  TdxFootNoteRun = class(TdxFootNoteRunBase)
  protected
    function GetCounterId: string; override;
    function GetNoteCollection(ADocumentModel: TdxCustomDocumentModel): TdxFootNoteBaseCollection; override;
    function CreateNote(ADocumentModel: TdxCustomDocumentModel): TdxFootNoteBase; override;
    procedure InsertFootNoteEndNoteRun(ACopyManager: TdxCustomDocumentModelCopyManager;
      AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; ANoteIndex: Integer); override;
  public
    procedure Export(const AExporter: IdxSimpleDocumentModelExporter); override;
  end;

  { TdxEndNoteRun }

  TdxEndNoteRun = class(TdxFootNoteRunBase)
  protected
    function GetCounterId: string; override;
    function GetNoteCollection(ADocumentModel: TdxCustomDocumentModel): TdxFootNoteBaseCollection; override;
    function CreateNote(ADocumentModel: TdxCustomDocumentModel): TdxFootNoteBase; override;
    procedure InsertFootNoteEndNoteRun(ACopyManager: TdxCustomDocumentModelCopyManager; AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; ANoteIndex: Integer); override;
  public
    procedure Export(const AExporter: IdxSimpleDocumentModelExporter); override;
  end;

  { TdxFootNoteNumberResultFormattingBase }

  TdxFootNoteNumberResultFormattingBase = class abstract(TdxFieldResultFormatting)
  protected
    function DoGetLogPosition(APieceTable: TdxSimplePieceTable): TdxDocumentLogPosition; virtual; abstract;
    function GetCounterId: string; virtual; abstract;
    function GetRecalculateOnSecondaryFormatting: Boolean; override;
    function GetValueCore(AFormatter: TObject; ADocumentModel: TdxCustomDocumentModel): Integer; override;
    function GetLogPosition(AFormatter: TdxParagraphBoxFormatter): TdxDocumentLogPosition;
    function GetFootNoteProperties(ASection: TdxSection): TdxSectionFootNote; overload; virtual; abstract;
    function GetFootNoteProperties(AFormatter: TdxParagraphBoxFormatter): TdxSectionFootNote; overload; virtual;

    property CounterId: string read GetCounterId;
  public
    constructor Create;
    function ApplyImplicitFormatting(AFormatter: TObject; const AValue: string; AIntValue: Integer): string; override;
  end;

  { TdxFootNoteNumberResultFormatting }

  TdxFootNoteNumberResultFormatting = class(TdxFootNoteNumberResultFormattingBase)
  strict private
    class var
      FInstance: TdxFootNoteNumberResultFormatting;
  strict private
    class constructor Initialize;
    class destructor Finalize;
  protected
    function DoGetLogPosition(APieceTable: TdxSimplePieceTable): TdxDocumentLogPosition; override;
    function GetCounterId: string; override;
    function GetFootNoteProperties(ASection: TdxSection): TdxSectionFootNote; override;
  public
    class property Instance: TdxFootNoteNumberResultFormatting read FInstance;
  end;

  { TdxEndNoteNumberResultFormatting }

  TdxEndNoteNumberResultFormatting = class(TdxFootNoteNumberResultFormattingBase)
  strict private
    class var
      FInstance: TdxEndNoteNumberResultFormatting;
  strict private
    class constructor Initialize;
    class destructor Finalize;
  protected
    function DoGetLogPosition(APieceTable: TdxSimplePieceTable): TdxDocumentLogPosition; override;
    function GetCounterId: string; override;
    function GetFootNoteProperties(ASection: TdxSection): TdxSectionFootNote; override;
  public
    class property Instance: TdxEndNoteNumberResultFormatting read FInstance;
  end;

implementation

uses
  RTLConsts, Contnrs,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.Platform.Font;

{ TdxLayoutDependentTextRun }

destructor TdxLayoutDependentTextRun.Destroy;
begin
  FreeAndNil(FFieldResultFormatting);
  inherited Destroy;
end;

function TdxLayoutDependentTextRun.CanPlaceCaretBefore: Boolean;
begin
  Result := True;
end;

function TdxLayoutDependentTextRun.CanJoinWith(ARun: TdxTextRunBase): Boolean;
begin
  Result := False;
end;

procedure TdxLayoutDependentTextRun.Measure(ABoxInfo: TdxBoxInfo; const AMeasurer: IdxObjectMeasurer);
var
  AFontInfo: TdxFontInfo;
begin
  AFontInfo := DocumentModel.FontCache[FontCacheIndex];
  AMeasurer.MeasureText(ABoxInfo, TdxLayoutDependentTextBox(ABoxInfo.Box).CalculatedText, AFontInfo);
end;

function TdxLayoutDependentTextRun.GetText(ABuffer: TdxChunkedStringBuilder; AFrom: Integer;
  ATo: Integer): string;
begin
  Result := '#';
end;

procedure TdxLayoutDependentTextRun.Export(const AExporter: IdxSimpleDocumentModelExporter);
begin
  AExporter.Export(Self);
end;

function TdxLayoutDependentTextRun.CreateRun(AParagraph: TdxSimpleParagraph; AStartIndex, ALength: Integer): TdxTextRun;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := nil;
end;

procedure TdxLayoutDependentTextRun.CopyContentCore(ACopyManager: TdxSimpleDocumentModelCopyManager);
var
  AParagraphIndex: TdxParagraphIndex;
  ALogPosition: TdxDocumentLogPosition;
begin
  ALogPosition := ACopyManager.TargetPosition.LogPosition;
  AParagraphIndex := ACopyManager.TargetPosition.ParagraphIndex;
  TdxPieceTable(ACopyManager.TargetPieceTable).InsertLayoutDependentTextRun(AParagraphIndex, ALogPosition, FFieldResultFormatting.Clone);
end;

function TdxLayoutDependentTextRun.CalculateRowProcessingFlags: TdxRowProcessingFlags;
begin
  Result := inherited CalculateRowProcessingFlags + [TdxRowProcessingFlag.ProcessLayoutDependentText];
end;

procedure TdxLayoutDependentTextRun.InheritRowProcessgFlags(ARun: TdxTextRunBase);
begin
  RowProcessingFlags := ARun.RowProcessingFlags + [TdxRowProcessingFlag.ProcessLayoutDependentText];
end;

function TdxLayoutDependentTextRun.TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean;
begin
  Result := False;
end;

{ TdxFootNoteRunBase }

function TdxFootNoteRunBase.GetNote: TdxFootNoteBase;
begin
  Result := NoteCollection[NoteIndex];
end;

function TdxFootNoteRunBase.InternalGetNoteCollection: TdxFootNoteBaseCollection;
begin
  Result := GetNoteCollection(DocumentModel);
end;

function TdxFootNoteRunBase.CreateNoteCopy(ACopyManager: TdxCustomDocumentModelCopyManager): Integer;
var
  ASource, ATarget: TdxFootNoteBase;
  ASourcePieceTable, ATargetPieceTable: TdxPieceTable;
  ANoteCopyManager: TdxDocumentModelCopyManager;
  AOperation: TdxCopySectionOperation;
  ATargetNoteCollection: TdxFootNoteBaseCollection;
  ATargetNoteIndex: Integer;
begin
  ASource := Note;
  ATarget := CreateNote(ACopyManager.TargetModel);

  ASourcePieceTable := TdxPieceTable(ASource.PieceTable);
  ATargetPieceTable := TdxPieceTable(ATarget.PieceTable);
  ANoteCopyManager := TdxDocumentModelCopyManager.Create(ASourcePieceTable, ATargetPieceTable, TdxParagraphNumerationCopyOptions.CopyAlways);
  try
    ANoteCopyManager.TargetModel.UnsafeEditor.InsertFirstParagraph(ATargetPieceTable);

    AOperation := TdxCopySectionOperation(ASourcePieceTable.DocumentModel.CreateCopySectionOperation(ANoteCopyManager));
    try
      AOperation.FixLastParagraph := True;
      AOperation.Execute(ASourcePieceTable.DocumentStartLogPosition, ASourcePieceTable.DocumentEndLogPosition - ASourcePieceTable.DocumentStartLogPosition + 1, False);
    finally
      AOperation.Free;
    end;

    ATargetNoteCollection := GetNoteCollection(ACopyManager.TargetModel);
    ATargetNoteIndex := ATargetNoteCollection.Count;
    ATargetNoteCollection.Add(ATarget);
    Result := ATargetNoteIndex;
  finally
    ANoteCopyManager.Free;
  end;
end;

procedure TdxFootNoteRunBase.CopyContentCore(ACopyManager: TdxSimpleDocumentModelCopyManager);
var
  ALogPosition: TdxDocumentLogPosition;
  AParagraphIndex: TdxParagraphIndex;
  ATargetNoteIndex: Integer;
begin
  ALogPosition := ACopyManager.TargetPosition.LogPosition;
  AParagraphIndex := ACopyManager.TargetPosition.ParagraphIndex;

  if TdxPieceTable(ACopyManager.TargetPieceTable).IsNote then
  begin
    InsertFootNoteEndNoteRun(ACopyManager, AParagraphIndex, ALogPosition, -1);
  end
  else
  begin
    ATargetNoteIndex := CreateNoteCopy(ACopyManager);
    InsertFootNoteEndNoteRun(ACopyManager, AParagraphIndex, ALogPosition, ATargetNoteIndex);
  end;
end;

{ TdxFootNoteRun }

function TdxFootNoteRun.GetCounterId: string;
begin
  Result := TdxFootNote.FootNoteCounterId;
end;

function TdxFootNoteRun.GetNoteCollection(ADocumentModel: TdxCustomDocumentModel): TdxFootNoteBaseCollection;
begin
  Result := TdxDocumentModel(ADocumentModel).FootNotes;
end;

function TdxFootNoteRun.CreateNote(ADocumentModel: TdxCustomDocumentModel): TdxFootNoteBase;
begin
  Result := TdxFootNote.Create(ADocumentModel);
end;

procedure TdxFootNoteRun.Export(const AExporter: IdxSimpleDocumentModelExporter);
begin
  AExporter.Export(Self);
end;

procedure TdxFootNoteRun.InsertFootNoteEndNoteRun(ACopyManager: TdxCustomDocumentModelCopyManager;
  AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; ANoteIndex: Integer);
begin
  TdxPieceTable(ACopyManager.TargetPieceTable).InsertFootNoteRun(AParagraphIndex, ALogPosition, ANoteIndex);
end;

{ TdxEndNoteRun }

function TdxEndNoteRun.GetCounterId: string;
begin
  Result := TdxEndNote.EndNoteCounterId;
end;

function TdxEndNoteRun.GetNoteCollection(ADocumentModel: TdxCustomDocumentModel): TdxFootNoteBaseCollection;
begin
  Result := TdxDocumentModel(ADocumentModel).EndNotes;
end;

function TdxEndNoteRun.CreateNote(ADocumentModel: TdxCustomDocumentModel): TdxFootNoteBase;
begin
  Result := TdxEndNote.Create(ADocumentModel);
end;

procedure TdxEndNoteRun.Export(const AExporter: IdxSimpleDocumentModelExporter);
begin
  AExporter.Export(Self);
end;

procedure TdxEndNoteRun.InsertFootNoteEndNoteRun(ACopyManager: TdxCustomDocumentModelCopyManager;
  AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; ANoteIndex: Integer);
begin
  TdxPieceTable(ACopyManager.TargetPieceTable).InsertEndNoteRun(AParagraphIndex, ALogPosition, ANoteIndex);
end;

{ TdxFootNoteNumberResultFormattingBase }

constructor TdxFootNoteNumberResultFormattingBase.Create;
begin
  inherited Create('', TArray<string>.Create());
end;

function TdxFootNoteNumberResultFormattingBase.GetRecalculateOnSecondaryFormatting: Boolean;
begin
  Result := False;
end;

function TdxFootNoteNumberResultFormattingBase.GetValueCore(AFormatter: TObject{TdxParagraphBoxFormatter}; ADocumentModel: TdxCustomDocumentModel): Integer;
var
  ACounter: TdxCounter;
  ALogPosition: TdxDocumentLogPosition;
  ATypedFormatter: TdxParagraphBoxFormatter;
begin
  ATypedFormatter := TdxParagraphBoxFormatter(AFormatter);
  ACounter := ATypedFormatter.RowsController.ColumnController.PageAreaController.PageController.DocumentLayout.Counters.GetCounter(CounterId);
  ALogPosition := GetLogPosition(ATypedFormatter);
  Inc(ALogPosition, ATypedFormatter.Iterator.Offset);
  Result := ACounter.Increment(ALogPosition);
end;

function TdxFootNoteNumberResultFormattingBase.GetLogPosition(AFormatter: TdxParagraphBoxFormatter): TdxDocumentLogPosition;
var
  APieceTable: TdxPieceTable;
begin
  APieceTable := AFormatter.PieceTable;
  if APieceTable.IsNote then
    Result := DoGetLogPosition(APieceTable)
  else
    Result := APieceTable.GetRunLogPosition(AFormatter.Iterator.RunIndex);
end;

function TdxFootNoteNumberResultFormattingBase.GetFootNoteProperties(AFormatter: TdxParagraphBoxFormatter): TdxSectionFootNote;
begin
  Result := GetFootNoteProperties(AFormatter.RowsController.ColumnController.PageAreaController.PageController.CurrentSection);
end;

function TdxFootNoteNumberResultFormattingBase.ApplyImplicitFormatting(AFormatter: TObject; const AValue: string; AIntValue: Integer): string;
var
  ATypedFormatter: TdxParagraphBoxFormatter;
begin
  ATypedFormatter := TdxParagraphBoxFormatter(AFormatter);
  Result := GetFootNoteProperties(ATypedFormatter).FormatCounterValue(AIntValue);
end;

{ TdxFootNoteNumberResultFormatting }

class constructor TdxFootNoteNumberResultFormatting.Initialize;
begin
  FInstance := TdxFootNoteNumberResultFormatting.Create;
end;

class destructor TdxFootNoteNumberResultFormatting.Finalize;
begin
  FreeAndNil(FInstance)
end;

function TdxFootNoteNumberResultFormatting.DoGetLogPosition(APieceTable: TdxSimplePieceTable): TdxDocumentLogPosition;
var
  ANote: TdxFootNote;
  ARun: TdxLayoutDependentTextRun;
begin
  ANote := Safe<TdxFootNote>.Cast(TdxPieceTable(APieceTable).ContentType);
  Assert(ANote <> nil);
  ARun := TdxLayoutDependentTextRun(ANote.ReferenceRun);
  Assert(ARun <> nil);
  APieceTable := ARun.Paragraph.PieceTable;
  Result := APieceTable.GetRunLogPosition(ARun);
end;

function TdxFootNoteNumberResultFormatting.GetCounterId: string;
begin
  Result := TdxFootNote.FootNoteCounterId;
end;

function TdxFootNoteNumberResultFormatting.GetFootNoteProperties(ASection: TdxSection): TdxSectionFootNote;
begin
  Result := ASection.FootNote;
end;

{ TdxEndNoteNumberResultFormatting }

class constructor TdxEndNoteNumberResultFormatting.Initialize;
begin
  FInstance := TdxEndNoteNumberResultFormatting.Create;
end;

class destructor TdxEndNoteNumberResultFormatting.Finalize;
begin
  FreeAndNil(FInstance)
end;

function TdxEndNoteNumberResultFormatting.DoGetLogPosition(APieceTable: TdxSimplePieceTable): TdxDocumentLogPosition;
var
  ANote: TdxEndNote;
  ARun: TdxLayoutDependentTextRun;
begin
  ANote := Safe<TdxEndNote>.Cast(TdxPieceTable(APieceTable).ContentType);
  Assert(ANote <> nil);
  ARun := TdxLayoutDependentTextRun(ANote.ReferenceRun);
  Assert(ARun <> nil);
  APieceTable := ARun.Paragraph.PieceTable;
  Result := APieceTable.GetRunLogPosition(ARun);
end;

function TdxEndNoteNumberResultFormatting.GetCounterId: string;
begin
  Result := TdxEndNote.EndNoteCounterId;
end;

function TdxEndNoteNumberResultFormatting.GetFootNoteProperties(ASection: TdxSection): TdxSectionFootNote;
begin
  Result := ASection.EndNote;
end;

end.
