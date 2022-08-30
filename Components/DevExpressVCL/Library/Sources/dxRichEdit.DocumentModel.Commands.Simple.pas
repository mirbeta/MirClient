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

unit dxRichEdit.DocumentModel.Commands.Simple;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.InlineObjectFormatting,
  dxRichEdit.DocumentModel.CopyManager.Core,
  dxRichEdit.DocumentModel.Commands.Core;

type
  { TdxSimplePieceTableInsertObjectAtLogPositionCommand }

  TdxSimplePieceTableInsertObjectAtLogPositionCommand = class(TdxPieceTableInsertObjectAtLogPositionCommand)
  strict private
    function GetDocumentModel: TdxSimpleDocumentModel;
    function GetPieceTable: TdxSimplePieceTable;
  public
    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
    property PieceTable: TdxSimplePieceTable read GetPieceTable;
  end;

  { TdxSimplePieceTableInsertObjectCommand }

  TdxSimplePieceTableInsertObjectCommand = class(TdxPieceTableInsertObjectCommand)
  strict private
    function GetDocumentModel: TdxSimpleDocumentModel;
    function GetPieceTable: TdxSimplePieceTable;
  public
    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
    property PieceTable: TdxSimplePieceTable read GetPieceTable;
  end;

  { TdxSimplePieceTableCommand }

  TdxSimplePieceTableCommand = class(TdxCustomPieceTableCommand)
  strict private
    function GetDocumentModel: TdxSimpleDocumentModel;
    function GetPieceTable: TdxSimplePieceTable;
  public
    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
    property PieceTable: TdxSimplePieceTable read GetPieceTable;
  end;

  { TdxPieceTableInsertTextAtLogPositionCommand }

  TdxPieceTableInsertTextAtLogPositionCommand = class(TdxSimplePieceTableInsertObjectAtLogPositionCommand)
  private
    FText: string;
  protected
    function GetChangeType: TdxDocumentModelChangeType; override;
    procedure ExecuteCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; ALogPosition: TdxDocumentLogPosition;
      const AText: string; AForceVisible: Boolean);

    property Text: string read FText;
  end;

  { TdxPieceTableDeleteFieldWithoutResultCommand }

  TdxPieceTableDeleteFieldWithoutResultCommand = class(TdxSimplePieceTableCommand)
  private
    FField: TdxField;
    FRunIndex: TdxRunIndex;
  protected
    procedure ExecuteCore; override;
    procedure CalculateExecutionParameters; override;
    procedure CalculateApplyChangesParameters; override;
    procedure ApplyChanges; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; AField: TdxField); reintroduce;
  end;

  { TdxPieceTableCreateFieldCommand }

  TdxPieceTableCreateFieldCommand = class(TdxSimplePieceTableInsertObjectAtLogPositionCommand)
  private
    FLength: Integer;
    FInsertedField: TdxField;
  protected
    procedure ExecuteCore; override;
    function GetChangeType: TdxDocumentModelChangeType; override;
  public
    constructor Create(APieceTable: TdxSimplePieceTable; ALogPosition: TdxDocumentLogPosition; ALength: Integer; AForceVisible: Boolean); reintroduce;

    property Length: Integer read FLength;
    property InsertedField: TdxField read FInsertedField;
  end;

  { TdxPieceTableCreateFieldWithResultCommand }

  TdxPieceTableCreateFieldWithResultCommand = class(TdxSimplePieceTableInsertObjectCommand)
  private
    FInsertedField: TdxField;
    FStartCode: TdxDocumentLogPosition;
    FEndCode: TdxDocumentLogPosition;
    FResultLength: Integer;
    FForceVisible: Boolean;
  protected
    function GetChangeType: TdxDocumentModelChangeType; override;
    function CalculateInsertionParagraphIndex: TdxParagraphIndex; override;
    procedure ExecuteCore; override;
    function InsertStartCodeRun: TdxRunIndex; virtual;
    function InsertEndCodeRun(AEndCodePosition: TdxDocumentLogPosition): TdxRunIndex; virtual;
    function InsertEndResultRun(AEndResultPosition: TdxDocumentLogPosition): TdxRunIndex; virtual;
  public
    constructor Create(APieceTable: TdxSimplePieceTable; AStartCode, AEndCode: TdxDocumentLogPosition;
      AResultLength: Integer; AForceVisible: Boolean); reintroduce;

    property InsertedField: TdxField read FInsertedField;
  end;

  { TdxPieceTableInsertObjectAtInputPositionCommand }

  TdxPieceTableInsertObjectAtInputPositionCommand = class abstract (TdxSimplePieceTableInsertObjectCommand)
  private
    FForceVisible: Boolean;
    FPosition: TdxInputPosition;
  protected
    function CalculateInsertionParagraphIndex: TdxParagraphIndex; override;
  public
    constructor Create(APieceTable: TdxSimplePieceTable; APosition: TdxInputPosition; AForceVisible: Boolean); reintroduce;

    property Position: TdxInputPosition read FPosition;
    property ForceVisible: Boolean read FForceVisible;
  end;

  { TdxPieceTableInsertTextAtInputPositionCommand }

  TdxPieceTableInsertTextAtInputPositionCommand = class(TdxPieceTableInsertObjectAtInputPositionCommand)
  private
    FText: string;
  protected
    procedure ExecuteCore; override;
    function GetChangeType: TdxDocumentModelChangeType; override;
  public
    constructor Create(APieceTable: TdxSimplePieceTable; AInputPosition: TdxInputPosition;
      const AText: string; AForceVisible: Boolean); reintroduce;

    property Text: string read FText;
  end;

  { TdxPieceTableInsertInlinePictureAtLogPositionCommand }

  TdxPieceTableInsertInlinePictureAtLogPositionCommand = class(TdxSimplePieceTableInsertObjectAtLogPositionCommand)
  private
    FScaleX: Integer;
    FScaleY: Integer;
    FImage: TdxOfficeImageReference;
  protected
    procedure ExecuteCore; override;
    function GetChangeType: TdxDocumentModelChangeType; override;
  public
    constructor Create(APieceTable: TdxSimplePieceTable; ALogPosition: TdxDocumentLogPosition;
      AImage: TdxOfficeImageReference; AScaleX, AScaleY: Integer; AForceVisible: Boolean); reintroduce;
    destructor Destroy; override;

    property Image: TdxOfficeImageReference read FImage;
    property ScaleX: Integer read FScaleX;
    property ScaleY: Integer read FScaleY;
  end;

  { TdxPieceTableInsertInlineCustomObjectAtLogPositionCommand }

  TdxPieceTableInsertInlineCustomObjectAtLogPositionCommand = class(TdxSimplePieceTableInsertObjectAtLogPositionCommand)
  private
    FCustomObject: IdxInlineCustomObject;
    FScaleX: Integer;
    FScaleY: Integer;
  protected
    function GetChangeType: TdxDocumentModelChangeType; override;
    procedure ExecuteCore; override;
  public
    constructor Create(APieceTable: TdxSimplePieceTable; ALogPosition: TdxDocumentLogPosition;
      const ACustomObject: IdxInlineCustomObject; AScaleX, AScaleY: Integer; AForceVisible: Boolean);

    property CustomObject: IdxInlineCustomObject read FCustomObject;
    property ScaleX: Integer read FScaleX;
    property ScaleY: Integer read FScaleY;
  end;

  { TdxPieceTableDeleteTextCommand }

  TdxPieceTableDeleteTextCommand = class sealed(TdxSimplePieceTableCommand)
  private
    FLength: Integer;
    FForceRemoveInnerFields: Boolean;
    FLogPosition: TdxDocumentLogPosition;
    FBackspacePressed: Boolean;
    FDocumentLastParagraphSelected: Boolean;
    FAllowPartiallyDeletingField: Boolean;
    FLeaveFieldIfResultIsRemoved: Boolean;
    FParagraphIndex: TdxParagraphIndex;
    FSectionBreakDeleted: Boolean;
  protected
    procedure CalculateExecutionParameters; override;
    procedure ExecuteCore; override;
    function CreateDeleteContentOperation: TdxCustomDeleteContentOperation;
    procedure CalculateApplyChangesParameters; override;
    procedure ApplyChanges; override;
  public
    constructor Create(APieceTable: TdxSimplePieceTable; ALogPosition: TdxDocumentLogPosition; ALength: Integer); reintroduce;

    property DocumentLastParagraphSelected: Boolean read FDocumentLastParagraphSelected write FDocumentLastParagraphSelected;
    property LeaveFieldIfResultIsRemoved: Boolean read FLeaveFieldIfResultIsRemoved write FLeaveFieldIfResultIsRemoved;
    property LogPosition: TdxDocumentLogPosition read FLogPosition;
    property Length: Integer read FLength;
    property AllowPartiallyDeletingField: Boolean read FAllowPartiallyDeletingField write FAllowPartiallyDeletingField;
    property ForceRemoveInnerFields: Boolean read FForceRemoveInnerFields write FForceRemoveInnerFields;
    property BackspacePressed: Boolean read FBackspacePressed write FBackspacePressed;
  end;

  { TdxPieceTableInsertPlainTextAtLogPositionCommand }

  TdxPieceTableInsertPlainTextAtLogPositionCommand = class(TdxPieceTableInsertTextAtLogPositionCommand)
  protected
    procedure ExecuteCore; override;
    procedure InsertPlainTextCore(APosition: TdxDocumentLogPosition; const AText: string); virtual;
    class function GetNextLine(const ASource: string; var AIndex: Integer): string; static;
    class function ShouldAddParagraph(const ASource: string; var AIndex: Integer): Boolean; static;
  end;

  { TdxPieceTableInsertPlainTextAtInputPositionCommand }

  TdxPieceTableInsertPlainTextAtInputPositionCommand = class(TdxPieceTableInsertObjectAtInputPositionCommand)
  private
    FText: string;
  protected
    function GetChangeType: TdxDocumentModelChangeType; override;
    procedure ExecuteCore; override;
    procedure InsertPlainTextCore(APosition: TdxInputPosition; const AText: string); virtual;
  public
    constructor Create(APieceTable: TdxSimplePieceTable;
      AInputPosition: TdxInputPosition; const AText: string; AForceVisible: Boolean); reintroduce;

    property Text: string read FText;
  end;

  { TdxPieceTableInsertParagraphAtLogPositionCommand }

  TdxPieceTableInsertParagraphAtLogPositionCommand = class(TdxSimplePieceTableInsertObjectAtLogPositionCommand)
  protected
    function GetChangeType: TdxDocumentModelChangeType; override;
    procedure ExecuteCore; override;
  end;

  { TdxPieceTableInsertParagraphAtInputPositionCommand }

  TdxPieceTableInsertParagraphAtInputPositionCommand = class(TdxSimplePieceTableInsertObjectAtLogPositionCommand)
  strict private
    FInputPosition: TdxInputPosition;
  protected
    function GetChangeType: TdxDocumentModelChangeType; override;
    procedure ExecuteCore; override;
  public
    constructor Create(APieceTable: TdxSimplePieceTable; ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean;
      AInputPosition: TdxInputPosition); reintroduce;
  end;

implementation

uses
  Math,
  dxRichEdit.DocumentModel.History.FieldHistory,
  dxRichEdit.Utils.Exceptions;

{ TdxSimplePieceTableInsertObjectAtLogPositionCommand }

function TdxSimplePieceTableInsertObjectAtLogPositionCommand.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := TdxSimpleDocumentModel(inherited DocumentModel);
end;

function TdxSimplePieceTableInsertObjectAtLogPositionCommand.GetPieceTable: TdxSimplePieceTable;
begin
  Result := TdxSimplePieceTable(inherited PieceTable);
end;

{ TdxSimplePieceTableInsertObjectCommand }

function TdxSimplePieceTableInsertObjectCommand.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := TdxSimpleDocumentModel(inherited DocumentModel);
end;

function TdxSimplePieceTableInsertObjectCommand.GetPieceTable: TdxSimplePieceTable;
begin
  Result := TdxSimplePieceTable(inherited PieceTable);
end;

{ TdxSimplePieceTableCommand }

function TdxSimplePieceTableCommand.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := TdxSimpleDocumentModel(inherited DocumentModel);
end;

function TdxSimplePieceTableCommand.GetPieceTable: TdxSimplePieceTable;
begin
  Result := TdxSimplePieceTable(inherited PieceTable);
end;

{ TdxPieceTableInsertTextAtLogPositionCommand }

constructor TdxPieceTableInsertTextAtLogPositionCommand.Create(APieceTable: TdxCustomPieceTable;
  ALogPosition: TdxDocumentLogPosition; const AText: string; AForceVisible: Boolean);
begin
  inherited Create(APieceTable, ALogPosition, AForceVisible);
  if AText = '' then
    TdxRichEditExceptions.ThrowArgumentException('text', text);
  FText := AText;
end;

procedure TdxPieceTableInsertTextAtLogPositionCommand.ExecuteCore;
begin
  PieceTable.InsertTextCore(ParagraphIndex, LogPosition, FText, ForceVisible);
end;

function TdxPieceTableInsertTextAtLogPositionCommand.GetChangeType: TdxDocumentModelChangeType;
begin
  Result := TdxDocumentModelChangeType.InsertText;
end;

{ TdxPieceTableDeleteFieldWithoutResultCommand }

constructor TdxPieceTableDeleteFieldWithoutResultCommand.Create(APieceTable: TdxCustomPieceTable;
  AField: TdxField);
begin
  Assert(AField <> nil);
  inherited Create(APieceTable);
  FField := AField;
end;

procedure TdxPieceTableDeleteFieldWithoutResultCommand.ApplyChanges;
begin
  PieceTable.ApplyChanges(TdxDocumentModelChangeType.DeleteContent, FRunIndex, MaxInt);
end;

procedure TdxPieceTableDeleteFieldWithoutResultCommand.CalculateApplyChangesParameters;
begin
  FRunIndex := FField.Code.Start;
end;

procedure TdxPieceTableDeleteFieldWithoutResultCommand.CalculateExecutionParameters;
begin
end;

procedure TdxPieceTableDeleteFieldWithoutResultCommand.ExecuteCore;
var
  ACodeRunsCount: Integer;
begin
  PieceTable.RemoveField(FField);
  ACodeRunsCount := FField.Code.&End - FField.Code.Start + 1;
  DocumentModel.UnsafeEditor.DeleteRuns(PieceTable, FField.Code.Start, ACodeRunsCount);
  DocumentModel.UnsafeEditor.DeleteRuns(PieceTable, FField.Result.&End - ACodeRunsCount, 1);
end;

{ TdxPieceTableCreateFieldCommand }

constructor TdxPieceTableCreateFieldCommand.Create(APieceTable: TdxSimplePieceTable; ALogPosition: TdxDocumentLogPosition;
  ALength: Integer; AForceVisible: Boolean);
begin
  inherited Create(APieceTable, ALogPosition, AForceVisible);
  Assert(ALength >= 0);
  FLength := ALength;
end;

procedure TdxPieceTableCreateFieldCommand.ExecuteCore;
var
  AEndLogPosition: TdxDocumentLogPosition;
  AEndParagraphIndex: TdxParagraphIndex;
  AItem: TdxAddFieldHistoryItem;
begin
  AEndLogPosition := Min(LogPosition + Length, PieceTable.DocumentEndLogPosition);
  AEndParagraphIndex := PieceTable.FindParagraphIndex(AEndLogPosition);
  DocumentModel.History.BeginTransaction;
  try
    AItem := TdxAddFieldHistoryItem.Create(PieceTable);
    AItem.CodeStartRunIndex := PieceTable.InsertFieldCodeStartRunCore(ParagraphIndex, LogPosition, ForceVisible);
    AItem.CodeEndRunIndex := PieceTable.InsertFieldCodeEndRunCore(AEndParagraphIndex, AEndLogPosition + 1, ForceVisible);
    AItem.ResultEndRunIndex := PieceTable.InsertFieldResultEndRunCore(AEndParagraphIndex, AEndLogPosition + 2, ForceVisible);
    DocumentModel.History.Add(AItem);
    AItem.Execute;
    FInsertedField := PieceTable.Fields[AItem.InsertedFieldIndex];
  finally
    DocumentModel.History.EndTransaction;
  end;
end;

function TdxPieceTableCreateFieldCommand.GetChangeType: TdxDocumentModelChangeType;
begin
  Result := TdxDocumentModelChangeType.InsertText;
end;

{ TdxPieceTableCreateFieldWithResultCommand }

constructor TdxPieceTableCreateFieldWithResultCommand.Create(APieceTable: TdxSimplePieceTable; AStartCode,
  AEndCode: TdxDocumentLogPosition; AResultLength: Integer; AForceVisible: Boolean);
begin
  inherited Create(APieceTable);
  FStartCode := AStartCode;
  FEndCode := AEndCode;
  FResultLength := AResultLength;
  FForceVisible := AForceVisible;
end;

function TdxPieceTableCreateFieldWithResultCommand.CalculateInsertionParagraphIndex: TdxParagraphIndex;
begin
  Result := PieceTable.FindParagraphIndex(FStartCode);
end;

procedure TdxPieceTableCreateFieldWithResultCommand.ExecuteCore;
var
  AItem: TdxAddFieldHistoryItem;
  AEndCodePosition: TdxDocumentLogPosition;
  AEndResultPosition: TdxDocumentLogPosition;
begin
  DocumentModel.History.BeginTransaction();
  try
    AItem := TdxAddFieldHistoryItem.Create(PieceTable);
    AItem.CodeStartRunIndex := InsertStartCodeRun;
    AEndCodePosition := Min(FEndCode + 1, PieceTable.DocumentEndLogPosition);
    AItem.CodeEndRunIndex := InsertEndCodeRun(AEndCodePosition);
    AEndResultPosition := Min(AEndCodePosition + FResultLength + 1, PieceTable.DocumentEndLogPosition);
    AItem.ResultEndRunIndex := InsertEndResultRun(AEndResultPosition);
    DocumentModel.History.Add(AItem);
    AItem.Execute();
    FInsertedField := PieceTable.Fields[AItem.InsertedFieldIndex];
  finally
    DocumentModel.History.EndTransaction;
  end;
end;

function TdxPieceTableCreateFieldWithResultCommand.GetChangeType: TdxDocumentModelChangeType;
begin
  Result := TdxDocumentModelChangeType.InsertText;
end;

function TdxPieceTableCreateFieldWithResultCommand.InsertEndCodeRun(
  AEndCodePosition: TdxDocumentLogPosition): TdxRunIndex;
var
  AEndCodeParagraphIndex: TdxParagraphIndex;
begin
  AEndCodeParagraphIndex := PieceTable.FindParagraphIndex(AEndCodePosition);
  Result := PieceTable.InsertFieldCodeEndRunCore(AEndCodeParagraphIndex, AEndCodePosition, FForceVisible);
end;

function TdxPieceTableCreateFieldWithResultCommand.InsertEndResultRun(
  AEndResultPosition: TdxDocumentLogPosition): TdxRunIndex;
var
  AEndResultParagraphIndex: TdxParagraphIndex;
begin
  AEndResultParagraphIndex := PieceTable.FindParagraphIndex(AEndResultPosition);
  Result := PieceTable.InsertFieldResultEndRunCore(AEndResultParagraphIndex, AEndResultPosition, FForceVisible);
end;

function TdxPieceTableCreateFieldWithResultCommand.InsertStartCodeRun: TdxRunIndex;
var
  AStartCodeParagraphIndex: TdxParagraphIndex;
begin
  AStartCodeParagraphIndex := ParagraphIndex;
  Result := PieceTable.InsertFieldCodeStartRunCore(AStartCodeParagraphIndex, FStartCode, FForceVisible);
end;

{ TdxPieceTableInsertObjectAtInputPositionCommand }

constructor TdxPieceTableInsertObjectAtInputPositionCommand.Create(APieceTable: TdxSimplePieceTable;
  APosition: TdxInputPosition; AForceVisible: Boolean);
begin
  if APosition.LogPosition < 0 then
    TdxRichEditExceptions.ThrowArgumentException('logPosition', APosition.LogPosition);
  inherited Create(APieceTable);
  FPosition := APosition;
  FForceVisible := AForceVisible;
end;

function TdxPieceTableInsertObjectAtInputPositionCommand.CalculateInsertionParagraphIndex: TdxParagraphIndex;
begin
  Result := Position.ParagraphIndex;
end;

{ TdxPieceTableInsertTextAtInputPositionCommand }

constructor TdxPieceTableInsertTextAtInputPositionCommand.Create(APieceTable: TdxSimplePieceTable;
  AInputPosition: TdxInputPosition; const AText: string; AForceVisible: Boolean);
begin
  if AText = '' then
    TdxRichEditExceptions.ThrowArgumentException('text', text);
  inherited Create(APieceTable, AInputPosition, AForceVisible);
  FText := AText;
end;

procedure TdxPieceTableInsertTextAtInputPositionCommand.ExecuteCore;
begin
  PieceTable.InsertTextCore(Position, FText, ForceVisible);
end;

function TdxPieceTableInsertTextAtInputPositionCommand.GetChangeType: TdxDocumentModelChangeType;
begin
  Result := TdxDocumentModelChangeType.InsertText;
end;

{ TdxPieceTableInsertInlinePictureAtLogPositionCommand }

constructor TdxPieceTableInsertInlinePictureAtLogPositionCommand.Create(APieceTable: TdxSimplePieceTable;
  ALogPosition: TdxDocumentLogPosition; AImage: TdxOfficeImageReference;
  AScaleX, AScaleY: Integer; AForceVisible: Boolean);
begin
  Assert(AImage <> nil);
  Assert(AScaleX > 0);
  Assert(AScaleY > 0);
  inherited Create(APieceTable, ALogPosition, AForceVisible);
  FImage := AImage.Clone(DocumentModel.ImageCache);
  FScaleX := AScaleX;
  FScaleY := AScaleY;
end;

destructor TdxPieceTableInsertInlinePictureAtLogPositionCommand.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TdxPieceTableInsertInlinePictureAtLogPositionCommand.ExecuteCore;
begin
  &Result := PieceTable.InsertInlineImageCore(ParagraphIndex, LogPosition, Image, ScaleX, ScaleY, False, ForceVisible);
end;

function TdxPieceTableInsertInlinePictureAtLogPositionCommand.GetChangeType: TdxDocumentModelChangeType;
begin
  Result := TdxDocumentModelChangeType.InsertInlinePicture;
end;

{ TdxPieceTableInsertInlineCustomObjectAtLogPositionCommand }

constructor TdxPieceTableInsertInlineCustomObjectAtLogPositionCommand.Create(APieceTable: TdxSimplePieceTable;
  ALogPosition: TdxDocumentLogPosition; const ACustomObject: IdxInlineCustomObject; AScaleX, AScaleY: Integer;
  AForceVisible: Boolean);
begin
  Assert(ACustomObject <> nil);
  Assert(AScaleX > 0);
  Assert(AScaleY > 0);
  inherited Create(APieceTable, ALogPosition, AForceVisible);
  FCustomObject := ACustomObject;
  FScaleX := AScaleX;
  FScaleY := AScaleY;
end;

procedure TdxPieceTableInsertInlineCustomObjectAtLogPositionCommand.ExecuteCore;
begin
  PieceTable.InsertInlineCustomObjectCore(ParagraphIndex, LogPosition, CustomObject, ScaleX, ScaleY, ForceVisible);
end;

function TdxPieceTableInsertInlineCustomObjectAtLogPositionCommand.GetChangeType: TdxDocumentModelChangeType;
begin
  Result := TdxDocumentModelChangeType.InsertInlineCustomObject;
end;

{ TdxPieceTableDeleteTextCommand }

procedure TdxPieceTableDeleteTextCommand.ApplyChanges;
var
  ASectionIndex: TdxSectionIndex;
  ARunIndex: TdxRunIndex;
begin
  if FSectionBreakDeleted then
  begin
    ASectionIndex := DocumentModel.FindSectionIndex(PieceTable.Paragraphs[FParagraphIndex].LogPosition);
    FParagraphIndex := DocumentModel.Sections[ASectionIndex].FirstParagraphIndex;
  end;
  FParagraphIndex := Min(FParagraphIndex, PieceTable.Paragraphs.Count - 1);
  ARunIndex := PieceTable.Paragraphs[FParagraphIndex].FirstRunIndex;
  PieceTable.ApplyChanges(TdxDocumentModelChangeType.DeleteContent, ARunIndex, MaxInt);
end;

procedure TdxPieceTableDeleteTextCommand.CalculateApplyChangesParameters;
begin
  FParagraphIndex := PieceTable.FindParagraphIndex(LogPosition);
end;

procedure TdxPieceTableDeleteTextCommand.CalculateExecutionParameters;
begin
end;

constructor TdxPieceTableDeleteTextCommand.Create(APieceTable: TdxSimplePieceTable; ALogPosition: TdxDocumentLogPosition;
  ALength: Integer);
begin
  inherited Create(APieceTable);
  if ALogPosition < 0 then
    TdxRichEditExceptions.ThrowArgumentException('logPosition', ALogPosition);
  Assert(ALength >= 0);
  FLogPosition := ALogPosition;
  FLength := ALength;
end;

function TdxPieceTableDeleteTextCommand.CreateDeleteContentOperation: TdxCustomDeleteContentOperation;
begin
  Result := PieceTable.CreateDeleteContentOperation;
end;

procedure TdxPieceTableDeleteTextCommand.ExecuteCore;
var
  AOperation: TdxCustomDeleteContentOperation;
begin
  AOperation := CreateDeleteContentOperation;
  try
    AOperation.AllowPartiallyDeletingField := AllowPartiallyDeletingField;
    AOperation.LeaveFieldIfResultIsRemoved := LeaveFieldIfResultIsRemoved;
    AOperation.ForceRemoveInnerFields := ForceRemoveInnerFields;
    AOperation.BackspacePressed := BackspacePressed;
    FSectionBreakDeleted := AOperation.Execute(LogPosition, Length, DocumentLastParagraphSelected);
  finally
    AOperation.Free;
  end;
end;

{ TdxPieceTableInsertPlainTextAtLogPositionCommand }

procedure TdxPieceTableInsertPlainTextAtLogPositionCommand.ExecuteCore;
begin
  InsertPlainTextCore(LogPosition, Text);
end;

class function TdxPieceTableInsertPlainTextAtLogPositionCommand.GetNextLine(const ASource: string;
  var AIndex: Integer): string;
var
  AStrings: TStringBuilder;
  ACount: Integer;
begin
  AStrings := TStringBuilder.Create;
  try
    ACount := Length(ASource);
    while (AIndex <= ACount) and (ASource[AIndex] <> #10) and (ASource[AIndex] <> #13) do
    begin
      AStrings.Append(ASource[AIndex]);
      Inc(AIndex);
    end;
    Result := AStrings.ToString;
  finally
    AStrings.Free;
  end;
end;

procedure TdxPieceTableInsertPlainTextAtLogPositionCommand.InsertPlainTextCore(APosition: TdxDocumentLogPosition;
  const AText: string);
var
  ACount, AIndex: Integer;
  ARunFormattingSource: TdxTextRunBase;
  ALastInsertedRun: TdxTextRunBase;
  ALine: string;
begin
  ACount := Length(AText);
  AIndex := 1;
  ARunFormattingSource := nil;
  while AIndex <= ACount do
  begin
    ALine := GetNextLine(AText, AIndex);
    if ALine <> '' then
    begin
      PieceTable.InsertText(APosition, ALine, ForceVisible);
      ALastInsertedRun := PieceTable.LastInsertedRunInfo.Run;
      if ARunFormattingSource <> nil then
        ALastInsertedRun.ApplyFormatting(ARunFormattingSource.CharacterProperties.Info.Info,
          ARunFormattingSource.CharacterProperties.Info.Options, ARunFormattingSource.CharacterStyleIndex,
          ForceVisible)
      else
        ARunFormattingSource := ALastInsertedRun;
      APosition := APosition + Length(ALine);
    end;
    if ShouldAddParagraph(AText, AIndex) then
    begin
      PieceTable.InsertParagraph(APosition, ForceVisible);
      Inc(APosition);
    end;
  end;
end;

class function TdxPieceTableInsertPlainTextAtLogPositionCommand.ShouldAddParagraph(const ASource: string; var AIndex: Integer): Boolean;
var
  ACount: Integer;
begin
  ACount := Length(ASource);
  Result := AIndex <= ACount;
  if Result then
  begin
    if ASource[AIndex] = #13 then
      Inc(AIndex);
    if (AIndex <= ACount) and (ASource[AIndex] = #10) then
      Inc(AIndex);
  end;
end;

{ TdxPieceTableInsertPlainTextAtInputPositionCommand }

constructor TdxPieceTableInsertPlainTextAtInputPositionCommand.Create(APieceTable: TdxSimplePieceTable;
  AInputPosition: TdxInputPosition; const AText: string; AForceVisible: Boolean);
begin
  inherited Create(APieceTable, AInputPosition, AForceVisible);
  if AText = '' then
    TdxRichEditExceptions.ThrowArgumentException('text', text);
  FText := AText;
end;

procedure TdxPieceTableInsertPlainTextAtInputPositionCommand.ExecuteCore;
begin
  InsertPlainTextCore(Position, Text);
end;

function TdxPieceTableInsertPlainTextAtInputPositionCommand.GetChangeType: TdxDocumentModelChangeType;
begin
  Result := TdxDocumentModelChangeType.InsertText;
end;

procedure TdxPieceTableInsertPlainTextAtInputPositionCommand.InsertPlainTextCore(APosition: TdxInputPosition;
  const AText: string);
var
  AIndex: Integer;
  ALine: string;
begin
  AIndex := 1;
  while AIndex <= Length(AText) do
  begin
    ALine := TdxPieceTableInsertPlainTextAtLogPositionCommand.GetNextLine(AText, AIndex);
    if ALine <> '' then
      PieceTable.InsertTextCore(APosition, ALine);
    if TdxPieceTableInsertPlainTextAtLogPositionCommand.ShouldAddParagraph(AText, AIndex) then
      PieceTable.InsertParagraphCore(APosition);
  end;
end;

{ TdxPieceTableInsertParagraphAtLogPositionCommand }

procedure TdxPieceTableInsertParagraphAtLogPositionCommand.ExecuteCore;
begin
  PieceTable.InsertParagraphCore(ParagraphIndex, LogPosition, ForceVisible);
  PieceTable.ApplyNumberingToInsertedParagraph(ParagraphIndex);
end;

function TdxPieceTableInsertParagraphAtLogPositionCommand.GetChangeType: TdxDocumentModelChangeType;
begin
  Result := TdxDocumentModelChangeType.InsertParagraph;
end;

{ TdxPieceTableInsertParagraphAtInputPositionCommand }

constructor TdxPieceTableInsertParagraphAtInputPositionCommand.Create(APieceTable: TdxSimplePieceTable;
  ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean; AInputPosition: TdxInputPosition);
begin
  inherited Create(APieceTable, ALogPosition, AForceVisible);
  FInputPosition := AInputPosition;
end;

function TdxPieceTableInsertParagraphAtInputPositionCommand.GetChangeType: TdxDocumentModelChangeType;
begin
  Result := TdxDocumentModelChangeType.InsertParagraph;
end;

procedure TdxPieceTableInsertParagraphAtInputPositionCommand.ExecuteCore;
var
  ARunIndex: TdxRunIndex;
begin
  ARunIndex := PieceTable.InsertParagraphCore(ParagraphIndex, LogPosition, ForceVisible);
  PieceTable.Runs[ARunIndex].ApplyFormatting(FInputPosition);
  PieceTable.ApplyNumberingToInsertedParagraph(ParagraphIndex);
end;

end.
