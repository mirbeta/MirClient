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

unit dxRichEdit.DocumentModel.PieceTableModifiers.Simple;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils,
  dxCore, dxCoreClasses, dxCoreGraphics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.FastComparer,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.History.Simple,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.InlineObjectFormatting,
  dxRichEdit.DocumentModel.PieceTableModifiers.Core;

type
  { TdxRunPropertyModifier<T> }

  TdxRunPropertyModifier<T> = class abstract (TdxRunPropertyModifierBase)
  strict private
    FComparerType: TdxFastComparerType;
  private
    FNewValue: T;
  protected
    function ValidateNewValue(const ANewValue: T): T; virtual;
  public
    constructor Create(const ANewValue: T);
    procedure CleanupValue(var AValue: T); virtual;
    procedure ModifyInputPosition(APos: TdxInputPosition); virtual;
    function GetRunPropertyValue(ARun: TdxTextRunBase): T; virtual; abstract;
    procedure ModifyInputPositionCore(APos: TdxInputPosition); virtual; abstract;
    function GetInputPositionPropertyValue(APos: TdxInputPosition): T; virtual; abstract;
    function IsValueEquals(const AValue1, AValue2: T): Boolean; virtual;

    function ObtainRunsPropertyValue(APieceTable: TdxSimplePieceTable; const ALogPositionStart: TdxDocumentLogPosition;
      ALength: Integer; out AValue: T): Boolean;

    function ObtainMergedRunsPropertyValue(APieceTable: TdxSimplePieceTable;
      ALogPositionStart: TdxDocumentLogPosition; ALength: Integer): T; virtual;

    property NewValue: T read FNewValue;
  end;

  { TdxMergedRunPropertyModifier<T> }

  TdxMergedRunPropertyModifier<T> = class abstract (TdxRunPropertyModifier<T>)
  public
    function CanModifyRun(ARun: TdxTextRunBase): Boolean; virtual;
    function Merge(const ALeftValue, ARightValue: T): T; virtual; abstract;

    function ObtainMergedRunsPropertyValue(APieceTable: TdxSimplePieceTable;
      ALogPositionStart: TdxDocumentLogPosition; ALength: Integer): T; override;
  end;

  { TdxSimplePieceTableObjectInserter }

  TdxSimplePieceTableObjectInserter = class(TdxObjectInserter)
  strict private
    function GetPieceTable: TdxSimplePieceTable;
  public
    property PieceTable: TdxSimplePieceTable read GetPieceTable;
  end;

  { TdxSimpleTextInserter }

  TdxSimpleTextInserter = class(TdxSimplePieceTableObjectInserter)
  protected
    procedure InsertTextRun(AParagraph: TdxSimpleParagraph; AWhere: TdxRunIndex;
      AStartIndex, ALength: Integer; AForceVisible: Boolean);
    function CreateRunInsertedHistoryItem: TdxTextRunInsertedHistoryItem; virtual;
  public
    function CanMerge(ALogPosition: TdxDocumentLogPosition): Boolean; override;
    procedure Merge(ALogPosition: TdxDocumentLogPosition; AParagraphIndex: TdxParagraphIndex); override;
    procedure PerformInsert(AParagraph: TdxParagraphBase; ARunIndex: TdxRunIndex; ALogPosition: TdxDocumentLogPosition;
      AForceVisible: Boolean); override;
  end;

  { TdxParagraphInserter }

  TdxParagraphInserter = class(TdxSimplePieceTableObjectInserter)
  strict private
    procedure InsertParagraphRun(AParagraph: TdxParagraphBase; AWhere: TdxRunIndex; AStartIndex: Integer; AForceVisible: Boolean);
    procedure InsertParagraphIntoParagraphTable(AParagraph: TdxSimpleParagraph; AParagraphMarkRunIndex: TdxRunIndex; ALogPosition: TdxDocumentLogPosition);
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;

    function CreateInsertParagraphRangeHistoryItem: TdxParagraphRunInsertedHistoryItem; virtual;
    function CanMerge(ALogPosition: TdxDocumentLogPosition): Boolean; override;
    procedure Merge(ALogPosition: TdxDocumentLogPosition; AParagraphIndex: TdxParagraphIndex); override;
    procedure PerformInsert(AParagraph: TdxParagraphBase; ARunIndex: TdxRunIndex; ALogPosition: TdxDocumentLogPosition;
      AForceVisible: Boolean); override;
  end;

  { TdxInlineCustomObjectInserter }

  TdxInlineCustomObjectInserter = class(TdxSimplePieceTableObjectInserter)
  strict private
    FCustomObject: IdxInlineCustomObject;
    FScaleX: Single;
    FScaleY: Single;

    procedure InsertInlineCustomObjectRun(AParagraph: TdxParagraphBase; AWhere: TdxRunIndex; AStartIndex: Integer;
      AForceVisible: Boolean);
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const ACustomObject: IdxInlineCustomObject; const AScaleX, AScaleY: Single); reintroduce;
    function CanMerge(ALogPosition: TdxDocumentLogPosition): Boolean; override;
    procedure Merge(ALogPosition: TdxDocumentLogPosition; AParagraphIndex: TdxParagraphIndex); override;
    procedure PerformInsert(AParagraph: TdxParagraphBase; ARunIndex: TdxRunIndex; ALogPosition: TdxDocumentLogPosition;
      AForceVisible: Boolean); override;
  end;

  { TdxInlinePictureInserter }

  TdxInlinePictureInserter = class(TdxSimplePieceTableObjectInserter)
  strict private
    FFillColor: TdxAlphaColor;
    FImage: TdxOfficeImageReference;
    FScaleX: Single;
    FScaleY: Single;
    FUseScreenDpi: Boolean;
    procedure InsertInlinePictureRun(AParagraph: TdxParagraphBase; AWhere: TdxRunIndex; AStartIndex: Integer;
      AForceVisible: Boolean);
  protected
    function CreateRunInsertedHistoryItem(APieceTable: TdxSimplePieceTable): TdxInlinePictureRunInsertedHistoryItem; virtual;
  public
    constructor Create(APieceTable: TdxSimplePieceTable; AImage: TdxOfficeImageReference; AScaleX, AScaleY: Single;
      AFillColor: TdxAlphaColor; AUseScreenDpi: Boolean); reintroduce; overload;
    constructor Create(APieceTable: TdxSimplePieceTable; AImage: TdxOfficeImageReference; AScaleX, AScaleY: Single;
      AFillColor: TdxAlphaColor); reintroduce; overload;
    constructor Create(APieceTable: TdxSimplePieceTable; AImage: TdxOfficeImageReference; AScaleX, AScaleY: Single); reintroduce; overload;
    destructor Destroy; override;
    function CanMerge(ALogPosition: TdxDocumentLogPosition): Boolean; override;
    procedure Merge(ALogPosition: TdxDocumentLogPosition; AParagraphIndex: TdxParagraphIndex); override;
    procedure PerformInsert(AParagraph: TdxParagraphBase; ARunIndex: TdxRunIndex; ALogPosition: TdxDocumentLogPosition;
      AForceVisible: Boolean); override;
  end;

  { TdxFieldRunInserterBase }

  TdxFieldRunInserterBase = class abstract(TdxSimplePieceTableObjectInserter)
  protected
    function CreateHistoryItem: TdxMarkRunInsertedHistoryItemBase; virtual; abstract;
  public
    function CanMerge(ALogPosition: TdxDocumentLogPosition): Boolean; override;
    procedure Merge(ALogPosition: TdxDocumentLogPosition; AParagraphIndex: TdxParagraphIndex); override;
    procedure PerformInsert(AParagraph: TdxParagraphBase; ARunIndex: TdxRunIndex; ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean); override;
  end;

  { TdxFieldResultEndRunInserter }

  TdxFieldResultEndRunInserter = class(TdxFieldRunInserterBase)
  protected
    function CreateHistoryItem: TdxMarkRunInsertedHistoryItemBase; override;
  end;

  { TdxFieldCodeStartRunInserter }

  TdxFieldCodeStartRunInserter = class(TdxFieldRunInserterBase)
  protected
    function CreateHistoryItem: TdxMarkRunInsertedHistoryItemBase; override;
  end;

  { TdxFieldCodeEndRunInserter }

  TdxFieldCodeEndRunInserter = class(TdxFieldRunInserterBase)
  protected
    function CreateHistoryItem: TdxMarkRunInsertedHistoryItemBase; override;
  end;

  { TdxRunCharacterStyleModifier }

  TdxRunCharacterStyleModifier = class(TdxRunPropertyModifier<Integer>)
  private
    FResetProperties: Boolean;
  public
    constructor Create(AStyleIndex: Integer; AResetProperties: Boolean = True);
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
    function GetRunPropertyValue(ARun: TdxTextRunBase): Integer; override;
    procedure ModifyInputPositionCore(APos: TdxInputPosition); override;
    function GetInputPositionPropertyValue(APos: TdxInputPosition): Integer; override;
  end;

  { TdxReplaceRunCharacterStylePropertiesModifier }

  TdxReplaceRunCharacterStylePropertiesModifier = class(TdxRunCharacterStyleModifier)
  private
    FProperties: TdxMergedCharacterProperties;
    function LeaveUseInStyleProperties(AStyle: TdxCharacterStyle;
      AProperties: TdxMergedCharacterProperties): TdxMergedCharacterProperties;
  public
    constructor Create(AStyleIndex: Integer; ASourceProperties: TdxMergedCharacterProperties);
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
  end;

  { TdxRunCharacterStyleKeepOldStylePropertiesModifier }

  TdxRunCharacterStyleKeepOldStylePropertiesModifier = class(TdxRunCharacterStyleModifier)
  private
    FApplyDefaultHyperlinkStyle: Boolean;
    FIgnoredOptions: TdxUsedCharacterFormattingOptions;
  public
    constructor Create(AStyleIndex: Integer; AApplyDefaultHyperlinkStyle: Boolean; const AIgnoredOptions: TdxUsedCharacterFormattingOptions); overload;
    constructor Create(AStyleIndex: Integer; AApplyDefaultHyperlinkStyle: Boolean); overload;
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
    procedure ModifyTextRunCore(ARun: TdxTextRunBase);
  end;

  { TdxSeparatorTextRunInserter }

  TdxSeparatorTextRunInserter = class sealed(TdxSimplePieceTableObjectInserter)
  protected
    function CreateHistoryItem: TdxSeparatorRunInsertedHistoryItem;
  public
    function CanMerge(ALogPosition: TdxDocumentLogPosition): Boolean; override;
    procedure Merge(ALogPosition: TdxDocumentLogPosition; AParagraphIndex: TdxParagraphIndex); override;
    procedure PerformInsert(AParagraph: TdxParagraphBase; ARunIndex: TdxRunIndex; ALogPosition: TdxDocumentLogPosition;
      AForceVisible: Boolean); override;
  end;

implementation

uses
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.DocumentModel.InlineObjectRange;

{ TdxRunPropertyModifier<T> }

constructor TdxRunPropertyModifier<T>.Create(const ANewValue: T);
begin
  FNewValue := ValidateNewValue(ANewValue);
end;

procedure TdxRunPropertyModifier<T>.CleanupValue(var AValue: T);
begin
end;

function TdxRunPropertyModifier<T>.IsValueEquals(const AValue1, AValue2: T): Boolean;
begin
  Result := TdxFastComparer<T>.IsValueEquals(FComparerType, AValue1, AValue2);
end;

function TdxRunPropertyModifier<T>.ObtainRunsPropertyValue(APieceTable: TdxSimplePieceTable; const ALogPositionStart: TdxDocumentLogPosition;
  ALength: Integer; out AValue: T): Boolean;
var
  AInfo: TdxRunInfo;
  I: TdxRunIndex;
  ARunValue: T;
begin
  AInfo := APieceTable.FindRunInfo(ALogPositionStart, ALength);
  try
    AValue := GetRunPropertyValue(APieceTable.Runs[AInfo.Start.RunIndex]);
    for I := AInfo.Start.RunIndex + 1 to AInfo.&End.RunIndex do
    begin
      ARunValue := GetRunPropertyValue(APieceTable.Runs[I]);
      if not IsValueEquals(AValue, ARunValue) then
        Exit(False);
    end;
    Result := True;
  finally
    AInfo.Free;
  end;
end;

function TdxRunPropertyModifier<T>.ObtainMergedRunsPropertyValue(APieceTable: TdxSimplePieceTable;
  ALogPositionStart: TdxDocumentLogPosition; ALength: Integer): T;
begin
  Result := Default(T);
  dxAbstractError;
end;

procedure TdxRunPropertyModifier<T>.ModifyInputPosition(APos: TdxInputPosition);
begin
  ModifyInputPositionCore(APos);
end;

function TdxRunPropertyModifier<T>.ValidateNewValue(const ANewValue: T): T;
begin
  Result := ANewValue;
end;

{ TdxMergedRunPropertyModifier<T> }

function TdxMergedRunPropertyModifier<T>.CanModifyRun(ARun: TdxTextRunBase): Boolean;
begin
  Result := True;
end;

function TdxMergedRunPropertyModifier<T>.ObtainMergedRunsPropertyValue(APieceTable: TdxSimplePieceTable;
  ALogPositionStart: TdxDocumentLogPosition; ALength: Integer): T;
var
  ARunInfo: TdxRunInfo;
  I, AIndex: TdxRunIndex;
  AValue, ARunValue: T;
begin
  ARunInfo := APieceTable.FindRunInfo(ALogPositionStart, ALength);
  try
    AIndex := ARunInfo.Start.RunIndex;
    for I := AIndex to ARunInfo.&End.RunIndex do
      if CanModifyRun(APieceTable.Runs[i]) then
      begin
        AIndex := I;
        Break;
      end;
    Result := GetRunPropertyValue(APieceTable.Runs[AIndex]);
    for I := AIndex + 1 to ARunInfo.&End.RunIndex do
    begin
      if CanModifyRun(APieceTable.Runs[I]) then
      begin
        ARunValue := GetRunPropertyValue(APieceTable.Runs[I]);
        try
          AValue := Result;
          try
            Result := Merge(AValue, ARunValue);
          finally
            CleanupValue(AValue);
          end;
        finally
          CleanupValue(ARunValue);
        end;
      end;
    end;
  finally
    ARunInfo.Free;
  end;
end;

{ TdxSimpleTextInserter }

function TdxSimpleTextInserter.CanMerge(
  ALogPosition: TdxDocumentLogPosition): Boolean;
var
  AInfo: TdxLastInsertedRunInfo;
begin
  AInfo := PieceTable.LastInsertedRunInfo;
  Result := ALogPosition = AInfo.LogPosition;
end;

function TdxSimpleTextInserter.CreateRunInsertedHistoryItem: TdxTextRunInsertedHistoryItem;
begin
  Result := TdxTextRunInsertedHistoryItem.Create(PieceTable);
end;

procedure TdxSimpleTextInserter.InsertTextRun(AParagraph: TdxSimpleParagraph; AWhere: TdxRunIndex;
  AStartIndex, ALength: Integer; AForceVisible: Boolean);
var
  AItem: TdxTextRunInsertedHistoryItem;
begin
  AItem := CreateRunInsertedHistoryItem;
  AItem.ForceVisible := AForceVisible;
  AItem.RunIndex := AWhere;
  AItem.StartIndex := AStartIndex;
  AItem.NewLength := ALength;
  AItem.ParagraphIndex := AParagraph.Index;
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

procedure TdxSimpleTextInserter.Merge(ALogPosition: TdxDocumentLogPosition;
  AParagraphIndex: TdxParagraphIndex);
var
  AItem: TdxTextRunAppendTextHistoryItem;
begin
  AItem := TdxTextRunAppendTextHistoryItem.Create(PieceTable);
  AItem.ParagraphIndex := AParagraphIndex;
  AItem.RunIndex := PieceTable.LastInsertedRunInfo.RunIndex;
  AItem.LogPosition := ALogPosition;
  AItem.TextLength := TextLength;
  if DocumentModel.History is TdxRichEditDocumentHistory then
    TdxRichEditDocumentHistory(DocumentModel.History).AddRangeTextAppendedHistoryItem(AItem)
  else
    DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

procedure TdxSimpleTextInserter.PerformInsert(AParagraph: TdxParagraphBase;
  ARunIndex: TdxRunIndex; ALogPosition: TdxDocumentLogPosition;
  AForceVisible: Boolean);
var
  AOldGrowBufferLength: Integer;
begin
  AOldGrowBufferLength := PieceTable.TextBuffer.Length - TextLength;
  InsertTextRun(TdxSimpleParagraph(AParagraph), ARunIndex, AOldGrowBufferLength, TextLength, AForceVisible);
  PieceTable.LastInsertedRunInfo.LogPosition := ALogPosition + TextLength;
end;

{ TdxParagraphInserter }

constructor TdxParagraphInserter.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  TextLength := 1;
end;

function TdxParagraphInserter.CreateInsertParagraphRangeHistoryItem: TdxParagraphRunInsertedHistoryItem;
begin
  Result := TdxParagraphRunInsertedHistoryItem.Create(PieceTable);
end;

function TdxParagraphInserter.CanMerge(ALogPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := False;
end;

procedure TdxParagraphInserter.Merge(ALogPosition: TdxDocumentLogPosition; AParagraphIndex: TdxParagraphIndex);
begin
  TdxRichEditExceptions.ThrowInternalException;
end;

procedure TdxParagraphInserter.PerformInsert(AParagraph: TdxParagraphBase;
  ARunIndex: TdxRunIndex; ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean);
var
  AOldGrowBufferLength: Integer;
begin
  AOldGrowBufferLength := PieceTable.TextBuffer.Length - 1;
  InsertParagraphRun(AParagraph, ARunIndex, AOldGrowBufferLength, AForceVisible);
  InsertParagraphIntoParagraphTable(TdxSimpleParagraph(AParagraph), ARunIndex, ALogPosition);
end;

procedure TdxParagraphInserter.InsertParagraphRun(AParagraph: TdxParagraphBase; AWhere: TdxRunIndex; AStartIndex: Integer; AForceVisible: Boolean);
var
  AItem: TdxParagraphRunInsertedHistoryItem;
begin
  AItem := CreateInsertParagraphRangeHistoryItem;
  AItem.ForceVisible := AForceVisible;
  AItem.RunIndex := AWhere;
  AItem.StartIndex := AStartIndex;
  AItem.ParagraphIndex := AParagraph.Index;
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

procedure TdxParagraphInserter.InsertParagraphIntoParagraphTable(AParagraph: TdxSimpleParagraph;
  AParagraphMarkRunIndex: TdxRunIndex; ALogPosition: TdxDocumentLogPosition);
var
  AItem: TdxParagraphInsertedBaseHistoryItem;
begin
  AItem := TdxParagraphInsertedBaseHistoryItem.Create(PieceTable);
  AItem.ParagraphIndex := AParagraph.Index;
  AItem.LogPosition := ALogPosition;
  AItem.ParagraphMarkRunIndex := AParagraphMarkRunIndex;
  AItem.SetTableCell(AParagraph.GetCellCore);
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

{ TdxInlineCustomObjectInserter }

constructor TdxInlineCustomObjectInserter.Create(APieceTable: TdxCustomPieceTable; const ACustomObject: IdxInlineCustomObject;
  const AScaleX, AScaleY: Single);
begin
  inherited Create(APieceTable);
  TextLength := 1;
  FCustomObject := ACustomObject;
  FScaleX := AScaleX;
  FScaleY := AScaleY;
end;

function TdxInlineCustomObjectInserter.CanMerge(ALogPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := False;
end;

procedure TdxInlineCustomObjectInserter.Merge(ALogPosition: TdxDocumentLogPosition; AParagraphIndex: TdxParagraphIndex);
begin
  TdxRichEditExceptions.ThrowInternalException;
end;

procedure TdxInlineCustomObjectInserter.PerformInsert(AParagraph: TdxParagraphBase; ARunIndex: TdxRunIndex;
  ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean);
var
  AOldGrowBufferLength: Integer;
begin
  AOldGrowBufferLength := TdxSimplePieceTable(PieceTable).TextBuffer.Length - 1;
  InsertInlineCustomObjectRun(AParagraph, ARunIndex, AOldGrowBufferLength, AForceVisible);
end;

procedure TdxInlineCustomObjectInserter.InsertInlineCustomObjectRun(AParagraph: TdxParagraphBase; AWhere: TdxRunIndex;
  AStartIndex: Integer; AForceVisible: Boolean);
var
  AItem: TdxInlineCustomObjectRunInsertedHistoryItem;
begin
  AItem := TdxInlineCustomObjectRunInsertedHistoryItem.Create(PieceTable);
  AItem.ForceVisible := AForceVisible;
  AItem.RunIndex := AWhere;
  AItem.StartIndex := AStartIndex;
  AItem.ParagraphIndex := AParagraph.Index;
  AItem.CustomObject := FCustomObject;
  AItem.ScaleX := FScaleX;
  AItem.ScaleY := FScaleY;
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

{ TdxSimplePieceTableObjectInserter }

function TdxSimplePieceTableObjectInserter.GetPieceTable: TdxSimplePieceTable;
begin
  Result := TdxSimplePieceTable(inherited PieceTable);
end;

{ TdxInlinePictureInserter }

constructor TdxInlinePictureInserter.Create(APieceTable: TdxSimplePieceTable; AImage: TdxOfficeImageReference; AScaleX,
  AScaleY: Single; AFillColor: TdxAlphaColor; AUseScreenDpi: Boolean);
begin
  inherited Create(APieceTable);
  TextLength := 1;
  FImage := AImage.Clone(TdxSimpleDocumentModel(DocumentModel).ImageCache);
  FScaleX := AScaleX;
  FScaleY := AScaleY;
  FFillColor := AFillColor;
  FUseScreenDpi := AUseScreenDpi;
end;

constructor TdxInlinePictureInserter.Create(APieceTable: TdxSimplePieceTable; AImage: TdxOfficeImageReference; AScaleX,
  AScaleY: Single; AFillColor: TdxAlphaColor);
begin
  Create(APieceTable, AImage, AScaleX, AScaleY, AFillColor, False);
end;

constructor TdxInlinePictureInserter.Create(APieceTable: TdxSimplePieceTable; AImage: TdxOfficeImageReference; AScaleX,
  AScaleY: Single);
begin
  Create(APieceTable, AImage, AScaleX, AScaleY, TdxAlphaColors.Empty);
end;

destructor TdxInlinePictureInserter.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

function TdxInlinePictureInserter.CanMerge(ALogPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := False;
end;

function TdxInlinePictureInserter.CreateRunInsertedHistoryItem(
  APieceTable: TdxSimplePieceTable): TdxInlinePictureRunInsertedHistoryItem;
begin
  Result := TdxInlinePictureRunInsertedHistoryItem.Create(APieceTable);
end;

procedure TdxInlinePictureInserter.InsertInlinePictureRun(AParagraph: TdxParagraphBase; AWhere: TdxRunIndex;
  AStartIndex: Integer; AForceVisible: Boolean);
var
  AItem: TdxInlinePictureRunInsertedHistoryItem;
  ARun: TdxInlinePictureRun;
  AProperties: TdxInlinePictureProperties;
begin
  AItem := CreateRunInsertedHistoryItem(PieceTable);
  AItem.ForceVisible := AForceVisible;
  AItem.RunIndex := AWhere;
  AItem.StartIndex := AStartIndex;
  AItem.ParagraphIndex := AParagraph.Index;
  AItem.Image := FImage;
  DocumentModel.History.Add(AItem);
  AItem.Execute;
  ARun := PieceTable.Runs[AWhere] as TdxInlinePictureRun;
  AProperties := ARun.Properties;
  AProperties.BeginUpdate;
  try
    AProperties.ScaleX := FScaleX;
    AProperties.ScaleY := FScaleY;
    AProperties.FillColor := FFillColor;
  finally
    AProperties.EndUpdate;
  end;
  ARun.GetPictureContent.EnsureActualSize(FUseScreenDpi);
end;

procedure TdxInlinePictureInserter.Merge(ALogPosition: TdxDocumentLogPosition; AParagraphIndex: TdxParagraphIndex);
begin
  TdxRichEditExceptions.ThrowInternalException;
end;

procedure TdxInlinePictureInserter.PerformInsert(AParagraph: TdxParagraphBase; ARunIndex: TdxRunIndex;
  ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean);
var
  AOldGrowBufferLength: Integer;
begin
  AOldGrowBufferLength := PieceTable.TextBuffer.Length - 1;
  InsertInlinePictureRun(AParagraph, ARunIndex, AOldGrowBufferLength, AForceVisible);
end;

{ TdxFieldRunInserterBase }

function TdxFieldRunInserterBase.CanMerge(ALogPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := False;
end;

procedure TdxFieldRunInserterBase.Merge(ALogPosition: TdxDocumentLogPosition; AParagraphIndex: TdxParagraphIndex);
begin
  TdxRichEditExceptions.ThrowInternalException;
end;

procedure TdxFieldRunInserterBase.PerformInsert(AParagraph: TdxParagraphBase; ARunIndex: TdxRunIndex;
  ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean);
var
  AItem: TdxMarkRunInsertedHistoryItemBase;
begin
  AItem := CreateHistoryItem;
  AItem.ForceVisible := AForceVisible;
  AItem.RunIndex := ARunIndex;
  AItem.StartIndex := PieceTable.TextBuffer.Length - 1;
  AItem.ParagraphIndex := AParagraph.Index;
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

{ TdxFieldResultEndRunInserter }

function TdxFieldResultEndRunInserter.CreateHistoryItem: TdxMarkRunInsertedHistoryItemBase;
begin
  Result := TdxFieldResultEndRunInsertedHistoryItem.Create(PieceTable);
end;

{ TdxFieldCodeStartRunInserter }

function TdxFieldCodeStartRunInserter.CreateHistoryItem: TdxMarkRunInsertedHistoryItemBase;
begin
  Result := TdxFieldCodeStartRunInsertedHistoryItem.Create(PieceTable);
end;

{ TdxFieldCodeEndRunInserter }

function TdxFieldCodeEndRunInserter.CreateHistoryItem: TdxMarkRunInsertedHistoryItemBase;
begin
  Result := TdxFieldCodeEndRunInsertedHistoryItem.Create(PieceTable);
end;

{ TdxRunCharacterStyleModifier }

constructor TdxRunCharacterStyleModifier.Create(AStyleIndex: Integer; AResetProperties: Boolean = True);
begin
  inherited Create(AStyleIndex);
  FResetProperties := AResetProperties;
end;

function TdxRunCharacterStyleModifier.GetInputPositionPropertyValue(APos: TdxInputPosition): Integer;
begin
  Result := APos.CharacterStyleIndex;
end;

function TdxRunCharacterStyleModifier.GetRunPropertyValue(ARun: TdxTextRunBase): Integer;
begin
  Result := ARun.CharacterStyleIndex;
end;

procedure TdxRunCharacterStyleModifier.ModifyInputPositionCore(APos: TdxInputPosition);
begin
end;

procedure TdxRunCharacterStyleModifier.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
var
  ADocumentModel: TdxCustomDocumentModel;
begin
  ADocumentModel := ARun.Paragraph.DocumentModel;
  ADocumentModel.History.BeginTransaction;
  try
    if FResetProperties then
      ARun.ResetCharacterProperties;
    ARun.CharacterStyleIndex := NewValue;
  finally
    ADocumentModel.History.EndTransaction;
  end;
end;

{ TdxReplaceRunCharacterStylePropertiesModifier }

constructor TdxReplaceRunCharacterStylePropertiesModifier.Create(AStyleIndex: Integer;
  ASourceProperties: TdxMergedCharacterProperties);
begin
  inherited Create(AStyleIndex, False);
  Assert(ASourceProperties <> nil, 'ASourceProperties = nil');
  FProperties := ASourceProperties;
end;

function TdxReplaceRunCharacterStylePropertiesModifier.LeaveUseInStyleProperties(AStyle: TdxCharacterStyle;
  AProperties: TdxMergedCharacterProperties): TdxMergedCharacterProperties;
var
  AStyleProperties: TdxMergedCharacterProperties;
begin
  Result := TdxMergedCharacterProperties.Create(AProperties.Info, AProperties.Options);
  AStyleProperties := AStyle.GetMergedCharacterProperties;
  try
    Result.Options.Value := AStyleProperties.Options.Value;
  finally
    AStyleProperties.Free;
  end;
end;

procedure TdxReplaceRunCharacterStylePropertiesModifier.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
var
  ADocumentModel: TdxSimpleDocumentModel;
  AStyle: TdxCharacterStyle;
  ANewProperties: TdxMergedCharacterProperties;
  AMergedProperties: TdxMergedCharacterProperties;
begin
  ADocumentModel := ARun.Paragraph.DocumentModel;
  ADocumentModel.History.BeginTransaction;
  try
    if ARun.CharacterStyleIndex <> NewValue then
      Exit;
    AStyle := ADocumentModel.CharacterStyles[NewValue];
    ANewProperties := LeaveUseInStyleProperties(AStyle, FProperties);
    try
      AMergedProperties := TdxMergedCharacterProperties.Create(ARun.CharacterProperties);
      try
        AMergedProperties.Merge(ANewProperties);
        ARun.CharacterProperties.CopyFrom(AMergedProperties);
      finally
        FreeAndNil(AMergedProperties);
      end;
    finally
      ANewProperties.Free;
    end;
    ARun.CharacterStyleIndex := TdxCharacterStyleCollection.EmptyCharacterStyleIndex;
  finally
    ADocumentModel.History.EndTransaction;
  end;
end;

{ TdxRunCharacterStyleKeepOldStylePropertiesModifier }

constructor TdxRunCharacterStyleKeepOldStylePropertiesModifier.Create(AStyleIndex: Integer;
  AApplyDefaultHyperlinkStyle: Boolean; const AIgnoredOptions: TdxUsedCharacterFormattingOptions);
begin
  inherited Create(AStyleIndex, False);
  FApplyDefaultHyperlinkStyle := AApplyDefaultHyperlinkStyle;
  FIgnoredOptions := AIgnoredOptions;
end;

constructor TdxRunCharacterStyleKeepOldStylePropertiesModifier.Create(AStyleIndex: Integer; AApplyDefaultHyperlinkStyle: Boolean);
begin
  Create(AStyleIndex, AApplyDefaultHyperlinkStyle, []);
end;

procedure TdxRunCharacterStyleKeepOldStylePropertiesModifier.ModifyTextRun(ARun: TdxTextRunBase;
  ARunIndex: TdxRunIndex);
begin
  if ARun.CharacterStyleIndex <> NewValue then
    ModifyTextRunCore(ARun);
end;

procedure TdxRunCharacterStyleKeepOldStylePropertiesModifier.ModifyTextRunCore(ARun: TdxTextRunBase);
var
  ADocumentModel: TdxSimpleDocumentModel;
  ACharacterProperties: TdxMergedCharacterProperties;
  AStypeProperties: TdxMergedCharacterProperties;
  AStyleOptions: TdxUsedCharacterFormattingOptions;
  AMergedProperties: TdxMergedCharacterProperties;
begin
  ADocumentModel := ARun.Paragraph.DocumentModel;
  ADocumentModel.History.BeginTransaction;
  try
    ACharacterProperties := ARun.CharacterStyle.GetMergedCharacterProperties;
    try
      if FApplyDefaultHyperlinkStyle then
      begin
        AStypeProperties := ADocumentModel.CharacterStyles[NewValue].GetMergedCharacterProperties;
        try
          AStyleOptions := AStypeProperties.Options.Value - FIgnoredOptions;
        finally
          AStypeProperties.Free;
        end;
        ACharacterProperties.Options.Value := ACharacterProperties.Options.Value - AStyleOptions;

        ARun.CharacterProperties.ResetUse(AStyleOptions);
      end;
      AMergedProperties := TdxMergedCharacterProperties.Create(ARun.CharacterProperties);
      try
        AMergedProperties.Merge(ACharacterProperties);
        ARun.CharacterProperties.CopyFrom(AMergedProperties);
        ARun.CharacterStyleIndex := NewValue;
      finally
        AMergedProperties.Free;
      end;
    finally
      ACharacterProperties.Free;
    end;
  finally
    ADocumentModel.History.EndTransaction;
  end;
end;

{ TdxSeparatorTextRunInserter }

function TdxSeparatorTextRunInserter.CanMerge(ALogPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := False;
end;

function TdxSeparatorTextRunInserter.CreateHistoryItem: TdxSeparatorRunInsertedHistoryItem;
begin
  Result := TdxSeparatorRunInsertedHistoryItem.Create(PieceTable);
end;

procedure TdxSeparatorTextRunInserter.Merge(ALogPosition: TdxDocumentLogPosition; AParagraphIndex: TdxParagraphIndex);
begin
  TdxRichEditExceptions.ThrowInternalException;
end;

procedure TdxSeparatorTextRunInserter.PerformInsert(AParagraph: TdxParagraphBase; ARunIndex: TdxRunIndex;
  ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean);
var
  AItem: TdxSeparatorRunInsertedHistoryItem;
begin
  AItem := CreateHistoryItem;
  AItem.ForceVisible := AForceVisible;
  AItem.RunIndex := ARunIndex;
  AItem.StartIndex := PieceTable.TextBuffer.Length - 1;
  AItem.ParagraphIndex := AParagraph.Index;
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

end.
