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

unit dxRichEdit.DocumentModel.FloatingObjectRange;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics, cxGeometry,
  dxRichEdit.NativeApi,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.CopyManager.Core,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.ShapeFormatting,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem;

type
  TdxFloatingObjectAnchorRun = class;

  { TdxTextBoxContentType }

  TdxTextBoxContentType = class(TdxContentTypeBase)
  strict private
    FAnchorRun: TdxFloatingObjectAnchorRun;
    function GetPieceTable: TdxSimplePieceTable;
  protected
    function GetIsMain: Boolean; override; final;
    function GetIsTextBox: Boolean; override; final;
    function GetIsReferenced: Boolean; override; final;
    function CalculateAnchorPieceTableStartRunIndex(ARunIndex: TdxRunIndex): TdxRunIndex;
    function CalculateAnchorPieceTableEndRunIndex(ARunIndex: TdxRunIndex): TdxRunIndex;
  public
    constructor Create(ADocumentModel: TdxSimpleDocumentModel); overload;
    constructor Create(APieceTable: TdxSimplePieceTable); overload;
    procedure ApplyChanges(AChangeType: TdxDocumentModelChangeType; AStartRunIndex: TdxRunIndex; AEndRunIndex: TdxRunIndex); override; final;
    procedure ApplyChangesCore(AActions: TdxDocumentModelChangeActions; AStartRunIndex: TdxRunIndex; AEndRunIndex: TdxRunIndex); override; final;
    procedure FixLastParagraphOfLastSection(AOriginalParagraphCount: Integer); override; final;
    function LookupSectionIndexByParagraphIndex(AParagraphIndex: TdxParagraphIndex): TdxSectionIndex; override; final;

    property AnchorRun: TdxFloatingObjectAnchorRun read FAnchorRun write FAnchorRun;
    property PieceTable: TdxSimplePieceTable read GetPieceTable;
  end;

  TdxTextBoxContentTypeList = class(TdxList<TdxTextBoxContentType>);

  { TdxFloatingObjectContent }

  TdxFloatingObjectContent = class abstract(TdxCustomObjectContent)
  strict private
    class var FEmpty: TdxFloatingObjectContent;

    class constructor Initialize;
    class destructor Finalize;
  public
    procedure Dispose; override;

    class property Empty: TdxFloatingObjectContent read FEmpty;
  end;

  { TdxEmptyFloatingObjectContent }

  TdxEmptyFloatingObjectContent = class(TdxFloatingObjectContent)
  protected
    function GetOriginalSize: TSize; override;
    procedure SetOriginalSize(const AValue: TSize); override;
  public
    constructor Create;

    function Clone(const ARun: TdxTextRunBase; ACopyManager: TdxCustomDocumentModelCopyManager): TdxCustomObjectContent; override;
  end;

  { TdxTextBoxFloatingObjectContent }

  TdxTextBoxFloatingObjectContent = class(TdxFloatingObjectContent, IdxTextBoxPropertiesContainer)
  strict private
    FTextBox: TdxTextBoxContentType;
    FTextBoxProperties: TdxTextBoxProperties;
    function GetPieceTable: TdxSimplePieceTable;
  protected
    function GetOriginalSize: TSize; override;
    procedure SetOriginalSize(const AValue: TSize); override;
    procedure SetAnchorRun(AAnchor: TdxFloatingObjectAnchorRun); virtual;
    function IdxTextBoxPropertiesContainer.GetPieceTable = GetTextBoxPropertiesContainerPieceTable;
    function GetTextBoxPropertiesContainerPieceTable: TdxCustomPieceTable;
  public
    constructor Create(AAnchor: TdxFloatingObjectAnchorRun; AContent: TdxTextBoxContentType);
    destructor Destroy; override;

    function Clone(const ARun: TdxTextRunBase; ACopyManager: TdxCustomDocumentModelCopyManager): TdxCustomObjectContent; override;
    function GetInsertOptions(AFormattingCopyOptions: TdxFormattingCopyOptions): TdxRichEditInsertOptions;
    function CreateTextBoxChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnTextBoxChanged;

    property TextBox: TdxTextBoxContentType read FTextBox;
    property TextBoxProperties: TdxTextBoxProperties read FTextBoxProperties;
    property PieceTable: TdxSimplePieceTable read GetPieceTable;
  end;

  { TdxExplicitFloatingObjectLocation }

  TdxExplicitFloatingObjectLocation = class(TInterfacedObject, IdxFloatingObjectLocation)
  strict private
    FActualHeight: Integer;
    FActualWidth: Integer;
    FHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
    FHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
    FOffsetX: Integer;
    FOffsetY: Integer;
    FPercentOffsetX: Integer;
    FPercentOffsetY: Integer;
    FRelativeHeight: TdxFloatingObjectRelativeHeight;
    FRelativeWidth: TdxFloatingObjectRelativeWidth;
    FTextWrapType: TdxFloatingObjectTextWrapType;
    FUseRelativeHeight: Boolean;
    FUseRelativeWidth: Boolean;
    FLayoutInTableCell: Boolean;
    FVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
    FVerticalPositionType: TdxFloatingObjectVerticalPositionType;
    function GetActualHeight: Integer;
    function GetActualWidth: Integer;
    function GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
    function GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
    function GetOffsetX: Integer;
    function GetOffsetY: Integer;
    function GetRelativeHeight: TdxFloatingObjectRelativeHeight;
    function GetRelativeWidth: TdxFloatingObjectRelativeWidth;
    function GetTextWrapType: TdxFloatingObjectTextWrapType;
    function GetUseRelativeHeight: Boolean;
    function GetUseRelativeWidth: Boolean;
    function GetLayoutInTableCell: Boolean;
    function GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
    function GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
    function GetPercentOffsetX: Integer;
    function GetPercentOffsetY: Integer;
  public
    property OffsetX: Integer read FOffsetX write FOffsetX;
    property OffsetY: Integer read FOffsetY write FOffsetY;
    property HorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment read FHorizontalPositionAlignment write FHorizontalPositionAlignment;
    property VerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment read FVerticalPositionAlignment write FVerticalPositionAlignment;
    property HorizontalPositionType: TdxFloatingObjectHorizontalPositionType read FHorizontalPositionType write FHorizontalPositionType;
    property VerticalPositionType: TdxFloatingObjectVerticalPositionType read FVerticalPositionType write FVerticalPositionType;
    property TextWrapType: TdxFloatingObjectTextWrapType read FTextWrapType write FTextWrapType;
    property ActualWidth: Integer read FActualWidth write FActualWidth;
    property ActualHeight: Integer read FActualHeight write FActualHeight;
    property UseRelativeWidth: Boolean read FUseRelativeWidth write FUseRelativeWidth;
    property UseRelativeHeight: Boolean read FUseRelativeHeight write FUseRelativeHeight;
    property LayoutInTableCell: Boolean read FLayoutInTableCell write FLayoutInTableCell;
    property RelativeWidth: TdxFloatingObjectRelativeWidth read FRelativeWidth write FRelativeWidth;
    property RelativeHeight: TdxFloatingObjectRelativeHeight read FRelativeHeight write FRelativeHeight;
    property PercentOffsetX: Integer read FPercentOffsetX write FPercentOffsetX;
    property PercentOffsetY: Integer read FPercentOffsetY write FPercentOffsetY;
  end;

  { TdxFloatingObject }

  TdxFloatingObject = class(TcxIUnknownObject, IdxFloatingObjectPropertiesContainer, IdxShapeContainer)
  strict private
    FAnchorRun: TdxFloatingObjectAnchorRun;
    FProperties: TdxFloatingObjectProperties;
    FShape: TdxShape;
    FContent: TdxCustomObjectContent;
    function GetPieceTable: TdxCustomPieceTable;
    function GetPieceTableProperty: TdxSimplePieceTable;
    function GetDocumentModel: TdxSimpleDocumentModel;

    function CreateFloatingObjectPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnFloatingObjectChanged;
    function CreateShapeChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnShapeChanged;
    procedure OnObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs);
    procedure AssignTextBoxAnchorRun(ARun: TdxFloatingObjectAnchorRun);
    procedure RegisterTextBoxPieceTable;
    function IsTextBoxActive(ATextBox: TdxTextBoxContentType): Boolean;
    procedure DeactivateTextBox;
    procedure SetSelectionByAnchorPosition;
  protected
    procedure BeforeRunRemoved; virtual;
    procedure AfterRunInserted; virtual;
    procedure SetContent(AContent: TdxCustomObjectContent);
  public
    constructor Create(AAnchorRun: TdxFloatingObjectAnchorRun);
    destructor Destroy; override;

    procedure CopyFrom(AFloatingObject: TdxFloatingObject; ACopyManager: TdxCustomDocumentModelCopyManager); virtual;
    property Properties: TdxFloatingObjectProperties read FProperties;
    property Shape: TdxShape read FShape;
    property Content: TdxCustomObjectContent read FContent;
    property PieceTable: TdxSimplePieceTable read GetPieceTableProperty;

    property AnchorRun: TdxFloatingObjectAnchorRun read FAnchorRun;
    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
  end;

  { TdxFloatingObjectAnchorRun }

  TdxFloatingObjectAnchorRun = class(TdxTextRunBase,
    IdxPictureContainerRun,
    IdxRectangularObject,
    IdxRectangularScalableObject)
  strict private
		FFloatingObject: TdxFloatingObject;
		FName: string;
		FExcludeFromLayout: Boolean;
    function GetActualSize: TSize;
    function GetActualSizeF: TdxSizeF;
    function GetContent: TdxCustomObjectContent;
    function GetFloatingObjectProperties: TdxFloatingObjectProperties;
    function GetOriginalSize: TSize;
    function GetPictureContent: TdxPictureFloatingObjectContent;
    function GetScaleX: Single;
    function GetScaleY: Single;
    function GetShape: TdxShape;
    procedure SetActualSize(const Value: TSize);
    procedure SetScaleX(const Value: Single);
    procedure SetScaleY(const Value: Single);
  public
    constructor Create(AParagraph: TdxParagraphBase; AStartIndex: Integer = 0; ALength: Integer = 1); override;
    destructor Destroy; override;
    procedure AfterRunInserted; override;
    procedure BeforeRunRemoved; override;
    function Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase; override;
    function CanJoinWith(ANextRun: TdxTextRunBase): Boolean; override;
    procedure Measure(ABoxInfo: TdxBoxInfo; const AMeasurer: IdxObjectMeasurer); override;
    procedure SetContent(AContent: TdxCustomObjectContent);
    function TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean; override;

  {$REGION 'interfaces'}
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
    function GetDocumentModel: TdxCustomDocumentModel;
    function GetLockAspectRatio: Boolean;
    function GetRun: TdxTextRunBase;
    function IdxPictureContainerRun.GetPictureContent = PictureContainerRunGetPictureContent;
    function PictureContainerRunGetPictureContent: TdxPictureFloatingObjectContent;
    procedure SetActualSizeInternal(const AActualSize: TSize);
    procedure Select;
    procedure SetLockAspectRatio(const Value: Boolean);

    property ActualSize: TSize read GetActualSize write SetActualSize;
    property ActualSizeF: TdxSizeF read GetActualSizeF;
    property LockAspectRatio: Boolean read GetLockAspectRatio write SetLockAspectRatio;
    property OriginalSize: TSize read GetOriginalSize;
    property ScaleX: Single read GetScaleX write SetScaleX;
    property ScaleY: Single read GetScaleY write SetScaleY;
  {$ENDREGION}


    property PictureContent: TdxPictureFloatingObjectContent read GetPictureContent;
    function CanPlaceCaretBefore: Boolean; override;

    property FloatingObjectProperties: TdxFloatingObjectProperties read GetFloatingObjectProperties;
    property FloatingObject: TdxFloatingObject read FFloatingObject;
    property Shape: TdxShape read GetShape;
    property Content: TdxCustomObjectContent read GetContent;
    property Name: string read FName write FName;
    property ExcludeFromLayout: Boolean read FExcludeFromLayout write FExcludeFromLayout;
  end;

implementation

uses
  Contnrs, Math,
  dxTypeHelpers,
  dxRichEdit.DocumentModel.PieceTable,

  dxRichEdit.Options,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.Commands;

{ TdxTextBoxContentType }

constructor TdxTextBoxContentType.Create(APieceTable: TdxSimplePieceTable);
begin
  inherited Create(APieceTable);
end;

constructor TdxTextBoxContentType.Create(ADocumentModel: TdxSimpleDocumentModel);
begin
  inherited Create(ADocumentModel);
  TdxDocumentModel(ADocumentModel).UnsafeEditor.InsertFirstParagraph(PieceTable);
  TdxPieceTable(PieceTable).SpellCheckerManager := TdxPieceTable(ADocumentModel.MainPieceTable).SpellCheckerManager.CreateInstance(TdxPieceTable(PieceTable));
  TdxPieceTable(PieceTable).SpellCheckerManager.Initialize;
end;

function TdxTextBoxContentType.GetIsMain: Boolean;
begin
  Result := False;
end;

function TdxTextBoxContentType.GetIsTextBox: Boolean;
begin
  Result := True;
end;

function TdxTextBoxContentType.GetIsReferenced: Boolean;
begin
  Result := AnchorRun <> nil;
end;

function TdxTextBoxContentType.CalculateAnchorPieceTableStartRunIndex(ARunIndex: TdxRunIndex): TdxRunIndex;
begin
  Result := AnchorRun.Paragraph.FirstRunIndex;
end;

function TdxTextBoxContentType.CalculateAnchorPieceTableEndRunIndex(ARunIndex: TdxRunIndex): TdxRunIndex;
begin
  Result := AnchorRun.Paragraph.LastRunIndex;
end;

function TdxTextBoxContentType.LookupSectionIndexByParagraphIndex(AParagraphIndex: TdxParagraphIndex): TdxSectionIndex;
begin
  Result := dxSectionIndexDontCare;
end;

procedure TdxTextBoxContentType.ApplyChanges(AChangeType: TdxDocumentModelChangeType; AStartRunIndex: TdxRunIndex; AEndRunIndex: TdxRunIndex);
begin
  if not IsReferenced then
    inherited ApplyChanges(AChangeType, AStartRunIndex, AEndRunIndex)
  else
    AnchorRun.PieceTable.ApplyChanges(AChangeType, CalculateAnchorPieceTableStartRunIndex(AStartRunIndex), CalculateAnchorPieceTableEndRunIndex(AEndRunIndex));
end;

procedure TdxTextBoxContentType.ApplyChangesCore(AActions: TdxDocumentModelChangeActions; AStartRunIndex: TdxRunIndex; AEndRunIndex: TdxRunIndex);
begin
  if not IsReferenced then
  begin
    inherited ApplyChangesCore(AActions, AStartRunIndex, AEndRunIndex);
    Exit;
  end;

  if TdxDocumentModelChangeAction.SplitRunByCharset in AActions then
  begin
    inherited ApplyChangesCore([TdxDocumentModelChangeAction.SplitRunByCharset], AStartRunIndex, AEndRunIndex);
    Exclude(AActions, TdxDocumentModelChangeAction.SplitRunByCharset);
  end;
  inherited ApplyChangesCore(AActions, CalculateAnchorPieceTableStartRunIndex(AStartRunIndex), CalculateAnchorPieceTableEndRunIndex(AEndRunIndex));
end;

procedure TdxTextBoxContentType.FixLastParagraphOfLastSection(AOriginalParagraphCount: Integer);
begin
end;

function TdxTextBoxContentType.GetPieceTable: TdxSimplePieceTable;
begin
  Result := TdxSimplePieceTable(inherited PieceTable);
end;

{ TdxFloatingObjectContent }

procedure TdxFloatingObjectContent.Dispose;
begin
  if Self <> Empty then
    Free;
end;

class constructor TdxFloatingObjectContent.Initialize;
begin
  FEmpty := TdxEmptyFloatingObjectContent.Create;
end;

class destructor TdxFloatingObjectContent.Finalize;
begin
  FreeAndNil(FEmpty);
end;

{ TdxEmptyFloatingObjectContent }

function TdxEmptyFloatingObjectContent.Clone(const ARun: TdxTextRunBase;
  ACopyManager: TdxCustomDocumentModelCopyManager): TdxCustomObjectContent;
begin
  Result := TdxEmptyFloatingObjectContent.Create;
end;

constructor TdxEmptyFloatingObjectContent.Create;
begin
  inherited Create(nil);
end;

function TdxEmptyFloatingObjectContent.GetOriginalSize: TSize;
begin
  Result := cxNullSize;
end;

procedure TdxEmptyFloatingObjectContent.SetOriginalSize(const AValue: TSize);
begin
end;

{ TdxTextBoxFloatingObjectContent }

constructor TdxTextBoxFloatingObjectContent.Create(AAnchor: TdxFloatingObjectAnchorRun; AContent: TdxTextBoxContentType);
begin
  inherited Create(AAnchor);
  FTextBox := AContent;
  FTextBoxProperties := TdxTextBoxProperties.Create(Self);
end;

destructor TdxTextBoxFloatingObjectContent.Destroy;
begin
  FreeAndNil(FTextBoxProperties);
  inherited Destroy;
end;

function TdxTextBoxFloatingObjectContent.GetOriginalSize: TSize;
begin
  Result.Init(1000, 1000);
end;

procedure TdxTextBoxFloatingObjectContent.SetOriginalSize(const AValue: TSize);
begin
end;

procedure TdxTextBoxFloatingObjectContent.SetAnchorRun(AAnchor: TdxFloatingObjectAnchorRun);
begin
  FTextBox.AnchorRun := AAnchor;
end;

function TdxTextBoxFloatingObjectContent.Clone(const ARun: TdxTextRunBase;
  ACopyManager: TdxCustomDocumentModelCopyManager): TdxCustomObjectContent;
var
  AContentTextBox: TdxTextBoxContentType;
  ATempDocumentModel: TdxDocumentModel;
  ATextBoxPieceTable: TdxPieceTable;
  AOptions: TdxDocumentModelCopyOptions;
  ACopyCommand: TdxDocumentModelCopyCommand;
  AInsertOptions: TdxInsertOptions;
  ACommand: TdxPieceTableInsertContentConvertedToDocumentModelCommand;
begin
  AContentTextBox := TdxTextBoxContentType.Create(ARun.DocumentModel);

  ATempDocumentModel := TdxDocumentModel(ARun.DocumentModel).CreateNew;
  try
    ATempDocumentModel.IntermediateModel := True;
    ATempDocumentModel.FieldOptions.CopyFrom(TdxDocumentModel(ARun.DocumentModel).FieldOptions);
    ATextBoxPieceTable := TdxPieceTable(TextBox.PieceTable);
    AOptions := TdxDocumentModelCopyOptions.Create(ATextBoxPieceTable.DocumentStartLogPosition,
      ATextBoxPieceTable.DocumentEndLogPosition - ATextBoxPieceTable.DocumentStartLogPosition + 1);
    try
      AOptions.DefaultPropertiesCopyOptions := TdxDefaultPropertiesCopyOptions.Always;
      ACopyCommand := TdxDocumentModelCopyCommand.Create(ATextBoxPieceTable, ATempDocumentModel, AOptions);
      try
        ACopyCommand.Execute;
      finally
        ACopyCommand.Free;
      end;
    finally
      AOptions.Free;
    end;

    AInsertOptions := GetInsertOptions(ACopyManager.FormattingCopyOptions);
    ACommand := TdxPieceTableInsertContentConvertedToDocumentModelCommand.Create(TdxPieceTable(AContentTextBox.PieceTable),
      TdxDocumentModel(ATempDocumentModel), 0, AInsertOptions, False);
    try
      ACommand.SuppressFieldsUpdate := True;
      ACommand.Execute;
    finally
      ACommand.Free;
    end;
    TdxPieceTable(AContentTextBox.PieceTable).FixLastParagraph;
  finally
    ATempDocumentModel.Free;
  end;
  Result := TdxTextBoxFloatingObjectContent.Create(ARun as TdxFloatingObjectAnchorRun, AContentTextBox);
  TdxTextBoxFloatingObjectContent(Result).TextBoxProperties.CopyFrom(TextBoxProperties.Info);
end;

function TdxTextBoxFloatingObjectContent.GetInsertOptions(AFormattingCopyOptions: TdxFormattingCopyOptions): TdxInsertOptions;
begin
  case AFormattingCopyOptions of
    TdxFormattingCopyOptions.KeepSourceFormatting:
      Exit(TdxInsertOptions.KeepSourceFormatting);
    TdxFormattingCopyOptions.UseDestinationStyles:
      Exit(TdxInsertOptions.MatchDestinationFormatting);
  else
    Assert(False);
    Exit(TdxInsertOptions.MatchDestinationFormatting);
  end;
end;

function TdxTextBoxFloatingObjectContent.GetPieceTable: TdxSimplePieceTable;
begin
  Result := Run.PieceTable;
end;

function TdxTextBoxFloatingObjectContent.GetTextBoxPropertiesContainerPieceTable: TdxCustomPieceTable;
begin
  Result := PieceTable;
end;

function TdxTextBoxFloatingObjectContent.CreateTextBoxChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(Run.PieceTable, TextBoxProperties);
end;

procedure TdxTextBoxFloatingObjectContent.OnTextBoxChanged;
begin
end;

{ TdxExplicitFloatingObjectLocation }

function TdxExplicitFloatingObjectLocation.GetActualHeight: Integer;
begin
  Result := FActualHeight;
end;

function TdxExplicitFloatingObjectLocation.GetActualWidth: Integer;
begin
  Result := FActualWidth;
end;

function TdxExplicitFloatingObjectLocation.GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
begin
  Result := FHorizontalPositionAlignment;
end;

function TdxExplicitFloatingObjectLocation.GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
begin
  Result := FHorizontalPositionType;
end;

function TdxExplicitFloatingObjectLocation.GetOffsetX: Integer;
begin
  Result := FOffsetX;
end;

function TdxExplicitFloatingObjectLocation.GetOffsetY: Integer;
begin
  Result := FOffsetY;
end;

function TdxExplicitFloatingObjectLocation.GetPercentOffsetX: Integer;
begin
  Result := FPercentOffsetX;
end;

function TdxExplicitFloatingObjectLocation.GetPercentOffsetY: Integer;
begin
  Result := FPercentOffsetY;
end;

function TdxExplicitFloatingObjectLocation.GetRelativeHeight: TdxFloatingObjectRelativeHeight;
begin
  Result := FRelativeHeight;
end;

function TdxExplicitFloatingObjectLocation.GetRelativeWidth: TdxFloatingObjectRelativeWidth;
begin
  Result := FRelativeWidth;
end;

function TdxExplicitFloatingObjectLocation.GetTextWrapType: TdxFloatingObjectTextWrapType;
begin
  Result := FTextWrapType;
end;

function TdxExplicitFloatingObjectLocation.GetUseRelativeHeight: Boolean;
begin
  Result := FUseRelativeHeight;
end;

function TdxExplicitFloatingObjectLocation.GetUseRelativeWidth: Boolean;
begin
  Result := FUseRelativeWidth;
end;

function TdxExplicitFloatingObjectLocation.GetLayoutInTableCell: Boolean;
begin
  Result := FLayoutInTableCell;
end;

function TdxExplicitFloatingObjectLocation.GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
begin
  Result := FVerticalPositionAlignment;
end;

function TdxExplicitFloatingObjectLocation.GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
begin
  Result := FVerticalPositionType;
end;

{ TdxFloatingObject }

constructor TdxFloatingObject.Create(AAnchorRun: TdxFloatingObjectAnchorRun);
begin
  inherited Create;
  FContent := TdxFloatingObjectContent.Empty;
  Assert(AAnchorRun <> nil);
  FAnchorRun := AAnchorRun;
  FProperties := TdxFloatingObjectProperties.Create(Self);
  FProperties.OnObtainAffectedRange.Add(OnObtainAffectedRange);
  FShape := TdxShape.Create(Self);
  FShape.OnObtainAffectedRange.Add(OnObtainAffectedRange);
end;

destructor TdxFloatingObject.Destroy;
begin
  FreeAndNil(FProperties);
  FreeAndNil(FShape);
  FContent.Dispose;
  inherited Destroy;
end;

procedure TdxFloatingObject.CopyFrom(AFloatingObject: TdxFloatingObject; ACopyManager: TdxCustomDocumentModelCopyManager);
var
  ATextBoxContent: TdxTextBoxFloatingObjectContent;
begin
  Properties.CopyFrom(AFloatingObject.Properties.Info);
  Shape.CopyFrom(AFloatingObject.Shape.Info);
  SetContent(AFloatingObject.Content.Clone(AnchorRun, ACopyManager));
  ATextBoxContent := Safe<TdxTextBoxFloatingObjectContent>.Cast(FContent);
  if (ATextBoxContent <> nil) and (ATextBoxContent.TextBox <> nil) then
  begin
    if TdxDocumentModel(DocumentModel).ModelForExport then
      TdxPieceTable(ATextBoxContent.TextBox.PieceTable).FieldUpdater.UpdateFields(TdxUpdateFieldOperationType.CreateModelForExport)
    else
      TdxPieceTable(ATextBoxContent.TextBox.PieceTable).FieldUpdater.UpdateFields(TdxUpdateFieldOperationType.Copy);
  end;
end;

procedure TdxFloatingObject.AfterRunInserted;
begin
  AssignTextBoxAnchorRun(AnchorRun);
end;

procedure TdxFloatingObject.AssignTextBoxAnchorRun(ARun: TdxFloatingObjectAnchorRun);
begin
  if FContent is TdxTextBoxFloatingObjectContent then
    TdxTextBoxFloatingObjectContent(FContent).SetAnchorRun(ARun);
end;

procedure TdxFloatingObject.BeforeRunRemoved;
begin
  if (FContent is TdxTextBoxFloatingObjectContent) and
    IsTextBoxActive(TdxTextBoxFloatingObjectContent(FContent).TextBox) then
    DeactivateTextBox;
  AssignTextBoxAnchorRun(nil);
end;

function TdxFloatingObject.CreateFloatingObjectPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(PieceTable, Properties);
end;

function TdxFloatingObject.CreateShapeChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(PieceTable, Shape);
end;

procedure TdxFloatingObject.DeactivateTextBox;
var
  AHeaderFooter: TdxSectionHeaderFooterBase;
begin
  if PieceTable.ContentType.IsHeaderFooter then
  begin
    AHeaderFooter := Safe<TdxSectionHeaderFooterBase>.Cast(PieceTable.ContentType);
    TdxDocumentModel(DocumentModel).SetActivePieceTable(TdxPieceTable(PieceTable), AHeaderFooter.GetSection);
  end
  else
    DocumentModel.SetActivePieceTable(DocumentModel.MainPart);
  SetSelectionByAnchorPosition;
end;

function TdxFloatingObject.GetPieceTableProperty: TdxSimplePieceTable;
begin
  Result := AnchorRun.PieceTable;
end;

function TdxFloatingObject.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := PieceTable.DocumentModel;
end;

function TdxFloatingObject.GetPieceTable: TdxCustomPieceTable;
begin
  Result := PieceTable;
end;

function TdxFloatingObject.IsTextBoxActive(ATextBox: TdxTextBoxContentType): Boolean;
begin
  Result := ATextBox.PieceTable = DocumentModel.GetActivePieceTableCore;
end;

procedure TdxFloatingObject.OnFloatingObjectChanged;
begin
end;

procedure TdxFloatingObject.OnObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs);
begin
  AnchorRun.OnCharacterPropertiesObtainAffectedRange(ASender, E);
end;

procedure TdxFloatingObject.OnShapeChanged;
begin
end;

procedure TdxFloatingObject.RegisterTextBoxPieceTable;
begin
  if (FContent is TdxTextBoxFloatingObjectContent) and (TdxTextBoxFloatingObjectContent(FContent).TextBox <> nil) then
    TdxPieceTable(PieceTable).TextBoxes.Add(TdxTextBoxFloatingObjectContent(FContent).TextBox);
end;

procedure TdxFloatingObject.SetContent(AContent: TdxCustomObjectContent);
begin
  Assert(AContent <> nil);
  Assert(Content is TdxEmptyFloatingObjectContent);
  Assert(AContent.Run = AnchorRun);

  AssignTextBoxAnchorRun(nil);
  FContent.Dispose;
  FContent := AContent;
  AssignTextBoxAnchorRun(AnchorRun);

  RegisterTextBoxPieceTable;
end;

procedure TdxFloatingObject.SetSelectionByAnchorPosition;
var
  ARunIndex: TdxRunIndex;
  AAnchorPosition: TdxDocumentModelPosition;
begin
  if PieceTable <> DocumentModel.GetActivePieceTableCore then
    Exit;
  ARunIndex := AnchorRun.GetRunIndex;
  AAnchorPosition := TdxDocumentModelPosition.FromRunStart(PieceTable, ARunIndex);
  DocumentModel.Selection.Start := AAnchorPosition.LogPosition;
  DocumentModel.Selection.&End := AAnchorPosition.LogPosition;
end;

{ TdxFloatingObjectAnchorRun }

procedure TdxFloatingObjectAnchorRun.AfterRunInserted;
begin
  FFloatingObject.AfterRunInserted;
end;

procedure TdxFloatingObjectAnchorRun.BeforeRunRemoved;
begin
  FFloatingObject.BeforeRunRemoved;
end;

function TdxFloatingObjectAnchorRun.Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase;
var
  ATargetPieceTable: TdxPieceTable;
  ATargetPosition: TdxDocumentModelPosition;
  ARun: TdxFloatingObjectAnchorRun;
begin
  ATargetPieceTable := TdxPieceTable(ACopyManager.TargetPieceTable);
  ATargetPosition := ACopyManager.TargetPosition;
  Assert(DocumentModel = ACopyManager.SourceModel);
  Assert(ATargetPosition.PieceTable = ATargetPieceTable);
  Assert(ATargetPosition.RunOffset = 0);
  if not ACopyManager.TargetModel.DocumentCapabilities.FloatingObjectsAllowed then
  begin
    ATargetPieceTable.InsertText(ATargetPosition.LogPosition, ' ');
    Result := ATargetPieceTable.Runs[ATargetPosition.RunIndex];
    Exit;
  end;
  ATargetPieceTable.DocumentModel.BeginUpdate;
  try
    ARun := ATargetPieceTable.InsertFloatingObjectAnchorCore(ATargetPosition.ParagraphIndex,
      ATargetPosition.LogPosition);
    ARun.FFloatingObject.CopyFrom(FFloatingObject, ACopyManager);
    ARun.Name := Name;
  finally
    ATargetPieceTable.DocumentModel.EndUpdate;
  end;
  Result := ARun;
end;

function TdxFloatingObjectAnchorRun.CanJoinWith(ANextRun: TdxTextRunBase): Boolean;
begin
  Result := False;
end;

function TdxFloatingObjectAnchorRun.CanPlaceCaretBefore: Boolean;
begin
  Result := True;
end;

constructor TdxFloatingObjectAnchorRun.Create(AParagraph: TdxParagraphBase; AStartIndex: Integer = 0; ALength: Integer = 1);
begin
  inherited Create(AParagraph);
  FFloatingObject := TdxFloatingObject.Create(Self);
end;

destructor TdxFloatingObjectAnchorRun.Destroy;
begin
  FreeAndNil(FFloatingObject);
  inherited Destroy;
end;

function TdxFloatingObjectAnchorRun.GetActualSize: TSize;
begin
  Result := FloatingObjectProperties.ActualSize;
end;

function TdxFloatingObjectAnchorRun.GetActualSizeF: TdxSizeF;
var
  AActualSize: TSize;
begin
  AActualSize := FloatingObjectProperties.ActualSize;
  Result.cx := AActualSize.cx;
  Result.cy := AActualSize.cy;
end;

function TdxFloatingObjectAnchorRun.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := [TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetPrimaryLayout,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ResetSelectionLayout,
    TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
    TdxDocumentModelChangeAction.ResetRuler];
end;

procedure TdxFloatingObjectAnchorRun.Select;
var
  ALogPosition: TdxDocumentLogPosition;
  ASelection: TdxSelection;
begin
  ALogPosition := PieceTable.GetRunLogPosition(Self);
  ASelection := TdxDocumentModel(DocumentModel).Selection;
  ASelection.BeginUpdate;
  try
    ASelection.Start := ALogPosition;
    ASelection.&End := ALogPosition + 1;
  finally
    ASelection.EndUpdate;
  end;
end;

function TdxFloatingObjectAnchorRun.TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean;
begin
  Result := False;
end;

function TdxFloatingObjectAnchorRun.GetContent: TdxCustomObjectContent;
begin
  Result := FFloatingObject.Content;
end;

function TdxFloatingObjectAnchorRun.GetFloatingObjectProperties: TdxFloatingObjectProperties;
begin
  Result := FFloatingObject.Properties;
end;

function TdxFloatingObjectAnchorRun.GetLockAspectRatio: Boolean;
begin
  Result := FFloatingObject.Properties.LockAspectRatio;
end;

function TdxFloatingObjectAnchorRun.GetOriginalSize: TSize;
begin
  Result := Content.OriginalSize;
end;

function TdxFloatingObjectAnchorRun.GetPictureContent: TdxPictureFloatingObjectContent;
begin
  Result := TdxPictureFloatingObjectContent(Content);
end;

function TdxFloatingObjectAnchorRun.GetDocumentModel: TdxCustomDocumentModel;
begin
  Result := DocumentModel;
end;

function TdxFloatingObjectAnchorRun.PictureContainerRunGetPictureContent: TdxPictureFloatingObjectContent;
begin
  Result := TdxPictureFloatingObjectContent(FFloatingObject.Content);
end;

function TdxFloatingObjectAnchorRun.GetRun: TdxTextRunBase;
begin
  Result := Self;
end;

function TdxFloatingObjectAnchorRun.GetScaleX: Single;
begin
  if PictureContent = nil then
    Result := 100
  else if PictureContent.InvalidActualSize then
    Result := 100
  else
    Result := 100 * FloatingObjectProperties.ActualSize.Width / Max(1, Content.OriginalSize.Width);
end;

function TdxFloatingObjectAnchorRun.GetScaleY: Single;
begin
  if PictureContent = nil then
    Result := 100
  else if PictureContent.InvalidActualSize then
    Result := 100
  else
    Result := 100 * FloatingObjectProperties.ActualSize.Height / Max(1, Content.OriginalSize.Height);
end;

function TdxFloatingObjectAnchorRun.GetShape: TdxShape;
begin
  Result := FFloatingObject.Shape;
end;

procedure TdxFloatingObjectAnchorRun.Measure(ABoxInfo: TdxBoxInfo; const AMeasurer: IdxObjectMeasurer);
begin
  ABoxInfo.Size := cxNullSize;
end;

procedure TdxFloatingObjectAnchorRun.SetActualSize(const Value: TSize);
begin
  FloatingObjectProperties.ActualSize := Value;
end;

procedure TdxFloatingObjectAnchorRun.SetActualSizeInternal(const AActualSize: TSize);
begin
 (Self as IdxPictureContainerRun).ActualSize := AActualSize;
end;

procedure TdxFloatingObjectAnchorRun.SetContent(AContent: TdxCustomObjectContent);
begin
  FFloatingObject.SetContent(AContent);
end;

procedure TdxFloatingObjectAnchorRun.SetLockAspectRatio(const Value: Boolean);
begin
  FFloatingObject.Properties.LockAspectRatio := Value;
end;

procedure TdxFloatingObjectAnchorRun.SetScaleX(const Value: Single);
var
  AVal: Integer;
begin
  AVal := Round(Abs(Value) * Math.Max(1, Content.OriginalSize.Width) / 100);
  if FloatingObjectProperties.ActualSize.Width = AVal then
    Exit;
  FloatingObjectProperties.ActualSize := cxSize(AVal, FloatingObjectProperties.ActualSize.Height);
end;

procedure TdxFloatingObjectAnchorRun.SetScaleY(const Value: Single);
var
  AVal: Integer;
begin
  AVal := Round(Abs(Value) * Max(1, Content.OriginalSize.Height) / 100);
  if FloatingObjectProperties.ActualSize.Height = AVal then
    Exit;
  FloatingObjectProperties.ActualSize := cxSize(FloatingObjectProperties.ActualSize.Width, AVal);
end;

end.
