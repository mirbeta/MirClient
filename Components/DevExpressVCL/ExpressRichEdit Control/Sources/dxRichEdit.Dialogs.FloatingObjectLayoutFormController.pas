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

unit dxRichEdit.Dialogs.FloatingObjectLayoutFormController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes, SysUtils, Variants, Types,
  dxCoreClasses,

  Generics.Defaults, Generics.Collections,
  dxRichEdit.Types,
  dxRichEdit.View.Core,
  dxRichEdit.Utils.Types,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.LayoutEngine.Formatter,
  dxGenerics,
  dxRichEdit.Utils.Properties;

type
  { TdxFloatingInlineObjectLayoutOptionsFormControllerParameters }

  TdxFloatingInlineObjectLayoutOptionsFormControllerParameters = class(TdxFormControllerParameters)
  strict private
    FFloatingInlineObjectParameters: TdxFloatingInlineObjectParameters;
    FInitialTabPage: TdxPageSetupFormInitialTabPage;
    FPlacementInfo: TdxFloatingObjectTargetPlacementInfo;
    FOriginalSize: TdxNullableValue<TSize>;
    FFloatingObjectProperties: TdxFloatingObjectProperties;
  protected
    property InitialTabPage: TdxPageSetupFormInitialTabPage read FInitialTabPage write FInitialTabPage;
    property PlacementInfo: TdxFloatingObjectTargetPlacementInfo read FPlacementInfo;
    property OriginalSize: TdxNullableValue<TSize> read FOriginalSize;
  public
    constructor Create(const AControl: IdxRichEditControl; const AFloatingInlineObjectParameters: TdxFloatingInlineObjectParameters);
    procedure InitialFillPlacementInfo(APieceTable: TdxCustomPieceTable; AParagraph: TdxParagraphBase);
    property FloatingInlineObjectParameters: TdxFloatingInlineObjectParameters read FFloatingInlineObjectParameters;
    property FloatingObjectProperties: TdxFloatingObjectProperties read FFloatingObjectProperties write FFloatingObjectProperties;
  end;

  { TdxFloatingObjectRichTextIndentEditProperties }

  TdxFloatingObjectRichTextIndentEditProperties = record
  strict private
    FMaxValue: Integer;
    FDefaultUnitType: TdxMeasurementUnit;
    FValueUnitConverter: TdxDocumentModelUnitConverter;
  public
    constructor Create(ADefaultUnitType: TdxMeasurementUnit; AValueUnitConverter: TdxDocumentModelUnitConverter);
    function GetMinValue(AAllowNegativeValues: Boolean): Integer;

    property MaxValue: Integer read FMaxValue;
    property DefaultUnitType: TdxMeasurementUnit read FDefaultUnitType;
    property ValueUnitConverter: TdxDocumentModelUnitConverter read FValueUnitConverter;
  end;

  { TdxTextWrapTypeInfoPreset }

  TdxTextWrapTypeInfoPreset = packed record
  public
    TextWrapType: TdxFloatingObjectTextWrapType;
    IsBehindDocument: Boolean;
    constructor Create(ATextWrapType: TdxFloatingObjectTextWrapType; AIsBehindDocument: Boolean);
  end;

  { TdxFloatingObjectLayoutOptionsFormController }

  TdxFloatingObjectLayoutOptionsFormController = class(TdxFormController)
  strict private
    FControllerParameters: TdxFloatingInlineObjectLayoutOptionsFormControllerParameters;
    FFloatingInlineObjectParameters: TdxFloatingInlineObjectParameters;
    FUIUnitConverter: TdxUIUnitConverter;
    FHorizontalPositionAlignment: TdxNullableValue<TdxFloatingObjectHorizontalPositionAlignment>;
    FVerticalPositionAlignment: TdxNullableValue<TdxFloatingObjectVerticalPositionAlignment>;
    FVerticalPositionType: TdxNullableValue<TdxFloatingObjectVerticalPositionType>;
    FHorizontalPositionType: TdxNullableValue<TdxFloatingObjectHorizontalPositionType>;
    FLocked: TdxNullableBoolean;
    FOffsetX: TdxNullableInteger;
    FOffsetY: TdxNullableInteger;
    FIsHorizontalAbsolutePosition: Boolean;
    FIsVerticalAbsolutePosition: Boolean;
    FIsBehindDocument: TdxNullableBoolean;
    FFloatingObjectRichTextIndentEditProperties: TdxFloatingObjectRichTextIndentEditProperties;
    FTextWrapType: TdxFloatingObjectTextWrapType;
    FTextWrapSide: TdxNullableValue<TdxFloatingObjectTextWrapSide>;
    FTopDistance: TdxNullableInteger;
    FBottomDistance: TdxNullableInteger;
    FLeftDistance: TdxNullableInteger;
    FRightDistance: TdxNullableInteger;
    FActualHeight: Integer;
    FActualWidth: Integer;
    FRotation: TdxNullableInteger;
    FLockAspectRatio: Boolean;
    function GetFloatingObjectAnchorRun: TdxFloatingObjectAnchorRun;
    function GetInlinePictureRun: TdxInlinePictureRun;
    function GetFloatingObjectProperties: TdxFloatingObjectProperties;
    function GetControl: IdxRichEditControl;
    function GetUIType: TdxMeasurementUnit;
    function GetDocumentModel: TdxDocumentModel;
    function GetActiveView: TdxRichEditView;
    function GetInitialTabPage: TdxPageSetupFormInitialTabPage;
    function GetPlacementInfo: TdxFloatingObjectTargetPlacementInfo;
    function GetToDocumentLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
    function GetOriginalSize: TdxNullableValue<TSize>;
    procedure SetFloatingObjectProperties(const Value: TdxFloatingObjectProperties);
    function GetHorizontalPositionAlignmentTable(Index: TdxFloatingObjectHorizontalPositionAlignment): string;
    function GetHorizontalPositionTypeTable(Index: TdxFloatingObjectHorizontalPositionType): string;
    function GetVerticalPositionAlignmentTable(Index: TdxFloatingObjectVerticalPositionAlignment): string;
    function GetVerticalPositionTypeTable(Index: TdxFloatingObjectVerticalPositionType): string;
  protected
    procedure InitializeController; virtual;
    procedure ApplyChangesInlinePicture;
    procedure SetPositionProperties(AFloatObjProp: TdxFloatingObjectProperties);
    procedure SetWrappingOptions(AFloatObjProp: TdxFloatingObjectProperties);
    procedure SetSizeOptions(ARun: TdxFloatingObjectAnchorRun; AFloatObjProp: TdxFloatingObjectProperties);
    procedure ApplyChangesFloatingObject(ARun: TdxFloatingObjectAnchorRun);
    function  SubstituteInlineToFloatingObjectRun: TdxFloatingObjectAnchorRun;

    property ControllerParameters: TdxFloatingInlineObjectLayoutOptionsFormControllerParameters read FControllerParameters;
    property FloatingInlineObjectParameters: TdxFloatingInlineObjectParameters read FFloatingInlineObjectParameters;
    property FloatingObjectAnchorRun: TdxFloatingObjectAnchorRun read GetFloatingObjectAnchorRun;
    property InlinePictureRun: TdxInlinePictureRun read GetInlinePictureRun;
    property FloatingObjectProperties: TdxFloatingObjectProperties read GetFloatingObjectProperties write SetFloatingObjectProperties;
    property Control: IdxRichEditControl read GetControl;
    property UIType: TdxMeasurementUnit read GetUIType;
    property ActiveView: TdxRichEditView read GetActiveView;
    property InitialTabPage: TdxPageSetupFormInitialTabPage read GetInitialTabPage;
  public
    constructor Create(AControllerParameters: TdxFloatingInlineObjectLayoutOptionsFormControllerParameters);
    procedure ApplyChanges; override;
    procedure ApplyPreset(const APreset: TdxTextWrapTypeInfoPreset);
    procedure ResetActualHeight;
    procedure ResetActualWidth;
    procedure ResetRotation;
    procedure RecalculateSizeDependingOnHeight(ALockAspectRatio: Boolean; ANewHeight: Integer);
    procedure RecalculateSizeDependingOnWidth(ALockAspectRatio: Boolean; ANewWidth: Integer);
    function StringToTwips(const S: string): Integer;
    function OriginalHeightAsString: string;
    function OriginalWidthAsString: string;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property FloatingObjectRichTextIndentEditProperties: TdxFloatingObjectRichTextIndentEditProperties read FFloatingObjectRichTextIndentEditProperties;
    property PlacementInfo: TdxFloatingObjectTargetPlacementInfo read GetPlacementInfo;
    property ToDocumentLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter read GetToDocumentLayoutUnitConverter;

    property HorizontalPositionAlignment: TdxNullableValue<TdxFloatingObjectHorizontalPositionAlignment> read FHorizontalPositionAlignment write FHorizontalPositionAlignment;
    property VerticalPositionAlignment: TdxNullableValue<TdxFloatingObjectVerticalPositionAlignment> read FVerticalPositionAlignment write FVerticalPositionAlignment;
    property VerticalPositionType: TdxNullableValue<TdxFloatingObjectVerticalPositionType> read FVerticalPositionType write FVerticalPositionType;
    property HorizontalPositionType: TdxNullableValue<TdxFloatingObjectHorizontalPositionType> read FHorizontalPositionType write FHorizontalPositionType;
    property Locked: TdxNullableBoolean read FLocked write FLocked;
    property TextWrapType: TdxFloatingObjectTextWrapType read FTextWrapType write FTextWrapType;
    property TextWrapSide: TdxNullableValue<TdxFloatingObjectTextWrapSide> read FTextWrapSide write FTextWrapSide;
    property IsBehindDocument: TdxNullableBoolean read FIsBehindDocument write FIsBehindDocument;
    property TopDistance: TdxNullableInteger read FTopDistance write FTopDistance;
    property BottomDistance: TdxNullableInteger read FBottomDistance write FBottomDistance;
    property LeftDistance: TdxNullableInteger read FLeftDistance write FLeftDistance;
    property RightDistance: TdxNullableInteger read FRightDistance write FRightDistance;

    property HorizontalPositionTypeTable[Index: TdxFloatingObjectHorizontalPositionType]: string read GetHorizontalPositionTypeTable;
    property HorizontalPositionAlignmentTable[Index: TdxFloatingObjectHorizontalPositionAlignment]: string read GetHorizontalPositionAlignmentTable;
    property VerticalPositionTypeTable[Index: TdxFloatingObjectVerticalPositionType]: string read GetVerticalPositionTypeTable;
    property VerticalPositionAlignmentTable[Index: TdxFloatingObjectVerticalPositionAlignment]: string read GetVerticalPositionAlignmentTable;

    property OffsetX: TdxNullableInteger read FOffsetX write FOffsetX;
    property OffsetY: TdxNullableInteger read FOffsetY write FOffsetY;
    property IsHorizontalAbsolutePosition: Boolean read FIsHorizontalAbsolutePosition write FIsHorizontalAbsolutePosition;
    property IsVerticalAbsolutePosition: Boolean read FIsVerticalAbsolutePosition write FIsVerticalAbsolutePosition;
    property OriginalSize: TdxNullableValue<TSize> read GetOriginalSize;
    property ActualHeight: Integer read FActualHeight write FActualHeight;
    property ActualWidth: Integer read FActualWidth write FActualWidth;
    property LockAspectRatio: Boolean read FLockAspectRatio write FLockAspectRatio;
    property Rotation: TdxNullableInteger read FRotation write FRotation;
  end;

implementation

uses
  Math, dxTypeHelpers, dxCore,
  dxRichEdit.DocumentLayout.Position,
  dxRichEdit.Dialogs.Strs,
  dxMeasurementUnits;

{ TdxFloatingInlineObjectLayoutOptionsFormControllerParameters }

constructor TdxFloatingInlineObjectLayoutOptionsFormControllerParameters.Create(const AControl: IdxRichEditControl;
  const AFloatingInlineObjectParameters: TdxFloatingInlineObjectParameters);
begin
  inherited Create(AControl);
  FFloatingInlineObjectParameters := AFloatingInlineObjectParameters;
  FPlacementInfo.Init;
  if AFloatingInlineObjectParameters.FloatingObjectAnchorRun <> nil then
  begin
    FFloatingObjectProperties := AFloatingInlineObjectParameters.FloatingObjectAnchorRun.FloatingObjectProperties;
    InitialFillPlacementInfo(AFloatingInlineObjectParameters.FloatingObjectAnchorRun.PieceTable, AFloatingInlineObjectParameters.FloatingObjectAnchorRun.Paragraph);

    FOriginalSize := AFloatingInlineObjectParameters.FloatingObjectAnchorRun.OriginalSize;
  end
  else
  begin
    FFloatingObjectProperties := nil;
    InitialFillPlacementInfo(AFloatingInlineObjectParameters.InlinePictureRun.PieceTable, AFloatingInlineObjectParameters.InlinePictureRun.Paragraph);
    FOriginalSize := AFloatingInlineObjectParameters.InlinePictureRun.OriginalSize;
  end;
end;

procedure TdxFloatingInlineObjectLayoutOptionsFormControllerParameters.InitialFillPlacementInfo(APieceTable: TdxCustomPieceTable; AParagraph: TdxParagraphBase);
var
  AActiveView: TdxRichEditView;
  ALogPosition: TdxDocumentLogPosition;
  APositionWithDetailsLevelColumn, APositionWithDetailsLevelPage: TdxDocumentLayoutPosition;
begin
  AActiveView := Control.InnerControl.ActiveView;
  ALogPosition := APieceTable.Paragraphs[AParagraph.Index].LogPosition;

  APositionWithDetailsLevelColumn := AActiveView.DocumentLayout.CreateLayoutPosition(APieceTable, ALogPosition, 0) as TdxDocumentLayoutPosition;
  try
    APositionWithDetailsLevelColumn.Update(AActiveView.DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Column);
    FPlacementInfo.ColumnBounds := APositionWithDetailsLevelColumn.Column.Bounds;

    APositionWithDetailsLevelPage := AActiveView.DocumentLayout.CreateLayoutPosition(APieceTable, ALogPosition, 0) as TdxDocumentLayoutPosition;
    try
      APositionWithDetailsLevelPage.Update(AActiveView.DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Page);
      FPlacementInfo.PageBounds := APositionWithDetailsLevelColumn.Page.Bounds;
      FPlacementInfo.PageClientBounds := APositionWithDetailsLevelColumn.Page.ClientBounds;
    finally
      APositionWithDetailsLevelPage.Free;
    end;
  finally
    APositionWithDetailsLevelColumn.Free;
  end;
end;

{ TdxFloatingObjectRichTextIndentEditProperties }

constructor TdxFloatingObjectRichTextIndentEditProperties.Create(ADefaultUnitType: TdxMeasurementUnit;
  AValueUnitConverter: TdxDocumentModelUnitConverter);
begin
  FMaxValue := AValueUnitConverter.TwipsToModelUnits(1440 * 22);
  FDefaultUnitType := ADefaultUnitType;
  FValueUnitConverter := AValueUnitConverter;
end;

function TdxFloatingObjectRichTextIndentEditProperties.GetMinValue(AAllowNegativeValues: Boolean): Integer;
begin
  if AAllowNegativeValues then
    Result := -FValueUnitConverter.TwipsToModelUnits(1440 * 22)
  else
    Result := 0;
end;

{ TdxTextWrapTypeInfoPreset }

constructor TdxTextWrapTypeInfoPreset.Create(ATextWrapType: TdxFloatingObjectTextWrapType; AIsBehindDocument: Boolean);
begin
  TextWrapType := ATextWrapType;
  IsBehindDocument := AIsBehindDocument;
end;

{ TdxFloatingObjectLayoutOptionsFormController }

constructor TdxFloatingObjectLayoutOptionsFormController.Create(AControllerParameters: TdxFloatingInlineObjectLayoutOptionsFormControllerParameters);
begin
  Assert(AControllerParameters <> nil);
  inherited Create;
  FControllerParameters := AControllerParameters;
  FFloatingInlineObjectParameters := AControllerParameters.FloatingInlineObjectParameters;
  FUIUnitConverter := TdxUIUnitConverter.Create(TdxUnitPrecisions.DefaultPrecisions);
  InitializeController;
end;

function TdxFloatingObjectLayoutOptionsFormController.GetFloatingObjectAnchorRun: TdxFloatingObjectAnchorRun;
begin
  Result := FFloatingInlineObjectParameters.FloatingObjectAnchorRun;
end;

function TdxFloatingObjectLayoutOptionsFormController.GetInlinePictureRun: TdxInlinePictureRun;
begin
  Result := FloatingInlineObjectParameters.InlinePictureRun;
end;

function TdxFloatingObjectLayoutOptionsFormController.GetFloatingObjectProperties: TdxFloatingObjectProperties;
begin
  Result := FControllerParameters.FloatingObjectProperties;
end;

function TdxFloatingObjectLayoutOptionsFormController.GetHorizontalPositionAlignmentTable(
  Index: TdxFloatingObjectHorizontalPositionAlignment): string;
begin
  case Index of
    TdxFloatingObjectHorizontalPositionAlignment.Left:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_HorizontalPositionAlignmentLeft);
    TdxFloatingObjectHorizontalPositionAlignment.Center:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_HorizontalPositionAlignmentCenter);
    TdxFloatingObjectHorizontalPositionAlignment.Right:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_HorizontalPositionAlignmentRight);
    else
      Result := '';
  end;
end;

function TdxFloatingObjectLayoutOptionsFormController.GetHorizontalPositionTypeTable(
  Index: TdxFloatingObjectHorizontalPositionType): string;
begin
  case Index of
    TdxFloatingObjectHorizontalPositionType.Margin:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeMargin);
    TdxFloatingObjectHorizontalPositionType.Page:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypePage);
    TdxFloatingObjectHorizontalPositionType.Column:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeColumn);
    TdxFloatingObjectHorizontalPositionType.Character:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeCharacter);
    TdxFloatingObjectHorizontalPositionType.LeftMargin:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeLeftMargin);
    TdxFloatingObjectHorizontalPositionType.RightMargin:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeRightMargin);
    TdxFloatingObjectHorizontalPositionType.InsideMargin:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeInsideMargin);
    TdxFloatingObjectHorizontalPositionType.OutsideMargin:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeOutsideMargin);
    else
      Result := '';
  end;
end;

function TdxFloatingObjectLayoutOptionsFormController.GetControl: IdxRichEditControl;
begin
  Result := ControllerParameters.Control;
end;

function TdxFloatingObjectLayoutOptionsFormController.GetUIType: TdxMeasurementUnit;
begin
  Result := Control.InnerControl.UIUnit;
end;

function TdxFloatingObjectLayoutOptionsFormController.GetVerticalPositionAlignmentTable(
  Index: TdxFloatingObjectVerticalPositionAlignment): string;
begin
  case Index of
    TdxFloatingObjectVerticalPositionAlignment.Top:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_VerticalPositionAlignmentTop);
    TdxFloatingObjectVerticalPositionAlignment.Center:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_VerticalPositionAlignmentCenter);
    TdxFloatingObjectVerticalPositionAlignment.Bottom:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_VerticalPositionAlignmentBottom);
    TdxFloatingObjectVerticalPositionAlignment.Inside:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_VerticalPositionAlignmentInside);
    TdxFloatingObjectVerticalPositionAlignment.Outside:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_VerticalPositionAlignmentOutside);
    else
      Result := '';
  end;
end;

function TdxFloatingObjectLayoutOptionsFormController.GetVerticalPositionTypeTable(
  Index: TdxFloatingObjectVerticalPositionType): string;
begin
  case Index of
    TdxFloatingObjectVerticalPositionType.Margin:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeMargin);
    TdxFloatingObjectVerticalPositionType.Page:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypePage);
    TdxFloatingObjectVerticalPositionType.Line:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeLine);
    TdxFloatingObjectVerticalPositionType.TopMargin:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeTopMargin);
    TdxFloatingObjectVerticalPositionType.BottomMargin:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeBottomMargin);
    TdxFloatingObjectVerticalPositionType.InsideMargin:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeInsideMargin);
    TdxFloatingObjectVerticalPositionType.OutsideMargin:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeOutsideMargin);
    TdxFloatingObjectVerticalPositionType.Paragraph:
      Result := cxGetResourceString(@sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeParagraph);
    else
      Result := '';
  end;
end;

function TdxFloatingObjectLayoutOptionsFormController.GetDocumentModel: TdxDocumentModel;
begin
  Result := Control.InnerControl.DocumentModel;
end;

function TdxFloatingObjectLayoutOptionsFormController.GetActiveView: TdxRichEditView;
begin
  Result := Control.InnerControl.ActiveView;
end;

function TdxFloatingObjectLayoutOptionsFormController.GetInitialTabPage: TdxPageSetupFormInitialTabPage;
begin
  Result := ControllerParameters.InitialTabPage;
end;

function TdxFloatingObjectLayoutOptionsFormController.GetPlacementInfo: TdxFloatingObjectTargetPlacementInfo;
begin
  Result := ControllerParameters.PlacementInfo;
end;

function TdxFloatingObjectLayoutOptionsFormController.GetToDocumentLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
begin
  Result := Control.InnerControl.DocumentModel.ToDocumentLayoutUnitConverter;
end;

function TdxFloatingObjectLayoutOptionsFormController.GetOriginalSize: TdxNullableValue<TSize>;
begin
  Result := ControllerParameters.OriginalSize;
end;

procedure TdxFloatingObjectLayoutOptionsFormController.InitializeController;
var
  AInnerControl: IdxInnerControl;
  AUnitConverter: TdxDocumentModelUnitConverter;
begin
  AInnerControl := Control.InnerControl;
  AUnitConverter := AInnerControl.DocumentModel.UnitConverter;
  FFloatingObjectRichTextIndentEditProperties := TdxFloatingObjectRichTextIndentEditProperties.Create(AInnerControl.UIUnit, AUnitConverter);

  if FloatingObjectAnchorRun = nil then
  begin
    HorizontalPositionAlignment := TdxFloatingObjectHorizontalPositionAlignment.Center;
    HorizontalPositionType := TdxFloatingObjectHorizontalPositionType.Column;
    VerticalPositionAlignment := TdxFloatingObjectVerticalPositionAlignment.Top;
    VerticalPositionType := TdxFloatingObjectVerticalPositionType.Paragraph;
    FLocked.Reset;
    FOffsetX.Reset;
    FOffsetY.Reset;
    IsHorizontalAbsolutePosition := False;
    IsVerticalAbsolutePosition := False;

    FIsBehindDocument.Reset;
    TextWrapType := TdxFloatingObjectTextWrapType.Inline;
    FTextWrapSide.Reset;
    FTopDistance.Reset;
    FBottomDistance.Reset;
    FLeftDistance.Reset;
    FRightDistance.Reset;

    LockAspectRatio := InlinePictureRun.LockAspectRatio;
    ActualHeight := InlinePictureRun.ActualSize.Height;
    ActualWidth := InlinePictureRun.ActualSize.Width;
    FRotation.Reset;
  end
  else
  begin
    HorizontalPositionAlignment := FloatingObjectProperties.HorizontalPositionAlignment;
    VerticalPositionAlignment := FloatingObjectProperties.VerticalPositionAlignment;
    VerticalPositionType := FloatingObjectProperties.VerticalPositionType;
    HorizontalPositionType := FloatingObjectProperties.HorizontalPositionType;
    Locked := FloatingObjectProperties.Locked;
    OffsetX := FloatingObjectProperties.OffsetX;
    OffsetY := FloatingObjectProperties.OffsetY;
    IsHorizontalAbsolutePosition := (HorizontalPositionAlignment = TdxFloatingObjectHorizontalPositionAlignment.None);
    IsVerticalAbsolutePosition := (VerticalPositionAlignment = TdxFloatingObjectVerticalPositionAlignment.None);

    IsBehindDocument := FloatingObjectProperties.IsBehindDoc;
    TextWrapType := FloatingObjectProperties.TextWrapType;
    TextWrapSide := FloatingObjectProperties.TextWrapSide;
    TopDistance := FloatingObjectProperties.TopDistance;
    BottomDistance := FloatingObjectProperties.BottomDistance;
    LeftDistance := FloatingObjectProperties.LeftDistance;
    RightDistance := FloatingObjectProperties.RightDistance;

    LockAspectRatio := FloatingObjectProperties.LockAspectRatio;
    ActualHeight := FloatingObjectProperties.ActualHeight;
    ActualWidth := FloatingObjectProperties.ActualWidth;
    Rotation := FloatingInlineObjectParameters.FloatingObjectAnchorRun.Shape.Rotation;
  end;
end;

procedure TdxFloatingObjectLayoutOptionsFormController.ApplyChangesInlinePicture;
var
  ARun: TdxInlinePictureRun;
  ANewSize: TSize;
begin
  ARun := FFloatingInlineObjectParameters.InlinePictureRun;

  ANewSize := ARun.ActualSize;
  ANewSize.Height := ActualHeight;
  ANewSize.Width := ActualWidth;
  if not ARun.ActualSize.IsEqual(ANewSize) then
    ARun.ActualSize := ANewSize;

  if LockAspectRatio <> ARun.LockAspectRatio then
    ARun.LockAspectRatio := LockAspectRatio;
end;

procedure TdxFloatingObjectLayoutOptionsFormController.SetFloatingObjectProperties(
  const Value: TdxFloatingObjectProperties);
begin
  FControllerParameters.FloatingObjectProperties := Value;
end;

procedure TdxFloatingObjectLayoutOptionsFormController.SetPositionProperties(AFloatObjProp: TdxFloatingObjectProperties);
begin
  if HorizontalPositionType.HasValue and (AFloatObjProp.HorizontalPositionType <> HorizontalPositionType.Value) then
    AFloatObjProp.HorizontalPositionType := HorizontalPositionType.Value;

  if FIsHorizontalAbsolutePosition then
  begin
    if OffsetX.HasValue and (OffsetX <> AFloatObjProp.OffsetX) then
      AFloatObjProp.OffsetX := OffsetX.Value;
  end
  else
  begin
    if VerticalPositionAlignment.HasValue and (HorizontalPositionAlignment <> AFloatObjProp.HorizontalPositionAlignment) then
      AFloatObjProp.HorizontalPositionAlignment := HorizontalPositionAlignment.Value;
    AFloatObjProp.OffsetX := 0;
  end;

  if VerticalPositionType.HasValue and (AFloatObjProp.VerticalPositionType <> VerticalPositionType.Value) then
    AFloatObjProp.VerticalPositionType := VerticalPositionType.Value;

  if FIsVerticalAbsolutePosition then
  begin
    if OffsetY.HasValue and (OffsetY <> AFloatObjProp.OffsetY) then
      AFloatObjProp.OffsetY := OffsetY.Value;
  end
  else
  begin
    if VerticalPositionAlignment.HasValue and (VerticalPositionAlignment <> AFloatObjProp.VerticalPositionAlignment) then
      AFloatObjProp.VerticalPositionAlignment := VerticalPositionAlignment.Value;
    AFloatObjProp.OffsetY := 0;
  end;

  if Locked.HasValue and (Locked <> AFloatObjProp.Locked) then
    AFloatObjProp.Locked := Locked.Value;
end;

procedure TdxFloatingObjectLayoutOptionsFormController.SetWrappingOptions(AFloatObjProp: TdxFloatingObjectProperties);
begin
  if TextWrapType <> AFloatObjProp.TextWrapType then
    AFloatObjProp.TextWrapType := TextWrapType;

  if TextWrapSide.HasValue and (TextWrapSide <> AFloatObjProp.TextWrapSide) then
    AFloatObjProp.TextWrapSide := TextWrapSide.Value;

  if TopDistance.HasValue and (TopDistance <> AFloatObjProp.TopDistance) then
    AFloatObjProp.TopDistance := TopDistance.Value;

  if BottomDistance.HasValue and (BottomDistance <> AFloatObjProp.BottomDistance) then
    AFloatObjProp.BottomDistance := BottomDistance.Value;

  if RightDistance.HasValue and (RightDistance <> AFloatObjProp.RightDistance) then
    AFloatObjProp.RightDistance := RightDistance.Value;

  if LeftDistance.HasValue and (LeftDistance <> AFloatObjProp.LeftDistance) then
    AFloatObjProp.LeftDistance := LeftDistance.Value;

  if IsBehindDocument.HasValue and (IsBehindDocument <> AFloatObjProp.IsBehindDoc) then
    AFloatObjProp.IsBehindDoc := IsBehindDocument.Value;
end;

procedure TdxFloatingObjectLayoutOptionsFormController.SetSizeOptions(ARun: TdxFloatingObjectAnchorRun; AFloatObjProp: TdxFloatingObjectProperties);
begin
  if ActualHeight <> AFloatObjProp.ActualHeight then
    AFloatObjProp.ActualHeight := ActualHeight;

  if ActualWidth <> AFloatObjProp.ActualWidth then
    AFloatObjProp.ActualWidth := ActualWidth;

  if Rotation.HasValue and (Rotation <> ARun.Shape.Rotation) then
    ARun.Shape.Rotation := Rotation.Value;

  if LockAspectRatio <> AFloatObjProp.LockAspectRatio then
    AFloatObjProp.LockAspectRatio := LockAspectRatio;
end;

procedure TdxFloatingObjectLayoutOptionsFormController.ApplyChangesFloatingObject(ARun: TdxFloatingObjectAnchorRun);
var
  AFloatObjProp: TdxFloatingObjectProperties;
begin
  AFloatObjProp := ARun.FloatingObjectProperties;

  AFloatObjProp.BeginUpdate;
  try
    SetPositionProperties(AFloatObjProp);
    SetWrappingOptions(AFloatObjProp);
    SetSizeOptions(ARun, AFloatObjProp);
  finally
    AFloatObjProp.EndUpdate;
  end;
end;

function TdxFloatingObjectLayoutOptionsFormController. SubstituteInlineToFloatingObjectRun: TdxFloatingObjectAnchorRun;
var
  APieceTable: TdxPieceTable;
  ALogPosition: TdxDocumentLogPosition;
  AParagraphIndex: TdxParagraphIndex;
  ANewRun: TdxFloatingObjectAnchorRun;
  ANewContent: TdxPictureFloatingObjectContent;
begin
  APieceTable := TdxPieceTable(FloatingInlineObjectParameters.InlinePictureRun.PieceTable);
  ALogPosition := DocumentModel.Selection.Interval.NormalizedStart.LogPosition;
  AParagraphIndex := FloatingInlineObjectParameters.InlinePictureRun.Paragraph.Index;

  ANewRun := APieceTable.InsertFloatingObjectAnchorCore(AParagraphIndex, ALogPosition);
  ANewContent := TdxPictureFloatingObjectContent.Create(ANewRun, FloatingInlineObjectParameters.InlinePictureRun.Image);
  ANewRun.SetContent(ANewContent);
  APieceTable.DeleteContent(ALogPosition + 1, 1, False);

  Result := ANewRun;
end;

procedure TdxFloatingObjectLayoutOptionsFormController.ApplyChanges;
var
  ARun: TdxFloatingObjectAnchorRun;
begin
  DocumentModel.BeginUpdate;
  if (TextWrapType = TdxFloatingObjectTextWrapType.Inline) and ((not Rotation.HasValue) or ((Rotation = 0))) then
    ApplyChangesInlinePicture
  else
  begin
    if FFloatingInlineObjectParameters.FloatingObjectAnchorRun = nil then
    begin
      ARun :=  SubstituteInlineToFloatingObjectRun;
      FFloatingInlineObjectParameters.FloatingObjectAnchorRun := ARun;
      ApplyChangesFloatingObject(ARun);
    end
    else
    begin
      ApplyChangesFloatingObject(FloatingInlineObjectParameters.FloatingObjectAnchorRun);
    end;
  end;
  DocumentModel.EndUpdate;
end;

procedure TdxFloatingObjectLayoutOptionsFormController.ApplyPreset(const APreset: TdxTextWrapTypeInfoPreset);
begin
  TextWrapType := APreset.TextWrapType;
  IsBehindDocument := APreset.IsBehindDocument;
end;

procedure TdxFloatingObjectLayoutOptionsFormController.ResetActualHeight;
begin
  if OriginalSize.IsNull then
    ActualHeight := 0
  else
    ActualHeight := OriginalSize.Value.Height;
end;

procedure TdxFloatingObjectLayoutOptionsFormController.ResetActualWidth;
begin
  if OriginalSize.IsNull then
    ActualWidth := 0
  else
    ActualWidth := OriginalSize.Value.Width;
end;

procedure TdxFloatingObjectLayoutOptionsFormController.ResetRotation;
begin
  Rotation := TdxNullableInteger.IfThen(Rotation.HasValue, 0);
end;

procedure TdxFloatingObjectLayoutOptionsFormController.RecalculateSizeDependingOnHeight(ALockAspectRatio: Boolean; ANewHeight: Integer);
begin
  ActualHeight := ANewHeight;
  if ALockAspectRatio then
  begin
    if OriginalSize.HasValue and (OriginalSize.Value.Height <> 0) then
      ActualWidth := MulDiv(OriginalSize.Value.Width, ActualHeight, OriginalSize.Value.Height)
    else
      ActualWidth := 0;
  end;
end;

procedure TdxFloatingObjectLayoutOptionsFormController.RecalculateSizeDependingOnWidth(ALockAspectRatio: Boolean; ANewWidth: Integer);
begin
  ActualWidth := ANewWidth;
  if ALockAspectRatio then
  begin
    if OriginalSize.HasValue and (OriginalSize.Value.Width <> 0) then
      ActualHeight := MulDiv(OriginalSize.Value.Height, ActualWidth, OriginalSize.Value.Width)
    else
      ActualHeight := 0;
  end;
end;

function TdxFloatingObjectLayoutOptionsFormController.StringToTwips(const S: string): Integer;
var
  AUnit: TdxUIUnit;
begin
  if S = '' then
    Exit(0);
  AUnit := TdxUIUnit.Create(S, UIType, TdxUnitPrecisions.DefaultPrecisions, False);
  Result := FUIUnitConverter.ToTwipsUnit(AUnit, False);
end;

function TdxFloatingObjectLayoutOptionsFormController.OriginalHeightAsString: string;
var
  AHeight: Integer;
  AUnit: TdxUIUnit;
begin
  if OriginalSize.IsNull then
    AHeight := 0
  else
    AHeight := OriginalSize.Value.Height;
  AUnit := FUIUnitConverter.ToUIUnitF(AHeight, UIType, False);
  Result := AUnit.ToString;
end;

function TdxFloatingObjectLayoutOptionsFormController.OriginalWidthAsString: string;
var
  AWidth: Integer;
  AUnit: TdxUIUnit;
begin
  if OriginalSize.IsNull then
    AWidth := 0
  else
    AWidth := OriginalSize.Value.Width;
  AUnit := FUIUnitConverter.ToUIUnitF(AWidth, UIType, False);
  Result := AUnit.ToString;
end;

end.
