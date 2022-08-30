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

unit dxRichEdit.DocumentModel.InlineObjectRange;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils,
  dxCore, dxCoreClasses, dxCoreGraphics, cxGeometry,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.ChunkedStringBuilder,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.CopyManager.Core,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.InlineObjectFormatting;

type
  TdxPictureFloatingObjectContent = class;

  { IdxPictureContainerRun }

  IdxPictureContainerRun = interface
  ['{5ED7BD56-E585-4226-927E-96A29978C435}']
    function GetRun: TdxTextRunBase;
    function GetActualSize: TSize;
    function GetActualSizeF: TdxSizeF;
    function GetLockAspectRatio: Boolean;
    function GetScaleX: Single;
    function GetScaleY: Single;
    function GetDocumentModel: TdxCustomDocumentModel;
    function GetPictureContent: TdxPictureFloatingObjectContent;
    procedure SetActualSize(const AValue: TSize);
    procedure SetLockAspectRatio(const Value: Boolean);

    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
    procedure SetActualSizeInternal(const AActualSize: TSize);

    property Run: TdxTextRunBase read GetRun;
    property ActualSize: TSize read GetActualSize write SetActualSize;
    property ActualSizeF: TdxSizeF read GetActualSizeF;
    property ScaleX: Single read GetScaleX;
    property ScaleY: Single read GetScaleY;
    property DocumentModel: TdxCustomDocumentModel read GetDocumentModel;
    property LockAspectRatio: Boolean read GetLockAspectRatio write SetLockAspectRatio;
    property PictureContent: TdxPictureFloatingObjectContent read GetPictureContent;
  end;

  { IdxInlineObjectRun }

  IdxInlineObjectRun = interface
  ['{289BD48B-B271-431E-BED5-FE0FBBC3513E}']
    function MeasureRun(const AMeasurer: IdxObjectMeasurer): TSize;
    function CreateBox: TdxBoxBase;
    function GetBoxClassType: TdxBoxClass;
  end;

  { TdxImageScaleCalculator }

  TdxImageScaleCalculator = class
  public
    class function GetScale(ADesired, AOriginalInModelUnits: Integer; const ADefaultScale: Single): Single; overload; static;
    class function GetScale(const ADesiredSize, AOriginalSize: TSize; const ADefaultScale: Single): TdxSizeF; overload; static;
    class function GetDesiredImageSizeInModelUnits(ADesiredSizeInPixels: TSize; AUnitConverter: TdxDocumentModelUnitConverter): TSize; overload; static;
    class function GetDesiredImageSizeInModelUnits(AWidthInPixels, AHeightInPixels: Integer): TSize; overload; static;
  end;

  { TdxInlineObjectRun }

  TdxInlineObjectRun = class abstract(TdxTextRunBase,
    IdxRectangularObject,
    IdxRectangularScalableObject)
  protected
    procedure CopyOriginalSize(const AOriginalSize: TSize); virtual; abstract;

    function GetOriginalSize: TSize; virtual; abstract;
    function GetActualSizeF: TdxSizeF; virtual; abstract;
    function GetActualSize: TSize; virtual; abstract;
    function GetScaleX: Single; virtual; abstract;
    function GetScaleY: Single; virtual; abstract;
    procedure SetOriginalSize(const AValue: TSize); virtual; abstract;
    procedure SetActualSize(const AValue: TSize); virtual; abstract;
    procedure SetScaleX(const AValue: Single); virtual; abstract;
    procedure SetScaleY(const AValue: Single); virtual; abstract;
  public
    constructor Create(AParagraph: TdxSimpleParagraph; const AOriginalSize: TSize); reintroduce; virtual;

    function GetPlainText(AGrowBuffer: TdxChunkedStringBuilder): string; override;
    function GetPlainText(AGrowBuffer: TdxChunkedStringBuilder; AFrom, ATo: Integer): string; override;
    function CanJoinWith(ANextRun: TdxTextRunBase): Boolean; override;
    procedure Measure(ABoxInfo: TdxBoxInfo; const AMeasurer: IdxObjectMeasurer); override;
    function TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean; override;

    property OriginalSize: TSize read GetOriginalSize;
    property ActualSizeF: TdxSizeF read GetActualSizeF;
    property ActualSize: TSize read GetActualSize write SetActualSize;
    property ScaleX: Single read GetScaleX write SetScaleX;
    property ScaleY: Single read GetScaleY write SetScaleY;
  end;

  { TdxInlineObjectRunBase<T: TdxInlineObjectInfo> }

  TdxInlineObjectRunBase<T: TdxInlineObjectInfo> = class abstract(TdxInlineObjectRun)
  strict private
    FProperties: TdxInlineObjectProperties<T>;
  protected
    function CreateProperties(APieceTable: TdxCustomPieceTable): TdxInlineObjectProperties<T>; virtual; abstract;
    procedure InternalSetPropertiesScaleX(const Value: Single); virtual; abstract;
    procedure InternalSetPropertiesScaleY(const Value: Single); virtual; abstract;

    function GetActualSizeF: TdxSizeF; override;
    function GetActualSize: TSize; override;
    procedure SetScaleX(const Value: Single); override;
    procedure SetScaleY(const Value: Single); override;
    procedure SetActualSize(const Value: TSize); override;

    property Properties: TdxInlineObjectProperties<T> read FProperties;
  public
    constructor Create(AParagraph: TdxSimpleParagraph; const AOriginalSize: TSize); override;
    destructor Destroy; override;
    procedure SetActualSizeInternal(const AActualSize: TSize);
  end;

  { TdxInlineCustomObjectRun }

  TdxInlineCustomObjectRun = class(TdxInlineObjectRunBase<TdxInlineCustomObjectInfo>)
  strict private
    FOriginalSize: TSize;
    FCustomObject: IdxInlineCustomObject;
    function GetCustomObjectProperties: TdxInlineCustomObjectProperties;
    function GetProperties: TdxInlineCustomObjectProperties;
  protected
    function GetOriginalSize: TSize; override;

    function GetScaleX: Single; override;
    function GetScaleY: Single; override;
    procedure InternalSetPropertiesScaleX(const Value: Single); override;
    procedure InternalSetPropertiesScaleY(const Value: Single); override;
    procedure SetOriginalSize(const ASize: TSize); override;
    procedure CopyOriginalSize(const AOriginalSize: TSize); override;
    function CreateProperties(APieceTable: TdxCustomPieceTable): TdxInlineObjectProperties<TdxInlineCustomObjectInfo>; override;
    procedure SubscribeCustomObjectEvents; virtual;
    procedure SubscribePropertiesEvents; virtual;
    property CustomObjectProperties: TdxInlineCustomObjectProperties read GetCustomObjectProperties;
    property Properties: TdxInlineCustomObjectProperties read GetProperties;
  public
    constructor Create(AParagraph: TdxSimpleParagraph; const ACustomObject: IdxInlineCustomObject); reintroduce;

    function CanPlaceCaretBefore: Boolean; override;
    function Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase; override;
    property CustomObject: IdxInlineCustomObject read FCustomObject;
  end;

  { TdxInlinePictureRun }

  TdxInlinePictureRun = class(TdxInlineObjectRunBase<TdxInlinePictureInfo>,
    IdxInlinePicturePropertiesContainer,
    IdxPictureContainerRun,
    IdxInlineObjectRun,
    IdxHighlightableTextRun)
  strict private
    FImageContent: TdxPictureFloatingObjectContent;
    function GetProperties: TdxInlinePictureProperties;
    function GetImage: TdxOfficeImageReference;
    function GetPictureProperties: TdxInlinePictureProperties;
    function GetSizing: TdxImageSizeMode;
    function GetResizingShadowDisplayMode: TdxResizingShadowDisplayMode;
    function GetLockAspectRatio: Boolean;
    function IdxPictureContainerRun.GetDocumentModel = PictureContainerRunGetDocumentModel;
    function PictureContainerRunGetDocumentModel: TdxCustomDocumentModel;
    procedure SetSizing(const Value: TdxImageSizeMode);
    procedure SetResizingShadowDisplayMode(const Value: TdxResizingShadowDisplayMode);
    procedure SetLockAspectRatio(const Value: Boolean);
  protected
    function GetScaleX: Single; override;
    function GetScaleY: Single; override;
    procedure InternalSetPropertiesScaleX(const Value: Single); override;
    procedure InternalSetPropertiesScaleY(const Value: Single); override;

    function CreatePictureObjectContent(AImage: TdxOfficeImageReference): TdxPictureFloatingObjectContent; virtual;

    function GetOriginalSize: TSize; override;
    function GetActualSize: TSize; override;
    procedure SetActualSize(const Value: TSize); override;
    procedure SetScaleX(const Value: Single); override;
    procedure SetScaleY(const Value: Single); override;

    procedure SetOriginalSize(const ASize: TSize); override;
    procedure CopyOriginalSize(const AOriginalSize: TSize); override;
    function CreateProperties(APieceTable: TdxCustomPieceTable): TdxInlineObjectProperties<TdxInlinePictureInfo>; override;
    procedure InsertInlinePicture(ATargetPieceTable: TdxSimplePieceTable; ALogPosition: TdxDocumentLogPosition); virtual;

    function GetPieceTable: TdxCustomPieceTable;
    function CreateInlinePicturePropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnInlineCharacterPropertiesChanged;
  public
    constructor Create(AParagraph: TdxSimpleParagraph; AImage: TdxOfficeImageReference); reintroduce;
    destructor Destroy; override;

    class function ShouldExportInlinePictureRunCharacterProperties(AInfo: TdxCharacterFormattingInfo; const AOptions: TdxCharacterFormattingOptions): Boolean; static;
    function CanPlaceCaretBefore: Boolean; override;
    function Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase; override;
    procedure Export(const AExporter: IdxSimpleDocumentModelExporter); override;
    function TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean; override;

    function GetRun: TdxTextRunBase;
    function IdxPictureContainerRun.GetActualSize = GetActualSize;
    procedure IdxPictureContainerRun.SetActualSize = SetActualSize;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
    function GetPictureContent: TdxPictureFloatingObjectContent;
    function MeasureRun(const AMeasurer: IdxObjectMeasurer): TSize;
    function CreateBox: TdxBoxBase;
    function GetBoxClassType: TdxBoxClass; virtual;

    procedure AfterRunInserted; override;
    procedure BeforeRunRemoved; override;
    procedure Measure(ABoxInfo: TdxBoxInfo; const AMeasurer: IdxObjectMeasurer); override;

    property ActualSize: TSize read GetActualSize write SetActualSize;
    property Image: TdxOfficeImageReference read GetImage;
    property LockAspectRatio: Boolean read GetLockAspectRatio write SetLockAspectRatio;
    property Properties: TdxInlinePictureProperties read GetProperties;
    property PictureProperties: TdxInlinePictureProperties read GetPictureProperties;
    property ResizingShadowDisplayMode: TdxResizingShadowDisplayMode read GetResizingShadowDisplayMode write SetResizingShadowDisplayMode;
    property Sizing: TdxImageSizeMode read GetSizing write SetSizing;
  end;

  { TdxCustomObjectContent }

  TdxCustomObjectContent = class abstract(TcxIUnknownObject)
  strict private
    FRun: TdxTextRunBase;

    function GetParagraph: TdxSimpleParagraph;
    function GetDocumentModel: TdxSimpleDocumentModel;
    function GetRunPieceTable: TdxSimplePieceTable;
  protected
    function GetOriginalSize: TSize; virtual; abstract;
    procedure SetOriginalSize(const AValue: TSize); virtual; abstract;
  public
    constructor Create(ARun: TdxTextRunBase);

    function Clone(const ARun: TdxTextRunBase;
      ACopyManager: TdxCustomDocumentModelCopyManager): TdxCustomObjectContent; virtual; abstract;
    procedure Dispose; virtual;

    property Run: TdxTextRunBase read FRun;
    property Paragraph: TdxSimpleParagraph read GetParagraph;
    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
    property OriginalSize: TSize read GetOriginalSize;
    property RunPieceTable: TdxSimplePieceTable read GetRunPieceTable;
  end;

  { TdxPictureFloatingObjectContent }

  TdxPictureFloatingObjectContent = class(TdxCustomObjectContent)
  strict private
    FImage: TdxOfficeImageReference;
    FInvalidActualSize: Boolean;
    FOriginalSize: TSize;
    FPictureContainerRun: IdxPictureContainerRun;
    FIsScreenDpiUsed: Boolean;
    function ApplyScales(const AImageSize: TSize): TSize;
    function CalculateActualSize(const ADesiredSize: TSize): TSize;
    function GetImageSizeWithActualDpi: TSize;
    procedure SetActualSize(const ADesiredSize: TSize);
  protected
    procedure AfterRunInserted; virtual;
    procedure BeforeRunRemoved; virtual;
    procedure HandleImageChanged(E: TdxNativeImageChangedEventArgs); virtual;
    function GetImageSizeWithDocumentModelDpi: TSize;
    function GetOriginalSize: TSize; override;
    procedure OnImageChanged(Sender: TObject; E: TdxNativeImageChangedEventArgs); virtual;
    procedure OnImageChanging(Sender: TObject; E: TdxEventArgs); virtual;
    procedure SubscribePicturePropertiesEvents; virtual;
    procedure UnsubscribePicturePropertiesEvents; virtual;
  public
    constructor Create(const ARun: IdxPictureContainerRun; AImage: TdxOfficeImageReference); reintroduce;
    destructor Destroy; override;

    function Clone(const ARun: TdxTextRunBase;
      ACopyManager: TdxCustomDocumentModelCopyManager): TdxCustomObjectContent; override;
    procedure EnsureActualSize(AUseScreenDpi: Boolean);
    procedure SetOriginalSize(const AValue: TSize); override;

    property Image: TdxOfficeImageReference read FImage;
    property InvalidActualSize: Boolean read FInvalidActualSize write FInvalidActualSize;
  end;

implementation

uses
  Math,
  dxTypeHelpers,
  dxMeasurementUnits,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs,
  dxRichEdit.Options.Simple,
  dxRichEdit.DocumentModel.History.Simple;

type
  TdxInlinePicturePropertiesAccess = class(TdxInlinePictureProperties);

{ TdxImageScaleCalculator }

class function TdxImageScaleCalculator.GetScale(ADesired, AOriginalInModelUnits: Integer;
  const ADefaultScale: Single): Single;
begin
  Result := ADefaultScale;
  if ADesired > 0 then
    Result := Math.Max(1, 100 * ADesired / Math.Max(1, AOriginalInModelUnits));
  Result := Math.Max(1, Result);
end;

class function TdxImageScaleCalculator.GetDesiredImageSizeInModelUnits(ADesiredSizeInPixels: TSize;
  AUnitConverter: TdxDocumentModelUnitConverter): TSize;
begin
  Result := AUnitConverter.PixelsToModelUnits(ADesiredSizeInPixels, TdxCustomDocumentModel.DpiX, TdxCustomDocumentModel.DpiY);
end;

class function TdxImageScaleCalculator.GetDesiredImageSizeInModelUnits(AWidthInPixels, AHeightInPixels: Integer): TSize;
begin
  Result := PixelsToTwips(TSize.Create(AWidthInPixels, AHeightInPixels), TdxCustomDocumentModel.DpiX, TdxCustomDocumentModel.DpiY);
end;

class function TdxImageScaleCalculator.GetScale(const ADesiredSize, AOriginalSize: TSize;
  const ADefaultScale: Single): TdxSizeF;
begin
  Result := dxSizeF(GetScale(ADesiredSize.Width, AOriginalSize.Width, ADefaultScale),
    GetScale(ADesiredSize.Height, AOriginalSize.Height, ADefaultScale));
end;

{ TdxInlineObjectRun }

function TdxInlineObjectRun.CanJoinWith(ANextRun: TdxTextRunBase): Boolean;
begin
  Assert(ANextRun <> nil);
  Result := False;
end;

constructor TdxInlineObjectRun.Create(AParagraph: TdxSimpleParagraph; const AOriginalSize: TSize);
begin
  inherited Create(AParagraph);
  SetOriginalSize(AOriginalSize);
end;

function TdxInlineObjectRun.GetPlainText(AGrowBuffer: TdxChunkedStringBuilder; AFrom, ATo: Integer): string;
begin
  Result := GetPlainText(AGrowBuffer);
end;

function TdxInlineObjectRun.GetPlainText(AGrowBuffer: TdxChunkedStringBuilder): string;
begin
  Result := EmptyStr;
end;

procedure TdxInlineObjectRun.Measure(ABoxInfo: TdxBoxInfo; const AMeasurer: IdxObjectMeasurer);
begin
  ABoxInfo.Size := OriginalSize;
end;

function TdxInlineObjectRun.TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean;
begin
  Result := False;
end;

{ TdxInlineObjectRunBase<T> }

constructor TdxInlineObjectRunBase<T>.Create(AParagraph: TdxSimpleParagraph; const AOriginalSize: TSize);
begin
  inherited Create(AParagraph, AOriginalSize);
  FProperties := CreateProperties(AParagraph.PieceTable);
end;

destructor TdxInlineObjectRunBase<T>.Destroy;
begin
  FreeAndNil(FProperties);
  inherited Destroy;
end;

function TdxInlineObjectRunBase<T>.GetActualSize: TSize;
begin
  Result := cxSize(Round(OriginalSize.Width * ScaleX / 100),
    Round(OriginalSize.Height * ScaleY / 100));
end;

function TdxInlineObjectRunBase<T>.GetActualSizeF: TdxSizeF;
begin
  Result := dxSizeF(OriginalSize.Width * ScaleX / 100,
    OriginalSize.Height * ScaleY / 100);
end;

procedure TdxInlineObjectRunBase<T>.SetActualSize(const Value: TSize);
begin
  SetActualSizeInternal(Value);
end;

procedure TdxInlineObjectRunBase<T>.SetActualSizeInternal(const AActualSize: TSize);
var
  ADocumentModel: TdxSimpleDocumentModel;
begin
  ADocumentModel := Paragraph.DocumentModel;
  ADocumentModel.BeginUpdate;
  try
    ScaleX := TdxImageScaleCalculator.GetScale(AActualSize.Width, OriginalSize.Width, 100);
    ScaleY := TdxImageScaleCalculator.GetScale(AActualSize.Height, OriginalSize.Height, 100);
  finally
    ADocumentModel.EndUpdate;
  end;
end;

procedure TdxInlineObjectRunBase<T>.SetScaleX(const Value: Single);
begin
  if ScaleX = Value then
    Exit;
  InternalSetPropertiesScaleX(Value);
end;

procedure TdxInlineObjectRunBase<T>.SetScaleY(const Value: Single);
begin
  if ScaleY = Value then
    Exit;
  InternalSetPropertiesScaleY(Value);
end;

{ TdxInlineCustomObjectRun }

constructor TdxInlineCustomObjectRun.Create(AParagraph: TdxSimpleParagraph; const ACustomObject: IdxInlineCustomObject);
begin
  inherited Create(AParagraph, cxSize(1, 1));
  Assert(ACustomObject <> nil);
  FCustomObject := ACustomObject;

  FOriginalSize := ACustomObject.CalculateOriginalSize(AParagraph.DocumentModel.UnitConverter);
  SubscribeCustomObjectEvents;
  SubscribePropertiesEvents;
end;

function TdxInlineCustomObjectRun.CanPlaceCaretBefore: Boolean;
begin
  Result := True;
end;

function TdxInlineCustomObjectRun.Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase;
var
  ATargetPieceTable: TdxSimplePieceTable;
  ATargetPosition: TdxDocumentModelPosition;
  ARun: TdxInlineCustomObjectRun;
begin
  ATargetPieceTable := TdxSimplePieceTable(ACopyManager.TargetPieceTable);
  ATargetPosition := ACopyManager.TargetPosition;

  Assert(DocumentModel = ACopyManager.SourceModel);
  Assert(ATargetPosition.PieceTable = ATargetPieceTable);
  Assert(ATargetPosition.RunOffset = 0);

  ATargetPieceTable.InsertInlineCustomObject(ATargetPosition.LogPosition, CustomObject.Clone);
  ARun := TdxInlineCustomObjectRun(ATargetPieceTable.Runs[ATargetPosition.RunIndex]);
  ARun.CustomObjectProperties.CopyFrom(CustomObjectProperties.Info);
  ARun.CopyOriginalSize(OriginalSize);
  Result := ARun;
end;

procedure TdxInlineCustomObjectRun.CopyOriginalSize(const AOriginalSize: TSize);
begin
  FOriginalSize := AOriginalSize;
end;

function TdxInlineCustomObjectRun.CreateProperties(
  APieceTable: TdxCustomPieceTable): TdxInlineObjectProperties<TdxInlineCustomObjectInfo>;
begin
  Result := TdxInlineCustomObjectProperties.Create(APieceTable);
end;

function TdxInlineCustomObjectRun.GetOriginalSize: TSize;
begin
  Result := FOriginalSize;
end;

function TdxInlineCustomObjectRun.GetScaleX: Single;
begin
  Result := Properties.ScaleX;
end;

function TdxInlineCustomObjectRun.GetScaleY: Single;
begin
  Result := Properties.ScaleY;
end;

procedure TdxInlineCustomObjectRun.InternalSetPropertiesScaleX(const Value: Single);
begin
  Properties.ScaleX := Value;
end;

procedure TdxInlineCustomObjectRun.InternalSetPropertiesScaleY(const Value: Single);
begin
  Properties.ScaleY := Value;
end;

procedure TdxInlineCustomObjectRun.SetOriginalSize(const ASize: TSize);
begin
  FOriginalSize := ASize;
end;

procedure TdxInlineCustomObjectRun.SubscribeCustomObjectEvents;
begin
  NotImplemented;
end;

procedure TdxInlineCustomObjectRun.SubscribePropertiesEvents;
begin
  NotImplemented;
end;

function TdxInlineCustomObjectRun.GetCustomObjectProperties: TdxInlineCustomObjectProperties;
begin
  Result := TdxInlineCustomObjectProperties(Properties);
end;

function TdxInlineCustomObjectRun.GetProperties: TdxInlineCustomObjectProperties;
begin
  Result := TdxInlineCustomObjectProperties(inherited Properties);
end;

{ TdxInlinePictureRun }

constructor TdxInlinePictureRun.Create(AParagraph: TdxSimpleParagraph; AImage: TdxOfficeImageReference);
begin
  inherited Create(AParagraph, TSize.Create(1, 1));
  FImageContent := CreatePictureObjectContent(AImage);
  PictureProperties.OnObtainAffectedRange.Add(OnCharacterPropertiesObtainAffectedRange);
end;

destructor TdxInlinePictureRun.Destroy;
begin
  FreeAndNil(FImageContent);
  inherited Destroy;
end;

procedure TdxInlinePictureRun.CopyOriginalSize(const AOriginalSize: TSize);
begin
  FImageContent.SetOriginalSize(AOriginalSize);
end;

function TdxInlinePictureRun.CreateBox: TdxBoxBase;
begin
  Result := GetBoxClassType.Create;
end;

function TdxInlinePictureRun.GetBoxClassType: TdxBoxClass;
begin
  Result := TdxInlinePictureBox;
end;

procedure TdxInlinePictureRun.AfterRunInserted;
begin
  inherited AfterRunInserted;
  FImageContent.AfterRunInserted;
end;

procedure TdxInlinePictureRun.BeforeRunRemoved;
begin
  inherited BeforeRunRemoved;
  FImageContent.BeforeRunRemoved;
end;

procedure TdxInlinePictureRun.Measure(ABoxInfo: TdxBoxInfo; const AMeasurer: IdxObjectMeasurer);
begin
  ABoxInfo.Size := AMeasurer.MeasureRectangularObject(Self);
end;

function TdxInlinePictureRun.GetPieceTable: TdxCustomPieceTable;
begin
  Result := PieceTable;
end;

function TdxInlinePictureRun.CreateInlinePicturePropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxRunInlinePicturePropertiesChangedHistoryItem.Create(Paragraph.PieceTable, GetRunIndex);
end;

function TdxInlinePictureRun.CreateProperties(
  APieceTable: TdxCustomPieceTable): TdxInlineObjectProperties<TdxInlinePictureInfo>;
begin
  Result := TdxInlinePictureProperties.Create(Self);
end;

procedure TdxInlinePictureRun.Export(const AExporter: IdxSimpleDocumentModelExporter);
begin
  Image.EnsureLoadComplete;
  inherited Export(AExporter);
end;

function TdxInlinePictureRun.GetActualSize: TSize;
begin
  Result := inherited GetActualSize;
end;

function TdxInlinePictureRun.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxInlinePicturePropertiesAccess(PictureProperties).BatchUpdateChangeActions;
end;

function TdxInlinePictureRun.GetImage: TdxOfficeImageReference;
begin
  Result := FImageContent.Image;
end;

function TdxInlinePictureRun.GetLockAspectRatio: Boolean;
begin
  Result := Properties.LockAspectRatio;
end;

function TdxInlinePictureRun.PictureContainerRunGetDocumentModel: TdxCustomDocumentModel;
begin
  Result := inherited DocumentModel;
end;

function TdxInlinePictureRun.CanPlaceCaretBefore: Boolean;
begin
  Result := True;
end;

function TdxInlinePictureRun.Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase;
var
  ATargetPieceTable: TdxSimplePieceTable;
  ATargetPosition: TdxDocumentModelPosition;
  ATargetModel: TdxSimpleDocumentModel;
  ARun: TdxInlinePictureRun;
  AOptions: TdxSimpleDocumentCapabilitiesOptions;
begin
  ATargetPieceTable := TdxSimplePieceTable(ACopyManager.TargetPieceTable);
  ATargetPosition := ACopyManager.TargetPosition;

  Assert(DocumentModel = ACopyManager.SourceModel);
  Assert(ATargetPosition.PieceTable = ATargetPieceTable);
  Assert(ATargetPosition.RunOffset = 0);
  ATargetModel := TdxSimpleDocumentModel(ACopyManager.TargetModel);
  if not ATargetModel.DocumentCapabilities.InlinePicturesAllowed then
  begin
    ATargetPieceTable.InsertText(ATargetPosition.LogPosition, ' ');
    Result := ATargetPieceTable.Runs[ATargetPosition.RunIndex];
    Exit;
  end;
  InsertInlinePicture(ATargetPieceTable, ATargetPosition.LogPosition);
  ARun := TdxInlinePictureRun(ATargetPieceTable.Runs[ATargetPosition.RunIndex]);
  ARun.PictureProperties.CopyFrom(PictureProperties.Info);
  AOptions := ATargetModel.DocumentCapabilities;
  if AOptions.CharacterFormattingAllowed then
    ARun.CharacterProperties.CopyFrom(CharacterProperties.Info);
  if AOptions.CharacterStyleAllowed then
    ARun.CharacterStyleIndex := CharacterStyle.Copy(ATargetModel);
  ARun.CopyOriginalSize(OriginalSize);
  Result := ARun;
end;

function TdxInlinePictureRun.GetScaleX: Single;
begin
  Result := Properties.ScaleX;
end;

function TdxInlinePictureRun.GetScaleY: Single;
begin
  Result := Properties.ScaleY;
end;

procedure TdxInlinePictureRun.InternalSetPropertiesScaleX(const Value: Single);
begin
  Properties.ScaleX := Value;
end;

procedure TdxInlinePictureRun.InternalSetPropertiesScaleY(const Value: Single);
begin
  Properties.ScaleY := Value;
end;

function TdxInlinePictureRun.CreatePictureObjectContent(AImage: TdxOfficeImageReference): TdxPictureFloatingObjectContent;
begin
  Result := TdxPictureFloatingObjectContent.Create(Self, AImage);
end;

function TdxInlinePictureRun.GetOriginalSize: TSize;
begin
  Result := FImageContent.OriginalSize;
end;

function TdxInlinePictureRun.GetPictureContent: TdxPictureFloatingObjectContent;
begin
  Result := FImageContent;
end;

function TdxInlinePictureRun.GetPictureProperties: TdxInlinePictureProperties;
begin
  Result := TdxInlinePictureProperties(Properties);
end;

function TdxInlinePictureRun.GetProperties: TdxInlinePictureProperties;
begin
  Result := TdxInlinePictureProperties(inherited Properties);
end;

function TdxInlinePictureRun.GetResizingShadowDisplayMode: TdxResizingShadowDisplayMode;
begin
  Result := Properties.ResizingShadowDisplayMode;
end;

function TdxInlinePictureRun.GetRun: TdxTextRunBase;
begin
  Result := Self;
end;

function TdxInlinePictureRun.GetSizing: TdxImageSizeMode;
begin
  Result := Properties.Sizing;
end;

procedure TdxInlinePictureRun.InsertInlinePicture(ATargetPieceTable: TdxSimplePieceTable;
  ALogPosition: TdxDocumentLogPosition);
begin
  ATargetPieceTable.InsertInlinePicture(ALogPosition, Image);
end;

function TdxInlinePictureRun.TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean;
begin
  Result := False;
end;

function TdxInlinePictureRun.MeasureRun(const AMeasurer: IdxObjectMeasurer): TSize;
begin
  Result := AMeasurer.MeasureRectangularObject(Self);
end;

procedure TdxInlinePictureRun.OnInlineCharacterPropertiesChanged;
begin
end;

procedure TdxInlinePictureRun.SetActualSize(const Value: TSize);
begin
  inherited SetActualSize(Value);
  if not Image.IsLoaded then
  begin
    FImageContent.Image.DesiredSizeAfterLoad := Value;
    FImageContent.Image.ShouldSetDesiredSizeAfterLoad := True;
  end;
end;

procedure TdxInlinePictureRun.SetLockAspectRatio(const Value: Boolean);
begin
  if LockAspectRatio = Value then
    Exit;
  Properties.LockAspectRatio := Value;
end;

procedure TdxInlinePictureRun.SetOriginalSize(const ASize: TSize);
begin
  if FImageContent <> nil then
    FImageContent.SetOriginalSize(ASize);
end;

procedure TdxInlinePictureRun.SetResizingShadowDisplayMode(const Value: TdxResizingShadowDisplayMode);
begin
  if ResizingShadowDisplayMode = Value then
    Exit;
  Properties.ResizingShadowDisplayMode := Value;
end;

procedure TdxInlinePictureRun.SetScaleX(const Value: Single);
begin
  inherited SetScaleX(Value);
  if not Image.IsLoaded then
    FImageContent.Image.ShouldSetDesiredSizeAfterLoad := False;
end;

procedure TdxInlinePictureRun.SetScaleY(const Value: Single);
begin
  inherited SetScaleY(Value);
  if not Image.IsLoaded then
    FImageContent.Image.ShouldSetDesiredSizeAfterLoad := False;
end;

procedure TdxInlinePictureRun.SetSizing(const Value: TdxImageSizeMode);
begin
  if Sizing = Value then
    Exit;
  Properties.Sizing := Value;
end;

class function TdxInlinePictureRun.ShouldExportInlinePictureRunCharacterProperties(AInfo: TdxCharacterFormattingInfo;
  const AOptions: TdxCharacterFormattingOptions): Boolean;
begin
  Result := (AOptions.UseHidden and AInfo.Hidden) or
    (AOptions.UseFontUnderlineType and (AInfo.FontUnderlineType <> TdxUnderlineType.None)) or
    (AOptions.UseFontStrikeoutType and (AInfo.FontStrikeoutType <> TdxStrikeoutType.None)) or
    (AOptions.UseBackColor and not TdxAlphaColors.IsTransparentOrEmpty(AInfo.BackColor));
end;

{ TdxCustomObjectContent }

constructor TdxCustomObjectContent.Create(ARun: TdxTextRunBase);
begin
  inherited Create;
  FRun := ARun;
end;

procedure TdxCustomObjectContent.Dispose;
begin
  Free;
end;

function TdxCustomObjectContent.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := FRun.DocumentModel;
end;

function TdxCustomObjectContent.GetParagraph: TdxSimpleParagraph;
begin
  Result := FRun.Paragraph;
end;

function TdxCustomObjectContent.GetRunPieceTable: TdxSimplePieceTable;
begin
  Result := FRun.PieceTable;
end;

{ TdxPictureFloatingObjectContent }

constructor TdxPictureFloatingObjectContent.Create(const ARun: IdxPictureContainerRun;
  AImage: TdxOfficeImageReference);
var
  AImageSizeInModelUnits: TSize;
begin
  inherited Create(ARun.Run);
  Assert(AImage <> nil);
  FImage := AImage.Clone(DocumentModel.ImageCache);
  FPictureContainerRun := ARun;
  AImageSizeInModelUnits := AImage.CalculateImageSizeInModelUnits(Paragraph.DocumentModel.UnitConverter);

  SetOriginalSize(AImageSizeInModelUnits);
  SubscribePicturePropertiesEvents;
end;

destructor TdxPictureFloatingObjectContent.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TdxPictureFloatingObjectContent.EnsureActualSize(AUseScreenDpi: Boolean);
begin
  FIsScreenDpiUsed := AUseScreenDpi;
end;

procedure TdxPictureFloatingObjectContent.AfterRunInserted;
begin
  UnsubscribePicturePropertiesEvents;
  SubscribePicturePropertiesEvents;
end;

procedure TdxPictureFloatingObjectContent.BeforeRunRemoved;
begin
  UnsubscribePicturePropertiesEvents;
end;

procedure TdxPictureFloatingObjectContent.HandleImageChanged(E: TdxNativeImageChangedEventArgs);
var
  ARunIndex: TdxRunIndex;
  AImageSize: TSize;
begin
  ARunIndex := RunPieceTable.CalculateRunIndex(Run, dxRunIndexDontCare);
  if ARunIndex = dxRunIndexDontCare then
    Exit;

  AImageSize := FImage.CalculateImageSizeInModelUnits(DocumentModel.UnitConverter);
  SetOriginalSize(AImageSize);

  SetActualSize(E.DesiredImageSizeInTwips);
  if FImage.IsLoaded and FImage.ShouldSetDesiredSizeAfterLoad then
    FPictureContainerRun.ActualSize := FImage.DesiredSizeAfterLoad;

  RunPieceTable.ApplyChangesCore(FPictureContainerRun.GetBatchUpdateChangeActions,
    ARunIndex, ARunIndex);
end;

function TdxPictureFloatingObjectContent.GetImageSizeWithDocumentModelDpi: TSize;
var
  AWidthInPixels, AHeightInPixels, AResultWidth, AResultHeight: Integer;
begin
  AWidthInPixels := FImage.SizeInPixels.Width;
  AHeightInPixels := FImage.SizeInPixels.Height;
  AResultWidth := PixelsToTwips(AWidthInPixels, DocumentModel.ScreenDpiX);
  AResultHeight := PixelsToTwips(AHeightInPixels, DocumentModel.ScreenDpiY);
  Result := TSize.Create(AResultWidth, AResultHeight);
end;

function TdxPictureFloatingObjectContent.GetOriginalSize: TSize;
begin
  Result := FOriginalSize;
end;

procedure TdxPictureFloatingObjectContent.OnImageChanged(Sender: TObject; E: TdxNativeImageChangedEventArgs);
begin
  if DocumentModel.IsDestroying then
    Exit;

  if Image.Image <> nil then
  begin
    DocumentModel.SwitchToEmptyHistory(False);
    try
      HandleImageChanged(E);
    finally
      DocumentModel.SwitchToNormalHistory(True);
    end;
  end;
  DocumentModel.EndUpdate;
end;

procedure TdxPictureFloatingObjectContent.OnImageChanging(Sender: TObject; E: TdxEventArgs);
begin
  if DocumentModel.IsDestroying then
    Exit;
  DocumentModel.BeginUpdate;
end;

procedure TdxPictureFloatingObjectContent.SetOriginalSize(const AValue: TSize);
begin
  if (AValue.Width <= 0) or (AValue.Height <= 0) then
    TdxRichEditExceptions.ThrowArgumentException('OriginalSize', AValue);
  FOriginalSize := AValue;
end;

procedure TdxPictureFloatingObjectContent.SubscribePicturePropertiesEvents;
begin
  Image.NativeImageChanging.Add(OnImageChanging);
  Image.NativeImageChanged.Add(OnImageChanged);
end;

procedure TdxPictureFloatingObjectContent.UnsubscribePicturePropertiesEvents;
begin
  Image.NativeImageChanging.Remove(OnImageChanging);
  Image.NativeImageChanged.Remove(OnImageChanged);
end;

function TdxPictureFloatingObjectContent.ApplyScales(const AImageSize: TSize): TSize;
var
  AScaleX, AScaleY: Single;
begin
  AScaleX := FPictureContainerRun.ScaleX;
  AScaleY := FPictureContainerRun.ScaleY;
  Result := AImageSize;
  if AScaleX > 0 then
    Result.Width := Max(1, Round(AScaleX * AImageSize.Width / 100.0));
  if AScaleY > 0 then
    Result.Height := Max(1, Round(AScaleY * AImageSize.Height / 100.0));
end;

function TdxPictureFloatingObjectContent.CalculateActualSize(const ADesiredSize: TSize): TSize;
var
  AUnitConverter: TdxDocumentModelUnitConverter;
  AAspect: Single;
begin
  if (ADesiredSize.Width <= 0) and (ADesiredSize.Height <= 0) then
    Exit(ApplyScales(GetImageSizeWithActualDpi));

  AUnitConverter := DocumentModel.UnitConverter;
  Result := TSize.Empty;
  Result.Width := AUnitConverter.TwipsToModelUnits(ADesiredSize.Width);
  Result.Height := AUnitConverter.TwipsToModelUnits(ADesiredSize.Height);
  AAspect := FImage.SizeInOriginalUnits.Width / FImage.SizeInOriginalUnits.Height;
  if Result.Width <= 0 then
    Result.Width := Round(AAspect * Result.Height);
  if Result.Height <= 0 then
    Result.Height := Round(Result.Width / AAspect);
end;

function TdxPictureFloatingObjectContent.Clone(const ARun: TdxTextRunBase;
  ACopyManager: TdxCustomDocumentModelCopyManager): TdxCustomObjectContent;
var
  I: IdxPictureContainerRun;
begin
  Supports(ARun, IdxPictureContainerRun, I);
  Result := TdxPictureFloatingObjectContent.Create(I, Image);
end;

function TdxPictureFloatingObjectContent.GetImageSizeWithActualDpi: TSize;
begin
  if FIsScreenDpiUsed then
    Result := GetImageSizeWithDocumentModelDpi
  else
    Result := Image.CalculateImageSizeInModelUnits(DocumentModel.UnitConverter);
end;

procedure TdxPictureFloatingObjectContent.SetActualSize(const ADesiredSize: TSize);
var
  AActualSize: TSize;
begin
  AActualSize := CalculateActualSize(ADesiredSize);
  if not AActualSize.IsEqual(FPictureContainerRun.ActualSize) then
    FPictureContainerRun.SetActualSizeInternal(AActualSize);
end;

end.
