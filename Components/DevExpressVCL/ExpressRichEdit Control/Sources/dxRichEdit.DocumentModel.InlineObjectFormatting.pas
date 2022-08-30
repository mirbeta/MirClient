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

unit dxRichEdit.DocumentModel.InlineObjectFormatting;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, dxCoreClasses, dxCoreGraphics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem;

type
  TdxInlineObjectChangeType = (
    None,
    ScaleX,
    ScaleY,
    BatchUpdate,
    Sizing);

  TdxInlineCustomObjectChangeType = (
    None,
    State,
    BatchUpdate);

  { IdxInlineCustomObject }

  IdxInlineCustomObject = interface
    function Clone: IdxInlineCustomObject;
    function CalculateOriginalSize(AUnitConverter: TdxDocumentModelUnitConverter): TSize;
  end;

  { TdxInlineObjectInfo }

  TdxInlineObjectInfo = class(TdxCloneable)
  private
    FScaleX: Single;
    FScaleY: Single;
  protected
    procedure CopyFromCore(const AInfo: TdxInlineObjectInfo);
  public
    function Equals(Obj: TObject): Boolean; override;

    property ScaleX: Single read FScaleX write FScaleX;
    property ScaleY: Single read FScaleY write FScaleY;
  end;

  { TdxInlineCustomObjectInfo }

  TdxInlineCustomObjectInfo = class(TdxInlineObjectInfo)
  private
    FState: TArray<Byte>;
  public
    function Equals(Obj: TObject): Boolean; override;
    function Clone: TdxInlineCustomObjectInfo; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;

    property State: TArray<Byte> read FState write FState;
  end;

  { TdxInlineObjectChangeActionsCalculator }

  TdxInlineObjectChangeActionsCalculator = class
  public
    class function CalculateChangeActions(AChange: TdxInlineObjectChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxInlineObjectProperties }

  TdxInlineObjectProperties<T: TdxInlineObjectInfo> = class abstract(TdxRichEditIndexBasedObject<T>)
  private
    function GetScaleY: Single;
    procedure SetScaleY(const AValue: Single);
    function GetScaleX: Single;
    procedure SetScaleX(const AValue: Single);
  public
    property ScaleY: Single read GetScaleY write SetScaleY;
    property ScaleX: Single read GetScaleX write SetScaleX;
  end;

  { TdxInlineCustomObjectChangeActionsCalculator }

  TdxInlineCustomObjectChangeActionsCalculator = class
  public
    class function CalculateChangeActions(AChange: TdxInlineCustomObjectChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxInlineCustomObjectProperties }

  TdxInlineCustomObjectProperties = class(TdxInlineObjectProperties<TdxInlineCustomObjectInfo>)
  protected
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxInlineCustomObjectInfo>; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    procedure OnIndexChanged; override;

    procedure RaiseIndexChanged;
  end;

  { TdxInlineCustomObjectInfoCache }

  TdxInlineCustomObjectInfoCache = class(TdxUniqueItemsCache<TdxInlineCustomObjectInfo>)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxInlineCustomObjectInfo; override;
  end;


  TdxResizingShadowDisplayMode = (
    Content,
    WireFrame);

  TdxInlinePictureChangeType = (
    None = 0,
    BatchUpdate);

  { IdxInlinePicturePropertiesContainer }

  IdxInlinePicturePropertiesContainer = interface
  ['{DBD39111-B5B8-4AE2-B7DC-2AD216C22A01}']
    function GetPieceTable: TdxCustomPieceTable;
    function CreateInlinePicturePropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnInlineCharacterPropertiesChanged;

    property PieceTable: TdxCustomPieceTable read GetPieceTable;
  end;

  { TdxInlinePictureInfo }

  TdxInlinePictureInfo = class(TdxInlineObjectInfo)
  private
    FFillColor: TdxAlphaColor;
    FLockAspectRatio: Boolean;
    FPseudoInline: Boolean;
    FResizingShadowDisplayMode: TdxResizingShadowDisplayMode;
    FSizing: TdxImageSizeMode;
  public
    constructor Create; override;
    function GetHashCode: Integer; override;

    procedure CopyFrom(Source: TdxCloneable); override;
    function Clone: TdxInlinePictureInfo; reintroduce; inline;
    function Equals(Obj: TObject): Boolean; override;

    property FillColor: TdxAlphaColor read FFillColor write FFillColor;
    property Sizing: TdxImageSizeMode read FSizing write FSizing;
    property ResizingShadowDisplayMode: TdxResizingShadowDisplayMode read FResizingShadowDisplayMode write FResizingShadowDisplayMode;
    property LockAspectRatio: Boolean read FLockAspectRatio write FLockAspectRatio;
    property PseudoInline: Boolean read FPseudoInline write FPseudoInline;
  end;

  { TdxInlinePictureChangeActionsCalculator }

  TdxInlinePictureChangeActionsCalculator = class
  public
    class function CalculateChangeActions(AChange: TdxInlinePictureChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxInlinePictureProperties }

  TdxInlinePictureProperties = class(TdxInlineObjectProperties<TdxInlinePictureInfo>)
  strict private
    FOwner: IdxInlinePicturePropertiesContainer;
    function GetFillColor: TdxAlphaColor;
    function GetSizing: TdxImageSizeMode;
    function GetResizingShadowDisplayMode: TdxResizingShadowDisplayMode;
    function GetLockAspectRatio: Boolean;
    function GetPseudoInline: Boolean;
    procedure SetFillColor(const Value: TdxAlphaColor);
    procedure SetSizing(const Value: TdxImageSizeMode);
    procedure SetResizingShadowDisplayMode(const Value: TdxResizingShadowDisplayMode);
    procedure SetLockAspectRatio(const Value: Boolean);
    procedure SetPseudoInline(const Value: Boolean);
  strict protected
    function SetFillColorCore(const AInfo: TdxInlinePictureInfo; const AValue: TdxAlphaColor): TdxDocumentModelChangeActions; virtual;
    function SetSizingCore(const AInfo: TdxInlinePictureInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetResizingShadowDisplayModeCore(const AInfo: TdxInlinePictureInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetLockAspectRatioCore(const AInfo: TdxInlinePictureInfo; const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetPseudoInlineCore(const AInfo: TdxInlinePictureInfo; const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function CreateIndexChangedHistoryItem: TdxIndexChangedHistoryItemCore; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxInlinePictureInfo>; override;
    procedure OnIndexChanged; override;

    property Owner: IdxInlinePicturePropertiesContainer read FOwner;
  public
    constructor Create(const AOwner: IdxInlinePicturePropertiesContainer); reintroduce;

    property FillColor: TdxAlphaColor read GetFillColor write SetFillColor;
    property Sizing: TdxImageSizeMode read GetSizing write SetSizing;
    property ResizingShadowDisplayMode: TdxResizingShadowDisplayMode read GetResizingShadowDisplayMode write SetResizingShadowDisplayMode;
    property LockAspectRatio: Boolean read GetLockAspectRatio write SetLockAspectRatio;
    property PseudoInline: Boolean read GetPseudoInline write SetPseudoInline;
  end;

  { TdxInlinePictureInfoCache }

  TdxInlinePictureInfoCache = class(TdxUniqueItemsCache<TdxInlinePictureInfo>)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxInlinePictureInfo; override;
  end;

implementation

uses
  dxRichEdit.DocumentModel.Cache.Simple,
  dxRichEdit.Utils.BatchUpdateHelper;

{ TdxInlineObjectInfo }

function TdxInlineObjectInfo.Equals(Obj: TObject): Boolean;
var
  AInfo: TdxInlineObjectInfo absolute Obj;
begin
  Result := (AInfo <> nil) and (FScaleX = AInfo.ScaleX) and (FScaleY = AInfo.ScaleY);
end;

procedure TdxInlineObjectInfo.CopyFromCore(const AInfo: TdxInlineObjectInfo);
begin
  ScaleX := AInfo.ScaleX;
  ScaleY := AInfo.ScaleY;
end;

{ TdxInlineCustomObjectInfo }

function TdxInlineCustomObjectInfo.Clone: TdxInlineCustomObjectInfo;
begin
  Result := TdxInlineCustomObjectInfo(inherited Clone);
end;

procedure TdxInlineCustomObjectInfo.CopyFrom(Source: TdxCloneable);
var
  AInfo: TdxInlineCustomObjectInfo absolute Source;
begin
  CopyFromCore(AInfo);
  State := AInfo.State;
end;

function TdxInlineCustomObjectInfo.Equals(Obj: TObject): Boolean;
var
  AInfo: TdxInlineCustomObjectInfo absolute Obj;
  AInfoState: TArray<Byte>;
  I, ACount: Integer;
begin
  if AInfo = nil then
    Exit(False);
  if not inherited Equals(AInfo) then
    Exit(False);

  AInfoState := AInfo.State;
  if FState = AInfoState then
    Exit(True);
  if (FState = nil) or (AInfoState = nil) then
    Exit(False);
  if Length(FState) <> Length(AInfoState) then
    Exit(False);

  ACount := Length(FState);
  for I := 0 to ACount - 1 do
    if FState[I] <> AInfoState[I] then
    Exit(False);

  Result := True;
end;

{ TdxInlineObjectChangeActionsCalculator }

class function TdxInlineObjectChangeActionsCalculator.CalculateChangeActions(
  AChange: TdxInlineObjectChangeType): TdxDocumentModelChangeActions;
const
  InlineObjectChangeActionsMap: array[TdxInlineObjectChangeType] of TdxDocumentModelChangeActions = (
    [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
     TdxDocumentModelChangeAction.ResetRuler]
   );
begin
  Result := InlineObjectChangeActionsMap[AChange];
end;

{ TdxInlineObjectProperties<T> }

function TdxInlineObjectProperties<T>.GetScaleX: Single;
begin
  Result := Info.ScaleX;
end;

function TdxInlineObjectProperties<T>.GetScaleY: Single;
begin
  Result := Info.ScaleY;
end;

procedure TdxInlineObjectProperties<T>.SetScaleX(const AValue: Single);
var
  AInfo: T;
  AChangeActions: TdxDocumentModelChangeActions;
begin
  Assert(AValue > 0, 'ScaleX');
  if IsUpdateLocked then
    DeferredInfo.ScaleX := AValue
  else
  begin
    AInfo := T(InfoCore.Clone);
    AInfo.ScaleX := AValue;
    AChangeActions := TdxInlineObjectChangeActionsCalculator.CalculateChangeActions(TdxInlineObjectChangeType.ScaleX);
    ReplaceInfoCore(AInfo, AChangeActions);
    AInfo.Free;
  end;
end;

procedure TdxInlineObjectProperties<T>.SetScaleY(const AValue: Single);
var
  AInfo: T;
  AChangeActions: TdxDocumentModelChangeActions;
begin
  Assert(AValue > 0, 'ScaleY');
  if IsUpdateLocked then
    DeferredInfo.ScaleY := AValue
  else
  begin
    AInfo := T(InfoCore.Clone);
    AInfo.ScaleY := AValue;
    AChangeActions := TdxInlineObjectChangeActionsCalculator.CalculateChangeActions(TdxInlineObjectChangeType.ScaleY);
    ReplaceInfoCore(AInfo, AChangeActions);
    AInfo.Free;
  end;
end;

{ TdxInlineCustomObjectChangeActionsCalculator }

class function TdxInlineCustomObjectChangeActionsCalculator.CalculateChangeActions(
  AChange: TdxInlineCustomObjectChangeType): TdxDocumentModelChangeActions;
const
  InlineCustomObjectChangeActionsMap: array[TdxInlineCustomObjectChangeType] of TdxDocumentModelChangeActions = (
    [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
     TdxDocumentModelChangeAction.ResetRuler]
   );
begin
  Result := InlineCustomObjectChangeActionsMap[AChange];
end;

{ TdxInlineCustomObjectProperties }

function TdxInlineCustomObjectProperties.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxInlineCustomObjectChangeActionsCalculator.CalculateChangeActions(TdxInlineCustomObjectChangeType.BatchUpdate);
end;

function TdxInlineCustomObjectProperties.GetCache(
  const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxInlineCustomObjectInfo>;
begin
  Result := TdxSimpleDocumentCache(ADocumentModel.Cache).InlineCustomObjectInfoCache;
end;

procedure TdxInlineCustomObjectProperties.OnIndexChanged;
begin
  inherited OnIndexChanged;
  RaiseIndexChanged;
end;

procedure TdxInlineCustomObjectProperties.RaiseIndexChanged;
begin
NotImplemented;
end;

{ TdxInlineCustomObjectInfoCache }

function TdxInlineCustomObjectInfoCache.CreateDefaultItem(
  const AUnitConverter: IdxDocumentModelUnitConverter): TdxInlineCustomObjectInfo;
begin
  Result := TdxInlineCustomObjectInfo.Create;
  Result.ScaleX := 100;
  Result.ScaleY := 100;
end;


{ TdxInlinePictureInfo }

constructor TdxInlinePictureInfo.Create;
begin
  inherited Create;
  FFillColor := TdxAlphaColors.Empty;
  FSizing := TdxImageSizeMode.StretchImage;
  FResizingShadowDisplayMode := TdxResizingShadowDisplayMode.Content;
  FLockAspectRatio := True;
end;

function TdxInlinePictureInfo.GetHashCode: Integer;
begin
  Result := Round(ScaleX) or (Round(ScaleY) shl 12) or
    (Ord(Sizing) shl 24) or (Ord(ResizingShadowDisplayMode) shl 28) or
    (Ord(LockAspectRatio) shl 29) or (Ord(PseudoInline) shl 30) or
    (FillColor shl 31);
end;

function TdxInlinePictureInfo.Clone: TdxInlinePictureInfo;
begin
  Result := TdxInlinePictureInfo(inherited Clone);
end;

procedure TdxInlinePictureInfo.CopyFrom(Source: TdxCloneable);
var
  AInlinePictureInfo: TdxInlinePictureInfo absolute Source;
begin
  CopyFromCore(AInlinePictureInfo);
  Sizing := AInlinePictureInfo.Sizing;
  ResizingShadowDisplayMode := AInlinePictureInfo.ResizingShadowDisplayMode;
  LockAspectRatio := AInlinePictureInfo.LockAspectRatio;
  PseudoInline := AInlinePictureInfo.PseudoInline;
  FillColor := AInlinePictureInfo.FillColor;
end;

function TdxInlinePictureInfo.Equals(Obj: TObject): Boolean;
var
  AInfo: TdxInlinePictureInfo absolute Obj;
begin
  Result := inherited Equals(Obj);
  if Result then
  begin
    Result := (AInfo.Sizing = Sizing) and
      (AInfo.ResizingShadowDisplayMode = ResizingShadowDisplayMode) and
      (AInfo.LockAspectRatio = LockAspectRatio) and
      (AInfo.PseudoInline = PseudoInline) and
      (AInfo.FillColor = FillColor);
  end;
end;

{ TdxInlinePictureChangeActionsCalculator }

class function TdxInlinePictureChangeActionsCalculator.CalculateChangeActions(
  AChange: TdxInlinePictureChangeType): TdxDocumentModelChangeActions;
const
  InlinePictureChangeActionsMap: array[TdxInlinePictureChangeType] of TdxDocumentModelChangeActions = (
    [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
     TdxDocumentModelChangeAction.ResetRuler]
   );
begin
  Result := InlinePictureChangeActionsMap[AChange];
end;

{ TdxInlinePictureProperties }

constructor TdxInlinePictureProperties.Create(const AOwner: IdxInlinePicturePropertiesContainer);
begin
  inherited Create(AOwner.PieceTable);
  FOwner := AOwner;
end;

function TdxInlinePictureProperties.CreateIndexChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := FOwner.CreateInlinePicturePropertiesChangedHistoryItem;
end;

function TdxInlinePictureProperties.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxInlinePictureChangeActionsCalculator.CalculateChangeActions(TdxInlinePictureChangeType.BatchUpdate);
end;

function TdxInlinePictureProperties.GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxInlinePictureInfo>;
begin
  Result := TdxSimpleDocumentCache(ADocumentModel.Cache).InlinePictureInfoCache;
end;

function TdxInlinePictureProperties.GetFillColor: TdxAlphaColor;
begin
  Result := Info.FillColor;
end;

function TdxInlinePictureProperties.GetLockAspectRatio: Boolean;
begin
  Result := Info.LockAspectRatio;
end;

function TdxInlinePictureProperties.GetPseudoInline: Boolean;
begin
  Result := Info.PseudoInline;
end;

function TdxInlinePictureProperties.GetResizingShadowDisplayMode: TdxResizingShadowDisplayMode;
begin
  Result := Info.ResizingShadowDisplayMode;
end;

function TdxInlinePictureProperties.GetSizing: TdxImageSizeMode;
begin
  Result := Info.Sizing;
end;

procedure TdxInlinePictureProperties.OnIndexChanged;
begin
  Owner.OnInlineCharacterPropertiesChanged;
end;

function TdxInlinePictureProperties.SetFillColorCore(const AInfo: TdxInlinePictureInfo;
  const AValue: TdxAlphaColor): TdxDocumentModelChangeActions;
begin
  AInfo.FillColor := AValue;
  Result := [
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetPrimaryLayout,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ResetSelectionLayout,
    TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
    TdxDocumentModelChangeAction.ResetRuler];
end;

procedure TdxInlinePictureProperties.SetFillColor(const Value: TdxAlphaColor);
begin
  SetPropertyValue<TdxAlphaColor>(SetFillColorCore, Value);
end;

procedure TdxInlinePictureProperties.SetLockAspectRatio(const Value: Boolean);
begin
  SetPropertyValue<Boolean>(SetLockAspectRatioCore, Value);
end;

function TdxInlinePictureProperties.SetLockAspectRatioCore(const AInfo: TdxInlinePictureInfo;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.LockAspectRatio := AValue;
  Result := [];
end;

procedure TdxInlinePictureProperties.SetPseudoInline(const Value: Boolean);
begin
  SetPropertyValue<Boolean>(SetPseudoInlineCore, Value);
end;

function TdxInlinePictureProperties.SetPseudoInlineCore(const AInfo: TdxInlinePictureInfo;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.PseudoInline := AValue;
  Result := [];
end;

procedure TdxInlinePictureProperties.SetResizingShadowDisplayMode(const Value: TdxResizingShadowDisplayMode);
begin
  SetPropertyValue<Integer>(SetResizingShadowDisplayModeCore, Ord(Value));
end;

function TdxInlinePictureProperties.SetResizingShadowDisplayModeCore(const AInfo: TdxInlinePictureInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.ResizingShadowDisplayMode := TdxResizingShadowDisplayMode(AValue);
  Result := [];
end;

procedure TdxInlinePictureProperties.SetSizing(const Value: TdxImageSizeMode);
begin
  SetPropertyValue<Integer>(SetSizingCore, Ord(Value));
end;

function TdxInlinePictureProperties.SetSizingCore(const AInfo: TdxInlinePictureInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.Sizing := TdxImageSizeMode(AValue);
  Result := TdxInlineObjectChangeActionsCalculator.CalculateChangeActions(TdxInlineObjectChangeType.Sizing);
end;

{ TdxInlinePictureInfoCache }

function TdxInlinePictureInfoCache.CreateDefaultItem(
  const AUnitConverter: IdxDocumentModelUnitConverter): TdxInlinePictureInfo;
begin
  Result := TdxInlinePictureInfo.Create;
  Result.ScaleX := 100;
  Result.ScaleY := 100;
end;

end.
