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

unit dxRichEdit.DocumentModel.ShapeFormatting;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Graphics,
  dxCoreClasses, dxCoreGraphics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem;

type
  TdxShapeChangeType = (
    None,
    FillColor,
    OutlineColor,
    OutlineWidth,
    Rotation,
    BatchUpdate);

  { IdxShapeContainer }

  IdxShapeContainer = interface
  ['{0D719EF7-DFE5-4248-BC70-4ED4C6B99013}']
    function GetPieceTable: TdxCustomPieceTable;

    procedure OnShapeChanged;
    function CreateShapeChangedHistoryItem: TdxIndexChangedHistoryItemCore;

    property PieceTable: TdxCustomPieceTable read GetPieceTable;
  end;

  { TdxShapeInfo }

  TdxShapeInfo = class(TdxCloneable)
  private
    FFillColor: TdxAlphaColor;
    FOutlineColor: TdxAlphaColor;
    FOutlineWidth: Integer;
    FRotation: Integer;
  public
    function Clone: TdxShapeInfo; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Equals(AObj: TObject): Boolean; override;
    function GetHashCode: Integer; override;

    property FillColor: TdxAlphaColor read FFillColor write FFillColor;
    property OutlineColor: TdxAlphaColor read FOutlineColor write FOutlineColor;
    property OutlineWidth: Integer read FOutlineWidth write FOutlineWidth;
    property Rotation: Integer read FRotation write FRotation;
  end;

  TdxUsedShapeOption = (
    UseFillColor,
    UseOutlineColor,
    UseOutlineWidth,
    UseRotation
  );

  TdxUsedShapeOptions = set of TdxUsedShapeOption;

  { TdxShapeOptions }

  TdxShapeOptions = record
  public const
    MaskUseNone = [];
    MaskUseAll = [Low(TdxUsedShapeOption)..High(TdxUsedShapeOption)];
  private
    FValue: TdxUsedShapeOptions;
    procedure SetVal(AMask: TdxUsedShapeOption; ABitVal: Boolean);
    function GetVal(AMask: TdxUsedShapeOption): Boolean;
    class function GetEmptyShapeOption: TdxShapeOptions; static;
  public
    constructor Create(AValue: TdxUsedShapeOptions);
    function GetHashCode: Integer;
    function Clone: TdxShapeOptions;
    procedure CopyFrom(const AOptions: TdxShapeOptions);

    class operator Equal(const A, B: TdxShapeOptions): Boolean;
    class property EmptyShapeOption: TdxShapeOptions read GetEmptyShapeOption;

    property Value: TdxUsedShapeOptions read FValue write FValue;
    property UseFillColor: Boolean index TdxUsedShapeOption.UseFillColor read GetVal write SetVal;
    property UseOutlineColor: Boolean index TdxUsedShapeOption.UseOutlineColor read GetVal write SetVal;
    property UseOutlineWidth: Boolean index TdxUsedShapeOption.UseOutlineWidth read GetVal write SetVal;
    property UseRotation: Boolean index TdxUsedShapeOption.UseRotation read GetVal write SetVal;
  end;

  { TdxShapeInfoCache }

  TdxShapeInfoCache = class(TdxUniqueItemsCache<TdxShapeInfo>)
  public const
    DefaultItemIndex = 0;
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxShapeInfo; override;
  end;

  { TdxShapeFormatting }

  TdxShapeFormatting = class(TdxIndexBasedObjectB<TdxShapeInfo, TdxShapeOptions>)
  private
    procedure SetFillColorCore(const AInfo: TdxShapeInfo; const AValue: TdxAlphaColor);
    procedure SetOutlineColorCore(const AInfo: TdxShapeInfo; const AValue: Integer);
    procedure SetOutlineWidthCore(const AInfo: TdxShapeInfo; const AValue: Integer);
    procedure SetRotationCore(const AInfo: TdxShapeInfo; const AValue: Integer);

    function GetFillColor: TdxAlphaColor;
    procedure SetFillColor(const Value: TdxAlphaColor);
    function GetOutlineColor: TdxAlphaColor;
    procedure SetOutlineColor(const Value: TdxAlphaColor);
    function GetOutlineWidth: Integer;
    procedure SetOutlineWidth(const Value: Integer);
    function GetRotation: Integer;
    procedure SetRotation(const Value: Integer);
  protected
    function CanSetPropertyValue: Boolean; override;
    function PropertyEquals(const AOther: TdxIndexBasedObject<TdxShapeInfo, TdxShapeOptions>): Boolean; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const ADocumentModel: TdxCustomDocumentModel;
      const AFormattingInfo: TdxShapeInfo; const AFormattingOptions: TdxShapeOptions); reintroduce;

    function Clone: TdxCloneable; override;
    procedure CopyFrom(Source: TdxCloneable); overload; override;
    procedure CopyFrom(const AInfo: TdxShapeInfo; const AOptions: TdxShapeOptions); reintroduce; overload;

    property FillColor: TdxAlphaColor read GetFillColor write SetFillColor;
    property OutlineColor: TdxAlphaColor read GetOutlineColor write SetOutlineColor;
    property OutlineWidth: Integer read GetOutlineWidth write SetOutlineWidth;
    property Rotation: Integer read GetRotation write SetRotation;
  end;

  { TdxShapeFormattingCache }

  TdxShapeFormattingCache = class(TdxUniqueItemsCache<TdxShapeFormatting>)
  public const
    EmptyShapeFormattingIndex = 0;
  private
    FDefaultShapeInfo: TdxShapeInfo;
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxShapeFormatting; override;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); reintroduce;
    destructor Destroy; override;

    property DefaultShapeInfo: TdxShapeInfo read FDefaultShapeInfo;
  end;

  { TdxShapeChangeActionsCalculator }

  TdxShapeChangeActionsCalculator = class
  public
    class function CalculateChangeActions(const AChange: TdxShapeChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxShape }

  TdxShape = class(TdxRichEditIndexBasedObject<TdxShapeFormatting>)
  strict private
    FOwner: IdxShapeContainer;
    function GetRotation: Integer;
    procedure SetRotation(const Value: Integer);
    function GetUseRotation: Boolean;
    function GetOutlineColor: TdxAlphaColor;
    procedure SetOutlineColor(const Value: TdxAlphaColor);
    function GetUseOutlineColor: Boolean;
    function GetOutlineWidth: Integer;
    procedure SetOutlineWidth(const Value: Integer);
    function GetUseOutlineWidth: Boolean;
    function GetFillColor: TdxAlphaColor;
    procedure SetFillColor(const Value: TdxAlphaColor);
    function GetUseFillColor: Boolean;
  strict protected
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    procedure OnIndexChanged; override;
    function CreateIndexChangedHistoryItem: TdxIndexChangedHistoryItemCore; override;
    function SetFillColorCore(const AInfo: TdxShapeFormatting; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetOutlineWidthCore(const AInfo: TdxShapeFormatting; const AValue: Integer): TdxDocumentModelChangeActions; virtual;

    function SetRotationCore(const AInfo: TdxShapeFormatting; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetOutlineColorCore(const AInfo: TdxShapeFormatting; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxShapeFormatting>; override;
  public
    constructor Create(const AOwner: IdxShapeContainer); reintroduce;

    procedure Reset;
    function Equals(AObj: TObject): Boolean; override;
    procedure ResetUse(AMask: TdxUsedShapeOptions);
    procedure ResetAllUse;
    function GetHashCode: Integer; override;

    property Owner: IdxShapeContainer read FOwner;
    property FillColor: TdxAlphaColor read GetFillColor write SetFillColor;
    property OutlineColor: TdxAlphaColor read GetOutlineColor write SetOutlineColor;
    property OutlineWidth: Integer read GetOutlineWidth write SetOutlineWidth;
    property Rotation: Integer read GetRotation write SetRotation;
    property UseFillColor: Boolean read GetUseFillColor;
    property UseOutlineColor: Boolean read GetUseOutlineColor;
    property UseOutlineWidth: Boolean read GetUseOutlineWidth;
    property UseRotation: Boolean read GetUseRotation;
  end;

implementation

uses
  dxRichEdit.DocumentModel.Cache;

{ TdxShapeInfo }

function TdxShapeInfo.Clone: TdxShapeInfo;
begin
  Result := TdxShapeInfo(inherited Clone);
end;

procedure TdxShapeInfo.CopyFrom(Source: TdxCloneable);
var
  AVal: TdxShapeInfo absolute Source;
begin
  FillColor := AVal.FillColor;
  OutlineColor := AVal.OutlineColor;
  OutlineWidth := AVal.OutlineWidth;
  Rotation := AVal.Rotation;
end;

function TdxShapeInfo.Equals(AObj: TObject): Boolean;
var
  AInfo: TdxShapeInfo absolute AObj;
begin
  Result := (FillColor = AInfo.FillColor) and (OutlineColor = AInfo.OutlineColor) and
    (OutlineWidth = AInfo.OutlineWidth) and (Rotation = AInfo.Rotation);
end;

function TdxShapeInfo.GetHashCode: Integer;
begin
  Result := inherited GetHashCode;
end;

{ TdxShapeOptions }

procedure TdxShapeOptions.CopyFrom(const AOptions: TdxShapeOptions);
begin
  FValue := AOptions.Value;
end;

function TdxShapeOptions.Clone: TdxShapeOptions;
begin
  Result := TdxShapeOptions.Create(FValue);
end;

constructor TdxShapeOptions.Create(AValue: TdxUsedShapeOptions);
begin
  FValue := AValue;
end;

class operator TdxShapeOptions.Equal(const A, B: TdxShapeOptions): Boolean;
begin
  Result := A.FValue = B.FValue;
end;

class function TdxShapeOptions.GetEmptyShapeOption: TdxShapeOptions;
begin
  Result := TdxShapeOptions.Create([]);
end;

function TdxShapeOptions.GetHashCode: Integer;
begin
  Result := Byte(FValue);
end;

function TdxShapeOptions.GetVal(AMask: TdxUsedShapeOption): Boolean;
begin
  Result := AMask in FValue;
end;

procedure TdxShapeOptions.SetVal(AMask: TdxUsedShapeOption; ABitVal: Boolean);
begin
  if ABitVal then
    Include(FValue, AMask)
  else
    Exclude(FValue, AMask);
end;

{ TdxShapeInfoCache }

function TdxShapeInfoCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxShapeInfo;
begin
  Result := TdxShapeInfo.Create;
end;

{ TdxShapeFormatting }

constructor TdxShapeFormatting.Create(APieceTable: TdxCustomPieceTable; const ADocumentModel: TdxCustomDocumentModel;
  const AFormattingInfo: TdxShapeInfo; const AFormattingOptions: TdxShapeOptions);
begin
  inherited Create(APieceTable, ADocumentModel, AFormattingInfo, AFormattingOptions);
end;

function TdxShapeFormatting.Clone: TdxCloneable;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxShapeFormatting.Create(PieceTable, DocumentModel, Info, Options);
end;

function TdxShapeFormatting.GetFillColor: TdxAlphaColor;
begin
  Result := Info.FillColor;
end;

function TdxShapeFormatting.CanSetPropertyValue: Boolean;
begin
  Result := DocumentModel.DocumentCapabilities.FloatingObjectsAllowed;
end;

function TdxShapeFormatting.PropertyEquals(const AOther: TdxIndexBasedObject<TdxShapeInfo, TdxShapeOptions>): Boolean;
begin
  Assert(AOther <> nil);
  Result := (Options.Value = AOther.Options.Value) and Info.Equals(AOther.Info);
end;

function TdxShapeFormatting.GetOutlineColor: TdxAlphaColor;
begin
  Result := Info.OutlineColor;
end;

function TdxShapeFormatting.GetOutlineWidth: Integer;
begin
  Result := Info.OutlineWidth;
end;

function TdxShapeFormatting.GetRotation: Integer;
begin
  Result := Info.Rotation;
end;

procedure TdxShapeFormatting.SetFillColor(const Value: TdxAlphaColor);
begin
  if (Info.FillColor = Value) and Options.UseFillColor then
    Exit;
  SetPropertyValue<TdxAlphaColor>(SetFillColorCore, Value);
  Options.UseFillColor := True;
end;

procedure TdxShapeFormatting.SetFillColorCore(const AInfo: TdxShapeInfo; const AValue: TdxAlphaColor);
begin
  AInfo.FillColor := AValue;
end;

procedure TdxShapeFormatting.SetOutlineColor(const Value: TdxAlphaColor);
begin
  if (Info.OutlineColor = Value) and Options.UseOutlineColor then
    Exit;
  SetPropertyValue<Integer>(SetOutlineColorCore, Value);
  Options.UseOutlineColor := True;
end;

procedure TdxShapeFormatting.SetOutlineColorCore(const AInfo: TdxShapeInfo; const AValue: Integer);
begin
  AInfo.OutlineColor := TdxAlphaColor(AValue);
end;

procedure TdxShapeFormatting.SetOutlineWidth(const Value: Integer);
begin
  if (Info.OutlineWidth = Value) and Options.UseOutlineWidth then
    Exit;
  SetPropertyValue<Integer>(SetOutlineWidthCore, Value);
  Options.UseOutlineWidth := True;
end;

procedure TdxShapeFormatting.SetOutlineWidthCore(const AInfo: TdxShapeInfo; const AValue: Integer);
begin
  AInfo.OutlineWidth := AValue;
end;

procedure TdxShapeFormatting.SetRotation(const Value: Integer);
begin
  if (Info.Rotation = Value) and Options.UseRotation then
    Exit;
  SetPropertyValue<Integer>(SetRotationCore, Value);
  Options.UseRotation := True;
end;

procedure TdxShapeFormatting.SetRotationCore(const AInfo: TdxShapeInfo; const AValue: Integer);
begin
  AInfo.Rotation := AValue;
end;

procedure TdxShapeFormatting.CopyFrom(Source: TdxCloneable);
var
  AShapeFormatting: TdxShapeFormatting absolute Source;
begin
  CopyFrom(AShapeFormatting.Info, AShapeFormatting.Options);
end;

procedure TdxShapeFormatting.CopyFrom(const AInfo: TdxShapeInfo; const AOptions: TdxShapeOptions);
begin
  CopyFromCore(AInfo, AOptions);
end;

{ TdxShapeFormattingCache }

constructor TdxShapeFormattingCache.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel.UnitConverter, ADocumentModel);
  Assert(ADocumentModel <> nil);
  FDefaultShapeInfo := TdxShapeInfo.Create;
  AppendItem(TdxShapeFormatting.Create(DocumentModel.MainPart, DocumentModel,
    FDefaultShapeInfo, TdxShapeOptions.EmptyShapeOption));
end;

function TdxShapeFormattingCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxShapeFormatting;
begin
  Result := nil;
end;

destructor TdxShapeFormattingCache.Destroy;
begin
  FDefaultShapeInfo.Free;
  inherited Destroy;
end;

{ TdxShapeChangeActionsCalculator }

class function TdxShapeChangeActionsCalculator.CalculateChangeActions(
  const AChange: TdxShapeChangeType): TdxDocumentModelChangeActions;
const
  ShapeChangeActionsMap: array[TdxShapeChangeType] of TdxDocumentModelChangeActions = (
    [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler]
  );
begin
  Result := ShapeChangeActionsMap[AChange];
end;

{ TdxShape }

constructor TdxShape.Create(const AOwner: IdxShapeContainer);
begin
  inherited Create(AOwner.PieceTable);
  FOwner := AOwner;
end;

function TdxShape.CreateIndexChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := FOwner.CreateShapeChangedHistoryItem;
end;

function TdxShape.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxShapeChangeActionsCalculator.CalculateChangeActions(TdxShapeChangeType.BatchUpdate);
end;

function TdxShape.GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxShapeFormatting>;
begin
  Result := TdxDocumentCache(ADocumentModel.Cache).ShapeFormattingCache;
end;

function TdxShape.GetFillColor: TdxAlphaColor;
begin
  Result := Info.FillColor;
end;

function TdxShape.GetOutlineColor: TdxAlphaColor;
begin
  Result := Info.OutlineColor;
end;

function TdxShape.GetOutlineWidth: Integer;
begin
  Result := Info.OutlineWidth;
end;

function TdxShape.GetRotation: Integer;
begin
  Result := Info.Rotation;
end;

function TdxShape.GetUseFillColor: Boolean;
begin
  Result := Info.Options.UseFillColor;
end;

function TdxShape.GetUseOutlineColor: Boolean;
begin
  Result := Info.Options.UseOutlineColor;
end;

function TdxShape.GetUseOutlineWidth: Boolean;
begin
  Result := Info.Options.UseOutlineWidth;
end;

function TdxShape.GetUseRotation: Boolean;
begin
  Result := Info.Options.UseRotation;
end;

procedure TdxShape.OnIndexChanged;
begin
  inherited OnIndexChanged;
  FOwner.OnShapeChanged;
end;

procedure TdxShape.Reset;
var
  AInfo: TdxShapeFormatting;
  AEmptyInfo: TdxShapeFormatting;
  AIsDeferredInfo: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferredInfo);
  AEmptyInfo := GetCache(DocumentModel)[TdxShapeFormattingCache.EmptyShapeFormattingIndex];
  AInfo.ReplaceInfo(AEmptyInfo.Info, AEmptyInfo.Options);
  ReplaceInfo(AInfo, BatchUpdateChangeActions);
  if not AIsDeferredInfo then AInfo.Free;
end;

function TdxShape.Equals(AObj: TObject): Boolean;
var
  AOther: TdxShape absolute AObj;
begin
  if not (AObj is TdxShape) then
    Exit(False);
  if DocumentModel = AOther.DocumentModel then
    Result := Index = AOther.Index
  else
    Result := Info.Equals(AOther.Info);
end;

procedure TdxShape.ResetUse(AMask: TdxUsedShapeOptions);
var
  AInfo: TdxShapeFormatting;
  AOptions: TdxShapeOptions;
  AIsDeferredInfo: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferredInfo);
  AOptions := AInfo.Options;
  AOptions.Value := AOptions.Value - AMask;
  AInfo.Options := AOptions;
  ReplaceInfo(AInfo, BatchUpdateChangeActions);
  if not AIsDeferredInfo then AInfo.Free;
end;

procedure TdxShape.ResetAllUse;
var
  AInfo: TdxShapeFormatting;
  AIsDeferredInfo: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferredInfo);
  AInfo.Options := TdxShapeOptions.Create(TdxShapeOptions.MaskUseNone);
  ReplaceInfo(AInfo, BatchUpdateChangeActions);
  if not AIsDeferredInfo then AInfo.Free;
end;

function TdxShape.GetHashCode: Integer;
begin
  Result := Index;
end;

procedure TdxShape.SetFillColor(const Value: TdxAlphaColor);
begin
  if (Info.FillColor = Value) and UseFillColor then
    Exit;
  SetPropertyValue<Integer>(SetFillColorCore, Value);
end;

function TdxShape.SetFillColorCore(const AInfo: TdxShapeFormatting; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.FillColor := TdxAlphaColor(AValue);
  Result := TdxShapeChangeActionsCalculator.CalculateChangeActions(TdxShapeChangeType.FillColor);
end;

procedure TdxShape.SetOutlineColor(const Value: TdxAlphaColor);
begin
  if (Info.OutlineColor = Value) and UseOutlineColor then
    Exit;
  SetPropertyValue<Integer>(SetOutlineColorCore, Ord(Value));
end;

function TdxShape.SetOutlineColorCore(const AInfo: TdxShapeFormatting; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.OutlineColor := TdxAlphaColor(AValue);
  Result := TdxShapeChangeActionsCalculator.CalculateChangeActions(TdxShapeChangeType.OutlineColor);
end;

procedure TdxShape.SetOutlineWidth(const Value: Integer);
begin
  if (Info.OutlineWidth = Value) and UseOutlineWidth then
    Exit;
  SetPropertyValue<Integer>(SetOutlineWidthCore, Value);
end;

function TdxShape.SetOutlineWidthCore(const AInfo: TdxShapeFormatting; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.OutlineWidth := AValue;
  Result := TdxShapeChangeActionsCalculator.CalculateChangeActions(TdxShapeChangeType.OutlineWidth);
end;

procedure TdxShape.SetRotation(const Value: Integer);
begin
  if (Info.Rotation = Value) and UseRotation then
    Exit;
  SetPropertyValue<Integer>(SetRotationCore, Value);
end;

function TdxShape.SetRotationCore(const AInfo: TdxShapeFormatting; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.Rotation := AValue;
  Result := TdxShapeChangeActionsCalculator.CalculateChangeActions(TdxShapeChangeType.Rotation);
end;

end.
