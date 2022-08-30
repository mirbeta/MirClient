{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxSparkline;

{$I cxVer.inc}

interface

uses
  Types, Variants, Windows, Classes, SysUtils, Graphics, Contnrs, Controls, Messages,
  dxCore, dxCoreGraphics, dxCoreClasses, cxGraphics, cxGeometry, dxGDIPlusApi, dxGDIPlusClasses,
  cxClasses, cxContainer, cxCustomData, cxDataStorage, cxDataUtils,
  cxEdit, cxEditConsts;


type
  TdxSparklineValues = array of Variant;
  TdxSparklineSeriesType = (stLine, stPoint, stArea, stBar);
  TdxSparklineMinMaxRangeType = (mmrtAuto, mmrtZeroBasedAuto, mmrtCustom);
  TdxSparklineProperties = class;
  TdxSparklineViewInfo = class;

  { TdxSparklineItemDataBinding }

  TdxSparklineItemDataBinding = class(TPersistent)
  strict private
    FDataIndex: Integer;
    FDefaultValueTypeClass: TcxValueTypeClass;
    FOwner: TPersistent;
    FProperties: TdxSparklineProperties;
    FValueType: string;
    function GetDataController: TcxCustomDataController;
    function GetValueType: string;
    procedure SetValueType(const AValue: string);
  protected
    function GetOwner: TPersistent; override;
    procedure Initialize; virtual;
    function IsBindingActive: Boolean; virtual;
    function IsValueTypeStored: Boolean; virtual;

    property DataController: TcxCustomDataController read GetDataController;
    property DefaultValueTypeClass: TcxValueTypeClass read FDefaultValueTypeClass write FDefaultValueTypeClass;
    property Owner: TPersistent read FOwner;
  public
    constructor Create(AOwner: TPersistent; AProperties: TdxSparklineProperties;
      ADefaultValueTypeClass: TcxValueTypeClass); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property DataIndex: Integer read FDataIndex write FDataIndex;
    property Properties: TdxSparklineProperties read FProperties;
  published
    property ValueType: string read GetValueType write SetValueType stored IsValueTypeStored;
  end;

  TdxSparklineItemDataBindingClass = class of TdxSparklineItemDataBinding;

  { TdxSparklineCategories }

  TdxSparklineCategories = class(TPersistent)
  strict private
    FDataBinding: TdxSparklineItemDataBinding;
    FMaxValue: Double;
    FMinMaxRangeType: TdxSparklineMinMaxRangeType;
    FMinValue: Double;
    FOwner: TdxSparklineProperties;

    function GetDataController: TcxCustomDataController;
    function GetDataIndex: Integer;
    function GetValue(AIndex: Integer): Variant; inline;
    function GetValueCount: Integer; inline;
    function IsMaxValueStored: Boolean;
    function IsMinValueStored: Boolean;
    procedure SetDataBinding(AValue: TdxSparklineItemDataBinding);
    procedure SetMaxValue(AValue: Double);
    procedure SetMinMaxRangeType(AValue: TdxSparklineMinMaxRangeType);
    procedure SetMinValue(AValue: Double);
  protected
    function CreateDataBinding: TdxSparklineItemDataBinding; virtual;
    function GetOwner: TPersistent; override;

//    property DataBinding: TdxSparklineItemDataBinding read FDataBinding;
    property DataController: TcxCustomDataController read GetDataController;
  public
    constructor Create(AOwner: TdxSparklineProperties); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property DataIndex: Integer read GetDataIndex;
    property Owner: TdxSparklineProperties read FOwner;
    property ValueCount: Integer read GetValueCount;
    property Values[Index: Integer]: Variant read GetValue;
  published
    property DataBinding: TdxSparklineItemDataBinding read FDataBinding write SetDataBinding;
    property MaxValue: Double read FMaxValue write SetMaxValue stored IsMaxValueStored;
    property MinMaxRangeType: TdxSparklineMinMaxRangeType read FMinMaxRangeType write SetMinMaxRangeType default mmrtAuto;
    property MinValue: Double read FMinValue write SetMinValue stored IsMinValueStored;
  end;

  { TdxSparklineSeries }

  TdxSparklineSeries = class(TCollectionItem)
  strict private
    FDataBinding: TdxSparklineItemDataBinding;
    FSeriesType: TdxSparklineSeriesType;
    FVisible: Boolean;

    FColor: TColor;
    FEndPointColor: TColor;
    FMaxPointColor: TColor;
    FMinPointColor: TColor;
    FNegativePointColor: TColor;
    FStartPointColor: TColor;

    FAreaOpacity: Byte;

    FLineWidth: Integer;
    FMarkerColor: TColor;
    FMarkerSize: Integer;

    function GetDataController: TcxCustomDataController;
    function GetDataIndex: Integer; inline;
    function GetOwnerProperties: TdxSparklineProperties;
    function GetScaleFactor: TdxScaleFactor;
    function GetValue(AIndex: Integer): Variant; inline;
    function GetValueCount: Integer; inline;
    procedure SetAreaOpacity(AValue: Byte);
    procedure SetColor(AValue: TColor);
    procedure SetDataBinding(AValue: TdxSparklineItemDataBinding);
    procedure SetEndPointColor(AValue: TColor);
    procedure SetLineWidth(AValue: Integer);
    procedure SetMarkerColor(AValue: TColor);
    procedure SetMarkerSize(AValue: Integer);
    procedure SetMaxPointColor(AValue: TColor);
    procedure SetMinPointColor(AValue: TColor);
    procedure SetNegativePointColor(AValue: TColor);
    procedure SetSeriesType(AValue: TdxSparklineSeriesType);
    procedure SetStartPointColor(AValue: TColor);
    procedure SetVisible(AValue: Boolean);
  protected
    procedure ChangeScale(M, D: Integer); virtual;
    function CreateDataBinding: TdxSparklineItemDataBinding; virtual;
    procedure SetCollection(Value: TCollection); override;

    property DataController: TcxCustomDataController read GetDataController;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;

    property DataIndex: Integer read GetDataIndex;
    property Owner: TdxSparklineProperties read GetOwnerProperties;
    property ValueCount: Integer read GetValueCount;
    property Values[Index: Integer]: Variant read GetValue;
  published
    property DataBinding: TdxSparklineItemDataBinding read FDataBinding write SetDataBinding;
    property AreaOpacity: Byte read FAreaOpacity write SetAreaOpacity default 127;
    property Color: TColor read FColor write SetColor default clDefault;
    property EndPointColor: TColor read FEndPointColor write SetEndPointColor default clNone;
    property LineWidth: Integer read FLineWidth write SetLineWidth default 1;
    property MarkerColor: TColor read FMarkerColor write SetMarkerColor default clNone;
    property MarkerSize: Integer read FMarkerSize write SetMarkerSize default 3;
    property MaxPointColor: TColor read FMaxPointColor write SetMaxPointColor default clNone;
    property MinPointColor: TColor read FMinPointColor write SetMinPointColor default clNone;
    property NegativePointColor: TColor read FNegativePointColor write SetNegativePointColor default clNone;
    property SeriesType: TdxSparklineSeriesType read FSeriesType write SetSeriesType default stLine;
    property StartPointColor: TColor read FStartPointColor write SetStartPointColor default clNone;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TdxSparklineSeriesClass = class of TdxSparklineSeries;

  { TdxSparklineSeriesCollection }

  TdxSparklineSeriesCollection = class(TCollection)
  strict private
    FMaxValue: Double;
    FMinMaxRangeType: TdxSparklineMinMaxRangeType;
    FMinValue: Double;
    FProperties: TdxSparklineProperties;

    function GetItem(AIndex: Integer): TdxSparklineSeries;
    function IsMaxValueStored: Boolean;
    function IsMinValueStored: Boolean;
    procedure SetItem(AIndex: Integer; AValue: TdxSparklineSeries);
    procedure SetMaxValue(AValue: Double);
    procedure SetMinMaxRangeType(AValue: TdxSparklineMinMaxRangeType);
    procedure SetMinValue(AValue: Double);
  protected
    procedure ChangeScale(M, D: Integer);
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AProperties: TdxSparklineProperties); virtual;
    function Add: TdxSparklineSeries;
    procedure Assign(Source: TPersistent); override;

    property Properties: TdxSparklineProperties read FProperties;
    property Items[Index: Integer]: TdxSparklineSeries read GetItem write SetItem; default;
  published
    property MaxValue: Double read FMaxValue write SetMaxValue stored IsMaxValueStored;
    property MinMaxRangeType: TdxSparklineMinMaxRangeType read FMinMaxRangeType write SetMinMaxRangeType default mmrtAuto;
    property MinValue: Double read FMinValue write SetMinValue stored IsMinValueStored;
  end;

  { TdxSparklineProperties }

  TdxSparklineProperties = class(TcxCustomEditProperties)
  strict private
    FAntialiasing: Boolean;
    FCategories: TdxSparklineCategories;
    FDataBindings: TcxObjectList;
    FDataController: TcxCustomDataController;
    FPadding: TcxMargin;
    FSeries: TdxSparklineSeriesCollection;

    function GetDataValue: Variant;
    procedure SetAntialiasing(AValue: Boolean);
    procedure SetCategories(AValue: TdxSparklineCategories);
    procedure SetPadding(AValue: TcxMargin);
    procedure SetSeries(AValue: TdxSparklineSeriesCollection);
  protected
    procedure AddDataBinding(ADataBinding: TdxSparklineItemDataBinding); virtual;
    procedure RemoveDataBinding(ADataBinding: TdxSparklineItemDataBinding); virtual;
    //
    procedure ChangeScale(M, D: Integer); override;
    function CreateCategories: TdxSparklineCategories; virtual;
    function CreateDataController: TcxCustomDataController; virtual;
    function CreateSeriesCollection: TdxSparklineSeriesCollection; virtual;
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    function GetItemsDataBinidingClass: TdxSparklineItemDataBindingClass; virtual;
    function GetSeriesClass: TdxSparklineSeriesClass; virtual;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function IsActive: Boolean; virtual;
    procedure LoadRecords(const AEditValue: TcxEditValue; AList: TList); virtual;
    procedure UpdateControl(AInfo: TcxUpdateControlInfo); virtual;
    procedure UpdateItems;
    procedure UpdateItemIndexes;

    property DataBindings: TcxObjectList read FDataBindings;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure DataChanged; override;
    class function GetContainerClass: TcxContainerClass; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;

    property DataController: TcxCustomDataController read FDataController;
    property DataValue: Variant read GetDataValue;
  published
    property Antialiasing: Boolean read FAntialiasing write SetAntialiasing default True;
    property Categories: TdxSparklineCategories read FCategories write SetCategories;
    property Series: TdxSparklineSeriesCollection read FSeries write SetSeries;
    property Padding: TcxMargin read FPadding write SetPadding;
  end;

{ TdxSparklineSeriesViewInfo }

  TdxChartPoint = record
  case Integer of
    0: (Category, Value: Single);
    1: (X, Y: Single);
  end;

  TdxValueRange = record
    Min: Single;
    Max: Single;
  end;

  TdxSeriesRange = record
  case Integer of
    0: (Category, Value: TdxValueRange);
    1: (X, Y: TdxValueRange);
  end;

  TdxSparklineSeriesViewInfo = class
  strict private
    FOwner: TdxSparklineViewInfo;
    FPointColors: TColors;

    FColor: TColor;
    FEndPointColor: TColor;
    FMaxPointColor: TColor;
    FMinPointColor: TColor;
    FNegativePointColor: TColor;
    FStartPointColor: TColor;

    function GetIndex: Integer;
  protected
    function GetColorForDrawing(AColor: TColor; ATint: Integer = 0): TColor;
    function GetPointColor: TColor; virtual;
    function HasData: Boolean; virtual; abstract;
    procedure Offset(DX: Integer; DY: Integer); virtual;

    property Color: TColor read FColor;
    property Owner: TdxSparklineViewInfo read FOwner;
    property PointColors: TColors read FPointColors;
  public
    constructor Create(AOwner: TdxSparklineViewInfo);
    procedure Calculate(const ACategories, AValues: array of Variant;
      const ADataRange: TdxSeriesRange; AValueRange: TdxValueRange; const AStep: TdxChartPoint; const ABounds: TRect); virtual;
    procedure Initialize(ASeries: TdxSparklineSeries); virtual;
    procedure Paint(ACanvas: TcxCanvas); virtual; abstract;
  end;

  TdxSparklineSeriesViewInfoClass = class of TdxSparklineSeriesViewInfo;

  TdxSparklinePointSeriesViewInfo = class(TdxSparklineSeriesViewInfo)
  strict private
    FMarkerColor: TColor;
    FMarkerWidth: Single;
    FPointArray: TdxPointsF;
  protected
    function GetPointColor: TColor; override;
    function HasData: Boolean; override;
    procedure DoPaint(AGPCanvas: TdxGPCanvas); virtual;
    procedure Offset(DX: Integer; DY: Integer); override;

    property MarkerWidth: Single read FMarkerWidth;
    property PointArray: TdxPointsF read FPointArray;
  public
    procedure Calculate(const ACategories, AValues: array of Variant;
      const ADataRange: TdxSeriesRange; AValueRange: TdxValueRange; const AStep: TdxChartPoint; const ABounds: TRect); override;
    procedure Initialize(ASeries: TdxSparklineSeries); override;
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TdxSparklineLineSeriesViewInfo = class(TdxSparklinePointSeriesViewInfo)
  strict private
    FLineWidth: Integer;
  protected
    procedure DoPaint(AGPCanvas: TdxGPCanvas); override;

    property LineWidth: Integer read FLineWidth write FLineWidth;
  public
    procedure Initialize(ASeries: TdxSparklineSeries); override;
  end;

  TdxSparklineAreaSeriesViewInfo = class(TdxSparklineLineSeriesViewInfo)
  strict private
    FPolygonPoints: TdxPointsF;
    FOpacity: Byte;
  protected
    procedure DoPaint(AGPCanvas: TdxGPCanvas); override;
    procedure Offset(DX: Integer; DY: Integer); override;

    property Opacity: Byte read FOpacity write FOpacity;
  public
    procedure Calculate(const ACategories, AValues: array of Variant;
      const ADataRange: TdxSeriesRange; AValueRange: TdxValueRange; const AStep: TdxChartPoint; const ABounds: TRect); override;
    procedure Initialize(ASeries: TdxSparklineSeries); override;
  end;

  TdxSparklineBarSeriesViewInfo = class(TdxSparklineSeriesViewInfo)
  strict private
    FBarRectArray: TdxRectsF;
    function GetBarIndex: Integer;
  protected
    function HasData: Boolean; override;
    procedure Offset(DX: Integer; DY: Integer); override;

    property BarIndex: Integer read GetBarIndex;
  public
    procedure Calculate(const ACategories, AValues: array of Variant;
      const ADataRange: TdxSeriesRange; AValueRange: TdxValueRange; const AStep: TdxChartPoint; const ABounds: TRect); override;
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  { TdxSparklineViewInfo }

  TdxSparklineViewInfo = class(TcxCustomEditViewInfo)
  strict private
    FBarSeries: TObjectList;
    FBarWidth: Integer;
    FSeries: TObjectList;
    FSmoothingMode: TdxGPSmoothingMode;

    procedure CalculateBarSeriesInfo(const AChartRange: TdxSeriesRange; const AStep: TdxChartPoint);

    function GetBarSeriesCount: Integer;
    function GetSeriesCount: Integer;
  protected
    procedure AddSeries(ASeries: TdxSparklineSeries);
    procedure CalculateSeriesViewInfo(const ACategories: TdxSparklineValues; const AValues: array of TdxSparklineValues;
      const ADataRange: TdxSeriesRange; const AStep: TdxChartPoint);
    procedure Clear; virtual;
    procedure InternalPaint(ACanvas: TcxCanvas); override;

    property BarSeries: TObjectList read FBarSeries;
    property BarSeriesCount: Integer read GetBarSeriesCount;
    property BarWidth: Integer read FBarWidth;
    property Series: TObjectList read FSeries;
    property SeriesCount: Integer read GetSeriesCount;
    property SmoothingMode: TdxGPSmoothingMode read FSmoothingMode write FSmoothingMode;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Offset(DX: Integer; DY: Integer); override;
  end;

  { TdxSparklineViewData }

  TdxSparklineViewData = class(TcxCustomEditViewData)
  strict private
    Categories: TdxSparklineValues;
    Values: array of TdxSparklineValues;
    DataRange: TdxSeriesRange;
    Step: TdxChartPoint;

    procedure CalculateRange;
    procedure CalculateStep;

    function GetDataController: TcxCustomDataController; inline;
    function GetProperties: TdxSparklineProperties; inline;
  protected
    procedure Clear;
    procedure PopulateValues(AViewInfo: TdxSparklineViewInfo; ARecords: TList); virtual;

    property DataController: TcxCustomDataController read GetDataController;
    property Properties: TdxSparklineProperties read GetProperties;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean); override;
    procedure EditValueToDrawValue(const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo); override;
    function GetClientExtent(ACanvas: TcxCanvas; AViewInfo: TcxCustomEditViewInfo): TRect; override;
    function GetEditContentSize(ACanvas: TcxCanvas; const AEditValue: TcxEditValue;
      const AEditSizeProperties: TcxEditSizeProperties; AErrorData: TcxEditValidateInfo = nil): TSize; override;
  end;

  { TdxCustomSparklineEdit }

  TdxCustomSparklineEdit = class(TcxCustomEdit)
  strict private
    function GetActiveProperties: TdxSparklineProperties;
    function GetProperties: TdxSparklineProperties;
    procedure SetProperties(AValue: TdxSparklineProperties);
  protected
    procedure Initialize; override;
    procedure KeyPress(var Key: Char); override;
    procedure PropertiesChanged(Sender: TObject); override;
  public
    function CanFocus: Boolean; override;
    procedure CopyToClipboard; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure InitializeViewData(AViewData: TcxCustomEditViewData); override;
    procedure InternalSetEditValue(const Value: TcxEditValue; AIsValueValid: Boolean); override;

    property ActiveProperties: TdxSparklineProperties read GetActiveProperties;
  published
    property Properties: TdxSparklineProperties read GetProperties  write SetProperties;
    property AutoSize default False;
  end;

  { TdxSparklineEdit }

  TdxSparklineEdit = class(TdxCustomSparklineEdit)
  protected
    function GetEditValue: TcxEditValue; override;
    //
  published
    property Anchors;
    property AutoSize;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor default False;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Math, cxVariants, Clipbrd;

const
  dxcMinBarWidth = 2;
  dxcMinBarHeight = 1;
  SeriesViewInfoTypes: array[TdxSparklineSeriesType] of TdxSparklineSeriesViewInfoClass =
    (TdxSparklineLineSeriesViewInfo, TdxSparklinePointSeriesViewInfo, TdxSparklineAreaSeriesViewInfo, TdxSparklineBarSeriesViewInfo);

var
  dxNullSeriesRange: TdxSeriesRange;
  dxNullChartPoint: TdxChartPoint;

type
  TdxSparklineDataController = class(TcxCustomDataController)
  strict private
    FSparkline: TdxSparklineProperties;
  protected
    procedure UpdateControl(AInfo: TcxUpdateControlInfo); override;
  public
    constructor Create(AOwner: TdxSparklineProperties); reintroduce; overload;
    function GetItem(Index: Integer): TObject; override;
    function GetItemID(AItem: TObject): Integer; override;
    procedure UpdateData; override;
    procedure UpdateItemIndexes; override;

    property Sparkline: TdxSparklineProperties read FSparkline;
  end;

{ TdxSparklineDataController }

constructor TdxSparklineDataController.Create(AOwner: TdxSparklineProperties);
begin
  FSparkline := AOwner;
  inherited Create(TComponent(Sparkline.Owner));
end;

function TdxSparklineDataController.GetItem(Index: Integer): TObject;
begin
  Result := Sparkline.DataBindings[Index];
end;

function TdxSparklineDataController.GetItemID(AItem: TObject): Integer;
begin
  Result := Sparkline.DataBindings.IndexOf(AItem);
end;

procedure TdxSparklineDataController.UpdateData;
begin
  Sparkline.DataChanged
end;

procedure TdxSparklineDataController.UpdateItemIndexes;
begin
  Sparkline.UpdateItemIndexes;
end;

procedure TdxSparklineDataController.UpdateControl(AInfo: TcxUpdateControlInfo);
begin
  Sparkline.UpdateControl(AInfo);
end;

{ TdxSparklineItemDataBinding }

constructor TdxSparklineItemDataBinding.Create(AOwner: TPersistent; AProperties: TdxSparklineProperties;
  ADefaultValueTypeClass: TcxValueTypeClass);
begin
  FOwner := AOwner;
  FProperties := AProperties;
  FDataIndex := -1;
  Properties.AddDataBinding(Self);
  FDefaultValueTypeClass := ADefaultValueTypeClass;
  ValueType := ADefaultValueTypeClass.Caption;
end;

destructor TdxSparklineItemDataBinding.Destroy;
begin
  Properties.RemoveDataBinding(Self);
  inherited Destroy;
end;

procedure TdxSparklineItemDataBinding.Assign(Source: TPersistent);
begin
  if Source is TdxSparklineItemDataBinding then
    FValueType := TdxSparklineItemDataBinding(Source).ValueType;
end;

function TdxSparklineItemDataBinding.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TdxSparklineItemDataBinding.Initialize;
var
  AValueType: TcxValueTypeClass;
begin
  if Properties.Owner = nil then
    Exit;
  AValueType := cxValueTypeClassList.ItemByCaption(ValueType);
  if AValueType <> nil then
    DataController.ChangeValueTypeClass(DataIndex, AValueType);
end;

function TdxSparklineItemDataBinding.IsBindingActive: Boolean;
begin
  Result := True;
end;

function TdxSparklineItemDataBinding.IsValueTypeStored: Boolean;
begin
  Result := not SameText(ValueType, DefaultValueTypeClass.Caption);
end;

function TdxSparklineItemDataBinding.GetDataController: TcxCustomDataController;
begin
  Result := Properties.DataController;
end;

function TdxSparklineItemDataBinding.GetValueType: string;
var
  AValueType: TcxValueTypeClass;
begin
  Result := FValueType;
  if (DataIndex >= 0) and (Properties.Owner <> nil)  then
  begin
    AValueType := DataController.GetItemValueTypeClass(DataIndex);
    if AValueType <> nil then
      Result := AValueType.Caption
  end;
end;

procedure TdxSparklineItemDataBinding.SetValueType(const AValue: string);
begin
  if ValueType <> AValue then
  begin
    FValueType := AValue;
    Initialize;
  end;
end;

{ TdxSparklineCategories }

constructor TdxSparklineCategories.Create(AOwner: TdxSparklineProperties);
begin
  FOwner := AOwner;
  FDataBinding := CreateDataBinding;
end;

destructor TdxSparklineCategories.Destroy;
begin
  FreeAndNil(FDataBinding);
  inherited Destroy;
end;

procedure TdxSparklineCategories.Assign(Source: TPersistent);
begin
  if Source is TdxSparklineCategories then
  begin
    DataBinding := TdxSparklineCategories(Source).DataBinding;
    MaxValue := TdxSparklineCategories(Source).MaxValue;
    MinValue := TdxSparklineCategories(Source).MinValue;
    MinMaxRangeType := TdxSparklineCategories(Source).MinMaxRangeType;
  end;
end;

function TdxSparklineCategories.CreateDataBinding: TdxSparklineItemDataBinding;
begin
  Result := Owner.GetItemsDataBinidingClass.Create(Self, Owner, TcxStringValueType);
end;

function TdxSparklineCategories.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TdxSparklineCategories.GetDataController: TcxCustomDataController;
begin
  Result := Owner.DataController;
end;

function TdxSparklineCategories.GetDataIndex: Integer;
begin
  Result := DataBinding.DataIndex;
end;

function TdxSparklineCategories.GetValue(AIndex: Integer): Variant;
begin
  if DataBinding.IsBindingActive then
    Result := DataController.Values[AIndex, DataIndex]
  else
    Result := AIndex;
end;

function TdxSparklineCategories.GetValueCount: Integer;
begin
  Result := DataController.RecordCount;
end;

function TdxSparklineCategories.IsMaxValueStored: Boolean;
begin
  Result := FMaxValue <> 0;
end;

function TdxSparklineCategories.IsMinValueStored: Boolean;
begin
  Result := FMinValue <> 0;
end;

procedure TdxSparklineCategories.SetDataBinding(AValue: TdxSparklineItemDataBinding);
begin
  DataBinding.Assign(AValue);
end;

procedure TdxSparklineCategories.SetMaxValue(AValue: Double);
begin
  if FMaxValue <> AValue then
  begin
    FMaxValue := AValue;
    FMinMaxRangeType := mmrtCustom;
    Owner.Changed;
  end;
end;

procedure TdxSparklineCategories.SetMinMaxRangeType(AValue: TdxSparklineMinMaxRangeType);
begin
  if FMinMaxRangeType <> AValue then
  begin
    FMinMaxRangeType := AValue;
    Owner.Changed;
  end;
end;

procedure TdxSparklineCategories.SetMinValue(AValue: Double);
begin
  if FMinValue <> AValue then
  begin
    FMinValue := AValue;
    FMinMaxRangeType := mmrtCustom;
    Owner.Changed;
  end;
end;

{ TdxSparklineSeries }

constructor TdxSparklineSeries.Create(Collection: TCollection);
begin
  inherited;
  FAreaOpacity := 127;
  FColor := clDefault;
  FMarkerSize := 3;
  FLineWidth := 1;
  FVisible := True;
  FEndPointColor := clNone;
  FMarkerColor := clNone;
  FMaxPointColor := clNone;
  FMinPointColor := clNone;
  FNegativePointColor := clNone;
  FStartPointColor := clNone;
end;

procedure TdxSparklineSeries.Assign(Source: TPersistent);
begin
  if Source is TdxSparklineSeries then
  begin
    DataBinding := TdxSparklineSeries(Source).DataBinding;
    AreaOpacity := TdxSparklineSeries(Source).AreaOpacity;
    Color := TdxSparklineSeries(Source).Color;
    EndPointColor := TdxSparklineSeries(Source).EndPointColor;
    LineWidth := ScaleFactor.Apply(TdxSparklineSeries(Source).LineWidth, TdxSparklineSeries(Source).ScaleFactor);
    MarkerColor := TdxSparklineSeries(Source).MarkerColor;
    MarkerColor := TdxSparklineSeries(Source).MarkerColor;
    MarkerSize := ScaleFactor.Apply(TdxSparklineSeries(Source).MarkerSize, TdxSparklineSeries(Source).ScaleFactor);
    MaxPointColor := TdxSparklineSeries(Source).MaxPointColor;
    MinPointColor := TdxSparklineSeries(Source).MinPointColor;
    NegativePointColor := TdxSparklineSeries(Source).NegativePointColor;
    SeriesType := TdxSparklineSeries(Source).SeriesType;
    StartPointColor := TdxSparklineSeries(Source).StartPointColor;
    Visible := TdxSparklineSeries(Source).Visible;
  end;
end;

procedure TdxSparklineSeries.ChangeScale(M, D: Integer);
begin
  LineWidth := MulDiv(LineWidth, M, D);
  MarkerSize := MulDiv(MarkerSize, M, D);
end;

function TdxSparklineSeries.CreateDataBinding: TdxSparklineItemDataBinding;
begin
  Result := Owner.GetItemsDataBinidingClass.Create(Self, Owner, TcxFloatValueType);
end;

procedure TdxSparklineSeries.SetCollection(Value: TCollection);
begin
  inherited SetCollection(Value);
  FreeAndNil(FDataBinding);
  if Collection <> nil then
  begin
    Owner.BeginUpdate;
    try
      FDataBinding := CreateDataBinding;
    finally
      Owner.EndUpdate;
    end;
  end;
end;

function TdxSparklineSeries.GetDataController: TcxCustomDataController;
begin
  Result := Owner.DataController;
end;

function TdxSparklineSeries.GetDataIndex: Integer;
begin
  Result := DataBinding.DataIndex;
end;

function TdxSparklineSeries.GetOwnerProperties: TdxSparklineProperties;
begin
  Result := nil;
  if Collection <> nil then
    Result := TdxSparkLineSeriesCollection(Collection).Properties;
end;

function TdxSparklineSeries.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.ScaleFactor;
end;

function TdxSparklineSeries.GetValue(AIndex: Integer): Variant;
begin
  Result := DataController.Values[AIndex, DataIndex];
end;

function TdxSparklineSeries.GetValueCount: Integer;
begin
  Result := DataController.RecordCount;
end;

procedure TdxSparklineSeries.SetAreaOpacity(AValue: Byte);
begin
  if AreaOpacity <> AValue then
  begin
    FAreaOpacity := AValue;
    Changed(False);
  end;
end;

procedure TdxSparklineSeries.SetColor(AValue: TColor);
begin
  if Color <> AValue then
  begin
    FColor := AValue;
    Changed(False);
  end;
end;

procedure TdxSparklineSeries.SetDataBinding(AValue: TdxSparklineItemDataBinding);
begin
  DataBinding.Assign(AValue);
end;

procedure TdxSparklineSeries.SetEndPointColor(AValue: TColor);
begin
  if EndPointColor <> AValue then
  begin
    FEndPointColor := AValue;
    Changed(False);
  end;
end;

procedure TdxSparklineSeries.SetMarkerColor(AValue: TColor);
begin
  if MarkerColor <> AValue then
  begin
    FMarkerColor := AValue;
    Changed(False);
  end;
end;

procedure TdxSparklineSeries.SetMarkerSize(AValue: Integer);
begin
  if MarkerSize <> AValue then
  begin
    FMarkerSize := AValue;
    Changed(False);
  end;
end;

procedure TdxSparklineSeries.SetMaxPointColor(AValue: TColor);
begin
  if MaxPointColor <> AValue then
  begin
    FMaxPointColor := AValue;
    Changed(False);
  end;
end;

procedure TdxSparklineSeries.SetMinPointColor(AValue: TColor);
begin
  if MinPointColor <> AValue then
  begin
    FMinPointColor := AValue;
    Changed(False);
  end;
end;

procedure TdxSparklineSeries.SetNegativePointColor(AValue: TColor);
begin
  if NegativePointColor <> AValue then
  begin
    FNegativePointColor := AValue;
    Changed(False);
  end;
end;

procedure TdxSparklineSeries.SetSeriesType(AValue: TdxSparklineSeriesType);
begin
  if SeriesType <> AValue then
  begin
    FSeriesType := AValue;
    Changed(True);
  end;
end;

procedure TdxSparklineSeries.SetStartPointColor(AValue: TColor);
begin
  if StartPointColor <> AValue then
  begin
    FStartPointColor := AValue;
    Changed(False);
  end;
end;

procedure TdxSparklineSeries.SetLineWidth(AValue: Integer);
begin
  if LineWidth <> AValue then
  begin
    FLineWidth := AValue;
    Changed(False);
  end;
end;

procedure TdxSparklineSeries.SetVisible(AValue: Boolean);
begin
  if Visible <> AValue then
  begin
    FVisible := AValue;
    Changed(True);
  end;
end;

{ TdxSparklineSeriesCollection }

constructor TdxSparklineSeriesCollection.Create(AProperties: TdxSparklineProperties);
begin
  inherited Create(AProperties.GetSeriesClass);
  FProperties := AProperties;
end;

function TdxSparklineSeriesCollection.Add: TdxSparklineSeries;
begin
  Result := TdxSparklineSeries(inherited Add);
end;

procedure TdxSparklineSeriesCollection.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxSparklineSeriesCollection then
  begin
    BeginUpdate;
    try
      MaxValue := TdxSparklineSeriesCollection(Source).MaxValue;
      MinValue := TdxSparklineSeriesCollection(Source).MinValue;
      MinMaxRangeType := TdxSparklineSeriesCollection(Source).MinMaxRangeType;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSparklineSeriesCollection.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Count - 1 do
      Items[I].ChangeScale(M, D);
  finally
    EndUpdate;
  end;
end;

function TdxSparklineSeriesCollection.GetOwner: TPersistent;
begin
  Result := FProperties;
end;

procedure TdxSparklineSeriesCollection.Update(Item: TCollectionItem);
begin
  inherited;
  Properties.DataChanged;
end;

function TdxSparklineSeriesCollection.GetItem(AIndex: Integer): TdxSparklineSeries;
begin
  Result := TdxSparklineSeries(inherited Items[AIndex]);
end;

function TdxSparklineSeriesCollection.IsMaxValueStored: Boolean;
begin
  Result := FMaxValue <> 0;
end;

function TdxSparklineSeriesCollection.IsMinValueStored: Boolean;
begin
  Result := FMinValue <> 0;
end;

procedure TdxSparklineSeriesCollection.SetItem(AIndex: Integer; AValue: TdxSparklineSeries);
begin
  Items[AIndex].Assign(AValue);
end;

procedure TdxSparklineSeriesCollection.SetMaxValue(AValue: Double);
begin
  if FMaxValue <> AValue then
  begin
    FMaxValue := AValue;
    FMinMaxRangeType := mmrtCustom;
    Changed;
  end;
end;

procedure TdxSparklineSeriesCollection.SetMinMaxRangeType(AValue: TdxSparklineMinMaxRangeType);
begin
  if FMinMaxRangeType <> AValue then
  begin
    FMinMaxRangeType := AValue;
    Changed;
  end;
end;

procedure TdxSparklineSeriesCollection.SetMinValue(AValue: Double);
begin
  if FMinValue <> AValue then
  begin
    FMinValue := AValue;
    FMinMaxRangeType := mmrtCustom;
    Changed;
  end;
end;

{ TdxSparklineProperties }

constructor TdxSparklineProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FDataBindings := TcxObjectList.Create;
  FDataController := CreateDataController;
  FSeries := CreateSeriesCollection;
  FCategories := CreateCategories;
  FAntialiasing := True;
  FPadding := TcxMargin.Create(Self, 2);
  FPadding.OnChange := ChangeHandler;
end;

destructor TdxSparklineProperties.Destroy;
begin
  BeginUpdate;
  FreeAndNil(FPadding);
  FreeAndNil(FSeries);
  FreeAndNil(FCategories);
  FDataBindings.Free;
  FreeAndNil(FDataController);
  inherited Destroy;
end;

procedure TdxSparklineProperties.DataChanged;
begin
  Changed;
end;

class function TdxSparklineProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TdxSparklineEdit;
end;

class function TdxSparklineProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxSparkLineViewInfo;
end;

function TdxSparklineProperties.GetDataValue: Variant;
var
  ADataStream: TMemoryStream;
begin
  Result := Null;
  if DataController.RecordCount = 0 then
    Exit;
  ADataStream := TMemoryStream.Create;
  try
    DataController.SaveToStream(ADataStream);
    Result := dxStreamToVariant(ADataStream);
  finally
    ADataStream.Free;
  end;
end;

procedure TdxSparklineProperties.SetAntialiasing(AValue: Boolean);
begin
  if AValue <> Antialiasing then
  begin
    FAntialiasing := AValue;
    Changed;
  end;
end;

procedure TdxSparklineProperties.SetCategories(AValue: TdxSparklineCategories);
begin
  Categories.Assign(AValue);
end;

procedure TdxSparklineProperties.SetPadding(AValue: TcxMargin);
begin
  Padding.Assign(AValue);
  Changed;
end;

procedure TdxSparklineProperties.SetSeries(AValue: TdxSparklineSeriesCollection);
begin
  Series.Assign(AValue);
end;

procedure TdxSparklineProperties.AddDataBinding(ADataBinding: TdxSparklineItemDataBinding);
begin
  DataBindings.Add(ADataBinding);
  if Owner <> nil then
    DataController.AddItem(ADataBinding);
  UpdateItemIndexes;
end;

procedure TdxSparklineProperties.RemoveDataBinding(ADataBinding: TdxSparklineItemDataBinding);
begin
  DataBindings.Remove(ADataBinding);
  if Owner <> nil then
    DataController.RemoveItem(ADataBinding);
  UpdateItemIndexes;
end;

procedure TdxSparklineProperties.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  Padding.ChangeScale(M, D);
  Series.ChangeScale(M, D);
end;

function TdxSparklineProperties.CreateCategories: TdxSparklineCategories;
begin
  Result := TdxSparklineCategories.Create(Self);
end;

function TdxSparklineProperties.CreateDataController: TcxCustomDataController;
begin
  Result := TdxSparklineDataController.Create(Self)
end;

function TdxSparklineProperties.CreateSeriesCollection: TdxSparklineSeriesCollection;
begin
  Result := TdxSparklineSeriesCollection.Create(Self);
end;

procedure TdxSparklineProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited DoAssign(AProperties);
  if AProperties is TdxSparklineProperties then
  begin
    FAntialiasing := TdxSparklineProperties(AProperties).Antialiasing;
    Categories := TdxSparklineProperties(AProperties).Categories;
    Series := TdxSparklineProperties(AProperties).Series;
    Padding := TdxSparklineProperties(AProperties).Padding;
    Padding.ChangeScale(ScaleFactor.Numerator, TdxSparklineProperties(AProperties).ScaleFactor.Numerator);
  end;
end;

function TdxSparklineProperties.GetItemsDataBinidingClass: TdxSparklineItemDataBindingClass;
begin
  Result := TdxSparklineItemDataBinding;
end;

function TdxSparklineProperties.GetSeriesClass: TdxSparklineSeriesClass;
begin
  Result := TdxSparklineSeries;
end;

class function TdxSparklineProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TdxSparklineViewData;
end;

function TdxSparklineProperties.IsActive: Boolean;
begin
  Result := (Series <> nil) and (Series.Count > 0);
end;

procedure TdxSparklineProperties.LoadRecords(const AEditValue: TcxEditValue; AList: TList);
var
  AData: AnsiString;
  AStream: TMemoryStream;
  I: TdxNativeInt;
begin
  AList.Clear;
  AStream := TMemoryStream.Create;
  try
    DataController.RecordCount := 0;
    AData := dxVariantToAnsiString(AEditValue);
    if Length(AData) > 0 then
    begin
      AStream.WriteBuffer(AData[1], Length(AData));
      AStream.Position := 0;
      try
        DataController.LoadFromStream(AStream);
      except
        on EReadError do;
        on EcxInvalidDataControllerOperation do;
      end;
    end;
    AList.Count := DataController.RecordCount;
    for I := 0 to DataController.RecordCount - 1 do
      AList.List[I] := Pointer(I);
  finally
    AStream.Free;
  end;
end;

procedure TdxSparklineProperties.UpdateControl(AInfo: TcxUpdateControlInfo);
begin
  if (AInfo is TcxDataChangedInfo) or (AInfo is TcxLayoutChangedInfo) or
    (AInfo is TcxUpdateRecordInfo) or (AInfo is TcxFindFilterChangedInfo) then
      DataChanged;
end;

procedure TdxSparklineProperties.UpdateItemIndexes;
var
  I: Integer;
begin
  for I := 0 to DataBindings.Count - 1 do
    TdxSparklineItemDataBinding(DataBindings[I]).DataIndex := I;
  DataChanged;
end;

procedure TdxSparklineProperties.UpdateItems;
begin
  DataChanged;
end;

{ TdxSparklineViewInfo }

function dxSparkLineDiagramScale(const ADiagramRange: TdxSeriesRange; const ACanvasRect: TRect): TdxPointF;
var
  AWidth, AHeigth: Single;
begin
  AWidth := cxRectWidth(ACanvasRect);
  AHeigth := cxRectHeight(ACanvasRect);
  if (AWidth > 0) and (AHeigth > 0) then
  begin
    Result.X := (ADiagramRange.X.Max - ADiagramRange.X.Min) / AWidth;
    Result.Y := (ADiagramRange.Y.Max - ADiagramRange.Y.Min) / AHeigth;
  end
  else
    Result := dxPointF(0, 0);
end;

function dxSparkLineDiagramPointToCanvasPoint(const ADiagramRange: TdxSeriesRange; const ACanvasRect: TRect;
  const ADiagramPoint: TdxPointF): TdxPointF;
var
  AScale: TdxPointF;
begin
  AScale := dxSparkLineDiagramScale(ADiagramRange, ACanvasRect);

  if AScale.X <> 0 then
    Result.X := ACanvasRect.Left + (ADiagramPoint.X - ADiagramRange.X.Min) / AScale.X
  else
    Result.X := -1;
  if AScale.Y <> 0 then
    Result.Y := ACanvasRect.Bottom - (ADiagramPoint.Y - ADiagramRange.Y.Min) / AScale.Y
  else
    Result.Y := -1;
end;

function dxSparkLineValueToFloat(AValues: array of Variant; AIndex: Integer): Single;
begin
  if VarIsNumeric(AValues[AIndex]) then
    Result := AValues[AIndex]
  else
    Result := AIndex;
end;

function GetValueRange(const AValues: array of Variant; const AMin, AMax: Double;
  ARangeType: TdxSparklineMinMaxRangeType): TdxValueRange; overload;
var
  I: Integer;
  AValue: Single;
begin
  if ARangeType = mmrtCustom then
  begin
    Result.Min := AMin;
    Result.Max := AMax;
  end
  else
  begin
    Result.Max := dxSparkLineValueToFloat(AValues, 0);
    if ARangeType = mmrtZeroBasedAuto then
      Result.Min := 0
    else
      Result.Min := Result.Max;
    for I := 1 to Length(AValues) - 1 do
    begin
      AValue := dxSparkLineValueToFloat(AValues, I);
      Result.Min := Min(Result.Min, AValue);
      Result.Max := Max(Result.Max, AValue);
    end;
  end;
end;

function GetValueRange(const AValues: array of Variant): TdxValueRange; overload;
begin
  Result := GetValueRange(AValues, 0, 0, mmrtAuto);
end;

function GetAxisPoint(const ADiagramRange: TdxSeriesRange): Single;
begin
  if (ADiagramRange.Value.Min < 0) and (ADiagramRange.Value.Max > 0) then
    Result := 0
  else
    if ADiagramRange.Value.Min < 0 then
      Result := ADiagramRange.Value.Max
    else
      Result := ADiagramRange.Value.Min;
end;

function dxGetDefaultValueColor(AIndex: Integer): TColor;
const
  ColorCount = 24;
  Colors: array[0..ColorCount - 1] of TColor =
    ($0204FF, $B4835C, $7C58A5, $657C6C, $6379E6, $9AA05B, $605DCF, $6A8846,
     $61A3F5, $58999E, $5A8CFF, $AD977A, $808E54, $95C9B9, $6763A5, $AC8C4D,
     $80E4FB, $956349, $4D50C0, $67B48B, $D6A584, $73D8DD, $89674D, $9CB5A5);
begin
  Result := Colors[AIndex mod ColorCount];
end;

constructor TdxSparklineSeriesViewInfo.Create(AOwner: TdxSparklineViewInfo);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TdxSparklineSeriesViewInfo.Calculate(const ACategories, AValues: array of Variant;
  const ADataRange: TdxSeriesRange; AValueRange: TdxValueRange; const AStep: TdxChartPoint; const ABounds: TRect);

  function GetValueColor(AIndex: Integer): TColor;
  begin
    if (FStartPointColor <> clNone) and (AIndex = 0) then
     Result := FStartPointColor
    else
      if (FEndPointColor <> clNone) and (AIndex = Length(AValues) - 1) then
        Result := FEndPointColor
      else
        if (FMinPointColor <> clNone) and (AValues[AIndex] = AValueRange.Min) then
          Result := FMinPointColor
        else
          if (FMaxPointColor <> clNone) and (AValues[AIndex] = AValueRange.Max) then
            Result := FMaxPointColor
          else
            if (FNegativePointColor <> clNone) and (AValues[AIndex] < 0) then
              Result := FNegativePointColor
            else
              Result := GetPointColor;
  end;

var
  I: Integer;
begin
  dxTestCheck(Length(AValues) = Length(ACategories), 'TdxSparklineSeriesViewInfo.Calculate fails');

  SetLength(FPointColors, Length(AValues));
  for I := 0 to Length(PointColors) - 1 do
    PointColors[I] := GetValueColor(I);
end;

procedure TdxSparklineSeriesViewInfo.Initialize(ASeries: TdxSparklineSeries);
begin
  FColor := GetColorForDrawing(ASeries.Color);
  FEndPointColor := GetColorForDrawing(ASeries.EndPointColor, 50);
  FMaxPointColor := GetColorForDrawing(ASeries.MaxPointColor, 70);
  FMinPointColor := GetColorForDrawing(ASeries.MinPointColor, -70);
  FNegativePointColor := GetColorForDrawing(ASeries.NegativePointColor, 30);
  FStartPointColor := GetColorForDrawing(ASeries.StartPointColor, -50);
end;

function TdxSparklineSeriesViewInfo.GetColorForDrawing(AColor: TColor; ATint: Integer): TColor;
begin
  if AColor = clDefault then
    Result := dxGetColorTint(dxGetDefaultValueColor(GetIndex), ATint)
  else
    Result := AColor;
end;

function TdxSparklineSeriesViewInfo.GetPointColor: TColor;
begin
  Result := Color;
end;

procedure TdxSparklineSeriesViewInfo.Offset(DX: Integer; DY: Integer);
begin
// do nothing
end;

function TdxSparklineSeriesViewInfo.GetIndex: Integer;
begin
  Result := Owner.Series.IndexOf(Self);
end;

{ TdxSparklinePointSeriesViewInfo }

procedure TdxSparklinePointSeriesViewInfo.Calculate(const ACategories, AValues: array of Variant;
  const ADataRange: TdxSeriesRange; AValueRange: TdxValueRange; const AStep: TdxChartPoint; const ABounds: TRect);
var
  I: Integer;
begin
  inherited;

  SetLength(FPointArray, Length(AValues));
  for I := 0 to Length(FPointArray) - 1 do
    FPointArray[I] := dxSparklineDiagramPointToCanvasPoint(ADataRange, ABounds,
      dxPointF(dxSparkLineValueToFloat(ACategories, I), dxSparkLineValueToFloat(AValues, I)));
end;

procedure TdxSparklinePointSeriesViewInfo.Initialize(ASeries: TdxSparklineSeries);
begin
  inherited;
  FMarkerColor := GetColorForDrawing(ASeries.MarkerColor);
  FMarkerWidth := Owner.ScaleFactor.Apply(ASeries.MarkerSize, ASeries.Owner.ScaleFactor) / 2;
end;

procedure TdxSparklinePointSeriesViewInfo.Paint(ACanvas: TcxCanvas);
begin
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, Owner.Bounds);
  try
    dxGPPaintCanvas.SmoothingMode := Owner.SmoothingMode;
    DoPaint(dxGPPaintCanvas);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

function TdxSparklinePointSeriesViewInfo.GetPointColor: TColor;
begin
  Result := FMarkerColor;
end;

function TdxSparklinePointSeriesViewInfo.HasData: Boolean;
begin
  Result := Length(PointArray) > 0;
end;

procedure TdxSparklinePointSeriesViewInfo.DoPaint(AGPCanvas: TdxGPCanvas);
var
  AMarkerRect: TdxRectF;
  I: Integer;
begin
  for I := 0 to Length(FPointArray) - 1 do
    if PointColors[I] <> clNone then
    begin
      AMarkerRect := dxRectF(
        FPointArray[I].X - FMarkerWidth, FPointArray[I].Y - FMarkerWidth,
        FPointArray[I].X + FMarkerWidth, FPointArray[I].Y + FMarkerWidth);
      AGPCanvas.Ellipse(AMarkerRect, dxColorToAlphaColor(PointColors[I]), dxColorToAlphaColor(PointColors[I]));
    end;
end;

procedure TdxSparklinePointSeriesViewInfo.Offset(DX: Integer; DY: Integer);
begin
  inherited;
  cxPointsOffset(FPointArray, cxPointF(DX, DY));
end;

{ TdxSparklineSeriesViewInfo }

procedure TdxSparklineLineSeriesViewInfo.Initialize(ASeries: TdxSparklineSeries);
begin
  inherited;
  LineWidth := Owner.ScaleFactor.Apply(ASeries.LineWidth, ASeries.Owner.ScaleFactor);
end;

procedure TdxSparklineLineSeriesViewInfo.DoPaint(AGPCanvas: TdxGPCanvas);
begin
  if Length(PointArray) > 1 then
    AGPCanvas.Polyline(PointArray, Color, LineWidth, psSolid, 255);
  inherited;
end;

{ TdxSparklineAreaSeriesViewInfo }

procedure TdxSparklineAreaSeriesViewInfo.Calculate(const ACategories, AValues: array of Variant;
  const ADataRange: TdxSeriesRange; AValueRange: TdxValueRange; const AStep: TdxChartPoint; const ABounds: TRect);
var
  I: Integer;
  AAxisPoint: Single;
begin
  inherited;

  AAxisPoint := GetAxisPoint(ADataRange);
  SetLength(FPolygonPoints, Length(PointArray) * 2);
  for I := 0 to Length(PointArray) - 1 do
  begin
    FPolygonPoints[I] := PointArray[I];
    FPolygonPoints[Length(FPolygonPoints)-1-I].X := PointArray[I].X;
    FPolygonPoints[Length(FPolygonPoints)-1-I].Y := dxSparklineDiagramPointToCanvasPoint(ADataRange, ABounds, dxPointF(0, AAxisPoint)).Y;
  end;
end;

procedure TdxSparklineAreaSeriesViewInfo.Initialize(ASeries: TdxSparklineSeries);
begin
  inherited;
  Opacity := ASeries.AreaOpacity;
end;

procedure TdxSparklineAreaSeriesViewInfo.DoPaint(AGPCanvas: TdxGPCanvas);
begin
  if Length(FPolygonPoints) > 3 then
    AGPCanvas.Polygon(FPolygonPoints, Color, Color, 0, psSolid, 0, Opacity);
  inherited;
end;

procedure TdxSparklineAreaSeriesViewInfo.Offset(DX: Integer; DY: Integer);
begin
  inherited;
  cxPointsOffset(FPolygonPoints, cxPointF(DX, DY));
 end;

{ TdxSparklineBarSeriesViewInfo }

procedure TdxSparklineBarSeriesViewInfo.Calculate(const ACategories, AValues: array of Variant;
  const ADataRange: TdxSeriesRange; AValueRange: TdxValueRange; const AStep: TdxChartPoint; const ABounds: TRect);

  function RoundPoint(const P: TdxPointF): TdxPointF;
  begin
    Result.X := Round(P.X);
    Result.Y := Round(P.Y);
  end;

  function GetBarRect(const P1, P2: TdxPointF): TdxRectF;
  var
    AWidth: Single;
  begin
    Result.Bottom := P2.Y;
    Result.Top := Min(P1.Y, P2.Y - dxcMinBarHeight);
    AWidth := Owner.BarWidth;
    Result.Left := P1.X - Round(AWidth * Owner.BarSeriesCount / 2) + AWidth * BarIndex;
    Result.Right := Result.Left + AWidth;
  end;

var
  I: Integer;
  P1, P2: TdxPointF;
  APrevBarRect: TdxRectF;
  AAxisPoint: Single;
begin
  inherited;

  AAxisPoint := GetAxisPoint(ADataRange);
  SetLength(FBarRectArray, Length(AValues));
  for I := 0 to Length(FBarRectArray) - 1 do
  begin
    P1 := dxSparklineDiagramPointToCanvasPoint(ADataRange, ABounds,
      dxPointF(dxSparkLineValueToFloat(ACategories, I), dxSparkLineValueToFloat(AValues, I)));
    P2 := dxSparklineDiagramPointToCanvasPoint(ADataRange, ABounds,
      dxPointF(dxSparkLineValueToFloat(ACategories, I), AAxisPoint));
    if P1.Y >  P2.Y then
      FBarRectArray[I] := GetBarRect(RoundPoint(P2), RoundPoint(P1))
    else
      FBarRectArray[I] := GetBarRect(RoundPoint(P1), RoundPoint(P2));
    if Owner.BarSeriesCount > 1 then
    begin
      if (I > 0) and (FBarRectArray[I].Left <= APrevBarRect.Left + Owner.BarWidth * Owner.BarSeriesCount + 1) then
        FBarRectArray[I] := dxNullRectF
      else
        APrevBarRect := FBarRectArray[I];
    end;
  end;
end;

procedure TdxSparklineBarSeriesViewInfo.Paint(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, Owner.Bounds);
  try
    for I := 0 to Length(FBarRectArray) - 1 do
      dxGPPaintCanvas.FillRectangle(FBarRectArray[I], dxColorToAlphaColor(PointColors[I]));
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

function TdxSparklineBarSeriesViewInfo.HasData: Boolean;
begin
  Result := Length(FBarRectArray) > 0;
end;

procedure TdxSparklineBarSeriesViewInfo.Offset(DX: Integer; DY: Integer);
begin
  inherited;
  cxRectsOffset(FBarRectArray, cxPointF(DX, DY));
end;

function TdxSparklineBarSeriesViewInfo.GetBarIndex: Integer;
begin
  Result := Owner.BarSeries.IndexOf(Self);
end;

{ TdxSparklineViewInfo }

constructor TdxSparklineViewInfo.Create;
begin
  inherited;
  FSeries := TObjectList.Create;
  FBarSeries := TObjectList.Create(False);
end;

destructor TdxSparklineViewInfo.Destroy;
begin
  FreeAndNil(FBarSeries);
  FreeAndNil(FSeries);
  inherited;
end;

procedure TdxSparklineViewInfo.Offset(DX: Integer; DY: Integer);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FSeries.Count - 1 do
    TdxSparklineSeriesViewInfo(FSeries[I]).Offset(DX, DY);
end;

procedure TdxSparklineViewInfo.Clear;
begin
  FSeries.Clear;
  FBarSeries.Clear;
end;

procedure TdxSparklineViewInfo.AddSeries(ASeries: TdxSparklineSeries);
var
  ASeriesViewInfo: TdxSparklineSeriesViewInfo;
begin
  ASeriesViewInfo := SeriesViewInfoTypes[ASeries.SeriesType].Create(Self);
  FSeries.Add(ASeriesViewInfo);
  if ASeries.SeriesType = stBar then
    FBarSeries.Add(ASeriesViewInfo);
  ASeriesViewInfo.Initialize(ASeries);
end;

procedure TdxSparklineViewInfo.CalculateSeriesViewInfo(const ACategories: TdxSparklineValues;
  const AValues: array of TdxSparklineValues; const ADataRange: TdxSeriesRange; const AStep: TdxChartPoint);

  function GetChartRect(ASeries: TdxSparklineSeriesViewInfo): TRect;
  var
    AMargin: Integer;
    AMargins: TRect;
  begin
    if ASeries is TdxSparklinePointSeriesViewInfo then
    begin
      AMargin := Round(TdxSparklinePointSeriesViewInfo(ASeries).MarkerWidth);
      AMargins := Rect(AMargin, AMargin, AMargin, AMargin);
    end
    else
    begin
      AMargin := BarWidth * BarSeriesCount div 2;
      AMargins := Rect(AMargin, 1, AMargin, 1);
    end;

    Result := cxRectContent(ClientRect, AMargins);
  end;

var
  I: Integer;
  AValueRange: TdxValueRange;
begin
  CalculateBarSeriesInfo(ADataRange, AStep);

  for I := 0 to FSeries.Count - 1 do
  begin
    AValueRange := GetValueRange(AValues[I]);
    TdxSparklineSeriesViewInfo(FSeries[I]).Calculate(ACategories, AValues[I], ADataRange, AValueRange, AStep, GetChartRect(TdxSparklineSeriesViewInfo(FSeries[I])));
  end;
end;

procedure TdxSparklineViewInfo.InternalPaint(ACanvas: TcxCanvas);
var
  I: Integer;
  AHasData: Boolean;
begin
  inherited;

  AHasData := False;
  for I := 0 to FSeries.Count - 1 do
    if TdxSparklineSeriesViewInfo(FSeries[I]).HasData then
    begin
      TdxSparklineSeriesViewInfo(FSeries[I]).Paint(ACanvas);
      AHasData := True;
    end;

  if not AHasData then
  begin
    ACanvas.Font := Font;
    cxDrawText(ACanvas, cxGetResourceString(@sdxSparklineNoData), ClientRect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
  end;
end;

procedure TdxSparklineViewInfo.CalculateBarSeriesInfo(const AChartRange: TdxSeriesRange; const AStep: TdxChartPoint);
var
  AChartWidth: Integer;
  ABarCount: Integer;
begin
  FBarWidth := 0;
  if BarSeriesCount = 0 then Exit;

  AChartWidth := cxRectWidth(ClientRect);
  ABarCount := Round((AChartRange.Category.Max - AChartRange.Category.Min) / AStep.Category) + 1;
  FBarWidth := Trunc(3 * AChartWidth / (3 * BarSeriesCount * ABarCount + 2 * ABarCount - 2));
  if Odd(FBarWidth) then
    Dec(FBarWidth);
  FBarWidth := Max(dxcMinBarWidth, FBarWidth);
end;

function TdxSparklineViewInfo.GetBarSeriesCount: Integer;
begin
  Result := FBarSeries.Count;
end;

function TdxSparklineViewInfo.GetSeriesCount: Integer;
begin
  Result := FSeries.Count;
end;

{ TdxSparklineViewData }

procedure TdxSparklineViewData.Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
  Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
const
  SmoothingModeMap: array [Boolean] of TdxGPSmoothingMode = (smDefault, smAntiAlias);
begin
  inherited;
  CalculateRange;
  CalculateStep;

  (AViewInfo as TdxSparklineViewInfo).CalculateSeriesViewInfo(Categories, Values, DataRange, Step);
  (AViewInfo as TdxSparklineViewInfo).SmoothingMode := SmoothingModeMap[Properties.Antialiasing];
end;

procedure TdxSparklineViewData.EditValueToDrawValue(const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
var
  AList: TList;
begin
  Clear;
  TdxSparklineViewInfo(AViewInfo).Clear;
  Properties.LockUpdate(True);
  try
    if not Properties.IsActive then
      Exit;
    AList  := TList.Create;
    try
      Properties.LoadRecords(AEditValue, AList);
      PopulateValues(TdxSparklineViewInfo(AViewInfo), AList);
    finally
      AList.Free;
    end;
  finally
    Properties.LockUpdate(False);
  end;
end;

function TdxSparklineViewData.GetClientExtent(ACanvas: TcxCanvas; AViewInfo: TcxCustomEditViewInfo): TRect;
var
  AMargin: TRect;
begin
  Result := inherited GetClientExtent(ACanvas, AViewInfo);
  AMargin := ScaleFactor.Apply(Properties.Padding.Margin, Properties.ScaleFactor);
  Inc(Result.Top, AMargin.Top);
  Inc(Result.Left, AMargin.Left);
  Inc(Result.Right, AMargin.Right);
  Inc(Result.Bottom, AMargin.Bottom);
end;

function TdxSparklineViewData.GetEditContentSize(ACanvas: TcxCanvas; const AEditValue: TcxEditValue;
  const AEditSizeProperties: TcxEditSizeProperties; AErrorData: TcxEditValidateInfo): TSize;
begin
  Result := inherited GetEditContentSize(ACanvas, AEditValue, AEditSizeProperties, AErrorData);
  Result.cx := 100;
  Result.cy := cxTextHeight(ACanvas.Handle) + GetEditContentSizeCorrection.cy;
end;

procedure TdxSparklineViewData.Clear;
begin
  SetLength(Categories, 0);
  SetLength(Values, 0);
end;

procedure TdxSparklineViewData.PopulateValues(AViewInfo: TdxSparklineViewInfo; ARecords: TList);
var
  AIndex, ARecordIndex: Integer;
  AVisibleSeries: TList;
begin
  if ARecords.Count = 0 then
    Exit;

  SetLength(Categories, ARecords.Count);

  AVisibleSeries := TList.Create;
  try
    for AIndex := 0 to Properties.Series.Count - 1 do
      if Properties.Series[AIndex].Visible then
        AVisibleSeries.Add(Properties.Series[AIndex]);

    SetLength(Values, AVisibleSeries.Count);

    AViewInfo.Series.Clear;
    for AIndex := 0 to AVisibleSeries.Count - 1 do
    begin
      SetLength(Values[AIndex], ARecords.Count);
      AViewInfo.AddSeries(TdxSparklineSeries(AVisibleSeries[AIndex]));
    end;
    for ARecordIndex := 0 to ARecords.Count - 1 do
    begin
      Categories[ARecordIndex] := Properties.Categories.Values[Integer(ARecords[ARecordIndex])];
      for AIndex := 0 to AVisibleSeries.Count - 1 do
        Values[AIndex, ARecordIndex] := TdxSparklineSeries(AVisibleSeries[AIndex]).Values[Integer(ARecords[ARecordIndex])];
    end;
  finally
    AVisibleSeries.Free;
  end;
end;

procedure TdxSparklineViewData.CalculateRange;

  procedure ValidateRange(var AVaueRange: TdxValueRange);
  begin
    if AVaueRange.Min = AVaueRange.Max then
    begin
      AVaueRange.Min := AVaueRange.Min - 1;
      AVaueRange.Max := AVaueRange.Max + 1;
    end;
  end;

var
  ARange: TdxValueRange;
  I: Integer;
begin
  if (Length(Categories) = 0) or (Length(Values) = 0) then
    DataRange := dxNullSeriesRange
  else
  begin
    DataRange.Category := GetValueRange(Categories, Properties.Categories.MinValue,
      Properties.Categories.MaxValue, Properties.Categories.MinMaxRangeType);
    DataRange.Value := GetValueRange(Values[0], Properties.Series.MinValue,
      Properties.Series.MaxValue, Properties.Series.MinMaxRangeType);
    for I := 1 to Length(Values) - 1 do
    begin
      ARange := GetValueRange(Values[I], Properties.Series.MinValue,
        Properties.Series.MaxValue, Properties.Series.MinMaxRangeType);
      DataRange.Value.Min := Min(DataRange.Value.Min, ARange.Min);
      DataRange.Value.Max := Max(DataRange.Value.Max, ARange.Max);
    end;
    ValidateRange(DataRange.Category);
    ValidateRange(DataRange.Value);
  end;
end;

procedure TdxSparklineViewData.CalculateStep;
var
  I, AValuesCount: Integer;
begin
  if (Length(Categories) = 0) or (Length(Values) = 0) then
    Step := dxNullChartPoint
  else
  begin
    Step.X := DataRange.Category.Max - DataRange.Category.Min;
    for I := 0 to Length(Categories) - 1 do
      Step.X := Min(Step.X, Abs(dxSparkLineValueToFloat(Categories, I) - dxSparkLineValueToFloat(Categories, I-1)));
    if Step.X = 0 then
      Step.X := 1;

    AValuesCount := Length(Values[0]);
    for I := 1 to Length(Values) - 1 do
      AValuesCount := Max(AValuesCount, Length(Values[I]));
    Step.Y := (DataRange.Value.Max - DataRange.Value.Min) / AValuesCount;
    if Step.Y = 0 then
      Step.Y := 1;
  end;
end;

function TdxSparklineViewData.GetDataController: TcxCustomDataController;
begin
  Result := Properties.DataController;
end;

function TdxSparklineViewData.GetProperties: TdxSparklineProperties;
begin
  Result := TdxSparklineProperties(FProperties);
end;

{ TdxCustomSparklineEdit }

function TdxCustomSparklineEdit.CanFocus: Boolean;
begin
  Result := IsInplace;
end;

procedure TdxCustomSparklineEdit.CopyToClipboard;
var
  ABitmap: TcxBitmap;
begin
  ABitmap := TcxBitmap.CreateSize(ClientBounds);
  try
    PaintTo(ABitmap.Canvas.Handle, 0, 0);
    Clipboard.Assign(ABitmap);
  finally
    ABitmap.Free;
  end;
end;

class function TdxCustomSparklineEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxSparklineProperties;
end;

procedure TdxCustomSparklineEdit.InitializeViewData(AViewData: TcxCustomEditViewData);
begin
  inherited InitializeViewData(AViewData);
  AViewData.EditValueToDrawValue(FEditValue, ViewInfo);
end;

procedure TdxCustomSparklineEdit.InternalSetEditValue(const Value: TcxEditValue; AIsValueValid: Boolean);
begin
  inherited InternalSetEditValue(Value, AIsValueValid);
  ShortRefreshContainer(False);
end;

procedure TdxCustomSparklineEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  BeginUserAction;
  try
    if Key = ^C then
      CopyToClipBoard;
  finally
    EndUserAction;
  end;
end;

procedure TdxCustomSparklineEdit.PropertiesChanged(Sender: TObject);
begin
  EditValue := GetEditValue;
  inherited PropertiesChanged(Sender);
end;

procedure TdxCustomSparklineEdit.Initialize;
begin
  inherited Initialize;
  AutoSize := False;
  Width := 121;
  Height := 21 * 2;
end;

function TdxCustomSparklineEdit.GetActiveProperties: TdxSparklineProperties;
begin
  Result := inherited ActiveProperties as TdxSparklineProperties;
end;

function TdxCustomSparklineEdit.GetProperties: TdxSparklineProperties;
begin
  Result := inherited Properties as TdxSparklineProperties;
end;

procedure TdxCustomSparklineEdit.SetProperties(AValue: TdxSparklineProperties);
begin
  Properties.Assign(AValue);
end;

{ TdxSparklineEdit }

function TdxSparklineEdit.GetEditValue: TcxEditValue;
var
  AStream: TMemoryStream;
begin
  Result := inherited GetEditValue;
  if IsLoading or IsDestroying then
    Exit;
  AStream := TMemoryStream.Create;
  try
    Properties.DataController.SaveToStream(AStream);
    Result := dxStreamToVariant(AStream);
  finally
    AStream.Free;
  end;
end;

initialization
  RegisterClasses([TdxSparklineItemDataBinding, TdxSparklineSeries, TdxSparklineCategories, TdxSparklineProperties]);
  GetRegisteredEditProperties.Register(TdxSparklineProperties, scxSEditRepositorySparklineItem);

end.


