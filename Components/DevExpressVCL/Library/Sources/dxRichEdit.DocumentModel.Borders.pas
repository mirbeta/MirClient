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

unit dxRichEdit.DocumentModel.Borders;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes, Types, Graphics, SysUtils, Generics.Defaults, Generics.Collections, Contnrs,
  dxCoreClasses, dxCoreGraphics,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.PatternLine,
  dxRichEdit.DocumentModel.UnitConverter;

type
  TdxBorderChangeType = (
    None = 0,
    Style,
    Color,
    Width,
    Offset,
    Frame,
    Shadow,
    BatchUpdate
  );

  TdxProperties = (
    TopMargin,
    LeftMargin,
    RightMargin,
    BottomMargin,
    CellSpacing,
    PreferredWidth,
    TableIndent,
    TableLayout,
    TableLook,
    TableStyleColumnBandSize,
    TableStyleRowBandSize,
    IsTableOverlap,
    AvoidDoubleBorders,
    LeftBorder,
    RightBorder,
    TopBorder,
    BottomBorder,
    InsideHorizontalBorder,
    InsideVerticalBorder,
    TopLeftDiagonalBorder,
    TopRightDiagonalBorder,
    HideCellMark,
    NoWrap,
    FitText,
    TextDirection,
    VerticalAlignment,
    ColumnSpan,
    VerticalMerging,
    CellConditionalFormatting,
    RowHeight,
    WidthBefore,
    WidthAfter,
    Header,
    TableRowAlignment,
    TableRowConditionalFormatting,
    GridAfter,
    GridBefore,
    CantSplit,
    TableFloatingPosition,
    BackgroundColor,
    ForegroundColor,
    Shading,
    CellGeneralSettings,
    Borders,
    TableAlignment
  );

  { IdxPropertiesContainer }

  IdxPropertiesContainer = interface
  ['{37AD9DE4-820F-4EBD-8BD1-142F885D9116}']
    procedure BeginChanging(AChangedProperty: TdxProperties);
    procedure EndChanging;
    procedure BeginPropertiesUpdate;
    procedure EndPropertiesUpdate;
    procedure RaiseObtainAffectedRange(const AArgs: TdxObtainAffectedRangeEventArgs);
    procedure ResetPropertyUse(AProperty: TdxProperties);
    procedure ApplyChanges(const AChangeActions: TdxDocumentModelChangeActions);
  end;

  { IdxCellMarginsContainer }

  IdxCellMarginsContainer = interface(IdxPropertiesContainer)
  ['{F842C839-4EE1-4ABD-99FA-EB1ED07D8F8A}']
    function GetUseLeftMargin: Boolean;
    function GetUseRightMargin: Boolean;
    function GetUseTopMargin: Boolean;
    function GetUseBottomMargin: Boolean;

    property UseLeftMargin: Boolean read GetUseLeftMargin;
    property UseRightMargin: Boolean read GetUseRightMargin;
    property UseTopMargin: Boolean read GetUseTopMargin;
    property UseBottomMargin: Boolean read GetUseBottomMargin;
  end;

  { TdxBorderInfo }

  TdxBorderInfo = class(TdxCloneable)
  strict private
    class var FEmpty: TdxBorderInfo;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FStyle: TdxBorderLineStyle;
    FColor: TdxAlphaColor;
    FWidth: Integer;
    FOffset: Integer;
    FFrame: Boolean;
    FShadow: Boolean;
  public
    constructor Create; override;
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Clone: TdxBorderInfo; reintroduce; inline;
    class function AreBordersEqual(const A, B: TdxBorderInfo): Boolean;

    property Style: TdxBorderLineStyle read FStyle write FStyle;
    property Color: TdxAlphaColor read FColor write FColor;
    property Width: Integer read FWidth write FWidth;
    property Offset: Integer read FOffset write FOffset;
    property Frame: Boolean read FFrame write FFrame;
    property Shadow: Boolean read FShadow write FShadow;

    class property Empty: TdxBorderInfo read FEmpty;
  end;
  TdxBorderInfoList = class(TdxList<TdxBorderInfo>);

  { TdxBorderInfoCache }

  TdxBorderInfoCache = class(TdxUniqueItemsCache<TdxBorderInfo>)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxBorderInfo; override;
  end;

  { TdxBorderChangeActionsCalculator }

  TdxBorderChangeActionsCalculator = class
  public
    class function CalculateChangeActions(AChange: TdxBorderChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxBorderBase }

  TdxBorderBase = class abstract(TdxRichEditIndexBasedObject<TdxBorderInfo>)
  private
    FOwner: IdxPropertiesContainer;
    function GetColor: TdxAlphaColor;
    function GetFrame: Boolean;
    function GetOffset: Integer;
    function GetShadow: Boolean;
    function GetStyle: TdxBorderLineStyle;
    function GetWidth: Integer;
    procedure SetColor(const Value: TdxAlphaColor);
    procedure SetFrame(const Value: Boolean);
    procedure SetOffset(const Value: Integer);
    procedure SetShadow(const Value: Boolean);
    procedure SetStyle(const Value: TdxBorderLineStyle);
    procedure SetWidth(const Value: Integer);
  strict protected
    function SetStyleCore(const AInfo: TdxBorderInfo; const AValue: Integer): TdxDocumentModelChangeActions;
    function SetColorCore(const AInfo: TdxBorderInfo; const AValue: Integer): TdxDocumentModelChangeActions;
    function SetWidthCore(const AInfo: TdxBorderInfo; const AValue: Integer): TdxDocumentModelChangeActions;
    function SetOffsetCore(const AInfo: TdxBorderInfo; const AValue: Integer): TdxDocumentModelChangeActions;
    function SetFrameCore(const AInfo: TdxBorderInfo; const AValue: Boolean): TdxDocumentModelChangeActions;
    function SetShadowCore(const AInfo: TdxBorderInfo; const AValue: Boolean): TdxDocumentModelChangeActions;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxBorderInfo>; override;
    procedure OnBeginAssign; override;
    procedure OnEndAssign; override;
    procedure RaiseObtainAffectedRange(const AArgs: TdxObtainAffectedRangeEventArgs); override;
    procedure ApplyChanges(const AChangeActions: TdxDocumentModelChangeActions); override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    function GetProperty: TdxProperties; virtual; abstract;
    property Owner: IdxPropertiesContainer read FOwner;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxPropertiesContainer); reintroduce;
    procedure ResetBorder;

    property Color: TdxAlphaColor read GetColor write SetColor;
    property Frame: Boolean read GetFrame write SetFrame;
    property Offset: Integer read GetOffset write SetOffset;
    property &Property: TdxProperties read GetProperty;
    property Shadow: Boolean read GetShadow write SetShadow;
    property Style: TdxBorderLineStyle read GetStyle write SetStyle;
    property Width: Integer read GetWidth write SetWidth;
  end;
  TdxBorderBaseList = class(TdxList<TdxBorderBase>);

  { TdxInsideHorizontalBorder }

  TdxInsideHorizontalBorder = class(TdxBorderBase)
  protected
    function GetProperty: TdxProperties; override;
  end;

  { TdxInsideVerticalBorder }

  TdxInsideVerticalBorder = class(TdxBorderBase)
  protected
    function GetProperty: TdxProperties; override;
  end;

  { TdxLeftBorder }

  TdxLeftBorder = class(TdxBorderBase)
  protected
    function GetProperty: TdxProperties; override;
  end;

  { TdxRightBorder }

  TdxRightBorder = class(TdxBorderBase)
  protected
    function GetProperty: TdxProperties; override;
  end;

  { TdxTopBorder }

  TdxTopBorder = class(TdxBorderBase)
  protected
    function GetProperty: TdxProperties; override;
  end;

  { TdxBottomBorder }

  TdxBottomBorder = class(TdxBorderBase)
  protected
    function GetProperty: TdxProperties; override;
  end;

  { TdxWidthUnitInfo }

  TdxWidthUnitInfo = class(TdxCloneable)
  private
    FType: TdxWidthUnitType;
    FValue: Integer;
  public
    constructor Create(AType: TdxWidthUnitType; AValue: Integer); reintroduce; overload;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Clone: TdxWidthUnitInfo; reintroduce; inline;
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;

    property &Type: TdxWidthUnitType read FType write FType;
    property Value: Integer read FValue write FValue;
  end;

  { TdxWidthUnit }

  TdxWidthUnit = class abstract(TdxRichEditIndexBasedObject<TdxWidthUnitInfo>)
  private
    FOwner: IdxPropertiesContainer;
    function GetType: TdxWidthUnitType;
    procedure SetType(const AValue: TdxWidthUnitType);
    function GetValue: Integer;
    procedure SetValue(const AValue: Integer);
  strict protected
    function SetTypeCore(const AUnit: TdxWidthUnitInfo;
      const AValue: TdxWidthUnitType): TdxDocumentModelChangeActions; virtual;
    function SetValueCore(const AUnit: TdxWidthUnitInfo;
      const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxWidthUnitInfo>; override;
    procedure OnEndAssign; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    procedure RaiseObtainAffectedRange(const AArgs: TdxObtainAffectedRangeEventArgs); override;
    procedure ApplyChanges(const AChangeActions: TdxDocumentModelChangeActions); override;
    property Owner: IdxPropertiesContainer read FOwner;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxPropertiesContainer); reintroduce;

    property &Type: TdxWidthUnitType read GetType write SetType;
    property Value: Integer read GetValue write SetValue;
  end;

  { TdxWidthUnitInfoCache }

  TdxWidthUnitInfoCache = class(TdxUniqueItemsCache<TdxWidthUnitInfo>)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxWidthUnitInfo; override;
  end;

  { TdxHeightUnitInfo }

  TdxHeightUnitInfo = class(TdxCloneable)
  private
    FType: TdxHeightUnitType;
    FVal: Integer;
  public
    constructor Create(AValue: Integer; AType: TdxHeightUnitType); reintroduce; overload;
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Clone: TdxHeightUnitInfo; reintroduce; inline;

    property &Type: TdxHeightUnitType read FType write FType;
    property Value: Integer read FVal write FVal;
  end;

  { TdxHeightUnit }

  TdxHeightUnit = class(TdxRichEditIndexBasedObject<TdxHeightUnitInfo>)
  strict private
    FOwner: IdxPropertiesContainer;
    function GetType: TdxHeightUnitType;
    procedure SetType(const Value: TdxHeightUnitType);
    function GetValue: Integer;
    procedure SetValue(const Value: Integer);
  strict protected
    function SetTypeCore(const AUnit: TdxHeightUnitInfo;
      const AValue: Integer): TdxDocumentModelChangeActions;
    function SetValueCore(const AUnit: TdxHeightUnitInfo;
      const AValue: Integer): TdxDocumentModelChangeActions;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxHeightUnitInfo>; override;
    procedure OnEndAssign; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    procedure RaiseObtainAffectedRange(const AArgs: TdxObtainAffectedRangeEventArgs); override;
    procedure ApplyChanges(const AChangeActions: TdxDocumentModelChangeActions); override;

    property Owner: IdxPropertiesContainer read FOwner;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxPropertiesContainer); reintroduce;

    property &Type: TdxHeightUnitType read GetType write SetType;
    property Value: Integer read GetValue write SetValue;
  end;

  { TdxHeightUnitInfoCache }

  TdxHeightUnitInfoCache = class(TdxUniqueItemsCache<TdxHeightUnitInfo>)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxHeightUnitInfo; override;
  end;

  { TdxCombinedCellMarginsInfo }

  TdxCellMargins = class;

  TdxCombinedCellMarginsInfo = class(TdxCloneable)
  private
    FTop: TdxWidthUnitInfo;
    FLeft: TdxWidthUnitInfo;
    FRight: TdxWidthUnitInfo;
    FBottom: TdxWidthUnitInfo;
  public
    constructor Create(ACellMargins: TdxCellMargins = nil); reintroduce;
    destructor Destroy; override;
    procedure CopyFrom(Source: TdxCloneable); override;

    property Top: TdxWidthUnitInfo read FTop;
    property Left: TdxWidthUnitInfo read FLeft;
    property Right: TdxWidthUnitInfo read FRight;
    property Bottom: TdxWidthUnitInfo read FBottom;
  end;

  { TdxCellMargins }

  TdxMarginUnitBase = class;
  TdxTopMarginUnit = class;
  TdxBottomMarginUnit = class;
  TdxLeftMarginUnit = class;
  TdxRightMarginUnit = class;

  TdxCellMargins = class
  private
    FOwner: IdxCellMarginsContainer;
    FTop: TdxMarginUnitBase;
    FLeft: TdxMarginUnitBase;
    FRight: TdxMarginUnitBase;
    FBottom: TdxMarginUnitBase;
    function GetUseLeftMargin: Boolean;
    function GetUseRightMargin: Boolean;
    function GetUseTopMargin: Boolean;
    function GetUseBottomMargin: Boolean;
  protected
    function CreateTopMargin(APieceTable: TdxCustomPieceTable): TdxTopMarginUnit; virtual;
    function CreateBottomMargin(APieceTable: TdxCustomPieceTable): TdxBottomMarginUnit; virtual;
    function CreateLeftMargin(APieceTable: TdxCustomPieceTable): TdxLeftMarginUnit; virtual;
    function CreateRightMargin(APieceTable: TdxCustomPieceTable): TdxRightMarginUnit; virtual;

    property Owner: IdxCellMarginsContainer read FOwner;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxCellMarginsContainer);
    destructor Destroy; override;

    function AreSame(AOther: TdxCellMargins): Boolean;
    procedure CopyFrom(ANewMargins: TdxCellMargins); overload;
    procedure CopyFrom(ANewMargins: TdxCombinedCellMarginsInfo); overload;
    procedure Merge(AMargins: TdxCellMargins);

    property Top: TdxMarginUnitBase read FTop;
    property Left: TdxMarginUnitBase read FLeft;
    property Right: TdxMarginUnitBase read FRight;
    property Bottom: TdxMarginUnitBase read FBottom;

    property UseLeftMargin: Boolean read GetUseLeftMargin;
    property UseRightMargin: Boolean read GetUseRightMargin;
    property UseTopMargin: Boolean read GetUseTopMargin;
    property UseBottomMargin: Boolean read GetUseBottomMargin;
  end;

  { TdxMarginUnitBase }

  TdxMarginUnitBase = class abstract (TdxWidthUnit)
  public const
    MaskUseLeftMargin = $00000010;
    MaskUseRightMargin = $00000020;
    MaskUseTopMargin = $00000040;
    MaskUseBottomMargin = $00000080;
    TableMaskUseLeftMargin = $00000001;
    TableMaskUseRightMargin = $00000002;
    TableMaskUseTopMargin = $00000004;
    TableMaskUseBottomMargin = $00000008;
  public type
    TdxMarginPropertyAccessorBase = class abstract
    public
      function CellPropertiesMask: Integer; virtual; abstract;
      function TablePropertiesMask: Integer; virtual; abstract;
      function GetValue(ACellMargins: TdxCellMargins): TdxMarginUnitBase; virtual; abstract;
    end;
  protected
    function GetProperty: TdxProperties; virtual; abstract;
    procedure OnBeginAssign; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    function SetTypeCore(const AUnit: TdxWidthUnitInfo; const AValue: TdxWidthUnitType): TdxDocumentModelChangeActions; override;
    function SetValueCore(const AUnit: TdxWidthUnitInfo; const AValue: Integer): TdxDocumentModelChangeActions; override;

    property &Property: TdxProperties read GetProperty;
  end;

  { TdxLeftMarginUnit }

  TdxLeftMarginUnit = class(TdxMarginUnitBase)
  public type
    TdxLeftPropertyAccessor = class(TdxMarginUnitBase.TdxMarginPropertyAccessorBase)
    public
      function CellPropertiesMask: Integer; override;
      function TablePropertiesMask: Integer; override;
      function GetValue(ACellMargins: TdxCellMargins): TdxMarginUnitBase; override;
    end;
  protected
    class var FPropertyAccessor: TdxMarginUnitBase.TdxMarginPropertyAccessorBase;
    class constructor Initialize;
    class destructor Finalize;

    function GetProperty: TdxProperties; override;
  public
    class property PropertyAccessor: TdxMarginUnitBase.TdxMarginPropertyAccessorBase read FPropertyAccessor;
  end;

  { TdxRightMarginUnit }

  TdxRightMarginUnit = class(TdxMarginUnitBase)
  public type
    TdxRightPropertyAccessor = class(TdxMarginUnitBase.TdxMarginPropertyAccessorBase)
    public
      function CellPropertiesMask: Integer; override;
      function TablePropertiesMask: Integer; override;
      function GetValue(ACellMargins: TdxCellMargins): TdxMarginUnitBase; override;
    end;
  protected
    class var FPropertyAccessor: TdxMarginUnitBase.TdxMarginPropertyAccessorBase;
    class constructor Initialize;
    class destructor Finalize;

    function GetProperty: TdxProperties; override;
  public
    class property PropertyAccessor: TdxMarginUnitBase.TdxMarginPropertyAccessorBase read FPropertyAccessor;
  end;

  { TdxTopMarginUnit }

  TdxTopMarginUnit = class(TdxMarginUnitBase)
  public type
    TdxTopPropertyAccessor = class(TdxMarginUnitBase.TdxMarginPropertyAccessorBase)
    public
      function CellPropertiesMask: Integer; override;
      function TablePropertiesMask: Integer; override;
      function GetValue(ACellMargins: TdxCellMargins): TdxMarginUnitBase; override;
    end;
  protected
    class var FPropertyAccessor: TdxMarginUnitBase.TdxMarginPropertyAccessorBase;
    class constructor Initialize;
    class destructor Finalize;

    function GetProperty: TdxProperties; override;
  public
    class property PropertyAccessor: TdxMarginUnitBase.TdxMarginPropertyAccessorBase read FPropertyAccessor;
  end;

  { TdxBottomMarginUnit }

  TdxBottomMarginUnit = class(TdxMarginUnitBase)
  public type
    TdxBottomPropertyAccessor = class(TdxMarginUnitBase.TdxMarginPropertyAccessorBase)
    public
      function CellPropertiesMask: Integer; override;
      function TablePropertiesMask: Integer; override;
      function GetValue(ACellMargins: TdxCellMargins): TdxMarginUnitBase; override;
    end;
  protected
    class var FPropertyAccessor: TdxMarginUnitBase.TdxMarginPropertyAccessorBase;
    class constructor Initialize;
    class destructor Finalize;

    function GetProperty: TdxProperties; override;
  public
    class property PropertyAccessor: TdxMarginUnitBase.TdxMarginPropertyAccessorBase read FPropertyAccessor;
  end;

  { TdxPreferredWidth }

  TdxPreferredWidth = class(TdxWidthUnit)
  protected
    procedure OnBeginAssign; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    function SetTypeCore(const AUnit: TdxWidthUnitInfo;
      const AValue: TdxWidthUnitType): TdxDocumentModelChangeActions; override;
    function SetValueCore(const AUnit: TdxWidthUnitInfo;
      const AValue: Integer): TdxDocumentModelChangeActions; override;
  end;

  { TdxTopLeftDiagonalBorder }

  TdxTopLeftDiagonalBorder = class(TdxBorderBase)
  protected
    function GetProperty: TdxProperties; override;
  end;

  { TdxTopRightDiagonalBorder }

  TdxTopRightDiagonalBorder = class(TdxBorderBase)
  protected
    function GetProperty: TdxProperties; override;
  end;

  { TdxBorderInfoRepository }

  TdxBorderInfoRepository = class
  strict private
    class var
      UnderlineTable: array[TdxBorderLineStyle] of TdxUnderline;
    class constructor Initialize;
    class destructor Finalize;
    class procedure CreateUnderlineTable;
  private
    FItems: TdxBorderInfoList;
    FUnitConverter: TdxDocumentModelUnitConverter;
    FCurrentItemIndex: Integer;
    FOnUpdateUI: TNotifyEvent;
    function GetCurrentItem: TdxBorderInfo;
    procedure SetCurrentItemIndex(const Value: Integer);
  protected
    procedure AddItem(ALineStyle: TdxBorderLineStyle; AWidthInTwips: Integer);
    procedure PopulateItems;
    procedure DoUpdateUI;
  public
    constructor Create(AUnitConverter: TdxDocumentModelUnitConverter);
    destructor Destroy; override;
    function GetItemIndexByLineStyle(AStyle: TdxBorderLineStyle): Integer;
    function GetItemByLineStyle(AStyle: TdxBorderLineStyle): TdxBorderInfo;
    class function GetUnderlineByBorderLineStyle(AStyle: TdxBorderLineStyle): TdxUnderline; static;
    property CurrentItem: TdxBorderInfo read GetCurrentItem;
    property CurrentItemIndex: Integer read FCurrentItemIndex write SetCurrentItemIndex;
    property Items: TdxBorderInfoList read FItems;
    property OnUpdateUI: TNotifyEvent read FOnUpdateUI write FOnUpdateUI;
  end;

  { TdxBorderLineRepository }

  TdxBorderLineRepository = class(TdxPatternLineRepository)
  strict private
    class function MapBorderStyleToUnderlineTryGetValue(ALineStyle: TdxBorderLineStyle;
      var AUnderlineClass: TdxUnderlineClass): Boolean; static;
  protected
    procedure PopulateRepository; override;
    function GetUnderlineType(ALineStyle: TdxBorderLineStyle): TdxUnderlineClass;
  public
    destructor Destroy; override;
    function GetCharacterLineByType(ALineStyle: TdxBorderLineStyle): TdxUnderline;
  end;

implementation

uses
  RTLConsts, Math,
  dxRichEdit.DocumentModel.Cache.Simple,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.BatchUpdateHelper;

{ TdxBorderInfo }

class constructor TdxBorderInfo.Initialize;
begin
  FEmpty := TdxBorderInfo.Create;
  FEmpty.Color := TdxAlphaColors.Empty;
  FEmpty.Style := TdxBorderLineStyle.None;
  FEmpty.Width := 0;
  FEmpty.Frame := False;
  FEmpty.Offset := 0;
  FEmpty.Shadow := False;
end;

class destructor TdxBorderInfo.Finalize;
begin
  FEmpty.Free;
end;

constructor TdxBorderInfo.Create;
begin
  inherited Create;
  FStyle := TdxBorderLineStyle.None;
end;

class function TdxBorderInfo.AreBordersEqual(const A, B: TdxBorderInfo): Boolean;
begin
  if A = B then
    Exit(True);
  if A = nil then
    Exit(False);
  Result := A.Equals(B);
end;


function TdxBorderInfo.Clone: TdxBorderInfo;
begin
  Result := TdxBorderInfo(inherited Clone);
end;

procedure TdxBorderInfo.CopyFrom(Source: TdxCloneable);
var
  AInfo: TdxBorderInfo absolute Source;
begin
  Style := AInfo.Style;
  Color := AInfo.Color;
  Width := AInfo.Width;
  Frame := AInfo.Frame;
  Shadow := AInfo.Shadow;
  Offset := AInfo.Offset;
end;

function TdxBorderInfo.Equals(AObject: TObject): Boolean;
var
  AInfo: TdxBorderInfo absolute AObject;
begin
  if AObject is TdxBorderInfo then
  begin
    Result := (Style = AInfo.Style) and (Color = AInfo.Color) and (Width = AInfo.Width) and (Frame = AInfo.Frame) and
      (Shadow = AInfo.Shadow) and (Offset = AInfo.Offset);
  end
  else
    Result := False;
end;

function TdxBorderInfo.GetHashCode: Integer;
begin
  Result := ((Ord(Style) shl 20) or (Width shl 15) or (Offset shl 10) or
    (IfThen(Frame, 1, 0) shl 5) or (IfThen(Shadow, 1, 0) shl 4)) xor Integer(Color);
end;

{ TdxBorderInfoCache }

function TdxBorderInfoCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxBorderInfo;
begin
  Result := TdxBorderInfo.Create;
  Result.Style := TdxBorderLineStyle.&Nil;
  Result.Color := TdxAlphaColors.Black;
  Result.Width := 0;
end;

{ TdxBorderBase }

constructor TdxBorderBase.Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxPropertiesContainer);
begin
  inherited Create(APieceTable);
  FOwner := AOwner;
end;

procedure TdxBorderBase.ApplyChanges(const AChangeActions: TdxDocumentModelChangeActions);
begin
  if Owner <> nil then
    Owner.ApplyChanges(AChangeActions)
  else
    inherited ApplyChanges(AChangeActions);
end;

function TdxBorderBase.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxBorderChangeActionsCalculator.CalculateChangeActions(TdxBorderChangeType.BatchUpdate);
end;

function TdxBorderBase.GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxBorderInfo>;
begin
  Result := TdxSimpleDocumentCache(ADocumentModel.Cache).BorderInfoCache;
end;

function TdxBorderBase.GetColor: TdxAlphaColor;
begin
  Result := Info.Color;
end;

function TdxBorderBase.GetFrame: Boolean;
begin
  Result := Info.Frame;
end;

function TdxBorderBase.GetOffset: Integer;
begin
  Result := Info.Offset;
end;

function TdxBorderBase.GetShadow: Boolean;
begin
  Result := Info.Shadow;
end;

function TdxBorderBase.GetStyle: TdxBorderLineStyle;
begin
  Result := Info.Style;
end;

function TdxBorderBase.GetWidth: Integer;
begin
  Result := Info.Width;
end;

procedure TdxBorderBase.OnBeginAssign;
begin
  inherited OnBeginAssign;
  if Owner <> nil then
    Owner.BeginChanging(&Property);
end;

procedure TdxBorderBase.OnEndAssign;
begin
  if Owner <> nil then
    Owner.EndChanging;
  inherited OnEndAssign;
end;

procedure TdxBorderBase.RaiseObtainAffectedRange(const AArgs: TdxObtainAffectedRangeEventArgs);
begin
  if Owner <> nil then
    Owner.RaiseObtainAffectedRange(AArgs);
end;

procedure TdxBorderBase.ResetBorder;
begin
  Owner.ResetPropertyUse(&Property);
end;

procedure TdxBorderBase.SetColor(const Value: TdxAlphaColor);
begin
  if Value = Info.Color then
    NotifyFakeAssign
  else
    SetPropertyValue<Integer>(SetColorCore, Ord(Value));
end;

function TdxBorderBase.SetColorCore(const AInfo: TdxBorderInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.Color := AValue;
  Result := TdxBorderChangeActionsCalculator.CalculateChangeActions(TdxBorderChangeType.Color);
end;

procedure TdxBorderBase.SetFrame(const Value: Boolean);
begin
  if Value = Info.Frame then
    NotifyFakeAssign
  else
    SetPropertyValue<Boolean>(SetFrameCore, Value);
end;

function TdxBorderBase.SetFrameCore(const AInfo: TdxBorderInfo; const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.Frame := AValue;
  Result := TdxBorderChangeActionsCalculator.CalculateChangeActions(TdxBorderChangeType.Frame);
end;

procedure TdxBorderBase.SetOffset(const Value: Integer);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('Offset', value);
  if Value = Info.Offset then
    NotifyFakeAssign
  else
    SetPropertyValue<Integer>(SetOffsetCore, Value);
end;

function TdxBorderBase.SetOffsetCore(const AInfo: TdxBorderInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.Offset := AValue;
  Result := TdxBorderChangeActionsCalculator.CalculateChangeActions(TdxBorderChangeType.Offset);
end;

procedure TdxBorderBase.SetShadow(const Value: Boolean);
begin
  if Value = Info.Shadow then
    NotifyFakeAssign
  else
    SetPropertyValue<Boolean>(SetShadowCore, Value);
end;

function TdxBorderBase.SetShadowCore(const AInfo: TdxBorderInfo; const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.Shadow := AValue;
  Result := TdxBorderChangeActionsCalculator.CalculateChangeActions(TdxBorderChangeType.Shadow);
end;

procedure TdxBorderBase.SetStyle(const Value: TdxBorderLineStyle);
begin
  if Value = Info.Style then
    NotifyFakeAssign
  else
    SetPropertyValue<Integer>(SetStyleCore, Ord(Value));
end;

function TdxBorderBase.SetStyleCore(const AInfo: TdxBorderInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.Style := TdxBorderLineStyle(AValue);
  Result := TdxBorderChangeActionsCalculator.CalculateChangeActions(TdxBorderChangeType.Style);
end;

procedure TdxBorderBase.SetWidth(const Value: Integer);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('Width', value);
  if Value = Info.Width then
    NotifyFakeAssign
  else
    SetPropertyValue<Integer>(SetWidthCore, Value);
end;

function TdxBorderBase.SetWidthCore(const AInfo: TdxBorderInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.Width := AValue;
  Result := TdxBorderChangeActionsCalculator.CalculateChangeActions(TdxBorderChangeType.Width);
end;

{ TdxWidthUnit }

procedure TdxWidthUnit.ApplyChanges(const AChangeActions: TdxDocumentModelChangeActions);
begin
  if Owner <> nil then
    Owner.ApplyChanges(AChangeActions)
  else
    inherited ApplyChanges(AChangeActions);
end;

constructor TdxWidthUnit.Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxPropertiesContainer);
begin
  Assert(AOwner <> nil);
  inherited Create(APieceTable);
  FOwner := AOwner;
end;

function TdxWidthUnit.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := [];
end;

function TdxWidthUnit.GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxWidthUnitInfo>;
begin
  Result := TdxSimpleDocumentCache(ADocumentModel.Cache).UnitInfoCache;
end;

function TdxWidthUnit.GetType: TdxWidthUnitType;
begin
  Result := Info.&Type;
end;

function TdxWidthUnit.GetValue: Integer;
begin
  Result := Info.Value;
end;

procedure TdxWidthUnit.OnEndAssign;
begin
  Owner.EndChanging;
  inherited OnEndAssign;
end;

procedure TdxWidthUnit.RaiseObtainAffectedRange(const AArgs: TdxObtainAffectedRangeEventArgs);
begin
  if Owner <> nil then
    Owner.RaiseObtainAffectedRange(AArgs);
end;

procedure TdxWidthUnit.SetType(const AValue: TdxWidthUnitType);
begin
  if AValue = Info.&Type then
  begin
    NotifyFakeAssign;
    Exit;
  end;
  SetPropertyValue<TdxWidthUnitType>(SetTypeCore, AValue);
end;

function TdxWidthUnit.SetTypeCore(const AUnit: TdxWidthUnitInfo; const AValue: TdxWidthUnitType): TdxDocumentModelChangeActions;
begin
  AUnit.&Type := AValue;
  Result := [];
end;

procedure TdxWidthUnit.SetValue(const AValue: Integer);
begin
  if AValue = Info.Value then
  begin
    NotifyFakeAssign;
    Exit;
  end;
  SetPropertyValue<Integer>(SetValueCore, AValue);
end;

function TdxWidthUnit.SetValueCore(const AUnit: TdxWidthUnitInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AUnit.Value := AValue;
  Result := [];
end;

{ TdxWidthUnitInfo }

constructor TdxWidthUnitInfo.Create(AType: TdxWidthUnitType; AValue: Integer);
begin
  inherited Create;
  FType := AType;
  FValue := AValue;
end;

function TdxWidthUnitInfo.Clone: TdxWidthUnitInfo;
begin
  Result := TdxWidthUnitInfo(inherited Clone);
end;

procedure TdxWidthUnitInfo.CopyFrom(Source: TdxCloneable);
var
  AInfo: TdxWidthUnitInfo absolute Source;
begin
  FType := AInfo.&Type;
  FValue := AInfo.Value;
end;

function TdxWidthUnitInfo.Equals(AObject: TObject): Boolean;
var
  AInfo: TdxWidthUnitInfo;
begin
  if AObject is TdxWidthUnitInfo then
  begin
    AInfo := TdxWidthUnitInfo(AObject);
    Result := (FType = AInfo.&Type) and (FValue = AInfo.Value);
  end
  else
    Result := False;
end;

function TdxWidthUnitInfo.GetHashCode: Integer;
begin
  Result := (Ord(FType) shl 3) or FValue;
end;

{ TdxWidthUnitInfoCache }

function TdxWidthUnitInfoCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxWidthUnitInfo;
begin
  Result := TdxWidthUnitInfo.Create;
end;

{ TdxMarginUnitBase }

function TdxMarginUnitBase.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetPrimaryLayout,
    TdxDocumentModelChangeAction.ResetSecondaryLayout, TdxDocumentModelChangeAction.ResetRuler,
    TdxDocumentModelChangeAction.RaiseContentChanged, TdxDocumentModelChangeAction.ResetSelectionLayout,
    TdxDocumentModelChangeAction.ResetRuler];
end;

procedure TdxMarginUnitBase.OnBeginAssign;
begin
  inherited OnBeginAssign;
  Owner.BeginChanging(&Property);
end;

function TdxMarginUnitBase.SetTypeCore(const AUnit: TdxWidthUnitInfo;
  const AValue: TdxWidthUnitType): TdxDocumentModelChangeActions;
begin
  Result := inherited SetTypeCore(AUnit, AValue) + [TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetPrimaryLayout, TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ResetRuler, TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.ResetSelectionLayout, TdxDocumentModelChangeAction.ResetRuler];
end;

function TdxMarginUnitBase.SetValueCore(const AUnit: TdxWidthUnitInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  Result := inherited SetValueCore(AUnit, AValue) + [TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetPrimaryLayout, TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ResetRuler, TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.ResetSelectionLayout, TdxDocumentModelChangeAction.ResetRuler];
end;

{ TdxLeftMarginUnit.TdxLeftPropertyAccessor }

function TdxLeftMarginUnit.TdxLeftPropertyAccessor.CellPropertiesMask: Integer;
begin
  Result := MaskUseLeftMargin;
end;

function TdxLeftMarginUnit.TdxLeftPropertyAccessor.TablePropertiesMask: Integer;
begin
  Result := TableMaskUseLeftMargin;
end;

function TdxLeftMarginUnit.TdxLeftPropertyAccessor.GetValue(ACellMargins: TdxCellMargins): TdxMarginUnitBase;
begin
  Result := ACellMargins.Left;
end;

{ TdxLeftMarginUnit }

class constructor TdxLeftMarginUnit.Initialize;
begin
  FPropertyAccessor := TdxLeftPropertyAccessor.Create;
end;

class destructor TdxLeftMarginUnit.Finalize;
begin
  FreeAndNil(FPropertyAccessor);
end;

function TdxLeftMarginUnit.GetProperty: TdxProperties;
begin
  Result := TdxProperties.LeftMargin;
end;

{ TdxRightMarginUnit.TdxRightPropertyAccessor }

function TdxRightMarginUnit.TdxRightPropertyAccessor.CellPropertiesMask: Integer;
begin
  Result := MaskUseRightMargin;
end;

function TdxRightMarginUnit.TdxRightPropertyAccessor.TablePropertiesMask: Integer;
begin
  Result := TableMaskUseRightMargin;
end;

function TdxRightMarginUnit.TdxRightPropertyAccessor.GetValue(ACellMargins: TdxCellMargins): TdxMarginUnitBase;
begin
  Result := ACellMargins.Right;
end;

{ TdxRightMarginUnit }

class constructor TdxRightMarginUnit.Initialize;
begin
  FPropertyAccessor := TdxRightPropertyAccessor.Create;
end;

class destructor TdxRightMarginUnit.Finalize;
begin
  FreeAndNil(FPropertyAccessor);
end;

function TdxRightMarginUnit.GetProperty: TdxProperties;
begin
  Result := TdxProperties.RightMargin;
end;

{ TdxTopMarginUnit.TdxTopPropertyAccessor }

function TdxTopMarginUnit.TdxTopPropertyAccessor.CellPropertiesMask: Integer;
begin
  Result := MaskUseTopMargin;
end;

function TdxTopMarginUnit.TdxTopPropertyAccessor.TablePropertiesMask: Integer;
begin
  Result := TableMaskUseTopMargin;
end;

function TdxTopMarginUnit.TdxTopPropertyAccessor.GetValue(ACellMargins: TdxCellMargins): TdxMarginUnitBase;
begin
  Result := ACellMargins.Top;
end;

{ TdxTopMarginUnit }

class constructor TdxTopMarginUnit.Initialize;
begin
  FPropertyAccessor := TdxTopPropertyAccessor.Create;
end;

class destructor TdxTopMarginUnit.Finalize;
begin
  FreeAndNil(FPropertyAccessor);
end;

function TdxTopMarginUnit.GetProperty: TdxProperties;
begin
  Result := TdxProperties.TopMargin;
end;

{ TdxHeightUnitInfo }

constructor TdxHeightUnitInfo.Create(AValue: Integer; AType: TdxHeightUnitType);
begin
  inherited Create;
  FVal := AValue;
  FType := AType;
end;

function TdxHeightUnitInfo.Clone: TdxHeightUnitInfo;
begin
  Result := TdxHeightUnitInfo(inherited Clone);
end;

procedure TdxHeightUnitInfo.CopyFrom(Source: TdxCloneable);
var
  AInfo: TdxHeightUnitInfo absolute Source;
begin
  &Type := AInfo.&Type;
  Value := AInfo.Value;
end;

function TdxHeightUnitInfo.Equals(AObject: TObject): Boolean;
var
  AInfo: TdxHeightUnitInfo;
begin
  if AObject is TdxHeightUnitInfo then
  begin
    AInfo := TdxHeightUnitInfo(AObject);
    Result := (&Type = AInfo.&Type) and (Value = AInfo.Value);
  end
  else
    Result := False;
end;

function TdxHeightUnitInfo.GetHashCode: Integer;
begin
  Result := (Ord(&Type) shl 3) or Value;
end;

{ TdxHeightUnit }

procedure TdxHeightUnit.ApplyChanges(const AChangeActions: TdxDocumentModelChangeActions);
begin
  if Owner <> nil then
    Owner.ApplyChanges(AChangeActions)
  else
    inherited ApplyChanges(AChangeActions);
end;

constructor TdxHeightUnit.Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxPropertiesContainer);
begin
  inherited Create(APieceTable);
  Assert(AOwner <> nil);
  FOwner := AOwner;
end;

function TdxHeightUnit.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := [];
end;

function TdxHeightUnit.GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxHeightUnitInfo>;
begin
  Result := TdxSimpleDocumentCache(ADocumentModel.Cache).HeightUnitInfoCache;
end;

function TdxHeightUnit.GetType: TdxHeightUnitType;
begin
  Result := Info.&Type;
end;

function TdxHeightUnit.GetValue: Integer;
begin
  Result := Info.Value;
end;

procedure TdxHeightUnit.OnEndAssign;
begin
  Owner.EndChanging;
  inherited OnEndAssign;
end;

procedure TdxHeightUnit.RaiseObtainAffectedRange(const AArgs: TdxObtainAffectedRangeEventArgs);
begin
  if Owner <> nil then
    Owner.RaiseObtainAffectedRange(AArgs);
end;

procedure TdxHeightUnit.SetType(const Value: TdxHeightUnitType);
begin
  if Value = Info.&Type then
    NotifyFakeAssign
  else
    SetPropertyValue<Integer>(SetTypeCore, Ord(Value));
end;

function TdxHeightUnit.SetTypeCore(const AUnit: TdxHeightUnitInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AUnit.&Type := TdxHeightUnitType(AValue);
  Result := BatchUpdateChangeActions;
end;

procedure TdxHeightUnit.SetValue(const Value: Integer);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('Value', value);
  if Value = Info.Value then
    NotifyFakeAssign
  else
    SetPropertyValue<Integer>(SetValueCore, Value);
end;

function TdxHeightUnit.SetValueCore(const AUnit: TdxHeightUnitInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AUnit.Value := AValue;
  Result := BatchUpdateChangeActions;
end;

{ TdxHeightUnitInfoCache }

function TdxHeightUnitInfoCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxHeightUnitInfo;
begin
  Result := TdxHeightUnitInfo.Create;
  Result.&Type := TdxHeightUnitType.Auto;
  Result.Value := 0;
end;

{ TdxCombinedCellMarginsInfo }

constructor TdxCombinedCellMarginsInfo.Create(ACellMargins: TdxCellMargins = nil);
begin
  inherited Create;
  if ACellMargins = nil then
  begin
    FLeft := TdxWidthUnitInfo.Create;
    FTop := TdxWidthUnitInfo.Create;
    FRight := TdxWidthUnitInfo.Create;
    FBottom := TdxWidthUnitInfo.Create;
  end
  else
  begin
    FLeft := ACellMargins.Left.Info.Clone;
    FTop := ACellMargins.Top.Info.Clone;
    FRight := ACellMargins.Right.Info.Clone;
    FBottom := ACellMargins.Bottom.Info.Clone;
  end;
end;

destructor TdxCombinedCellMarginsInfo.Destroy;
begin
  FreeAndNil(FBottom);
  FreeAndNil(FRight);
  FreeAndNil(FTop);
  FreeAndNil(FLeft);
  inherited Destroy;
end;

procedure TdxCombinedCellMarginsInfo.CopyFrom(Source: TdxCloneable);
var
  AInfo: TdxCombinedCellMarginsInfo absolute Source;
begin
  FLeft.CopyFrom(AInfo.Left);
  FTop.CopyFrom(AInfo.Top);
  FRight.CopyFrom(AInfo.Right);
  FBottom.CopyFrom(AInfo.Bottom);
end;

{ TdxCellMargins }

constructor TdxCellMargins.Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxCellMarginsContainer);
begin
  inherited Create;
  Assert(AOwner <> nil);
  FOwner := AOwner;
  FTop := CreateTopMargin(APieceTable);
  FLeft := CreateLeftMargin(APieceTable);
  FRight := CreateRightMargin(APieceTable);
  FBottom := CreateBottomMargin(APieceTable);
end;

destructor TdxCellMargins.Destroy;
begin
  FBottom.Free;
  FRight.Free;
  FLeft.Free;
  FTop.Free;
  inherited Destroy;
end;

function TdxCellMargins.AreSame(AOther: TdxCellMargins): Boolean;
begin
  Result := (Left.Index = AOther.Left.Index) and
  (Right.Index = AOther.Right.Index) and
  (Top.Index = AOther.Top.Index) and
  (Bottom.Index = AOther.Bottom.Index);
end;

procedure TdxCellMargins.CopyFrom(ANewMargins: TdxCellMargins);
begin
  Owner.BeginPropertiesUpdate;
  try
    if Left.DocumentModel = ANewMargins.Left.DocumentModel then
    begin
      Top.CopyFrom(ANewMargins.Top);
      Left.CopyFrom(ANewMargins.Left);
      Right.CopyFrom(ANewMargins.Right);
      Bottom.CopyFrom(ANewMargins.Bottom);
    end
    else
    begin
      Top.CopyFrom(ANewMargins.Top.Info);
      Left.CopyFrom(ANewMargins.Left.Info);
      Right.CopyFrom(ANewMargins.Right.Info);
      Bottom.CopyFrom(ANewMargins.Bottom.Info);
    end;
  finally
    Owner.EndPropertiesUpdate;
  end;
end;

procedure TdxCellMargins.CopyFrom(ANewMargins: TdxCombinedCellMarginsInfo);
begin
  Owner.BeginPropertiesUpdate;
  try
    Top.CopyFrom(ANewMargins.Top);
    Left.CopyFrom(ANewMargins.Left);
    Right.CopyFrom(ANewMargins.Right);
    Bottom.CopyFrom(ANewMargins.Bottom);
  finally
    Owner.EndPropertiesUpdate;
  end;
end;

function TdxCellMargins.CreateBottomMargin(APieceTable: TdxCustomPieceTable): TdxBottomMarginUnit;
begin
  Result := TdxBottomMarginUnit.Create(APieceTable, FOwner);
end;

function TdxCellMargins.CreateLeftMargin(APieceTable: TdxCustomPieceTable): TdxLeftMarginUnit;
begin
  Result := TdxLeftMarginUnit.Create(APieceTable, FOwner);
end;

function TdxCellMargins.CreateRightMargin(APieceTable: TdxCustomPieceTable): TdxRightMarginUnit;
begin
  Result := TdxRightMarginUnit.Create(APieceTable, FOwner);
end;

function TdxCellMargins.CreateTopMargin(APieceTable: TdxCustomPieceTable): TdxTopMarginUnit;
begin
  Result := TdxTopMarginUnit.Create(APieceTable, FOwner);
end;

function TdxCellMargins.GetUseBottomMargin: Boolean;
begin
  Result := Owner.UseBottomMargin;
end;

function TdxCellMargins.GetUseLeftMargin: Boolean;
begin
  Result := Owner.UseLeftMargin;
end;

function TdxCellMargins.GetUseRightMargin: Boolean;
begin
  Result := Owner.UseRightMargin;
end;

function TdxCellMargins.GetUseTopMargin: Boolean;
begin
  Result := Owner.UseTopMargin;
end;

procedure TdxCellMargins.Merge(AMargins: TdxCellMargins);
begin
  if not UseLeftMargin and AMargins.UseLeftMargin then
    Left.CopyFrom(AMargins.Left);
  if not UseRightMargin and AMargins.UseRightMargin then
    Right.CopyFrom(AMargins.Right);
  if not UseTopMargin and AMargins.UseTopMargin then
    Top.CopyFrom(AMargins.Top);
  if not UseBottomMargin and AMargins.UseBottomMargin then
    Bottom.CopyFrom(AMargins.Bottom);
end;

{ TdxBorderChangeActionsCalculator }

class function TdxBorderChangeActionsCalculator.CalculateChangeActions(AChange: TdxBorderChangeType): TdxDocumentModelChangeActions;
const
  BorderChangeActionsMap: array[TdxBorderChangeType] of TdxDocumentModelChangeActions = (
    [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout]
  );
begin
  Result := BorderChangeActionsMap[AChange];
end;

{ TdxBottomMarginUnit.TdxBottomPropertyAccessor }

function TdxBottomMarginUnit.TdxBottomPropertyAccessor.CellPropertiesMask: Integer;
begin
  Result := MaskUseBottomMargin;
end;

function TdxBottomMarginUnit.TdxBottomPropertyAccessor.TablePropertiesMask: Integer;
begin
  Result := TableMaskUseBottomMargin;
end;

function TdxBottomMarginUnit.TdxBottomPropertyAccessor.GetValue(ACellMargins: TdxCellMargins): TdxMarginUnitBase;
begin
  Result := ACellMargins.Bottom;
end;

{ TdxBottomMarginUnit }

class constructor TdxBottomMarginUnit.Initialize;
begin
  FPropertyAccessor := TdxBottomPropertyAccessor.Create;
end;

class destructor TdxBottomMarginUnit.Finalize;
begin
  FreeAndNil(FPropertyAccessor);
end;

function TdxBottomMarginUnit.GetProperty: TdxProperties;
begin
  Result := TdxProperties.BottomMargin;
end;

{ TdxPreferredWidth }

function TdxPreferredWidth.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetPrimaryLayout,
    TdxDocumentModelChangeAction.ResetSecondaryLayout, TdxDocumentModelChangeAction.ResetRuler,
    TdxDocumentModelChangeAction.RaiseContentChanged, TdxDocumentModelChangeAction.ResetSelectionLayout,
    TdxDocumentModelChangeAction.ResetRuler];
end;

procedure TdxPreferredWidth.OnBeginAssign;
begin
  inherited OnBeginAssign;
  Owner.BeginChanging(TdxProperties.PreferredWidth);
end;

function TdxPreferredWidth.SetTypeCore(const AUnit: TdxWidthUnitInfo;
  const AValue: TdxWidthUnitType): TdxDocumentModelChangeActions;
begin
  Result := inherited SetTypeCore(AUnit, AValue) + [TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetPrimaryLayout, TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ResetRuler, TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.ResetSelectionLayout, TdxDocumentModelChangeAction.ResetRuler];
end;

function TdxPreferredWidth.SetValueCore(const AUnit: TdxWidthUnitInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  Result := inherited SetValueCore(AUnit, AValue) + [TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetPrimaryLayout, TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ResetRuler, TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.ResetSelectionLayout, TdxDocumentModelChangeAction.ResetRuler];
end;

{ TdxInsideHorizontalBorder }

function TdxInsideHorizontalBorder.GetProperty: TdxProperties;
begin
  Result := TdxProperties.InsideHorizontalBorder;
end;

{ TdxInsideVerticalBorder }

function TdxInsideVerticalBorder.GetProperty: TdxProperties;
begin
  Result := TdxProperties.InsideVerticalBorder;
end;

{ TdxLeftBorder }

function TdxLeftBorder.GetProperty: TdxProperties;
begin
  Result := TdxProperties.LeftBorder;
end;

{ TdxRightBorder }

function TdxRightBorder.GetProperty: TdxProperties;
begin
  Result := TdxProperties.RightBorder;
end;

{ TdxTopBorder }

function TdxTopBorder.GetProperty: TdxProperties;
begin
  Result := TdxProperties.TopBorder;
end;

{ TdxBottomBorder }

function TdxBottomBorder.GetProperty: TdxProperties;
begin
  Result := TdxProperties.BottomBorder;
end;

{ TdxTopLeftDiagonalBorder }

function TdxTopLeftDiagonalBorder.GetProperty: TdxProperties;
begin
  Result := TdxProperties.TopLeftDiagonalBorder;
end;

{ TdxTopRightDiagonalBorder }

function TdxTopRightDiagonalBorder.GetProperty: TdxProperties;
begin
  Result := TdxProperties.TopRightDiagonalBorder;
end;

{ TdxBorderInfoRepository }

constructor TdxBorderInfoRepository.Create(AUnitConverter: TdxDocumentModelUnitConverter);
begin
  inherited Create;
  FUnitConverter := AUnitConverter;
  FItems := TdxBorderInfoList.Create(True);
  FCurrentItemIndex := 1;
  PopulateItems;
end;

destructor TdxBorderInfoRepository.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

class destructor TdxBorderInfoRepository.Finalize;
var
  I: TdxBorderLineStyle;
begin
  for I := Low(UnderlineTable) to High(UnderlineTable) do
    UnderlineTable[I].Free;
end;

procedure TdxBorderInfoRepository.AddItem(ALineStyle: TdxBorderLineStyle; AWidthInTwips: Integer);
var
  ABorderInfo: TdxBorderInfo;
begin
  ABorderInfo := TdxBorderInfo.Create;
  ABorderInfo.Color := TdxAlphaColors.Black;
  ABorderInfo.Width := FUnitConverter.TwipsToModelUnits(AWidthInTwips);
  ABorderInfo.Style := ALineStyle;
  Items.Add(ABorderInfo);
end;

class procedure TdxBorderInfoRepository.CreateUnderlineTable;
begin
  UnderlineTable[TdxBorderLineStyle.None] := TdxNoneUnderline.Create;
  UnderlineTable[TdxBorderLineStyle.Nil] := TdxNoneUnderline.Create;
  UnderlineTable[TdxBorderLineStyle.Single] := TdxUnderlineSingle.Create;
  UnderlineTable[TdxBorderLineStyle.Dotted] := TdxUnderlineDotted.Create;
  UnderlineTable[TdxBorderLineStyle.DashSmallGap] := TdxUnderlineDashSmallGap.Create;
  UnderlineTable[TdxBorderLineStyle.Dashed] := TdxUnderlineDashed.Create;
  UnderlineTable[TdxBorderLineStyle.DotDash] := TdxUnderlineDashDotted.Create;
  UnderlineTable[TdxBorderLineStyle.DotDotDash] := TdxUnderlineDashDotDotted.Create;
  UnderlineTable[TdxBorderLineStyle.Double] := TdxUnderlineDouble.Create;
  UnderlineTable[TdxBorderLineStyle.Wave] := TdxUnderlineWave.Create;
  UnderlineTable[TdxBorderLineStyle.DoubleWave] := TdxUnderlineDoubleWave.Create;
end;

class constructor TdxBorderInfoRepository.Initialize;
begin
  CreateUnderlineTable;
end;

procedure TdxBorderInfoRepository.DoUpdateUI;
begin
  if Assigned(FOnUpdateUI) then
    FOnUpdateUI(Self);
end;

function TdxBorderInfoRepository.GetCurrentItem: TdxBorderInfo;
begin
  Result := FItems[CurrentItemIndex];
end;

function TdxBorderInfoRepository.GetItemByLineStyle(AStyle: TdxBorderLineStyle): TdxBorderInfo;
var
  AIndex: Integer;
begin
  AIndex := GetItemIndexByLineStyle(AStyle);
  if AIndex < 0 then
    Result := nil
  else
    Result := Items[AIndex];
end;

procedure TdxBorderInfoRepository.PopulateItems;
begin
  AddItem(TdxBorderLineStyle.None, 0);
  AddItem(TdxBorderLineStyle.Single, 10);
  AddItem(TdxBorderLineStyle.Dotted, 10);
  AddItem(TdxBorderLineStyle.DashSmallGap, 10);
  AddItem(TdxBorderLineStyle.Dashed, 10);
  AddItem(TdxBorderLineStyle.DotDash, 10);
  AddItem(TdxBorderLineStyle.DotDotDash, 10);
  AddItem(TdxBorderLineStyle.Double, 10);
  AddItem(TdxBorderLineStyle.Triple, 10);
  AddItem(TdxBorderLineStyle.ThickThinSmallGap, 60);
  AddItem(TdxBorderLineStyle.ThinThickSmallGap, 60);
  AddItem(TdxBorderLineStyle.ThinThickThinSmallGap, 60);
  AddItem(TdxBorderLineStyle.ThickThinMediumGap, 60);
  AddItem(TdxBorderLineStyle.ThinThickMediumGap, 60);
  AddItem(TdxBorderLineStyle.ThinThickThinMediumGap, 60);
  AddItem(TdxBorderLineStyle.ThickThinLargeGap, 60);
  AddItem(TdxBorderLineStyle.ThinThickLargeGap, 60);
  AddItem(TdxBorderLineStyle.ThinThickThinLargeGap, 60);
  AddItem(TdxBorderLineStyle.Wave, 15);
  AddItem(TdxBorderLineStyle.DoubleWave, 15);
  AddItem(TdxBorderLineStyle.DashDotStroked, 60);
  AddItem(TdxBorderLineStyle.ThreeDEmboss, 60);
  AddItem(TdxBorderLineStyle.ThreeDEngrave, 60);
  AddItem(TdxBorderLineStyle.Inset, 15);
  AddItem(TdxBorderLineStyle.Outset, 15);
  AddItem(TdxBorderLineStyle.Disabled, 0);
end;

procedure TdxBorderInfoRepository.SetCurrentItemIndex(const Value: Integer);
begin
  if FCurrentItemIndex = Value then
    Exit;
  FCurrentItemIndex := Value;
  DoUpdateUI;
end;

function TdxBorderInfoRepository.GetItemIndexByLineStyle(AStyle: TdxBorderLineStyle): Integer;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    if FItems[I].Style = AStyle then
      Exit(I);
  Result := -1;
end;

class function TdxBorderInfoRepository.GetUnderlineByBorderLineStyle(AStyle: TdxBorderLineStyle): TdxUnderline;
begin
  Result := UnderlineTable[AStyle];
  if Result = nil then
    Result := UnderlineTable[TdxBorderLineStyle.Single];
end;

{ TdxBorderLineRepository }

destructor TdxBorderLineRepository.Destroy;
begin
  Items.Extract(TdxUnderline.UnderlineNone);
  inherited Destroy;
end;

procedure TdxBorderLineRepository.PopulateRepository;
begin
  RegisterPatternLine(TdxUnderline.UnderlineNone);
  RegisterPatternLine(TdxUnderlineSingle.Create);
  RegisterPatternLine(TdxUnderlineDotted.Create);
  RegisterPatternLine(TdxUnderlineDashed.Create);
  RegisterPatternLine(TdxUnderlineDashSmallGap.Create);
  RegisterPatternLine(TdxUnderlineDashDotted.Create);
  RegisterPatternLine(TdxUnderlineDashDotDotted.Create);
  RegisterPatternLine(TdxUnderlineDouble.Create);
  RegisterPatternLine(TdxUnderlineHeavyWave.Create);
  RegisterPatternLine(TdxUnderlineLongDashed.Create);
  RegisterPatternLine(TdxUnderlineThickSingle.Create);
  RegisterPatternLine(TdxUnderlineThickDotted.Create);
  RegisterPatternLine(TdxUnderlineThickDashed.Create);
  RegisterPatternLine(TdxUnderlineThickDashDotted.Create);
  RegisterPatternLine(TdxUnderlineThickDashDotDotted.Create);
  RegisterPatternLine(TdxUnderlineThickLongDashed.Create);
  RegisterPatternLine(TdxUnderlineDoubleWave.Create);
  RegisterPatternLine(TdxUnderlineWave.Create);
end;

function TdxBorderLineRepository.GetCharacterLineByType(ALineStyle: TdxBorderLineStyle): TdxUnderline;
var
  AUnderlineType: TdxUnderlineClass;
begin
  AUnderlineType := GetUnderlineType(ALineStyle);
  if AUnderlineType <> TdxUnderlineNone then
    Result := GetPatternLineByType(AUnderlineType)
  else
    Result := nil;
end;

function TdxBorderLineRepository.GetUnderlineType(ALineStyle: TdxBorderLineStyle): TdxUnderlineClass;
begin
  MapBorderStyleToUnderlineTryGetValue(ALineStyle, Result);
end;

class function TdxBorderLineRepository.MapBorderStyleToUnderlineTryGetValue(ALineStyle: TdxBorderLineStyle;
  var AUnderlineClass: TdxUnderlineClass): Boolean;
begin
  case ALineStyle of
    TdxBorderLineStyle.Double:
    	AUnderlineClass := TdxUnderlineDouble;
    TdxBorderLineStyle.Dotted:
    	AUnderlineClass := TdxUnderlineDotted;
    TdxBorderLineStyle.Dashed:
    	AUnderlineClass := TdxUnderlineDashed;
    TdxBorderLineStyle.DotDash:
    	AUnderlineClass := TdxUnderlineDashDotted;
    TdxBorderLineStyle.DotDotDash:
    	AUnderlineClass := TdxUnderlineDashDotDotted;
    TdxBorderLineStyle.Wave:
    	AUnderlineClass := TdxUnderlineWave;
    TdxBorderLineStyle.DoubleWave:
    	AUnderlineClass := TdxUnderlineDoubleWave;
    TdxBorderLineStyle.DashSmallGap:
    	AUnderlineClass := TdxUnderlineDashSmallGap;
    else
    begin
      AUnderlineClass := TdxUnderlineNone;
      Exit(False);
    end;
  end;
  Result := True;
end;

end.
