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

unit dxRichEdit.DocumentModel.FloatingObjectFormatting;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes, Types, Generics.Defaults, Generics.Collections, dxCoreClasses,
  dxRichEdit.NativeApi,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem;

type
  TdxFloatingObjectHorizontalPositionAlignment = TdxRichEditShapeHorizontalAlignment;
  TdxFloatingObjectVerticalPositionAlignment = TdxRichEditShapeVerticalAlignment;
  TdxFloatingObjectHorizontalPositionType = TdxRichEditShapeRelativeHorizontalPosition;
  TdxFloatingObjectVerticalPositionType = TdxRichEditShapeRelativeVerticalPosition;

  TdxFloatingObjectRelativeFromHorizontal = (
    Margin,
    Page,
    LeftMargin,
    RightMargin,
    InsideMargin,
    OutsideMargin
  );

  TdxFloatingObjectRelativeFromVertical = (
    Margin,
    Page,
    TopMargin,
    BottomMargin,
    InsideMargin,
    OutsideMargin
  );

  TdxFloatingObjectTextWrapType = (
    None,
    TopAndBottom,
    Tight,
    Through,
    Square,
    &Inline
  );

  TdxFloatingObjectTextWrapSide = (
    Both,
    Left,
    Right,
    Largest
  );

  { IdxFloatingObjectPropertiesContainer }

  IdxFloatingObjectPropertiesContainer = interface
  ['{187717E4-B32B-4042-8A17-33FCE8F42EEE}']
    function GetPieceTable: TdxCustomPieceTable;
    function CreateFloatingObjectPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnFloatingObjectChanged;

    property PieceTable: TdxCustomPieceTable read GetPieceTable;
  end;

  { TdxFloatingObjectRelativeWidth }

  TdxFloatingObjectRelativeWidth = record
  strict private
    FWidth: Integer;
    FFrom: TdxFloatingObjectRelativeFromHorizontal;
  public
    constructor Create(AFrom: TdxFloatingObjectRelativeFromHorizontal; AWidth: Integer);
    class function CreateEmpty: TdxFloatingObjectRelativeWidth; static;
    class operator Equal(const A, B: TdxFloatingObjectRelativeWidth): Boolean;
    class operator NotEqual(const A, B: TdxFloatingObjectRelativeWidth): Boolean;

    property Width: Integer read FWidth;
    property From: TdxFloatingObjectRelativeFromHorizontal read FFrom;
  end;

  { TdxFloatingObjectRelativeHeight }

  TdxFloatingObjectRelativeHeight = record
  strict private
    FHeight: Integer;
    FFrom: TdxFloatingObjectRelativeFromVertical;
  public
    constructor Create(AFrom: TdxFloatingObjectRelativeFromVertical; AHeight: Integer);
    class function CreateEmpty: TdxFloatingObjectRelativeHeight; static;
    class operator Equal(const A, B: TdxFloatingObjectRelativeHeight): Boolean;
    class operator NotEqual(const A, B: TdxFloatingObjectRelativeHeight): Boolean;

    property Height: Integer read FHeight;
    property From: TdxFloatingObjectRelativeFromVertical read FFrom;
  end;

  { IdxFloatingObjectLocation }

  IdxFloatingObjectLocation = interface
  ['{B4E56D96-F2AA-4AFA-B990-F2F351FA5FBF}']
    function GetOffsetX: Integer;
    function GetOffsetY: Integer;
    function GetPercentOffsetX: Integer;
    function GetPercentOffsetY: Integer;
    function GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
    function GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
    function GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
    function GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
    function GetTextWrapType: TdxFloatingObjectTextWrapType;
    function GetActualWidth: Integer;
    function GetActualHeight: Integer;
    function GetUseRelativeWidth: Boolean;
    function GetUseRelativeHeight: Boolean;
    function GetLayoutInTableCell: Boolean;
    function GetRelativeWidth: TdxFloatingObjectRelativeWidth;
    function GetRelativeHeight: TdxFloatingObjectRelativeHeight;

    property OffsetX: Integer read GetOffsetX;
    property OffsetY: Integer read GetOffsetY;
    property PercentOffsetX: Integer read GetPercentOffsetX;
    property PercentOffsetY: Integer read GetPercentOffsetY;
    property HorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment read GetHorizontalPositionAlignment;
    property VerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment read GetVerticalPositionAlignment;
    property HorizontalPositionType: TdxFloatingObjectHorizontalPositionType read GetHorizontalPositionType;
    property VerticalPositionType: TdxFloatingObjectVerticalPositionType read GetVerticalPositionType;
    property TextWrapType: TdxFloatingObjectTextWrapType read GetTextWrapType;
    property ActualWidth: Integer read GetActualWidth;
    property ActualHeight: Integer read GetActualHeight;
    property UseRelativeWidth: Boolean read GetUseRelativeWidth;
    property UseRelativeHeight: Boolean read GetUseRelativeHeight;
    property LayoutInTableCell: Boolean read GetLayoutInTableCell;
    property RelativeWidth: TdxFloatingObjectRelativeWidth read GetRelativeWidth;
    property RelativeHeight: TdxFloatingObjectRelativeHeight read GetRelativeHeight;
  end;

  { TdxFloatingObjectInfo }

  TdxFloatingObjectInfo = class(TdxCloneable)
  private
    FAllowOverlap: Boolean;
    FHidden: Boolean;
    FLayoutInTableCell: Boolean;
    FLocked: Boolean;
    FLockAspectRatio: Boolean;
    FLeftDistance: Integer;
    FRightDistance: Integer;
    FTopDistance: Integer;
    FBottomDistance: Integer;
    FZOrder: Integer;
    FActualSize: TSize;
    FRelativeWidth: TdxFloatingObjectRelativeWidth;
    FRelativeHeight: TdxFloatingObjectRelativeHeight;
    FTextWrapType: TdxFloatingObjectTextWrapType;
    FTextWrapSide: TdxFloatingObjectTextWrapSide;
    FHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
    FHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
    FVerticalPositionType: TdxFloatingObjectVerticalPositionType;
    FVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
    FOffset: TPoint;
    FPercentOffset: TPoint;
    FIsBehindDoc: Boolean;
    FPseudoInline: Boolean;
  public
    function Clone: TdxFloatingObjectInfo; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Equals(Obj: TObject): Boolean; override;

    property AllowOverlap: Boolean read FAllowOverlap write FAllowOverlap;
    property Hidden: Boolean read FHidden write FHidden;
    property LayoutInTableCell: Boolean read FLayoutInTableCell write FLayoutInTableCell;
    property Locked: Boolean read FLocked write FLocked;
    property LockAspectRatio: Boolean read FLockAspectRatio write FLockAspectRatio;
    property LeftDistance: Integer read FLeftDistance write FLeftDistance;
    property RightDistance: Integer read FRightDistance write FRightDistance;
    property TopDistance: Integer read FTopDistance write FTopDistance;
    property BottomDistance: Integer read FBottomDistance write FBottomDistance;
    property ZOrder: Integer read FZOrder write FZOrder;
    property ActualSize: TSize read FActualSize write FActualSize;
    property TextWrapType: TdxFloatingObjectTextWrapType read FTextWrapType write FTextWrapType;
    property TextWrapSide: TdxFloatingObjectTextWrapSide read FTextWrapSide write FTextWrapSide;
    property HorizontalPositionType: TdxFloatingObjectHorizontalPositionType read FHorizontalPositionType write FHorizontalPositionType;
    property HorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment read FHorizontalPositionAlignment write FHorizontalPositionAlignment;
    property VerticalPositionType: TdxFloatingObjectVerticalPositionType read FVerticalPositionType write FVerticalPositionType;
    property VerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment read FVerticalPositionAlignment write FVerticalPositionAlignment;
    property Offset: TPoint read FOffset write FOffset;
    property PercentOffset: TPoint read FPercentOffset write FPercentOffset;
    property RelativeWidth: TdxFloatingObjectRelativeWidth read FRelativeWidth write FRelativeWidth;
    property RelativeHeight: TdxFloatingObjectRelativeHeight read FRelativeHeight write FRelativeHeight;
    property IsBehindDoc: Boolean read FIsBehindDoc write FIsBehindDoc;
    property PseudoInline: Boolean read FPseudoInline write FPseudoInline;
  end;

  TdxUseFloatingObjectOption = (
    UseAllowOverlap,
    UseHidden,
    UseLayoutInTableCell,
    UseLocked,
    UseLeftDistance,
    UseRightDistance,
    UseTopDistance,
    UseBottomDistance,
    UseZOrder,
    UseActualSize,
    UseTextWrapType,
    UseTextWrapSide,
    UseHorizontalPositionType,
    UseHorizontalPositionAlignment,
    UseVerticalPositionType,
    UseVerticalPositionAlignment,
    UseOffset,
    UseLockAspectRatio,
    UseIsBehindDoc,
    UsePseudoInline,
    UseRelativeWidth,
    UseRelativeHeight,
    UsePercentOffset
  );

  TdxUseFloatingObjectOptions = set of TdxUseFloatingObjectOption;

  { TdxFloatingObjectOptions }

  TdxFloatingObjectOptions = record
  private const
    MaskUseNone = [];
    MaskUseAll  = [Low(TdxUseFloatingObjectOption)..High(TdxUseFloatingObjectOption)];
  private
    FVal: TdxUseFloatingObjectOptions;
    procedure SetVal(AMask: TdxUseFloatingObjectOption; AValue: Boolean);
    function GetVal(AMask: TdxUseFloatingObjectOption): Boolean;
    class function GetEmptyFloatingObjectOption: TdxFloatingObjectOptions; static;

    property Value: TdxUseFloatingObjectOptions read FVal write FVal;
  public
    constructor Create(const AVal: TdxUseFloatingObjectOptions);

    class operator Equal(const A, B: TdxFloatingObjectOptions): Boolean;
    function Clone: TdxFloatingObjectOptions;
    procedure CopyFrom(const Source: TdxFloatingObjectOptions);
    function GetHashCode: Integer;

    class property EmptyFloatingObjectOption: TdxFloatingObjectOptions read GetEmptyFloatingObjectOption;

    property UseAllowOverlap: Boolean index TdxUseFloatingObjectOption.UseAllowOverlap read GetVal write SetVal;
    property UseHidden: Boolean index TdxUseFloatingObjectOption.UseHidden read GetVal write SetVal;
    property UseLayoutInTableCell: Boolean index TdxUseFloatingObjectOption.UseLayoutInTableCell read GetVal write SetVal;
    property UseLocked: Boolean index TdxUseFloatingObjectOption.UseLocked read GetVal write SetVal;
    property UseLockAspectRatio: Boolean index TdxUseFloatingObjectOption.UseLockAspectRatio read GetVal write SetVal;
    property UseLeftDistance: Boolean index TdxUseFloatingObjectOption.UseLeftDistance read GetVal write SetVal;
    property UseRightDistance: Boolean index TdxUseFloatingObjectOption.UseRightDistance read GetVal write SetVal;
    property UseTopDistance: Boolean index TdxUseFloatingObjectOption.UseTopDistance read GetVal write SetVal;
    property UseBottomDistance: Boolean index TdxUseFloatingObjectOption.UseBottomDistance read GetVal write SetVal;
    property UseZOrder: Boolean index TdxUseFloatingObjectOption.UseZOrder read GetVal write SetVal;
    property UseActualSize: Boolean index TdxUseFloatingObjectOption.UseActualSize read GetVal write SetVal;
    property UseTextWrapType: Boolean index TdxUseFloatingObjectOption.UseTextWrapType read GetVal write SetVal;
    property UseTextWrapSide: Boolean index TdxUseFloatingObjectOption.UseTextWrapSide read GetVal write SetVal;
    property UseHorizontalPositionType: Boolean index TdxUseFloatingObjectOption.UseHorizontalPositionType read GetVal write SetVal;
    property UseHorizontalPositionAlignment: Boolean index TdxUseFloatingObjectOption.UseHorizontalPositionAlignment read GetVal write SetVal;
    property UseVerticalPositionType: Boolean index TdxUseFloatingObjectOption.UseVerticalPositionType read GetVal write SetVal;
    property UseVerticalPositionAlignment: Boolean index TdxUseFloatingObjectOption.UseVerticalPositionAlignment read GetVal write SetVal;
    property UseOffset: Boolean index TdxUseFloatingObjectOption.UseOffset read GetVal write SetVal;
    property UseIsBehindDoc: Boolean index TdxUseFloatingObjectOption.UseIsBehindDoc read GetVal write SetVal;
    property UsePseudoInline: Boolean index TdxUseFloatingObjectOption.UsePseudoInline read GetVal write SetVal;
    property UseRelativeWidth: Boolean index TdxUseFloatingObjectOption.UseRelativeWidth read GetVal write SetVal;
    property UseRelativeHeight: Boolean index TdxUseFloatingObjectOption.UseRelativeHeight read GetVal write SetVal;
    property UsePercentOffset: Boolean index TdxUseFloatingObjectOption.UsePercentOffset read GetVal write SetVal;
  end;

  { TdxFloatingObjectFormatting }

  TdxFloatingObjectFormatting = class(TdxIndexBasedObjectB<TdxFloatingObjectInfo, TdxFloatingObjectOptions>)
  private
    procedure SetAllowOverlapCore(const AInfo: TdxFloatingObjectInfo; const AValue: Boolean);
    procedure SetHiddenCore(const AInfo: TdxFloatingObjectInfo; const AValue: Boolean);
    procedure SetLayoutInTableCellCore(const AInfo: TdxFloatingObjectInfo; const AValue: Boolean);
    procedure SetLockedCore(const AInfo: TdxFloatingObjectInfo; const AValue: Boolean);
    procedure SetLockAspectRatioCore(const AInfo: TdxFloatingObjectInfo; const AValue: Boolean);
    procedure SetLeftDistanceCore(const AInfo: TdxFloatingObjectInfo; const AValue: Integer);
    procedure SetRightDistanceCore(const AInfo: TdxFloatingObjectInfo; const AValue: Integer);
    procedure SetTopDistanceCore(const AInfo: TdxFloatingObjectInfo; const AValue: Integer);
    procedure SetBottomDistanceCore(const AInfo: TdxFloatingObjectInfo; const AValue: Integer);
    procedure SetZOrderCore(const AInfo: TdxFloatingObjectInfo; const AValue: Integer);
    procedure SetActualSizeCore(const AInfo: TdxFloatingObjectInfo; const AValue: TSize);
    procedure SetRelativeWidthCore(const AInfo: TdxFloatingObjectInfo; const AValue: TdxFloatingObjectRelativeWidth);
    procedure SetRelativeHeightCore(const AInfo: TdxFloatingObjectInfo; const AValue: TdxFloatingObjectRelativeHeight);
    procedure SetTextWrapSideCore(const AInfo: TdxFloatingObjectInfo; const AValue: Integer);
    procedure SetTextWrapTypeCore(const AInfo: TdxFloatingObjectInfo; const AValue: Integer);
    procedure SetHorizontalPositionTypeCore(const AInfo: TdxFloatingObjectInfo; const AValue: Integer);
    procedure SetHorizontalPositionAlignmentCore(const AInfo: TdxFloatingObjectInfo; const AValue: Integer);
    procedure SetVerticalPositionTypeCore(const AInfo: TdxFloatingObjectInfo; const AValue: Integer);
    procedure SetVerticalPositionAlignmentCore(const AInfo: TdxFloatingObjectInfo; const AValue: Integer);
    procedure SetOffsetCore(const AInfo: TdxFloatingObjectInfo; const AValue: TPoint);
    procedure SetPercentOffsetCore(const AInfo: TdxFloatingObjectInfo; const AValue: TPoint);
    procedure SetIsBehindDocCore(const AInfo: TdxFloatingObjectInfo; const AValue: Boolean);
    procedure SetPseudoInlineCore(const AInfo: TdxFloatingObjectInfo; const AValue: Boolean);

    function GetActualSize: TSize;
    function GetAllowOverlap: Boolean;
    function GetBottomDistance: Integer;
    function GetHidden: Boolean;
    function GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
    function GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
    function GetIsBehindDoc: Boolean;
    function GetLayoutInTableCell: Boolean;
    function GetLeftDistance: Integer;
    function GetLockAspectRatio: Boolean;
    function GetLocked: Boolean;
    function GetOffset: TPoint;
    function GetPercentOffset: TPoint;
    function GetPseudoInline: Boolean;
    function GetRelativeHeight: TdxFloatingObjectRelativeHeight;
    function GetRelativeWidth: TdxFloatingObjectRelativeWidth;
    function GetRightDistance: Integer;
    function GetTextWrapSide: TdxFloatingObjectTextWrapSide;
    function GetTextWrapType: TdxFloatingObjectTextWrapType;
    function GetTopDistance: Integer;
    function GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
    function GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
    function GetZOrder: Integer;
    procedure SetActualSize(const Value: TSize);
    procedure SetAllowOverlap(const Value: Boolean);
    procedure SetBottomDistance(const Value: Integer);
    procedure SetHidden(const Value: Boolean);
    procedure SetHorizontalPositionAlignment(const Value: TdxFloatingObjectHorizontalPositionAlignment);
    procedure SetHorizontalPositionType(const Value: TdxFloatingObjectHorizontalPositionType);
    procedure SetIsBehindDoc(const Value: Boolean);
    procedure SetLayoutInTableCell(const Value: Boolean);
    procedure SetLeftDistance(const Value: Integer);
    procedure SetLockAspectRatio(const Value: Boolean);
    procedure SetLocked(const Value: Boolean);
    procedure SetOffset(const Value: TPoint);
    procedure SetPercentOffset(const Value: TPoint);
    procedure SetPseudoInline(const Value: Boolean);
    procedure SetRelativeHeight(const Value: TdxFloatingObjectRelativeHeight);
    procedure SetRelativeWidth(const Value: TdxFloatingObjectRelativeWidth);
    procedure SetRightDistance(const Value: Integer);
    procedure SetTextWrapSide(const Value: TdxFloatingObjectTextWrapSide);
    procedure SetTextWrapType(const Value: TdxFloatingObjectTextWrapType);
    procedure SetTopDistance(const Value: Integer);
    procedure SetVerticalPositionAlignment(const Value: TdxFloatingObjectVerticalPositionAlignment);
    procedure SetVerticalPositionType(const Value: TdxFloatingObjectVerticalPositionType);
    procedure SetZOrder(const Value: Integer);
  protected
    function CanSetPropertyValue: Boolean; override;
    function PropertyEquals(const AOther: TdxIndexBasedObject<TdxFloatingObjectInfo, TdxFloatingObjectOptions>): Boolean; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const ADocumentModel: TdxCustomDocumentModel;
      const AFormattingInfo: TdxFloatingObjectInfo; const AFormattingOptions: TdxFloatingObjectOptions); reintroduce; overload;

    function Clone: TdxCloneable; override;
    procedure CopyFrom(Source: TdxCloneable); overload; override;
    procedure CopyFrom(const AInfo: TdxFloatingObjectInfo; const AOptions: TdxFloatingObjectOptions); reintroduce; overload;

    property AllowOverlap: Boolean read GetAllowOverlap write SetAllowOverlap;
    property Hidden: Boolean read GetHidden write SetHidden;
    property LayoutInTableCell: Boolean read GetLayoutInTableCell write SetLayoutInTableCell;
    property Locked: Boolean read GetLocked write SetLocked;
    property LockAspectRatio: Boolean read GetLockAspectRatio write SetLockAspectRatio;
    property LeftDistance: Integer read GetLeftDistance write SetLeftDistance;
    property RightDistance: Integer read GetRightDistance write SetRightDistance;
    property TopDistance: Integer read GetTopDistance write SetTopDistance;
    property BottomDistance: Integer read GetBottomDistance write SetBottomDistance;
    property ZOrder: Integer read GetZOrder write SetZOrder;
    property ActualSize: TSize read GetActualSize write SetActualSize;
    property RelativeWidth: TdxFloatingObjectRelativeWidth read GetRelativeWidth write SetRelativeWidth;
    property RelativeHeight: TdxFloatingObjectRelativeHeight read GetRelativeHeight write SetRelativeHeight;
    property TextWrapType: TdxFloatingObjectTextWrapType read GetTextWrapType write SetTextWrapType;
    property TextWrapSide: TdxFloatingObjectTextWrapSide read GetTextWrapSide write SetTextWrapSide;
    property HorizontalPositionType: TdxFloatingObjectHorizontalPositionType read GetHorizontalPositionType write SetHorizontalPositionType;
    property HorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment read GetHorizontalPositionAlignment write SetHorizontalPositionAlignment;
    property VerticalPositionType: TdxFloatingObjectVerticalPositionType read GetVerticalPositionType write SetVerticalPositionType;
    property VerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment read GetVerticalPositionAlignment write SetVerticalPositionAlignment;
    property Offset: TPoint read GetOffset write SetOffset;
    property PercentOffset: TPoint read GetPercentOffset write SetPercentOffset;
    property IsBehindDoc: Boolean read GetIsBehindDoc write SetIsBehindDoc;
    property PseudoInline: Boolean read GetPseudoInline write SetPseudoInline;
  end;

  { TdxFloatingObjectInfoCache }

  TdxFloatingObjectInfoCache = class(TdxUniqueItemsCache<TdxFloatingObjectInfo>)
  public const
    DefaultItemIndex = 0;
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxFloatingObjectInfo; override;
  end;

  TdxFloatingObjectChangeType = (
    None = 0,
    AllowOverlap,
    Hidden,
    LayoutInTableCell,
    Locked,
    LockAspectRatio,
    LeftDistance,
    RightDistance,
    TopDistance,
    BottomDistance,
    ZOrder,
    ActualSize,
    TextWrapType,
    TextWrapSide,
    HorizontalPositionType,
    HorizontalPositionAlignment,
    VerticalPositionType,
    VerticalPositionAlignment,
    Offset,
    PercentOffset,
    BatchUpdate,
    IsBehindDoc,
    PseudoInline,
    RelativeWidth,
    RelativeHeight
  );

  { TdxFloatingObjectChangeActionsCalculator }

  TdxFloatingObjectChangeActionsCalculator = class
  public
    class function CalculateChangeActions(AChange: TdxFloatingObjectChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxFloatingObjectProperties }

  TdxFloatingObjectProperties = class(TdxRichEditIndexBasedObject<TdxFloatingObjectFormatting>,
    IdxFloatingObjectLocation, IdxZOrderedObject)
  private
    FOwner: IdxFloatingObjectPropertiesContainer;
    FDisableHistory: Boolean;
    function GetActualHeight: Integer;
    function GetActualSize: TSize;
    function GetActualWidth: Integer;
    function GetAllowOverlap: Boolean;
    function GetBottomDistance: Integer;
    function GetCanPutTextAtLeft: Boolean;
    function GetCanPutTextAtRight: Boolean;
    function GetHidden: Boolean;
    function GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
    function GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
    function GetIsBehindDoc: Boolean;
    function GetLayoutInTableCell: Boolean;
    function GetLeftDistance: Integer;
    function GetLockAspectRatio: Boolean;
    function GetLocked: Boolean;
    function GetOffset: TPoint;
    function GetOffsetX: Integer;
    function GetOffsetY: Integer;
    function GetPercentOffset: TPoint;
    function GetPercentOffsetX: Integer;
    function GetPercentOffsetY: Integer;
    function GetPseudoInline: Boolean;
    function GetPutTextAtLargestSide: Boolean;
    function GetRelativeHeight: TdxFloatingObjectRelativeHeight;
    function GetRelativeWidth: TdxFloatingObjectRelativeWidth;
    function GetRightDistance: Integer;
    function GetTextWrapSide: TdxFloatingObjectTextWrapSide;
    function GetTextWrapType: TdxFloatingObjectTextWrapType;
    function GetTopDistance: Integer;
    function GetUseActualSize: Boolean;
    function GetUseAllowOverlap: Boolean;
    function GetUseBottomDistance: Boolean;
    function GetUseHidden: Boolean;
    function GetUseHorizontalPositionAlignment: Boolean;
    function GetUseHorizontalPositionType: Boolean;
    function GetUseLayoutInTableCell: Boolean;
    function GetUseLeftDistance: Boolean;
    function GetUseLockAspectRatio: Boolean;
    function GetUseLocked: Boolean;
    function GetUseOffset: Boolean;
    function GetUsePercentOffset: Boolean;
    function GetUsePseudoInline: Boolean;
    function GetUseRelativeHeight: Boolean;
    function GetUseRelativeWidth: Boolean;
    function GetUseRightDistance: Boolean;
    function GetUseTextWrapSide: Boolean;
    function GetUseTextWrapType: Boolean;
    function GetUseTopDistance: Boolean;
    function GetUseVerticalPositionAlignment: Boolean;
    function GetUseVerticalPositionType: Boolean;
    function GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
    function GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
    function GetZOrder: Integer;
    procedure SetActualHeight(const Value: Integer);
    procedure SetActualSize(const Value: TSize);
    procedure SetActualWidth(const Value: Integer);
    procedure SetAllowOverlap(const Value: Boolean);
    procedure SetBottomDistance(const Value: Integer);
    procedure SetHidden(const Value: Boolean);
    procedure SetHorizontalPositionAlignment(const Value: TdxFloatingObjectHorizontalPositionAlignment);
    procedure SetHorizontalPositionType(const Value: TdxFloatingObjectHorizontalPositionType);
    procedure SetIsBehindDoc(const Value: Boolean);
    procedure SetLayoutInTableCell(const Value: Boolean);
    procedure SetLeftDistance(const Value: Integer);
    procedure SetLockAspectRatio(const Value: Boolean);
    procedure SetLocked(const Value: Boolean);
    procedure SetOffset(const Value: TPoint);
    procedure SetOffsetX(const Value: Integer);
    procedure SetOffsetY(const Value: Integer);
    procedure SetPercentOffset(const Value: TPoint);
    procedure SetPercentOffsetX(const Value: Integer);
    procedure SetPercentOffsetY(const Value: Integer);
    procedure SetPseudoInline(const Value: Boolean);
    procedure SetRelativeHeight(const Value: TdxFloatingObjectRelativeHeight);
    procedure SetRelativeWidth(const Value: TdxFloatingObjectRelativeWidth);
    procedure SetRightDistance(const Value: Integer);
    procedure SetTextWrapSide(const Value: TdxFloatingObjectTextWrapSide);
    procedure SetTextWrapType(const Value: TdxFloatingObjectTextWrapType);
    procedure SetTopDistance(const Value: Integer);
    procedure SetVerticalPositionAlignment(const Value: TdxFloatingObjectVerticalPositionAlignment);
    procedure SetVerticalPositionType(const Value: TdxFloatingObjectVerticalPositionType);
    procedure SetZOrder(const Value: Integer);

    function InnerGetPieceTable: TdxCustomPieceTable;
  strict protected

    function SetAllowOverlapCore(const AInfo: TdxFloatingObjectFormatting; const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetHiddenCore(const AInfo: TdxFloatingObjectFormatting; const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetLayoutInTableCellCore(const AInfo: TdxFloatingObjectFormatting; const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetLockedCore(const AInfo: TdxFloatingObjectFormatting; const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetLockAspectRatioCore(const AInfo: TdxFloatingObjectFormatting; const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetLeftDistanceCore(const AInfo: TdxFloatingObjectFormatting; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetRightDistanceCore(const AInfo: TdxFloatingObjectFormatting; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetTopDistanceCore(const AInfo: TdxFloatingObjectFormatting; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetBottomDistanceCore(const AInfo: TdxFloatingObjectFormatting; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetZOrderCore(const AInfo: TdxFloatingObjectFormatting; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetActualSizeCore(const AInfo: TdxFloatingObjectFormatting; const AValue: TSize): TdxDocumentModelChangeActions; virtual;
    function SetRelativeWidthCore(const AInfo: TdxFloatingObjectFormatting; const AValue: TdxFloatingObjectRelativeWidth): TdxDocumentModelChangeActions; virtual;
    function SetRelativeHeightCore(const AInfo: TdxFloatingObjectFormatting; const AValue: TdxFloatingObjectRelativeHeight): TdxDocumentModelChangeActions; virtual;
    function SetTextWrapTypeCore(const AInfo: TdxFloatingObjectFormatting; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetTextWrapSideCore(const AInfo: TdxFloatingObjectFormatting; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetHorizontalPositionTypeCore(const AInfo: TdxFloatingObjectFormatting; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetHorizontalPositionAlignmentCore(const AInfo: TdxFloatingObjectFormatting; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetVerticalPositionTypeCore(const AInfo: TdxFloatingObjectFormatting; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetVerticalPositionAlignmentCore(const AInfo: TdxFloatingObjectFormatting; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetOffsetCore(const AInfo: TdxFloatingObjectFormatting; const AValue: TPoint): TdxDocumentModelChangeActions; virtual;
    function SetPercentOffsetCore(const AInfo: TdxFloatingObjectFormatting; const AValue: TPoint): TdxDocumentModelChangeActions; virtual;
    function SetIsBehindDocCore(const AInfo: TdxFloatingObjectFormatting; const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetPseudoInlineCore(const AInfo: TdxFloatingObjectFormatting; const AValue: Boolean): TdxDocumentModelChangeActions; virtual;

    function AllowTextWrapAround: Boolean;

    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxFloatingObjectFormatting>; override;
    function UseVal(AMask: TdxUseFloatingObjectOption): Boolean;
    function CreateIndexChangedHistoryItem: TdxIndexChangedHistoryItemCore; override;
    procedure OnIndexChanged; override;
    procedure OnFirstBeginUpdateCore; override;
    procedure OnLastEndUpdateCore; override;
  public
    constructor Create(const AOwner: IdxFloatingObjectPropertiesContainer); reintroduce;
    procedure ChangeIndexCore(ANewIndex: Integer; const AChangeActions: TdxDocumentModelChangeActions); override;
    procedure Reset;

    property ActualHeight: Integer read GetActualHeight write SetActualHeight;
    property ActualSize: TSize read GetActualSize write SetActualSize;
    property ActualWidth: Integer read GetActualWidth write SetActualWidth;
    property AllowOverlap: Boolean read GetAllowOverlap write SetAllowOverlap;
    property BottomDistance: Integer read GetBottomDistance write SetBottomDistance;
    property CanPutTextAtLeft: Boolean read GetCanPutTextAtLeft;
    property CanPutTextAtRight: Boolean read GetCanPutTextAtRight;
    property DisableHistory: Boolean read FDisableHistory write FDisableHistory;
    property Hidden: Boolean read GetHidden write SetHidden;
    property HorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment read GetHorizontalPositionAlignment write SetHorizontalPositionAlignment;
    property HorizontalPositionType: TdxFloatingObjectHorizontalPositionType read GetHorizontalPositionType write SetHorizontalPositionType;
    property IsBehindDoc: Boolean read GetIsBehindDoc write SetIsBehindDoc;
    property LayoutInTableCell: Boolean read GetLayoutInTableCell write SetLayoutInTableCell;
    property LeftDistance: Integer read GetLeftDistance write SetLeftDistance;
    property LockAspectRatio: Boolean read GetLockAspectRatio write SetLockAspectRatio;
    property Locked: Boolean read GetLocked write SetLocked;
    property Offset: TPoint read GetOffset write SetOffset;
    property OffsetX: Integer read GetOffsetX write SetOffsetX;
    property OffsetY: Integer read GetOffsetY write SetOffsetY;
    property PercentOffset: TPoint read GetPercentOffset write SetPercentOffset;
    property PercentOffsetX: Integer read GetPercentOffsetX write SetPercentOffsetX;
    property PercentOffsetY: Integer read GetPercentOffsetY write SetPercentOffsetY;
    property PieceTable: TdxCustomPieceTable read InnerGetPieceTable;
    property PseudoInline: Boolean read GetPseudoInline write SetPseudoInline;
    property PutTextAtLargestSide: Boolean read GetPutTextAtLargestSide;
    property RelativeHeight: TdxFloatingObjectRelativeHeight read GetRelativeHeight write SetRelativeHeight;
    property RelativeWidth: TdxFloatingObjectRelativeWidth read GetRelativeWidth write SetRelativeWidth;
    property RightDistance: Integer read GetRightDistance write SetRightDistance;
    property TextWrapSide: TdxFloatingObjectTextWrapSide read GetTextWrapSide write SetTextWrapSide;
    property TextWrapType: TdxFloatingObjectTextWrapType read GetTextWrapType write SetTextWrapType;
    property TopDistance: Integer read GetTopDistance write SetTopDistance;
    property UseActualSize: Boolean read GetUseActualSize;
    property UseAllowOverlap: Boolean read GetUseAllowOverlap;
    property UseBottomDistance: Boolean read GetUseBottomDistance;
    property UseHidden: Boolean read GetUseHidden;
    property UseHorizontalPositionAlignment: Boolean read GetUseHorizontalPositionAlignment;
    property UseHorizontalPositionType: Boolean read GetUseHorizontalPositionType;
    property UseLayoutInTableCell: Boolean read GetUseLayoutInTableCell;
    property UseLeftDistance: Boolean read GetUseLeftDistance;
    property UseLockAspectRatio: Boolean read GetUseLockAspectRatio;
    property UseLocked: Boolean read GetUseLocked;
    property UseOffset: Boolean read GetUseOffset;
    property UsePercentOffset: Boolean read GetUsePercentOffset;
    property UsePseudoInline: Boolean read GetUsePseudoInline;
    property UseRelativeHeight: Boolean read GetUseRelativeHeight;
    property UseRelativeWidth: Boolean read GetUseRelativeWidth;
    property UseRightDistance: Boolean read GetUseRightDistance;
    property UseTextWrapSide: Boolean read GetUseTextWrapSide;
    property UseTextWrapType: Boolean read GetUseTextWrapType;
    property UseTopDistance: Boolean read GetUseTopDistance;
    property UseVerticalPositionAlignment: Boolean read GetUseVerticalPositionAlignment;
    property UseVerticalPositionType: Boolean read GetUseVerticalPositionType;
    property VerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment read GetVerticalPositionAlignment write SetVerticalPositionAlignment;
    property VerticalPositionType: TdxFloatingObjectVerticalPositionType read GetVerticalPositionType write SetVerticalPositionType;
    property ZOrder: Integer read GetZOrder write SetZOrder;
  end;

  { TdxFloatingObjectFormattingCache }

  TdxFloatingObjectFormattingCache = class(TdxUniqueItemsCache<TdxFloatingObjectFormatting>)
  public const
    EmptyFloatingObjectFormattingIndex = 0;
  private
    FDefaultFloatingObjectInfo: TdxFloatingObjectInfo;
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxFloatingObjectFormatting; override;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); reintroduce;
    destructor Destroy; override;

    property DefaultFloatingObjectInfo: TdxFloatingObjectInfo read FDefaultFloatingObjectInfo;
  end;

  { TdxZOrderManager }

  TdxZOrderManager = class
  public
    procedure BringToFront(const AObjects: TdxIZOrderedObjectList; AObjectIndex: Integer); virtual;
    procedure BringForward(const AObjects: TdxIZOrderedObjectList; AObjectIndex: Integer); virtual;
    procedure Swap(const AObjects: TdxIZOrderedObjectList; AObjectIndex: Integer; ASecondIndex: Integer);
    procedure IncrementZOrders(const AFloatingObjectList: TdxIZOrderedObjectList; AFrom: Integer; ATo: Integer);
    procedure SendBackward(const AObjects: TdxIZOrderedObjectList; AObjectIndex: Integer); virtual;
    procedure SendToBack(const AObjects: TdxIZOrderedObjectList; AObjectIndex: Integer); virtual;
    procedure Normalize(const AObjects: TdxIZOrderedObjectList);
  end;

  { IdxTextBoxPropertiesContainer }

  IdxTextBoxPropertiesContainer = interface
  ['{3E283966-A8E8-42B8-B575-166B1909C765}']
    function GetPieceTable: TdxCustomPieceTable;
    function CreateTextBoxChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnTextBoxChanged;

    property PieceTable: TdxCustomPieceTable read GetPieceTable;
  end;

  { TdxTextBoxInfo }

  TdxTextBoxInfo = class(TdxCloneable)
  strict private
    FLeftMargin: Integer;
    FRightMargin: Integer;
    FTopMargin: Integer;
    FBottomMargin: Integer;
    FResizeShapeToFitText: Boolean;
    FWrapText: Boolean;
    FUpright: Boolean;
    FVerticalAlignment: TdxVerticalAlignment;
  public
    function Clone: TdxTextBoxInfo; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Equals(Obj: TObject): Boolean; override;

    property LeftMargin: Integer read FLeftMargin write FLeftMargin;
    property RightMargin: Integer read FRightMargin write FRightMargin;
    property TopMargin: Integer read FTopMargin write FTopMargin;
    property BottomMargin: Integer read FBottomMargin write FBottomMargin;
    property ResizeShapeToFitText: Boolean read FResizeShapeToFitText write FResizeShapeToFitText;
    property WrapText: Boolean read FWrapText write FWrapText;
    property VerticalAlignment: TdxVerticalAlignment read FVerticalAlignment write FVerticalAlignment;
    property Upright: Boolean read FUpright write FUpright;
  end;

  TdxUseTextBoxOption = (
    UseLeftMargin,
    UseRightMargin,
    UseTopMargin,
    UseBottomMargin,
    UseResizeShapeToFitText,
    UseWrapText,
    UseVerticalAlignment,
    UseUpright
  );

  TdxUseTextBoxOptions = set of TdxUseTextBoxOption;

  { TdxTextBoxOptions }

  TdxTextBoxOptions = record
  public const
    MaskUseNone = [];
    MaskUseAll  = [Low(TdxUseTextBoxOption)..High(TdxUseTextBoxOption)];
  strict private
    FVal: TdxUseTextBoxOptions;
    function GetVal(AMask: TdxUseTextBoxOption): Boolean;
    procedure SetVal(AMask: TdxUseTextBoxOption; AValue: Boolean);

    class function GetEmptyTextBoxOption: TdxTextBoxOptions; static;
  public
    constructor Create(const AVal: TdxUseTextBoxOptions);
    class operator Equal(const A, B: TdxTextBoxOptions): Boolean;
    function Clone: TdxTextBoxOptions;
    procedure CopyFrom(const Source: TdxTextBoxOptions);
    function GetHashCode: Integer;

    class property EmptyTextBoxOption: TdxTextBoxOptions read GetEmptyTextBoxOption;
    property Value: TdxUseTextBoxOptions read FVal write FVal;

    property UseLeftMargin: Boolean index TdxUseTextBoxOption.UseLeftMargin read GetVal write SetVal;
    property UseRightMargin: Boolean index TdxUseTextBoxOption.UseRightMargin read GetVal write SetVal;
    property UseTopMargin: Boolean index TdxUseTextBoxOption.UseTopMargin read GetVal write SetVal;
    property UseBottomMargin: Boolean index TdxUseTextBoxOption.UseBottomMargin read GetVal write SetVal;
    property UseResizeShapeToFitText: Boolean index TdxUseTextBoxOption.UseResizeShapeToFitText read GetVal write SetVal;
    property UseWrapText: Boolean index TdxUseTextBoxOption.UseWrapText read GetVal write SetVal;
    property UseVerticalAlignment: Boolean index TdxUseTextBoxOption.UseVerticalAlignment read GetVal write SetVal;
    property UseUpright: Boolean index TdxUseTextBoxOption.UseUpright read GetVal write SetVal;
  end;

  { TdxTextBoxFormatting }

  TdxTextBoxFormatting = class(TdxIndexBasedObjectB<TdxTextBoxInfo, TdxTextBoxOptions>)
  strict private
    procedure SetLeftMarginCore(const AInfo: TdxTextBoxInfo; const AValue: Integer);
    procedure SetRightMarginCore(const AInfo: TdxTextBoxInfo; const AValue: Integer);
    procedure SetTopMarginCore(const AInfo: TdxTextBoxInfo; const AValue: Integer);
    procedure SetBottomMarginCore(const AInfo: TdxTextBoxInfo; const AValue: Integer);
    procedure SetResizeShapeToFitTextCore(const AInfo: TdxTextBoxInfo; const AValue: Boolean);
    procedure SetWrapTextCore(const AInfo: TdxTextBoxInfo; const AValue: Boolean);
    procedure SetVerticalAlignmentCore(const AInfo: TdxTextBoxInfo; const AValue: Integer);
    procedure SetUprightCore(const AInfo: TdxTextBoxInfo; const AValue: Boolean);

    function GetBottomMargin: Integer;
    function GetLeftMargin: Integer;
    function GetResizeShapeToFitText: Boolean;
    function GetRightMargin: Integer;
    function GetTopMargin: Integer;
    function GetUpright: Boolean;
    function GetVerticalAlignment: TdxVerticalAlignment;
    function GetWrapText: Boolean;
    procedure SetBottomMargin(const Value: Integer);
    procedure SetLeftMargin(const Value: Integer);
    procedure SetResizeShapeToFitText(const Value: Boolean);
    procedure SetRightMargin(const Value: Integer);
    procedure SetTopMargin(const Value: Integer);
    procedure SetUpright(const Value: Boolean);
    procedure SetVerticalAlignment(const Value: TdxVerticalAlignment);
    procedure SetWrapText(const Value: Boolean);
  protected
    function CanSetPropertyValue: Boolean; override;
    function PropertyEquals(const AOther: TdxIndexBasedObject<TdxTextBoxInfo, TdxTextBoxOptions>): Boolean; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const ADocumentModel: TdxCustomDocumentModel;
      const AFormattingInfo: TdxTextBoxInfo; const AFormattingOptions: TdxTextBoxOptions); reintroduce;

    function Clone: TdxCloneable; override;
    procedure CopyFrom(Source: TdxCloneable); overload; override;
    procedure CopyFrom(const AInfo: TdxTextBoxInfo; const AOptions: TdxTextBoxOptions); reintroduce; overload;

    property LeftMargin: Integer read GetLeftMargin write SetLeftMargin;
    property RightMargin: Integer read GetRightMargin write SetRightMargin;
    property TopMargin: Integer read GetTopMargin write SetTopMargin;
    property BottomMargin: Integer read GetBottomMargin write SetBottomMargin;
    property ResizeShapeToFitText: Boolean read GetResizeShapeToFitText write SetResizeShapeToFitText;
    property WrapText: Boolean read GetWrapText write SetWrapText;
    property VerticalAlignment: TdxVerticalAlignment read GetVerticalAlignment write SetVerticalAlignment;
    property Upright: Boolean read GetUpright write SetUpright;
  end;

  { TdxTextBoxProperties }

  TdxTextBoxProperties = class(TdxRichEditIndexBasedObject<TdxTextBoxFormatting>)
  strict private
    FOwner: IdxTextBoxPropertiesContainer;
    class function GetPieceTable(const AOwner: IdxTextBoxPropertiesContainer): TdxCustomPieceTable; static;
    function GetLeftMargin: Integer;
    procedure SetLeftMargin(const AValue: Integer);
    function GetUseLeftMargin: Boolean;
    function GetRightMargin: Integer;
    procedure SetRightMargin(const AValue: Integer);
    function GetUseRightMargin: Boolean;
    function GetTopMargin: Integer;
    procedure SetTopMargin(const AValue: Integer);
    function GetUseTopMargin: Boolean;
    function GetBottomMargin: Integer;
    procedure SetBottomMargin(const AValue: Integer);
    function GetUseBottomMargin: Boolean;
    function GetResizeShapeToFitText: Boolean;
    procedure SetResizeShapeToFitText(const AValue: Boolean);
    function GetUseResizeShapeToFitText: Boolean;
    function GetWrapText: Boolean;
    procedure SetWrapText(const AValue: Boolean);
    function GetUseWrapText: Boolean;
    function GetVerticalAlignment: TdxVerticalAlignment;
    procedure SetVerticalAlignment(const AValue: TdxVerticalAlignment);
    function GetUseVerticalAlignment: Boolean;
    function GetUpright: Boolean;
    procedure SetUpright(const AValue: Boolean);
    function GetUseUpright: Boolean;
  strict protected
    function SetLeftMarginCore(const AInfo: TdxTextBoxFormatting; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetRightMarginCore(const AInfo: TdxTextBoxFormatting; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetTopMarginCore(const AInfo: TdxTextBoxFormatting; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetBottomMarginCore(const AInfo: TdxTextBoxFormatting; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetResizeShapeToFitTextCore(const AInfo: TdxTextBoxFormatting; const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetWrapTextCore(const AInfo: TdxTextBoxFormatting; const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetVerticalAlignmentCore(const AInfo: TdxTextBoxFormatting; const AValue: TdxVerticalAlignment): TdxDocumentModelChangeActions; virtual;
    function SetUprightCore(const AInfo: TdxTextBoxFormatting; const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxTextBoxFormatting>; override;
    function UseVal(AMask: TdxUseTextBoxOption): Boolean;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    procedure OnIndexChanged; override;
    function CreateIndexChangedHistoryItem: TdxIndexChangedHistoryItemCore; override;
  public
    constructor Create(const AOwner: IdxTextBoxPropertiesContainer); reintroduce;
    procedure Reset;
    function Equals(AObj: TObject): Boolean; override;
    procedure ResetUse(AMask: TdxUseTextBoxOptions);
    procedure ResetAllUse;
    function GetHashCode: Integer; override;

    property LeftMargin: Integer read GetLeftMargin write SetLeftMargin;
    property UseLeftMargin: Boolean read GetUseLeftMargin;
    property RightMargin: Integer read GetRightMargin write SetRightMargin;
    property UseRightMargin: Boolean read GetUseRightMargin;
    property TopMargin: Integer read GetTopMargin write SetTopMargin;
    property UseTopMargin: Boolean read GetUseTopMargin;
    property BottomMargin: Integer read GetBottomMargin write SetBottomMargin;
    property UseBottomMargin: Boolean read GetUseBottomMargin;
    property ResizeShapeToFitText: Boolean read GetResizeShapeToFitText write SetResizeShapeToFitText;
    property UseResizeShapeToFitText: Boolean read GetUseResizeShapeToFitText;
    property WrapText: Boolean read GetWrapText write SetWrapText;
    property UseWrapText: Boolean read GetUseWrapText;
    property VerticalAlignment: TdxVerticalAlignment read GetVerticalAlignment write SetVerticalAlignment;
    property UseVerticalAlignment: Boolean read GetUseVerticalAlignment;
    property Upright: Boolean read GetUpright write SetUpright;
    property UseUpright: Boolean read GetUseUpright;
  end;

  { TdxTextBoxFormattingCache }

  TdxTextBoxFormattingCache = class(TdxUniqueItemsCache<TdxTextBoxFormatting>)
  public const
    EmptyTextBoxFormattingIndex = 0;
  strict private
    FDefaultTextBoxInfo: TdxTextBoxInfo;
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxTextBoxFormatting; override;
    function CreateDefaultInfoItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxTextBoxInfo;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); reintroduce;
    destructor Destroy; override;

    property DefaultTextBoxInfo: TdxTextBoxInfo read FDefaultTextBoxInfo;
  end;

  { TdxTextBoxInfoCache }

  TdxTextBoxInfoCache = class(TdxUniqueItemsCache<TdxTextBoxInfo>)
  public const
    DefaultItemIndex = 0;
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxTextBoxInfo; override;
    class function CreateDefaultItemCore(const AUnitConverter: IdxDocumentModelUnitConverter): TdxTextBoxInfo; static;
  end;

implementation

uses
  RTLConsts, Math, cxGeometry, dxTypeHelpers,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.DocumentModel.Cache;

type
  { TdxTextBoxPropertiesChangeActionsCalculator }

  TdxTextBoxPropertiesChangeActionsCalculator = class
  public
    class function CalculateChangeActions(AChange: TdxTextBoxPropertiesChangeType): TdxDocumentModelChangeActions; static;
  end;

{ TdxTextBoxPropertiesChangeActionsCalculator }

class function TdxTextBoxPropertiesChangeActionsCalculator.CalculateChangeActions(AChange: TdxTextBoxPropertiesChangeType): TdxDocumentModelChangeActions;
const
  TextBoxPropertiesChangeActionsMap: array[TdxTextBoxPropertiesChangeType] of TdxDocumentModelChangeActions = (
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
  Result := TextBoxPropertiesChangeActionsMap[AChange];
end;

{ TdxFloatingObjectRelativeWidth }

constructor TdxFloatingObjectRelativeWidth.Create(AFrom: TdxFloatingObjectRelativeFromHorizontal;
  AWidth: Integer);
begin
  FWidth := AWidth;
  FFrom := AFrom;
end;

class function TdxFloatingObjectRelativeWidth.CreateEmpty: TdxFloatingObjectRelativeWidth;
begin
  Result.FFrom := TdxFloatingObjectRelativeFromHorizontal.Margin;
  Result.FWidth := 0;
end;

class operator TdxFloatingObjectRelativeWidth.Equal(const A, B: TdxFloatingObjectRelativeWidth): Boolean;
begin
  Result := (A.Width = B.Width) and (A.From = B.From);
end;

class operator TdxFloatingObjectRelativeWidth.NotEqual(const A, B: TdxFloatingObjectRelativeWidth): Boolean;
begin
  Result := not (A = B);
end;


{ TdxFloatingObjectRelativeHeight }

constructor TdxFloatingObjectRelativeHeight.Create(AFrom: TdxFloatingObjectRelativeFromVertical;
  AHeight: Integer);
begin
  FHeight := AHeight;
  FFrom := AFrom;
end;

class function TdxFloatingObjectRelativeHeight.CreateEmpty: TdxFloatingObjectRelativeHeight;
begin
  Result.FFrom := TdxFloatingObjectRelativeFromVertical.Margin;
  Result.FHeight := 0;
end;

class operator TdxFloatingObjectRelativeHeight.Equal(const A, B: TdxFloatingObjectRelativeHeight): Boolean;
begin
  Result := (A.Height = B.Height) and (A.From = B.From);
end;

class operator TdxFloatingObjectRelativeHeight.NotEqual(const A, B: TdxFloatingObjectRelativeHeight): Boolean;
begin
  Result := not (A = B);
end;

{ TdxFloatingObjectInfo }

function TdxFloatingObjectInfo.Clone: TdxFloatingObjectInfo;
begin
  Result := TdxFloatingObjectInfo(inherited Clone);
end;

procedure TdxFloatingObjectInfo.CopyFrom(Source: TdxCloneable);
var
  AFloatingObjectInfo: TdxFloatingObjectInfo absolute Source;
begin
  AllowOverlap := AFloatingObjectInfo.AllowOverlap;
  Hidden := AFloatingObjectInfo.Hidden;
  LayoutInTableCell := AFloatingObjectInfo.LayoutInTableCell;
  Locked := AFloatingObjectInfo.Locked;
  LockAspectRatio := AFloatingObjectInfo.LockAspectRatio;
  LeftDistance := AFloatingObjectInfo.LeftDistance;
  RightDistance := AFloatingObjectInfo.RightDistance;
  TopDistance := AFloatingObjectInfo.TopDistance;
  BottomDistance := AFloatingObjectInfo.BottomDistance;
  ZOrder := AFloatingObjectInfo.ZOrder;
  ActualSize := AFloatingObjectInfo.ActualSize;
  TextWrapType := AFloatingObjectInfo.TextWrapType;
  TextWrapSide := AFloatingObjectInfo.TextWrapSide;
  HorizontalPositionType := AFloatingObjectInfo.HorizontalPositionType;
  HorizontalPositionAlignment := AFloatingObjectInfo.HorizontalPositionAlignment;
  VerticalPositionType := AFloatingObjectInfo.VerticalPositionType;
  VerticalPositionAlignment := AFloatingObjectInfo.VerticalPositionAlignment;
  Offset := AFloatingObjectInfo.Offset;
  IsBehindDoc := AFloatingObjectInfo.IsBehindDoc;
  PseudoInline := AFloatingObjectInfo.PseudoInline;
  RelativeWidth := AFloatingObjectInfo.RelativeWidth;
  RelativeHeight := AFloatingObjectInfo.RelativeHeight;
  PercentOffset := AFloatingObjectInfo.PercentOffset;
end;

function TdxFloatingObjectInfo.Equals(Obj: TObject): Boolean;
var
  AInfo: TdxFloatingObjectInfo absolute Obj;
begin
  Result :=
    Offset.IsEqual(AInfo.Offset) and
    ActualSize.IsEqual(AInfo.ActualSize) and
    (AllowOverlap = AInfo.AllowOverlap) and
    (Hidden = AInfo.Hidden) and
    (LayoutInTableCell = AInfo.LayoutInTableCell) and
    (Locked = AInfo.Locked) and
    (LockAspectRatio = AInfo.LockAspectRatio) and
    (LeftDistance = AInfo.LeftDistance) and
    (RightDistance = AInfo.RightDistance) and
    (TopDistance = AInfo.TopDistance) and
    (BottomDistance = AInfo.BottomDistance) and
    (ZOrder = AInfo.ZOrder) and
    (TextWrapType = AInfo.TextWrapType) and
    (TextWrapSide = AInfo.TextWrapSide) and
    (HorizontalPositionType = AInfo.HorizontalPositionType) and
    (HorizontalPositionAlignment = AInfo.HorizontalPositionAlignment) and
    (VerticalPositionType = AInfo.VerticalPositionType) and
    (VerticalPositionAlignment = AInfo.VerticalPositionAlignment) and
    (IsBehindDoc = AInfo.IsBehindDoc) and
    (PseudoInline = AInfo.PseudoInline) and
    (RelativeWidth = AInfo.RelativeWidth) and
    (RelativeHeight = AInfo.RelativeHeight) and
    PercentOffset.IsEqual(AInfo.PercentOffset);
end;

{ TdxFloatingObjectOptions }

constructor TdxFloatingObjectOptions.Create(const AVal: TdxUseFloatingObjectOptions);
begin
  FVal := AVal;
end;

class operator TdxFloatingObjectOptions.Equal(const A, B: TdxFloatingObjectOptions): Boolean;
begin
  Result := A.FVal = B.FVal;
end;

procedure TdxFloatingObjectOptions.CopyFrom(const Source: TdxFloatingObjectOptions);
begin
  FVal := Source.Value;
end;

class function TdxFloatingObjectOptions.GetEmptyFloatingObjectOption: TdxFloatingObjectOptions;
begin
  Result := TdxFloatingObjectOptions.Create([]);
end;

function TdxFloatingObjectOptions.GetHashCode: Integer;
begin
  Result := Integer(Value);
end;

function TdxFloatingObjectOptions.GetVal(AMask: TdxUseFloatingObjectOption): Boolean;
begin
  Result := AMask in FVal;
end;

function TdxFloatingObjectOptions.Clone: TdxFloatingObjectOptions;
begin
  Result := TdxFloatingObjectOptions.Create(FVal);
end;

procedure TdxFloatingObjectOptions.SetVal(AMask: TdxUseFloatingObjectOption; AValue: Boolean);
begin
  if AValue then
    Include(FVal, AMask)
  else
    Exclude(FVal, AMask);
end;

{ TdxFloatingObjectFormatting }

constructor TdxFloatingObjectFormatting.Create(APieceTable: TdxCustomPieceTable; const ADocumentModel: TdxCustomDocumentModel;
  const AFormattingInfo: TdxFloatingObjectInfo; const AFormattingOptions: TdxFloatingObjectOptions);
begin
  inherited Create(APieceTable, ADocumentModel, AFormattingInfo, AFormattingOptions);
end;

function TdxFloatingObjectFormatting.Clone: TdxCloneable;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxFloatingObjectFormatting.Create(PieceTable, DocumentModel, Info, Options);
end;

function TdxFloatingObjectFormatting.GetActualSize: TSize;
begin
  Result := Info.ActualSize;
end;

function TdxFloatingObjectFormatting.GetAllowOverlap: Boolean;
begin
  Result := Info.AllowOverlap;
end;

function TdxFloatingObjectFormatting.GetBottomDistance: Integer;
begin
  Result := Info.BottomDistance;
end;

function TdxFloatingObjectFormatting.GetHidden: Boolean;
begin
  Result := Info.Hidden;
end;

function TdxFloatingObjectFormatting.GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
begin
  Result := Info.HorizontalPositionAlignment;
end;

function TdxFloatingObjectFormatting.GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
begin
  Result := Info.HorizontalPositionType;
end;

function TdxFloatingObjectFormatting.GetIsBehindDoc: Boolean;
begin
  Result := Info.IsBehindDoc;
end;

function TdxFloatingObjectFormatting.GetLayoutInTableCell: Boolean;
begin
  Result := Info.LayoutInTableCell;
end;

function TdxFloatingObjectFormatting.GetLeftDistance: Integer;
begin
  Result := Info.LeftDistance;
end;

function TdxFloatingObjectFormatting.GetLockAspectRatio: Boolean;
begin
  Result := Info.LockAspectRatio;
end;

function TdxFloatingObjectFormatting.GetLocked: Boolean;
begin
  Result := Info.Locked;
end;

function TdxFloatingObjectFormatting.GetOffset: TPoint;
begin
  Result := Info.Offset;
end;

function TdxFloatingObjectFormatting.GetPercentOffset: TPoint;
begin
  Result := Info.PercentOffset;
end;

function TdxFloatingObjectFormatting.GetPseudoInline: Boolean;
begin
  Result := Info.PseudoInline;
end;

function TdxFloatingObjectFormatting.GetRelativeHeight: TdxFloatingObjectRelativeHeight;
begin
  Result := Info.RelativeHeight;
end;

function TdxFloatingObjectFormatting.GetRelativeWidth: TdxFloatingObjectRelativeWidth;
begin
  Result := Info.RelativeWidth;
end;

function TdxFloatingObjectFormatting.GetRightDistance: Integer;
begin
  Result := Info.RightDistance;
end;

function TdxFloatingObjectFormatting.GetTextWrapSide: TdxFloatingObjectTextWrapSide;
begin
  Result := Info.TextWrapSide;
end;

function TdxFloatingObjectFormatting.GetTextWrapType: TdxFloatingObjectTextWrapType;
begin
  Result := Info.TextWrapType;
end;

function TdxFloatingObjectFormatting.GetTopDistance: Integer;
begin
  Result := Info.TopDistance;
end;

function TdxFloatingObjectFormatting.GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
begin
  Result := Info.VerticalPositionAlignment;
end;

function TdxFloatingObjectFormatting.GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
begin
  Result := Info.VerticalPositionType;
end;

function TdxFloatingObjectFormatting.GetZOrder: Integer;
begin
  Result := Info.ZOrder;
end;

procedure TdxFloatingObjectFormatting.SetActualSize(const Value: TSize);
begin
  if cxSizeIsEqual(Info.ActualSize, Value) and Options.UseActualSize then
    Exit;
  SetPropertyValue<TSize>(SetActualSizeCore, Value);
  Options.UseActualSize := True;
  Options.UseRelativeWidth := False;
  Options.UseRelativeHeight := False;
end;

procedure TdxFloatingObjectFormatting.SetActualSizeCore(const AInfo: TdxFloatingObjectInfo; const AValue: TSize);
begin
  AInfo.ActualSize := AValue;
end;

procedure TdxFloatingObjectFormatting.SetAllowOverlap(const Value: Boolean);
begin
  if (Info.AllowOverlap = Value) and Options.UseAllowOverlap then
    Exit;
  SetPropertyValue<Boolean>(SetAllowOverlapCore, Value);
  Options.UseAllowOverlap := True;
end;

procedure TdxFloatingObjectFormatting.SetAllowOverlapCore(const AInfo: TdxFloatingObjectInfo; const AValue: Boolean);
begin
  AInfo.AllowOverlap := AValue;
end;

procedure TdxFloatingObjectFormatting.SetBottomDistance(const Value: Integer);
begin
  if (Info.BottomDistance = Value) and Options.UseBottomDistance then
    Exit;
  SetPropertyValue<Integer>(SetBottomDistanceCore, Value);
  Options.UseBottomDistance := True;
end;

procedure TdxFloatingObjectFormatting.SetBottomDistanceCore(const AInfo: TdxFloatingObjectInfo; const AValue: Integer);
begin
  AInfo.BottomDistance := AValue;
end;

procedure TdxFloatingObjectFormatting.SetHidden(const Value: Boolean);
begin
  if (Info.Hidden = Value) and Options.UseHidden then
    Exit;
  SetPropertyValue<Boolean>(SetHiddenCore, Value);
  Options.UseHidden := True;
end;

procedure TdxFloatingObjectFormatting.SetHiddenCore(const AInfo: TdxFloatingObjectInfo; const AValue: Boolean);
begin
  AInfo.Hidden := AValue;
end;

procedure TdxFloatingObjectFormatting.SetHorizontalPositionAlignment(
  const Value: TdxFloatingObjectHorizontalPositionAlignment);
begin
  if (Info.HorizontalPositionAlignment = Value) and Options.UseHorizontalPositionAlignment then
    Exit;
  SetPropertyValue<Integer>(SetHorizontalPositionAlignmentCore, Ord(Value));
  Options.UseHorizontalPositionAlignment := True;
end;

procedure TdxFloatingObjectFormatting.SetHorizontalPositionAlignmentCore(const AInfo: TdxFloatingObjectInfo;
  const AValue: Integer);
begin
  AInfo.HorizontalPositionAlignment := TdxFloatingObjectHorizontalPositionAlignment(AValue);
end;

procedure TdxFloatingObjectFormatting.SetHorizontalPositionType(const Value: TdxFloatingObjectHorizontalPositionType);
begin
  if (Info.HorizontalPositionType = Value) and Options.UseHorizontalPositionType then
    Exit;
  SetPropertyValue<Integer>(SetHorizontalPositionTypeCore, Ord(Value));
  Options.UseHorizontalPositionType := True;
end;

procedure TdxFloatingObjectFormatting.SetHorizontalPositionTypeCore(const AInfo: TdxFloatingObjectInfo;
  const AValue: Integer);
begin
  AInfo.HorizontalPositionType := TdxFloatingObjectHorizontalPositionType(AValue);
end;

procedure TdxFloatingObjectFormatting.SetIsBehindDoc(const Value: Boolean);
begin
  if (Info.IsBehindDoc = Value) and Options.UseIsBehindDoc then
    Exit;
  SetPropertyValue<Boolean>(SetIsBehindDocCore, Value);
  Options.UseIsBehindDoc := True;
end;

procedure TdxFloatingObjectFormatting.SetIsBehindDocCore(const AInfo: TdxFloatingObjectInfo; const AValue: Boolean);
begin
  AInfo.IsBehindDoc := AValue;
end;

procedure TdxFloatingObjectFormatting.SetLayoutInTableCell(const Value: Boolean);
begin
  if (Info.LayoutInTableCell = Value) and Options.UseLayoutInTableCell then
    Exit;
  SetPropertyValue<Boolean>(SetLayoutInTableCellCore, Value);
  Options.UseLayoutInTableCell := True;
end;

procedure TdxFloatingObjectFormatting.SetLayoutInTableCellCore(const AInfo: TdxFloatingObjectInfo; const AValue: Boolean);
begin
  AInfo.LayoutInTableCell := AValue;
end;

procedure TdxFloatingObjectFormatting.SetLeftDistance(const Value: Integer);
begin
  if (Info.LeftDistance = Value) and Options.UseLeftDistance then
    Exit;
  SetPropertyValue<Integer>(SetLeftDistanceCore, Value);
  Options.UseLeftDistance := True;
end;

procedure TdxFloatingObjectFormatting.SetLeftDistanceCore(const AInfo: TdxFloatingObjectInfo; const AValue: Integer);
begin
  AInfo.LeftDistance := AValue;
end;

procedure TdxFloatingObjectFormatting.SetLockAspectRatio(const Value: Boolean);
begin
  if (Info.LockAspectRatio = Value) and Options.UseLockAspectRatio then
    Exit;
  SetPropertyValue<Boolean>(SetLockAspectRatioCore, Value);
  Options.UseLockAspectRatio := True;
end;

procedure TdxFloatingObjectFormatting.SetLockAspectRatioCore(const AInfo: TdxFloatingObjectInfo; const AValue: Boolean);
begin
  AInfo.LockAspectRatio := AValue;
end;

procedure TdxFloatingObjectFormatting.SetLocked(const Value: Boolean);
begin
  if (Info.Locked = Value) and Options.UseLocked then
    Exit;
  SetPropertyValue<Boolean>(SetLockedCore, Value);
  Options.UseLocked := True;
end;

procedure TdxFloatingObjectFormatting.SetLockedCore(const AInfo: TdxFloatingObjectInfo; const AValue: Boolean);
begin
  AInfo.Locked := AValue;
end;

procedure TdxFloatingObjectFormatting.SetOffset(const Value: TPoint);
begin
  if cxPointIsEqual(Info.Offset, Value) and Options.UseOffset then
    Exit;
  SetPropertyValue<TPoint>(SetOffsetCore, Value);
  Options.UseOffset := True;
end;

procedure TdxFloatingObjectFormatting.SetOffsetCore(const AInfo: TdxFloatingObjectInfo; const AValue: TPoint);
begin
  AInfo.Offset := AValue;
end;

procedure TdxFloatingObjectFormatting.SetPercentOffset(const Value: TPoint);
begin
  if cxPointIsEqual(Info.PercentOffset, Value) and Options.UsePercentOffset then
    Exit;
  SetPropertyValue<TPoint>(SetPercentOffsetCore, Value);
  Options.UsePercentOffset := True;
end;

procedure TdxFloatingObjectFormatting.SetPercentOffsetCore(const AInfo: TdxFloatingObjectInfo; const AValue: TPoint);
begin
  AInfo.PercentOffset := AValue;
end;

function TdxFloatingObjectFormatting.CanSetPropertyValue: Boolean;
begin
  Result := DocumentModel.DocumentCapabilities.FloatingObjectsAllowed;
end;

function TdxFloatingObjectFormatting.PropertyEquals(const AOther: TdxIndexBasedObject<TdxFloatingObjectInfo, TdxFloatingObjectOptions>): Boolean;
begin
  Result := (Options.Value = AOther.Options.Value) and
    Info.Equals(AOther.Info);
end;

procedure TdxFloatingObjectFormatting.SetPseudoInline(const Value: Boolean);
begin
  if (Info.PseudoInline = Value) and Options.UsePseudoInline then
    Exit;
  SetPropertyValue<Boolean>(SetPseudoInlineCore, Value);
  Options.UsePseudoInline := True;
end;

procedure TdxFloatingObjectFormatting.SetPseudoInlineCore(const AInfo: TdxFloatingObjectInfo; const AValue: Boolean);
begin
  AInfo.PseudoInline := AValue;
end;

procedure TdxFloatingObjectFormatting.SetRelativeHeight(const Value: TdxFloatingObjectRelativeHeight);
begin
  if (Info.RelativeHeight = Value) and Options.UseRelativeHeight then
    Exit;
  SetPropertyValue<TdxFloatingObjectRelativeHeight>(SetRelativeHeightCore, Value);
  Options.UseRelativeHeight := True;
end;

procedure TdxFloatingObjectFormatting.SetRelativeHeightCore(const AInfo: TdxFloatingObjectInfo;
  const AValue: TdxFloatingObjectRelativeHeight);
begin
  AInfo.RelativeHeight := AValue;
end;

procedure TdxFloatingObjectFormatting.SetRelativeWidth(const Value: TdxFloatingObjectRelativeWidth);
begin
  if (Info.RelativeWidth = Value) and Options.UseRelativeWidth then
    Exit;
  SetPropertyValue<TdxFloatingObjectRelativeWidth>(SetRelativeWidthCore, Value);
  Options.UseRelativeWidth := True;
end;

procedure TdxFloatingObjectFormatting.SetRelativeWidthCore(const AInfo: TdxFloatingObjectInfo;
  const AValue: TdxFloatingObjectRelativeWidth);
begin
  AInfo.RelativeWidth := AValue;
end;

procedure TdxFloatingObjectFormatting.SetRightDistance(const Value: Integer);
begin
  if (Info.RightDistance = Value) and Options.UseRightDistance then
    Exit;
  SetPropertyValue<Integer>(SetRightDistanceCore, Value);
  Options.UseRightDistance := True;
end;

procedure TdxFloatingObjectFormatting.SetRightDistanceCore(const AInfo: TdxFloatingObjectInfo; const AValue: Integer);
begin
  AInfo.RightDistance := AValue;
end;

procedure TdxFloatingObjectFormatting.SetTextWrapSide(const Value: TdxFloatingObjectTextWrapSide);
begin
  if (Info.TextWrapSide = Value) and Options.UseTextWrapSide then
    Exit;
  SetPropertyValue<Integer>(SetTextWrapSideCore, Ord(Value));
  Options.UseTextWrapSide := True;
end;

procedure TdxFloatingObjectFormatting.SetTextWrapSideCore(const AInfo: TdxFloatingObjectInfo;
  const AValue: Integer);
begin
  AInfo.TextWrapSide := TdxFloatingObjectTextWrapSide(AValue);
end;

procedure TdxFloatingObjectFormatting.SetTextWrapType(const Value: TdxFloatingObjectTextWrapType);
begin
  if (Info.TextWrapType = Value) and Options.UseTextWrapType then
    Exit;
  SetPropertyValue<Integer>(SetTextWrapTypeCore, Ord(Value));
  Options.UseTextWrapType := True;
end;

procedure TdxFloatingObjectFormatting.SetTextWrapTypeCore(const AInfo: TdxFloatingObjectInfo; const AValue: Integer);
begin
  AInfo.TextWrapType := TdxFloatingObjectTextWrapType(AValue);
end;

procedure TdxFloatingObjectFormatting.SetTopDistance(const Value: Integer);
begin
  if (Info.TopDistance = Value) and Options.UseTopDistance then
    Exit;
  SetPropertyValue<Integer>(SetTopDistanceCore, Value);
  Options.UseTopDistance := True;
end;

procedure TdxFloatingObjectFormatting.SetTopDistanceCore(const AInfo: TdxFloatingObjectInfo; const AValue: Integer);
begin
  AInfo.TopDistance := AValue;
end;

procedure TdxFloatingObjectFormatting.SetVerticalPositionAlignment(
  const Value: TdxFloatingObjectVerticalPositionAlignment);
begin
  if (Info.VerticalPositionAlignment = Value) and Options.UseVerticalPositionAlignment then
    Exit;
  SetPropertyValue<Integer>(SetVerticalPositionAlignmentCore, Ord(Value));
  Options.UseVerticalPositionAlignment := True;
end;

procedure TdxFloatingObjectFormatting.SetVerticalPositionAlignmentCore(const AInfo: TdxFloatingObjectInfo;
  const AValue: Integer);
begin
  AInfo.VerticalPositionAlignment := TdxFloatingObjectVerticalPositionAlignment(AValue);
end;

procedure TdxFloatingObjectFormatting.SetVerticalPositionType(const Value: TdxFloatingObjectVerticalPositionType);
begin
  if (Info.VerticalPositionType = Value) and Options.UseVerticalPositionType then
    Exit;
  SetPropertyValue<Integer>(SetVerticalPositionTypeCore, Ord(Value));
  Options.UseVerticalPositionType := True;
end;

procedure TdxFloatingObjectFormatting.SetVerticalPositionTypeCore(const AInfo: TdxFloatingObjectInfo;
  const AValue: Integer);
begin
  AInfo.VerticalPositionType := TdxFloatingObjectVerticalPositionType(AValue);
end;

procedure TdxFloatingObjectFormatting.SetZOrder(const Value: Integer);
begin
  if (Info.ZOrder = Value) and Options.UseZOrder then
    Exit;
  SetPropertyValue<Integer>(SetZOrderCore, Value);
  Options.UseZOrder := True;
end;

procedure TdxFloatingObjectFormatting.SetZOrderCore(const AInfo: TdxFloatingObjectInfo; const AValue: Integer);
begin
  AInfo.ZOrder := AValue;
end;

procedure TdxFloatingObjectFormatting.CopyFrom(Source: TdxCloneable);
var
  AFloatingObjectFormatting: TdxFloatingObjectFormatting absolute Source;
begin
  CopyFrom(AFloatingObjectFormatting.Info, AFloatingObjectFormatting.Options);
end;

procedure TdxFloatingObjectFormatting.CopyFrom(const AInfo: TdxFloatingObjectInfo; const AOptions: TdxFloatingObjectOptions);
begin
  CopyFromCore(AInfo, AOptions);
end;

{ TdxFloatingObjectInfoCache }

function TdxFloatingObjectInfoCache.CreateDefaultItem(
  const AUnitConverter: IdxDocumentModelUnitConverter): TdxFloatingObjectInfo;
begin
  Result := TdxFloatingObjectInfo.Create;
end;

{ TdxFloatingObjectProperties }

function TdxFloatingObjectProperties.AllowTextWrapAround: Boolean;
begin
  Result := (TextWrapType = TdxFloatingObjectTextWrapType.Square) or
    (TextWrapType = TdxFloatingObjectTextWrapType.Through) or (TextWrapType = TdxFloatingObjectTextWrapType.Tight);
end;

procedure TdxFloatingObjectProperties.ChangeIndexCore(ANewIndex: Integer;
  const AChangeActions: TdxDocumentModelChangeActions);
begin
  if FDisableHistory then
    SetIndexInitial(ANewIndex)
  else
    inherited ChangeIndexCore(ANewIndex, AChangeActions);
end;

constructor TdxFloatingObjectProperties.Create(const AOwner: IdxFloatingObjectPropertiesContainer);
begin
  inherited Create(AOwner.PieceTable);
  FOwner := AOwner;
end;

function TdxFloatingObjectProperties.CreateIndexChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := FOwner.CreateFloatingObjectPropertiesChangedHistoryItem;
end;

function TdxFloatingObjectProperties.GetActualHeight: Integer;
begin
  Result := ActualSize.Height;
end;

function TdxFloatingObjectProperties.GetActualSize: TSize;
begin
  Result := Info.ActualSize;
end;

function TdxFloatingObjectProperties.GetActualWidth: Integer;
begin
  Result := ActualSize.Width;
end;

function TdxFloatingObjectProperties.GetAllowOverlap: Boolean;
begin
  Result := Info.AllowOverlap;
end;

function TdxFloatingObjectProperties.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.BatchUpdate);
end;

function TdxFloatingObjectProperties.GetBottomDistance: Integer;
begin
  Result := Info.BottomDistance;
end;

function TdxFloatingObjectProperties.GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxFloatingObjectFormatting>;
begin
  Result := TdxDocumentCache(ADocumentModel.Cache).FloatingObjectFormattingCache;
end;

function TdxFloatingObjectProperties.GetCanPutTextAtLeft: Boolean;
begin
  Result := AllowTextWrapAround and (TextWrapSide <> TdxFloatingObjectTextWrapSide.Right);
end;

function TdxFloatingObjectProperties.GetCanPutTextAtRight: Boolean;
begin
  Result := AllowTextWrapAround and (TextWrapSide <> TdxFloatingObjectTextWrapSide.Left);
end;

function TdxFloatingObjectProperties.GetHidden: Boolean;
begin
  Result := Info.Hidden;
end;

function TdxFloatingObjectProperties.GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
begin
  Result := Info.HorizontalPositionAlignment;
end;

function TdxFloatingObjectProperties.GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
begin
  Result := Info.HorizontalPositionType;
end;

function TdxFloatingObjectProperties.GetIsBehindDoc: Boolean;
begin
  Result := Info.IsBehindDoc;
end;

function TdxFloatingObjectProperties.GetLayoutInTableCell: Boolean;
begin
  Result := Info.LayoutInTableCell;
end;

function TdxFloatingObjectProperties.GetLeftDistance: Integer;
begin
  Result := Info.LeftDistance;
end;

function TdxFloatingObjectProperties.GetLockAspectRatio: Boolean;
begin
  Result := Info.LockAspectRatio;
end;

function TdxFloatingObjectProperties.GetLocked: Boolean;
begin
  Result := Info.Locked;
end;

function TdxFloatingObjectProperties.GetOffset: TPoint;
begin
  Result := Info.Offset;
end;

function TdxFloatingObjectProperties.GetOffsetX: Integer;
begin
  Result := Offset.X;
end;

function TdxFloatingObjectProperties.GetOffsetY: Integer;
begin
  Result := Offset.Y;
end;

function TdxFloatingObjectProperties.GetPercentOffset: TPoint;
begin
  Result := Info.PercentOffset;
end;

function TdxFloatingObjectProperties.GetPercentOffsetX: Integer;
begin
  Result := PercentOffset.X;
end;

function TdxFloatingObjectProperties.GetPercentOffsetY: Integer;
begin
  Result := PercentOffset.Y;
end;

function TdxFloatingObjectProperties.GetPseudoInline: Boolean;
begin
  Result := Info.PseudoInline;
end;

function TdxFloatingObjectProperties.GetPutTextAtLargestSide: Boolean;
begin
  Result := AllowTextWrapAround and (TextWrapSide = TdxFloatingObjectTextWrapSide.Largest);
end;

function TdxFloatingObjectProperties.GetRelativeHeight: TdxFloatingObjectRelativeHeight;
begin
  Result := Info.RelativeHeight;
end;

function TdxFloatingObjectProperties.GetRelativeWidth: TdxFloatingObjectRelativeWidth;
begin
  Result := Info.RelativeWidth;
end;

function TdxFloatingObjectProperties.GetRightDistance: Integer;
begin
  Result := Info.RightDistance;
end;

function TdxFloatingObjectProperties.GetTextWrapSide: TdxFloatingObjectTextWrapSide;
begin
  Result := Info.TextWrapSide;
end;

function TdxFloatingObjectProperties.GetTextWrapType: TdxFloatingObjectTextWrapType;
begin
  Result := Info.TextWrapType;
end;

function TdxFloatingObjectProperties.GetTopDistance: Integer;
begin
  Result := Info.TopDistance;
end;

function TdxFloatingObjectProperties.GetUseActualSize: Boolean;
begin
  Result := Info.Options.UseActualSize;
end;

function TdxFloatingObjectProperties.GetUseAllowOverlap: Boolean;
begin
  Result := Info.Options.UseAllowOverlap;
end;

function TdxFloatingObjectProperties.GetUseBottomDistance: Boolean;
begin
  Result := Info.Options.UseBottomDistance;
end;

function TdxFloatingObjectProperties.GetUseHidden: Boolean;
begin
  Result := Info.Options.UseHidden;
end;

function TdxFloatingObjectProperties.GetUseHorizontalPositionAlignment: Boolean;
begin
  Result := Info.Options.UseHorizontalPositionAlignment;
end;

function TdxFloatingObjectProperties.GetUseHorizontalPositionType: Boolean;
begin
  Result := Info.Options.UseHorizontalPositionType;
end;

function TdxFloatingObjectProperties.GetUseLayoutInTableCell: Boolean;
begin
  Result := Info.Options.UseLayoutInTableCell;
end;

function TdxFloatingObjectProperties.GetUseLeftDistance: Boolean;
begin
  Result := Info.Options.UseLeftDistance;
end;

function TdxFloatingObjectProperties.GetUseLockAspectRatio: Boolean;
begin
  Result := Info.Options.UseLockAspectRatio;
end;

function TdxFloatingObjectProperties.GetUseLocked: Boolean;
begin
  Result := Info.Options.UseLocked;
end;

function TdxFloatingObjectProperties.GetUseOffset: Boolean;
begin
  Result := Info.Options.UseOffset;
end;

function TdxFloatingObjectProperties.GetUsePercentOffset: Boolean;
begin
  Result := Info.Options.UsePercentOffset;
end;

function TdxFloatingObjectProperties.GetUsePseudoInline: Boolean;
begin
  Result := Info.Options.UsePseudoInline;
end;

function TdxFloatingObjectProperties.GetUseRelativeHeight: Boolean;
begin
  Result := Info.Options.UseRelativeHeight;
end;

function TdxFloatingObjectProperties.GetUseRelativeWidth: Boolean;
begin
  Result := Info.Options.UseRelativeWidth;
end;

function TdxFloatingObjectProperties.GetUseRightDistance: Boolean;
begin
  Result := Info.Options.UseRightDistance;
end;

function TdxFloatingObjectProperties.GetUseTextWrapSide: Boolean;
begin
  Result := Info.Options.UseTextWrapSide;
end;

function TdxFloatingObjectProperties.GetUseTextWrapType: Boolean;
begin
  Result := Info.Options.UseTextWrapType;
end;

function TdxFloatingObjectProperties.GetUseTopDistance: Boolean;
begin
  Result := Info.Options.UseTopDistance;
end;

function TdxFloatingObjectProperties.GetUseVerticalPositionAlignment: Boolean;
begin
  Result := Info.Options.UseVerticalPositionAlignment;
end;

function TdxFloatingObjectProperties.GetUseVerticalPositionType: Boolean;
begin
  Result := Info.Options.UseVerticalPositionType;
end;

function TdxFloatingObjectProperties.GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
begin
  Result := Info.VerticalPositionAlignment;
end;

function TdxFloatingObjectProperties.GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
begin
  Result := Info.VerticalPositionType;
end;

function TdxFloatingObjectProperties.GetZOrder: Integer;
begin
  Result := Info.ZOrder;
end;

function TdxFloatingObjectProperties.InnerGetPieceTable: TdxCustomPieceTable;
begin
  Result := DocumentModelPart;
end;

procedure TdxFloatingObjectProperties.OnFirstBeginUpdateCore;
begin
  inherited OnFirstBeginUpdateCore;
  DeferredInfo.BeginUpdate;
end;

procedure TdxFloatingObjectProperties.OnIndexChanged;
begin
  inherited OnIndexChanged;
  FOwner.OnFloatingObjectChanged;
end;

procedure TdxFloatingObjectProperties.OnLastEndUpdateCore;
begin
  DeferredInfo.EndUpdate;
  inherited OnLastEndUpdateCore;
end;

procedure TdxFloatingObjectProperties.Reset;
var
  AInfo, AEmptyInfo: TdxFloatingObjectFormatting;
  AIsDeferred: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferred);
  AEmptyInfo := GetCache(DocumentModel)[TdxFloatingObjectFormattingCache.EmptyFloatingObjectFormattingIndex];
  AInfo.ReplaceInfo(AEmptyInfo.Info, AEmptyInfo.Options);
  ReplaceInfo(AInfo, BatchUpdateChangeActions);
  if not AIsDeferred then AInfo.Free;
end;



procedure TdxFloatingObjectProperties.SetActualHeight(const Value: Integer);
var
  AActualSize: TSize;
begin
  if (ActualHeight = Value) and UseActualSize then
    Exit;
  AActualSize := ActualSize;
  AActualSize.Height := Value;
  ActualSize := AActualSize;
end;

procedure TdxFloatingObjectProperties.SetActualSize(const Value: TSize);
begin
  if cxSizeIsEqual(Info.ActualSize, Value) and UseActualSize then
    Exit;
  SetPropertyValue<TSize>(SetActualSizeCore, Value);
end;

function TdxFloatingObjectProperties.SetActualSizeCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: TSize): TdxDocumentModelChangeActions;
begin
  AInfo.ActualSize := AValue;
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.ActualSize);
end;

procedure TdxFloatingObjectProperties.SetActualWidth(const Value: Integer);
var
  AActualSize: TSize;
begin
  if (ActualWidth = Value) and UseActualSize then
    Exit;
  AActualSize := ActualSize;
  AActualSize.Width := Value;
  ActualSize := AActualSize;
end;

procedure TdxFloatingObjectProperties.SetAllowOverlap(const Value: Boolean);
begin
  if (Info.AllowOverlap = Value) and UseAllowOverlap then
    Exit;
  SetPropertyValue<Boolean>(SetAllowOverlapCore, Value);
end;

function TdxFloatingObjectProperties.SetAllowOverlapCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.AllowOverlap := AValue;
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.AllowOverlap);
end;

procedure TdxFloatingObjectProperties.SetBottomDistance(const Value: Integer);
begin
  if (Info.BottomDistance = Value) and UseBottomDistance then
    Exit;
  SetPropertyValue<Integer>(SetBottomDistanceCore, Value);
end;

function TdxFloatingObjectProperties.SetBottomDistanceCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.BottomDistance := AValue;
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.BottomDistance);
end;

procedure TdxFloatingObjectProperties.SetHidden(const Value: Boolean);
begin
  if (Info.Hidden = Value) and UseHidden then
    Exit;
  SetPropertyValue<Boolean>(SetHiddenCore, Value);
end;

function TdxFloatingObjectProperties.SetHiddenCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.Hidden := AValue;
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.Hidden);
end;

procedure TdxFloatingObjectProperties.SetHorizontalPositionAlignment(
  const Value: TdxFloatingObjectHorizontalPositionAlignment);
begin
  if (Info.HorizontalPositionAlignment = Value) and UseHorizontalPositionAlignment then
    Exit;
  SetPropertyValue<Integer>(SetHorizontalPositionAlignmentCore, Ord(Value));
end;

function TdxFloatingObjectProperties.SetHorizontalPositionAlignmentCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.HorizontalPositionAlignment := TdxFloatingObjectHorizontalPositionAlignment(AValue);
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.HorizontalPositionAlignment);
end;

procedure TdxFloatingObjectProperties.SetHorizontalPositionType(const Value: TdxFloatingObjectHorizontalPositionType);
begin
  if (Info.HorizontalPositionType = Value) and UseHorizontalPositionType then
    Exit;
  SetPropertyValue<Integer>(SetHorizontalPositionTypeCore, Ord(Value));
end;

function TdxFloatingObjectProperties.SetHorizontalPositionTypeCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.HorizontalPositionType := TdxFloatingObjectHorizontalPositionType(AValue);
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.HorizontalPositionType);
end;

procedure TdxFloatingObjectProperties.SetIsBehindDoc(const Value: Boolean);
begin
  if Info.IsBehindDoc = Value then
    Exit;
  SetPropertyValue<Boolean>(SetIsBehindDocCore, Value);
end;

function TdxFloatingObjectProperties.SetIsBehindDocCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.IsBehindDoc := AValue;
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.IsBehindDoc);
end;

procedure TdxFloatingObjectProperties.SetLayoutInTableCell(const Value: Boolean);
begin
  if (Info.LayoutInTableCell = Value) and UseLayoutInTableCell then
    Exit;
  SetPropertyValue<Boolean>(SetLayoutInTableCellCore, Value);
end;

function TdxFloatingObjectProperties.SetLayoutInTableCellCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.LayoutInTableCell := AValue;
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.LayoutInTableCell);
end;

procedure TdxFloatingObjectProperties.SetLeftDistance(const Value: Integer);
begin
  if (Info.LeftDistance = Value) and UseLeftDistance then
    Exit;
  SetPropertyValue<Integer>(SetLeftDistanceCore, Value);
end;

function TdxFloatingObjectProperties.SetLeftDistanceCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.LeftDistance := AValue;
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.LeftDistance);
end;

procedure TdxFloatingObjectProperties.SetLockAspectRatio(const Value: Boolean);
begin
  if (Info.LockAspectRatio = Value) and UseLockAspectRatio then
    Exit;
  SetPropertyValue<Boolean>(SetLockAspectRatioCore, Value);
end;

function TdxFloatingObjectProperties.SetLockAspectRatioCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.LockAspectRatio := AValue;
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.LockAspectRatio);
end;

procedure TdxFloatingObjectProperties.SetLocked(const Value: Boolean);
begin
  if (Info.Locked = Value) and UseLocked then
    Exit;
  SetPropertyValue<Boolean>(SetLockedCore, Value);
end;

function TdxFloatingObjectProperties.SetLockedCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.Locked := AValue;
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.Locked);
end;

procedure TdxFloatingObjectProperties.SetOffset(const Value: TPoint);
begin
  if cxPointIsEqual(Info.Offset, Value) and UseOffset then
    Exit;
  SetPropertyValue<TPoint>(SetOffsetCore, Value);
end;

function TdxFloatingObjectProperties.SetOffsetCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: TPoint): TdxDocumentModelChangeActions;
begin
  AInfo.Offset := AValue;
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.Offset);
end;

procedure TdxFloatingObjectProperties.SetOffsetX(const Value: Integer);
var
  AOffset: TPoint;
begin
  if (OffsetX = Value) and UseOffset then
    Exit;
  AOffset := Offset;
  AOffset.X := Value;
  Offset := AOffset;
end;

procedure TdxFloatingObjectProperties.SetOffsetY(const Value: Integer);
var
  AOffset: TPoint;
begin
  if (OffsetY = Value) and UseOffset then
    Exit;
  AOffset := Offset;
  AOffset.Y := Value;
  Offset := AOffset;
end;

procedure TdxFloatingObjectProperties.SetPercentOffset(const Value: TPoint);
begin
  if cxPointIsEqual(Info.PercentOffset, Value) and UsePercentOffset then
    Exit;
  SetPropertyValue<TPoint>(SetPercentOffsetCore, Value);
end;

function TdxFloatingObjectProperties.SetPercentOffsetCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: TPoint): TdxDocumentModelChangeActions;
begin
  AInfo.PercentOffset := AValue;
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.PercentOffset);
end;

procedure TdxFloatingObjectProperties.SetPercentOffsetX(const Value: Integer);
var
  APercentOffset: TPoint;
begin
  if (PercentOffsetX = Value) and UsePercentOffset then
    Exit;
  APercentOffset := PercentOffset;
  APercentOffset.X := Value;
  PercentOffset := APercentOffset;
end;

procedure TdxFloatingObjectProperties.SetPercentOffsetY(const Value: Integer);
var
  APercentOffset: TPoint;
begin
  if (PercentOffsetY = Value) and UsePercentOffset then
    Exit;
  APercentOffset := PercentOffset;
  APercentOffset.Y := Value;
  PercentOffset := APercentOffset;
end;

procedure TdxFloatingObjectProperties.SetPseudoInline(const Value: Boolean);
begin
  if (Info.PseudoInline = Value) and UsePseudoInline then
    Exit;
  SetPropertyValue<Boolean>(SetPseudoInlineCore, value);
end;

function TdxFloatingObjectProperties.SetPseudoInlineCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.PseudoInline := AValue;
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.PseudoInline);
end;

procedure TdxFloatingObjectProperties.SetRelativeHeight(const Value: TdxFloatingObjectRelativeHeight);
begin
  if (Info.RelativeHeight = Value) and UseRelativeHeight then
    Exit;
  SetPropertyValue<TdxFloatingObjectRelativeHeight>(SetRelativeHeightCore, Value);
end;

function TdxFloatingObjectProperties.SetRelativeHeightCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: TdxFloatingObjectRelativeHeight): TdxDocumentModelChangeActions;
begin
  AInfo.RelativeHeight := AValue;
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.RelativeHeight);
end;

procedure TdxFloatingObjectProperties.SetRelativeWidth(const Value: TdxFloatingObjectRelativeWidth);
begin
  if (Info.RelativeWidth = Value) and UseRelativeWidth then
    Exit;
  SetPropertyValue<TdxFloatingObjectRelativeWidth>(SetRelativeWidthCore, Value);
end;

function TdxFloatingObjectProperties.SetRelativeWidthCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: TdxFloatingObjectRelativeWidth): TdxDocumentModelChangeActions;
begin
  AInfo.RelativeWidth := AValue;
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.RelativeWidth);
end;

procedure TdxFloatingObjectProperties.SetRightDistance(const Value: Integer);
begin
  if (Info.RightDistance = Value) and UseRightDistance then
    Exit;
  SetPropertyValue<Integer>(SetRightDistanceCore, Value);
end;

function TdxFloatingObjectProperties.SetRightDistanceCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.RightDistance := AValue;
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.RightDistance);
end;

procedure TdxFloatingObjectProperties.SetTextWrapSide(const Value: TdxFloatingObjectTextWrapSide);
begin
  if (Info.TextWrapSide = Value) and UseTextWrapSide then
    Exit;
  SetPropertyValue<Integer>(SetTextWrapSideCore, Ord(Value));
end;

function TdxFloatingObjectProperties.SetTextWrapSideCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.TextWrapSide := TdxFloatingObjectTextWrapSide(AValue);
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.TextWrapSide);
end;

procedure TdxFloatingObjectProperties.SetTextWrapType(const Value: TdxFloatingObjectTextWrapType);
begin
  if (Info.TextWrapType = Value) and UseTextWrapType then
    Exit;
  SetPropertyValue<Integer>(SetTextWrapTypeCore, Ord(Value));
end;

function TdxFloatingObjectProperties.SetTextWrapTypeCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.TextWrapType := TdxFloatingObjectTextWrapType(AValue);
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.TextWrapType);
end;

procedure TdxFloatingObjectProperties.SetTopDistance(const Value: Integer);
begin
  if (Info.TopDistance = Value) and UseTopDistance then
    Exit;
  SetPropertyValue<Integer>(SetTopDistanceCore, value);
end;

function TdxFloatingObjectProperties.SetTopDistanceCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.TopDistance := AValue;
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.TopDistance);
end;

procedure TdxFloatingObjectProperties.SetVerticalPositionAlignment(
  const Value: TdxFloatingObjectVerticalPositionAlignment);
begin
  if (Info.VerticalPositionAlignment = Value) and UseVerticalPositionAlignment then
    Exit;
  SetPropertyValue<Integer>(SetVerticalPositionAlignmentCore, Ord(Value));
end;

function TdxFloatingObjectProperties.SetVerticalPositionAlignmentCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.VerticalPositionAlignment := TdxFloatingObjectVerticalPositionAlignment(AValue);
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.VerticalPositionAlignment);
end;

procedure TdxFloatingObjectProperties.SetVerticalPositionType(const Value: TdxFloatingObjectVerticalPositionType);
begin
  if (Info.VerticalPositionType = Value) and UseVerticalPositionType then
    Exit;
  SetPropertyValue<Integer>(SetVerticalPositionTypeCore, Ord(Value));
end;

function TdxFloatingObjectProperties.SetVerticalPositionTypeCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.VerticalPositionType := TdxFloatingObjectVerticalPositionType(AValue);
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.VerticalPositionType);
end;

procedure TdxFloatingObjectProperties.SetZOrder(const Value: Integer);
begin
  if Info.ZOrder = Value then
    Exit;
  SetPropertyValue<Integer>(SetZOrderCore, Value);
end;

function TdxFloatingObjectProperties.SetZOrderCore(const AInfo: TdxFloatingObjectFormatting;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.ZOrder := AValue;
  Result := TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(TdxFloatingObjectChangeType.ZOrder);
end;

function TdxFloatingObjectProperties.UseVal(AMask: TdxUseFloatingObjectOption): Boolean;
begin
  Result := AMask in Info.Options.Value;
end;

{ TdxFloatingObjectChangeActionsCalculator }

class function TdxFloatingObjectChangeActionsCalculator.CalculateChangeActions(
  AChange: TdxFloatingObjectChangeType): TdxDocumentModelChangeActions;
const
  FloatingObjectChangeActionsMap: array[TdxFloatingObjectChangeType] of TdxDocumentModelChangeActions = (
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
    [TdxDocumentModelChangeAction.RaiseContentChanged],
    [TdxDocumentModelChangeAction.RaiseContentChanged],
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
  Result := FloatingObjectChangeActionsMap[AChange];
end;

{ TdxFloatingObjectFormattingCache }

constructor TdxFloatingObjectFormattingCache.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  Assert(ADocumentModel <> nil);
  inherited Create(ADocumentModel.UnitConverter, ADocumentModel);
  FDefaultFloatingObjectInfo := TdxFloatingObjectInfo.Create;
  AppendItem(TdxFloatingObjectFormatting.Create(DocumentModel.MainPart, DocumentModel,
    FDefaultFloatingObjectInfo, TdxFloatingObjectOptions.EmptyFloatingObjectOption));
end;

function TdxFloatingObjectFormattingCache.CreateDefaultItem(
  const AUnitConverter: IdxDocumentModelUnitConverter): TdxFloatingObjectFormatting;
begin
  Result := nil;
end;

destructor TdxFloatingObjectFormattingCache.Destroy;
begin
  FDefaultFloatingObjectInfo.Free;
  inherited Destroy;
end;

{ TdxZOrderManager }

procedure TdxZOrderManager.BringToFront(const AObjects: TdxIZOrderedObjectList; AObjectIndex: Integer);
var
  AMaxIndex: Integer;
begin
  AMaxIndex := AObjects.Count - 1;
  if (AObjectIndex >= AMaxIndex) or (AObjectIndex < 0) then
    Exit;

  AObjects[AObjectIndex].ZOrder := AObjects[AMaxIndex].ZOrder + 1;
end;

procedure TdxZOrderManager.BringForward(const AObjects: TdxIZOrderedObjectList; AObjectIndex: Integer);
var
  AMaxIndex: Integer;
begin
  AMaxIndex := AObjects.Count - 1;
  if (AObjectIndex >= AMaxIndex) or (AObjectIndex < 0) then
    Exit;

  if AObjectIndex < AMaxIndex then
  begin
    if (AObjectIndex + 2 <= AMaxIndex) and ((AObjects[AObjectIndex + 2].ZOrder - AObjects[AObjectIndex + 1].ZOrder = 1)) then
      Swap(AObjects, AObjectIndex, AObjectIndex + 1);

    AObjects[AObjectIndex].ZOrder := AObjects[AObjectIndex + 1].ZOrder + 1;
  end;
end;

procedure TdxZOrderManager.Swap(const AObjects: TdxIZOrderedObjectList; AObjectIndex: Integer; ASecondIndex: Integer);
var
  AZOrder: Integer;
begin
  AZOrder := AObjects[AObjectIndex].ZOrder;
  AObjects[AObjectIndex].ZOrder := AObjects[ASecondIndex].ZOrder;
  AObjects[ASecondIndex].ZOrder := AZOrder;
end;

procedure TdxZOrderManager.IncrementZOrders(const AFloatingObjectList: TdxIZOrderedObjectList; AFrom: Integer; ATo: Integer);
var
  I: Integer;
begin
  for I := AFrom + 1 to ATo - 1 do
  begin
    if AFloatingObjectList[I].ZOrder - AFloatingObjectList[I - 1].ZOrder >= 1 then
      Break;
    AFloatingObjectList[I].ZOrder := AFloatingObjectList[I].ZOrder + 1;
  end;
end;

procedure TdxZOrderManager.SendBackward(const AObjects: TdxIZOrderedObjectList; AObjectIndex: Integer);
begin
  if (AObjectIndex <= 0) or (AObjectIndex > AObjects.Count - 1) then
    Exit;

  if (AObjectIndex - 2 >= 0) and (Abs(AObjects[AObjectIndex - 1].ZOrder - AObjects[AObjectIndex - 2].ZOrder) = 1) then
    Swap(AObjects, AObjectIndex, AObjectIndex - 1);

  AObjects[AObjectIndex].ZOrder := AObjects[AObjectIndex - 1].ZOrder - 1;
end;

procedure TdxZOrderManager.SendToBack(const AObjects: TdxIZOrderedObjectList; AObjectIndex: Integer);
var
  AMinZorder: Integer;
begin
  AMinZorder := AObjects[0].ZOrder;
  if (AObjectIndex < 0) or (AObjectIndex > AObjects.Count - 1) then
    Exit;
  if AObjectIndex = 0 then
    IncrementZOrders(AObjects, 0, AObjects.Count - 1)
  else
    if AObjects[0].ZOrder >= 1 then
      AObjects[AObjectIndex].ZOrder := AMinZorder - 1
    else
    begin
      AObjects[AObjectIndex].ZOrder := 0;
      AObjects[0].ZOrder := AObjects[0].ZOrder + 1;
      IncrementZOrders(AObjects, 0, AObjectIndex);
    end;
end;

type
  TdxZOrderedObjectComparer = class(TcxIUnknownObject, IComparer<IdxZOrderedObject>)
  public
    function Compare(const Left, Right: IdxZOrderedObject): Integer;
  end;

function TdxZOrderedObjectComparer.Compare(const Left, Right: IdxZOrderedObject): Integer;
begin
  Result := Sign(Left.ZOrder - Right.ZOrder);
end;

procedure TdxZOrderManager.Normalize(const AObjects: TdxIZOrderedObjectList);
var
  AIndex: Integer;
  AObj, APrev: IdxZOrderedObject;
  AFront, ABack, ABackmost: TdxIZOrderedObjectList;
  AComparer: TdxZOrderedObjectComparer;
begin
  ABackmost := TdxIZOrderedObjectList.Create;
  try
    AFront := TdxIZOrderedObjectList.Create;
    try
      ABack := TdxIZOrderedObjectList.Create;
      try
        for AObj in AObjects do
        begin
          if AObj.IsBehindDoc then
            ABackmost.Add(AObj)
          else
            if AObj.ZOrder >= 0 then
              AFront.Add(AObj)
            else
              ABack.Add(AObj);
        end;
        AComparer := TdxZOrderedObjectComparer.Create;
        try
          AFront.Sort(AComparer);
          ABack.Sort(AComparer);
          ABackmost.Sort(AComparer);
        finally
          AComparer.Free;
        end;
        ABackmost.AddRange(ABack);
      finally
        ABack.Free;
      end;
      AIndex := 0;
      APrev := nil;
      for AObj in AFront do
      begin
        if APrev = nil then
        begin
          if (AObj.ZOrder = 0) then
            AIndex := 0
          else
            AIndex := 1;
        end
        else
          if AObj.ZOrder <> APrev.ZOrder then
            Inc(AIndex);
        AObj.ZOrder := AIndex;
        APrev := AObj;
      end;
    finally
      AFront.Free;
    end;
    APrev := nil;
    for AObj in ABackmost do
    begin
      if APrev = nil then
        AIndex := 1
      else
        if AObj.ZOrder <> APrev.ZOrder then
          Inc(AIndex);
      AObj.IsBehindDoc := True;
      AObj.ZOrder := AIndex;
      APrev := AObj;
    end;
  finally
    ABackmost.Free;
  end;
end;

{ TdxTextBoxInfo }

function TdxTextBoxInfo.Clone: TdxTextBoxInfo;
begin
  Result := TdxTextBoxInfo(inherited Clone);
end;

procedure TdxTextBoxInfo.CopyFrom(Source: TdxCloneable);
var
  ATextBoxInfo: TdxTextBoxInfo absolute Source;
begin
  LeftMargin := ATextBoxInfo.LeftMargin;
  RightMargin := ATextBoxInfo.RightMargin;
  TopMargin := ATextBoxInfo.TopMargin;
  BottomMargin := ATextBoxInfo.BottomMargin;
  ResizeShapeToFitText := ATextBoxInfo.ResizeShapeToFitText;
  WrapText := ATextBoxInfo.WrapText;
  VerticalAlignment := ATextBoxInfo.VerticalAlignment;
  Upright := ATextBoxInfo.Upright;
end;

function TdxTextBoxInfo.Equals(Obj: TObject): Boolean;
var
  AInfo: TdxTextBoxInfo;
begin
  AInfo := TdxTextBoxInfo(Obj);
  Result :=
    (LeftMargin = AInfo.LeftMargin) and
    (RightMargin = AInfo.RightMargin) and
    (TopMargin = AInfo.TopMargin) and
    (BottomMargin = AInfo.BottomMargin) and
    (ResizeShapeToFitText = AInfo.ResizeShapeToFitText) and
    (WrapText = AInfo.WrapText) and
    (Upright = AInfo.Upright) and
    (VerticalAlignment = AInfo.VerticalAlignment);
end;

{ TdxTextBoxOptions }

constructor TdxTextBoxOptions.Create(const AVal: TdxUseTextBoxOptions);
begin
  FVal := AVal;
end;

class operator TdxTextBoxOptions.Equal(const A, B: TdxTextBoxOptions): Boolean;
begin
  Result := A.FVal = B.FVal;
end;

function TdxTextBoxOptions.Clone: TdxTextBoxOptions;
begin
  Result := TdxTextBoxOptions.Create(FVal);
end;

procedure TdxTextBoxOptions.CopyFrom(const Source: TdxTextBoxOptions);
begin
  FVal := Source.Value;
end;

class function TdxTextBoxOptions.GetEmptyTextBoxOption: TdxTextBoxOptions;
begin
  Result := TdxTextBoxOptions.Create([]);
end;

function TdxTextBoxOptions.GetHashCode: Integer;
begin
  Result := Byte(FVal);
end;

function TdxTextBoxOptions.GetVal(AMask: TdxUseTextBoxOption): Boolean;
begin
  Result := AMask in FVal;
end;

procedure TdxTextBoxOptions.SetVal(AMask: TdxUseTextBoxOption; AValue: Boolean);
begin
  if AValue then
    Include(FVal, AMask)
  else
    Exclude(FVal, AMask);
end;

{ TdxTextBoxFormatting }

constructor TdxTextBoxFormatting.Create(APieceTable: TdxCustomPieceTable; const ADocumentModel: TdxCustomDocumentModel;
  const AFormattingInfo: TdxTextBoxInfo; const AFormattingOptions: TdxTextBoxOptions);
begin
  inherited Create(APieceTable, ADocumentModel, AFormattingInfo, AFormattingOptions);
end;

procedure TdxTextBoxFormatting.CopyFrom(Source: TdxCloneable);
var
  ATextBoxFormatting: TdxTextBoxFormatting absolute Source;
begin
  CopyFrom(ATextBoxFormatting.Info, ATextBoxFormatting.Options);
end;

procedure TdxTextBoxFormatting.CopyFrom(const AInfo: TdxTextBoxInfo; const AOptions: TdxTextBoxOptions);
begin
  CopyFromCore(AInfo, AOptions);
end;

function TdxTextBoxFormatting.CanSetPropertyValue: Boolean;
begin
  Result := DocumentModel.DocumentCapabilities.FloatingObjectsAllowed;
end;

function TdxTextBoxFormatting.PropertyEquals(const AOther: TdxIndexBasedObject<TdxTextBoxInfo, TdxTextBoxOptions>): Boolean;
begin
  Assert(AOther <> nil);
  Result := (Options.Value = AOther.Options.Value) and Info.Equals(AOther.Info);
end;

function TdxTextBoxFormatting.Clone: TdxCloneable;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxTextBoxFormatting.Create(PieceTable, DocumentModel, Info, Options);
end;

function TdxTextBoxFormatting.GetBottomMargin: Integer;
begin
  Result := Info.BottomMargin;
end;

function TdxTextBoxFormatting.GetLeftMargin: Integer;
begin
  Result := Info.LeftMargin;
end;

function TdxTextBoxFormatting.GetResizeShapeToFitText: Boolean;
begin
  Result := Info.ResizeShapeToFitText;
end;

function TdxTextBoxFormatting.GetRightMargin: Integer;
begin
  Result := Info.RightMargin;
end;

function TdxTextBoxFormatting.GetTopMargin: Integer;
begin
  Result := Info.TopMargin;
end;

function TdxTextBoxFormatting.GetUpright: Boolean;
begin
  Result := Info.Upright;
end;

function TdxTextBoxFormatting.GetVerticalAlignment: TdxVerticalAlignment;
begin
  Result := Info.VerticalAlignment;
end;

function TdxTextBoxFormatting.GetWrapText: Boolean;
begin
  Result := Info.WrapText;
end;

procedure TdxTextBoxFormatting.SetBottomMargin(const Value: Integer);
begin
  if (Info.BottomMargin = Value) and Options.UseBottomMargin then
    Exit;
  SetPropertyValue<Integer>(SetBottomMarginCore, Value);
  Options.UseBottomMargin := True;
end;

procedure TdxTextBoxFormatting.SetBottomMarginCore(const AInfo: TdxTextBoxInfo; const AValue: Integer);
begin
  AInfo.BottomMargin := AValue;
end;

procedure TdxTextBoxFormatting.SetLeftMargin(const Value: Integer);
begin
  if (Info.LeftMargin = Value) and Options.UseLeftMargin then
    Exit;
  SetPropertyValue<Integer>(SetLeftMarginCore, Value);
  Options.UseLeftMargin := True;
end;

procedure TdxTextBoxFormatting.SetLeftMarginCore(const AInfo: TdxTextBoxInfo; const AValue: Integer);
begin
  AInfo.LeftMargin := AValue;
end;

procedure TdxTextBoxFormatting.SetResizeShapeToFitText(const Value: Boolean);
begin
  if (Info.ResizeShapeToFitText = Value) and Options.UseResizeShapeToFitText then
    Exit;
  SetPropertyValue<Boolean>(SetResizeShapeToFitTextCore, Value);
  Options.UseResizeShapeToFitText := True;
end;

procedure TdxTextBoxFormatting.SetResizeShapeToFitTextCore(const AInfo: TdxTextBoxInfo; const AValue: Boolean);
begin
  AInfo.ResizeShapeToFitText := AValue;
end;

procedure TdxTextBoxFormatting.SetRightMargin(const Value: Integer);
begin
  if (Info.RightMargin = Value) and Options.UseRightMargin then
    Exit;
  SetPropertyValue<Integer>(SetRightMarginCore, Value);
  Options.UseRightMargin := True;
end;

procedure TdxTextBoxFormatting.SetRightMarginCore(const AInfo: TdxTextBoxInfo; const AValue: Integer);
begin
  AInfo.RightMargin := AValue;
end;

procedure TdxTextBoxFormatting.SetTopMargin(const Value: Integer);
begin
  if (Info.TopMargin = Value) and Options.UseTopMargin then
    Exit;
  SetPropertyValue<Integer>(SetTopMarginCore, Value);
  Options.UseTopMargin := True;
end;

procedure TdxTextBoxFormatting.SetTopMarginCore(const AInfo: TdxTextBoxInfo; const AValue: Integer);
begin
  AInfo.TopMargin := AValue;
end;

procedure TdxTextBoxFormatting.SetUpright(const Value: Boolean);
begin
  if (Info.Upright = Value) and Options.UseUpright then
    Exit;
  SetPropertyValue<Boolean>(SetUprightCore, Value);
  Options.UseUpright := True;
end;

procedure TdxTextBoxFormatting.SetUprightCore(const AInfo: TdxTextBoxInfo; const AValue: Boolean);
begin
  AInfo.Upright := AValue;
end;

procedure TdxTextBoxFormatting.SetVerticalAlignment(const Value: TdxVerticalAlignment);
begin
  if (Info.VerticalAlignment = Value) and Options.UseVerticalAlignment then
    Exit;
  SetPropertyValue<Integer>(SetVerticalAlignmentCore, Ord(Value));
  Options.UseVerticalAlignment := True;
end;

procedure TdxTextBoxFormatting.SetVerticalAlignmentCore(const AInfo: TdxTextBoxInfo; const AValue: Integer);
begin
  AInfo.VerticalAlignment := TdxVerticalAlignment(AValue);
end;

procedure TdxTextBoxFormatting.SetWrapText(const Value: Boolean);
begin
  if (Info.WrapText = Value) and Options.UseWrapText then
    Exit;
  SetPropertyValue<Boolean>(SetWrapTextCore, Value);
  Options.UseWrapText := True;
end;

procedure TdxTextBoxFormatting.SetWrapTextCore(const AInfo: TdxTextBoxInfo; const AValue: Boolean);
begin
  AInfo.WrapText := AValue;
end;

{ TdxTextBoxProperties }

constructor TdxTextBoxProperties.Create(const AOwner: IdxTextBoxPropertiesContainer);
begin
  inherited Create(GetPieceTable(AOwner));
  FOwner := AOwner;
end;

class function TdxTextBoxProperties.GetPieceTable(const AOwner: IdxTextBoxPropertiesContainer): TdxCustomPieceTable;
begin
  Assert(AOwner <> nil);
  Result := AOwner.PieceTable;
end;

function TdxTextBoxProperties.GetLeftMargin: Integer;
begin
  Result := Info.LeftMargin;
end;

procedure TdxTextBoxProperties.SetLeftMargin(const AValue: Integer);
begin
  if (Info.LeftMargin = AValue) and UseLeftMargin then
    Exit;
  SetPropertyValue<Integer>(SetLeftMarginCore, AValue);
end;


function TdxTextBoxProperties.GetUseLeftMargin: Boolean;
begin
  Result := Info.Options.UseLeftMargin;
end;

function TdxTextBoxProperties.SetLeftMarginCore(const AInfo: TdxTextBoxFormatting; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.LeftMargin := AValue;
  Result := TdxTextBoxPropertiesChangeActionsCalculator.CalculateChangeActions(TdxTextBoxPropertiesChangeType.LeftMargin);
end;

function TdxTextBoxProperties.GetRightMargin: Integer;
begin
  Result := Info.RightMargin;
end;

procedure TdxTextBoxProperties.SetRightMargin(const AValue: Integer);
begin
  if (Info.RightMargin = AValue) and UseRightMargin then
    Exit;
  SetPropertyValue<Integer>(SetRightMarginCore, AValue);
end;

function TdxTextBoxProperties.GetUseRightMargin: Boolean;
begin
  Result := Info.Options.UseRightMargin;
end;

function TdxTextBoxProperties.SetRightMarginCore(const AInfo: TdxTextBoxFormatting; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.RightMargin := AValue;
  Result := TdxTextBoxPropertiesChangeActionsCalculator.CalculateChangeActions(TdxTextBoxPropertiesChangeType.RightMargin);
end;

function TdxTextBoxProperties.GetTopMargin: Integer;
begin
  Result := Info.TopMargin;
end;

procedure TdxTextBoxProperties.SetTopMargin(const AValue: Integer);
begin
  if (Info.TopMargin = AValue) and UseTopMargin then
    Exit;
  SetPropertyValue<Integer>(SetTopMarginCore, AValue);
end;

function TdxTextBoxProperties.GetUseTopMargin: Boolean;
begin
  Result := Info.Options.UseTopMargin;
end;

function TdxTextBoxProperties.SetTopMarginCore(const AInfo: TdxTextBoxFormatting; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.TopMargin := AValue;
  Result := TdxTextBoxPropertiesChangeActionsCalculator.CalculateChangeActions(TdxTextBoxPropertiesChangeType.TopMargin);
end;

function TdxTextBoxProperties.GetBottomMargin: Integer;
begin
  Result := Info.BottomMargin;
end;

procedure TdxTextBoxProperties.SetBottomMargin(const AValue: Integer);
begin
  if (Info.BottomMargin = AValue) and UseBottomMargin then
    Exit;
  SetPropertyValue<Integer>(SetBottomMarginCore, AValue);
end;

function TdxTextBoxProperties.GetUseBottomMargin: Boolean;
begin
  Result := Info.Options.UseBottomMargin;
end;

function TdxTextBoxProperties.SetBottomMarginCore(const AInfo: TdxTextBoxFormatting; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.BottomMargin := AValue;
  Result := TdxTextBoxPropertiesChangeActionsCalculator.CalculateChangeActions(TdxTextBoxPropertiesChangeType.BottomMargin);
end;

function TdxTextBoxProperties.GetResizeShapeToFitText: Boolean;
begin
  Result := Info.ResizeShapeToFitText;
end;

procedure TdxTextBoxProperties.SetResizeShapeToFitText(const AValue: Boolean);
begin
  if (Info.ResizeShapeToFitText = AValue) and UseResizeShapeToFitText then
    Exit;
  SetPropertyValue<Boolean>(SetResizeShapeToFitTextCore, AValue);
end;

function TdxTextBoxProperties.GetUseResizeShapeToFitText: Boolean;
begin
  Result := Info.Options.UseResizeShapeToFitText;
end;

function TdxTextBoxProperties.SetResizeShapeToFitTextCore(const AInfo: TdxTextBoxFormatting; const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.ResizeShapeToFitText := AValue;
  Result := TdxTextBoxPropertiesChangeActionsCalculator.CalculateChangeActions(TdxTextBoxPropertiesChangeType.ResizeShapeToFitText);
end;

function TdxTextBoxProperties.GetWrapText: Boolean;
begin
  Result := Info.WrapText;
end;

procedure TdxTextBoxProperties.SetWrapText(const AValue: Boolean);
begin
  if (Info.WrapText = AValue) and UseWrapText then
    Exit;
  SetPropertyValue<Boolean>(SetWrapTextCore, AValue);
end;

function TdxTextBoxProperties.GetUseWrapText: Boolean;
begin
  Result := Info.Options.UseWrapText;
end;

function TdxTextBoxProperties.SetWrapTextCore(const AInfo: TdxTextBoxFormatting; const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.WrapText := AValue;
  Result := TdxTextBoxPropertiesChangeActionsCalculator.CalculateChangeActions(TdxTextBoxPropertiesChangeType.WrapText);
end;

function TdxTextBoxProperties.GetVerticalAlignment: TdxVerticalAlignment;
begin
  Result := Info.VerticalAlignment;
end;

procedure TdxTextBoxProperties.SetVerticalAlignment(const AValue: TdxVerticalAlignment);
begin
  if (Info.VerticalAlignment = AValue) and UseVerticalAlignment then
    Exit;
  SetPropertyValue<TdxVerticalAlignment>(SetVerticalAlignmentCore, AValue);
end;

function TdxTextBoxProperties.GetUseVerticalAlignment: Boolean;
begin
  Result := Info.Options.UseVerticalAlignment;
end;

function TdxTextBoxProperties.SetVerticalAlignmentCore(const AInfo: TdxTextBoxFormatting; const AValue: TdxVerticalAlignment): TdxDocumentModelChangeActions;
begin
  AInfo.VerticalAlignment := AValue;
  Result := TdxTextBoxPropertiesChangeActionsCalculator.CalculateChangeActions(TdxTextBoxPropertiesChangeType.VerticalAlignment);
end;

function TdxTextBoxProperties.GetUpright: Boolean;
begin
  Result := Info.Upright;
end;

procedure TdxTextBoxProperties.SetUpright(const AValue: Boolean);
begin
  if (Info.Upright = AValue) and UseUpright then
    Exit;
  SetPropertyValue<Boolean>(SetUprightCore, AValue);
end;

function TdxTextBoxProperties.GetUseUpright: Boolean;
begin
  Result := Info.Options.UseUpright;
end;

function TdxTextBoxProperties.SetUprightCore(const AInfo: TdxTextBoxFormatting; const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.Upright := AValue;
  Result := TdxTextBoxPropertiesChangeActionsCalculator.CalculateChangeActions(TdxTextBoxPropertiesChangeType.Upright);
end;

function TdxTextBoxProperties.GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxTextBoxFormatting>;
begin
  Result := TdxDocumentCache(ADocumentModel.Cache).TextBoxFormattingCache;
end;

function TdxTextBoxProperties.UseVal(AMask: TdxUseTextBoxOption): Boolean;
begin
  Result := AMask in Info.Options.Value;
end;

procedure TdxTextBoxProperties.Reset;
var
  AInfo, AEmptyInfo: TdxTextBoxFormatting;
  AIsDeferred: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferred);
  AEmptyInfo := GetCache(DocumentModel)[TdxTextBoxFormattingCache.EmptyTextBoxFormattingIndex];
  AInfo.ReplaceInfo(AEmptyInfo.Info, AEmptyInfo.Options);
  ReplaceInfo(AInfo, BatchUpdateChangeActions);
  if not AIsDeferred then
    AInfo.Free;
end;

function TdxTextBoxProperties.Equals(AObj: TObject): Boolean;
var
  AOther: TdxTextBoxProperties absolute AObj;
begin
  if not (AObj is TdxTextBoxProperties) then
    Exit(False);

  if DocumentModel = AOther.DocumentModel then
    Result := Index = AOther.Index
  else
    Result := Info.Equals(AOther.Info);
end;

procedure TdxTextBoxProperties.ResetUse(AMask: TdxUseTextBoxOptions);
var
  AInfo: TdxTextBoxFormatting;
  ATextBoxInfo: TdxTextBoxInfo;
  AIsDeferredInfo: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferredInfo);
  ATextBoxInfo:= AInfo.Info;
  AInfo.ReplaceInfo(ATextBoxInfo, TdxTextBoxOptions.Create(AMask));
  ReplaceInfo(AInfo, BatchUpdateChangeActions);
  if not AIsDeferredInfo then AInfo.Free;
end;

procedure TdxTextBoxProperties.ResetAllUse;
var
  AInfo: TdxTextBoxFormatting;
  ATextBoxInfo: TdxTextBoxInfo;
  AIsDeferredInfo: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferredInfo);
  ATextBoxInfo := AInfo.Info;
  AInfo.ReplaceInfo(ATextBoxInfo, TdxTextBoxOptions.Create(TdxTextBoxOptions.MaskUseNone));
  ReplaceInfo(AInfo, BatchUpdateChangeActions);
  if not AIsDeferredInfo then AInfo.Free;
end;

function TdxTextBoxProperties.GetHashCode: Integer;
begin
  Result := Index;
end;

function TdxTextBoxProperties.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxTextBoxPropertiesChangeActionsCalculator.CalculateChangeActions(TdxTextBoxPropertiesChangeType.BatchUpdate);
end;

procedure TdxTextBoxProperties.OnIndexChanged;
begin
  inherited OnIndexChanged;
  FOwner.OnTextBoxChanged;
end;

function TdxTextBoxProperties.CreateIndexChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := FOwner.CreateTextBoxChangedHistoryItem;
end;

{ TdxTextBoxFormattingCache }

constructor TdxTextBoxFormattingCache.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel.UnitConverter, ADocumentModel);
  FDefaultTextBoxInfo := CreateDefaultInfoItem(ADocumentModel.UnitConverter);
  AppendItem(TdxTextBoxFormatting.Create(DocumentModel.MainPart, DocumentModel, FDefaultTextBoxInfo, TdxTextBoxOptions.EmptyTextBoxOption));
end;

function TdxTextBoxFormattingCache.CreateDefaultInfoItem(
  const AUnitConverter: IdxDocumentModelUnitConverter): TdxTextBoxInfo;
begin
  Result := TdxTextBoxInfoCache.CreateDefaultItemCore(AUnitConverter);
end;

function TdxTextBoxFormattingCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxTextBoxFormatting;
begin
  Result := nil;
end;

destructor TdxTextBoxFormattingCache.Destroy;
begin
  FDefaultTextBoxInfo.Free;
  inherited Destroy;
end;

{ TdxTextBoxInfoCache }

function TdxTextBoxInfoCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxTextBoxInfo;
begin
  Result := CreateDefaultItemCore(AUnitConverter);
end;

class function TdxTextBoxInfoCache.CreateDefaultItemCore(
  const AUnitConverter: IdxDocumentModelUnitConverter): TdxTextBoxInfo;
begin
  Result := TdxTextBoxInfo.Create;
  Result.WrapText := True;
  Result.LeftMargin := AUnitConverter.DocumentsToModelUnits(30);
  Result.RightMargin := AUnitConverter.DocumentsToModelUnits(30);
  Result.TopMargin := AUnitConverter.DocumentsToModelUnits(15);
  Result.BottomMargin := AUnitConverter.DocumentsToModelUnits(15);
end;

end.
