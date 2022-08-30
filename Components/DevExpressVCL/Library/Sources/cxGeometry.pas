{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCore Library                                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCORE LIBRARY AND ALL           }
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

unit cxGeometry;

{$I cxVer.inc}

interface

uses
  SysUtils, Windows, Classes, Types, Math, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses;

type
  TcxPtInRectType = (ptrtNone, ptrtArea, ptrtLeft, ptrtTop, ptrtRight, ptrtBottom);

  { TdxSector }

  TdxSector = record
    StartAngle: Single;
    SweepAngle: Single;
  end;

  { TdxSizeF }

  TdxSizeF = record
    cx: Single;
    cy: Single;
  public
    class function Create(const AWidth, AHeight: Single): TdxSizeF; static; inline;
    class function Null: TdxSizeF; static; inline;
    function Ceil: TdxSizeF; inline;
    procedure Init(const AWidth, AHeight: Single); inline;

    property Width: Single read cx;
    property Height: Single read cy;
  end;

  { TdxPointF }

  TdxPointF = record
    X: Single;
    Y: Single;
  public
    class function Create(const AX, AY: Single): TdxPointF; static;

    class operator Implicit(const Value: TPoint): TdxPointF;
    class operator Implicit(const Value: TdxPointF): TPoint;
    class operator Equal(const P1, P2: TdxPointF): Boolean;
    class operator NotEqual(const P1, P2: TdxPointF): Boolean;

    procedure Init(const AX, AY: Single); inline;
    procedure Offset(const DX, DY: Single; APositiveFactor: Boolean = True); overload; inline;
    procedure Offset(const P: TdxPointF; APositiveFactor: Boolean = True); overload; inline;
    procedure Offset(const S: TdxSizeF; APositiveFactor: Boolean = True); overload; inline;
    procedure Scale(const DX, DY: Single); overload; inline;
    procedure Scale(const P: TdxPointF); overload; inline;
    procedure Scale(const S: TdxSizeF); overload; inline;

    class function Null: TdxPointF; static;
  end;

  { TdxRectF }

  TdxRectF = record
    Left: Single;
    Top: Single;
    Right: Single;
    Bottom: Single;
  strict private
    function GetBottomLeft: TdxPointF; inline;
    function GetBottomRight: TdxPointF; inline;
    function GetHeight: Single; inline;
    function GetLocation: TdxPointF; inline;
    function GetSize: TdxSizeF; inline;
    function GetTopLeft: TdxPointF; inline;
    function GetTopRight: TdxPointF; inline;
    function GetWidth: Single; inline;
    procedure SetBottomLeft(const P: TdxPointF); inline;
    procedure SetBottomRight(const P: TdxPointF); inline;
    procedure SetLocation(const P: TdxPointF); inline;
    procedure SetTopLeft(const P: TdxPointF); inline;
    procedure SetTopRight(const P: TdxPointF); inline;
    procedure SetHeight(const Value: Single); inline;
    procedure SetWidth(const Value: Single); inline;
  public
    class function Create(const ALeft, ATop, ARight, ABottom: Single): TdxRectF; overload; static;
    class function CreateSize(const ALeft, ATop, AWidth, AHeight: Single): TdxRectF; overload; static;
    procedure Init(const ALeft, ATop, ARight, ABottom: Single); overload; inline;
    procedure InitSize(const ALeft, ATop, AWidth, AHeight: Single); overload; inline;

    class operator Implicit(const Value: TRect): TdxRectF;
    class operator Equal(const R1, R2: TdxRectF): Boolean;
    class operator NotEqual(const R1, R2: TdxRectF): Boolean;

    procedure Assign(const Value: TRect);
    function CenterPoint: TdxPointF;
    procedure Empty;
    procedure Inflate(DX, DY: Single); overload; inline;
    procedure Inflate(const P: TdxPointF); overload; inline;
    procedure Inflate(const S: TdxSizeF); overload; inline;
    function IsEmpty: Boolean; inline;
    function IsZero : Boolean;
    procedure Intersect(const R: TdxRectF);
    procedure Normalize;
    class function Null: TdxRectF; static;
    procedure Offset(X, Y: Single);
    function ZoomInto(const ASize: TdxSizeF): TdxRectF; overload;
    function ZoomInto(const ABounds: TdxRectF): TdxRectF; overload;

    property Location: TdxPointF read GetLocation write SetLocation;
    property Height: Single read GetHeight write SetHeight;
    property Width: Single read GetWidth write SetWidth;
    property Size: TdxSizeF read GetSize;

    property BottomLeft: TdxPointF read GetBottomLeft write SetBottomLeft;
    property BottomRight: TdxPointF read GetBottomRight write SetBottomRight;
    property TopLeft: TdxPointF read GetTopLeft write SetTopLeft;
    property TopRight: TdxPointF read GetTopRight write SetTopRight;
  end;

  { TdxSizeDouble }

  TdxSizeDouble = record
  public
    Height: Double;
    Width: Double;
    procedure Init(AWidth, AHeight: Double); inline;
    class function Create(const AWidth, AHeight: Double): TdxSizeDouble; static; inline;
    class operator Equal(const R1, R2: TdxSizeDouble): Boolean;
    class operator NotEqual(const R1, R2: TdxSizeDouble): Boolean;
    class operator Implicit(const Value: TdxSizeDouble): TSize;
    class function Null: TdxSizeDouble; static; inline;
  end;

  { TdxPointDouble }

  TdxPointDouble = record
  public
    X: Double;
    Y: Double;
    procedure Init(const AX, AY: Double); inline;
    class function Create(const AX, AY: Double): TdxPointDouble; static; inline;
    class operator Equal(const R1, R2: TdxPointDouble): Boolean;
    class operator NotEqual(const R1, R2: TdxPointDouble): Boolean;
    class operator Implicit(const Value: TdxPointDouble): TPoint;
    class operator Implicit(const Value: TdxPointDouble): TdxPointF;
    procedure Offset(const DX, DY: Double); overload; inline;
    procedure Offset(const P: TdxPointDouble); overload; inline;
    procedure Offset(const S: TdxSizeDouble); overload; inline;
    procedure Scale(const DX, DY: Double); overload; inline;
    procedure Scale(const P: TdxPointDouble); overload; inline;
    procedure Scale(const S: TdxSizeDouble); overload; inline;
    class function Null: TdxPointDouble; static; inline;
  end;

  { TdxRectDouble }

  TdxRectDouble = record
  private
    function GetBottom: Double; inline;
    function GetBottomRight: TdxPointDouble; inline;
    function GetRight: Double; inline;
    function GetSize: TdxSizeDouble; inline;
    function GetTopLeft: TdxPointDouble; inline;
    procedure SetBottom(const AValue: Double); inline;
    procedure SetBottomRight(const ABottomRight: TdxPointDouble); inline;
    procedure SetRight(const AValue: Double); inline;
    procedure SetTopLeft(const ATopLeft: TdxPointDouble); inline;
  public
    Left: Double;
    Top: Double;
    Width: Double;
    Height: Double;
    class function CreateFromLTRB(const ALeft, ATop, ARight, ABottom: Double): TdxRectDouble; overload; static;
    class function Create(const AOrigin: TdxPointDouble): TdxRectDouble; overload; static;
    class function Create(const ATopLeft, ABottomRight: TdxPointDouble): TdxRectDouble; overload; static;
    class function Create(const ASize: TdxSizeDouble): TdxRectDouble; overload; static;
    class function Create(const AOrigin: TdxPointDouble; AWidth, AHeight: Double): TdxRectDouble; overload; static;
    class function Create(const ALeft, ATop, AWidth, AHeight: Double): TdxRectDouble; overload; static;
    class function Create(const ALeft, ATop: Double; const ASize: TdxSizeDouble): TdxRectDouble; overload; static;
    class function Create(const AOrigin: TdxPointDouble; const ASize: TdxSizeDouble): TdxRectDouble; overload; static;
    function IsEmpty: Boolean;
    class function Null: TdxRectDouble; static; inline;
    function Contains(const P: TdxPointDouble): Boolean;
    procedure Inflate(const AValue: Double); overload;
    procedure Union(const R: TdxRectDouble);
    property Bottom: Double read GetBottom write SetBottom;
    property BottomRight: TdxPointDouble read GetBottomRight write SetBottomRight;
    property Right: Double read GetRight write SetRight;
    property Size: TdxSizeDouble read GetSize;
    property TopLeft: TdxPointDouble read GetTopLeft write SetTopLeft;
  end;

  { TcxGeometryObject }

  TcxGeometryObject = class(TcxOwnedPersistent)
  strict private
    FOnChange: TNotifyEvent;
  protected
    procedure DoChange; virtual;
  public
    procedure ChangeScale(M, D: Integer); virtual;
    procedure LoadFromStream(AStream: TStream); virtual;
    procedure SaveToStream(AStream: TStream); virtual;
    //
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TcxRect }

  TcxRect = class(TcxGeometryObject)
  strict private
    FRect: TRect;

    function GetHeight: Integer;
    function GetVertex(AIndex: Integer): TPoint;
    function GetWidth: Integer;
    procedure SetHeight(AValue: Integer);
    procedure SetRect(AValue: TRect);
    procedure SetVertex(AIndex: Integer; const AValue: TPoint);
    procedure SetWidth(AValue: Integer);
  protected
    procedure DoAssign(Source: TPersistent); override;
    procedure ValidateRect(var R: TRect); virtual;
  public
    function GetValue(AIndex: Integer): Integer;
    procedure SetValue(AIndex, AValue: Integer);

    procedure ChangeScale(M, D: Integer); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(AStream: TStream); override;

    function IsEmpty: Boolean;
    function IsEqual(const ARect: TRect): Boolean; overload;
    function IsEqual(ARect: TcxRect): Boolean; overload;

    property BottomLeft: TPoint index 2 read GetVertex write SetVertex;
    property BottomRight: TPoint index 3 read GetVertex write SetVertex;
    property Height: Integer read GetHeight write SetHeight;
    property Rect: TRect read FRect write SetRect;
    property TopLeft: TPoint index 0 read GetVertex write SetVertex;
    property TopRight: TPoint index 1 read GetVertex write SetVertex;
    property Width: Integer read GetWidth write SetWidth;
  published
    property Left: Integer index 0 read GetValue write SetValue default 0;
    property Top: Integer index 1 read GetValue write SetValue default 0;
    property Right: Integer index 2 read GetValue write SetValue default 0;
    property Bottom: Integer index 3 read GetValue write SetValue default 0;
    property OnChange;
  end;

  { TcxSize }

  TcxSize = class(TcxGeometryObject)
  strict private
    FDefaultSize: TSize;

    function GetSize: TSize;
    procedure SetValue(Index, Value: Integer);
  protected
    FSize: TSize;

    procedure DoAssign(Source: TPersistent); override;
    function GetValue(Index: Integer): Integer; virtual;
    function IsSizeStored(Index: Integer): Boolean; virtual;
    procedure SetSize(const Value: TSize); virtual;
  public
    constructor Create(AOwner: TPersistent; ADefaultWidth: Integer = 0; ADefaultHeight: Integer = 0); reintroduce;
    procedure ChangeScale(M, D: Integer); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(AStream: TStream); override;

    function IsEmpty: Boolean;
    function IsEqual(const ASize: TSize): Boolean; overload;
    function IsEqual(ASize: TcxSize): Boolean; overload;

    property Size: TSize read GetSize write SetSize;
  published
    property Height: Integer index 0 read GetValue write SetValue stored IsSizeStored;
    property Width: Integer index 1 read GetValue write SetValue stored IsSizeStored;
  end;

  { TcxMargin }

  TcxMargin = class(TcxGeometryObject)
  strict private
    FDefaultValue: Integer;
    FMargin: TRect;

    function GetValue(AIndex: Integer): Integer;
    function IsMarginStored(AIndex: Integer): Boolean;
    procedure SetMargin(const Value: TRect);
    procedure SetValue(AIndex, AValue: Integer);
  protected
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent; ADefaultValue: Integer = 0); reintroduce;
    procedure ChangeScale(M, D: Integer); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(AStream: TStream); override;
    function IsEqual(AMargin: TcxMargin): Boolean;

    property Margin: TRect read FMargin write SetMargin;
  published
    property All: Integer index 0 read GetValue write SetValue stored IsMarginStored;
    property Left: Integer index 1 read GetValue write SetValue stored IsMarginStored;
    property Top: Integer index 2 read GetValue write SetValue stored IsMarginStored;
    property Right: Integer index 3 read GetValue write SetValue stored IsMarginStored;
    property Bottom: Integer index 4 read GetValue write SetValue stored IsMarginStored;
  end;

  { TdxPoint }

  TdxPoint = class(TcxGeometryObject)
  strict private
    FDefaultPoint: TPoint;
    FPoint: TPoint;

    function GetPoint: TPoint;
    function GetValue(Index: Integer): Integer;
    procedure SetPoint(const Value: TPoint);
    procedure SetValue(Index, Value: Integer);
  protected
    procedure DoAssign(Source: TPersistent); override;
    function IsPointStored(Index: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TPersistent; ADefaultX: Integer = 0; ADefaultY: Integer = 0); reintroduce;
    procedure ChangeScale(M, D: Integer); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(AStream: TStream); override;

    function IsEqual(const APoint: TPoint): Boolean; overload;
    function IsEqual(APoint: TdxPoint): Boolean; overload;

    property Point: TPoint read GetPoint write SetPoint;
  published
    property X: Integer index 0 read GetValue write SetValue stored IsPointStored;
    property Y: Integer index 1 read GetValue write SetValue stored IsPointStored;
  end;

  { TdxPointDoublePersistent }

  TdxPointDoublePersistent = class(TcxGeometryObject)
  strict private
    FDefaultPoint: TdxPointDouble;
    FPoint: TdxPointDouble;

    function GetValue(AIndex: Integer): Double;
    function IsPointStored(AIndex: Integer): Boolean;
    procedure SetPoint(const AValue: TdxPointDouble);
    procedure SetValue(AIndex: Integer; AValue: Double);
  protected
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent; const ADefaultX: Double = 0; const ADefaultY: Double = 0); reintroduce;
    function IsEqual(const APoint: TdxPointDouble): Boolean;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure Reset;

    property Point: TdxPointDouble read FPoint write SetPoint;
  published
    property X: Double index 0 read GetValue write SetValue stored IsPointStored;
    property Y: Double index 1 read GetValue write SetValue stored IsPointStored;
  end;

  TdxPointsF = array of TdxPointF;
  TdxRectsF = array of TdxRectF;

  { TdxMatrix }

  TdxTransformationOrder = (moPrepend, moAppend);

  TdxMatrix = class
  strict private
    FMatrix: TXForm;

    procedure DoMultiply(const AXForm: TXForm; AOrder: TdxTransformationOrder = moPrepend);
  public
    constructor Create;
    constructor CreateEx(M11, M12, M21, M22, DX, DY: Single); overload;
    constructor CreateEx(M: TdxMatrix); overload;

    procedure Assign(M: TdxMatrix); overload;
    procedure Assign(M11, M12, M21, M22, DX, DY: Single); overload;

    function IsIdentity: Boolean;
    function Transform(const APoint: TdxPointF): TdxPointF; overload;
    function Transform(const ARect: TdxRectF): TdxRectF; overload;

    procedure Multiply(M: TdxMatrix; AOrder: TdxTransformationOrder = moPrepend);
    procedure Reset;
    procedure Rotate(AAngle: Single; AOrder: TdxTransformationOrder = moPrepend); overload;
    procedure Rotate(AAngle: Single; const APivotPoint: TPoint; AOrder: TdxTransformationOrder = moPrepend); overload;
    procedure Rotate(AAngle: Single; const APivotPoint: TdxPointF; AOrder: TdxTransformationOrder = moPrepend); overload;
    procedure Scale(AScaleX, AScaleY: Single; AOrder: TdxTransformationOrder = moPrepend); overload;
    procedure Scale(const AScale: TdxPointF; AOrder: TdxTransformationOrder = moPrepend); overload;
    procedure Translate(DX, DY: Single; AOrder: TdxTransformationOrder = moPrepend); overload;
    procedure Translate(const AOffset: TdxPointF; AOrder: TdxTransformationOrder = moPrepend); overload;

    property XForm: TXForm read FMatrix write FMatrix;
  end;

  { TdxScaleFactor }

  TdxScaleFactorNotifyEvent = procedure (Sender: TObject; M, D: Integer; IsLoading: Boolean) of object;

  TdxScaleFactor = class
  strict private
    FDenominator: Integer;
    FNumerator: Integer;
    FListeners: TList<TdxScaleFactorNotifyEvent>;

    function GetAssigned: Boolean; inline;
    function TransformScaleFactor(var ASourceNumerator, ASourceDenominator: Integer): Boolean;
  protected
    procedure Changed(M, D: Integer; IsLoading: Boolean); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ANumerator, ADenominator: Integer; AIsLoading: Boolean = False); overload;
    procedure Assign(ASource: TdxScaleFactor); overload;
    procedure Change(ANumerator, ADenominator: Integer);
    function Equals(Obj: TObject): Boolean; override;

    function Apply(const V: Integer): Integer; overload; inline;
    function Apply(const V: TPoint): TPoint; overload; inline;
    function Apply(const V: TRect): TRect; overload; inline;
    function Apply(const V: TSize): TSize; overload; inline;
    function ApplyF(const V: Double): Double; overload; inline;

    function Apply(const V: Integer; ASourceScaleFactor: TdxScaleFactor): Integer; overload; inline;
    function Apply(const V: TPoint; ASourceScaleFactor: TdxScaleFactor): TPoint; overload; inline;
    function Apply(const V: TRect; ASourceScaleFactor: TdxScaleFactor): TRect; overload; inline;
    function Apply(const V: TSize; ASourceScaleFactor: TdxScaleFactor): TSize; overload; inline;

    function Apply(const V: Integer; ASourceNumerator, ASourceDenominator: Integer): Integer; overload;
    function Apply(const V: TPoint; ASourceNumerator, ASourceDenominator: Integer): TPoint; overload;
    function Apply(const V: TRect; ASourceNumerator, ASourceDenominator: Integer): TRect; overload;
    function Apply(const V: TSize; ASourceNumerator, ASourceDenominator: Integer): TSize; overload;

    function Revert(const V: Integer): Integer; overload; inline;
    function Revert(const V: TPoint): TPoint; overload; inline;
    function Revert(const V: TRect): TRect; overload; inline;
    function Revert(const V: TSize): TSize; overload; inline;
    function RevertF(const V: Double): Double; overload; inline;

    procedure ListenerAdd(AEventHandler: TdxScaleFactorNotifyEvent);
    procedure ListenerRemove(AEventHandler: TdxScaleFactorNotifyEvent);

    property Assigned: Boolean read GetAssigned;
    property Denominator: Integer read FDenominator;
    property Numerator: Integer read FNumerator;
  end;

  { IdxScaleFactor }

  IdxScaleFactor = interface
  ['{367A805A-A84B-441A-83A3-6A7E489D3B6C}']
    function GetScaleFactor: TdxScaleFactor;
    property Value: TdxScaleFactor read GetScaleFactor;
  end;

  { TdxOwnedScaleFactor }

  TdxOwnedScaleFactor = class(TdxScaleFactor)
  strict private
    FOwner: TdxScaleFactor;
    FUseOwnerValue: Boolean;

    procedure OwnerChangeHandler(Sender: TObject; M, D: Integer; IsLoading: Boolean);
    procedure SetOwner(const Value: TdxScaleFactor);
    procedure SetUseOwnerValue(const Value: Boolean);
    procedure SynchronizeValue(AIsLoading: Boolean = False);
  protected
    procedure Changed(M, D: Integer; IsLoading: Boolean); override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    //
    property Owner: TdxScaleFactor read FOwner write SetOwner;
    property UseOwnerValue: Boolean read FUseOwnerValue write SetUseOwnerValue;
  end;

  { TdxXFormHelper }

  TdxXFormHelper = record helper for TXForm
  public
    class function Combine(const AMatrix1, AMatrix2: TXForm): TXForm; static;
    class function CreateFlip(AFlipHorizontally, AFlipVertically: Boolean; const APivotPointX, APivotPointY: Single): TXForm; static;
    class function CreateIdentityMatrix: TXForm; static;
    class function CreateMatrix(M11, M12, M21, M22, DX, DY: Single): TXForm; static;
    class function CreateRotationMatrix(AAngle: Single): TXForm; static;
    class function CreateScaleMatrix(AScale: Single): TXForm; overload; static;
    class function CreateScaleMatrix(AScaleX, AScaleY: Single): TXForm; overload; static;
    class function CreateTranslateMatrix(AOffsetX, AOffsetY: Single): TXForm; static;
    class function IsEqual(const AMatrix1, AMatrix2: TXForm): Boolean; static;
    class function IsIdentity(const AMatrix: TXForm): Boolean; static;
  end;

function cxMargins(const ARect, AContentRect: TRect): TRect;
function cxMarginsHeight(const R: TRect): Integer; inline;
function cxMarginsWidth(const R: TRect): Integer; inline;

// rect helper routines
function cxHalfCoordinate(ASize: Integer): Integer; inline;
function cxRectCompare(const R1, R2: TRect): Boolean; inline;
function cxRectCenter(const R: TRect): TPoint; overload; inline;
function cxRectCenter(const ABounds: TRect; const ASize: TSize): TRect; overload; inline;
function cxRectCenter(const ABounds: TRect; AWidth, AHeight: Integer): TRect; overload; inline;
function cxRectCenter(const ABounds: TdxRectF; AWidth, AHeight: Single): TdxRectF; overload; inline;
function cxRectCenter(const R: TdxRectF): TdxPointF; overload; inline;
function cxRectCenterHorizontally(const R: TdxRectF; const AWidth: Double): TdxRectF; overload; inline;
function cxRectCenterHorizontally(const R: TRect; AWidth: Integer): TRect; overload; inline;
function cxRectCenterVertically(const R: TdxRectF; const AHeight: Double): TdxRectF; overload; inline;
function cxRectCenterVertically(const R: TRect; AHeight: Integer): TRect; overload; inline;
function cxRectContain(const ABounds, AInner: TRect): Boolean; inline;
function cxRectContent(const R, AMargins: TRect): TRect; overload;
function cxRectContent(const R, AMargins: TdxRectF): TdxRectF; overload;
function cxRectHeight(const R: TRect): Integer; overload; inline;
function cxRectHeight(const R: TdxRectF): Single; overload; inline;
function cxRectIntersect(const R1, R2: TRect): Boolean; overload; inline;
function cxRectIntersect(const R1, R2: TdxRectF): Boolean; overload; inline;
function cxRectIntersect(out R: TRect; const R1, R2: TRect): Boolean; overload; inline;
function cxRectIntersect(out R: TdxRectF; const R1, R2: TdxRectF): Boolean; overload; inline;
function cxRectInflate(const R: TRect; DX, DY: Integer): TRect; overload; inline;
function cxRectInflate(const R: TRect; DX1, DY1, DX2, DY2: Integer): TRect; overload; inline;
function cxRectInflate(const R: TRect; Delta: Integer): TRect; overload; inline;
function cxRectInflate(const R, AMargins: TRect): TRect; overload; inline;
function cxRectInflate(const R: TdxRectF; DX, DY: Single): TdxRectF; overload; inline;
function cxRectInflate(const R: TdxRectF; Delta: Single): TdxRectF; overload; inline;
function cxRectInflate(const ARect, AMargins: TdxRectF): TdxRectF; overload; inline;
function cxRectInvert(const R: TRect): TRect; inline;
function cxRectIsEmpty(const R: TRect): Boolean; overload; inline;
function cxRectIsEmpty(const R: TdxRectF): Boolean; overload; inline;
function cxRectIsEqual(const R, R1: TRect): Boolean; inline;
function cxRectIsInvalid(const R: TRect): Boolean; inline;
function cxRectIsNull(const R: TRect): Boolean; inline;
function cxRectGetItem(const ARect: TRect; AIndex: Integer): Integer; inline;
function cxRectGetSize(const ARect: TRect; AIndex: Integer): Integer; inline;
function cxRectLeftBottom(const R: TRect): TPoint; inline;
procedure cxRectMinMaxHeight(const R: TRect; var AMax, AMin: Integer); inline;
procedure cxRectMinMaxInit(var AMin, AMax: Integer); inline;
procedure cxRectMinMaxWidth(const R: TRect; var AMax, AMin: Integer); inline;
function cxRectOffset(const R: TRect; const P: TPoint; APositiveFactor: Boolean = True): TRect; overload; inline;
function cxRectOffset(const R: TRect; const Ps: array of TPoint; APositiveFactor: Boolean = True): TRect; overload;
function cxRectOffset(const R: TRect; DX, DY: Integer; APositiveFactor: Boolean = True): TRect; overload; inline;
function cxRectOffset(const R: TdxRectF; const P: TdxPointF; APositiveFactor: Boolean = True): TdxRectF; overload; inline;
function cxRectOffset(const R: TdxRectF; DX, DY: Single; APositiveFactor: Boolean = True): TdxRectF; overload; inline;
procedure cxRectsOffset(var Rs: array of TdxRectF; const AOffset: TdxPointF; APositiveFactor: Boolean = True); overload;
function cxRectOffsetHorz(const R: TRect; DX: Integer): TRect; inline;
function cxRectOffsetVert(const R: TRect; DY: Integer): TRect; inline;
procedure cxRectOverlapped(const ASrc, ADst: TRect; out ASrcH, ASrcV, ADstH, ADstV: TRect);
function cxRectPtIn(const R: TRect; const P: TPoint): Boolean; overload; inline;
function cxRectPtIn(const R: TRect; const X, Y: Integer): Boolean; overload; inline;
function cxRectPtInEx(const R: TRect; X, Y, ADelta: Integer): TcxPtInRectType; overload; inline;
function cxRectPtInEx(const R: TRect; X, Y, ADeltaX, ADeltaY: Integer): TcxPtInRectType; overload; inline;
function cxRectPtInEx(const R: TRect; const X, Y: Integer; DL, DT, DR, DB: Integer): TcxPtInRectType; overload; inline;
function cxRectRotate(const R: TRect): TRect; inline;
function cxRect(const ASize: TSize): TRect; overload; inline;
function cxRect(const ALeft, ATop, ARight, ABottom: Integer): TRect; overload; inline;
function cxRect(const ATopLeft, ABottomRight: TPoint): TRect; overload; inline;
function cxRect(const ARect: TdxRectF; AUseTrunc: Boolean = True): TRect; overload; inline;
function cxRectF(const ATopLeft, ABottomRight: TdxPointF): TdxRectF; overload; inline;
function cxRectF(const ALeft, ATop, ARight, ABottom: Single): TdxRectF; overload; inline;
function cxRectF(const ARect: TRect): TdxRectF; overload; inline;

function cxRectAdjust(const R: TRect): TRect;
function cxRectAdjustF(const R: TdxRectF): TdxRectF;
function cxRectBounds(ALeft, ATop, AWidth, AHeight: Integer): TRect; overload; inline;
function cxRectBounds(ALeft, ATop: Integer; const ASize: TSize): TRect; overload; inline;
function cxRectFBounds(ALeft, ATop, AWidth, AHeight: Single): TdxRectF; overload; inline;
function cxRectBounds(const P: TPoint; AWidth, AHeight: Integer): TRect; overload; inline;
function cxRectRightTop(const R: TRect): TPoint; inline;
function cxRectScale(const R: TdxRectF; AScaleFactor: Single): TdxRectF; overload; inline;
function cxRectScale(const R: TRect; ANumerator, ADenominator: Integer): TRect; overload; inline;
function cxRectScale(const R: TRect; ANumeratorX, ADenominatorX, ANumeratorY, ADenominatorY: Integer): TRect; overload; inline;
function cxRectScaleSize(const R: TRect; ANumerator, ADenominator: Integer): TRect; overload; inline;
function cxRectScaleHeight(const R: TRect; Y1, Y2, H1, H2: Integer): TRect; inline;
function cxRectScaleWidth(const R: TRect; X1, X2, W1, W2: Integer): TRect; inline;
function cxRectSetBottom(const R: TRect; Y: Integer): TRect; overload; inline;
function cxRectSetBottom(const R: TRect; Y, H: Integer): TRect; overload; inline;
function cxRectSetHeight(const R: TRect; H: Integer): TRect; inline;
function cxRectSetLeft(const R: TRect; X: Integer): TRect; overload; inline;
function cxRectSetLeft(const R: TRect; X, W: Integer): TRect; overload; inline;
function cxRectSetOrigin(const R: TRect; const APos: TPoint): TRect; inline;
function cxRectSetNullOrigin(const R: TRect): TRect; inline;
function cxRectSetTop(const R: TRect; Y: Integer): TRect; overload; inline;
function cxRectSetTop(const R: TdxRectF; Y: Single): TdxRectF; overload; inline;
function cxRectSetTop(const R: TRect; Y, H: Integer): TRect; overload; inline;
function cxRectSetTop(const R: TdxRectF; Y, H: Single): TdxRectF; overload; inline;
function cxRectSetRight(const R: TRect; X: Integer): TRect; overload; inline;
function cxRectSetRight(const R: TRect; X, W: Integer): TRect; overload; inline;
function cxRectSetSize(const R: TRect; W, H: Integer): TRect; overload; inline;
function cxRectSetSize(const R: TRect; const ASize: TSize): TRect; overload; inline;
function cxRectSetSizeF(const R: TdxRectF; const ASize: TdxSizeF): TdxRectF; overload; inline;
function cxRectSetWidth(const R: TRect; W: Integer): TRect; overload; inline;
function cxRectSetWidth(const R: TRect; X, W: Integer): TRect; overload; inline;
function cxRectSetXPos(const R: TRect; X1, X2: Integer): TRect; inline;
function cxRectSetYPos(const R: TRect; Y1, Y2: Integer): TRect; inline;
function cxRectSize(const R: TRect): TSize; inline;
procedure cxRectSplitHorz(const ABounds: TRect; var ARect1, ARect2: TRect); inline;
procedure cxRectSplitVert(const ABounds: TRect; var ARect1, ARect2: TRect); inline;
function cxRectSquare(const R: TRect): Integer; inline;
function cxRectTransform(const ARect: TRect; ALeftOffset, ATopOffset,
  ARightOffset, ABottomOffset: Integer): TRect; overload; inline;
function cxRectTransform(const ARect, AOffsets: TRect): TRect; overload; inline;
function cxRectUnion(const R1, R2: TRect): TRect; overload;
function cxRectUnion(const R1, R2: TdxRectF): TdxRectF; overload;
function cxRectWidth(const R: TRect): Integer; overload; inline;
function cxRectWidth(const R: TdxRectF): Single; overload; inline;
function cxRectProportionalStretch(const R: TRect; const ASize: TSize): TRect; overload; inline;
function cxRectProportionalStretch(const R: TRect; W, H: Integer): TRect; overload; inline;

// point helper routines
function cxPointDistance(const P1, P2: TPoint): Single;
function cxPointDistanceF(const P1, P2: TdxPointF): Single;
function cxPointInvert(const P: TPoint): TPoint; overload; inline;
function cxPointInLine(const P, LinePoint1, LinePoint2: TdxPointF): Boolean; overload;
function cxPointInLine(const P, LinePoint1, LinePoint2: TPoint): Boolean; overload;
function cxPointIsEqual(const P1, P2: TPoint): Boolean; overload; inline;
function cxPointIsEqual(const P1, P2: TdxPointF): Boolean; overload; inline;
function cxPointIsInvalid(const P: TPoint): Boolean; inline;
function cxPointIsNull(const P: TPoint): Boolean; overload; inline;
function cxPointGetItem(const P: TPoint; AIndex: Integer): Integer; inline;
function cxPoint(X, Y: Integer): TPoint; overload; inline;
function cxPoint(const APointF: TdxPointF; AUsedTrunc: Boolean = True): TPoint; overload; inline;
function cxPoint(const ASize: TSize): TPoint; overload; inline;
function cxPointF(X, Y: Single): TdxPointF; overload; inline;
function cxPointF(const P: TPoint): TdxPointF; overload; inline;
function cxPointOffset(const P, DP: TPoint; APositiveFactor: Boolean = True): TPoint; overload; inline;
function cxPointOffset(const P: TPoint; const Ps: array of TPoint; APositiveFactor: Boolean = True): TPoint; overload;
function cxPointOffset(const P: TPoint; X, Y: Integer; APositiveFactor: Boolean = True): TPoint; overload; inline;
function cxPointOffset(const P, AOffset: TdxPointF; APositiveFactor: Boolean = True): TdxPointF; overload; inline;
function cxPointOffset(const P: TdxPointF; X, Y: Single; APositiveFactor: Boolean = True): TdxPointF; overload; inline;
procedure cxPointsOffset(var Ps: array of TdxPointF; const AOffset: TdxPointF); overload;
function cxPointReplaceItem(const P: TPoint; const AIndex, AValue: Integer): TPoint; inline;
function cxPointScale(const P: TPoint; ANumerator, ADenominator: Integer): TPoint; overload; inline;
function cxPointScale(const P: TdxPointF; AScaleFactor: Single): TdxPointF; overload; inline;
function cxPointSum(const Ps: array of TPoint): TPoint;
function cxPointsBox(const Ps: array of TPoint): TRect; overload;
function cxPointsBox(const Ps: array of TdxPointF): TdxRectF; overload;
procedure cxPointsOffset(var Ps: array of TPoint; DX, DY: Integer); overload;
procedure cxPointsOffset(var Ps: array of TPoint; const AOffset: TPoint); overload;

function cxSize(XY: Integer): TSize; overload; inline;
function cxSize(DX, DY: Integer): TSize; overload; inline;
function cxSize(const P: TPoint): TSize; overload; inline;
function cxSize(const R: TRect): TSize; overload; inline;
function cxSize(const S: TdxSizeF; AUseTrunc: Boolean = True): TSize; overload; inline;
function cxSizeIsEmpty(const S1: TSize): Boolean; inline;
function cxSizeIsValid(const S1: TSize): Boolean; inline;
function cxSizeIsEqual(const S1, S2: TSize): Boolean; overload; inline;
function cxSizeIsEqual(const R1, R2: TRect): Boolean; overload; inline;
function cxSizeMax(const S1, S2: TSize): TSize; inline;
function cxSizeProportionalStretch(const S: TSize; const ASize: TSize): TSize; overload; inline;
function cxSizeProportionalStretch(const S: TSize; W, H: Integer): TSize; overload; inline;
function cxSizeScale(const S: TSize; AScaleFactor: Single): TSize; overload; inline;
function cxSizeScale(const S: TSize; ANumerator, ADenominator: Integer): TSize; overload; inline;

// float routines
function dxPointF(X, Y: Single): TdxPointF; overload; inline;
function dxPointF(const P: TPoint): TdxPointF; overload; inline;
function dxSizeF(DX, DY: Single): TdxSizeF; inline;
function dxRectF(const ALeft, ATop, ARight, ABottom: Single): TdxRectF; overload; inline;
function dxRectF(const ARect: TRect): TdxRectF; overload; inline;

function dxRingPtIn(const ARingRect: TRect; const P: TPoint): Boolean; inline;
function dxRingAngle(const ABase, APoint: TdxPointF): Single; inline;
function dxRingPoint(const ABase: TdxPointF; ARadius, AAngle: Single): TdxPointF; // inline;
function dxRingRelativePoint(const ABase, APoint: TdxPointF): TdxPointF; inline;
function dxRingRotatePoint(const ABase, APoint: TdxPointF; AAngle: Single): TdxPointF; inline;
procedure dxRingRotatePoints(const ABase: TdxPointF; var APoints: array of TdxPointF; AAngle: Single);

// Angle
function dxNormalizeAngle(const AAngle: Double): Double;

// Double routines
function dxSizeDouble(AWidth, AHeight: Double): TdxSizeDouble; inline;
function dxSizeDoubleIsEqual(const ASize1, ASize2: TdxSizeDouble): Boolean; inline;
function dxSizeDoubleScale(const ASize: TdxSizeDouble; const AScale: TdxPointDouble): TdxSizeDouble; inline;
function dxPointDouble(X, Y: Double): TdxPointDouble; overload; inline;
function dxPointDouble(const APoint: TPoint): TdxPointDouble; overload; inline;
function dxPointDouble(const APoint: TdxPointF): TdxPointDouble; overload; inline;
function dxPointDoubleIsEqual(const APoint1, APoint2: TdxPointDouble): Boolean; inline;
function dxPointDoubleToPoint(const APoint: TdxPointDouble): TPoint; inline;
function dxPointDoubleToPointF(const APoint: TdxPointDouble): TdxPointF; inline;
function dxPointDoubleOffset(const APoint: TdxPointDouble; X, Y: Double;
  APositiveFactor: Boolean = True): TdxPointDouble; overload; inline;
function dxPointDoubleOffset(const APoint: TdxPointDouble; const AOffset: TdxPointDouble;
  APositiveFactor: Boolean = True): TdxPointDouble; overload; inline;
function dxPointDoubleScale(const APoint: TdxPointDouble; const AScale: TdxPointDouble): TdxPointDouble; overload; inline;
function dxRectDouble(ALeft, ATop, AWidth, AHeight: Double): TdxRectDouble; overload; inline;
function dxRectDouble(const ATopLeft, ABottomRight: TdxPointDouble): TdxRectDouble; overload; inline;
function dxRectDouble(const ARect: TRect): TdxRectDouble; overload; inline;
function dxRectDoubleIntersect(const ARect1, ARect2: TdxRectDouble): Boolean; inline;
function dxRectDoubleIsEmpty(const ARect: TdxRectDouble): Boolean; inline;
function dxRectDoubleIsEqual(const ARect1, ARect2: TdxRectDouble): Boolean; inline;
function dxRectDoubleOffset(const ARect: TdxRectDouble; const APoint: TdxPointDouble;
  APositiveFactor: Boolean = True): TdxRectDouble; inline;
function dxRectDoubleToRect(const AMapRect: TdxRectDouble): TRect; inline;
function dxRectDoubleToRectF(const AMapRect: TdxRectDouble): TdxRectF; inline;

procedure cxReduceFraction(var A, B: Integer);

const
  cxInvalidRect: TRect = (Left: -1; Top: -1; Right: -1; Bottom: -1);
  cxSimpleRect: TRect = (Left: 1; Top: 1; Right: 1; Bottom: 1);
  cxInvalidPoint: TPoint = (X: -1; Y: -1);
  cxInvalidSize: TSize = (cx: -1; cy: -1);
  cxSimplePoint: TPoint = (X: 1; Y: 1);
  cxInvisiblePoint: TPoint = (X: -10000; Y: -10000);
  cxMaxRectSize = 32000;

var
  cxNullPoint: TPoint;
  dxNullPointF: TdxPointF;
  dxNullPointDouble: TdxPointDouble;

  cxNullRect: TRect;
  dxNullRectF: TdxRectF;
  dxNullRectDouble: TdxRectDouble;

  cxNullSize: TSize;

implementation

type

  { TdxScaleFactorListenerComparer }

  TdxScaleFactorListenerComparer = class(TInterfacedObject, IComparer<TdxScaleFactorNotifyEvent>)
  strict private
    class var FDefault: IComparer<TdxScaleFactorNotifyEvent>;
    class function GetDefault: IComparer<TdxScaleFactorNotifyEvent>; static;
  protected
    // IComparer<TdxScaleFactorNotifyEvent>
    function Compare(const Left, Right: TdxScaleFactorNotifyEvent): Integer;
  public
    class property Default: IComparer<TdxScaleFactorNotifyEvent> read GetDefault;
  end;

function cxHalfCoordinate(ASize: Integer): Integer; inline;
begin
  Result := (ASize - Integer(Odd(ASize))) div 2;
end;

function cxRectHeight(const R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;

function cxRectHeight(const R: TdxRectF): Single;
begin
  Result := R.Bottom - R.Top;
end;

function cxRectIntersect(const R1, R2: TRect): Boolean;
var
  R: TRect;
begin
  Result := cxRectIntersect(R, R1, R2);
end;

function cxRectIntersect(const R1, R2: TdxRectF): Boolean;
var
  R: TdxRectF;
begin
  Result := cxRectIntersect(R, R1, R2);
end;

function cxRectIntersect(out R: TRect; const R1, R2: TRect): Boolean;
begin
  R.Left := Max(R2.Left, R1.Left);
  R.Top := Max(R2.Top, R1.Top);
  R.Right := Min(R2.Right, R1.Right);
  R.Bottom := Min(R2.Bottom, R1.Bottom);
  Result := not ((R.Right <= R.Left) or (R.Bottom <= R.Top));
  if not Result then
    R := cxNullRect;
end;

function cxRectIntersect(out R: TdxRectF; const R1, R2: TdxRectF): Boolean;
begin
  R.Left := Max(R2.Left, R1.Left);
  R.Top := Max(R2.Top, R1.Top);
  R.Right := Min(R2.Right, R1.Right);
  R.Bottom := Min(R2.Bottom, R1.Bottom);
  Result := not ((R.Right <= R.Left) or (R.Bottom <= R.Top));
  if not Result then
    R := dxNullRectF;
end;

function cxRectInflate(const R: TRect; DX, DY: Integer): TRect;
begin
  Result := cxRectInflate(R, DX, DY, DX, DY);
end;

function cxRectInflate(const R: TRect; DX1, DY1, DX2, DY2: Integer): TRect;
begin
  Result := R;
  Dec(Result.Left, DX1);
  Dec(Result.Top, DY1);
  Inc(Result.Right, DX2);
  Inc(Result.Bottom, DY2);
end;

function cxRectInflate(const R: TRect; Delta: Integer): TRect;
begin
  Result := cxRectInflate(R, Delta, Delta);
end;

function cxRectInflate(const R, AMargins: TRect): TRect;
begin
  with AMargins do
    Result := cxRectInflate(R, Left, Top, Right, Bottom);
end;

function cxRectInflate(const R: TdxRectF; DX, DY: Single): TdxRectF; overload; inline;
begin
  Result := cxRectInflate(R, cxRectF(DX, DY, DX, DY));
end;

function cxRectInflate(const R: TdxRectF; Delta: Single): TdxRectF; overload; inline;
begin
  Result := cxRectInflate(R, Delta, Delta);
end;

function cxRectInflate(const ARect, AMargins: TdxRectF): TdxRectF;
begin
  Result := ARect;
  Result.Bottom := Result.Bottom + AMargins.Bottom;
  Result.Right := Result.Right + AMargins.Right;
  Result.Left := Result.Left - AMargins.Left;
  Result.Top := Result.Top - AMargins.Top;
end;

function cxRectInvert(const R: TRect): TRect;
begin
  Result.TopLeft := cxPointInvert(R.TopLeft);
  Result.BottomRight := cxPointInvert(R.BottomRight);
end;

function cxRectIsEmpty(const R: TRect): Boolean;
begin
  with R do
    Result := (Right <= Left) or (Bottom <= Top);
end;

function cxRectIsEmpty(const R: TdxRectF): Boolean; inline;
begin
  Result := (R.Right <= R.Left) or (R.Bottom <= R.Top);
end;

function cxRectIsEqual(const R, R1: TRect): Boolean;
begin
  Result := (R.Left = R1.Left) and (R.Top = R1.Top) and
    (R.Right = R1.Right) and (R.Bottom = R1.Bottom);
end;

function cxRectIsInvalid(const R: TRect): Boolean;
begin
  Result := cxRectIsEqual(R, cxInvalidRect);
end;

function cxRectIsNull(const R: TRect): Boolean;
begin
  Result := (R.Left = 0) and (R.Top = 0) and (R.Right = 0) and (R.Bottom = 0);
end;

function cxRectGetItem(const ARect: TRect; AIndex: Integer): Integer;
begin
  case AIndex of
    0:
      Result := ARect.Left;
    1:
      Result := ARect.Top;
    2:
      Result := ARect.Right;
    3:
      Result := ARect.Bottom;
  else
    Result := 0
  end;
end;

function cxRectGetSize(const ARect: TRect; AIndex: Integer): Integer;
begin
  if AIndex = 0 then
    Result := ARect.Right - ARect.Left
  else
    Result := ARect.Bottom - ARect.Top;
end;

function cxRectLeftBottom(const R: TRect): TPoint;
begin
  Result := cxPoint(R.Left, R.Bottom);
end;

procedure cxRectMinMaxHeight(const R: TRect; var AMax, AMin: Integer);
begin
  with R do
  begin
    if AMax < Bottom then AMax := Bottom;
    if AMin > Top then AMin := Top;
  end;
end;

procedure cxRectMinMaxInit(var AMin, AMax: Integer);
begin
  AMin := cxMaxRectSize;
  AMax := -1;
end;

procedure cxRectMinMaxWidth(const R: TRect; var AMax, AMin: Integer);
begin
  if AMax < R.Right then
    AMax := R.Right;
  if AMin > R.Left then
    AMin := R.Left;
end;

function cxRectOffset(const R: TRect; const P: TPoint; APositiveFactor: Boolean = True): TRect;
begin
  Result := cxRectOffset(R, P.X, P.Y, APositiveFactor);
end;

function cxRectOffset(const R: TRect; DX, DY: Integer; APositiveFactor: Boolean = True): TRect;
begin
  Result := cxRect(cxPointOffset(R.TopLeft, DX, DY, APositiveFactor), cxPointOffset(R.BottomRight, DX, DY, APositiveFactor));
end;

function cxRectOffset(const R: TdxRectF; const P: TdxPointF; APositiveFactor: Boolean = True): TdxRectF;
begin
  Result := cxRectOffset(R, P.X, P.Y, APositiveFactor);
end;

function cxRectOffset(const R: TdxRectF; DX, DY: Single; APositiveFactor: Boolean = True): TdxRectF;
begin
  if not APositiveFactor then
  begin
    DX := -DX;
    DY := -DY;
  end;

  Result.Bottom := R.Bottom + DY;
  Result.Left := R.Left + DX;
  Result.Right := R.Right + DX;
  Result.Top := R.Top + DY;
end;

procedure cxRectsOffset(var Rs: array of TdxRectF; const AOffset: TdxPointF; APositiveFactor: Boolean);
var
  I: Integer;
begin
  for I := Low(Rs) to High(Rs) do
    Rs[I] := cxRectOffset(Rs[I], AOffset, APositiveFactor);
end;

function cxRectOffset(const R: TRect; const Ps: array of TPoint; APositiveFactor: Boolean = True): TRect;
begin
  with cxPointSum(Ps) do
    Result := cxRectOffset(R, X, Y, APositiveFactor);
end;

function cxRectOffsetHorz(const R: TRect; DX: Integer): TRect;
begin
  Result := R;
  Inc(Result.Left, DX);
  Inc(Result.Right, DX);
end;

function cxRectOffsetVert(const R: TRect; DY: Integer): TRect;
begin
  Result := R;
  Inc(Result.Top, DY);
  Inc(Result.Bottom, DY);
end;

function GetRectCoordinate(const R: TRect; ABottomRight, AVertCoordinate: Boolean): Integer;
begin
  if ABottomRight then
    if AVertCoordinate then
      Result := R.Bottom
    else
      Result := R.Right
  else
    if AVertCoordinate then
      Result := R.Top
    else
      Result := R.Left;
end;

procedure SetRectCoordinate(var R: TRect; ABottomRight, AVertCoordinate: Boolean; AValue: Integer);
begin
  if ABottomRight then
    if AVertCoordinate then
      R.Bottom := AValue
    else
      R.Right := AValue
  else
    if AVertCoordinate then
      R.Top := AValue
    else
      R.Left := AValue;
end;

procedure cxRectOverlapped(const ASrc, ADst: TRect; out ASrcH, ASrcV, ADstH, ADstV: TRect);
var
  H, W: Integer;

  procedure IncV(const ARect: TRect; AVertCoordinate, ABottomRight: Boolean;
    Value: Integer; out AResult: TRect);
  begin
    if Value <> 0 then
    begin
      AResult := ARect;
      SetRectCoordinate(AResult, ABottomRight, AVertCoordinate,
        GetRectCoordinate(AResult, not ABottomRight, AVertCoordinate) + Value);
    end
    else
      AResult := cxNullRect;
  end;

begin
  H := ASrc.Bottom - ADst.Bottom;
  W := ASrc.Right - ADst.Right;
  IncV(ASrc, True, H < 0, -H, ASrcH);
  IncV(ADst, True, H > 0, H, ADstH);
  IncV(ASrc, False, W < 0, -W, ASrcV);
  IncV(ADst, False, W > 0, W, ADstV);
end;

function cxRectPtIn(const R: TRect; const P: TPoint): Boolean;
begin
  Result := cxRectPtIn(R, P.X, P.Y);
end;

function cxRectPtIn(const R: TRect; const X, Y: Integer): Boolean;
begin
  with R do
    Result := (X >= Left) and (X < Right) and (Y >= Top) and (Y < Bottom);
end;

function cxRectPtInEx(const R: TRect; const X, Y: Integer; DL, DT, DR, DB: Integer): TcxPtInRectType;

  function InRange(V, V1, V2: Integer): Boolean; inline;
  begin
    V := V - V1;
    Result := (V >= -V2) and (V < V2);
  end;

begin
  if cxRectPtIn(R, X, Y) then
    Result := ptrtArea
  else
    if cxRectPtIn(cxRectInflate(R, DL, DT, DR, DB), X, Y) then
    begin
      with R do
      begin
        if InRange(X, Right, DR) then
          Result := ptrtRight
        else
          if InRange(Y, Bottom, DB) then
            Result := ptrtBottom
          else
            if InRange(X, Left, DL) then
              Result := ptrtLeft
            else
              Result := ptrtTop;
      end;
    end
    else
      Result := ptrtNone;
end;

function cxRectRotate(const R: TRect): TRect;
begin
  Result := Rect(R.Top, R.Left, R.Bottom, R.Right);
end;


function cxRectPtInEx(const R: TRect; X, Y, ADelta: Integer): TcxPtInRectType;
begin
  Result := cxRectPtInEx(R, X, Y, ADelta, ADelta, ADelta, ADelta);
end;

function cxRectPtInEx(const R: TRect;
  X, Y, ADeltaX, ADeltaY: Integer): TcxPtInRectType;
begin
  Result := cxRectPtInEx(R, X, Y, ADeltaX, ADeltaY, ADeltaX, ADeltaY);
end;

function cxRect(const ASize: TSize): TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := ASize.cx;
  Result.Bottom := ASize.cy;
end;

function cxRect(const ALeft, ATop, ARight, ABottom: Integer): TRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ARight;
  Result.Bottom := ABottom;
end;

function cxRect(const ATopLeft, ABottomRight: TPoint): TRect;
begin
  Result.TopLeft := ATopLeft;
  Result.BottomRight := ABottomRight;
end;

function cxRect(const ARect: TdxRectF; AUseTrunc: Boolean = True): TRect;
begin
  if AUseTrunc then
    Result := cxRect(Trunc(ARect.Left), Trunc(ARect.Top), Trunc(ARect.Right), Trunc(ARect.Bottom))
  else
    Result := cxRect(Round(ARect.Left), Round(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));
end;

function cxRectF(const ATopLeft, ABottomRight: TdxPointF): TdxRectF;
begin
  Result := cxRectF(ATopLeft.X, ATopLeft.Y, ABottomRight.X, ABottomRight.Y);
end;

function cxRectF(const ALeft, ATop, ARight, ABottom: Single): TdxRectF;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ARight;
  Result.Bottom := ABottom;
end;

function cxRectF(const ARect: TRect): TdxRectF;
begin
  Result := cxRectF(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

function cxRectAdjust(const R: TRect): TRect;
begin
  Result := Rect(Min(R.Left, R.Right), Min(R.Top, R.Bottom), Max(R.Left, R.Right), Max(R.Top, R.Bottom));
end;

function cxRectAdjustF(const R: TdxRectF): TdxRectF;
begin
  Result := dxRectF(Min(R.Left, R.Right), Min(R.Top, R.Bottom), Max(R.Left, R.Right), Max(R.Top, R.Bottom));
end;

function cxRectBounds(ALeft, ATop, AWidth, AHeight: Integer): TRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ALeft + AWidth;
  Result.Bottom := ATop + AHeight;
end;

function cxRectBounds(ALeft, ATop: Integer; const ASize: TSize): TRect;
begin
  Result := cxRectBounds(ALeft, ATop, ASize.cx, ASize.cy);
end;

function cxRectFBounds(ALeft, ATop, AWidth, AHeight: Single): TdxRectF;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ALeft + AWidth;
  Result.Bottom := ATop + AHeight;
end;

function cxRectBounds(const P: TPoint; AWidth, AHeight: Integer): TRect;
begin
  Result.Left := P.X;
  Result.Top := P.Y;
  Result.Right := P.X + AWidth;
  Result.Bottom := P.Y + AHeight;
end;

function cxRectRightTop(const R: TRect): TPoint;
begin
  Result := cxPoint(R.Right, R.Top);
end;

function cxRectScale(const R: TdxRectF; AScaleFactor: Single): TdxRectF;
begin
  Result.Bottom := R.Bottom * AScaleFactor;
  Result.Left := R.Left * AScaleFactor;
  Result.Right := R.Right * AScaleFactor;
  Result.Top := R.Top * AScaleFactor;
end;

function cxRectScale(const R: TRect; ANumerator, ADenominator: Integer): TRect;
begin
  Result := cxRectScale(R, ANumerator, ADenominator, ANumerator, ADenominator);
end;

function cxRectScale(const R: TRect; ANumeratorX, ADenominatorX, ANumeratorY, ADenominatorY: Integer): TRect;
begin
  Result.Left := MulDiv(R.Left, ANumeratorX, ADenominatorX);
  Result.Top := MulDiv(R.Top, ANumeratorY, ADenominatorY);
  Result.Right := MulDiv(R.Right, ANumeratorX, ADenominatorX);
  Result.Bottom := MulDiv(R.Bottom, ANumeratorY, ADenominatorY);
end;

function cxRectScaleSize(const R: TRect; ANumerator, ADenominator: Integer): TRect;
begin
  Result := R;
  Result.Right := Result.Left + MulDiv(Result.Right - Result.Left, ANumerator, ADenominator);
  Result.Bottom := Result.Top + MulDiv(Result.Bottom - Result.Top, ANumerator, ADenominator);
end;

function cxRectScaleHeight(const R: TRect; Y1, Y2, H1, H2: Integer): TRect;
var
  H: Integer;
begin
  Result := R;
  with Result do
  begin
    H := MulDiv(Bottom - Top, H2, H1);
    Top := MulDiv(Y1 - Top, H2, H1) + Y2;
    Bottom := Top + H;
  end;
end;

function cxRectScaleWidth(const R: TRect; X1, X2, W1,
  W2: Integer): TRect;
var
  L: Integer;
begin
  Result := R;
  with Result do
  begin
    L := MulDiv(Right - Left, W2, W1);
    Left := MulDiv(X1 - Left, W2, W1) + X2;
    Right := Left + L;
  end;
end;

function cxRectSetBottom(const R: TRect; Y: Integer): TRect;
begin
  Result := cxRectSetBottom(R, Y, R.Bottom - R.Top);
end;

function cxRectSetBottom(const R: TRect; Y, H: Integer): TRect;
begin
  Result := R;
  Result.Bottom := Y;
  Result.Top := Y - H;
end;

function cxRectSetHeight(const R: TRect; H: Integer): TRect;
begin
  Result := R;
  Result.Bottom := Result.Top + H;
end;

function cxRectSetLeft(const R: TRect; X: Integer): TRect;
begin
  Result := cxRectSetLeft(R, X, R.Right - R.Left);
end;

function cxRectSetLeft(const R: TRect; X, W: Integer): TRect;
begin
  Result := R;
  Result.Left := X;
  Result.Right := X + W;
end;

function cxRectSetOrigin(const R: TRect; const APos: TPoint): TRect;
begin
  Result := cxRectOffset(R, APos.X - R.Left, APos.Y - R.Top);
end;

function cxRectSetNullOrigin(const R: TRect): TRect;
begin
  Result := cxRectSetOrigin(R, cxNullPoint);
end;

function cxRectSetTop(const R: TRect; Y: Integer): TRect;
begin
  Result := cxRectSetTop(R, Y, R.Bottom - R.Top);
end;

function cxRectSetTop(const R: TdxRectF; Y: Single): TdxRectF;
begin
  Result := cxRectSetTop(R, Y, R.Bottom - R.Top);
end;

function cxRectSetTop(const R: TRect; Y, H: Integer): TRect;
begin
  Result := R;
  Result.Top := Y;
  Result.Bottom := Y + H;
end;

function cxRectSetTop(const R: TdxRectF; Y, H: Single): TdxRectF;
begin
  Result := R;
  Result.Top := Y;
  Result.Bottom := Y + H;
end;

function cxRectSetRight(const R: TRect; X: Integer): TRect;
begin
  Result := cxRectSetRight(R, X, R.Right - R.Left);
end;

function cxRectSetRight(const R: TRect; X, W: Integer): TRect;
begin
  Result := R;
  Result.Right := X;
  Result.Left := X - W;
end;

function cxRectSetSize(const R: TRect; W, H: Integer): TRect;
begin
  Result := R;
  with R do
  begin
    Result.Right := Left + W;
    Result.Bottom := Top + H;
  end;
end;

function cxRectSetSize(const R: TRect; const ASize: TSize): TRect;
begin
  Result := R;
  Result.Right := Result.Left + ASize.cx;
  Result.Bottom := Result.Top + ASize.cy;
end;

function cxRectSetSizeF(const R: TdxRectF; const ASize: TdxSizeF): TdxRectF;
begin
  Result := R;
  Result.Right := Result.Left + ASize.cx;
  Result.Bottom := Result.Top + ASize.cy;
end;

function cxRectSetWidth(const R: TRect; W: Integer): TRect;
begin
  Result := R;
  Result.Right := R.Left + W;
end;

function cxRectSetWidth(const R: TRect; X, W: Integer): TRect;
begin
  Result := R;
  Result.Left := X;
  Result.Right := X + W;
end;

function cxRectSetXPos(const R: TRect; X1, X2: Integer): TRect;
begin
  with R do
    Result := cxRect(X1, Top, X2, Bottom);
end;

function cxRectSetYPos(const R: TRect; Y1, Y2: Integer): TRect;
begin
  with R do
    Result := cxRect(Left, Y1, Right, Y2);
end;

function cxRectSize(const R: TRect): TSize;
begin
  Result.cx := cxRectWidth(R);
  Result.cy := cxRectHeight(R);
end;

procedure cxRectSplitHorz(const ABounds: TRect; var ARect1, ARect2: TRect);
begin
  ARect1 := ABounds;
  ARect2 := ABounds;
  ARect1.Right := (ABounds.Right + ABounds.Left) div 2;
  ARect2.Left := ARect1.Right;
end;

function cxRectSquare(const R: TRect): Integer;
begin
  Result := (R.Right - R.Left) * (R.Bottom - R.Top);
end;

procedure cxRectSplitVert(const ABounds: TRect; var ARect1, ARect2: TRect);
begin
  ARect1 := ABounds;
  ARect2 := ABounds;
  ARect1.Bottom := (ABounds.Bottom + ABounds.Top) div 2;
  ARect2.Top := ARect1.Bottom;
end;

function cxRectTransform(const ARect: TRect; ALeftOffset, ATopOffset, ARightOffset, ABottomOffset: Integer): TRect;
begin
  Result := ARect;
  Inc(Result.Left, ALeftOffset);
  Inc(Result.Top, ATopOffset);
  Inc(Result.Right, ARightOffset);
  Inc(Result.Bottom, ABottomOffset);
end;

function cxRectTransform(const ARect, AOffsets: TRect): TRect;
begin
  with AOffsets do
    Result := cxRectTransform(ARect, Left, Top, Right, Bottom);
end;

function cxRectUnion(const R1, R2: TRect): TRect;
begin
  Result := R1;
  if (R2.Right - R2.Left <= 0) or (R2.Bottom - R2.Top <= 0) then Exit;
  if R2.Left < R1.Left then
    Result.Left := R2.Left;
  if R2.Top < R1.Top then
    Result.Top := R2.Top;
  if R2.Right > R1.Right then
    Result.Right := R2.Right;
  if R2.Bottom > R1.Bottom then
    Result.Bottom := R2.Bottom;
end;

function cxRectUnion(const R1, R2: TdxRectF): TdxRectF;
begin
  Result := R1;
  if (R2.Right - R2.Left <= 0) or (R2.Bottom - R2.Top <= 0) then Exit;
  if R2.Left < R1.Left then
    Result.Left := R2.Left;
  if R2.Top < R1.Top then
    Result.Top := R2.Top;
  if R2.Right > R1.Right then
    Result.Right := R2.Right;
  if R2.Bottom > R1.Bottom then
    Result.Bottom := R2.Bottom;
end;

function cxRectWidth(const R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;

function cxRectWidth(const R: TdxRectF): Single;
begin
  Result := R.Right - R.Left;
end;

function cxRectProportionalStretch(const R: TRect; const ASize: TSize): TRect;
begin
  Result := cxRectProportionalStretch(R, ASize.cx, ASize.cy);
end;

function cxRectProportionalStretch(const R: TRect; W, H: Integer): TRect;
begin
  Result := cxRectSetSize(R, cxSizeProportionalStretch(cxSize(R), W, H));
end;

function cxMargins(const ARect, AContentRect: TRect): TRect;
begin
  Result.Left := AContentRect.Left - ARect.Left;
  Result.Top := AContentRect.Top - ARect.Top;
  Result.Right := ARect.Right - AContentRect.Right;
  Result.Bottom := ARect.Bottom - AContentRect.Bottom;
end;

function cxMarginsHeight(const R: TRect): Integer;
begin
  Result := R.Top + R.Bottom;
end;

function cxMarginsWidth(const R: TRect): Integer;
begin
  Result := R.Left + R.Right;
end;

function cxRectCompare(const R1, R2: TRect): Boolean;
begin
  Result := cxRectIsEqual(R1, R2);
end;

function cxRectCenter(const R: TRect): TPoint;
begin
  Result := cxPoint(cxHalfCoordinate(R.Right + R.Left), cxHalfCoordinate(R.Bottom + R.Top));
end;

function cxRectCenter(const ABounds: TRect; const ASize: TSize): TRect;
begin
  Result := cxRectCenter(ABounds, ASize.cx, ASize.cy)
end;

function cxRectCenter(const ABounds: TRect; AWidth, AHeight: Integer): TRect;
begin
  Result.Left := cxHalfCoordinate(ABounds.Left + ABounds.Right - AWidth);
  Result.Top := cxHalfCoordinate(ABounds.Top + ABounds.Bottom - AHeight);
  Result.Right := Result.Left + AWidth;
  Result.Bottom := Result.Top + AHeight;
end;

function cxRectCenter(const ABounds: TdxRectF; AWidth, AHeight: Single): TdxRectF;
begin
  Result.Left := (ABounds.Left + ABounds.Right - AWidth) * 0.5;
  Result.Top := (ABounds.Top + ABounds.Bottom - AHeight) * 0.5;
  Result.Right := Result.Left + AWidth;
  Result.Bottom := Result.Top + AHeight;
end;

function cxRectCenter(const R: TdxRectF): TdxPointF;
begin
  Result.X := (R.Left + R.Right) / 2;
  Result.Y := (R.Top + R.Bottom) / 2;
end;

function cxRectCenterHorizontally(const R: TdxRectF; const AWidth: Double): TdxRectF;
begin
  Result := R;
  Result.Left := (R.Left + R.Right - AWidth) * 0.5;
  Result.Right := Result.Left + AWidth;
end;

function cxRectCenterHorizontally(const R: TRect; AWidth: Integer): TRect;
begin
  Result := R;
  Result.Left := cxHalfCoordinate(R.Left + R.Right - AWidth);
  Result.Right := Result.Left + AWidth;
end;

function cxRectCenterVertically(const R: TdxRectF; const AHeight: Double): TdxRectF;
begin
  Result := R;
  Result.Top := (R.Top + R.Bottom - AHeight) * 0.5;
  Result.Bottom := Result.Top + AHeight;
end;

function cxRectCenterVertically(const R: TRect; AHeight: Integer): TRect;
begin
  Result := R;
  Result.Top := cxHalfCoordinate(R.Top + R.Bottom - AHeight);
  Result.Bottom := Result.Top + AHeight;
end;

function cxRectContain(const ABounds, AInner: TRect): Boolean;
begin
  Result := (ABounds.Left <= AInner.Left) and (ABounds.Right >= AInner.Right) and
    (ABounds.Top <= AInner.Top) and (ABounds.Bottom >= AInner.Bottom);
end;

function cxRectContent(const R, AMargins: TRect): TRect;
begin
  Result.Left := R.Left + AMargins.Left;
  Result.Top := R.Top + AMargins.Top;
  Result.Right := R.Right - AMargins.Right;
  Result.Bottom := R.Bottom - AMargins.Bottom;
end;

function cxRectContent(const R, AMargins: TdxRectF): TdxRectF;
begin
  with Result do
  begin
    Left := R.Left + AMargins.Left;
    Top := R.Top + AMargins.Top;
    Right := R.Right - AMargins.Right;
    Bottom := R.Bottom - AMargins.Bottom;
  end;
end;

function cxPointDistance(const P1, P2: TPoint): Single;
begin
  Result := Sqrt(Sqr(P1.X - P2.X) + Sqr(P1.Y - P2.Y));
end;

function cxPointDistanceF(const P1, P2: TdxPointF): Single;
begin
  Result := Sqrt(Sqr(P1.X - P2.X) + Sqr(P1.Y - P2.Y));
end;

function cxPointInvert(const P: TPoint): TPoint;
begin
  Result.X := -P.X;
  Result.Y := -P.Y;
end;

function cxPointInLine(const P, LinePoint1, LinePoint2: TdxPointF): Boolean;
const
  Epsilon = 0.01;
var
  X0, X1, Y0, Y1: Single;
begin
  X0 := Min(LinePoint1.X, LinePoint2.X);
  X1 := Max(LinePoint1.X, LinePoint2.X);
  Y0 := Min(LinePoint1.Y, LinePoint2.Y);
  Y1 := Max(LinePoint1.Y, LinePoint2.Y);
  Result := InRange(P.X, X0, X1) and InRange(P.Y, Y0, Y1) and (
    (Abs(Y0 - Y1) < Epsilon) or (Abs(X0 - X1) < Epsilon) or
    (Abs((P.X - X0) / (X1 - X0) - (P.Y - Y0) / (Y1 - Y0)) < Epsilon));
end;

function cxPointInLine(const P, LinePoint1, LinePoint2: TPoint): Boolean;
begin
  Result := cxPointInLine(cxPointF(P), cxPointF(LinePoint1), cxPointF(LinePoint2));
end;

function cxPointIsEqual(const P1, P2: TPoint): Boolean;
begin
  Result := (P1.X = P2.X) and (P1.Y = P2.Y);
end;

function cxPointIsEqual(const P1, P2: TdxPointF): Boolean;
begin
  Result := SameValue(P1.X, P2.X) and SameValue(P1.Y, P2.Y);
end;

function cxPointIsInvalid(const P: TPoint): Boolean;
begin
  Result := cxPointIsEqual(P, cxInvalidPoint);
end;

function cxPointIsNull(const P: TPoint): Boolean;
begin
  Result := (P.X = cxNullPoint.X) and (P.Y = cxNullPoint.Y);
end;

function cxPointGetItem(const P: TPoint; AIndex: Integer): Integer;
begin
  if AIndex = 0 then
    Result := P.X
  else
    Result := P.Y;
end;

function cxPointOffset(const P: TPoint; const Ps: array of TPoint; APositiveFactor: Boolean = True): TPoint;
begin
  with cxPointSum(Ps) do
    Result := cxPointOffset(P, X, Y, APositiveFactor);
end;

function cxPointOffset(const P: TPoint; X, Y: Integer; APositiveFactor: Boolean = True): TPoint;
begin
  if APositiveFactor then
    Result := cxPoint(P.X + X, P.Y + Y)
  else
    Result := cxPoint(P.X - X, P.Y - Y);
end;

function cxPointOffset(const P, DP: TPoint; APositiveFactor: Boolean = True): TPoint;
begin
  Result := cxPointOffset(P, DP.X, DP.Y, APositiveFactor);
end;

function cxPointOffset(const P, AOffset: TdxPointF; APositiveFactor: Boolean = True): TdxPointF;
begin
  Result := cxPointOffset(P, AOffset.X, AOffset.Y, APositiveFactor);
end;

function cxPointOffset(const P: TdxPointF; X, Y: Single; APositiveFactor: Boolean = True): TdxPointF;
begin
  if APositiveFactor then
    Result := dxPointF(P.X + X, P.Y + Y)
  else
    Result := dxPointF(P.X - X, P.Y - Y);
end;

procedure cxPointsOffset(var Ps: array of TdxPointF; const AOffset: TdxPointF);
var
  I: Integer;
begin
  for I := Low(Ps) to High(Ps) do
    Ps[I] := cxPointOffset(Ps[I], AOffset);
end;

function cxPointReplaceItem(const P: TPoint; const AIndex, AValue: Integer): TPoint;
begin
  if AIndex = 0 then
    Result.X := AValue
  else
    Result.Y := AValue;
end;

function cxPoint(X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function cxPoint(const APointF: TdxPointF; AUsedTrunc: Boolean = True): TPoint;
begin
  if AUsedTrunc then
    Result := cxPoint(Trunc(APointF.X), Trunc(APointF.Y))
  else
    Result := cxPoint(Round(APointF.X), Round(APointF.Y));
end;

function cxPoint(const ASize: TSize): TPoint;
begin
  with Result do
  begin
    X := ASize.cx;
    Y := ASize.cy;
  end;
end;

function cxPointF(X, Y: Single): TdxPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

function cxPointF(const P: TPoint): TdxPointF;
begin
  Result.X := P.X;
  Result.Y := P.Y;
end;

function cxPointSum(const Ps: array of TPoint): TPoint;
var
  I: Integer;
begin
  if Length(Ps) >= 1 then
  begin
    Result := Ps[Low(Ps)];
    for I := Low(Ps) + 1 to High(Ps) do
      with Ps[I] do
      begin
        Inc(Result.X, X);
        Inc(Result.Y, Y);
      end;
  end
  else
    Result := cxNullPoint;
end;

function cxPointScale(const P: TPoint; ANumerator, ADenominator: Integer): TPoint;
begin
  Result.X := MulDiv(P.X, ANumerator, ADenominator);
  Result.Y := MulDiv(P.Y, ANumerator, ADenominator);
end;

function cxPointScale(const P: TdxPointF; AScaleFactor: Single): TdxPointF;
begin
  Result.X := P.X * AScaleFactor;
  Result.Y := P.Y * AScaleFactor;
end;

function cxPointsBox(const Ps: array of TPoint): TRect;
var
  I: Integer;
begin
  if Length(Ps) > 0 then
  begin
    Result := cxRect(Ps[Low(Ps)], Ps[Low(Ps)]);
    for I := Low(Ps) + 1 to High(Ps) do
      with Ps[I] do
      begin
        Result.Bottom := Max(Result.Bottom, Y);
        Result.Left := Min(Result.Left, X);
        Result.Right := Max(Result.Right, X);
        Result.Top := Min(Result.Top, Y);
      end;
  end
  else
    Result := cxNullRect;
end;

function cxPointsBox(const Ps: array of TdxPointF): TdxRectF;
var
  I: Integer;
begin
  if Length(Ps) > 0 then
  begin
    Result := cxRectF(Ps[Low(Ps)], Ps[Low(Ps)]);
    for I := Low(Ps) + 1 to High(Ps) do
      with Ps[I] do
      begin
        Result.Bottom := Max(Result.Bottom, Y);
        Result.Left := Min(Result.Left, X);
        Result.Right := Max(Result.Right, X);
        Result.Top := Min(Result.Top, Y);
      end;
  end
  else
    Result := dxNullRectF;
end;

procedure cxPointsOffset(var Ps: array of TPoint; DX, DY: Integer);
var
  I: Integer;
begin
  for I := Low(Ps) to High(Ps) do
    with Ps[I] do
    begin
      Inc(X, DX);
      Inc(Y, DY);
    end;
end;

procedure cxPointsOffset(var Ps: array of TPoint; const AOffset: TPoint);
begin
  cxPointsOffset(Ps, AOffset.X, AOffset.Y);
end;


function cxSize(XY: Integer): TSize;
begin
  Result.cx := XY;
  Result.cy := XY;
end;

function cxSize(DX, DY: Integer): TSize;
begin
  Result.cx := DX;
  Result.cy := DY;
end;

function cxSize(const P: TPoint): TSize;
begin
  Result.cx := P.X;
  Result.cy := P.Y;
end;

function cxSize(const R: TRect): TSize;
begin
  Result := cxRectSize(R);
end;

function cxSize(const S: TdxSizeF; AUseTrunc: Boolean = True): TSize;
begin
  if AUseTrunc then
    Result := cxSize(Trunc(S.cx), Trunc(S.cy))
  else
    Result := cxSize(Round(S.cx), Round(S.cy));
end;

function cxSizeIsEmpty(const S1: TSize): Boolean;
begin
  Result := cxSizeIsEqual(S1, cxNullSize);
end;

function cxSizeIsValid(const S1: TSize): Boolean;
begin
  Result := (S1.cx > 0) and (S1.cy > 0);
end;

function cxSizeIsEqual(const S1, S2: TSize): Boolean;
begin
  Result := (S1.cx = S2.cx) and (S1.cy = S2.cy);
end;

function cxSizeIsEqual(const R1, R2: TRect): Boolean;
begin
  Result := (cxRectWidth(R1) = cxRectWidth(R2)) and (cxRectHeight(R1) = cxRectHeight(R2));
end;

function cxSizeMax(const S1, S2: TSize): TSize;
begin
  Result := cxSize(Max(S1.cx, S2.cx), Max(S1.cy, S2.cy));
end;

function cxSizeProportionalStretch(const S: TSize; const ASize: TSize): TSize;
begin
  Result := cxSizeProportionalStretch(S, ASize.cx, ASize.cy);
end;

function cxSizeProportionalStretch(const S: TSize; W, H: Integer): TSize;
var
  ARatio: Double;
begin
  Result := S;
  if (S.cx > 0) and (S.cy > 0) and (W > 0) and (H > 0) then
  begin
    ARatio := W / H;
    if ARatio > S.cx / S.cy then
      Result.cy := Trunc(S.cx / ARatio)
    else
      Result.cx := Trunc(S.cy * ARatio);
  end;
end;

function cxSizeScale(const S: TSize; AScaleFactor: Single): TSize;
begin
  Result.cx := Trunc(S.cx * AScaleFactor);
  Result.cy := Trunc(S.cy * AScaleFactor);
end;

function cxSizeScale(const S: TSize; ANumerator, ADenominator: Integer): TSize;
begin
  Result.cx := MulDiv(S.cx, ANumerator, ADenominator);
  Result.cy := MulDiv(S.cy, ANumerator, ADenominator);
end;


function dxPointF(X, Y: Single): TdxPointF;
begin
  Result := cxPointF(X, Y);
end;

function dxPointF(const P: TPoint): TdxPointF;
begin
  Result := cxPointF(P);
end;

function dxSizeF(DX, DY: Single): TdxSizeF;
begin
  Result.cx := DX;
  Result.cy := DY;
end;

function dxRectF(const ALeft, ATop, ARight, ABottom: Single): TdxRectF;
begin
  Result := cxRectF(ALeft, ATop, ARight, ABottom);
end;

function dxRectF(const ARect: TRect): TdxRectF;
begin
  Result := cxRectF(ARect);
end;

function dxRingPtIn(const ARingRect: TRect; const P: TPoint): Boolean;
var
  ABase: TdxPointF;
  ARingRadius: Single;
begin
  ABase.X := (ARingRect.Left + ARingRect.Right) / 2;
  ABase.Y := (ARingRect.Top + ARingRect.Bottom) / 2;
  ARingRadius := Min(cxRectWidth(ARingRect), cxRectHeight(ARingRect)) / 2;
  Result := cxPointDistanceF(dxPointF(P), ABase) < ARingRadius;
end;

function dxRingAngle(const ABase, APoint: TdxPointF): Single;
var
  ASign: TPoint;
  PC: TdxPointF;
begin
  PC := dxRingRelativePoint(ABase, APoint);
  if PC.X = 0 then
    PC.X := 0.00001;
  if PC.Y = 0 then
    PC.Y := 0.00001;
  Result := ArcTan(Abs(PC.Y) / Abs(PC.X));
  ASign := Point(Sign(PC.X), Sign(PC.Y));
  if (ASign.X = -1) and (ASign.Y = 1) then
    Result := Pi - Result
  else
    if (ASign.X = -1) and (ASign.Y = -1) then
      Result := Pi + Result
    else
      if (ASign.X = 1) and (ASign.Y = -1) then
        Result := 2 * Pi - Result;
end;

function dxRingPoint(const ABase: TdxPointF; ARadius, AAngle: Single): TdxPointF;
begin
  Result := dxPointF(ABase.X + ARadius * Cos(AAngle), ABase.Y - ARadius * Sin(AAngle));
end;

function dxRingRelativePoint(const ABase, APoint: TdxPointF): TdxPointF;
begin
  Result := dxPointF(APoint.X - ABase.X, ABase.Y - APoint.Y);
end;

function dxRingRotatePoint(const ABase, APoint: TdxPointF; AAngle: Single): TdxPointF;
var
  A1, A2: Single;
begin
  A1 := dxRingAngle(ABase, APoint);
  A2 := A1 + AAngle;
  Result := dxRingPoint(ABase, cxPointDistanceF(ABase, APoint), A2);
end;

procedure dxRingRotatePoints(const ABase: TdxPointF; var APoints: array of TdxPointF; AAngle: Single);
var
  I: Integer;
begin
  for I := Low(APoints) to High(APoints) do
    APoints[I] := dxRingRotatePoint(ABase, APoints[I], AAngle);
end;

function dxNormalizeAngle(const AAngle: Double): Double;
begin
  Result := AAngle;
  while Result < 0 do
    Result := Result + 360;
  while Result >= 360 do
    Result := Result - 360;
end;

function dxSizeDouble(AWidth, AHeight: Double): TdxSizeDouble;
begin
  Result.Height := AHeight;
  Result.Width := AWidth;
end;

function dxSizeDoubleIsEqual(const ASize1, ASize2: TdxSizeDouble): Boolean;
begin
  Result := (ASize1.Height = ASize2.Height) and
    (ASize1.Width = ASize2.Width);
end;

function dxSizeDoubleScale(const ASize: TdxSizeDouble; const AScale: TdxPointDouble): TdxSizeDouble;
begin
  Result := dxSizeDouble(ASize.Width * AScale.X, ASize.Height * AScale.Y);
end;

function dxPointDouble(X, Y: Double): TdxPointDouble;
begin
  Result.X := X;
  Result.Y := Y;
end;

function dxPointDouble(const APoint: TPoint): TdxPointDouble;
begin
  Result := dxPointDouble(APoint.X, APoint.Y);
end;

function dxPointDouble(const APoint: TdxPointF): TdxPointDouble;
begin
  Result := dxPointDouble(APoint.X, APoint.Y);
end;

function dxPointDoubleIsEqual(const APoint1, APoint2: TdxPointDouble): Boolean;
begin
  Result := SameValue(APoint1.X, APoint2.X) and
    SameValue(APoint1.Y, APoint2.Y);
end;

function dxPointDoubleToPoint(const APoint: TdxPointDouble): TPoint;
begin
  Result.X := Round(APoint.X);
  Result.Y := Round(APoint.Y);
end;

function dxPointDoubleToPointF(const APoint: TdxPointDouble): TdxPointF;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
end;

function dxPointDoubleOffset(const APoint: TdxPointDouble; X, Y: Double;
  APositiveFactor: Boolean = True): TdxPointDouble;
begin
  if APositiveFactor then
  begin
    Result.X := APoint.X + X;
    Result.Y := APoint.Y + Y;
  end
  else
  begin
    Result.X := APoint.X - X;
    Result.Y := APoint.Y - Y;
  end;
end;

function dxPointDoubleOffset(const APoint: TdxPointDouble;
  const AOffset: TdxPointDouble; APositiveFactor: Boolean = True): TdxPointDouble;
begin
  Result := dxPointDoubleOffset(APoint, AOffset.X, AOffset.Y, APositiveFactor);
end;

function dxPointDoubleScale(const APoint: TdxPointDouble; const AScale: TdxPointDouble): TdxPointDouble;
begin
  Result.X := APoint.X * AScale.X;
  Result.Y := APoint.Y * AScale.Y;
end;

function dxRectDouble(ALeft, ATop, AWidth, AHeight: Double): TdxRectDouble;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

function dxRectDouble(const ATopLeft, ABottomRight: TdxPointDouble): TdxRectDouble;
begin
  Result.TopLeft := ATopLeft;
  Result.BottomRight := ABottomRight;
end;

function dxRectDouble(const ARect: TRect): TdxRectDouble;
begin
  Result.TopLeft := dxPointDouble(ARect.TopLeft);
  Result.BottomRight := dxPointDouble(ARect.BottomRight);
end;

function dxRectDoubleIntersect(const ARect1, ARect2: TdxRectDouble): Boolean;
var
  ALeft, ATop, ARight, ABottom: Double;
begin
  ALeft := Max(ARect2.Left, ARect1.Left);
  ATop := Max(ARect2.Top, ARect1.Top);
  ARight := Min(ARect2.Left + ARect2.Width, ARect1.Left + ARect1.Width);
  ABottom := Min(ARect2.Top + ARect2.Height, ARect1.Top + ARect1.Height);
  Result := not ((ARight <= ALeft) or (ABottom <= ATop));
end;

function dxRectDoubleIsEmpty(const ARect: TdxRectDouble): Boolean;
begin
  Result := (ARect.Width <= 0) or (ARect.Height <= 0);
end;

function dxRectDoubleIsEqual(const ARect1, ARect2: TdxRectDouble): Boolean;
begin
  Result := SameValue(ARect1.Left, ARect2.Left) and SameValue(ARect1.Top, ARect2.Top) and
    SameValue(ARect1.Width, ARect2.Width) and SameValue(ARect1.Height, ARect2.Height);
end;

function dxRectDoubleOffset(const ARect: TdxRectDouble; const APoint: TdxPointDouble;
  APositiveFactor: Boolean = True): TdxRectDouble;
begin
  if APositiveFactor then
    Result := dxRectDouble(ARect.Left + APoint.X, ARect.Top + APoint.Y,
      ARect.Width, ARect.Height)
  else
    Result := dxRectDouble(ARect.Left - APoint.X, ARect.Top - APoint.Y,
      ARect.Width, ARect.Height)
end;

function dxRectDoubleToRect(const AMapRect: TdxRectDouble): TRect;
begin
  Result.Left := Round(AMapRect.Left);
  Result.Top := Round(AMapRect.Top);
  Result.Right := Round(AMapRect.Left + AMapRect.Width);
  Result.Bottom := Round(AMapRect.Top + AMapRect.Height);
end;

function dxRectDoubleToRectF(const AMapRect: TdxRectDouble): TdxRectF;
begin
  Result.Left := AMapRect.Left;
  Result.Top := AMapRect.Top;
  Result.Right := AMapRect.Left + AMapRect.Width;
  Result.Bottom := AMapRect.Top + AMapRect.Height;
end;

procedure cxReduceFraction(var A, B: Integer);
var
  AIndex: Integer;
begin
  AIndex := 2;
  while (AIndex <= A) and (AIndex <= B) do
  begin
    if (A mod AIndex = 0) and (B mod AIndex = 0) then
    begin
      A := A div AIndex;
      B := B div AIndex;
      AIndex := 2;
    end
    else
      Inc(AIndex);
  end;
end;

{ TdxScaleFactorListenerComparer }

function TdxScaleFactorListenerComparer.Compare(
  const Left, Right: TdxScaleFactorNotifyEvent): Integer;
var
  AResult: NativeInt;
begin
  AResult := NativeInt(TMethod(Left).Data) - NativeInt(TMethod(Right).Data);
  if AResult = 0 then
    AResult := NativeInt(TMethod(Left).Code) - NativeInt(TMethod(Right).Code);
  Result := Sign(AResult);
end;

class function TdxScaleFactorListenerComparer.GetDefault: IComparer<TdxScaleFactorNotifyEvent>;
begin
  if FDefault = nil then
    FDefault := TdxScaleFactorListenerComparer.Create;
  Result := FDefault;
end;

{ TdxPointF }

class function TdxPointF.Create(const AX, AY: Single): TdxPointF;
begin
  Result.X := AX;
  Result.Y := AY;
end;

class operator TdxPointF.Equal(const P1, P2: TdxPointF): Boolean;
begin
  Result := SameValue(P1.X, P2.X) and SameValue(P1.Y, P2.Y);
end;

class operator TdxPointF.Implicit(const Value: TPoint): TdxPointF;
begin
  Result.X := Value.X;
  Result.Y := Value.Y;
end;

class operator TdxPointF.Implicit(const Value: TdxPointF): TPoint;
begin
  Result.X := Round(Value.X);
  Result.Y := Round(Value.Y);
end;

class operator TdxPointF.NotEqual(const P1, P2: TdxPointF): Boolean;
begin
  Result := not (P1 = P2);
end;

class function TdxPointF.Null: TdxPointF;
begin
  Result.X := 0;
  Result.Y := 0;
end;

procedure TdxPointF.Init(const AX, AY: Single);
begin
  X := AX;
  Y := AY;
end;

procedure TdxPointF.Offset(const DX, DY: Single; APositiveFactor: Boolean = True);
begin
  if APositiveFactor then
  begin
    X := X + DX;
    Y := Y + DY;
  end
  else
  begin
    X := X - DX;
    Y := Y - DY;
  end;
end;

procedure TdxPointF.Offset(const P: TdxPointF; APositiveFactor: Boolean = True);
begin
  if APositiveFactor then
  begin
    X := X + P.X;
    Y := Y + P.Y;
  end
  else
  begin
    X := X - P.X;
    Y := Y - P.Y;
  end;
end;

procedure TdxPointF.Offset(const S: TdxSizeF; APositiveFactor: Boolean = True);
begin
  if APositiveFactor then
  begin
    X := X + S.cx;
    Y := Y + S.cy;
  end
  else
  begin
    X := X - S.cx;
    Y := Y - S.cy;
  end;
end;

procedure TdxPointF.Scale(const DX, DY: Single);
begin
  X := X * DX;
  Y := Y * DY;
end;

procedure TdxPointF.Scale(const P: TdxPointF);
begin
  X := X * P.X;
  Y := Y * P.Y;
end;

procedure TdxPointF.Scale(const S: TdxSizeF);
begin
  X := X * S.cx;
  Y := Y * S.cy;
end;

{ TdxSizeF }

class function TdxSizeF.Create(const AWidth, AHeight: Single): TdxSizeF;
begin
  Result.cx := AWidth;
  Result.cy := AHeight;
end;

function TdxSizeF.Ceil: TdxSizeF;
begin
  Result := TdxSizeF.Create(Math.Ceil(cx), Math.Ceil(cy));
end;

procedure TdxSizeF.Init(const AWidth, AHeight: Single);
begin
  cx := AWidth;
  cy := AHeight;
end;

class function TdxSizeF.Null: TdxSizeF;
begin
  Result.cx := 0;
  Result.cy := 0;
end;

{ TdxRectF }

class function TdxRectF.Create(const ALeft, ATop, ARight, ABottom: Single): TdxRectF;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ARight;
  Result.Bottom := ABottom;
end;

class function TdxRectF.CreateSize(const ALeft, ATop, AWidth, AHeight: Single): TdxRectF;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ALeft + AWidth;
  Result.Bottom := ATop + AHeight;
end;

procedure TdxRectF.Init(const ALeft, ATop, ARight, ABottom: Single);
begin
  Left := ALeft;
  Top := ATop;
  Right := ARight;
  Bottom := ABottom;
end;

procedure TdxRectF.InitSize(const ALeft, ATop, AWidth, AHeight: Single);
begin
  Left := ALeft;
  Top := ATop;
  Right := ALeft + AWidth;
  Bottom := Top + AHeight;
end;

class operator TdxRectF.Implicit(const Value: TRect): TdxRectF;
begin
  Result.Left := Value.Left;
  Result.Top := Value.Top;
  Result.Right := Value.Right;
  Result.Bottom := Value.Bottom;
end;

class operator TdxRectF.Equal(const R1, R2: TdxRectF): Boolean;
begin
  Result := SameValue(R1.Left, R2.Left) and SameValue(R1.Top, R2.Top) and
    SameValue(R1.Right, R2.Right) and SameValue(R1.Bottom, R2.Bottom);
end;

class operator TdxRectF.NotEqual(const R1, R2: TdxRectF): Boolean;
begin
  Result := not (R1 = R2);
end;

procedure TdxRectF.Assign(const Value: TRect);
begin
  Left := Value.Left;
  Top := Value.Top;
  Right := Value.Right;
  Bottom := Value.Bottom;
end;

function TdxRectF.CenterPoint: TdxPointF;
begin
  Result.X := (Left + Right) / 2;
  Result.Y := (Top + Bottom) / 2;
end;

procedure TdxRectF.Empty;
begin
  Left := 0;
  Top := 0;
  Right := 0;
  Bottom := 0;
end;

procedure TdxRectF.Inflate(DX, DY: Single);
begin
  Left := Left - DX;
  Right := Right + DX;
  Top := Top - DY;
  Bottom := Bottom + DY;
end;

procedure TdxRectF.Inflate(const P: TdxPointF);
begin
  Inflate(P.X, P.Y);
end;

procedure TdxRectF.Inflate(const S: TdxSizeF);
begin
  Inflate(S.cx, S.cy);
end;

function TdxRectF.IsEmpty: Boolean;
begin
  Result := (Left >= Right) or (Top >= Bottom);
end;

function TdxRectF.IsZero : Boolean;
begin
  Result := Math.IsZero(Right) and Math.IsZero(Bottom) and Math.IsZero(Top) and Math.IsZero(Left);
end;

procedure TdxRectF.Intersect(const R: TdxRectF);
begin
  if R.Left > Left then Left := R.Left;
  if R.Top > Top then Top := R.Top;
  if R.Right < Right then Right := R.Right;
  if R.Bottom < Bottom then Bottom := R.Bottom;
  if (Right < Left) or (Bottom < Top) then
    Empty;
end;

procedure TdxRectF.Normalize;
var
  Temp: Single;
begin
  if Left > Right then
  begin
    Temp := Left;
    Left := Right;
    Right := Temp;
  end;
  if Top > Bottom then
  begin
    Temp := Top;
    Top := Bottom;
    Bottom := Temp;
  end;
end;

class function TdxRectF.Null: TdxRectF;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := 0;
  Result.Bottom := 0;
end;

procedure TdxRectF.Offset(X, Y: Single);
begin
  Left := Left + X;
  Right := Right + X;
  Top := Top + Y;
  Bottom := Bottom + Y;
end;

function TdxRectF.ZoomInto(const ASize: TdxSizeF): TdxRectF;
begin
  Result := ZoomInto(dxRectF(0, 0, ASize.cx, ASize.cy));
end;

function TdxRectF.ZoomInto(const ABounds: TdxRectF): TdxRectF;
var
  AInnerRatio, AOuterRatio: Single;
begin
  Result := Self;
  if (Result.Height <= 0) or (ABounds.Height <= 0) then
    Exit;
  AInnerRatio := Result.Width / Result.Height;
  AOuterRatio := ABounds.Width / ABounds.Height;
  if AInnerRatio < AOuterRatio then
  begin
    Result.Height := ABounds.Height;
    Result.Width := ABounds.Height * AInnerRatio;
    Result := cxRectOffset(Result, ABounds.Left + (ABounds.Width - Result.Width) / 2, ABounds.Top);
  end
  else
  begin
    Result.Width := ABounds.Width;
    Result.Height := ABounds.Width / AInnerRatio;
    Result := cxRectOffset(Result, ABounds.Left, ABounds.Top + (ABounds.Height - Result.Height) / 2);
  end;
end;

function TdxRectF.GetBottomLeft: TdxPointF;
begin
  Result := dxPointF(Left, Bottom);
end;

function TdxRectF.GetBottomRight: TdxPointF;
begin
  Result := dxPointF(Right, Bottom);
end;

function TdxRectF.GetHeight: Single;
begin
  Result := Bottom - Top;
end;

function TdxRectF.GetLocation: TdxPointF;
begin
  Result := TopLeft;
end;

function TdxRectF.GetSize: TdxSizeF;
begin
  Result := dxSizeF(Width, Height);
end;

function TdxRectF.GetTopLeft: TdxPointF;
begin
  Result := dxPointF(Left, Top);
end;

function TdxRectF.GetTopRight: TdxPointF;
begin
  Result := dxPointF(Right, Top);
end;

function TdxRectF.GetWidth: Single;
begin
  Result := Right - Left;
end;

procedure TdxRectF.SetBottomLeft(const P: TdxPointF);
begin
  Bottom := P.Y;
  Left := P.X;
end;

procedure TdxRectF.SetBottomRight(const P: TdxPointF);
begin
  Bottom := P.Y;
  Right := P.X;
end;

procedure TdxRectF.SetLocation(const P: TdxPointF);
begin
  Self := cxRectOffset(Self, P.X - Self.Left, P.Y - Self.Top);
end;

procedure TdxRectF.SetTopLeft(const P: TdxPointF);
begin
  Left := P.X;
  Top := P.Y;
end;

procedure TdxRectF.SetTopRight(const P: TdxPointF);
begin
  Right := P.X;
  Top := P.Y;
end;

procedure TdxRectF.SetHeight(const Value: Single);
begin
  Bottom := Top + Value;
end;

procedure TdxRectF.SetWidth(const Value: Single);
begin
  Right := Left + Value;
end;

{ TdxSizeDouble }

class function TdxSizeDouble.Create(const AWidth, AHeight: Double): TdxSizeDouble;
begin
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

class operator TdxSizeDouble.Equal(const R1, R2: TdxSizeDouble): Boolean;
begin
  Result := SameValue(R1.Width, R2.Width) and SameValue(R1.Height, R2.Height);
end;

class operator TdxSizeDouble.Implicit(const Value: TdxSizeDouble): TSize;
begin
  Result.cx := System.Round(Value.Width);
  Result.cy := System.Round(Value.Height);
end;

procedure TdxSizeDouble.Init(AWidth, AHeight: Double);
begin
  Width := AWidth;
  Height := AHeight;
end;

class operator TdxSizeDouble.NotEqual(const R1, R2: TdxSizeDouble): Boolean;
begin
  Result := not (R1 = R2);
end;

class function TdxSizeDouble.Null: TdxSizeDouble;
begin
  Result.Width := 0;
  Result.Height := 0;
end;

{ TdxPointDouble }

class function TdxPointDouble.Create(const AX, AY: Double): TdxPointDouble;
begin
  Result.X := AX;
  Result.Y := AY;
end;

procedure TdxPointDouble.Init(const AX, AY: Double);
begin
  X := AX;
  Y := AY;
end;

class operator TdxPointDouble.Equal(const R1, R2: TdxPointDouble): Boolean;
begin
  Result := SameValue(R1.X, R2.X) and SameValue(R1.Y, R2.Y);
end;

class operator TdxPointDouble.Implicit(const Value: TdxPointDouble): TPoint;
begin
  Result.X := System.Round(Value.X);
  Result.Y := System.Round(Value.Y);
end;

class operator TdxPointDouble.Implicit(const Value: TdxPointDouble): TdxPointF;
begin
  Result.X := Value.X;
  Result.Y := Value.Y;
end;

class operator TdxPointDouble.NotEqual(const R1, R2: TdxPointDouble): Boolean;
begin
  Result := not (R1 = R2);
end;

class function TdxPointDouble.Null: TdxPointDouble;
begin
  Result.X := 0;
  Result.Y := 0;
end;

procedure TdxPointDouble.Offset(const DX, DY: Double);
begin
  X := X + DX;
  Y := Y + DY;
end;

procedure TdxPointDouble.Offset(const P: TdxPointDouble);
begin
  X := X + P.X;
  Y := Y + P.Y;
end;

procedure TdxPointDouble.Offset(const S: TdxSizeDouble);
begin
  X := X + S.Width;
  Y := Y + S.Height;
end;

procedure TdxPointDouble.Scale(const DX, DY: Double);
begin
  X := X * DX;
  Y := Y * DY;
end;

procedure TdxPointDouble.Scale(const P: TdxPointDouble);
begin
  X := X * P.X;
  Y := Y * P.Y;
end;

procedure TdxPointDouble.Scale(const S: TdxSizeDouble);
begin
  X := X * S.Width;
  Y := Y * S.Height;
end;

{ TdxRectDouble}

class function TdxRectDouble.CreateFromLTRB(const ALeft, ATop, ARight, ABottom: Double): TdxRectDouble;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Width := ARight - ALeft;
  Result.Height := ABottom - ATop;
end;

class function TdxRectDouble.Create(const ALeft, ATop: Double; const ASize: TdxSizeDouble): TdxRectDouble;
begin
  Result := Create(0, 0, ASize.Width, ASize.Height);
end;

class function TdxRectDouble.Create(const ASize: TdxSizeDouble): TdxRectDouble;
begin
  Result := Create(0, 0, ASize);
end;

class function TdxRectDouble.Create(const ALeft, ATop, AWidth, AHeight: Double): TdxRectDouble;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

class function TdxRectDouble.Create(const AOrigin: TdxPointDouble): TdxRectDouble;
begin
  Result.TopLeft := AOrigin;
  Result.BottomRight := AOrigin;
end;

class function TdxRectDouble.Create(const AOrigin: TdxPointDouble; AWidth, AHeight: Double): TdxRectDouble;
begin
  Result.TopLeft := AOrigin;
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

function TdxRectDouble.Contains(const P: TdxPointDouble): Boolean;
begin
  Result := (P.X >= Left) and (P.X < Right) and (P.Y >= Top) and (P.Y < Bottom);
end;

class function TdxRectDouble.Create(const AOrigin: TdxPointDouble; const ASize: TdxSizeDouble): TdxRectDouble;
begin
  Result := Create(AOrigin, ASize.Width, ASize.Width);
end;

class function TdxRectDouble.Create(const ATopLeft, ABottomRight: TdxPointDouble): TdxRectDouble;
begin
  Result.TopLeft := ATopLeft;
  Result.BottomRight := ABottomRight;
end;

procedure TdxRectDouble.Inflate(const AValue: Double);
begin
  Left := Left - AValue;
  Top := Top - AValue;
  Width := Width + AValue * 2;
  Height := Height + AValue * 2;
end;

procedure TdxRectDouble.Union(const R: TdxRectDouble);
var
  ATemp: Double;
begin
  ATemp := Max(R.Right, Right);
  Left := Min(R.Left, Left);
  Right := ATemp;
  ATemp := Max(R.Bottom, Bottom);
  Top := Min(R.Top, Top);
  Bottom := ATemp;
end;

function TdxRectDouble.GetBottom: Double;
begin
  Result := Top + Height;
end;

function TdxRectDouble.GetBottomRight: TdxPointDouble;
begin
  Result := dxPointDouble(Right, Bottom);
end;

function TdxRectDouble.GetRight: Double;
begin
  Result := Left + Width;
end;

function TdxRectDouble.GetSize: TdxSizeDouble;
begin
  Result := dxSizeDouble(Width, Height);
end;

function TdxRectDouble.GetTopLeft: TdxPointDouble;
begin
  Result := dxPointDouble(Left, Top);
end;

function TdxRectDouble.IsEmpty: Boolean;
begin
  Result := dxRectDoubleIsEmpty(Self);
end;

class function TdxRectDouble.Null: TdxRectDouble;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Width := 0;
  Result.Height := 0;
end;

procedure TdxRectDouble.SetBottom(const AValue: Double);
begin
  Height := AValue - Top;
end;

procedure TdxRectDouble.SetBottomRight(const ABottomRight: TdxPointDouble);
begin
  Width := ABottomRight.X - Left;
  Height := ABottomRight.Y - Top;
end;

procedure TdxRectDouble.SetRight(const AValue: Double);
begin
  Width := AValue - Left;
end;

procedure TdxRectDouble.SetTopLeft(const ATopLeft: TdxPointDouble);
begin
  Left := ATopLeft.X;
  Top := ATopLeft.Y;
end;

{ TcxGeometryObject }

procedure TcxGeometryObject.DoChange;
begin
  dxCallNotify(OnChange, Self);
end;

procedure TcxGeometryObject.ChangeScale(M, D: Integer);
begin
  // do nothing
end;

procedure TcxGeometryObject.LoadFromStream(AStream: TStream);
begin
  // do nothing
end;

procedure TcxGeometryObject.SaveToStream(AStream: TStream);
begin
  // do nothing
end;

{ TcxRect }

procedure TcxRect.ChangeScale(M, D: Integer);
begin
  inherited;
  Rect := cxRectScale(Rect, M, D);
end;

procedure TcxRect.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TcxRect then
    Rect := TcxRect(Source).Rect;
end;

function TcxRect.IsEmpty: Boolean;
begin
  Result := cxRectIsEmpty(FRect);
end;

function TcxRect.IsEqual(const ARect: TRect): Boolean;
begin
  Result := cxRectIsEqual(ARect, FRect);
end;

function TcxRect.IsEqual(ARect: TcxRect): Boolean;
begin
  Result := IsEqual(ARect.Rect);
end;

procedure TcxRect.LoadFromStream(AStream: TStream);
var
  R: TRect;
begin
  AStream.ReadBuffer(R, SizeOf(TRect));
  Rect := R;
end;

procedure TcxRect.SaveToStream(AStream: TStream);
begin
  AStream.WriteBuffer(FRect, SizeOf(TRect));
end;

procedure TcxRect.ValidateRect(var R: TRect);
begin
end;

function TcxRect.GetHeight: Integer;
begin
  Result := FRect.Bottom - FRect.Top;
end;

function TcxRect.GetValue(AIndex: Integer): Integer;
begin
  case AIndex of
    0:
      Result := FRect.Left;
    1:
      Result := FRect.Top;
    2:
      Result := FRect.Right;
  else
    Result := FRect.Bottom;
  end;
end;

function TcxRect.GetVertex(AIndex: Integer): TPoint;
begin
  case AIndex of
    0:
      Result := FRect.TopLeft;
    1:
      Result := Point(FRect.Right, FRect.Top);
    2:
      Result := Point(FRect.Left, FRect.Bottom);
  else
    Result := FRect.BottomRight;
  end;
end;

function TcxRect.GetWidth: Integer;
begin
  Result := FRect.Right - FRect.Left;
end;

procedure TcxRect.SetHeight(AValue: Integer);
begin
  Bottom := Top + AValue;
end;

procedure TcxRect.SetRect(AValue: TRect);
begin
  ValidateRect(AValue);
  if not IsEqual(AValue) then
  begin
    FRect := AValue;
    DoChange;
  end;
end;

procedure TcxRect.SetValue(AIndex, AValue: Integer);
var
  ARect: TRect;
begin
  ARect := Rect;
  case AIndex of
    0:
      ARect.Left := AValue;
    1:
      ARect.Top := AValue;
    2:
      ARect.Right := AValue;
    3:
      ARect.Bottom := AValue;
  end;
  Rect := ARect;
end;

procedure TcxRect.SetVertex(AIndex: Integer; const AValue: TPoint);
var
  ARect: TRect;
begin
  ARect := Rect;
  case AIndex of
    0:
      ARect.TopLeft := AValue;
    1:
    begin
      ARect.Right := AValue.X;
      ARect.Top := AValue.Y;
    end;
    2:
    begin
      ARect.Left := AValue.X;
      ARect.Bottom := AValue.Y;
    end;
    3:
      ARect.BottomRight := AValue;
  end;
  Rect := ARect;
end;

procedure TcxRect.SetWidth(AValue: Integer);
begin
  Right := Left + AValue;
end;

{ TcxSize }

constructor TcxSize.Create(AOwner: TPersistent; ADefaultWidth,
  ADefaultHeight: Integer);
begin
  inherited Create(AOwner);
  FDefaultSize := cxSize(ADefaultWidth, ADefaultHeight);
  FSize := FDefaultSize;
end;

function TcxSize.IsEmpty: Boolean;
begin
  Result := cxSizeIsEmpty(FSize);
end;

function TcxSize.IsEqual(const ASize: TSize): Boolean;
begin
  Result := cxSizeIsEqual(ASize, FSize);
end;

function TcxSize.IsEqual(ASize: TcxSize): Boolean;
begin
  Result := IsEqual(ASize.Size);
end;

procedure TcxSize.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TcxSize then
    Size := TcxSize(Source).Size;
end;

function TcxSize.IsSizeStored(Index: Integer): Boolean;
begin
  if Index = 0 then
    Result := FSize.cy <> FDefaultSize.cy
  else
    Result := FSize.cx <> FDefaultSize.cx;
end;

procedure TcxSize.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  Size := cxSizeScale(Size, M, D);
end;

procedure TcxSize.LoadFromStream(AStream: TStream);
var
  ASize: TSize;
begin
  AStream.ReadBuffer(ASize, SizeOf(TSize));
  Size := ASize;
end;

procedure TcxSize.SaveToStream(AStream: TStream);
begin
  AStream.WriteBuffer(FSize, SizeOf(TSize));
end;

function TcxSize.GetSize: TSize;
begin
  Result := cxSize(Width, Height);
end;

function TcxSize.GetValue(Index: Integer): Integer;
begin
  if Index = 0 then
    Result := FSize.cy
  else
    Result := FSize.cx;
end;

procedure TcxSize.SetValue(Index, Value: Integer);
var
  ASize: TSize;
begin
  ASize := Size;
  if Index = 0 then
    ASize.cy := Value
  else
    ASize.cx := Value;
  Size := ASize;
end;

procedure TcxSize.SetSize(const Value: TSize);
var
  ASize: TSize;
begin
  ASize.cx := Max(0, Value.cx);
  ASize.cy := Max(0, Value.cy);
  if not IsEqual(ASize) then
  begin
    FSize := ASize;
    DoChange;
  end;
end;

{ TcxMargin }

constructor TcxMargin.Create(AOwner: TPersistent; ADefaultValue: Integer);
begin
  inherited Create(AOwner);
  FDefaultValue := ADefaultValue;
  All := ADefaultValue;
end;

procedure TcxMargin.ChangeScale(M, D: Integer);
begin
  inherited;
  FMargin := cxRectScale(FMargin, M, D);
end;

procedure TcxMargin.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TcxMargin then
    Margin := TcxMargin(Source).Margin;
end;

procedure TcxMargin.LoadFromStream(AStream: TStream);
var
  R: TRect;
begin
  AStream.ReadBuffer(R, SizeOf(TRect));
  Margin := R;
end;

procedure TcxMargin.SaveToStream(AStream: TStream);
begin
  AStream.WriteBuffer(FMargin, SizeOf(TRect));
end;

function TcxMargin.GetValue(AIndex: Integer): Integer;
begin
  case AIndex of
    1: Result := FMargin.Left;
    2: Result := FMargin.Top;
    3: Result := FMargin.Right;
    4: Result := FMargin.Bottom;
  else // 0
    if (FMargin.Left = FMargin.Top) and (FMargin.Left = FMargin.Right) and (FMargin.Left = FMargin.Bottom) then
      Result := FMargin.Left
    else
      Result := -1;
  end;
end;

procedure TcxMargin.SetMargin(const Value: TRect);
begin
  if not cxRectIsEqual(Value, FMargin) then
  begin
    FMargin := Value;
    DoChange;
  end;
end;

procedure TcxMargin.SetValue(AIndex, AValue: Integer);
begin
  if GetValue(AIndex) <> AValue then
  begin
    case AIndex of
      1: FMargin.Left := AValue;
      2: FMargin.Top := AValue;
      3: FMargin.Right := AValue;
      4: FMargin.Bottom := AValue;
    else // 0
      FMargin := cxRect(AValue, AValue, AValue, AValue);
    end;

    DoChange;
  end;
end;

function TcxMargin.IsEqual(AMargin: TcxMargin): Boolean;
begin
  Result := cxRectIsEqual(Margin, AMargin.Margin);
end;

function TcxMargin.IsMarginStored(AIndex: Integer): Boolean;
begin
  case AIndex of
    1..4: Result := All = -1;
  else // 0
    Result := All <> FDefaultValue;
  end;
end;

{ TdxPoint }

constructor TdxPoint.Create(AOwner: TPersistent; ADefaultX, ADefaultY: Integer);
begin
  inherited Create(AOwner);
  FDefaultPoint := cxPoint(ADefaultX, ADefaultY);
  FPoint := FDefaultPoint;
end;

procedure TdxPoint.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  Point := cxPointScale(Point, M, D);
end;

procedure TdxPoint.LoadFromStream(AStream: TStream);
var
  APoint: TPoint;
begin
  AStream.ReadBuffer(APoint, SizeOf(TPoint));
  Point := APoint;
end;

procedure TdxPoint.SaveToStream(AStream: TStream);
begin
  AStream.WriteBuffer(FPoint, SizeOf(TPoint));
end;

function TdxPoint.IsEqual(const APoint: TPoint): Boolean;
begin
  Result := cxPointIsEqual(APoint, FPoint);
end;

function TdxPoint.IsEqual(APoint: TdxPoint): Boolean;
begin
  Result := IsEqual(APoint.Point);
end;

procedure TdxPoint.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxPoint then
    Point := TdxPoint(Source).Point;
end;

function TdxPoint.IsPointStored(Index: Integer): Boolean;
begin
  if Index = 0 then
    Result := FPoint.X <> FDefaultPoint.X
  else
    Result := FPoint.Y <> FDefaultPoint.Y;
end;

function TdxPoint.GetPoint: TPoint;
begin
  Result := cxPoint(X, Y);
end;

function TdxPoint.GetValue(Index: Integer): Integer;
begin
  if Index = 0 then
    Result := FPoint.X
  else
    Result := FPoint.Y;
end;

procedure TdxPoint.SetPoint(const Value: TPoint);
begin
  if not IsEqual(Value) then
  begin
    FPoint := Value;
    DoChange;
  end;
end;

procedure TdxPoint.SetValue(Index, Value: Integer);
var
  APoint: TPoint;
begin
  APoint := Point;
  if Index = 0 then
    APoint.X := Value
  else
    APoint.Y := Value;
  Point := APoint;
end;

{ TdxPointDoublePersistent }

constructor TdxPointDoublePersistent.Create(AOwner: TPersistent; const ADefaultX: Double = 0; const ADefaultY: Double = 0);
begin
  inherited Create(AOwner);
  FDefaultPoint := dxPointDouble(ADefaultX, ADefaultY);
  FPoint := FDefaultPoint;
end;

function TdxPointDoublePersistent.IsEqual(const APoint: TdxPointDouble): Boolean;
begin
  Result := dxPointDoubleIsEqual(FPoint, APoint);
end;

procedure TdxPointDoublePersistent.LoadFromStream(AStream: TStream);
var
  APoint: TdxPointDouble;
begin
  AStream.ReadBuffer(APoint, SizeOf(FPoint));
  FPoint := APoint;
end;

procedure TdxPointDoublePersistent.SaveToStream(AStream: TStream);
begin
  AStream.WriteBuffer(FPoint, SizeOf(FPoint));
end;

procedure TdxPointDoublePersistent.Reset;
begin
  Point := FDefaultPoint;
end;

procedure TdxPointDoublePersistent.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxPointDoublePersistent then
    Point := TdxPointDoublePersistent(Source).Point;
end;

function TdxPointDoublePersistent.GetValue(AIndex: Integer): Double;
begin
  if AIndex = 0 then
    Result := FPoint.X
  else
    Result := FPoint.Y;
end;

function TdxPointDoublePersistent.IsPointStored(AIndex: Integer): Boolean;
begin
  if AIndex = 0 then
    Result := not SameValue(FPoint.X, FDefaultPoint.X)
  else
    Result := not SameValue(FPoint.Y, FDefaultPoint.Y);
end;

procedure TdxPointDoublePersistent.SetPoint(const AValue: TdxPointDouble);
begin
  if not dxPointDoubleIsEqual(AValue, FPoint) then
  begin
    FPoint := AValue;
    DoChange;
  end;
end;

procedure TdxPointDoublePersistent.SetValue(AIndex: Integer; AValue: Double);
begin
  if GetValue(AIndex) <> AValue then
  begin
    if AIndex = 0 then
      FPoint.X := AValue
    else
      FPoint.Y := AValue;

    DoChange;
  end;
end;

{ TdxMatrix }

constructor TdxMatrix.Create;
begin
  CreateEx(1, 0, 0, 1, 0, 0);
end;

constructor TdxMatrix.CreateEx(M11, M12, M21, M22, DX, DY: Single);
begin
  inherited Create;
  Assign(M11, M12, M21, M22, DX, DY);
end;

constructor TdxMatrix.CreateEx(M: TdxMatrix);
begin
  CreateEx(M.XForm.eM11, M.XForm.eM12, M.XForm.eM21, M.XForm.eM22, M.XForm.eDx, M.XForm.eDy);
end;

procedure TdxMatrix.Assign(M: TdxMatrix);
begin
  FMatrix := M.XForm;
end;

procedure TdxMatrix.Assign(M11, M12, M21, M22, DX, DY: Single);
begin
  FMatrix := TXForm.CreateMatrix(M11, M12, M21, M22, DX, DY);
end;

function TdxMatrix.IsIdentity: Boolean;
begin
  Result := TXForm.IsIdentity(XForm);
end;

function TdxMatrix.Transform(const APoint: TdxPointF): TdxPointF;
begin
  Result := dxPointF(
    APoint.X * XForm.eM11 + APoint.Y * XForm.eM21 + XForm.eDx,
    APoint.X * XForm.eM12 + APoint.Y * XForm.eM22 + XForm.eDy);
end;

function TdxMatrix.Transform(const ARect: TdxRectF): TdxRectF;
begin
  Result.TopLeft := Transform(ARect.TopLeft);
  Result.BottomRight := Transform(ARect.BottomRight);
end;

procedure TdxMatrix.Multiply(M: TdxMatrix; AOrder: TdxTransformationOrder = moPrepend);
begin
  DoMultiply(M.XForm, AOrder);
end;

procedure TdxMatrix.Reset;
begin
  Assign(1, 0, 0, 1, 0, 0);
end;

procedure TdxMatrix.Rotate(AAngle: Single; AOrder: TdxTransformationOrder = moPrepend);
begin
  DoMultiply(TXForm.CreateRotationMatrix(AAngle), AOrder);
end;

procedure TdxMatrix.Rotate(AAngle: Single; const APivotPoint: TPoint; AOrder: TdxTransformationOrder = moPrepend);
begin
  Rotate(AAngle, dxPointF(APivotPoint), AOrder);
end;

procedure TdxMatrix.Rotate(AAngle: Single; const APivotPoint: TdxPointF; AOrder: TdxTransformationOrder = moPrepend);
const
  TranslateDirectionMap: array[TdxTransformationOrder] of Integer = (1, -1);
begin
  Translate(TranslateDirectionMap[AOrder] * APivotPoint.X, TranslateDirectionMap[AOrder] * APivotPoint.Y, AOrder);
  Rotate(AAngle, AOrder);
  Translate(-TranslateDirectionMap[AOrder] * APivotPoint.X, -TranslateDirectionMap[AOrder] * APivotPoint.Y, AOrder);
end;

procedure TdxMatrix.Scale(AScaleX, AScaleY: Single; AOrder: TdxTransformationOrder = moPrepend);
begin
  DoMultiply(TXForm.CreateScaleMatrix(AScaleX, AScaleY), AOrder);
end;

procedure TdxMatrix.Scale(const AScale: TdxPointF; AOrder: TdxTransformationOrder = moPrepend);
begin
  Scale(AScale.X, AScale.Y, AOrder);
end;

procedure TdxMatrix.Translate(DX, DY: Single; AOrder: TdxTransformationOrder = moPrepend);
begin
  DoMultiply(TXForm.CreateTranslateMatrix(DX, DY), AOrder);
end;

procedure TdxMatrix.Translate(const AOffset: TdxPointF; AOrder: TdxTransformationOrder = moPrepend);
begin
  Translate(AOffset.X, AOffset.Y, AOrder);
end;

procedure TdxMatrix.DoMultiply(const AXForm: TXForm; AOrder: TdxTransformationOrder = moPrepend);
begin
  if AOrder = moPrepend then
    FMatrix := TXForm.Combine(AXForm, FMatrix)
  else
    FMatrix := TXForm.Combine(FMatrix, AXForm);
end;

{ TdxScaleFactor }

constructor TdxScaleFactor.Create;
begin
  FDenominator := 1;
  FNumerator := 1;
end;

destructor TdxScaleFactor.Destroy;
begin
  FreeAndNil(FListeners);
  inherited Destroy;
end;

procedure TdxScaleFactor.Assign(ANumerator, ADenominator: Integer; AIsLoading: Boolean = False);
var
  M, D: Integer;
begin
  cxReduceFraction(ANumerator, ADenominator);
  if (ADenominator <> FDenominator) or (ANumerator <> FNumerator) then
  begin
    M := ANumerator * FDenominator;
    D := ADenominator * FNumerator;
    FDenominator := ADenominator;
    FNumerator := ANumerator;
    Changed(M, D, AIsLoading);
  end;
end;

procedure TdxScaleFactor.Assign(ASource: TdxScaleFactor);
begin
  Assign(ASource.Numerator, ASource.Denominator);
end;

procedure TdxScaleFactor.Change(ANumerator, ADenominator: Integer);
begin
  ANumerator := FNumerator * ANumerator;
  ADenominator := FDenominator * ADenominator;
  Assign(ANumerator, ADenominator);
end;

function TdxScaleFactor.Equals(Obj: TObject): Boolean;
begin
  Result := TdxScaleFactor(Obj).Denominator * Numerator = TdxScaleFactor(Obj).Numerator * Denominator;
end;

function TdxScaleFactor.Apply(const V: TPoint): TPoint;
begin
  Result.X := Apply(V.X);
  Result.Y := Apply(V.Y);
end;

function TdxScaleFactor.Apply(const V: Integer): Integer;
begin
  if Assigned then
    Result := MulDiv(V, Numerator, Denominator)
  else
    Result := V;
end;

function TdxScaleFactor.Apply(const V: TSize): TSize;
begin
  if Assigned then
    Result := cxSizeScale(V, Numerator, Denominator)
  else
    Result := V;
end;

function TdxScaleFactor.ApplyF(const V: Double): Double;
begin
  if Assigned then
    Result := V * Numerator / Denominator
  else
    Result := V;
end;

function TdxScaleFactor.Apply(const V: TRect): TRect;
begin
  if Assigned then
    Result := cxRectScale(V, Numerator, Denominator)
  else
    Result := V;
end;

function TdxScaleFactor.Apply(const V: TPoint; ASourceScaleFactor: TdxScaleFactor): TPoint;
begin
  Result := Apply(V, ASourceScaleFactor.Numerator, ASourceScaleFactor.Denominator);
end;

function TdxScaleFactor.Apply(const V: Integer; ASourceScaleFactor: TdxScaleFactor): Integer;
begin
  Result := Apply(V, ASourceScaleFactor.Numerator, ASourceScaleFactor.Denominator);
end;

function TdxScaleFactor.Apply(const V: TSize; ASourceScaleFactor: TdxScaleFactor): TSize;
begin
  Result := Apply(V, ASourceScaleFactor.Numerator, ASourceScaleFactor.Denominator);
end;

function TdxScaleFactor.Apply(const V: TRect; ASourceScaleFactor: TdxScaleFactor): TRect;
begin
  Result := Apply(V, ASourceScaleFactor.Numerator, ASourceScaleFactor.Denominator);
end;

function TdxScaleFactor.Apply(const V: TPoint; ASourceNumerator, ASourceDenominator: Integer): TPoint;
begin
  if TransformScaleFactor(ASourceNumerator, ASourceDenominator) then
    Result := cxPointScale(V, ASourceNumerator, ASourceDenominator)
  else
    Result := V;
end;

function TdxScaleFactor.Apply(const V: Integer; ASourceNumerator, ASourceDenominator: Integer): Integer;
begin
  if TransformScaleFactor(ASourceNumerator, ASourceDenominator) then
    Result := MulDiv(V, ASourceNumerator, ASourceDenominator)
  else
    Result := V;
end;

function TdxScaleFactor.Apply(const V: TSize; ASourceNumerator, ASourceDenominator: Integer): TSize;
begin
  if TransformScaleFactor(ASourceNumerator, ASourceDenominator) then
    Result := cxSizeScale(V, ASourceNumerator, ASourceDenominator)
  else
    Result := V;
end;

function TdxScaleFactor.Apply(const V: TRect; ASourceNumerator, ASourceDenominator: Integer): TRect;
begin
  if TransformScaleFactor(ASourceNumerator, ASourceDenominator) then
    Result := cxRectScale(V, ASourceNumerator, ASourceDenominator)
  else
    Result := V;
end;

function TdxScaleFactor.Revert(const V: TPoint): TPoint;
begin
  Result.X := Revert(V.X);
  Result.Y := Revert(V.Y);
end;

function TdxScaleFactor.Revert(const V: Integer): Integer;
begin
  if Assigned then
    Result := MulDiv(V, Denominator, Numerator)
  else
    Result := V;
end;

function TdxScaleFactor.Revert(const V: TSize): TSize;
begin
  if Assigned then
    Result := cxSizeScale(V, Denominator, Numerator)
  else
    Result := V;
end;

function TdxScaleFactor.RevertF(const V: Double): Double;
begin
  if Assigned then
    Result := V * Denominator / Numerator
  else
    Result := V;
end;

function TdxScaleFactor.Revert(const V: TRect): TRect;
begin
  if Assigned then
    Result := cxRectScale(V, Denominator, Numerator)
  else
    Result := V;
end;

procedure TdxScaleFactor.ListenerAdd(AEventHandler: TdxScaleFactorNotifyEvent);
begin
  if FListeners = nil then
    FListeners := TList<TdxScaleFactorNotifyEvent>.Create(TdxScaleFactorListenerComparer.Default);
  if FListeners.Contains(AEventHandler) then
    raise EdxException.Create('Listener is already registered');
  FListeners.Add(AEventHandler);
end;

procedure TdxScaleFactor.ListenerRemove(AEventHandler: TdxScaleFactorNotifyEvent);
begin
  if (Self <> nil) and (FListeners <> nil) then
  begin
    FListeners.Remove(AEventHandler);
    if FListeners.Count = 0 then
      FreeAndNil(FListeners);
  end;
end;

procedure TdxScaleFactor.Changed(M, D: Integer; IsLoading: Boolean);
var
  I: Integer;
begin
  if (M <> D) and (FListeners <> nil) then
  begin
    for I := 0 to FListeners.Count - 1 do
      FListeners[I](Self, M, D, IsLoading);
  end;
end;

function TdxScaleFactor.GetAssigned: Boolean;
begin
  Result := Numerator <> Denominator;
end;

function TdxScaleFactor.TransformScaleFactor(var ASourceNumerator, ASourceDenominator: Integer): Boolean;
var
  M, D: Integer;
begin
  M := Numerator * ASourceDenominator;
  D := Denominator * ASourceNumerator;
  Result := M <> D;
  if Result then
  begin
    cxReduceFraction(M, D);
    ASourceDenominator := D;
    ASourceNumerator := M;
  end;
end;

{ TdxOwnedScaleFactor }

procedure TdxOwnedScaleFactor.AfterConstruction;
begin
  inherited;
  FUseOwnerValue := True;
end;

procedure TdxOwnedScaleFactor.BeforeDestruction;
begin
  inherited BeforeDestruction;
  Owner := nil;
end;

procedure TdxOwnedScaleFactor.Changed(M, D: Integer; IsLoading: Boolean);
begin
  if not IsLoading then
    FUseOwnerValue := False;
  inherited;
end;

procedure TdxOwnedScaleFactor.OwnerChangeHandler(Sender: TObject; M, D: Integer; IsLoading: Boolean);
begin
  SynchronizeValue(IsLoading);
end;

procedure TdxOwnedScaleFactor.SetOwner(const Value: TdxScaleFactor);
begin
  if (FOwner <> Value) and (Value <> Self) then
  begin
    if FOwner <> nil then
    begin
      FOwner.ListenerRemove(OwnerChangeHandler);
      FOwner := nil;
    end;
    if Value <> nil then
    begin
      FOwner := Value;
      FOwner.ListenerAdd(OwnerChangeHandler);
    end;
    SynchronizeValue;
  end;
end;

procedure TdxOwnedScaleFactor.SetUseOwnerValue(const Value: Boolean);
begin
  if FUseOwnerValue <> Value then
  begin
    FUseOwnerValue := Value;
    SynchronizeValue;
  end;
end;

procedure TdxOwnedScaleFactor.SynchronizeValue(AIsLoading: Boolean);
begin
  if UseOwnerValue and (Owner <> nil) then
  begin
    Assign(Owner.Numerator, Owner.Denominator, AIsLoading);
    FUseOwnerValue := True;
  end;
end;

{ TdxXFormHelper }

class function TdxXFormHelper.Combine(const AMatrix1, AMatrix2: TXForm): TXForm;
begin
  CombineTransform(Result, AMatrix1, AMatrix2);
end;

class function TdxXFormHelper.CreateFlip(
  AFlipHorizontally, AFlipVertically: Boolean; const APivotPointX, APivotPointY: Single): TXForm;
const
  FlipValueMap: array[Boolean] of Integer = (1, -1);
begin
  Result := CreateMatrix(
    FlipValueMap[AFlipHorizontally], 0, 0, FlipValueMap[AFlipVertically],
    IfThen(AFlipHorizontally, 2 * APivotPointX),
    IfThen(AFlipVertically, 2 * APivotPointY));
end;

class function TdxXFormHelper.CreateIdentityMatrix: TXForm;
begin
  Result := CreateMatrix(1, 0, 0, 1, 0, 0);
end;

class function TdxXFormHelper.CreateMatrix(M11, M12, M21, M22, DX, DY: Single): TXForm;
begin
  Result.eM11 := M11;
  Result.eM12 := M12;
  Result.eM21 := M21;
  Result.eM22 := M22;
  Result.eDx := DX;
  Result.eDy := DY;
end;

class function TdxXFormHelper.CreateRotationMatrix(AAngle: Single): TXForm;
begin
  AAngle := DegToRad(AAngle);
  Result := CreateMatrix(Cos(AAngle), Sin(AAngle), -Sin(AAngle), Cos(AAngle), 0, 0);
end;

class function TdxXFormHelper.CreateScaleMatrix(AScale: Single): TXForm;
begin
  Result := CreateScaleMatrix(AScale, AScale);
end;

class function TdxXFormHelper.CreateScaleMatrix(AScaleX, AScaleY: Single): TXForm;
begin
  Result := CreateMatrix(AScaleX, 0, 0, AScaleY, 0, 0);
end;

class function TdxXFormHelper.CreateTranslateMatrix(AOffsetX, AOffsetY: Single): TXForm;
begin
  Result := CreateMatrix(1, 0, 0, 1, AOffsetX, AOffsetY);
end;

class function TdxXFormHelper.IsEqual(const AMatrix1, AMatrix2: TXForm): Boolean;
begin
  Result :=
    SameValue(AMatrix1.eM11, AMatrix2.eM11) and
    SameValue(AMatrix1.eM12, AMatrix2.eM12) and
    SameValue(AMatrix1.eM21, AMatrix2.eM21) and
    SameValue(AMatrix1.eM22, AMatrix2.eM22) and
    SameValue(AMatrix1.eDx, AMatrix2.eDx) and
    SameValue(AMatrix1.eDy, AMatrix2.eDy);
end;

class function TdxXFormHelper.IsIdentity(const AMatrix: TXForm): Boolean;
begin
  Result :=
    (AMatrix.eM11 = 1) and (AMatrix.eM12 = 0) and
    (AMatrix.eM21 = 0) and (AMatrix.eM22 = 1) and
    (AMatrix.eDx = 0) and (AMatrix.eDy = 0);
end;

initialization
  RegisterClasses([TcxRect]);

finalization
  UnregisterClasses([TcxRect]);
end.
