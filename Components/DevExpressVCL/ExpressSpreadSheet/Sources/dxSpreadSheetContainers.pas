{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetContainers;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, Graphics, Controls, cxVariants, dxGDIPlusClasses, cxGeometry, cxLibraryConsts, dxCore,
  dxSpreadSheetCore, dxSpreadSheetClasses, dxSpreadSheetTypes, dxSpreadSheetGraphics, dxSpreadSheetUtils,
  dxCoreClasses, dxCoreGraphics, cxDrawTextUtils, cxGraphics, dxSpreadSheetCoreStyles;

type
  TdxSpreadSheetCommentContainerCalculator = class;
  TdxSpreadSheetCommentContainerViewInfo = class;
  TdxSpreadSheetCustomTextBoxContainerCalculator = class;

  { TdxSpreadSheetShape }

  TdxSpreadSheetShapeType = (stRect, stRoundRect, stEllipse);

  TdxSpreadSheetShape = class(TdxSpreadSheetCustomDrawingObject)
  strict private
    FBrush: TdxGPBrush;
    FPen: TdxGPPen;
    FShapeType: TdxSpreadSheetShapeType;

    procedure ChangeHandler(Sender: TObject);

    procedure SetBrush(const AValue: TdxGPBrush);
    procedure SetPen(const AValue: TdxGPPen);
    procedure SetShapeType(const AValue: TdxSpreadSheetShapeType);
  protected
    function IsEmpty: Boolean;
  public
    constructor Create(ASpreadSheet: TdxCustomSpreadSheet); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Brush: TdxGPBrush read FBrush write SetBrush;
    property Pen: TdxGPPen read FPen write SetPen;
    property ShapeType: TdxSpreadSheetShapeType read FShapeType write SetShapeType;
  end;

  { TdxSpreadSheetShapeContainer }

  TdxSpreadSheetShapeContainer = class(TdxSpreadSheetContainer)
  strict private
    FShape: TdxSpreadSheetShape;

    procedure SetShape(AValue: TdxSpreadSheetShape);
    procedure ShapeChangeHandler(Sender: TObject);
  protected
    function CreateShape: TdxSpreadSheetShape; virtual;
    function CreateViewInfo: TdxSpreadSheetContainerViewInfo; override;
    procedure CreateSubClasses; override;
    procedure DoAssign(Source: TPersistent); override;

    procedure LoadBrushFromStream(ABrush: TdxGPBrush; AReader: TcxReader);
    procedure LoadImageFromStream(AImage: TdxGPImage; AReader: TcxReader);
    procedure LoadFromStream(AReader: TcxReader; AVersion: Word; ACanReadContent: Boolean = True); override;
    procedure SaveBrushToStream(ABrush: TdxGPBrush; AWriter: TcxWriter);
    procedure SaveImageToStream(AImage: TdxGPImage; AWriter: TcxWriter);
    procedure SaveToStream(AWriter: TcxWriter; ACanSaveContent: Boolean = True); override;
  public
    destructor Destroy; override;
    //
    property Shape: TdxSpreadSheetShape read FShape write SetShape;
  end;

  { TdxSpreadSheetShapeContainerViewInfo }

  TdxSpreadSheetShapeContainerViewInfo = class(TdxSpreadSheetContainerViewInfo)
  strict private
    FShapeOutlinePath: TdxGPPath;
    FShapePath: TdxGPPath;

    function GetOwner: TdxSpreadSheetShapeContainer;
    function GetShape: TdxSpreadSheetShape; inline;
    function GetShapeOutlinePath: TdxGPPath;
    function GetShapePath: TdxGPPath;
  protected
    procedure ContentBoundsChanged; override;
    function CreateShapeOutlinePath: TdxGPPath; virtual;
    function CreateShapePath: TdxGPPath; virtual;
    function DoCreateShapePath(const R: TdxRectF): TdxGPPath; virtual;
    function DoInitHitTest(const P: TPoint; AHitTest: TdxSpreadSheetCustomHitTest): Boolean; override;
    procedure DrawBackground(ACanvas: TdxGPCanvas); override;
    procedure DrawBorder(ACanvas: TdxGPCanvas); override;
    function GetBorderWidth: Single; override;
    function GetRadius: Single; virtual;
    function GetRealDrawingBounds: TRect; override;
  public
    destructor Destroy; override;
    procedure RecreateShape;
    //
    property Owner: TdxSpreadSheetShapeContainer read GetOwner;
    property Shape: TdxSpreadSheetShape read GetShape;
    property ShapeOutlinePath: TdxGPPath read GetShapeOutlinePath;
    property ShapePath: TdxGPPath read GetShapePath;
  end;

  { TdxSpreadSheetPicture }

  TdxSpreadSheetPicture = class(TdxSpreadSheetCustomDrawingObject)
  strict private
    FCropMargins: TRect;
    FImageHandle: TdxSpreadSheetSharedImageHandle;

    function GetEmpty: Boolean;
    function GetImage: TdxSmartImage;
    procedure SetCropMargins(const AValue: TRect);
    procedure SetImage(AValue: TdxSmartImage);
    procedure SetImageHandle(const AValue: TdxSpreadSheetSharedImageHandle);
  protected
    property ImageHandle: TdxSpreadSheetSharedImageHandle read FImageHandle write SetImageHandle;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property CropMargins: TRect read FCropMargins write SetCropMargins;
    property Empty: Boolean read GetEmpty;
    property Image: TdxSmartImage read GetImage write SetImage;
  end;

  { TdxSpreadSheetPictureContainer }

  TdxSpreadSheetPictureContainer = class(TdxSpreadSheetShapeContainer)
  strict private
    FPicture: TdxSpreadSheetPicture;
    FRelativeResize: Boolean;

    procedure PictureChangeHandler(Sender: TObject);
    procedure SetPicture(AValue: TdxSpreadSheetPicture);
  protected
    function CreatePicture: TdxSpreadSheetPicture; virtual;
    function CreateViewInfo: TdxSpreadSheetContainerViewInfo; override;
    procedure CreateSubClasses; override;
    procedure DoAssign(Source: TPersistent); override;
    procedure LoadFromStream(AReader: TcxReader; AVersion: Word; ACanReadContent: Boolean = True); override;
    procedure SaveToStream(AWriter: TcxWriter; ACanSaveContent: Boolean = True); override;
  public
    destructor Destroy; override;

    property Picture: TdxSpreadSheetPicture read FPicture write SetPicture;
    property RelativeResize: Boolean read FRelativeResize write FRelativeResize;
  end;

  { TdxSpreadSheetPictureContainerViewInfo }

  TdxSpreadSheetPictureContainerViewInfo = class(TdxSpreadSheetShapeContainerViewInfo)
  strict private
    function GetOwner: TdxSpreadSheetPictureContainer; inline;
    function GetPicture: TdxSpreadSheetPicture; inline;
  protected
    procedure DrawContent(ACanvas: TdxGPCanvas); override;
  public
    property Owner: TdxSpreadSheetPictureContainer read GetOwner;
    property Picture: TdxSpreadSheetPicture read GetPicture;
  end;

  { TdxSpreadSheetCustomTextBox }

  TdxSpreadSheetCustomTextBox = class(TdxSpreadSheetCustomDrawingObject)
  strict private
    FAlignHorz: TAlignment;
    FAlignVert: TVerticalAlignment;
    FAutoSize: Boolean;
    FContentOffsets: TRect;
    FFont: TdxSpreadSheetFont;
    FText: TdxSpreadSheetSharedString;
    FWordWrap: Boolean;

    function GetTextAsString: string;
    procedure SetAlignHorz(AValue: TAlignment);
    procedure SetAlignVert(AValue: TVerticalAlignment);
    procedure SetAutoSize(AValue: Boolean);
    procedure SetContentOffsets(AValue: TRect);
    procedure SetFont(AValue: TdxSpreadSheetFont);
    procedure SetText(AValue: TdxSpreadSheetSharedString);
    procedure SetTextAsString(const AValue: string);
    procedure SetWordWrap(AValue: Boolean);
  protected
    procedure LoadFromStream(AReader: TcxReader; AVersion: Word; ACanReadContent: Boolean = True); virtual;
    procedure SaveToStream(AWriter: TcxWriter; ACanSaveContent: Boolean = True); virtual;

    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
  public
    constructor Create(ASpreadSheet: TdxCustomSpreadSheet); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function IsEmpty: Boolean; virtual;
    //
    property AlignHorz: TAlignment read FAlignHorz write SetAlignHorz default taLeftJustify;
    property AlignVert: TVerticalAlignment read FAlignVert write SetAlignVert default taAlignTop;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property ContentOffsets: TRect read FContentOffsets write SetContentOffsets;
    property Font: TdxSpreadSheetFont read FFont write SetFont;
    property Text: TdxSpreadSheetSharedString read FText write SetText;
    property TextAsString: string read GetTextAsString write SetTextAsString;
  end;

  { TdxSpreadSheetTextBox }

  TdxSpreadSheetTextBox = class(TdxSpreadSheetCustomTextBox)
  public
    property WordWrap;
  end;

  { TdxSpreadSheetTextBoxFont }

  TdxSpreadSheetTextBoxFont = class(TdxSpreadSheetFont)
  strict private
    FOwner: TdxSpreadSheetCustomTextBox;
  protected
    procedure Changed; override;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomTextBox); reintroduce;
  end;

  { TdxSpreadSheetTextBoxViewInfo }

  TdxSpreadSheetTextBoxViewInfo = class
  strict private
    FBounds: TdxRectF;
    FFont: TdxGPFont;
    FFontBrush: TdxGPBrush;
    FStringFormat: TdxGPStringFormat;
    FTextBox: TdxSpreadSheetCustomTextBox;
  protected
    procedure CalculateFont; virtual;
    procedure CalculateStringFormat; virtual;
  public
    constructor Create(ATextBox: TdxSpreadSheetCustomTextBox);
    destructor Destroy; override;
    procedure Calculate; virtual;
    procedure Draw(ACanvas: TdxGPCanvas); virtual;
    function MeasureSize(const AWidth: Double): TdxSizeF; virtual;
    //
    property Bounds: TdxRectF read FBounds write FBounds;
    property Font: TdxGPFont read FFont;
    property FontBrush: TdxGPBrush read FFontBrush;
    property StringFormat: TdxGPStringFormat read FStringFormat;
    property TextBox: TdxSpreadSheetCustomTextBox read FTextBox;
  end;

  { TdxSpreadSheetCustomTextBoxContainer }

  TdxSpreadSheetCustomTextBoxContainer = class(TdxSpreadSheetShapeContainer)
  strict private
    FTextBox: TdxSpreadSheetCustomTextBox;

    function GetCalculator: TdxSpreadSheetCustomTextBoxContainerCalculator;
    procedure TextBoxChangeHandler(Sender: TObject);
    procedure SetTextBox(AValue: TdxSpreadSheetCustomTextBox);
  protected
    procedure Changed(AChanges: TdxSpreadSheetContainerChanges); override;
    procedure CheckAutoSize;
    function CreateTextBox: TdxSpreadSheetCustomTextBox; virtual;
    function CreateViewInfo: TdxSpreadSheetContainerViewInfo; override;
    procedure CreateSubClasses; override;
    procedure DoAssign(Source: TPersistent); override;
    procedure DoChanged(AChanges: TdxSpreadSheetContainerChanges); override;
    procedure LoadFromStream(AReader: TcxReader; AVersion: Word; ACanReadContent: Boolean = True); override;
    procedure SaveToStream(AWriter: TcxWriter; ACanSaveContent: Boolean = True); override;
    //
    property Calculator: TdxSpreadSheetCustomTextBoxContainerCalculator read GetCalculator;
    property TextBox: TdxSpreadSheetCustomTextBox read FTextBox write SetTextBox;
  public
    destructor Destroy; override;
  end;

  { TdxSpreadSheetCustomTextBoxContainerCalculator }

  TdxSpreadSheetCustomTextBoxContainerCalculator = class(TdxSpreadSheetContainerCalculator)
  strict private
    FAutoSizeLockCount: Integer;
  protected
    procedure BeginAutoSize;
    function CalculateAutoSize(AWidth: Integer): TSize;
    procedure EndAutoSize;
    function IsAutoSizing: Boolean;
    function KeepAutoSize(const APrevBounds, ANewBounds: TRect): Boolean; virtual;

    function GetTextBox: TdxSpreadSheetCustomTextBox; virtual; abstract;
  public
    procedure ApplyAutoSize;
    procedure UpdateAnchors(const ABounds: TRect); override;
    //
    property TextBox: TdxSpreadSheetCustomTextBox read GetTextBox;
  end;

  { TdxSpreadSheetCustomTextBoxContainerViewInfo }

  TdxSpreadSheetCustomTextBoxContainerViewInfo = class(TdxSpreadSheetShapeContainerViewInfo)
  strict private
    FTextBoxViewInfo: TdxSpreadSheetTextBoxViewInfo;

    function GetOwner: TdxSpreadSheetCustomTextBoxContainer; inline;
    function GetTextBox: TdxSpreadSheetCustomTextBox; inline;
  protected
    procedure ContentBoundsChanged; override;
    function CreateTextBoxViewInfo: TdxSpreadSheetTextBoxViewInfo; virtual;
    procedure DoCalculate; override;
    procedure DrawContent(ACanvas: TdxGPCanvas); override;
  public
    constructor Create(AContainer: TdxSpreadSheetContainer); override;
    destructor Destroy; override;
    //
    property Owner: TdxSpreadSheetCustomTextBoxContainer read GetOwner;
    property TextBox: TdxSpreadSheetCustomTextBox read GetTextBox;
    property TextBoxViewInfo: TdxSpreadSheetTextBoxViewInfo read FTextBoxViewInfo;
  end;

  { TdxSpreadSheetTextBoxContainer }

  TdxSpreadSheetTextBoxContainer = class(TdxSpreadSheetCustomTextBoxContainer)
  strict private
    function GetTextBox: TdxSpreadSheetTextBox;
    procedure SetTextBox(AValue: TdxSpreadSheetTextBox);
  protected
    function CreateCalculator: TdxSpreadSheetContainerCalculator; override;
    function CreateTextBox: TdxSpreadSheetCustomTextBox; override;
  public
    property TextBox: TdxSpreadSheetTextBox read GetTextBox write SetTextBox;
  end;

  { TdxSpreadSheetTextBoxContainerCalculator }

  TdxSpreadSheetTextBoxContainerCalculator = class(TdxSpreadSheetCustomTextBoxContainerCalculator)
  protected
    function KeepAutoSize(const APrevBounds, ANewBounds: TRect): Boolean; override;
    function GetTextBox: TdxSpreadSheetCustomTextBox; override;
  end;

  { TdxSpreadSheetCommentTextBox }

  TdxSpreadSheetCommentTextBox = class(TdxSpreadSheetCustomTextBox)
  public
    constructor Create(ASpreadSheet: TdxCustomSpreadSheet); override;
  end;

  { TdxSpreadSheetCommentTextBoxViewInfo }

  TdxSpreadSheetCommentTextBoxViewInfo = class(TdxSpreadSheetTextBoxViewInfo)
  public
    function MeasureSize(const AWidth: Double): TdxSizeF; override;
  end;

  { TdxSpreadSheetCommentContainer }

  TdxSpreadSheetCommentContainer = class(TdxSpreadSheetCustomTextBoxContainer)
  strict private
    FAuthor: string;
    FCell: TdxSpreadSheetCell;

    function GetCalculator: TdxSpreadSheetCommentContainerCalculator; inline;
    function GetTextBox: TdxSpreadSheetCommentTextBox; inline;
    procedure SetCell(AValue: TdxSpreadSheetCell);
    procedure SetTextBox(AValue: TdxSpreadSheetCommentTextBox);
  protected
    function CanClone: Boolean; override;
    function CanFocusViaKeyboard: Boolean; override;
    function CreateCalculator: TdxSpreadSheetContainerCalculator; override;
    function CreateTextBox: TdxSpreadSheetCustomTextBox; override;
    function CreateViewInfo: TdxSpreadSheetContainerViewInfo; override;
    procedure CreateSubClasses; override;
    procedure DoAssign(Source: TPersistent); override;
    procedure DoSetParent(AValue: TdxSpreadSheetCustomView); override;
    function IsTransformsSupported: Boolean; override;

    procedure CellRemoving(ACell: TdxSpreadSheetCell); override;
    function IsCellUsed(ACell: TdxSpreadSheetCell): Boolean; override;

    procedure LoadFromStream(AReader: TcxReader; AVersion: Word; ACanReadContent: Boolean = True); overload; override;
    procedure LoadFromStream(AReader: TcxReader; AVersion: Word;
      AAnchorRow, AAnchorColumn: Integer; ACanReadContent: Boolean = True); reintroduce; overload;
    procedure SaveToStream(AWriter: TcxWriter; ACanSaveContent: Boolean = True); overload; override;
    procedure SaveToStream(AWriter: TcxWriter; AAnchorRow, AAnchorColumn: Integer; ACanSaveContent: Boolean = True); reintroduce; overload;

    property Calculator: TdxSpreadSheetCommentContainerCalculator read GetCalculator;
  public
    property Author: string read FAuthor write FAuthor;
    property Cell: TdxSpreadSheetCell read FCell write SetCell;
    property TextBox: TdxSpreadSheetCommentTextBox read GetTextBox write SetTextBox;
  end;

  { TdxSpreadSheetCommentContainerCalculator }

  TdxSpreadSheetCommentContainerCalculator = class(TdxSpreadSheetCustomTextBoxContainerCalculator)
  protected
    function GetTextBox: TdxSpreadSheetCustomTextBox; override;
  end;

  { TdxSpreadSheetCommentContainerConnectionViewInfo }

  TdxSpreadSheetCommentContainerConnectionViewInfo = class(TdxSpreadSheetCellViewInfo)
  strict private const
    ArrowAngle = 45;
    ArrowSize = 6;
  strict private
    function GetContainerViewInfo: TdxSpreadSheetCommentContainerViewInfo; inline;
  protected
    FArrowPoints: array [0..2] of TdxPointF;
    FLinePoint1: TPoint;
    FLinePoint2: TPoint;

    procedure AdjustAbsoluteBounds; virtual;
    procedure CalculateArrow; virtual;
    procedure CalculateBounds; override;
    procedure CalculateLinePoints; virtual;
    procedure CheckBounds;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function GetColor: TdxAlphaColor; virtual;
    function GetSpreadSheet: TdxCustomSpreadSheet; override;
    function InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean; override;
    procedure InvalidateRect(const R: TRect); override;
  public
    property Color: TdxAlphaColor read GetColor;
    property ContainerViewInfo: TdxSpreadSheetCommentContainerViewInfo read GetContainerViewInfo;
    property LinePoint1: TPoint read FLinePoint1 write FLinePoint1;
    property LinePoint2: TPoint read FLinePoint2 write FLinePoint2;
  end;

  { TdxSpreadSheetCommentContainerHelper }

  TdxSpreadSheetCommentContainerHelper = class
  public
    class function GetAbsoluteCellBounds(ACell: TdxSpreadSheetCell): TRect;
    class function GetCellBounds(ACell: TdxSpreadSheetCell): TRect;
    class function GetDefaultPosition(ACell: TdxSpreadSheetCell): TRect;
  end;

  { TdxSpreadSheetCommentContainerViewInfo }

  TdxSpreadSheetCommentContainerViewInfo = class(TdxSpreadSheetCustomTextBoxContainerViewInfo)
  strict private
    FConnectionViewInfo: TdxSpreadSheetCommentContainerConnectionViewInfo;

    function GetOwner: TdxSpreadSheetCommentContainer;
  protected
    procedure ContentBoundsChanged; override;
    function CreateTextBoxViewInfo: TdxSpreadSheetTextBoxViewInfo; override;
    function DoCreateShapePath(const R: TdxRectF): TdxGPPath; override;
  public
    property ConnectionViewInfo: TdxSpreadSheetCommentContainerConnectionViewInfo read FConnectionViewInfo write FConnectionViewInfo;
    property Owner: TdxSpreadSheetCommentContainer read GetOwner;
  end;

  { TdxSpreadSheetContainerCustomMoveDragAndDropObject }

  TdxSpreadSheetContainerCustomMoveDragAndDropObject = class(TdxSpreadSheetContainerCustomDragAndDropObject)
  protected
    procedure AlignToAxes(var R: TRect); virtual;
    procedure BeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
  end;

  { TdxSpreadSheetClonedContainerMoveDragAndDropObject }

  TdxSpreadSheetClonedContainerMoveDragAndDropObject = class(TdxSpreadSheetContainerCustomMoveDragAndDropObject)
  strict private
    FContainer: TdxSpreadSheetContainer;

    function GetContainers: TdxSpreadSheetCellViewInfoList;
  protected
    procedure BeginDragAndDrop; override;
    function GetContainerViewInfo: TdxSpreadSheetContainerViewInfo; override;
    function TranslateCoords(const P: TPoint): TPoint; override;
  public
    constructor Create(AContainer: TdxSpreadSheetContainer); reintroduce; virtual;
    //
    property Containers: TdxSpreadSheetCellViewInfoList read GetContainers;
  end;

  { TdxSpreadSheetContainerMoveDragAndDropObject }

  TdxSpreadSheetContainerMoveDragAndDropObject = class(TdxSpreadSheetContainerCustomMoveDragAndDropObject)
  strict private
    FClonedContainer: TdxSpreadSheetContainer;
    FClonedContainerDragAndDropObject: TdxSpreadSheetClonedContainerMoveDragAndDropObject;

    function GetMoveClone: Boolean;
    procedure SetMoveClone(AValue: Boolean);
  protected
    function CreateDragAndDropObjectForClonedContainer: TdxSpreadSheetClonedContainerMoveDragAndDropObject; virtual;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    //
    property ClonedContainer: TdxSpreadSheetContainer read FClonedContainer;
    property ClonedContainerDragAndDropObject: TdxSpreadSheetClonedContainerMoveDragAndDropObject read FClonedContainerDragAndDropObject;
  public
    property MoveClone: Boolean read GetMoveClone write SetMoveClone;
  end;

  { TdxSpreadSheetContainerResizeDragAndDropObject }

  TdxSpreadSheetContainerResizeDragAndDropObject = class(TdxSpreadSheetContainerCustomDragAndDropObject)
  strict private
    procedure ApplyBounds(const AFixedPoint, AResizedPoint: TdxPointF);
    procedure GetPoints(out AFixedPoint, AResizedPoint: TdxPointF);
    function GetTransform: TdxSpreadSheetContainerTransform;
  protected
    FMarker: TdxSpreadSheetSizingMarker;

    procedure AlignToCells(var ADeltaX, ADeltaY: Single);
    function CalculateContentBounds(const ADeltaX, ADeltaY: Single): TdxRectF;
    procedure CheckAspectUsingCornerHandles(var ADeltaX, ADeltaY: Single);

    procedure BeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetContainerViewInfo: TdxSpreadSheetContainerViewInfo; override;
    procedure RecalculateBounds; virtual;
  public
    property Marker: TdxSpreadSheetSizingMarker read FMarker;
    property Transform: TdxSpreadSheetContainerTransform read GetTransform;
  end;

  { TdxSpreadSheetContainerRotateDragAndDropObject }

  TdxSpreadSheetContainerRotateDragAndDropObject = class(TdxSpreadSheetContainerCustomDragAndDropObject)
  protected
    procedure BeginDragAndDrop; override;
    procedure CheckScrollArea(const P: TPoint); override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetContainerViewInfo: TdxSpreadSheetContainerViewInfo; override;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
  end;

implementation

uses
  Math, SysUtils, dxSpreadSheetCoreHistory, dxHashUtils, dxTypeHelpers, dxGDIPlusAPI, dxSpreadSheetStrs,
  dxSpreadSheetCoreStrs;

type
  TdxCustomSpreadSheetAccess = class(TdxCustomSpreadSheet);
  TdxSpreadSheetContainerAccess = class(TdxSpreadSheetContainer);
  TdxSpreadSheetContainersAccess = class(TdxSpreadSheetContainers);
  TdxSpreadSheetCustomViewAccess = class(TdxSpreadSheetCustomView);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);
  TdxSpreadSheetTableViewControllerAccess = class(TdxSpreadSheetTableViewController);

{ TdxSpreadSheetShape }

constructor TdxSpreadSheetShape.Create(ASpreadSheet: TdxCustomSpreadSheet);
begin
  inherited Create(ASpreadSheet);
  FPen := TdxGPPen.Create;
  FPen.MiterLimit := 0;
  FPen.OnChange := ChangeHandler;
  FBrush := TdxGPBrush.Create;
  FBrush.OnChange := ChangeHandler;
end;

destructor TdxSpreadSheetShape.Destroy;
begin
  FreeAndNil(FBrush);
  FreeAndNil(FPen);
  inherited Destroy;
end;

procedure TdxSpreadSheetShape.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxSpreadSheetShape then
  begin
    Pen := TdxSpreadSheetShape(Source).Pen;
    Brush := TdxSpreadSheetShape(Source).Brush;
    ShapeType := TdxSpreadSheetShape(Source).ShapeType;
  end;
end;

function TdxSpreadSheetShape.IsEmpty: Boolean;
begin
  Result := Pen.IsEmpty and Brush.IsEmpty and (ShapeType = stRect);
end;

procedure TdxSpreadSheetShape.ChangeHandler(Sender: TObject);
begin
  Changed;
end;

procedure TdxSpreadSheetShape.SetBrush(const AValue: TdxGPBrush);
begin
  FBrush.Assign(AValue);
end;

procedure TdxSpreadSheetShape.SetPen(const AValue: TdxGPPen);
begin
  FPen.Assign(AValue);
end;

procedure TdxSpreadSheetShape.SetShapeType(const AValue: TdxSpreadSheetShapeType);
begin
  if AValue <> FShapeType then
  begin
    FShapeType := AValue;
    Changed;
  end;
end;

{ TdxSpreadSheetShapeContainer }

destructor TdxSpreadSheetShapeContainer.Destroy;
begin
  FreeAndNil(FShape);
  inherited Destroy;
end;

function TdxSpreadSheetShapeContainer.CreateShape: TdxSpreadSheetShape;
begin
  Result := TdxSpreadSheetShape.Create(SpreadSheet);
end;

procedure TdxSpreadSheetShapeContainer.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FShape := CreateShape;
  FShape.OnChange := ShapeChangeHandler;
end;

function TdxSpreadSheetShapeContainer.CreateViewInfo: TdxSpreadSheetContainerViewInfo;
begin
  Result := TdxSpreadSheetShapeContainerViewInfo.Create(Self);
end;

procedure TdxSpreadSheetShapeContainer.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxSpreadSheetShapeContainer then
    Shape := TdxSpreadSheetShapeContainer(Source).Shape;
end;

procedure TdxSpreadSheetShapeContainer.LoadBrushFromStream(ABrush: TdxGPBrush; AReader: TcxReader);
var
  AColor: TdxAlphaColor;
  ACount: Integer;
  AOffset: Single;
  I: Integer;
begin
  ABrush.Color := AReader.ReadCardinal;
  ABrush.Style := TdxGPBrushStyle(AReader.ReadInteger);
  ABrush.GradientMode := TdxGPBrushGradientMode(AReader.ReadInteger);
  ACount := AReader.ReadInteger;
  for I := 0 to ACount - 1 do
  begin
    AOffset := AReader.ReadSingle;
    AColor := AReader.ReadCardinal;
    ABrush.GradientPoints.Add(AOffset, AColor);
  end;
  LoadImageFromStream(ABrush.Texture, AReader);
end;

procedure TdxSpreadSheetShapeContainer.LoadImageFromStream(AImage: TdxGPImage; AReader: TcxReader);
var
  AImageData: TMemoryStream;
begin
  AImageData := TMemoryStream.Create;
  try
    AImageData.Size := AReader.ReadInteger;
    if AImageData.Size > 0 then
    begin
      AReader.Stream.ReadBuffer(AImageData.Memory^, AImageData.Size);
      AImage.LoadFromStream(AImageData);
    end
    else
      AImage.Clear;
  finally
    AImageData.Free;
  end;
end;

procedure TdxSpreadSheetShapeContainer.LoadFromStream(AReader: TcxReader; AVersion: Word; ACanReadContent: Boolean = True);
begin
  inherited LoadFromStream(AReader, AVersion, ACanReadContent);
  Shape.ShapeType := TdxSpreadSheetShapeType(AReader.ReadInteger);
  LoadBrushFromStream(Shape.Brush, AReader);

  Shape.Pen.Style := TdxGPPenStyle(AReader.ReadInteger);
  Shape.Pen.Width := AReader.ReadSingle;
  LoadBrushFromStream(Shape.Pen.Brush, AReader);
end;

procedure TdxSpreadSheetShapeContainer.SaveBrushToStream(ABrush: TdxGPBrush; AWriter: TcxWriter);
var
  I: Integer;
begin
  AWriter.WriteCardinal(ABrush.Color);
  AWriter.WriteInteger(Ord(ABrush.Style));
  AWriter.WriteInteger(Ord(ABrush.GradientMode));
  AWriter.WriteInteger(ABrush.GradientPoints.Count);
  for I := 0 to ABrush.GradientPoints.Count - 1 do
  begin
    AWriter.WriteSingle(ABrush.GradientPoints.Offsets[I]);
    AWriter.WriteCardinal(ABrush.GradientPoints.Colors[I]);
  end;
  SaveImageToStream(ABrush.Texture, AWriter);
end;

procedure TdxSpreadSheetShapeContainer.SaveImageToStream(AImage: TdxGPImage; AWriter: TcxWriter);
var
  AImageData: TMemoryStream;
begin
  if (AImage <> nil) and not AImage.Empty then
  begin
    AImageData := TMemoryStream.Create;
    try
      AImage.SaveToStream(AImageData);
      AImageData.Position := 0;
      AWriter.WriteInteger(AImageData.Size);
      AWriter.Stream.WriteBuffer(AImageData.Memory^, AImageData.Size);
    finally
      AImageData.Free;
    end;
  end
  else
    AWriter.WriteInteger(0);
end;

procedure TdxSpreadSheetShapeContainer.SaveToStream(AWriter: TcxWriter; ACanSaveContent: Boolean = True);
begin
  inherited SaveToStream(AWriter, ACanSaveContent);

  AWriter.WriteInteger(Ord(Shape.ShapeType));
  SaveBrushToStream(Shape.Brush, AWriter);

  AWriter.WriteInteger(Ord(Shape.Pen.Style));
  AWriter.WriteSingle(Shape.Pen.Width);
  SaveBrushToStream(Shape.Pen.Brush, AWriter);
end;

procedure TdxSpreadSheetShapeContainer.SetShape(AValue: TdxSpreadSheetShape);
begin
  Shape.Assign(AValue);
end;

procedure TdxSpreadSheetShapeContainer.ShapeChangeHandler(Sender: TObject);
begin
  Changed([ccContent]);
end;

{ TdxSpreadSheetShapeContainerViewInfo }

destructor TdxSpreadSheetShapeContainerViewInfo.Destroy;
begin
  FreeAndNil(FShapePath);
  FreeAndNil(FShapeOutlinePath);
  inherited Destroy;
end;

procedure TdxSpreadSheetShapeContainerViewInfo.RecreateShape;
begin
  FreeAndNil(FShapePath);
  FreeAndNil(FShapeOutlinePath);
end;

procedure TdxSpreadSheetShapeContainerViewInfo.ContentBoundsChanged;
begin
  inherited ContentBoundsChanged;
  RecreateShape;
end;

function TdxSpreadSheetShapeContainerViewInfo.CreateShapeOutlinePath: TdxGPPath;
begin
  Result := DoCreateShapePath(cxRectInflate(ContentBounds, Shape.Pen.Width / 2));
end;

function TdxSpreadSheetShapeContainerViewInfo.CreateShapePath: TdxGPPath;
begin
  Result := DoCreateShapePath(ContentBounds);
end;

function TdxSpreadSheetShapeContainerViewInfo.DoCreateShapePath(const R: TdxRectF): TdxGPPath;
var
  ARadius: Single;
begin
  Result := TdxGPPath.Create;
  case Shape.ShapeType of
    stRect:
      Result.AddRect(R);
    stEllipse:
      Result.AddEllipse(R);
    stRoundRect:
      begin
        ARadius := GetRadius;
        Result.AddRoundRect(R, ARadius, ARadius);
      end;
  end;
end;

function TdxSpreadSheetShapeContainerViewInfo.DoInitHitTest(const P: TPoint; AHitTest: TdxSpreadSheetCustomHitTest): Boolean;
begin
  Result := inherited DoInitHitTest(P, AHitTest) and
    (ShapePath.IsPointInPath(P) or ShapeOutlinePath.IsPointInPathOutline(P, Shape.Pen));
end;

procedure TdxSpreadSheetShapeContainerViewInfo.DrawBackground(ACanvas: TdxGPCanvas);
begin
  ACanvas.Path(ShapePath, nil, Shape.Brush);
end;

procedure TdxSpreadSheetShapeContainerViewInfo.DrawBorder(ACanvas: TdxGPCanvas);
begin
  ACanvas.Path(ShapeOutlinePath, Shape.Pen, nil);
end;

function TdxSpreadSheetShapeContainerViewInfo.GetBorderWidth: Single;
begin
  Result := Shape.Pen.Width;
end;

function TdxSpreadSheetShapeContainerViewInfo.GetRadius: Single;
begin
  Result := Min(ContentBounds.Width / 6, cxRectHeight(Bounds) / 6);
end;

function TdxSpreadSheetShapeContainerViewInfo.GetRealDrawingBounds: TRect;
begin
  Result := cxRectInflate(inherited GetRealDrawingBounds, 1);
end;

function TdxSpreadSheetShapeContainerViewInfo.GetOwner: TdxSpreadSheetShapeContainer;
begin
  Result := TdxSpreadSheetShapeContainer(inherited Owner);
end;

function TdxSpreadSheetShapeContainerViewInfo.GetShape: TdxSpreadSheetShape;
begin
  Result := Owner.Shape;
end;

function TdxSpreadSheetShapeContainerViewInfo.GetShapeOutlinePath: TdxGPPath;
begin
  if FShapeOutlinePath = nil then
    FShapeOutlinePath := CreateShapeOutlinePath;
  Result := FShapeOutlinePath;
end;

function TdxSpreadSheetShapeContainerViewInfo.GetShapePath: TdxGPPath;
begin
  if FShapePath = nil then
    FShapePath := CreateShapePath;
  Result := FShapePath;
end;

{ TdxSpreadSheetPicture }

destructor TdxSpreadSheetPicture.Destroy;
begin
  ImageHandle := nil;
  inherited Destroy;
end;

procedure TdxSpreadSheetPicture.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxSpreadSheetPicture then
  begin
    CropMargins := TdxSpreadSheetPicture(Source).CropMargins;
    ImageHandle := TdxSpreadSheetPicture(Source).ImageHandle;
  end;
end;

function TdxSpreadSheetPicture.GetEmpty: Boolean;
begin
  Result := (Image = nil) or Image.Empty;
end;

function TdxSpreadSheetPicture.GetImage: TdxSmartImage;
begin
  if ImageHandle <> nil then
    Result := ImageHandle.Image
  else
    Result := nil;
end;

procedure TdxSpreadSheetPicture.SetCropMargins(const AValue: TRect);
begin
  if not cxRectIsEqual(FCropMargins, AValue) then
  begin
    FCropMargins := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetPicture.SetImage(AValue: TdxSmartImage);
begin
  if AValue <> nil then
    ImageHandle := TdxCustomSpreadSheetAccess(SpreadSheet).SharedImages.Add(AValue)
  else
    ImageHandle := nil;
end;

procedure TdxSpreadSheetPicture.SetImageHandle(const AValue: TdxSpreadSheetSharedImageHandle);
begin
  if dxChangeHandle(TdxHashTableItem(FImageHandle), AValue) then
    Changed;
end;

{ TdxSpreadSheetPictureContainer }

destructor TdxSpreadSheetPictureContainer.Destroy;
begin
  FreeAndNil(FPicture);
  inherited Destroy;
end;

function TdxSpreadSheetPictureContainer.CreatePicture: TdxSpreadSheetPicture;
begin
  Result := TdxSpreadSheetPicture.Create(SpreadSheet);
end;

procedure TdxSpreadSheetPictureContainer.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FRelativeResize := True;
  Restrictions := Restrictions + [crNoChangeAspectUsingCornerHandles];
  FPicture := CreatePicture;
  FPicture.OnChange := PictureChangeHandler;
end;

function TdxSpreadSheetPictureContainer.CreateViewInfo: TdxSpreadSheetContainerViewInfo;
begin
  Result := TdxSpreadSheetPictureContainerViewInfo.Create(Self);
end;

procedure TdxSpreadSheetPictureContainer.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxSpreadSheetPictureContainer then
    Picture := TdxSpreadSheetPictureContainer(Source).Picture;
end;

procedure TdxSpreadSheetPictureContainer.LoadFromStream(AReader: TcxReader; AVersion: Word; ACanReadContent: Boolean = True);
var
  AImage: TdxSmartImage;
begin
  inherited LoadFromStream(AReader, AVersion, ACanReadContent);

  Picture.CropMargins := AReader.ReadRect;
  if AVersion >= 1 then
    RelativeResize := AReader.ReadBoolean;

  if ACanReadContent then
  begin
    AImage := TdxSmartImage.Create;
    try
      LoadImageFromStream(AImage, AReader);
      if AImage.Empty then
        Picture.Image := nil
      else
        Picture.Image := AImage;
    finally
      AImage.Free;
    end;
  end;
end;

procedure TdxSpreadSheetPictureContainer.SaveToStream(AWriter: TcxWriter; ACanSaveContent: Boolean = True);
begin
  inherited SaveToStream(AWriter, ACanSaveContent);
  AWriter.WriteRect(Picture.CropMargins);
  AWriter.WriteBoolean(RelativeResize);
  if ACanSaveContent then
    SaveImageToStream(Picture.Image, AWriter);
end;

procedure TdxSpreadSheetPictureContainer.PictureChangeHandler(Sender: TObject);
begin
  Changed([ccContent]);
end;

procedure TdxSpreadSheetPictureContainer.SetPicture(AValue: TdxSpreadSheetPicture);
begin
  FPicture.Assign(AValue);
end;

{ TdxSpreadSheetPictureContainerViewInfo }

procedure TdxSpreadSheetPictureContainerViewInfo.DrawContent(ACanvas: TdxGPCanvas);
begin
  if not Picture.Empty then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.SetClipPath(ShapePath, gmIntersect);
      ACanvas.Draw(Picture.ImageHandle.Image, cxRectInflate(ContentBounds, dxRectF(Picture.CropMargins)));
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

function TdxSpreadSheetPictureContainerViewInfo.GetOwner: TdxSpreadSheetPictureContainer;
begin
  Result := TdxSpreadSheetPictureContainer(inherited Owner);
end;

function TdxSpreadSheetPictureContainerViewInfo.GetPicture: TdxSpreadSheetPicture;
begin
  Result := Owner.Picture;
end;

{ TdxSpreadSheetTextBox }

constructor TdxSpreadSheetCustomTextBox.Create(ASpreadSheet: TdxCustomSpreadSheet);
begin
  inherited Create(ASpreadSheet);
  FFont := TdxSpreadSheetTextBoxFont.Create(Self);
end;

destructor TdxSpreadSheetCustomTextBox.Destroy;
begin
  Text := nil;
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TdxSpreadSheetCustomTextBox.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetTextBox then
  begin
    AlignHorz := TdxSpreadSheetTextBox(Source).AlignHorz;
    AlignVert := TdxSpreadSheetTextBox(Source).AlignVert;
    AutoSize := TdxSpreadSheetTextBox(Source).AutoSize;
    ContentOffsets := TdxSpreadSheetTextBox(Source).ContentOffsets;
    Text := TdxSpreadSheetTextBox(Source).Text;
    Font := TdxSpreadSheetTextBox(Source).Font;
    WordWrap := TdxSpreadSheetTextBox(Source).WordWrap;
  end;
end;

function TdxSpreadSheetCustomTextBox.IsEmpty: Boolean;
begin
  Result := TextAsString = '';
end;

procedure TdxSpreadSheetCustomTextBox.LoadFromStream(AReader: TcxReader; AVersion: Word; ACanReadContent: Boolean = True);
begin
  AlignHorz := TAlignment(AReader.ReadByte);
  AlignVert := TVerticalAlignment(AReader.ReadByte);
  AutoSize := AReader.ReadBoolean;
  ContentOffsets := AReader.ReadRect;
  WordWrap := AReader.ReadBoolean;
  Font.Handle := SpreadSheet.CellStyles.Fonts.CreateFromStream(AReader);

  if ACanReadContent then
  begin
    if AReader.ReadBoolean then
      Text := TdxCustomSpreadSheetAccess(SpreadSheet).StringTable.LoadItemFromStream(AReader)
    else
      Text := nil;
  end;
end;

procedure TdxSpreadSheetCustomTextBox.SaveToStream(AWriter: TcxWriter; ACanSaveContent: Boolean = True);
begin
  AWriter.WriteByte(Ord(AlignHorz));
  AWriter.WriteByte(Ord(AlignVert));
  AWriter.WriteBoolean(AutoSize);
  AWriter.WriteRect(ContentOffsets);
  AWriter.WriteBoolean(WordWrap);
  Font.Handle.SaveToStream(AWriter);

  if ACanSaveContent then
  begin
    AWriter.WriteBoolean(Text <> nil);
    if Text <> nil then
      Text.SaveToStream(AWriter);
  end;
end;

function TdxSpreadSheetCustomTextBox.GetTextAsString: string;
begin
  if Text <> nil then
    Result := Text.Value
  else
    Result := '';
end;

procedure TdxSpreadSheetCustomTextBox.SetAlignHorz(AValue: TAlignment);
begin
  if FAlignHorz <> AValue then
  begin
    FAlignHorz := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetCustomTextBox.SetAlignVert(AValue: TVerticalAlignment);
begin
  if FAlignVert <> AValue then
  begin
    FAlignVert := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetCustomTextBox.SetAutoSize(AValue: Boolean);
begin
  if FAutoSize <> AValue then
  begin
    FAutoSize := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetCustomTextBox.SetContentOffsets(AValue: TRect);
begin
  AValue.Bottom := Max(AValue.Bottom, 0);
  AValue.Left := Max(AValue.Left, 0);
  AValue.Right := Max(AValue.Right, 0);
  AValue.Top := Max(AValue.Top, 0);

  if not cxRectIsEqual(FContentOffsets, AValue) then
  begin
    FContentOffsets := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetCustomTextBox.SetFont(AValue: TdxSpreadSheetFont);
begin
  FFont.Assign(AValue);
end;

procedure TdxSpreadSheetCustomTextBox.SetText(AValue: TdxSpreadSheetSharedString);
begin
  if dxChangeHandle(TdxHashTableItem(FText), AValue) then
    Changed;
end;

procedure TdxSpreadSheetCustomTextBox.SetTextAsString(const AValue: string);
begin
  Text := TdxCustomSpreadSheetAccess(SpreadSheet).StringTable.Add(AValue);
end;

procedure TdxSpreadSheetCustomTextBox.SetWordWrap(AValue: Boolean);
begin
  if FWordWrap <> AValue then
  begin
    FWordWrap := AValue;
    Changed;
  end;
end;

{ TdxSpreadSheetTextBoxFont }

constructor TdxSpreadSheetTextBoxFont.Create(AOwner: TdxSpreadSheetCustomTextBox);
begin
  inherited Create(AOwner.SpreadSheet.CellStyles.Fonts);
  FOwner := AOwner;
end;

procedure TdxSpreadSheetTextBoxFont.Changed;
begin
  FOwner.Changed;
end;

{ TdxSpreadSheetTextBoxViewInfo }

constructor TdxSpreadSheetTextBoxViewInfo.Create(ATextBox: TdxSpreadSheetCustomTextBox);
begin
  inherited Create;
  FTextBox := ATextBox;
  FFont := TdxGPFont.Create;
  FFontBrush := TdxGPBrush.Create;
  FStringFormat := TdxGPStringFormat.Create;
end;

destructor TdxSpreadSheetTextBoxViewInfo.Destroy;
begin
  FreeAndNil(FStringFormat);
  FreeAndNil(FFontBrush);
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TdxSpreadSheetTextBoxViewInfo.Calculate;
begin
  CalculateFont;
  CalculateStringFormat;
end;

procedure TdxSpreadSheetTextBoxViewInfo.Draw(ACanvas: TdxGPCanvas);
begin
  ACanvas.DrawString(TextBox.TextAsString, Font, FontBrush, Bounds, StringFormat);
end;

function TdxSpreadSheetTextBoxViewInfo.MeasureSize(const AWidth: Double): TdxSizeF;
begin
  dxGPPaintCanvas.BeginPaint(cxScreenCanvas.Handle, cxSimpleRect);
  try
    if TextBox.WordWrap then
    begin
      Result := dxGPPaintCanvas.MeasureString(TextBox.TextAsString, Font, Max(AWidth, 1));
      Result.cx := AWidth;
    end
    else
      Result := dxGPPaintCanvas.MeasureString(TextBox.TextAsString, Font);
  finally
    dxGPPaintCanvas.EndPaint;
    cxScreenCanvas.Dormant;
  end;
end;

procedure TdxSpreadSheetTextBoxViewInfo.CalculateFont;

  function TryCreateFont(AFont: TFont; out AFontHandle: GpFont): Boolean;
  var
    ALogFont: TLogFont;
  begin
    GetObject(AFont.Handle, SizeOf(ALogFont), @ALogFont);
    Result := (GdipCreateFontFromLogfont(cxScreenCanvas.Handle, @ALogFont, AFontHandle) = OK) and (AFontHandle <> nil);
    cxScreenCanvas.Dormant;
  end;

var
  AFontHandle: GpFont;
  ATempFont: TFont;
begin
  ATempFont := TFont.Create;
  try
    FreeAndNil(FFont);
    ATempFont.PixelsPerInch := 96;
    TextBox.Font.AssignToFont(ATempFont);
    if not TryCreateFont(ATempFont, AFontHandle) then
    begin
      ATempFont.Name := string(DefFontData.Name);
      if not TryCreateFont(ATempFont, AFontHandle) then
        GdipCheck(NotTrueTypeFont);
    end;
    FFont := TdxGPFont.Create(AFontHandle, ATempFont.Charset, False);
    FFontBrush.Color := dxMakeAlphaColor(cxGetActualColor(ATempFont.Color, clWindowText));
  finally
    ATempFont.Free;
  end;
end;

procedure TdxSpreadSheetTextBoxViewInfo.CalculateStringFormat;
begin
  StringFormat.Alignment := dxGpAlignmentToStringAlignment[TextBox.AlignHorz];
  StringFormat.LineAlignment := dxGpVerticalAlignmentToLineAlignment[TextBox.AlignVert];
  StringFormat.FormatFlags := Integer(dxGpWordWrapFlagsMap[TextBox.WordWrap]);
end;

{ TdxSpreadSheetCustomTextBoxContainer }

destructor TdxSpreadSheetCustomTextBoxContainer.Destroy;
begin
  FreeAndNil(FTextBox);
  inherited Destroy;
end;

procedure TdxSpreadSheetCustomTextBoxContainer.Changed(AChanges: TdxSpreadSheetContainerChanges);
begin
  if ccAnchors in AChanges then
  begin
    if not Calculator.IsAutoSizing then
      TextBox.AutoSize := False;
  end;
  inherited Changed(AChanges);
end;

procedure TdxSpreadSheetCustomTextBoxContainer.CheckAutoSize;
begin
  if TextBox.AutoSize then
    Calculator.ApplyAutoSize;
end;

procedure TdxSpreadSheetCustomTextBoxContainer.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FTextBox := CreateTextBox;
  FTextBox.OnChange := TextBoxChangeHandler;
end;

function TdxSpreadSheetCustomTextBoxContainer.CreateTextBox: TdxSpreadSheetCustomTextBox;
begin
  Result := TdxSpreadSheetTextBox.Create(SpreadSheet);
end;

function TdxSpreadSheetCustomTextBoxContainer.CreateViewInfo: TdxSpreadSheetContainerViewInfo;
begin
  Result := TdxSpreadSheetCustomTextBoxContainerViewInfo.Create(Self);
end;

procedure TdxSpreadSheetCustomTextBoxContainer.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxSpreadSheetCustomTextBoxContainer then
    TextBox := TdxSpreadSheetCustomTextBoxContainer(Source).TextBox;
end;

procedure TdxSpreadSheetCustomTextBoxContainer.DoChanged(AChanges: TdxSpreadSheetContainerChanges);
begin
  if ccContent in AChanges then
    CheckAutoSize;
  inherited DoChanged(AChanges);
end;

procedure TdxSpreadSheetCustomTextBoxContainer.LoadFromStream(AReader: TcxReader; AVersion: Word; ACanReadContent: Boolean = True);
begin
  inherited LoadFromStream(AReader, AVersion, ACanReadContent);
  TextBox.LoadFromStream(AReader, AVersion, ACanReadContent);
end;

procedure TdxSpreadSheetCustomTextBoxContainer.SaveToStream(AWriter: TcxWriter; ACanSaveContent: Boolean = True);
begin
  inherited SaveToStream(AWriter, ACanSaveContent);
  TextBox.SaveToStream(AWriter, ACanSaveContent);
end;

function TdxSpreadSheetCustomTextBoxContainer.GetCalculator: TdxSpreadSheetCustomTextBoxContainerCalculator;
begin
  Result := TdxSpreadSheetCustomTextBoxContainerCalculator(inherited Calculator);
end;

procedure TdxSpreadSheetCustomTextBoxContainer.TextBoxChangeHandler(Sender: TObject);
begin
  Changed([ccContent]);
end;

procedure TdxSpreadSheetCustomTextBoxContainer.SetTextBox(AValue: TdxSpreadSheetCustomTextBox);
begin
  FTextBox.Assign(AValue);
end;

{ TdxSpreadSheetCustomTextBoxContainerCalculator }

procedure TdxSpreadSheetCustomTextBoxContainerCalculator.ApplyAutoSize;
var
  ABounds: TRect;
begin
  BeginAutoSize;
  try
    ABounds := CalculateBounds;
    ABounds := cxRectSetSize(ABounds, CalculateAutoSize(cxRectWidth(ABounds)));
    ABounds := CheckForContentArea(ABounds);
    if AnchorType = catTwoCell then
      AnchorType := catOneCell;
    UpdateAnchors(ABounds);
  finally
    EndAutoSize;
  end;
end;

procedure TdxSpreadSheetCustomTextBoxContainerCalculator.UpdateAnchors(const ABounds: TRect);

  function CheckAutoSize(const ABounds: TRect): Boolean;
  begin
    Result := KeepAutoSize(CalculateBounds, ABounds);
    if Result then
    begin
      BeginAutoSize;
      try
        UpdateAnchors(CheckForContentArea(cxRectCenter(ABounds, CalculateAutoSize(cxRectWidth(ABounds)))));
      finally
        EndAutoSize;
      end;
    end
    else
      TextBox.AutoSize := False;
  end;

begin
  if IsAutoSizing or not (TextBox.AutoSize and CheckAutoSize(ABounds)) then
    inherited UpdateAnchors(ABounds);
end;

procedure TdxSpreadSheetCustomTextBoxContainerCalculator.BeginAutoSize;
begin
  Inc(FAutoSizeLockCount);
end;

function TdxSpreadSheetCustomTextBoxContainerCalculator.CalculateAutoSize(AWidth: Integer): TSize;
var
  AViewInfo: TdxSpreadSheetCustomTextBoxContainerViewInfo;
begin
  AViewInfo := TdxSpreadSheetContainerAccess(Owner).CreateViewInfo as TdxSpreadSheetCustomTextBoxContainerViewInfo;
  try
    AViewInfo.Calculate;
    Dec(AWidth, cxMarginsWidth(TextBox.ContentOffsets));
    Result := cxSize(AViewInfo.TextBoxViewInfo.MeasureSize(AWidth).Ceil);
    Inc(Result.cx, cxMarginsWidth(TextBox.ContentOffsets));
    Inc(Result.cy, cxMarginsHeight(TextBox.ContentOffsets));
  finally
    AViewInfo.Free;
  end;
end;

procedure TdxSpreadSheetCustomTextBoxContainerCalculator.EndAutoSize;
begin
  Dec(FAutoSizeLockCount);
end;

function TdxSpreadSheetCustomTextBoxContainerCalculator.KeepAutoSize(const APrevBounds, ANewBounds: TRect): Boolean;
begin
  Result := cxSizeIsEqual(APrevBounds, ANewBounds);
end;

function TdxSpreadSheetCustomTextBoxContainerCalculator.IsAutoSizing: Boolean;
begin
  Result := FAutoSizeLockCount > 0;
end;

{ TdxSpreadSheetCustomTextBoxContainerViewInfo }

constructor TdxSpreadSheetCustomTextBoxContainerViewInfo.Create(AContainer: TdxSpreadSheetContainer);
begin
  inherited Create(AContainer);
  FTextBoxViewInfo := CreateTextBoxViewInfo;
end;

destructor TdxSpreadSheetCustomTextBoxContainerViewInfo.Destroy;
begin
  FreeAndNil(FTextBoxViewInfo);
  inherited Destroy;
end;

procedure TdxSpreadSheetCustomTextBoxContainerViewInfo.ContentBoundsChanged;
var
  ATextBoxBounds: TdxRectF;
  ATextSize: TdxSizeF;
begin
  inherited ContentBoundsChanged;

  ATextBoxBounds := cxRectContent(ContentBounds, dxRectF(TextBox.ContentOffsets));
  if not (TextBox.WordWrap or TextBox.AutoSize) then
  begin
    ATextSize := TextBoxViewInfo.MeasureSize(ATextBoxBounds.Width);
    if ATextSize.cx > ATextBoxBounds.Width then
      ATextBoxBounds := cxRectCenterHorizontally(ATextBoxBounds, ATextSize.cx);
  end;
  TextBoxViewInfo.Bounds := ATextBoxBounds;
end;

function TdxSpreadSheetCustomTextBoxContainerViewInfo.CreateTextBoxViewInfo: TdxSpreadSheetTextBoxViewInfo;
begin
  Result := TdxSpreadSheetTextBoxViewInfo.Create(TextBox);
end;

procedure TdxSpreadSheetCustomTextBoxContainerViewInfo.DoCalculate;
begin
  TextBoxViewInfo.Calculate;
  inherited DoCalculate;
end;

procedure TdxSpreadSheetCustomTextBoxContainerViewInfo.DrawContent(ACanvas: TdxGPCanvas);
begin
  inherited DrawContent(ACanvas);

  if not TextBox.IsEmpty then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.SetClipPath(ShapePath, gmIntersect);
      if FlipHorz and not FlipVert then
      begin
        ACanvas.SaveWorldTransform;
        try
          ACanvas.FlipWorldTransform(FlipHorz, False, cxRectCenter(ContentBounds));
          TextBoxViewInfo.Draw(ACanvas);
        finally
          ACanvas.RestoreWorldTransform;
        end;
      end
      else
        TextBoxViewInfo.Draw(ACanvas);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

function TdxSpreadSheetCustomTextBoxContainerViewInfo.GetOwner: TdxSpreadSheetCustomTextBoxContainer;
begin
  Result := TdxSpreadSheetCustomTextBoxContainer(inherited Owner);
end;

function TdxSpreadSheetCustomTextBoxContainerViewInfo.GetTextBox: TdxSpreadSheetCustomTextBox;
begin
  Result := Owner.TextBox;
end;

{ TdxSpreadSheetTextBoxContainer }

function TdxSpreadSheetTextBoxContainer.CreateCalculator: TdxSpreadSheetContainerCalculator;
begin
  Result := TdxSpreadSheetTextBoxContainerCalculator.Create(Self);
end;

function TdxSpreadSheetTextBoxContainer.CreateTextBox: TdxSpreadSheetCustomTextBox;
begin
  Result := TdxSpreadSheetTextBox.Create(SpreadSheet);
end;

function TdxSpreadSheetTextBoxContainer.GetTextBox: TdxSpreadSheetTextBox;
begin
  Result := TdxSpreadSheetTextBox(inherited TextBox);
end;

procedure TdxSpreadSheetTextBoxContainer.SetTextBox(AValue: TdxSpreadSheetTextBox);
begin
  TextBox.Assign(AValue);
end;

{ TdxSpreadSheetTextBoxContainerCalculator }

function TdxSpreadSheetTextBoxContainerCalculator.KeepAutoSize(const APrevBounds, ANewBounds: TRect): Boolean;
begin
  Result := inherited KeepAutoSize(APrevBounds, ANewBounds);
  if TextBox.WordWrap then
    Result := Result or (cxRectWidth(APrevBounds) <> cxRectWidth(ANewBounds));
end;

function TdxSpreadSheetTextBoxContainerCalculator.GetTextBox: TdxSpreadSheetCustomTextBox;
begin
  Result := TdxSpreadSheetTextBoxContainer(Owner).TextBox;
end;

{ TdxSpreadSheetCommentTextBox }

constructor TdxSpreadSheetCommentTextBox.Create(ASpreadSheet: TdxCustomSpreadSheet);
begin
  inherited Create(ASpreadSheet);
  WordWrap := True;
end;

{ TdxSpreadSheetCommentTextBoxViewInfo }

function TdxSpreadSheetCommentTextBoxViewInfo.MeasureSize(const AWidth: Double): TdxSizeF;
begin
  dxGPPaintCanvas.BeginPaint(cxScreenCanvas.Handle, cxSimpleRect);
  try
    Result := dxGPPaintCanvas.MeasureString(TextBox.TextAsString, Font);
  finally
    dxGPPaintCanvas.EndPaint;
    cxScreenCanvas.Dormant;
  end;
end;

{ TdxSpreadSheetCommentContainer }

function TdxSpreadSheetCommentContainer.CanClone: Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetCommentContainer.CanFocusViaKeyboard: Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetCommentContainer.CreateCalculator: TdxSpreadSheetContainerCalculator;
begin
  Result := TdxSpreadSheetCommentContainerCalculator.Create(Self);
end;

procedure TdxSpreadSheetCommentContainer.CreateSubClasses;
begin
  inherited CreateSubClasses;
  Author := dxGetUserNameEx(dxUserNameDisplay);
  TextBox.TextAsString := Author + ':' + dxCRLF;
  Shape.Brush.Color := TdxAlphaColors.InfoBk;
  Shape.Pen.Brush.Color := TdxAlphaColors.WindowText;
end;

function TdxSpreadSheetCommentContainer.CreateTextBox: TdxSpreadSheetCustomTextBox;
begin
  Result := TdxSpreadSheetCommentTextBox.Create(SpreadSheet);
end;

function TdxSpreadSheetCommentContainer.CreateViewInfo: TdxSpreadSheetContainerViewInfo;
begin
  Result := TdxSpreadSheetCommentContainerViewInfo.Create(Self);
end;

procedure TdxSpreadSheetCommentContainer.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxSpreadSheetCommentContainer then
  begin
    Cell := TdxSpreadSheetCommentContainer(Source).Cell;
    Author := TdxSpreadSheetCommentContainer(Source).Author;
  end;
end;

procedure TdxSpreadSheetCommentContainer.DoSetParent(AValue: TdxSpreadSheetCustomView);
var
  ASavedCellPos: TPoint;
begin
  if Cell <> nil then
    ASavedCellPos := Point(Cell.ColumnIndex, Cell.RowIndex)
  else
    ASavedCellPos := cxInvalidPoint;

  Cell := nil;

  inherited DoSetParent(AValue);

  Cell := TdxSpreadSheetCellHelper.GetCell(ASavedCellPos.Y, ASavedCellPos.X, Parent);
end;

function TdxSpreadSheetCommentContainer.IsTransformsSupported: Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetCommentContainer.CellRemoving(ACell: TdxSpreadSheetCell);
begin
  if ACell = Cell then
    Free
  else
    inherited CellRemoving(ACell);
end;

function TdxSpreadSheetCommentContainer.IsCellUsed(ACell: TdxSpreadSheetCell): Boolean;
begin
  Result := (ACell = Cell) or inherited IsCellUsed(ACell);
end;

procedure TdxSpreadSheetCommentContainer.LoadFromStream(AReader: TcxReader; AVersion: Word; ACanReadContent: Boolean = True);
begin
  LoadFromStream(AReader, AVersion, 0, 0, ACanReadContent);
end;

procedure TdxSpreadSheetCommentContainer.LoadFromStream(AReader: TcxReader;
  AVersion: Word; AAnchorRow, AAnchorColumn: Integer; ACanReadContent: Boolean = True);
begin
  inherited LoadFromStream(AReader, AVersion, ACanReadContent);
  Cell := TdxSpreadSheetCellHelper.ReadRef(AReader, Parent, AAnchorRow, AAnchorColumn);
  Author := AReader.ReadWideString;
end;

procedure TdxSpreadSheetCommentContainer.SaveToStream(AWriter: TcxWriter; ACanSaveContent: Boolean = True);
begin
  SaveToStream(AWriter, 0, 0, ACanSaveContent);
end;

procedure TdxSpreadSheetCommentContainer.SaveToStream(AWriter: TcxWriter;
  AAnchorRow, AAnchorColumn: Integer; ACanSaveContent: Boolean = True);
begin
  inherited SaveToStream(AWriter, ACanSaveContent);
  TdxSpreadSheetCellHelper.WriteRef(AWriter, Cell, AAnchorRow, AAnchorColumn);
  AWriter.WriteWideString(Author);
end;

function TdxSpreadSheetCommentContainer.GetCalculator: TdxSpreadSheetCommentContainerCalculator;
begin
  Result := TdxSpreadSheetCommentContainerCalculator(inherited Calculator);
end;

function TdxSpreadSheetCommentContainer.GetTextBox: TdxSpreadSheetCommentTextBox;
begin
  Result := TdxSpreadSheetCommentTextBox(inherited TextBox);
end;

procedure TdxSpreadSheetCommentContainer.SetCell(AValue: TdxSpreadSheetCell);
var
  AContainer: TdxSpreadSheetContainer;
begin
  if Cell <> AValue then
  begin
    if AValue <> nil then
    begin
      if AValue.View <> Parent then
        raise EdxSpreadSheetError.CreateFmt(cxGetResourceString(@sdxErrorInvalidAnchorCell), [TdxSpreadSheetCellHelper.EncodeRefAsString(AValue)]);
      if (Parent <> nil) and Parent.Containers.FindCommentContainer(AValue, AContainer) then
        raise EdxSpreadSheetError.CreateFmt(cxGetResourceString(@sdxErrorCommentAlreadyExists), [TdxSpreadSheetCellHelper.EncodeRefAsString(AValue)]);
    end;

    if Parent <> nil then
    begin
      TdxSpreadSheetContainersAccess(Parent.Containers).UnregisterCommentContainer(Self);
      FCell := AValue;
      TdxSpreadSheetContainersAccess(Parent.Containers).RegisterCommentContainer(Self);
    end
    else
      FCell := AValue;

    Changed([ccContent]);
  end;
end;

procedure TdxSpreadSheetCommentContainer.SetTextBox(AValue: TdxSpreadSheetCommentTextBox);
begin
  TextBox.Assign(AValue);
end;

{ TdxSpreadSheetCommentContainerCalculator }

function TdxSpreadSheetCommentContainerCalculator.GetTextBox: TdxSpreadSheetCustomTextBox;
begin
  Result := TdxSpreadSheetCommentContainer(Owner).TextBox;
end;

{ TdxSpreadSheetCommentContainerConnectionViewInfo }

procedure TdxSpreadSheetCommentContainerConnectionViewInfo.AdjustAbsoluteBounds;
begin
  FBounds := cxRect(
    Min(LinePoint1.X, LinePoint2.X), Min(LinePoint1.Y, LinePoint2.Y),
    Max(LinePoint1.X, LinePoint2.X), Max(LinePoint1.Y, LinePoint2.Y));
  FBounds := cxRectInflate(FBounds, ArrowSize);
end;

procedure TdxSpreadSheetCommentContainerConnectionViewInfo.CalculateArrow;

  function CalculateArrowPoint(const AAngle: Double): TdxPointF;
  begin
    Result := dxPointF(LinePoint1.X + ArrowSize * Sin(AAngle), LinePoint1.Y + ArrowSize * Cos(AAngle));
  end;

var
  AAngle: Double;
begin
  AAngle := Pi / 2 - ArcTan2(LinePoint2.Y - LinePoint1.Y, LinePoint2.X - LinePoint1.X);
  FArrowPoints[0] := CalculateArrowPoint(AAngle - ArrowAngle * Pi / 180);
  FArrowPoints[1] := dxPointF(LinePoint1);
  FArrowPoints[2] := CalculateArrowPoint(AAngle + ArrowAngle * Pi / 180);
end;

procedure TdxSpreadSheetCommentContainerConnectionViewInfo.CalculateBounds;
begin
  CalculateLinePoints;
  AdjustAbsoluteBounds;
  inherited CalculateBounds;
  if Visible then
    CalculateArrow;
end;

procedure TdxSpreadSheetCommentContainerConnectionViewInfo.CalculateLinePoints;
var
  ACell: TdxSpreadSheetCell;
  ACellBounds: TRect;
  AContainerBounds: TRect;
begin
  ACell := ContainerViewInfo.Owner.Cell;
  ACellBounds := TdxSpreadSheetCommentContainerHelper.GetCellBounds(ACell);
  AContainerBounds := cxRect(ContainerViewInfo.ContentBounds);

  LinePoint1 := Point(ACellBounds.Right, ACellBounds.Top);
  if AContainerBounds.Left >= ACellBounds.Right then
    LinePoint2 := AContainerBounds.TopLeft
  else
    if AContainerBounds.Bottom < ACellBounds.Top  then
      LinePoint2 := Point(AContainerBounds.Right, AContainerBounds.Bottom)
    else
      LinePoint2 := Point(AContainerBounds.Right, AContainerBounds.Top);

  if cxRectPtIn(AContainerBounds, LinePoint1) and cxRectPtIn(AContainerBounds, LinePoint2) then
    LinePoint2 := LinePoint1;
end;

procedure TdxSpreadSheetCommentContainerConnectionViewInfo.CheckBounds;
var
  APrevBounds: TRect;
begin
  APrevBounds := Bounds;
  CalculateBounds;
  if not cxRectIsEqual(APrevBounds, Bounds) then
    InvalidateRect(cxRectUnion(APrevBounds, Bounds));
end;

procedure TdxSpreadSheetCommentContainerConnectionViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, Bounds);
  try
    dxSpreadSheetSetupGpCanvas(dxGPPaintCanvas, ContainerViewInfo.OptionsView.Antialiasing);
    dxGPPaintCanvas.Line(LinePoint1.X, LinePoint1.Y, LinePoint2.X, LinePoint2.Y, Color);
    dxGPPaintCanvas.Polygon(FArrowPoints, TdxAlphaColors.Transparent, Color, 1, gppsSolid);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

function TdxSpreadSheetCommentContainerConnectionViewInfo.GetColor: TdxAlphaColor;
begin
  Result := ContainerViewInfo.Shape.Pen.Brush.Color;
  if (Result = TdxAlphaColors.Transparent) or (Result = TdxAlphaColors.Default) then
    Result := TdxAlphaColors.WindowText;
end;

function TdxSpreadSheetCommentContainerConnectionViewInfo.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := ContainerViewInfo.SpreadSheet;
end;

function TdxSpreadSheetCommentContainerConnectionViewInfo.InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean;
begin
  Result := cxPointInLine(AHitTest.ActualHitPoint, FLinePoint1, FLinePoint2);
  if Result then
    AHitTest.HitObject := Self;
end;

procedure TdxSpreadSheetCommentContainerConnectionViewInfo.InvalidateRect(const R: TRect);
begin
  ContainerViewInfo.InvalidateRect(R);
end;

function TdxSpreadSheetCommentContainerConnectionViewInfo.GetContainerViewInfo: TdxSpreadSheetCommentContainerViewInfo;
begin
  Result := TdxSpreadSheetCommentContainerViewInfo(Owner);
end;

{ TdxSpreadSheetCommentContainerHelper }

class function TdxSpreadSheetCommentContainerHelper.GetAbsoluteCellBounds(ACell: TdxSpreadSheetCell): TRect;
begin
  if ACell <> nil then
    Result := TdxSpreadSheetTableViewAccess(ACell.View).GetAbsoluteCellBounds(ACell.RowIndex, ACell.ColumnIndex)
  else
    Result := cxNullRect;
end;

class function TdxSpreadSheetCommentContainerHelper.GetCellBounds(ACell: TdxSpreadSheetCell): TRect;
var
  AView: TdxSpreadSheetTableViewAccess;
begin
  if ACell <> nil then
  begin
    AView := TdxSpreadSheetTableViewAccess(ACell.View);
    Result := AView.GetAbsoluteCellBounds(ACell.RowIndex, ACell.ColumnIndex);
    Result := cxRectOffset(Result, AView.GetContentOrigin, False);
  end
  else
    Result := cxNullRect;
end;

class function TdxSpreadSheetCommentContainerHelper.GetDefaultPosition(ACell: TdxSpreadSheetCell): TRect;
begin
  Result := GetAbsoluteCellBounds(ACell);
  Result := cxRectBounds(Result.Right + 15, Max(0, Result.Top - 10), 130, 75);
end;

{ TdxSpreadSheetCommentContainerViewInfo }

procedure TdxSpreadSheetCommentContainerViewInfo.ContentBoundsChanged;
begin
  inherited ContentBoundsChanged;
  if ConnectionViewInfo <> nil then
    ConnectionViewInfo.CheckBounds;
end;

function TdxSpreadSheetCommentContainerViewInfo.CreateTextBoxViewInfo: TdxSpreadSheetTextBoxViewInfo;
begin
  Result := TdxSpreadSheetCommentTextBoxViewInfo.Create(TextBox);
end;

function TdxSpreadSheetCommentContainerViewInfo.DoCreateShapePath(const R: TdxRectF): TdxGPPath;
begin
  Result := TdxGPPath.Create;
  Result.AddRect(R);
end;

function TdxSpreadSheetCommentContainerViewInfo.GetOwner: TdxSpreadSheetCommentContainer;
begin
  Result := TdxSpreadSheetCommentContainer(inherited Owner)
end;

{ TdxSpreadSheetContainerCustomMoveDragAndDropObject }

procedure TdxSpreadSheetContainerCustomMoveDragAndDropObject.AlignToAxes(var R: TRect);
var
  P1, P2: TPoint;
begin
  P1 := cxRectCenter(R);
  P2 := cxRectCenter(Bounds);
  if Abs(cxPointDistance(cxPoint(P2.X, P1.Y), P2)) > Abs(cxPointDistance(cxPoint(P1.X, P2.Y), P2)) then
    R := cxRectOffset(Bounds, 0, P1.Y - P2.Y)
  else
    R := cxRectOffset(Bounds, P1.X - P2.X, 0);
end;

procedure TdxSpreadSheetContainerCustomMoveDragAndDropObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  ViewInfo.Alpha := dxSpreadSheetMovingContainerAlpha;
end;

procedure TdxSpreadSheetContainerCustomMoveDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
var
  APrevRealDrawingBounds: TRect;
  ARect: TRect;
begin
  Accepted := True;
  APrevRealDrawingBounds := ViewInfo.RealDrawingBounds;
  ARect := cxRectOffset(Bounds, P.X - CapturePoint.X, P.Y - CapturePoint.Y);
  if IsAltPressed then
    AlignToCells(ARect);
  if IsShiftPressed then
    AlignToAxes(ARect);
  ViewInfo.SetBounds(ToContentRect(CheckForContentArea(ARect)), ViewInfo.ScreenClipRect);
  ViewInfo.Calculate;
  InvalidateRects(APrevRealDrawingBounds, ViewInfo.RealDrawingBounds);
  inherited DragAndDrop(P, Accepted);
end;

function TdxSpreadSheetContainerCustomMoveDragAndDropObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
begin
  if Accepted then
    Result := crSizeAll
  else
    Result := inherited GetDragAndDropCursor(Accepted);
end;

{ TdxSpreadSheetClonedContainerMoveDragAndDropObject }

constructor TdxSpreadSheetClonedContainerMoveDragAndDropObject.Create(AContainer: TdxSpreadSheetContainer);
begin
  FContainer := AContainer;
  inherited Create(FContainer.SpreadSheet);
end;

procedure TdxSpreadSheetClonedContainerMoveDragAndDropObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  ViewInfo.Alpha := dxSpreadSheetMovingClonedContainerAlpha;
end;

function TdxSpreadSheetClonedContainerMoveDragAndDropObject.GetContainerViewInfo: TdxSpreadSheetContainerViewInfo;
begin
  Result := Containers.Items[Containers.FindItem(FContainer)] as TdxSpreadSheetContainerViewInfo;
end;

function TdxSpreadSheetClonedContainerMoveDragAndDropObject.TranslateCoords(const P: TPoint): TPoint;
begin
  Result := P;
end;

function TdxSpreadSheetClonedContainerMoveDragAndDropObject.GetContainers: TdxSpreadSheetCellViewInfoList;
begin
  Result := TdxSpreadSheetCustomViewAccess(FContainer.Parent).ViewInfo.Containers;
end;

{ TdxSpreadSheetContainerMoveDragAndDropObject }

function TdxSpreadSheetContainerMoveDragAndDropObject.CreateDragAndDropObjectForClonedContainer: TdxSpreadSheetClonedContainerMoveDragAndDropObject;
begin
  Result := TdxSpreadSheetClonedContainerMoveDragAndDropObject.Create(ClonedContainer);
end;

procedure TdxSpreadSheetContainerMoveDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  TdxSpreadSheetTableViewControllerAccess(TdxSpreadSheetTableView(View).Controller).CanClickOnTheCell := False;
  MoveClone := IsControlPressed and TdxSpreadSheetContainerAccess(Container).CanClone;
  if ClonedContainerDragAndDropObject <> nil then
    ClonedContainerDragAndDropObject.DoDragAndDrop(P, Accepted)
  else
    inherited DragAndDrop(P, Accepted);
end;

procedure TdxSpreadSheetContainerMoveDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
begin
  if ClonedContainerDragAndDropObject <> nil then
  begin
    if Accepted then
    begin
      ClonedContainerDragAndDropObject.DoEndDragAndDrop(Accepted);
      FreeAndNil(FClonedContainerDragAndDropObject);
      ClonedContainer.Focused := True;
      inherited EndDragAndDrop(Accepted);
      Exit;
    end
    else
      MoveClone := False;
  end;
  inherited EndDragAndDrop(Accepted);
end;

function TdxSpreadSheetContainerMoveDragAndDropObject.GetMoveClone: Boolean;
begin
  Result := ClonedContainer <> nil;
end;

procedure TdxSpreadSheetContainerMoveDragAndDropObject.SetMoveClone(AValue: Boolean);
var
  AAccepted: Boolean;
begin
  if MoveClone <> AValue then
  begin
    if AValue then
    begin
      FClonedContainer := TdxSpreadSheetContainerClass(Container.ClassType).Create(View);
      FClonedContainer.Assign(Container);
      DragAndDrop(CapturePoint, AAccepted);
      FClonedContainerDragAndDropObject := CreateDragAndDropObjectForClonedContainer;
      FClonedContainerDragAndDropObject.DoBeginDragAndDrop;
      FClonedContainerDragAndDropObject.FCapturePoint := CapturePoint;
    end
    else
    begin
      FClonedContainerDragAndDropObject.DoEndDragAndDrop(False);
      FreeAndNil(FClonedContainerDragAndDropObject);
      FreeAndNil(FClonedContainer);
    end;
  end;
end;

{ TdxSpreadSheetContainerResizeDragAndDropObject }

procedure TdxSpreadSheetContainerResizeDragAndDropObject.AlignToCells(var ADeltaX, ADeltaY: Single);
var
  dX, dY: Integer;
  R: TdxRectF;
begin
  dX := MaxInt;
  dY := MaxInt;

  R := CalculateContentBounds(ADeltaX, ADeltaY);
  case Marker of
    smTop, smTopRight:
      AlignToCell(dX, dY, cxPoint(R.TopRight, False));
    smLeft, smTopLeft:
      AlignToCell(dX, dY, cxPoint(R.TopLeft, False));
    smBottom, smBottomLeft:
      AlignToCell(dX, dY, cxPoint(R.BottomLeft, False));
    smRight, smBottomRight:
      AlignToCell(dX, dY, cxPoint(R.BottomRight, False));
  end;

  if Marker in [smLeft, smTopLeft, smBottomLeft, smRight, smTopRight, smBottomRight] then
    ADeltaX := ADeltaX + IfThen(dX <> MaxInt, dX);
  if Marker in [smTop, smTopLeft, smTopRight, smBottom, smBottomLeft, smBottomRight] then
    ADeltaY := ADeltaY + IfThen(dY <> MaxInt, dY);
end;

function TdxSpreadSheetContainerResizeDragAndDropObject.CalculateContentBounds(const ADeltaX, ADeltaY: Single): TdxRectF;
begin
  Result := dxRectF(Bounds);
  if Marker in [smRight, smTopRight, smBottomRight] then
    Result.Right := Result.Right + ADeltaX;
  if Marker in [smTop, smTopLeft, smTopRight] then
    Result.Top := Result.Top + ADeltaY;
  if Marker in [smLeft, smTopLeft, smBottomLeft] then
    Result.Left := Result.Left + ADeltaX;
  if Marker in [smBottom, smBottomLeft, smBottomRight] then
    Result.Bottom := Result.Bottom + ADeltaY;

  if IsControlPressed then
  begin
    if Marker in [smRight, smTopRight, smBottomRight] then
      Result.Left := Result.Left - ADeltaX;
    if Marker in [smTop, smTopLeft, smTopRight] then
      Result.Bottom := Result.Bottom - ADeltaY;
    if Marker in [smLeft, smTopLeft, smBottomLeft] then
      Result.Right := Result.Right - ADeltaX;
    if Marker in [smBottom, smBottomLeft, smBottomRight] then
      Result.Top := Result.Top - ADeltaY;
  end;
end;

procedure TdxSpreadSheetContainerResizeDragAndDropObject.CheckAspectUsingCornerHandles(var ADeltaX, ADeltaY: Single);
var
  ARatio: Single;
  ARect: TdxRectF;
  ASign: Integer;
begin
  if (Bounds.Width = 0) or (Bounds.Height = 0) then
    Exit;

  ARatio := Bounds.Height / Bounds.Width;
  ASign := IfThen(Marker in [smBottomLeft, smTopRight], -1, 1);
  ARect := CalculateContentBounds(ADeltaX, ASign * ADeltaX * ARatio);

  case Marker of
    smTopLeft, smTopRight:
      ARect.Top := ARect.Top + CapturePoint.Y - ToAbsoluteRect(ViewInfo.Bounds).Top;
    smBottomLeft, smBottomRight:
      ARect.Bottom := ARect.Bottom + CapturePoint.Y - ToAbsoluteRect(ViewInfo.Bounds).Bottom;
  end;

  if (CapturePoint.Y + ADeltaY < ARect.Top) or (CapturePoint.Y + ADeltaY > ARect.Bottom) then
    ADeltaX := ASign * ADeltaY / ARatio
  else
    ADeltaY := ASign * ADeltaX * ARatio;
end;

procedure TdxSpreadSheetContainerResizeDragAndDropObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  FMarker := TdxSpreadSheetSizingMarker(HitTest.HitObjectData);
  FCapturePoint := ViewInfo.TransformMatrixInv.TransformPoint(CapturePoint);
  ViewInfo.Alpha := dxSpreadSheetResizingContainerAlpha;
  ViewInfo.IsDragging := True;
end;

procedure TdxSpreadSheetContainerResizeDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
var
  ADeltaX: Single;
  ADeltaY: Single;
  APrevRealDrawingBounds: TRect;
  ARect: TdxRectF;
  P1: TdxPointF;
begin
  Accepted := True;
  APrevRealDrawingBounds := ViewInfo.RealDrawingBounds;
  P1 := ViewInfo.TransformMatrixInv.TransformPoint(dxPointF(P));

  ADeltaX := P1.X - FCapturePoint.X;
  ADeltaY := P1.Y - FCapturePoint.Y;

  if IsAltPressed then
    AlignToCells(ADeltaX, ADeltaY);

  if TdxSpreadSheetContainerAccess(Container).KeepAspectUsingCornerHandles then
  begin
    if Marker in [smTopLeft, smTopRight, smBottomRight, smBottomLeft] then
      CheckAspectUsingCornerHandles(ADeltaX, ADeltaY);
  end;

  ARect := CalculateContentBounds(ADeltaX, ADeltaY);
  if TdxSpreadSheetContainerAccess(Container).IsTransformsSupported then
  begin
    ViewInfo.FlipHorz := Transform.FlipHorizontally;
    ViewInfo.FlipVert := Transform.FlipVertically;
    if ARect.Left > ARect.Right then
      ViewInfo.FlipHorz := not ViewInfo.FlipHorz;
    if ARect.Top > ARect.Bottom then
      ViewInfo.FlipVert := not ViewInfo.FlipVert;
  end;
  ViewInfo.ContentBounds := cxRectAdjustF(cxRectOffset(ARect, dxPointF(ContentOrigin), False));

  InvalidateRects(APrevRealDrawingBounds, ViewInfo.RealDrawingBounds);
  inherited DragAndDrop(P, Accepted);
end;

procedure TdxSpreadSheetContainerResizeDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
begin
  Control.BeginUpdate;
  Container.BeginUpdate;
  try
    History.BeginAction(TdxSpreadSheetHistoryChangeContainerAction);
    if Accepted then
    begin
      History.AddCommand(TdxSpreadSheetHistoryChangeContainerCommand.Create(Container));
      RecalculateBounds;
      Transform.FlipHorizontally := ViewInfo.FlipHorz;
      Transform.FlipVertically := ViewInfo.FlipVert;
    end;
    inherited EndDragAndDrop(Accepted);
  finally
    History.EndAction;
    Container.EndUpdate;
    Control.EndUpdate;
  end;
end;

function TdxSpreadSheetContainerResizeDragAndDropObject.GetContainerViewInfo: TdxSpreadSheetContainerViewInfo;
begin
  Result := (HitTest.HitObject as TdxSpreadSheetContainerSelectionCellViewInfo).ContainerViewInfo;
end;

procedure TdxSpreadSheetContainerResizeDragAndDropObject.RecalculateBounds;
var
  AFixedPoint: TdxPointF;
  AMatrix: TdxGPMatrix;
  APivotPoint: TdxPointF;
  AResizedPoint: TdxPointF;
begin
  GetPoints(AFixedPoint, AResizedPoint);

  AFixedPoint := ViewInfo.TransformMatrix.TransformPoint(AFixedPoint);
  AResizedPoint := ViewInfo.TransformMatrix.TransformPoint(AResizedPoint);

  AResizedPoint := cxPointOffset(AResizedPoint, dxPointF(ContentOrigin));
  AFixedPoint := cxPointOffset(AFixedPoint, dxPointF(ContentOrigin));

  AMatrix := TdxGPMatrix.Create;
  try
    APivotPoint := dxPointF((AResizedPoint.X + AFixedPoint.X) / 2, (AResizedPoint.Y + AFixedPoint.Y) / 2);

    AMatrix.Rotate(Transform.RotationAngle, APivotPoint);
    AMatrix.Invert;

    AResizedPoint := AMatrix.TransformPoint(AResizedPoint);
    AFixedPoint := AMatrix.TransformPoint(AFixedPoint);
  finally
    AMatrix.Free;
  end;

  ApplyBounds(AFixedPoint, AResizedPoint);
end;

procedure TdxSpreadSheetContainerResizeDragAndDropObject.ApplyBounds(const AFixedPoint, AResizedPoint: TdxPointF);
var
  R: TdxRectF;
begin
  case Marker of
    smTopLeft, smTop:
      R := dxRectF(AResizedPoint.X, AResizedPoint.Y, AFixedPoint.X, AFixedPoint.Y);
    smTopRight, smRight:
      R := dxRectF(AFixedPoint.X, AResizedPoint.Y, AResizedPoint.X, AFixedPoint.Y);
    smBottomRight, smBottom:
      R := dxRectF(AFixedPoint.X, AFixedPoint.Y, AResizedPoint.X, AResizedPoint.Y);
  else
    R := dxRectF(AResizedPoint.X, AFixedPoint.Y, AFixedPoint.X, AResizedPoint.Y);
  end;
  ViewInfo.SetBounds(ToContentRect(CheckForContentArea(cxRect(R, False))));
end;

procedure TdxSpreadSheetContainerResizeDragAndDropObject.GetPoints(out AFixedPoint, AResizedPoint: TdxPointF);
begin
  case Marker of
    smTopLeft, smTop:
      begin
        AResizedPoint := ViewInfo.ContentBounds.TopLeft;
        AFixedPoint := ViewInfo.ContentBounds.BottomRight;
      end;

    smTopRight, smRight:
      begin
        AResizedPoint := ViewInfo.ContentBounds.TopRight;
        AFixedPoint := ViewInfo.ContentBounds.BottomLeft;
      end;

    smBottomRight, smBottom:
      begin
        AResizedPoint := ViewInfo.ContentBounds.BottomRight;
        AFixedPoint := ViewInfo.ContentBounds.TopLeft;
      end;

  else
    begin
      AResizedPoint := ViewInfo.ContentBounds.BottomLeft;
      AFixedPoint := ViewInfo.ContentBounds.TopRight;
    end;
  end;
end;

function TdxSpreadSheetContainerResizeDragAndDropObject.GetTransform: TdxSpreadSheetContainerTransform;
begin
  Result := Container.Transform;
end;

{ TdxSpreadSheetContainerRotateDragAndDropObject }

procedure TdxSpreadSheetContainerRotateDragAndDropObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  FCapturePoint := cxPointOffset(FCapturePoint, ContentOrigin, False);
  FCapturePoint := ViewInfo.TransformMatrixInv.TransformPoint(CapturePoint);
  FCapturePoint := cxPointOffset(FCapturePoint, ContentOrigin);

  ViewInfo.Alpha := dxSpreadSheetRotationContainerAlpha;
end;

procedure TdxSpreadSheetContainerRotateDragAndDropObject.CheckScrollArea(const P: TPoint);
begin
  // do nothing
end;

procedure TdxSpreadSheetContainerRotateDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
const
  AlignAngle = 15;
var
  AAngle: Double;
  ACosValue: Double;
  ALength: Double;
  APrevRealDrawingBounds: TRect;
  AVector1: TPoint;
  AVector2: TPoint;
begin
  Accepted := True;
  AVector1 := cxPointOffset(CapturePoint, PivotPoint, False);
  AVector2 := cxPointOffset(P, PivotPoint, False);

  ALength := Sqrt(Sqr(AVector1.X) + Sqr(AVector1.Y)) * Sqrt(Sqr(AVector2.X) + Sqr(AVector2.Y));
  if not IsZero(ALength) then
  begin
    ACosValue := (AVector1.X * AVector2.X + AVector1.Y * AVector2.Y) / ALength;
    if SameValue(ACosValue, 1) then
      AAngle := 0
    else
      AAngle := 180 / Pi * ArcCos(ACosValue) * ValueIncr[P.X >= CapturePoint.X];

    if IsShiftPressed then
      AAngle := AlignAngle * Round(AAngle / AlignAngle);

    APrevRealDrawingBounds := ViewInfo.RealDrawingBounds;
    ViewInfo.RotationAngle := AAngle;
    InvalidateRects(APrevRealDrawingBounds, ViewInfo.RealDrawingBounds);
  end;
  inherited DragAndDrop(P, Accepted);
end;

procedure TdxSpreadSheetContainerRotateDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
begin
  if Accepted and not SameValue(ViewInfo.Transform.RotationAngle, ViewInfo.RotationAngle) then
  begin
    History.BeginAction(TdxSpreadSheetHistoryChangeContainerAction);
    try
      History.AddCommand(TdxSpreadSheetHistoryChangeContainerCommand.Create(Container));
      ViewInfo.Transform.RotationAngle := ViewInfo.RotationAngle;
      ViewInfo.SetBounds(ToContentRect(CheckForContentArea(Bounds)));
      inherited EndDragAndDrop(Accepted);
    finally
      History.EndAction;
    end;
  end
  else
    inherited EndDragAndDrop(Accepted);
end;

function TdxSpreadSheetContainerRotateDragAndDropObject.GetContainerViewInfo: TdxSpreadSheetContainerViewInfo;
begin
  Result := (HitTest.HitObject as TdxSpreadSheetContainerSelectionCellViewInfo).ContainerViewInfo;
end;

function TdxSpreadSheetContainerRotateDragAndDropObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
begin
  if Accepted then
    Result := crdxSpreadSheetRotation
  else
    Result := inherited GetDragAndDropCursor(Accepted);
end;

initialization
  RegisterClass(TdxSpreadSheetCommentContainer);
  RegisterClass(TdxSpreadSheetPictureContainer);
  RegisterClass(TdxSpreadSheetShapeContainer);
  RegisterClass(TdxSpreadSheetTextBoxContainer);

finalization
  UnregisterClass(TdxSpreadSheetCommentContainer);
  UnregisterClass(TdxSpreadSheetPictureContainer);
  UnregisterClass(TdxSpreadSheetShapeContainer);
  UnregisterClass(TdxSpreadSheetTextBoxContainer);
end.
