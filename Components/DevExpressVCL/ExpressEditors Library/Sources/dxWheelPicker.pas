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

unit dxWheelPicker;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, StdCtrls, Graphics, Controls, Messages, Forms, Buttons,
  dxCore, dxCoreClasses, cxClasses, cxGraphics, dxTouch, dxGDIPlusClasses, cxLookAndFeels, cxLookAndFeelPainters,
  cxControls, cxContainer, ImgList, cxEdit, dxAnimation, dxGallery, cxGeometry;

const
  dxWheelPickerItemContentMargin: Integer = 2;

type
  TdxWheelPicker = class;
  TdxCustomWheelPickerProperties = class;
  TdxWheelPickerProperties = class;
  TdxWheelPickerWheel = class;
  TdxWheelPickerWheels = class;
  TdxWheelPickerItem = class;
  TdxWheelPickerItems = class;
  TdxWheelPickerViewInfo = class;
  TdxCustomWheelPickerViewInfo = class;
  TdxCustomWheelPickerWheelViewInfo = class;
  TdxCustomWheelPickerItemViewInfo = class;
  TdxCustomWheelPickerItemViewInfoClass = class of TdxCustomWheelPickerItemViewInfo;
  TdxCustomWheelPickerWheelViewInfoClass = class of TdxCustomWheelPickerWheelViewInfo;

  TdxWheelPickerItemIndexes = array of Integer;

  { TdxWheelPickerWheelFadingHelper }

  TdxWheelPickerWheelFadingHelper = class(TcxCustomEditFadingHelper)
  private
    FWheelViewInfo: TdxCustomWheelPickerWheelViewInfo;
  protected
    procedure DrawItem(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); virtual;
    function GetEditViewInfo: TcxCustomEditViewInfo; override;
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap); override;
    function GetBounds: TRect; virtual;
  public
    constructor Create(AWheelViewInfo: TdxCustomWheelPickerWheelViewInfo); virtual;
    procedure Invalidate; override;

    property Bounds: TRect read GetBounds;
    property WheelViewInfo: TdxCustomWheelPickerWheelViewInfo read FWheelViewInfo;
  end;

  { TdxCustomWheelPickerPartViewInfo }

  TdxCustomWheelPickerPartViewInfo = class
  strict private
    FBounds: TRect;
    FWheelPickerViewInfo: TdxCustomWheelPickerViewInfo;

    function GetScaleFactor: TdxScaleFactor;
  protected
    FIndex: Integer;

    procedure Calculate(const ABounds: TRect); virtual;
    procedure Draw(ACanvas: TcxCanvas); virtual;
    function GetHitTest(const APoint: TPoint): TdxCustomWheelPickerPartViewInfo; virtual;
    function GetRowHeight: Integer; virtual;
    function GetPainter: TcxCustomLookAndFeelPainter; virtual;
    procedure Offset(DX, DY: Integer); virtual;
    procedure SetPressedState; virtual;
  public
    constructor Create(AWheelPickerViewInfo: TdxCustomWheelPickerViewInfo); virtual;

    property Bounds: TRect read FBounds;
    property RowHeight: Integer read GetRowHeight;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property WheelPickerViewInfo: TdxCustomWheelPickerViewInfo read FWheelPickerViewInfo;
  end;

  { TdxCustomWheelPickerItemViewInfo }

  TdxCustomWheelPickerItemViewInfo = class(TdxCustomWheelPickerPartViewInfo)
  private
    FBitmap: TcxBitmap32;
    FContentBounds: TRect;
    FContentRect: TRect;
    FWheelViewInfo: TdxCustomWheelPickerWheelViewInfo;
  protected
    FIsRecalculate: Boolean;

    procedure Calculate(const ABounds: TRect); override;
    procedure Draw(ACanvas: TcxCanvas); override;
    procedure Offset(DX: Integer; DY: Integer); override;
    procedure SetPressedState; override;

    procedure DoDraw(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState); virtual;
    procedure DrawContent(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState); virtual;
    function GetState: TcxButtonState; virtual;
    function IsItemSelected: Boolean; virtual;
    procedure Recalculate; virtual;

    property Bitmap: TcxBitmap32 read FBitmap;
    property ContentBounds: TRect read FContentBounds;
    property ContentRect: TRect read FContentRect;
    property WheelViewInfo: TdxCustomWheelPickerWheelViewInfo read FWheelViewInfo;
  public
    constructor Create(AWheelPickerViewInfo: TdxCustomWheelPickerViewInfo); override;
    destructor Destroy; override;
  end;

  { TdxCustomWheelPickerWheelViewInfoAnimationHelper }

  TdxCustomWheelPickerWheelViewInfoAnimationHelper = class
  private
    FAnimation: TdxAnimationTransition;
    FAnimationOffset: Integer;
    FAnimationStep: Integer;
    FWheelViewInfo: TdxCustomWheelPickerWheelViewInfo;
  protected
    procedure DoAnimation(Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean); virtual;
    procedure DoAnimationTerminate(Sender: TObject); virtual;
    function GetStepCount: Integer; virtual;
    function GetTime: Integer; virtual;
    procedure StartAnimation(ADelta: Integer);
    procedure StopAnimation;

    property AnimationOffset: Integer read FAnimationOffset;
  public
    constructor Create(AWheelViewInfo: TdxCustomWheelPickerWheelViewInfo); virtual;
    destructor Destroy; override;
  end;

  { TdxCustomWheelPickerWheelViewInfo }

  TdxCustomWheelPickerWheelViewInfo = class(TdxCustomWheelPickerPartViewInfo)
  private
    FAnimationHelper: TdxCustomWheelPickerWheelViewInfoAnimationHelper;
    FFadingHelper: TdxWheelPickerWheelFadingHelper;
    FItemBoundsRect: TRect;
    FItems: TcxObjectList;
    FItemIndex: Integer;
  protected
    FCentralRowRect: TRect;
    FCyclic: Boolean;
    FEditButtonViewInfo: TcxEditButtonViewInfo;
    FEnabled: Boolean;
    FWheelItemCount: Integer;
    FMovingOffset: Integer;

    procedure Calculate(const ABounds: TRect); override;
    procedure Draw(ACanvas: TcxCanvas); override;
    procedure Offset(DX, DY: Integer); override;
    procedure SetPressedState; override;

    procedure AddItem(const ABounds: TRect; AWheelItemIndex: Integer); virtual;
    procedure CalculateItems; virtual;
    procedure ChangeSelectedState; virtual;
    procedure DrawItemBackground(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState); virtual;
    function GetAnimationOffset: Integer; virtual;
    function GetItem(AIndex: Integer): TdxCustomWheelPickerItemViewInfo;
    function GetItemCount: Integer;
    function GetHitTest(const APoint: TPoint): TdxCustomWheelPickerPartViewInfo; override;
    function GetCentralRowRect: TRect; virtual;
    function GetState: TcxButtonState; virtual;
    class function GetWheelPickerItemViewInfoClass: TdxCustomWheelPickerItemViewInfoClass; virtual;
    procedure InitializeItem(AItem: TdxCustomWheelPickerItemViewInfo; AIndex: Integer); virtual;
    procedure Invalidate;
    procedure KeyDown(AKey: Word); virtual;
    procedure MouseWheel(ADelta: Integer); virtual;
    procedure RefreshItems; virtual;
    procedure Recalculate;
    procedure SetItemIndex(AValue: Integer);
    procedure StartAnimation(ADelta: Integer); virtual;
    procedure StopAnimation; virtual;

    function IsAnimating: Boolean;
    function IsDragged: Boolean; virtual;
    function IsSelected: Boolean; virtual;

    procedure BeginMoving;
    procedure EndMoving;
    procedure Moving(ADelta: Integer);

    property AnimationOffset: Integer read GetAnimationOffset;
    property FadingHelper: TdxWheelPickerWheelFadingHelper read FFadingHelper;
    property ItemBoundsRect: TRect read FItemBoundsRect;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TdxCustomWheelPickerItemViewInfo read GetItem;
  public
    constructor Create(AWheelPickerViewInfo: TdxCustomWheelPickerViewInfo); override;
    destructor Destroy; override;
  end;

  { TdxCustomWheelPickerViewInfo }

  TdxCustomWheelPickerViewInfo = class(TcxCustomEditViewInfo)
  private
    FDraggedWheel: TdxCustomWheelPickerWheelViewInfo;
    FPressedWheel: TdxCustomWheelPickerWheelViewInfo;
    FSelectedWheel: TdxCustomWheelPickerWheelViewInfo;

    function AddWheel: TdxCustomWheelPickerWheelViewInfo;
    function GetWheel(AIndex: Integer): TdxCustomWheelPickerWheelViewInfo;
    procedure SetWheelCount(ACount: Integer);

    procedure SetDraggedWheel(AValue: TdxCustomWheelPickerWheelViewInfo);
    procedure SetPressedWheel(AValue: TdxCustomWheelPickerWheelViewInfo);
    procedure SetSelectedWheel(AValue: TdxCustomWheelPickerWheelViewInfo);
  protected
    FIsDirty: Boolean;
    FRowHeight: Integer;
    FIsNullEditValue: Boolean;
    FPressedItemViewInfo: TdxCustomWheelPickerItemViewInfo;
    FUseRightToLeftReading: Boolean;
    FWheels: TcxObjectList;

    // Inherited
    procedure InternalPaint(ACanvas: TcxCanvas); override;

    // Dragging
    procedure BeginDragging;
    procedure BeginGestureScroll(APos: TPoint);
    procedure EndDragging;
    procedure EndGestureScroll;

    // Misc
    procedure SelectNextWheel(AIncrement: Integer); virtual;
    function GetHitTest(const APoint: TPoint): TdxCustomWheelPickerPartViewInfo; virtual;
    class function GetWheelPickerWheelViewInfoClass: TdxCustomWheelPickerWheelViewInfoClass; virtual;

    // Input
    procedure FocusChanged(AFocused: Boolean); virtual;
    procedure KeyDown(var AKey: Word; AShift: TShiftState); virtual;
    procedure MouseClick(APoint: TPoint); virtual;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint); virtual;

    // InternalValue
    function GetInternalValue: TcxEditValue; virtual;
    procedure SetInternalValue(AValue: Variant); virtual;
    procedure SynchronizeWheelIndexes; virtual;

    procedure SetPressedElements(const APoint: TPoint); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TObject); override;
    procedure Offset(DX: Integer; DY: Integer); override;

    property DraggedWheel: TdxCustomWheelPickerWheelViewInfo read FDraggedWheel write SetDraggedWheel;
    property PressedWheel: TdxCustomWheelPickerWheelViewInfo read FPressedWheel write SetPressedWheel;
    property SelectedWheel: TdxCustomWheelPickerWheelViewInfo read FSelectedWheel write SetSelectedWheel;
    property UseRightToLeftReading: Boolean read FUseRightToLeftReading;
  end;

  { TdxCustomWheelPickerViewData }

  TdxCustomWheelPickerViewData = class(TcxCustomEditViewData)
  private
    function GetProperties: TdxCustomWheelPickerProperties;
  protected
    procedure CalculateRowHeight(AViewInfo: TdxCustomWheelPickerViewInfo); virtual;
    procedure CalculateTextSize(AViewInfo: TdxCustomWheelPickerViewInfo); virtual;
    procedure CalculateWheels(AViewInfo: TdxCustomWheelPickerViewInfo); virtual;
    function GetOptimalRowHeight: Integer; virtual;
    function GetOptimalWidth: Integer; virtual;
    function GetWheelCount: Integer; virtual;
    function GetWheelWidth(AIndex: Integer): Integer; virtual;
    function GetWheelsHeight(AViewInfo: TdxCustomWheelPickerViewInfo): Integer; virtual;
    function GetWheelIndent: Integer; virtual;
    procedure InitializeWheel(AWheelViewInfo: TdxCustomWheelPickerWheelViewInfo; AIndex: Integer); virtual;
    function InternalGetEditConstantPartSize(ACanvas: TcxCanvas; AIsInplace: Boolean;
      AEditSizeProperties: TcxEditSizeProperties; var MinContentSize: TSize;
      AViewInfo: TcxCustomEditViewInfo): TSize; override;
    function IsWheelAutoSize(AIndex: Integer): Boolean; virtual;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint; Button: TcxMouseButton;
      Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean); override;
    procedure EditValueToDrawValue(const AEditValue: Variant; AViewInfo: TcxCustomEditViewInfo); override;

    property Properties: TdxCustomWheelPickerProperties read GetProperties;
  end;

  { TdxCustomWheelPickerProperties }

  TdxCustomWheelPickerProperties = class(TcxCustomEditProperties)
  private
    FCyclic: Boolean;
    FLineCount: Integer;

    procedure SetCyclic(AValue: Boolean);
    procedure SetLineCount(AValue: Integer);
  protected
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function HasDisplayValue: Boolean; override;
    procedure InternalPrepareValue(var AEditValue: TcxEditValue); virtual;

    property Cyclic: Boolean read FCyclic write SetCyclic;
  public
    function CanCompareEditValue: Boolean; override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetSpecialFeatures: TcxEditSpecialFeatures; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    function IsResetEditClass: Boolean; override;

    property LineCount: Integer read FLineCount write SetLineCount default 0;
  end;

  { TdxWheelPickerDragAndDropObject }

  TdxWheelPickerDragAndDropObject = class(TcxDragAndDropObject)
  protected
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
  end;

  { TdxCustomWheelPicker }

  TdxCustomWheelPicker = class(TcxCustomEdit)
  private
    FDraggingPosY: Integer;

    function GetActiveProperties: TdxCustomWheelPickerProperties;
    function GetProperties: TdxCustomWheelPickerProperties;
    function GetViewInfo: TdxCustomWheelPickerViewInfo;
    procedure SetProperties(AValue: TdxCustomWheelPickerProperties);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    // IdxGestureClient
    function AllowGesture(AGestureId: Integer): Boolean; override;
    function AllowPan(AScrollKind: TScrollBarKind): Boolean; override;
    procedure BeginGestureScroll(APos: TPoint); override;
    procedure EndGestureScroll; override;
    procedure GestureScroll(ADeltaX: Integer; ADeltaY: Integer); override;
    function GetPanOptions: Integer; override;
    function IsPanArea(const APoint: TPoint): Boolean; override;
    function NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean; override;

    // Inherited
    procedure BiDiModeChanged; override;
    function CanAutoWidth: Boolean; override;
    procedure ContainerStyleChanged(Sender: TObject); override;
    procedure DoFocusChanged; override;
    procedure Initialize; override;
    function InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function IsDoubleBufferedNeeded: Boolean; override;
    procedure Loaded; override;
    procedure PropertiesChanged(Sender: TObject); override;
    procedure SynchronizeDisplayValue; override;
    function WantNavigationKeys: Boolean; override;

    // Input
    function IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;

    // DragAndDrop
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function StartDragAndDrop(const P: TPoint): Boolean; override;

    // Internal
    procedure InternalSetSelectedValue(ASelectedValue: TcxEditValue);

    property ActiveProperties: TdxCustomWheelPickerProperties read GetActiveProperties;
    property AutoSize default False;
    property Properties: TdxCustomWheelPickerProperties read GetProperties write SetProperties;
    property ViewInfo: TdxCustomWheelPickerViewInfo read GetViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BeginDragAndDrop; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
  end;

  { TdxWheelPickerItemViewInfo }

  TdxWheelPickerItemViewInfo = class(TdxCustomWheelPickerItemViewInfo)
  strict private
    FImageRect: TRect;
    FImageSize: TSize;
    FTextFlags: Integer;
    FTextRect: TRect;
    FTextSize: TSize;

    procedure CalculateBottomAlignment(var ATextRect, AImageRect: TRect);
    procedure CalculateCenterHorizontallyAlignment(var ATextRect, AImageRect: TRect);
    procedure CalculateCenterVerticallyAlignment(var ATextRect, AImageRect: TRect);
    procedure CalculateLeftAlignment(var ATextRect, AImageRect: TRect);
    procedure CalculateRightAlignment(var ATextRect, AImageRect: TRect);
    procedure CalculateTopAlignment(var ATextRect, AImageRect: TRect);
    function GetWheelPickerViewInfo: TdxWheelPickerViewInfo;
  protected
    FGlyph: TdxSmartImage;
    FImageIndex: TcxImageIndex;
    FItemContentAlignHorz: TAlignment;
    FItemContentAlignVert: TcxAlignmentVert;
    FLayout: TButtonLayout;
    FText: string;

    procedure Calculate(const ABounds: TRect); override;
    procedure CalculateContentLayout(out ATextRect, AImageRect: TRect); virtual;
    procedure DrawContent(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState); override;
    procedure DrawGlyph(ACanvas: TcxCanvas; AState: TcxButtonState); virtual;
    procedure DrawText(ACanvas: TcxCanvas); virtual;
    function GetActualLayout: TButtonLayout;
    function GetImageSize: TSize; virtual;
    function GetTextFlags: Integer; virtual;
    function GetTextSize: TSize; virtual;
    function HasGlyph: Boolean; virtual;
    function HasText: Boolean; virtual;
    procedure InitializeByItem(AItem: TdxWheelPickerItem); virtual;

    property ActualLayout: TButtonLayout read GetActualLayout;
  public
    property WheelPickerViewInfo: TdxWheelPickerViewInfo read GetWheelPickerViewInfo;
  end;

  { TdxWheelPickerWheelViewInfo }

  TdxWheelPickerWheelViewInfo = class(TdxCustomWheelPickerWheelViewInfo)
  protected
    FWheel: TdxWheelPickerWheel;
    class function GetWheelPickerItemViewInfoClass: TdxCustomWheelPickerItemViewInfoClass; override;
    procedure InitializeItem(AItem: TdxCustomWheelPickerItemViewInfo; AIndex: Integer); override;
  end;

  { TdxWheelPickerViewInfo }

  TdxWheelPickerViewInfo = class(TdxCustomWheelPickerViewInfo)
  protected
    FImages: TCustomImageList;
    FItemIndexes: TdxWheelPickerItemIndexes; // InternalValue

    class function GetWheelPickerWheelViewInfoClass: TdxCustomWheelPickerWheelViewInfoClass; override;
    procedure SetInternalValue(AValue: Variant); override;
    procedure SynchronizeWheelIndexes; override;
  end;

  { TdxWheelPickerViewData }

  TdxWheelPickerViewData = class(TdxCustomWheelPickerViewData)
  private
    function GetProperties: TdxWheelPickerProperties;
    function CalculateMaxLineHight: Integer;
  protected
    procedure CalculateRowHeight(AViewInfo: TdxCustomWheelPickerViewInfo); override;
    procedure InitializeWheel(AWheelViewInfo: TdxCustomWheelPickerWheelViewInfo; AIndex: Integer); override;
    function GetOptimalRowHeight: Integer; override;
    function GetOptimalWidth: Integer; override;
    function GetWheelCount: Integer; override;
    function GetWheelWidth(AIndex: Integer): Integer; override;
    function IsWheelAutoSize(AIndex: Integer): Boolean; override;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint; Button: TcxMouseButton;
      Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean); override;
    property Properties: TdxWheelPickerProperties read GetProperties;
  end;

  { TdxWheelPickerItem }

  TdxWheelPickerItem = class(TcxInterfacedCollectionItem, IdxGalleryItem)
  private
    FGlyph: TdxSmartGlyph;
    FImageIndex: TcxImageIndex;
    FTag: TcxTag;
    FText: string;

    procedure SetGlyph(AValue: TdxSmartGlyph);
    procedure SetImageIndex(AValue: TcxImageIndex);
    procedure SetText(const AValue: string);

    // IdxGalleryItem
    function GetCaption: string;
    function GetIndex: Integer;
    function GetInstance: TObject;
    procedure SetCaption(const AValue: string);
    function GetGalleryItems: IdxGalleryItems;
    procedure SetGalleryItems(AItems: IdxGalleryItems);
    function IdxGalleryItem.GetItems = GetGalleryItems;
    procedure IdxGalleryItem.SetItems = SetGalleryItems;
  protected
    function GetCollection: TdxWheelPickerItems; virtual;
    procedure GlyphChangeHandler(Sender: TObject); virtual;

    property Collection: TdxWheelPickerItems read GetCollection;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Glyph: TdxSmartGlyph read FGlyph write SetGlyph;
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex default -1;
    property Tag: TcxTag read FTag write FTag default 0;
    property Text: string read FText write SetText;
  end;

  { TdxWheelPickerItems }

  TdxWheelPickerItems = class(TcxOwnedInterfacedCollection, IdxGalleryItems)
  private
    FAlignment: TcxAlignment;
    FLayout: TButtonLayout;

    procedure AlignmentChangeHandler(Sender: TObject);
    function GetItem(AIndex: Integer): TdxWheelPickerItem;
    procedure SetAlignment(AValue: TcxAlignment);
    procedure SetItem(Index: Integer; AValue: TdxWheelPickerItem);
    procedure SetLayout(AValue: TButtonLayout);

    // IdxGalleryItems
    function IdxGalleryItems.Add = AddGalleryItem;
    function IdxGalleryItems.GetCount = GetGalleryItemCount;
    function GetInstance: TObject;
    function IdxGalleryItems.GetItem = GetGalleryItem;
    function IdxGalleryItems.Insert = InsertGalleryItem;
    function AddGalleryItem: IdxGalleryItem;
    function GetGalleryItemCount: Integer;
    function GetGalleryItem(AIndex: Integer): IdxGalleryItem;
    function InsertGalleryItem(AIndex: Integer): IdxGalleryItem;
  protected
    procedure Update(Item: TCollectionItem); override;

    property Alignment: TcxAlignment read FAlignment write SetAlignment;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass); virtual;
    destructor Destroy; override;

    function Add: TdxWheelPickerItem;
    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TdxWheelPickerItem read GetItem write SetItem; default;
  published
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphTop;
  end;

  { TdxWheelPickerWheel }

  TdxWheelPickerWheel = class(TcxInterfacedCollectionItem, IdxGalleryGroup)
  strict private
    FItems: TdxWheelPickerItems;
    FCyclic: Boolean;
    FTag: TcxTag;
    FWidth: Integer;

    procedure SetItems(AValue: TdxWheelPickerItems);
    procedure SetCyclic(AValue: Boolean);
    procedure SetWidth(AValue: Integer);

    // IdxGalleryGroup
    function IdxGalleryGroup.GetCaption = GetGalleryGroupCaption;
    function IdxGalleryGroup.GetGroups = GetGalleryGroupCollection;
    function IdxGalleryGroup.GetIndex = GetGalleryGroupIndex;
    function GetInstance: TObject;
    function IdxGalleryGroup.GetItems = GetGalleryGroupItems;
    procedure IdxGalleryGroup.SetCaption = SetGalleryGroupCaption;
    function GetGalleryGroupCaption: string;
    function GetGalleryGroupCollection: IdxGalleryGroups;
    function GetGalleryGroupIndex: Integer;
    function GetGalleryGroupItems: IdxGalleryItems;
    procedure SetGalleryGroupCaption(const AValue: string);
  protected
    procedure Changed; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    function CreateItems: TdxWheelPickerItems; virtual;
    function GetCollection: TdxWheelPickerWheels; virtual;
    function GetItemCount: Integer; virtual;

    property Collection: TdxWheelPickerWheels read GetCollection;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property ItemCount: Integer read GetItemCount;
  published
    property Cyclic: Boolean read FCyclic write SetCyclic default False;
    property Items: TdxWheelPickerItems read FItems write SetItems;
    property Tag: TcxTag read FTag write FTag default 0;
    property Width: Integer read FWidth write SetWidth default 75;
  end;

  { TdxWheelPickerWheels }

  TdxWheelPickerWheels = class(TcxOwnedInterfacedCollection, IdxGalleryGroups)
  private
    function GetItem(AIndex: Integer): TdxWheelPickerWheel;
    procedure SetItem(Index: Integer; AValue: TdxWheelPickerWheel);

    // IdxGalleryGroups
    function IdxGalleryGroups.Add = AddGalleryGroup;
    function IdxGalleryGroups.GetCount = GetGalleryGroupCount;
    function IdxGalleryGroups.GetDisplayName = GetGalleryGroupDisplayName;
    function GetInstance: TObject;
    function IdxGalleryGroups.GetGroup = GetGalleryGroup;
    function IdxGalleryGroups.Insert = InsertGalleryGroup;
    function AddGalleryGroup: IdxGalleryGroup;
    function GetGalleryGroupCount: Integer;
    function GetGalleryGroupDisplayName: string;
    function GetGalleryGroup(AIndex: Integer): IdxGalleryGroup;
    function InsertGalleryGroup(AIndex: Integer): IdxGalleryGroup;
  protected
    procedure ChangeScale(M, D: Integer); virtual;
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TdxWheelPickerWheel;
    property Items[Index: Integer]: TdxWheelPickerWheel read GetItem write SetItem; default;
  end;

  { TdxWheelPickerProperties }

  TdxWheelPickerProperties = class(TdxCustomWheelPickerProperties)
  strict private
    FLineAutoHeight: Boolean;
    FLineHeight: Integer;
    FWheelAutoWidth: Boolean;
    FWheels: TdxWheelPickerWheels;

    procedure SetLineAutoHeight(AValue: Boolean);
    procedure SetLineHeight(AValue: Integer);
    procedure SetWheelAutoWidth(AValue: Boolean);
    procedure SetWheels(AValue: TdxWheelPickerWheels);
  protected
    function CalculateEditValueByItemIndexes(const AItemIndexes: TdxWheelPickerItemIndexes): TcxEditValue; virtual;
    procedure ChangeScale(M: Integer; D: Integer); override;
    function CreateWheels: TdxWheelPickerWheels; virtual;
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    function GetMaxContentItemHeight: Integer; virtual;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    procedure InternalPrepareValue(var AEditValue: TcxEditValue); override;
    procedure WheelsChanged; virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    class function GetContainerClass: TcxContainerClass; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    procedure PrepareDisplayValue(const AEditValue: Variant; var DisplayValue: Variant; AEditFocused: Boolean); override;
  published
    property ClearKey;
    property Images;
    property ImmediatePost;
    property LineAutoHeight: Boolean read FLineAutoHeight write SetLineAutoHeight default False;
    property LineCount;
    property LineHeight: Integer read FLineHeight write SetLineHeight default 20;
    property WheelAutoWidth: Boolean read FWheelAutoWidth write SetWheelAutoWidth default False;
    property Wheels: TdxWheelPickerWheels read FWheels write SetWheels;
    property OnChange;
    property OnEditValueChanged;
  end;

  { TdxWheelPicker }

  TdxWheelPicker = class(TdxCustomWheelPicker, IdxGallery)
  private
    FItemIndex: string;
    FItemIndexes: TdxWheelPickerItemIndexes;

    function GetActiveProperties: TdxWheelPickerProperties;
    function GetProperties: TdxWheelPickerProperties;
    procedure SetProperties(AValue: TdxWheelPickerProperties);
  protected
    function GetItemIndex(AIndex: Integer): Integer;
    procedure SetItemIndex(AIndex: Integer; AItemIndex: Integer);
    procedure SynchronizeEditValue; override;
    // IdxGallery
    function GetAllowedCustomizationActions: TdxGalleryCustomizationActions;
    function GetGroups: IdxGalleryGroups;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;

    property ActiveProperties: TdxWheelPickerProperties read GetActiveProperties;
    property ItemIndexes[AIndex: Integer]: Integer read GetItemIndex write SetItemIndex;
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property EditValue;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property ItemIndex: string read FItemIndex write FItemIndex stored False;
    property PopupMenu;
    property Properties: TdxWheelPickerProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnEditing;
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
  end;

implementation

uses
  SysUtils, Math, Variants, dxThemeConsts, cxDrawTextUtils;

const
  dxWheelPickerAnimationTime: Integer = 150;
  dxWheelPickerAnimationStepCount: Integer = 10;

  sdxWheelPickerWheelPrefix = 'Wheel';

function dxIncIndex(ACurrentIndex, AIncrement, AFirstIndex, ALastIndex: Integer; ACyclic: Boolean = False): Integer;

  function GetCount: Integer;
  begin
    Result := ALastIndex - AFirstIndex + 1;
  end;

  function GetNewIndex: Integer;
  begin
    Result := ACurrentIndex + AIncrement;
  end;

  function GetOffset: Integer;
  begin
    Result := GetNewIndex - AFirstIndex;
  end;

begin
  if not ACyclic then
    Result := Max(AFirstIndex, Min(ALastIndex, GetNewIndex))
  else
    Result := GetNewIndex;
    if (Result > ALastIndex) and (GetCount > 0) then
      Result := AFirstIndex + GetOffset mod GetCount
    else
      if (Result < AFirstIndex) and (GetCount > -1) then
        Result := Min(ALastIndex, ALastIndex + GetOffset mod GetCount + 1);
end;

{ TdxWheelPickerWheelFadingHelper }

constructor TdxWheelPickerWheelFadingHelper.Create(AWheelViewInfo: TdxCustomWheelPickerWheelViewInfo);
begin
  inherited Create;
  FWheelViewInfo := AWheelViewInfo;
end;

procedure TdxWheelPickerWheelFadingHelper.DrawItem(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  WheelViewInfo.DrawItemBackground(ACanvas, R, AState);
end;

function TdxWheelPickerWheelFadingHelper.GetEditViewInfo: TcxCustomEditViewInfo;
begin
  Result := WheelViewInfo.WheelPickerViewInfo;
end;

procedure TdxWheelPickerWheelFadingHelper.GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);

  function PrepareImage(AState: TcxButtonState): TcxBitmap32;
  begin
    Result := TcxBitmap32.CreateSize(WheelViewInfo.ItemBoundsRect, True);
    DrawItem(Result.cxCanvas, Result.ClientRect, AState);
  end;

begin
  AFadeOutImage := PrepareImage(cxbsNormal);
  AFadeInImage := PrepareImage(cxbsHot);
end;

procedure TdxWheelPickerWheelFadingHelper.Invalidate;
begin
  Invalidate(FWheelViewInfo.Bounds, False);
end;

function TdxWheelPickerWheelFadingHelper.GetBounds: TRect;
begin
  Result := WheelViewInfo.Bounds;
end;

{ TdxCustomWheelPickerPartViewInfo }

constructor TdxCustomWheelPickerPartViewInfo.Create(AWheelPickerViewInfo: TdxCustomWheelPickerViewInfo);
begin
  inherited Create;
  FWheelPickerViewInfo := AWheelPickerViewInfo;
end;

procedure TdxCustomWheelPickerPartViewInfo.Calculate(const ABounds: TRect);
begin
  FBounds := ABounds;
end;

procedure TdxCustomWheelPickerPartViewInfo.Draw(ACanvas: TcxCanvas);
begin
// do nothing
end;

function TdxCustomWheelPickerPartViewInfo.GetHitTest(const APoint: TPoint): TdxCustomWheelPickerPartViewInfo;
begin
  if cxRectPtIn(FBounds, APoint) then
    Result := Self
  else
    Result := nil;
end;

function TdxCustomWheelPickerPartViewInfo.GetRowHeight: Integer;
begin
  Result := WheelPickerViewInfo.FRowHeight;
end;

function TdxCustomWheelPickerPartViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := WheelPickerViewInfo.ScaleFactor;
end;

function TdxCustomWheelPickerPartViewInfo.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := WheelPickerViewInfo.Painter;
end;

procedure TdxCustomWheelPickerPartViewInfo.Offset(DX, DY: Integer);
begin
  FBounds := cxRectOffset(FBounds, DX, DY);
end;

procedure TdxCustomWheelPickerPartViewInfo.SetPressedState;
begin
// do nothing
end;

{ TdxCustomWheelPickerItemViewInfo }

constructor TdxCustomWheelPickerItemViewInfo.Create(AWheelPickerViewInfo: TdxCustomWheelPickerViewInfo);
begin
  inherited;
  FBitmap := TcxBitmap32.Create;
end;

destructor TdxCustomWheelPickerItemViewInfo.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TdxCustomWheelPickerItemViewInfo.Calculate(const ABounds: TRect);
var
  AMargin: Integer;
begin
  inherited Calculate(ABounds);
  Bitmap.Clear;
  AMargin := ScaleFactor.Apply(dxWheelPickerItemContentMargin);
  FContentRect := cxRectContent(Bounds, Rect(AMargin, AMargin, AMargin, AMargin));
  FContentBounds := cxRectSetNullOrigin(ContentRect);
  Bitmap.SetSize(ContentRect);
end;

procedure TdxCustomWheelPickerItemViewInfo.DoDraw(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState);
begin
  if IsItemSelected or not WheelViewInfo.FadingHelper.DrawImage(ACanvas.Handle, ABounds) then
    WheelViewInfo.DrawItemBackground(ACanvas, ABounds, AState);

  cxBitBlt(Bitmap.Canvas.Handle, ACanvas.Handle, ContentBounds, ContentRect.TopLeft, SRCCOPY);
  DrawContent(Bitmap.cxCanvas, FContentBounds, AState);
  cxBitBlt(ACanvas.Handle, Bitmap.Canvas.Handle, ContentRect, cxNullPoint, SRCCOPY);
end;

procedure TdxCustomWheelPickerItemViewInfo.Draw(ACanvas: TcxCanvas);
begin
  inherited;
  DoDraw(ACanvas, Bounds, GetState);
end;

procedure TdxCustomWheelPickerItemViewInfo.DrawContent(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState);
begin
// do nothing
end;

function TdxCustomWheelPickerItemViewInfo.GetState: TcxButtonState;
begin
  if IsItemSelected then
  begin
    if WheelViewInfo.IsSelected then
      Result := cxbsPressed
    else
      Result := cxbsHot;
  end
  else
    if WheelViewInfo.IsSelected then
      Result := cxbsHot
    else
      Result := cxbsNormal;

  if not WheelViewInfo.FEnabled then
    Result := cxbsDisabled;
end;

function TdxCustomWheelPickerItemViewInfo.IsItemSelected: Boolean;
begin
  Result := (Bounds.Top = WheelViewInfo.FCentralRowRect.Top) and not WheelViewInfo.IsDragged and
    not WheelViewInfo.IsAnimating;
end;

procedure TdxCustomWheelPickerItemViewInfo.Offset(DX, DY: Integer);
begin
  inherited;
  FContentRect := cxRectOffset(FContentRect, DX, DY);
end;

procedure TdxCustomWheelPickerItemViewInfo.Recalculate;
begin
  FIsRecalculate := True;
  Calculate(Bounds);
  FIsRecalculate := False;
end;

procedure TdxCustomWheelPickerItemViewInfo.SetPressedState;
begin
  inherited;
  WheelViewInfo.SetPressedState;
  if WheelPickerViewInfo.SelectedWheel = WheelViewInfo then
    WheelPickerViewInfo.FPressedItemViewInfo := Self;
end;

{ TdxCustomWheelPickerWheelViewInfoAnimationHelper }

constructor TdxCustomWheelPickerWheelViewInfoAnimationHelper.Create(AWheelViewInfo: TdxCustomWheelPickerWheelViewInfo);
begin
  inherited Create;
  FWheelViewInfo := AWheelViewInfo;
end;

destructor TdxCustomWheelPickerWheelViewInfoAnimationHelper.Destroy;
begin
  FreeAndNil(FAnimation);
  inherited;
end;

procedure TdxCustomWheelPickerWheelViewInfoAnimationHelper.DoAnimation(Sender: TdxAnimationTransition;
  var APosition: Integer; var AFinished: Boolean);
begin
  if APosition > 0 then
  begin
    if Abs(FAnimationOffset) < Abs(FAnimationStep) then
      FAnimationOffset := 0
    else
      FAnimationOffset := FAnimationOffset - FAnimationStep;
  end;
  FWheelViewInfo.RefreshItems;
end;

procedure TdxCustomWheelPickerWheelViewInfoAnimationHelper.DoAnimationTerminate(Sender: TObject);
begin
  FAnimation := nil;
  if FAnimationOffset <> 0 then
  begin
    FAnimationOffset := 0;
    FWheelViewInfo.RefreshItems;
  end;
end;

function TdxCustomWheelPickerWheelViewInfoAnimationHelper.GetStepCount: Integer;
begin
  Result := dxWheelPickerAnimationStepCount;
end;

function TdxCustomWheelPickerWheelViewInfoAnimationHelper.GetTime: Integer;
begin
  Result := dxWheelPickerAnimationTime;
end;

procedure TdxCustomWheelPickerWheelViewInfoAnimationHelper.StartAnimation(ADelta: Integer);
begin
  FAnimationOffset := FAnimationOffset + ADelta;
  FAnimationStep := FAnimationOffset div GetStepCount;
  if FAnimationStep > 0 then
    FAnimationStep := FAnimationStep + 1
  else
    if FAnimationStep < 0 then
      FAnimationStep := FAnimationStep - 1;
  FreeAndNil(FAnimation);
  FAnimation := TdxAnimationTransition.Create(GetTime, ateLinear, GetStepCount);
  FAnimation.FreeOnTerminate := True;
  FAnimation.OnTerminate := DoAnimationTerminate;
  FAnimation.OnAnimate := DoAnimation;
  FAnimation.Resume;
end;

procedure TdxCustomWheelPickerWheelViewInfoAnimationHelper.StopAnimation;
begin
  if FAnimation <> nil then
    FAnimation.Terminate;
end;

{ TdxCustomWheelPickerWheelViewInfo }

constructor TdxCustomWheelPickerWheelViewInfo.Create(AWheelPickerViewInfo: TdxCustomWheelPickerViewInfo);
begin
  inherited;
  FEditButtonViewInfo := TcxEditButtonViewInfo.Create;
  FItems := TcxObjectList.Create;
  FFadingHelper := TdxWheelPickerWheelFadingHelper.Create(Self);
  FAnimationHelper := TdxCustomWheelPickerWheelViewInfoAnimationHelper.Create(Self);
  FEnabled := True;
end;

destructor TdxCustomWheelPickerWheelViewInfo.Destroy;
begin
  FreeAndNil(FAnimationHelper);
  FreeAndNil(FFadingHelper);
  FreeAndNil(FItems);
  FreeAndNil(FEditButtonViewInfo);
  inherited;
end;

procedure TdxCustomWheelPickerWheelViewInfo.AddItem(const ABounds: TRect; AWheelItemIndex: Integer);
var
  AItem: TdxCustomWheelPickerItemViewInfo;
begin
  AItem := GetWheelPickerItemViewInfoClass.Create(WheelPickerViewInfo);
  FItems.Add(AItem);
  InitializeItem(AItem, AWheelItemIndex);
  AItem.Calculate(ABounds);
end;

procedure TdxCustomWheelPickerWheelViewInfo.BeginMoving;
begin
  RefreshItems;
end;

procedure TdxCustomWheelPickerWheelViewInfo.Calculate(const ABounds: TRect);
begin
  inherited;
  ItemIndex := Min(ItemIndex, FWheelItemCount - 1);
  FItemBoundsRect := cxRect(cxSize(cxRectWidth(Bounds), RowHeight));
  if (WheelPickerViewInfo.FPressedItemViewInfo <> nil) and not IsDragged then
    Recalculate
  else
    CalculateItems;
end;

procedure TdxCustomWheelPickerWheelViewInfo.CalculateItems;
var
  ATopItemIndex, ABottomItemIndex: Integer;
  ASelectedItemRect, ATopItemRect, AItemRect: TRect;
  AOffset: Integer;
  ATopRowCount, ABottomRowCount: Integer;
  AItemCount: Integer;
  I, AWheelItemIndex: Integer;
begin
  FItems.Clear;
  if (RowHeight <= 0) or (FWheelItemCount < 1) then
    Exit;
  FCentralRowRect := GetCentralRowRect;

  if IsDragged then
    AOffset := -FMovingOffset
  else
    AOffset := AnimationOffset;

  ASelectedItemRect := cxRectOffset(FCentralRowRect, 0, AOffset);
  ATopRowCount := (ASelectedItemRect.Top - Bounds.Top) div RowHeight;
  if Frac((ASelectedItemRect.Top - Bounds.Top) / RowHeight) > 0 then
    Inc(ATopRowCount);
  ABottomRowCount := (Bounds.Bottom - ASelectedItemRect.Bottom) div RowHeight;
  if Frac((Bounds.Bottom - ASelectedItemRect.Bottom) / RowHeight) > 0 then
    Inc(ABottomRowCount);

  ATopItemIndex := ItemIndex - ATopRowCount;
  ABottomItemIndex := ItemIndex + ABottomRowCount;
  if not FCyclic then
  begin
    ATopItemIndex := Max(0, ATopItemIndex);
    ABottomItemIndex := Min(FWheelItemCount - 1, ABottomItemIndex);
  end;
  ATopItemRect := cxRectOffset(ASelectedItemRect, 0, - (ItemIndex - ATopItemIndex) * RowHeight);
  AItemCount := ABottomItemIndex - ATopItemIndex + 1;
  AItemRect := ATopItemRect;
  for I := 0 to AItemCount - 1 do
  begin
    AWheelItemIndex := (I + ATopItemIndex) mod FWheelItemCount;
    if AWheelItemIndex < 0 then
      AWheelItemIndex := FWheelItemCount + AWheelItemIndex;
    AddItem(AItemRect, AWheelItemIndex);
    AItemRect := cxRectOffset(AItemRect, 0, RowHeight);
  end;
end;

procedure TdxCustomWheelPickerWheelViewInfo.ChangeSelectedState;
const
  AState: array[Boolean] of TcxButtonState = (cxbsNormal, cxbsHot);
begin
  if FFadingHelper.CanFade then
    FFadingHelper.CheckStartFading(AState[IsSelected], AState[not IsSelected])
  else
    Invalidate;
end;

procedure TdxCustomWheelPickerWheelViewInfo.Draw(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  inherited;
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(Bounds);
    for I := 0 to ItemCount - 1 do
      TdxCustomWheelPickerItemViewInfo(FItems[I]).Draw(ACanvas);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxCustomWheelPickerWheelViewInfo.DrawItemBackground(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState);
const
  ButtonStateToEditBtnState: array[TcxButtonState] of TcxEditButtonState = (ebsDisabled, ebsNormal, ebsSelected, ebsPressed, ebsDisabled);
var
  ABackgroundRect, AContentRect: TRect;
  AHandled: Boolean;
begin
  if AState in [cxbsHot, cxbsPressed] then
  begin
    if WheelPickerViewInfo.UseSkins then
      Painter.DrawWheelPickerItem(ACanvas, ABounds, AState)
    else
    begin
      FEditButtonViewInfo.Data.State := ButtonStateToEditBtnState[AState];
      FEditButtonViewInfo.EditViewInfo := WheelPickerViewInfo;
      FEditButtonViewInfo.Bounds := ABounds;
      ABackgroundRect := ABounds;
      AHandled := False;
      if Assigned(WheelPickerViewInfo.OnDrawButtonBackground) then
        WheelPickerViewInfo.OnDrawButtonBackground(FEditButtonViewInfo, ACanvas, ABackgroundRect, AHandled);
      if not AHandled then
        Painter.DrawWheelPickerItemBackground(ACanvas, ABounds, AState);
      AHandled := False;
      if Assigned(WheelPickerViewInfo.OnDrawButtonBorder) then
        WheelPickerViewInfo.OnDrawButtonBorder(FEditButtonViewInfo, ACanvas, ABackgroundRect, AContentRect, AHandled);
      if not AHandled then
        Painter.DrawWheelPickerItemBorder(ACanvas, ABounds, AState);
    end;
  end;
end;

procedure TdxCustomWheelPickerWheelViewInfo.EndMoving;

  function GetItemIndexAfterDragging: Integer;
  begin
    Result := ItemIndex + Round(FMovingOffset / RowHeight);
    if not FCyclic then
      Result := Max(0, Min(FWheelItemCount - 1, Result))
    else
    begin
      Result := Result mod FWheelItemCount;
      if Result < 0 then
        Result := FWheelItemCount + Result;
    end;
  end;

begin
  ItemIndex := GetItemIndexAfterDragging;
  FMovingOffset := 0;
  RefreshItems;
end;

function TdxCustomWheelPickerWheelViewInfo.GetAnimationOffset: Integer;
begin
  Result := -FAnimationHelper.AnimationOffset;
end;

function TdxCustomWheelPickerWheelViewInfo.GetItem(AIndex: Integer): TdxCustomWheelPickerItemViewInfo;
begin
  Result := TdxCustomWheelPickerItemViewInfo(FItems[AIndex]);
end;

function TdxCustomWheelPickerWheelViewInfo.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxCustomWheelPickerWheelViewInfo.GetHitTest(const APoint: TPoint): TdxCustomWheelPickerPartViewInfo;
var
  I: Integer;
  AViewInfo: TdxCustomWheelPickerPartViewInfo;
begin
  Result := inherited GetHitTest(APoint);
  if Result <> nil then
    for I := 0 to ItemCount - 1 do
    begin
      AViewInfo := Items[I].GetHitTest(APoint);
      if AViewInfo <> nil then
      begin
        Result := AViewInfo;
        Break;
      end;
    end
  else
    Result := nil;
end;

function TdxCustomWheelPickerWheelViewInfo.GetCentralRowRect: TRect;
begin
  Result := cxRectCenterVertically(Bounds, RowHeight);
end;

function TdxCustomWheelPickerWheelViewInfo.GetState: TcxButtonState;
begin
  if IsSelected then
    Result := cxbsHot
  else
    Result := cxbsNormal;
end;

class function TdxCustomWheelPickerWheelViewInfo.GetWheelPickerItemViewInfoClass: TdxCustomWheelPickerItemViewInfoClass;
begin
  Result := TdxCustomWheelPickerItemViewInfo;
end;

procedure TdxCustomWheelPickerWheelViewInfo.InitializeItem(AItem: TdxCustomWheelPickerItemViewInfo; AIndex: Integer);
begin
  AItem.FIndex := AIndex;
  AItem.FWheelViewInfo := Self;
end;

procedure TdxCustomWheelPickerWheelViewInfo.Invalidate;
begin
  if (WheelPickerViewInfo.Edit <> nil) and WheelPickerViewInfo.Edit.HandleAllocated then
    InvalidateRect(WheelPickerViewInfo.Edit.Handle, Bounds, True);
end;

function TdxCustomWheelPickerWheelViewInfo.IsAnimating: Boolean;
begin
  Result := AnimationOffset <> 0;
end;

function TdxCustomWheelPickerWheelViewInfo.IsDragged: Boolean;
begin
  Result := Self = WheelPickerViewInfo.DraggedWheel;
end;

function TdxCustomWheelPickerWheelViewInfo.IsSelected: Boolean;
begin
  Result := (((WheelPickerViewInfo.DraggedWheel = Self) or
    (WheelPickerViewInfo.PressedWheel = Self) or
    (WheelPickerViewInfo.SelectedWheel = Self))) and
    (WheelPickerViewInfo.Edit <> nil) and WheelPickerViewInfo.Edit.Focused;
end;

procedure TdxCustomWheelPickerWheelViewInfo.KeyDown(AKey: Word);

  procedure ChangeItemIndex(AIncrement: Integer);
  var
    ANewItemIndex: Integer;
  begin
    ANewItemIndex := dxIncIndex(ItemIndex, AIncrement, 0, FWheelItemCount - 1, FCyclic);
    if (ItemIndex <> ANewItemIndex) and (AnimationOffset = 0) then
    begin
      ItemIndex := ANewItemIndex;
      StartAnimation(-1 * Sign(AIncrement) * RowHeight);
    end;
  end;

begin
  case AKey of
    VK_HOME:
      ItemIndex := 0;
    VK_END:
      ItemIndex := FWheelItemCount - 1;
    VK_DOWN:
      ChangeItemIndex(1);
    VK_UP:
      ChangeItemIndex(-1);
  end
end;

procedure TdxCustomWheelPickerWheelViewInfo.MouseWheel(ADelta: Integer);
var
  ANewItemIndex: Integer;
begin
  ANewItemIndex := dxIncIndex(ItemIndex, -ADelta, 0, FWheelItemCount - 1, FCyclic);
  if (ANewItemIndex <> ItemIndex) or FCyclic then
  begin
    ItemIndex := ANewItemIndex;
    StartAnimation(ADelta * RowHeight);
  end;
end;

procedure TdxCustomWheelPickerWheelViewInfo.Moving(ADelta: Integer);
begin
  FMovingOffset := FMovingOffset + ADelta;
  if not FCyclic then
    FMovingOffset := Min(Max(FMovingOffset, -ItemIndex * RowHeight), (FWheelItemCount - ItemIndex - 1) * RowHeight);

  RefreshItems;
end;

procedure TdxCustomWheelPickerWheelViewInfo.Offset(DX, DY: Integer);
var
  I: Integer;
begin
  inherited;
  FCentralRowRect := cxRectOffset(FCentralRowRect, DX, DY);
  FItemBoundsRect := cxRectOffset(FItemBoundsRect, DX, DY);
  for I := 0 to ItemCount - 1 do
    Items[I].Offset(DX, DY);
end;

procedure TdxCustomWheelPickerWheelViewInfo.RefreshItems;
begin
  CalculateItems;
  Invalidate;
end;

procedure TdxCustomWheelPickerWheelViewInfo.Recalculate;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
    Items[I].Recalculate;
end;

procedure TdxCustomWheelPickerWheelViewInfo.SetPressedState;
begin
  inherited;
  if FEnabled then
    WheelPickerViewInfo.PressedWheel := Self
  else
    WheelPickerViewInfo.PressedWheel := nil;
end;

procedure TdxCustomWheelPickerWheelViewInfo.SetItemIndex(AValue: Integer);
begin
  if AValue <> FItemIndex then
  begin
    FItemIndex := Max(0, Min(FWheelItemCount - 1, AValue));
    RefreshItems;
  end;
end;

procedure TdxCustomWheelPickerWheelViewInfo.StartAnimation(ADelta: Integer);
begin
  if ADelta <> 0 then
    FAnimationHelper.StartAnimation(ADelta);
end;

procedure TdxCustomWheelPickerWheelViewInfo.StopAnimation;
begin
  FAnimationHelper.StopAnimation;
end;

{ TdxCustomWheelPickerViewInfo }

constructor TdxCustomWheelPickerViewInfo.Create;
begin
  inherited Create;
  FWheels := TcxObjectList.Create;
  FIsDirty := True;
end;

destructor TdxCustomWheelPickerViewInfo.Destroy;
begin
  FreeAndNil(FWheels);
  inherited;
end;

procedure TdxCustomWheelPickerViewInfo.Assign(Source: TObject);
begin
  inherited;
  if Source is TdxCustomWheelPickerViewInfo then
    FIsNullEditValue := TdxCustomWheelPickerViewInfo(Source).FIsNullEditValue;
end;

procedure TdxCustomWheelPickerViewInfo.Offset(DX, DY: Integer);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FWheels.Count - 1 do
   TdxCustomWheelPickerWheelViewInfo(FWheels[I]).Offset(DX, DY);
end;

procedure TdxCustomWheelPickerViewInfo.BeginDragging;
begin
  if PressedWheel <> nil then
  begin
    DraggedWheel := PressedWheel;
    DraggedWheel.FMovingOffset := -DraggedWheel.AnimationOffset;
    DraggedWheel.StopAnimation;
    DraggedWheel.BeginMoving;
    PressedWheel := nil;
  end;
end;

procedure TdxCustomWheelPickerViewInfo.BeginGestureScroll(APos: TPoint);
var
  AViewInfo: TdxCustomWheelPickerPartViewInfo;
begin
  AViewInfo := GetHitTest(APos);
  if AViewInfo is TdxCustomWheelPickerItemViewInfo then
    DraggedWheel := TdxCustomWheelPickerItemViewInfo(AViewInfo).WheelViewInfo
  else
    DraggedWheel := AViewInfo as TdxCustomWheelPickerWheelViewInfo;

  if DraggedWheel <> nil then
  begin
    SelectedWheel := nil;
    DraggedWheel.BeginMoving;
  end;
end;

procedure TdxCustomWheelPickerViewInfo.EndDragging;
begin
  DraggedWheel.EndMoving;
  SelectedWheel := DraggedWheel;
  DraggedWheel := nil;
end;

procedure TdxCustomWheelPickerViewInfo.EndGestureScroll;
begin
  if DraggedWheel <> nil then
  begin
    DraggedWheel.EndMoving;
    SelectedWheel := DraggedWheel;
    DraggedWheel := nil;
  end;
end;

procedure TdxCustomWheelPickerViewInfo.SelectNextWheel(AIncrement: Integer);

  function GetSelectedWheelIndex: Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to FWheels.Count - 1 do
      if SelectedWheel = FWheels[I] then
      begin
        Result := I;
        Break;
      end;
  end;

var
  I: Integer;
  ASelectedWheelIndex: Integer;
begin
  ASelectedWheelIndex := GetSelectedWheelIndex;
  SelectedWheel := nil;
  if FWheels.Count = 0 then
    Exit;

  if ASelectedWheelIndex = -1 then
  begin
    I := 0;
    AIncrement := 1;
  end
  else
    I := dxIncIndex(ASelectedWheelIndex, AIncrement, 0, FWheels.Count - 1, True);

  repeat
    if TdxCustomWheelPickerWheelViewInfo(FWheels[I]).FEnabled then
      SelectedWheel := TdxCustomWheelPickerWheelViewInfo(FWheels[I]);
    I := dxIncIndex(I, AIncrement, 0, FWheels.Count - 1, True);
  until (I = ASelectedWheelIndex) or (SelectedWheel <> nil);
end;

function TdxCustomWheelPickerViewInfo.GetHitTest(const APoint: TPoint): TdxCustomWheelPickerPartViewInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FWheels.Count - 1 do
  begin
    Result := TdxCustomWheelPickerWheelViewInfo(FWheels[I]).GetHitTest(APoint);
    if Result <> nil then
      Break;
  end
end;

class function TdxCustomWheelPickerViewInfo.GetWheelPickerWheelViewInfoClass: TdxCustomWheelPickerWheelViewInfoClass;
begin
  Result := TdxCustomWheelPickerWheelViewInfo;
end;

procedure TdxCustomWheelPickerViewInfo.InternalPaint(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  inherited;
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(ClientRect);
    DrawEditBackground(ACanvas, Bounds, cxNullRect, True);
    for I := 0 to FWheels.Count - 1 do
      TdxCustomWheelPickerWheelViewInfo(FWheels[I]).Draw(ACanvas);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxCustomWheelPickerViewInfo.FocusChanged(AFocused: Boolean);
var
  I: Integer;
begin
  if not AFocused then
  begin
    FSelectedWheel := nil;
    for I := 0 to FWheels.Count - 1 do
    begin
      if TdxCustomWheelPickerWheelViewInfo(FWheels[I]).IsAnimating then
        TdxCustomWheelPickerWheelViewInfo(FWheels[I]).StopAnimation;
      TdxCustomWheelPickerWheelViewInfo(FWheels[I]).Recalculate;
    end;
  end
  else
    if (SelectedWheel = nil) and (DraggedWheel = nil) and (PressedWheel = nil) and (GetMouseKeys = 0) then
      SelectNextWheel(1);
end;

procedure TdxCustomWheelPickerViewInfo.KeyDown(var AKey: Word; AShift: TShiftState);
begin
  if (PressedWheel = nil) and (DraggedWheel = nil) then
    case AKey of
      VK_LEFT:
        SelectNextWheel(-1);
      VK_RIGHT:
        SelectNextWheel(1);
    else
    begin
      if (SelectedWheel = nil) and not (AKey in [VK_UP, VK_DOWN]) then
        SelectedWheel := FWheels[0] as TdxCustomWheelPickerWheelViewInfo;
      if SelectedWheel <> nil then
        SelectedWheel.KeyDown(AKey);
    end;
  end;
end;

procedure TdxCustomWheelPickerViewInfo.MouseClick(APoint: TPoint);
var
  AAnimationDelta: Integer;
begin
  if (FPressedItemViewInfo <> nil) and (GetHitTest(APoint) = FPressedItemViewInfo) then
  begin
    if (SelectedWheel = PressedWheel) and FPressedItemViewInfo.IsItemSelected then
      SelectedWheel := nil
    else
      SelectedWheel := PressedWheel;
    AAnimationDelta := PressedWheel.FCentralRowRect.Top - FPressedItemViewInfo.Bounds.Top;
    FPressedItemViewInfo.WheelViewInfo.ItemIndex := FPressedItemViewInfo.FIndex;
    PressedWheel.StartAnimation(AAnimationDelta);
  end;
  if (FPressedItemViewInfo = nil) and (PressedWheel <> SelectedWheel) then
    SelectedWheel := PressedWheel;

  FPressedItemViewInfo := nil;
  PressedWheel := nil;
end;

procedure TdxCustomWheelPickerViewInfo.MouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint);
var
  APartViewInfo: TdxCustomWheelPickerPartViewInfo;
begin
  APartViewInfo := GetHitTest(MousePos);
  if APartViewInfo is TdxCustomWheelPickerWheelViewInfo then
    SelectedWheel := TdxCustomWheelPickerWheelViewInfo(APartViewInfo)
  else
    if APartViewInfo is TdxCustomWheelPickerItemViewInfo then
      SelectedWheel := TdxCustomWheelPickerItemViewInfo(APartViewInfo).WheelViewInfo;

  if (SelectedWheel <> nil) and (WheelDelta <> 0) then
    SelectedWheel.MouseWheel(Abs(WheelDelta) div WheelDelta);
end;

function TdxCustomWheelPickerViewInfo.GetInternalValue: TcxEditValue;
var
  ADisplayValue: TdxWheelPickerItemIndexes;
  I: Integer;
begin
  if FWheels.Count > 0 then
  begin
    SetLength(ADisplayValue, FWheels.Count);
    for I := 0 to FWheels.Count - 1 do
      ADisplayValue[I] := TdxCustomWheelPickerWheelViewInfo(FWheels[I]).ItemIndex;

    Result := ADisplayValue;
  end
  else
    Result := Null;
end;

procedure TdxCustomWheelPickerViewInfo.SetInternalValue(AValue: Variant);
begin
  if VarIsNull(AValue) <> FIsNullEditValue then
  begin
    FIsNullEditValue := VarIsNull(AValue);
    FIsDirty := True;
  end;
end;

procedure TdxCustomWheelPickerViewInfo.SynchronizeWheelIndexes;
begin
// do nothing
end;

procedure TdxCustomWheelPickerViewInfo.SetPressedElements(const APoint: TPoint);
var
  APartViewInfo: TdxCustomWheelPickerPartViewInfo;
begin
  APartViewInfo := GetHitTest(APoint);
  if APartViewInfo <> nil then
    APartViewInfo.SetPressedState
  else
    PressedWheel := nil;

  if PressedWheel <> SelectedWheel then
    SelectedWheel := nil;
end;

function TdxCustomWheelPickerViewInfo.AddWheel: TdxCustomWheelPickerWheelViewInfo;
begin
  Result := GetWheelPickerWheelViewInfoClass.Create(Self);
  FWheels.Add(Result);
end;

function TdxCustomWheelPickerViewInfo.GetWheel(AIndex: Integer): TdxCustomWheelPickerWheelViewInfo;
begin
  Result := TdxCustomWheelPickerWheelViewInfo(FWheels[AIndex]);
end;

procedure TdxCustomWheelPickerViewInfo.SetDraggedWheel(AValue: TdxCustomWheelPickerWheelViewInfo);
begin
  if DraggedWheel <> AValue then
  begin
    if (AValue <> nil) and (AValue <> SelectedWheel) and (AValue <> PressedWheel) then
      AValue.ChangeSelectedState;
    if (DraggedWheel <> nil) and (DraggedWheel <> SelectedWheel) and ((DraggedWheel <> PressedWheel)) then
      DraggedWheel.ChangeSelectedState;

    FDraggedWheel := AValue;
  end;
end;

procedure TdxCustomWheelPickerViewInfo.SetPressedWheel(AValue: TdxCustomWheelPickerWheelViewInfo);
begin
  if PressedWheel <> AValue then
  begin
    if (AValue <> nil) and (AValue <> SelectedWheel) and (AValue <> DraggedWheel) then
      AValue.ChangeSelectedState;
    if (PressedWheel <> nil) and (PressedWheel <> SelectedWheel) and ((PressedWheel <> DraggedWheel)) then
      PressedWheel.ChangeSelectedState;

    FPressedWheel := AValue;
  end;
end;

procedure TdxCustomWheelPickerViewInfo.SetSelectedWheel(AValue: TdxCustomWheelPickerWheelViewInfo);
begin
  if SelectedWheel <> AValue then
  begin
    if (AValue <> nil) and (AValue <> FPressedWheel) and (AValue <> DraggedWheel) then
      AValue.ChangeSelectedState;
    if (SelectedWheel <> nil) and (SelectedWheel <> FPressedWheel) and ((SelectedWheel <> DraggedWheel)) then
      SelectedWheel.ChangeSelectedState;

    FSelectedWheel := AValue;
  end;
end;

procedure TdxCustomWheelPickerViewInfo.SetWheelCount(ACount: Integer);
var
  I: Integer;
begin
  for I := FWheels.Count - 1 downto ACount do // if ACount < FWheels.Count then
    FWheels.FreeAndDelete(I);
  for I := FWheels.Count to ACount - 1 do     // if ACount > FWheels.Count then
    AddWheel;
end;

{ TdxCustomWheelPickerViewData }

procedure TdxCustomWheelPickerViewData.Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
  Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
var
  AWheelPickerViewInfo: TdxCustomWheelPickerViewInfo;
  APrevClientRect: TRect;
begin
  AWheelPickerViewInfo := AViewInfo as TdxCustomWheelPickerViewInfo;
  APrevClientRect := AViewInfo.ClientRect;
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, AViewInfo, AIsMouseEvent);
  if AWheelPickerViewInfo.FIsDirty or not cxRectIsEqual(APrevClientRect, AViewInfo.ClientRect) then
  begin
    CalculateTextSize(AWheelPickerViewInfo);
    CalculateRowHeight(AWheelPickerViewInfo);
    if not (((ABounds.Bottom >= cxMaxRectSize) or (ABounds.Right >= cxMaxRectSize)) and IsInplace) then
      CalculateWheels(AWheelPickerViewInfo);
  end;
  (AViewInfo as TdxCustomWheelPickerViewInfo).SynchronizeWheelIndexes;
  AWheelPickerViewInfo.FIsDirty := False;
  AWheelPickerViewInfo.FUseRightToLeftReading := UseRightToLeftReading;
end;

function TdxWheelPickerViewData.CalculateMaxLineHight: Integer;
var
  AMaxImageHeight: Integer;
  AMaxTextHeight: Integer;
  AWheel: TdxWheelPickerWheel;
  AWheelItem: TdxWheelPickerItem;
  I, J: Integer;
begin
  Result := 0;
  for I := 0 to Properties.Wheels.Count - 1 do
  begin
    AMaxImageHeight := 0;
    AMaxTextHeight := 0;
    AWheel := Properties.Wheels[I];
    for J := 0 to AWheel.ItemCount - 1 do
    begin
      AWheelItem := AWheel.Items[J];
      AMaxImageHeight := Max(AMaxImageHeight,
        dxGetImageSize(AWheelItem.Glyph, Properties.Images, AWheelItem.ImageIndex, ScaleFactor).cy);
      if AWheelItem.Text <> '' then
        AMaxTextHeight := Max(AMaxTextHeight, cxTextHeight(Style.Font));
    end;
    if AWheel.Items.Layout in [blGlyphLeft, blGlyphRight] then
      Result := Max(Result, Max(AMaxImageHeight, AMaxTextHeight))
    else
      Result := Max(Result, AMaxImageHeight + AMaxTextHeight + ScaleFactor.Apply(dxWheelPickerItemContentMargin));
  end;
  Result := Max(ScaleFactor.Apply(10), Result + 2 * ScaleFactor.Apply(dxWheelPickerItemContentMargin));
end;

procedure TdxCustomWheelPickerViewData.CalculateRowHeight(AViewInfo: TdxCustomWheelPickerViewInfo);
begin
  if Properties.LineCount <= 0 then
    AViewInfo.FRowHeight := Min(GetOptimalRowHeight, cxRectHeight(AViewInfo.ClientRect) - 2)
  else
    AViewInfo.FRowHeight := (cxRectHeight(AViewInfo.ClientRect) - 2 * GetWheelIndent) div Properties.LineCount;
end;

procedure TdxCustomWheelPickerViewData.CalculateTextSize(AViewInfo: TdxCustomWheelPickerViewInfo);
begin
// do nothing
end;

procedure TdxCustomWheelPickerViewData.CalculateWheels(AViewInfo: TdxCustomWheelPickerViewInfo);

  function GetFirstWheelPosX(ADisplayWheelsWidth: TcxAutoWidthObject): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to ADisplayWheelsWidth.Count - 1 do
      Result := Result + ADisplayWheelsWidth.Items[I].AutoWidth;

    Result := (cxRectWidth(AViewInfo.ClientRect) - Result - GetWheelIndent * (GetWheelCount - 1)) div 2;
    Result := AViewInfo.ClientRect.Left + Result;
  end;

var
  I: Integer;
  AWheelLeftPos: Integer;
  AWheelViewInfo: TdxCustomWheelPickerWheelViewInfo;
  AWheelWidth: Integer;
  AWheelBounds: TRect;
  AItem: TcxAutoWidthItem;
  AWheelCount: Integer;
  ADisplayWheelsWidth: TcxAutoWidthObject;
begin
  AWheelCount := GetWheelCount;
  AViewInfo.SetWheelCount(AWheelCount);

  ADisplayWheelsWidth := TcxAutoWidthObject.Create(AWheelCount);
  try
    ADisplayWheelsWidth.AvailableWidth := cxRectWidth(AViewInfo.ClientRect) - GetWheelIndent * (AWheelCount + 1);
    for I := 0 to AWheelCount - 1 do
    begin
      AItem := ADisplayWheelsWidth.AddItem;
      AItem.Width := GetWheelWidth(I);
      AItem.Fixed := not IsWheelAutoSize(I);
    end;
    ADisplayWheelsWidth.Calculate;
    AWheelLeftPos := GetFirstWheelPosX(ADisplayWheelsWidth);
    AWheelBounds := cxRectCenterVertically(AViewInfo.ClientRect, GetWheelsHeight(AViewInfo) - GetWheelIndent * 2);

    for I := 0 to AWheelCount - 1 do
    begin
      AWheelViewInfo := AViewInfo.GetWheel(I);
      InitializeWheel(AWheelViewInfo, I);

      AWheelWidth := ADisplayWheelsWidth[I].AutoWidth;
      AWheelBounds := cxRectSetLeft(cxRectSetWidth(AWheelBounds, AWheelWidth), AWheelLeftPos);
      AWheelViewInfo.Calculate(AWheelBounds);
      AWheelLeftPos := AWheelLeftPos + AWheelWidth + GetWheelIndent;
    end;
  finally
    ADisplayWheelsWidth.Free;
  end;
end;

procedure TdxCustomWheelPickerViewData.EditValueToDrawValue(const AEditValue: Variant; AViewInfo: TcxCustomEditViewInfo);
var
  ADisplayValue: TcxEditValue;
begin
  Properties.PrepareDisplayValue(AEditValue, ADisplayValue, InternalFocused);
  (AViewInfo as TdxCustomWheelPickerViewInfo).SetInternalValue(ADisplayValue);
end;

function TdxCustomWheelPickerViewData.GetOptimalRowHeight: Integer;
begin
  Result := 0;
end;

function TdxCustomWheelPickerViewData.GetOptimalWidth: Integer;
begin
  Result := 0;
end;

function TdxCustomWheelPickerViewData.GetWheelCount: Integer;
begin
  Result := 0;
end;

function TdxCustomWheelPickerViewData.GetWheelWidth(AIndex: Integer): Integer;
begin
  Result := 1;
end;

function TdxCustomWheelPickerViewData.GetWheelsHeight(AViewInfo: TdxCustomWheelPickerViewInfo): Integer;
begin
  Result := cxRectHeight(AViewInfo.ClientRect);
  if Properties.LineCount > 0 then
    Result := Min(Result, Properties.LineCount * AViewInfo.FRowHeight + 2 * GetWheelIndent);
end;

function TdxCustomWheelPickerViewData.GetWheelIndent: Integer;
begin
  Result := ScaleFactor.Apply(1);
end;

procedure TdxCustomWheelPickerViewData.InitializeWheel(AWheelViewInfo: TdxCustomWheelPickerWheelViewInfo; AIndex: Integer);
begin
  AWheelViewInfo.FIndex := AIndex;
end;

function TdxCustomWheelPickerViewData.InternalGetEditConstantPartSize(ACanvas: TcxCanvas; AIsInplace: Boolean;
  AEditSizeProperties: TcxEditSizeProperties; var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize;
var
  ARowCount: Integer;
begin
  Result := inherited InternalGetEditConstantPartSize(ACanvas, AIsInplace, AEditSizeProperties, MinContentSize, AViewInfo);
  if not IsInplace then
  begin
    ARowCount := Properties.LineCount;
    if Properties.LineCount <= 0 then
      ARowCount := 3;
    Result.cy := Result.cy + GetOptimalRowHeight * ARowCount + 2 * GetWheelIndent;
    Result.cx := Result.cx + GetOptimalWidth;
  end
  else
  begin
    ARowCount := Properties.LineCount;
    if Properties.LineCount <= 0 then
      ARowCount := 1;
    MinContentSize.cx := MinContentSize.cx + GetOptimalWidth;
    MinContentSize.cy := MinContentSize.cy + (cxTextHeight(Style.Font) +
      2 * ScaleFactor.Apply(dxWheelPickerItemContentMargin)) * ARowCount + 2 * GetWheelIndent;
  end;
end;

function TdxCustomWheelPickerViewData.IsWheelAutoSize(AIndex: Integer): Boolean;
begin
  Result := True;
end;

function TdxCustomWheelPickerViewData.GetProperties: TdxCustomWheelPickerProperties;
begin
  Result := FProperties as TdxCustomWheelPickerProperties;
end;

{ TdxCustomWheelPickerProperties }

function TdxCustomWheelPickerProperties.CanCompareEditValue: Boolean;
begin
  Result := True;
end;

class function TdxCustomWheelPickerProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TdxCustomWheelPicker;
end;

function TdxCustomWheelPickerProperties.GetSpecialFeatures: TcxEditSpecialFeatures;
begin
  Result := inherited GetSpecialFeatures + [esfMultiRow];
end;

class function TdxCustomWheelPickerProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxCustomWheelPickerViewInfo;
end;

function TdxCustomWheelPickerProperties.IsResetEditClass: Boolean;
begin
  Result := True;
end;

procedure TdxCustomWheelPickerProperties.SetCyclic(AValue: Boolean);
begin
  if FCyclic <> AValue then
  begin
    FCyclic := AValue;
    Changed;
  end;
end;

procedure TdxCustomWheelPickerProperties.SetLineCount(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if FLineCount <> AValue then
  begin
    FLineCount := AValue;
    Changed;
  end;
end;

procedure TdxCustomWheelPickerProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  if AProperties is TdxCustomWheelPickerProperties then
  begin
    Cyclic := TdxCustomWheelPickerProperties(AProperties).Cyclic;
    LineCount := TdxCustomWheelPickerProperties(AProperties).LineCount;
  end;
  inherited;
end;

class function TdxCustomWheelPickerProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TdxCustomWheelPickerViewData;
end;

function TdxCustomWheelPickerProperties.HasDisplayValue: Boolean;
begin
  Result := False;
end;

procedure TdxCustomWheelPickerProperties.InternalPrepareValue(var AEditValue: TcxEditValue);
begin
// do nothing
end;

{ TdxWheelPickerDragAndDropObject }

function TdxWheelPickerDragAndDropObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
begin
  Result := crArrow;
end;

{ TdxCustomWheelPicker }

constructor TdxCustomWheelPicker.Create(AOwner: TComponent);
begin
  inherited;
  AutoSize := False;
end;

procedure TdxCustomWheelPicker.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  FDraggingPosY := MouseDownPos.Y;
  ViewInfo.BeginDragging;
end;

class function TdxCustomWheelPicker.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxCustomWheelPickerProperties;
end;

function TdxCustomWheelPicker.AllowGesture(AGestureId: Integer): Boolean;
begin
  Result := AGestureId = GID_PAN;
end;

function TdxCustomWheelPicker.AllowPan(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := AScrollKind = sbVertical;
end;

procedure TdxCustomWheelPicker.BeginGestureScroll(APos: TPoint);
begin
  ViewInfo.BeginGestureScroll(APos);
end;

procedure TdxCustomWheelPicker.EndGestureScroll;
begin
  ViewInfo.EndGestureScroll;
  InternalSetSelectedValue(ViewInfo.GetInternalValue);
end;

procedure TdxCustomWheelPicker.GestureScroll(ADeltaX, ADeltaY: Integer);
begin
  if ViewInfo.DraggedWheel <> nil then
  begin
    ViewInfo.DraggedWheel.Moving(-ADeltaY);
    ShortRefreshContainer(False);
  end;
end;

function TdxCustomWheelPicker.GetPanOptions: Integer;
begin
  Result := dxTouchPanOptions and not GC_PAN_WITH_SINGLE_FINGER_HORIZONTALLY;
end;

function TdxCustomWheelPicker.IsPanArea(const APoint: TPoint): Boolean;
begin
  Result := True;
end;

function TdxCustomWheelPicker.NeedPanningFeedback(AScrollKind : TScrollBarKind): Boolean;
begin
  Result := False;
end;

procedure TdxCustomWheelPicker.PropertiesChanged(Sender: TObject);
begin
  (ViewInfo as TdxCustomWheelPickerViewInfo).FIsDirty := True;
  inherited;
end;

procedure TdxCustomWheelPicker.BiDiModeChanged;
begin
  inherited;
  SynchronizeDisplayValue;
end;

function TdxCustomWheelPicker.CanAutoWidth: Boolean;
begin
  Result := True;
end;

procedure TdxCustomWheelPicker.ContainerStyleChanged(Sender: TObject);
begin
  (ViewInfo as TdxCustomWheelPickerViewInfo).FIsDirty := True;
  inherited;
end;

procedure TdxCustomWheelPicker.DoFocusChanged;
begin
  inherited;
  ViewInfo.FocusChanged(Focused);
  Invalidate;
end;

procedure TdxCustomWheelPicker.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  inherited;
  ViewInfo.DraggedWheel.Moving(FDraggingPosY - P.Y);
  FDraggingPosY := P.Y;
end;

procedure TdxCustomWheelPicker.EndDragAndDrop(Accepted: Boolean);
begin
  inherited;
  FDraggingPosY := 0;
  ViewInfo.EndDragging;
  InternalSetSelectedValue(ViewInfo.GetInternalValue);
end;

function TdxCustomWheelPicker.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  Result := TdxWheelPickerDragAndDropObject;
end;

procedure TdxCustomWheelPicker.Initialize;
begin
  inherited;
  Width := 200;
  Height := 160;
end;

function TdxCustomWheelPicker.InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := inherited InternalMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
  begin
    Result := True;
    ViewInfo.MouseWheel(Shift, WheelDelta, ScreenToClient(MousePos));
  end;
  InternalSetSelectedValue(ViewInfo.GetInternalValue);
end;

procedure TdxCustomWheelPicker.InternalSetSelectedValue(ASelectedValue: TcxEditValue);
begin
  ActiveProperties.InternalPrepareValue(ASelectedValue);
  if (ASelectedValue <> EditValue) and DoEditing then
  begin
    LockChangeEvents(True);
    try
      InternalEditValue := ASelectedValue;
      ActiveProperties.Changed;
      ModifiedAfterEnter := True;
      if Properties.ImmediatePost and CanPostEditValue and ValidateEdit then
        InternalPostEditValue;
    finally
      LockChangeEvents(False);
    end;
    ShortRefreshContainer(False);
  end;
end;

function TdxCustomWheelPicker.IsDoubleBufferedNeeded: Boolean;
begin
  Result := True;
end;

function TdxCustomWheelPicker.IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := not IsDesigning;
end;

procedure TdxCustomWheelPicker.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if not (Key in [0, VK_ESCAPE]) then
  begin
    ViewInfo.KeyDown(Key, Shift);
    InternalSetSelectedValue(ViewInfo.GetInternalValue);
    Invalidate;
  end;
end;

procedure TdxCustomWheelPicker.Loaded;
begin
  inherited;
  SynchronizeDisplayValue;
end;

procedure TdxCustomWheelPicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    ViewInfo.SetPressedElements(Point(X, Y));
  inherited;
  if (Button = mbLeft) and (DragAndDropState = ddsStarting) and (ViewInfo.PressedWheel.AnimationOffset <> 0) then
    BeginDragAndDrop;
end;

procedure TdxCustomWheelPicker.MouseEnter(AControl: TControl);
begin
  inherited;
  BeginMouseTracking(Self, ClientRect, Self);
end;

procedure TdxCustomWheelPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ViewInfo.DraggedWheel = nil) and (ViewInfo.PressedWheel <> nil) then
  begin
    ViewInfo.MouseClick(Point(X, Y));
    InternalSetSelectedValue(ViewInfo.GetInternalValue);
  end
  else
    ViewInfo.FPressedItemViewInfo := nil;
  Invalidate;
  inherited;
end;

function TdxCustomWheelPicker.StartDragAndDrop(const P: TPoint): Boolean;
begin
  Result := ViewInfo.PressedWheel <> nil;
end;

procedure TdxCustomWheelPicker.SynchronizeDisplayValue;
var
  ADisplayValue: Variant;
begin
  ActiveProperties.PrepareDisplayValue(EditingValue, ADisplayValue, InternalFocused);
  ViewInfo.SetInternalValue(ADisplayValue);
  ViewInfo.FIsDirty := True;
  ShortRefreshContainer(False);
end;

function TdxCustomWheelPicker.WantNavigationKeys: Boolean;
begin
  Result := True;
end;

function TdxCustomWheelPicker.GetActiveProperties: TdxCustomWheelPickerProperties;
begin
  Result := InternalGetActiveProperties as TdxCustomWheelPickerProperties;
end;

function TdxCustomWheelPicker.GetProperties: TdxCustomWheelPickerProperties;
begin
  Result := inherited Properties as TdxCustomWheelPickerProperties;
end;

function TdxCustomWheelPicker.GetViewInfo: TdxCustomWheelPickerViewInfo;
begin
  Result := FViewInfo as TdxCustomWheelPickerViewInfo;
end;

procedure TdxCustomWheelPicker.SetProperties(AValue: TdxCustomWheelPickerProperties);
begin
  Properties.Assign(AValue);
end;

procedure TdxCustomWheelPicker.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
  if IsInplace then
    Message.Result := Message.Result or DLGC_WANTTAB;
end;

{ TdxWheelPickerItemViewInfo }

procedure TdxWheelPickerItemViewInfo.Calculate(const ABounds: TRect);
begin
  inherited;
  FImageSize := GetImageSize;
  FTextSize := GetTextSize;
  FTextFlags := GetTextFlags;
  CalculateContentLayout(FTextRect, FImageRect);
end;

procedure TdxWheelPickerItemViewInfo.CalculateContentLayout(out ATextRect, AImageRect: TRect);
begin
  AImageRect := cxRectSetSize(cxNullRect, FImageSize);
  ATextRect := cxRectSetSize(cxNullRect, FTextSize);
  case FItemContentAlignHorz of
    taLeftJustify:
      CalculateLeftAlignment(ATextRect, AImageRect);
    taRightJustify:
      CalculateRightAlignment(ATextRect, AImageRect);
    taCenter:
      CalculateCenterHorizontallyAlignment(ATextRect, AImageRect);
  end;
  case FItemContentAlignVert of
    vaTop:
      CalculateTopAlignment(ATextRect, AImageRect);
    vaBottom:
      CalculateBottomAlignment(ATextRect, AImageRect);
    vaCenter:
      CalculateCenterVerticallyAlignment(ATextRect, AImageRect);
  end;
end;

procedure TdxWheelPickerItemViewInfo.DrawContent(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState);
begin
  inherited;
  DrawText(ACanvas);
  DrawGlyph(ACanvas, AState);
end;

procedure TdxWheelPickerItemViewInfo.DrawGlyph(ACanvas: TcxCanvas; AState: TcxButtonState);
begin
  cxDrawImage(ACanvas, FImageRect, FGlyph, WheelPickerViewInfo.FImages, FImageIndex,
    ifmNormal, idmNormal, True, Painter.GetWheelPickerColorPalette(AState), ScaleFactor);
end;

procedure TdxWheelPickerItemViewInfo.DrawText(ACanvas: TcxCanvas);
begin
  if HasText then
  begin
    ACanvas.Font := WheelPickerViewInfo.Font;
    ACanvas.Font.Color := WheelPickerViewInfo.TextColor;
    cxTextOut(ACanvas.Handle, FText, FTextRect, FTextFlags);
  end;
end;

function TdxWheelPickerItemViewInfo.GetActualLayout: TButtonLayout;
begin
  Result := FLayout;
  if WheelPickerViewInfo.UseRightToLeftAlignment then
  case Result of
    blGlyphLeft:  Result := blGlyphRight;
    blGlyphRight: Result := blGlyphLeft;
  end;
end;

function TdxWheelPickerItemViewInfo.GetImageSize: TSize;
begin
  Result := dxGetImageSize(FGlyph, WheelPickerViewInfo.FImages, FImageIndex, ScaleFactor);
end;

function TdxWheelPickerItemViewInfo.GetTextFlags: Integer;
const
  AAlignHorz: array[TAlignment] of Integer = (CXTO_LEFT, CXTO_RIGHT, CXTO_CENTER_HORIZONTALLY);
  AAlignVert: array[TcxAlignmentVert] of Integer = (CXTO_TOP, CXTO_BOTTOM, CXTO_CENTER_VERTICALLY);
begin
  Result := CXTO_SINGLELINE or AAlignHorz[FItemContentAlignHorz] or AAlignVert[FItemContentAlignVert];
  if WheelPickerViewInfo.UseRightToLeftReading then
    Result := Result or CXTO_RTLREADING;
end;

function TdxWheelPickerItemViewInfo.GetTextSize: TSize;
var
  ARect: TRect;
begin
  if HasText then
  begin
    Result.cx := cxRectWidth(Bounds) - 2 * ScaleFactor.Apply(dxWheelPickerItemContentMargin);
    Result.cy := cxRectHeight(Bounds) - 2 * ScaleFactor.Apply(dxWheelPickerItemContentMargin);
    if HasGlyph then
    begin
      case ActualLayout of
        blGlyphLeft, blGlyphRight:
          Result.cx := Result.cx - FImageSize.cx - ScaleFactor.Apply(dxWheelPickerItemContentMargin);
        else
          Result.cx := Result.cy - FImageSize.cy - ScaleFactor.Apply(dxWheelPickerItemContentMargin);
      end;
    end;
    ARect := cxRect(Result);
    cxGetTextRect(ARect, FText, WheelPickerViewInfo.Font, DT_SINGLELINE);
    Result := cxRectSize(ARect);
  end
  else
    Result := cxNullSize;
end;

function TdxWheelPickerItemViewInfo.HasGlyph: Boolean;
begin
  Result := IsImageAssigned(FGlyph, WheelPickerViewInfo.FImages, FImageIndex);
end;

function TdxWheelPickerItemViewInfo.HasText: Boolean;
begin
  Result := FText <> '';
end;

procedure TdxWheelPickerItemViewInfo.InitializeByItem(AItem: TdxWheelPickerItem);
begin
  FText := AItem.Text;
  FGlyph := AItem.Glyph;
  FImageIndex := AItem.ImageIndex;
end;

procedure TdxWheelPickerItemViewInfo.CalculateBottomAlignment(var ATextRect, AImageRect: TRect);
begin
  if HasGlyph and HasText then
    case ActualLayout of
      blGlyphTop:
        begin
          ATextRect := cxRectSetBottom(ATextRect, ContentBounds.Bottom);
          AImageRect := cxRectSetBottom(AImageRect, ATextRect.Top - ScaleFactor.Apply(dxWheelPickerItemContentMargin));
        end;
      blGlyphBottom:
        begin
          AImageRect := cxRectSetBottom(AImageRect, ContentBounds.Bottom);
          ATextRect := cxRectSetBottom(ATextRect, AImageRect.Top - ScaleFactor.Apply(dxWheelPickerItemContentMargin));
        end;
      else
        begin
          ATextRect := cxRectSetBottom(ATextRect, ContentBounds.Bottom);
          AImageRect := cxRectSetBottom(AImageRect, ContentBounds.Bottom);
        end;
    end
  else
    if HasText then
      ATextRect := cxRectSetBottom(ATextRect, ContentBounds.Bottom)
    else
      if HasGlyph then
        AImageRect := cxRectSetBottom(AImageRect, ContentBounds.Bottom)
end;

procedure TdxWheelPickerItemViewInfo.CalculateCenterHorizontallyAlignment(var ATextRect, AImageRect: TRect);
var
  ARect: TRect;
begin
  if HasGlyph and HasText then
    case ActualLayout of
      blGlyphLeft:
        begin
          ARect := cxRectCenterHorizontally(ContentBounds, FImageSize.cx + FTextSize.cx + ScaleFactor.Apply(dxWheelPickerItemContentMargin));
          ATextRect := cxRectSetRight(ATextRect, ARect.Right);
          AImageRect := cxRectSetLeft(AImageRect, ARect.Left);
        end;
      blGlyphRight:
        begin
          ARect := cxRectCenterHorizontally(ContentBounds, FImageSize.cx + FTextSize.cx + ScaleFactor.Apply(dxWheelPickerItemContentMargin));
          ATextRect := cxRectSetLeft(ATextRect, ARect.Left);
          AImageRect := cxRectSetRight(AImageRect, ARect.Right);
        end;
      else
        begin
          ATextRect := Rect(ContentBounds.Left, ATextRect.Top, ContentBounds.Right, ATextRect.Bottom);
          AImageRect := Rect(ContentBounds.Left, AImageRect.Top, ContentBounds.Right, AImageRect.Bottom);
          ATextRect := cxRectCenterHorizontally(ATextRect, FTextSize.cx);
          AImageRect := cxRectCenterHorizontally(AImageRect, FImageSize.cx);
        end;
    end
  else
    if HasText then
    begin
      ATextRect := Rect(ContentBounds.Left, ATextRect.Top, ContentBounds.Right, ATextRect.Bottom);
      ATextRect := cxRectCenterHorizontally(cxRectSetWidth(ATextRect, cxRectWidth(ContentBounds)), FTextSize.cx);
    end
    else
      if HasGlyph then
      begin
        AImageRect := Rect(ContentBounds.Left, AImageRect.Top, ContentBounds.Right, AImageRect.Bottom);
        AImageRect := cxRectCenterHorizontally(cxRectSetWidth(AImageRect, cxRectWidth(ContentBounds)), FImageSize.cx);
      end;
end;

procedure TdxWheelPickerItemViewInfo.CalculateCenterVerticallyAlignment(var ATextRect, AImageRect: TRect);
var
  ARect: TRect;
begin
  if HasGlyph and HasText then
    case ActualLayout of
      blGlyphTop:
        begin
          ARect := cxRectCenterVertically(ContentBounds, FImageSize.cy + FTextSize.cy + ScaleFactor.Apply(dxWheelPickerItemContentMargin));
          ATextRect := cxRectSetBottom(ATextRect, ARect.Bottom);
          AImageRect := cxRectSetTop(AImageRect, ARect.Top);
        end;
      blGlyphBottom:
        begin
          ARect := cxRectCenterVertically(ContentBounds, FImageSize.cy + FTextSize.cy + ScaleFactor.Apply(dxWheelPickerItemContentMargin));
          ATextRect := cxRectSetTop(ATextRect, ARect.Top);
          AImageRect := cxRectSetBottom(AImageRect, ARect.Bottom);
        end;
      else
        begin
          ATextRect.Top := ContentBounds.Top;
          ATextRect.Bottom := ContentBounds.Bottom;
          AImageRect.Top := ContentBounds.Top;
          AImageRect.Bottom:= ContentBounds.Bottom;
          ATextRect := cxRectCenterVertically(ATextRect, FTextSize.cy);
          AImageRect := cxRectCenterVertically(AImageRect, FImageSize.cy);
        end;
    end
  else
    if HasText then
    begin
      ATextRect.Top := ContentBounds.Top;
      ATextRect.Bottom := ContentBounds.Bottom;
      ATextRect := cxRectCenterVertically(ATextRect, FTextSize.cy);
    end
    else
      if HasGlyph then
      begin
        AImageRect.Top := ContentBounds.Top;
        AImageRect.Bottom := ContentBounds.Bottom;
        AImageRect := cxRectCenterVertically(AImageRect, FImageSize.cy);
      end;
end;

procedure TdxWheelPickerItemViewInfo.CalculateLeftAlignment(var ATextRect, AImageRect: TRect);
begin
  if HasGlyph and HasText then
    case ActualLayout of
      blGlyphLeft:
        begin
          AImageRect := cxRectSetLeft(AImageRect, ContentBounds.Left);
          ATextRect := cxRectSetLeft(ATextRect, AImageRect.Right + ScaleFactor.Apply(dxWheelPickerItemContentMargin));
        end;
      blGlyphRight:
        begin
          ATextRect := cxRectSetLeft(ATextRect, ContentBounds.Left);
          AImageRect := cxRectSetLeft(AImageRect, ATextRect.Right + ScaleFactor.Apply(dxWheelPickerItemContentMargin));
        end;
      else
        begin
          ATextRect := cxRectSetLeft(ATextRect, ContentBounds.Left);
          AImageRect := cxRectSetLeft(AImageRect, ContentBounds.Left);
        end;
    end
  else
    if HasText then
      ATextRect := cxRectSetLeft(ATextRect, ContentBounds.Left)
    else
      if HasGlyph then
        AImageRect := cxRectSetLeft(AImageRect, ContentBounds.Left);
end;

procedure TdxWheelPickerItemViewInfo.CalculateRightAlignment(var ATextRect, AImageRect: TRect);
begin
  if HasGlyph and HasText then
    case ActualLayout of
      blGlyphLeft:
        begin
          ATextRect := cxRectSetRight(ATextRect, ContentBounds.Right);
          AImageRect := cxRectSetRight(AImageRect, ATextRect.Left - ScaleFactor.Apply(dxWheelPickerItemContentMargin));
        end;
      blGlyphRight:
        begin
          AImageRect := cxRectSetRight(AImageRect, ContentBounds.Right);
          ATextRect := cxRectSetRight(ATextRect, AImageRect.Left - ScaleFactor.Apply(dxWheelPickerItemContentMargin));
        end;
      else
        begin
          ATextRect := cxRectSetRight(ATextRect, ContentBounds.Right);
          AImageRect := cxRectSetRight(AImageRect, ContentBounds.Right);
        end;
    end
  else
    if HasText then
      ATextRect := cxRectSetRight(ATextRect, ContentBounds.Right)
    else
      if HasGlyph then
        AImageRect := cxRectSetRight(AImageRect, ContentBounds.Right)
end;

procedure TdxWheelPickerItemViewInfo.CalculateTopAlignment(var ATextRect, AImageRect: TRect);
begin
  if HasGlyph and HasText then
    case ActualLayout of
      blGlyphTop:
        begin
          AImageRect := cxRectSetTop(AImageRect, ContentBounds.Top);
          ATextRect := cxRectSetTop(ATextRect, AImageRect.Bottom + ScaleFactor.Apply(dxWheelPickerItemContentMargin));
        end;
      blGlyphBottom:
        begin
          ATextRect := cxRectSetTop(ATextRect, ContentBounds.Top);
          AImageRect := cxRectSetTop(AImageRect, ATextRect.Bottom + ScaleFactor.Apply(dxWheelPickerItemContentMargin));
        end;
      else
        begin
          ATextRect := cxRectSetTop(ATextRect, ContentBounds.Top);
          AImageRect := cxRectSetTop(AImageRect, ContentBounds.Top);
        end;
    end
  else
    if HasText then
      ATextRect := cxRectSetTop(ATextRect, ContentBounds.Top)
    else
      if HasGlyph then
        AImageRect := cxRectSetTop(AImageRect, ContentBounds.Top)
end;

function TdxWheelPickerItemViewInfo.GetWheelPickerViewInfo: TdxWheelPickerViewInfo;
begin
  Result := inherited WheelPickerViewInfo as TdxWheelPickerViewInfo;
end;

{ TdxWheelPickerWheelViewInfo }

class function TdxWheelPickerWheelViewInfo.GetWheelPickerItemViewInfoClass: TdxCustomWheelPickerItemViewInfoClass;
begin
  Result := TdxWheelPickerItemViewInfo;
end;

procedure TdxWheelPickerWheelViewInfo.InitializeItem(AItem: TdxCustomWheelPickerItemViewInfo; AIndex: Integer);
var
  AItemViewInfo: TdxWheelPickerItemViewInfo;
begin
  inherited;
  AItemViewInfo := AItem as TdxWheelPickerItemViewInfo;
  AItemViewInfo.FItemContentAlignHorz := FWheel.Items.Alignment.Horz;
  AItemViewInfo.FItemContentAlignVert := FWheel.Items.Alignment.Vert;
  AItemViewInfo.FLayout := FWheel.Items.Layout;
  AItemViewInfo.InitializeByItem(FWheel.Items[AIndex]);
end;

{ TdxWheelPickerViewInfo }

class function TdxWheelPickerViewInfo.GetWheelPickerWheelViewInfoClass: TdxCustomWheelPickerWheelViewInfoClass;
begin
  Result := TdxWheelPickerWheelViewInfo;
end;

procedure TdxWheelPickerViewInfo.SetInternalValue(AValue: Variant);
begin
  inherited;
  if AValue <> Null then
    FItemIndexes := AValue;
end;

procedure TdxWheelPickerViewInfo.SynchronizeWheelIndexes;
var
  I: Integer;
begin
  for I := 0 to Length(FItemIndexes) - 1 do
    if FWheels.Count > I then
      TdxWheelPickerWheelViewInfo(FWheels[I]).ItemIndex := FItemIndexes[I];
end;

{ TdxWheelPickerViewData }

procedure TdxWheelPickerViewData.Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
  Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
var
  AWheelPickerViewInfo: TdxWheelPickerViewInfo;
begin
  AWheelPickerViewInfo := AViewInfo as TdxWheelPickerViewInfo;
  AWheelPickerViewInfo.FImages := Properties.Images;
  AWheelPickerViewInfo.FUseRightToLeftReading := UseRightToLeftReading;
  inherited;
end;

procedure TdxWheelPickerViewData.CalculateRowHeight(AViewInfo: TdxCustomWheelPickerViewInfo);
begin
  AViewInfo.FRowHeight := GetOptimalRowHeight;
end;

procedure TdxWheelPickerViewData.InitializeWheel(AWheelViewInfo: TdxCustomWheelPickerWheelViewInfo; AIndex: Integer);
var
  AWheel: TdxWheelPickerWheel;
begin
  inherited;

  AWheel := Properties.Wheels[AIndex];
  (AWheelViewInfo as TdxWheelPickerWheelViewInfo).FWheel := AWheel;
  AWheelViewInfo.FCyclic := AWheel.Cyclic;
  AWheelViewInfo.FWheelItemCount :=AWheel.ItemCount;
end;

function TdxWheelPickerViewData.GetOptimalRowHeight: Integer;
begin
  if Properties.LineAutoHeight then
    Result := CalculateMaxLineHight
  else
    Result := ScaleFactor.Apply(Properties.LineHeight, Properties.ScaleFactor);
end;

function TdxWheelPickerViewData.GetOptimalWidth: Integer;
var
  I: Integer;
begin
  Result := GetWheelIndent * (Properties.Wheels.Count + 1);
  for I := 0 to Properties.Wheels.Count - 1 do
    Result := Result + Properties.Wheels[I].Width;
end;

function TdxWheelPickerViewData.GetWheelCount: Integer;
begin
  Result := Properties.Wheels.Count;
end;

function TdxWheelPickerViewData.GetWheelWidth(AIndex: Integer): Integer;
begin
  Result := ScaleFactor.Apply(Properties.Wheels[AIndex].Width, Properties.ScaleFactor);
end;

function TdxWheelPickerViewData.GetProperties: TdxWheelPickerProperties;
begin
  Result := FProperties as TdxWheelPickerProperties;
end;

function TdxWheelPickerViewData.IsWheelAutoSize(AIndex: Integer): Boolean;
begin
  Result := Properties.WheelAutoWidth;
end;

{ TdxWheelPickerItem }

constructor TdxWheelPickerItem.Create(Collection: TCollection);
begin
  inherited;
  FGlyph := TdxSmartGlyph.Create;
  FGlyph.OnChange := GlyphChangeHandler;
  FImageIndex := -1;
end;

destructor TdxWheelPickerItem.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited;
end;

procedure TdxWheelPickerItem.Assign(Source: TPersistent);
begin
  if Source is TdxWheelPickerItem then
  begin
    Glyph := TdxWheelPickerItem(Source).Glyph;
    FImageIndex := TdxWheelPickerItem(Source).ImageIndex;
    FText := TdxWheelPickerItem(Source).Text;
    Changed(True);
  end
  else
    inherited;
end;

function TdxWheelPickerItem.GetCollection: TdxWheelPickerItems;
begin
  Result := inherited Collection as TdxWheelPickerItems;
end;

procedure TdxWheelPickerItem.GlyphChangeHandler(Sender: TObject);
begin
  Changed(True);
end;

procedure TdxWheelPickerItem.SetGlyph(AValue: TdxSmartGlyph);
begin
  FGlyph.Assign(AValue);
end;

procedure TdxWheelPickerItem.SetImageIndex(AValue: TcxImageIndex);
begin
  if FImageIndex <> AValue then
  begin
    FImageIndex := AValue;
    Changed(False);
  end;
end;

procedure TdxWheelPickerItem.SetText(const AValue: string);
begin
  if FText <> AValue then
  begin
    FText := AValue;
    Changed(True);
  end;
end;

function TdxWheelPickerItem.GetCaption: string;
begin
  Result := Text;
end;

function TdxWheelPickerItem.GetGalleryItems: IdxGalleryItems;
begin
  Result := Collection as TdxWheelPickerItems;
end;

function TdxWheelPickerItem.GetIndex: Integer;
begin
  Result := Index;
end;

function TdxWheelPickerItem.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TdxWheelPickerItem.SetCaption(const AValue: string);
begin
  Text := AValue;
end;

procedure TdxWheelPickerItem.SetGalleryItems(AItems: IdxGalleryItems);
begin
  SetCollection(AItems.Instance as TCollection);
end;

{ TdxWheelPickerItems }

constructor TdxWheelPickerItems.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, ItemClass);

  FAlignment := TcxAlignment.Create(Self, False, taCenter, vaCenter);
  FAlignment.OnChanged := AlignmentChangeHandler;
  FLayout := blGlyphTop;
end;

destructor TdxWheelPickerItems.Destroy;
begin
  FreeAndNil(FAlignment);
  inherited;
end;

function TdxWheelPickerItems.Add: TdxWheelPickerItem;
begin
  Result := inherited Add as TdxWheelPickerItem;
end;

procedure TdxWheelPickerItems.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    inherited Assign(Source);
    if Source is TdxWheelPickerItems then
    begin
      FAlignment.Assign(TdxWheelPickerItems(Source).Alignment);
      FLayout := TdxWheelPickerItems(Source).Layout;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxWheelPickerItems.Update(Item: TCollectionItem);
begin
  inherited;
  TdxWheelPickerWheel(Owner).Changed;
end;

function TdxWheelPickerItems.GetItem(AIndex: Integer): TdxWheelPickerItem;
begin
  Result := TdxWheelPickerItem(inherited Items[AIndex]);
end;

procedure TdxWheelPickerItems.AlignmentChangeHandler(Sender: TObject);
begin
  Changed;
end;

procedure TdxWheelPickerItems.SetAlignment(AValue: TcxAlignment);
begin
  FAlignment.Assign(AValue);
  Changed;
end;

procedure TdxWheelPickerItems.SetItem(Index: Integer; AValue: TdxWheelPickerItem);
begin
  inherited SetItem(Index, AValue);
end;

procedure TdxWheelPickerItems.SetLayout(AValue: TButtonLayout);
begin
  if AValue <> FLayout then
  begin
    FLayout := AValue;
    Changed;
  end;
end;

function TdxWheelPickerItems.AddGalleryItem: IdxGalleryItem;
begin
  Result := Add;
end;

function TdxWheelPickerItems.GetGalleryItemCount: Integer;
begin
  Result := Count;
end;

function TdxWheelPickerItems.GetInstance: TObject;
begin
  Result := Self;
end;

function TdxWheelPickerItems.GetGalleryItem(AIndex: Integer): IdxGalleryItem;
begin
  Result := Items[AIndex];
end;

function TdxWheelPickerItems.InsertGalleryItem(AIndex: Integer): IdxGalleryItem;
begin
  Result := Insert(AIndex) as TdxWheelPickerItem;
end;

{ TdxWheelPickerWheel }

constructor TdxWheelPickerWheel.Create(ACollection: TCollection);
begin
  ACollection.BeginUpdate;
  try
    inherited Create(ACollection);
    FWidth := 75;
    FItems := CreateItems;
  finally
    ACollection.EndUpdate;
  end;
end;

destructor TdxWheelPickerWheel.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TdxWheelPickerWheel.Assign(Source: TPersistent);
var
  AWheel: TdxWheelPickerWheel;
begin
  if Source is TdxWheelPickerWheel then
  begin
    AWheel := TdxWheelPickerWheel(Source);
    FCyclic := AWheel.Cyclic;
    FWidth := AWheel.Width;
    FItems.Assign(AWheel.Items);
    Changed;
  end
  else
    inherited Assign(Source);
end;

function TdxWheelPickerWheel.CreateItems: TdxWheelPickerItems;
begin
  Result := TdxWheelPickerItems.Create(Self, TdxWheelPickerItem);
end;

procedure TdxWheelPickerWheel.Changed;
begin
  Collection.Changed;
end;

procedure TdxWheelPickerWheel.ChangeScale(M, D: Integer);
begin
  Width := MulDiv(Width, M, D);
end;

function TdxWheelPickerWheel.GetCollection: TdxWheelPickerWheels;
begin
  Result := inherited Collection as TdxWheelPickerWheels;
end;

function TdxWheelPickerWheel.GetItemCount: Integer;
begin
  Result := Items.Count;
end;

procedure TdxWheelPickerWheel.SetCyclic(AValue: Boolean);
begin
  if AValue <> FCyclic then
  begin
    FCyclic := AValue;
    Changed;
  end;
end;

procedure TdxWheelPickerWheel.SetItems(AValue: TdxWheelPickerItems);
begin
  FItems.Assign(AValue);
  Changed;
end;

procedure TdxWheelPickerWheel.SetWidth(AValue: Integer);
begin
  if AValue <> FWidth then
  begin
    FWidth := AValue;
    Changed;
  end;
end;

function TdxWheelPickerWheel.GetGalleryGroupCaption: string;
begin
  Result := sdxWheelPickerWheelPrefix + IntToStr(Index);
end;

function TdxWheelPickerWheel.GetGalleryGroupCollection: IdxGalleryGroups;
begin
  Result := Collection;
end;

function TdxWheelPickerWheel.GetGalleryGroupIndex: Integer;
begin
  Result := Index;
end;

function TdxWheelPickerWheel.GetInstance: TObject;
begin
  Result := Self;
end;

function TdxWheelPickerWheel.GetGalleryGroupItems: IdxGalleryItems;
begin
  Result := Items;
end;

procedure TdxWheelPickerWheel.SetGalleryGroupCaption(const AValue: string);
begin
// do nothing
end;

{ TdxWheelPickerWheels }

function TdxWheelPickerWheels.Add: TdxWheelPickerWheel;
begin
  Result := inherited Add as TdxWheelPickerWheel;
end;

procedure TdxWheelPickerWheels.ChangeScale(M, D: Integer);
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

procedure TdxWheelPickerWheels.Update(Item: TCollectionItem);
begin
  inherited;
  (Owner as TdxWheelPickerProperties).WheelsChanged;
end;

function TdxWheelPickerWheels.GetItem(AIndex: Integer): TdxWheelPickerWheel;
begin
  Result := inherited Items[AIndex] as TdxWheelPickerWheel;
end;

procedure TdxWheelPickerWheels.SetItem(Index: Integer; AValue: TdxWheelPickerWheel);
begin
  inherited SetItem(Index, AValue);
end;

function TdxWheelPickerWheels.AddGalleryGroup: IdxGalleryGroup;
begin
  Result := Add;
end;

function TdxWheelPickerWheels.GetGalleryGroupCount: Integer;
begin
  Result := Count;
end;

function TdxWheelPickerWheels.GetGalleryGroupDisplayName: string;
begin
  Result := sdxWheelPickerWheelPrefix;
end;

function TdxWheelPickerWheels.GetInstance: TObject;
begin
  Result := Self;
end;

function TdxWheelPickerWheels.GetGalleryGroup(AIndex: Integer): IdxGalleryGroup;
begin
  Result := Items[AIndex];
end;

function TdxWheelPickerWheels.InsertGalleryGroup(AIndex: Integer): IdxGalleryGroup;
begin
  Result := Insert(AIndex) as TdxWheelPickerWheel;
end;

{ TdxWheelPickerProperties }

constructor TdxWheelPickerProperties.Create(AOwner: TPersistent);
begin
  inherited;
  BeginUpdate;
  try
    FLineHeight := 20;
    FWheels := CreateWheels;
  finally
    EndUpdate;
  end;
end;

destructor TdxWheelPickerProperties.Destroy;
begin
  FreeAndNil(FWheels);
  inherited;
end;

function TdxWheelPickerProperties.CalculateEditValueByItemIndexes(
  const AItemIndexes: TdxWheelPickerItemIndexes): TcxEditValue;
var
  AData: AnsiString;
begin
  SetLength(AData, Length(AItemIndexes) * SizeOf(Integer));
  cxCopyData(Pointer(AItemIndexes), Pointer(AData), Length(AItemIndexes) * SizeOf(Integer));
  Result := AData;
end;

procedure TdxWheelPickerProperties.ChangeScale(M: Integer; D: Integer);
begin
  inherited ChangeScale(M, D);
  LineHeight := MulDiv(LineHeight, M, D);
  Wheels.ChangeScale(M, D);
end;

function TdxWheelPickerProperties.CreateWheels: TdxWheelPickerWheels;
begin
  Result := TdxWheelPickerWheels.Create(Self, TdxWheelPickerWheel);
end;

procedure TdxWheelPickerProperties.DoAssign(AProperties: TcxCustomEditProperties);
var
  AWheelPickerProperties: TdxWheelPickerProperties;
begin
  if AProperties is TdxWheelPickerProperties then
  begin
    AWheelPickerProperties := TdxWheelPickerProperties(AProperties);
    FWheelAutoWidth := AWheelPickerProperties.WheelAutoWidth;
    FWheels.Assign(AWheelPickerProperties.Wheels);
    FLineCount := AWheelPickerProperties.LineCount;
    FLineHeight := Self.ScaleFactor.Apply(AWheelPickerProperties.LineHeight, ScaleFactor);
    FLineAutoHeight := AWheelPickerProperties.LineAutoHeight;
  end;
  inherited;
end;

class function TdxWheelPickerProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TdxWheelPicker;
end;

function TdxWheelPickerProperties.GetMaxContentItemHeight: Integer;
begin
  Result := 0;
end;

class function TdxWheelPickerProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TdxWheelPickerViewData;
end;

class function TdxWheelPickerProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxWheelPickerViewInfo;
end;

procedure TdxWheelPickerProperties.InternalPrepareValue(var AEditValue: TcxEditValue);
var
  AValue: TdxWheelPickerItemIndexes;
begin
  inherited;
  if (AEditValue <> Null) and (Wheels.Count > 0) then
  begin
    AValue := AEditValue;
    AEditValue := CalculateEditValueByItemIndexes(AValue);
  end;
end;

procedure TdxWheelPickerProperties.PrepareDisplayValue(const AEditValue: Variant; var DisplayValue: Variant; AEditFocused: Boolean);
var
  AValueString: AnsiString;
  AItemIndexes: TdxWheelPickerItemIndexes;
begin
  inherited;
  SetLength(AItemIndexes, Wheels.Count);
  if (AEditValue <> Null) and (Wheels.Count > 0) then
  begin
    AValueString := dxVariantToAnsiString(AEditValue);
    cxCopyData(Pointer(AValueString), Pointer(AItemIndexes), Wheels.Count * SizeOf(Integer));
    DisplayValue := AItemIndexes;
  end
  else
    if Wheels.Count > 0 then
      DisplayValue := AItemIndexes
    else
      DisplayValue := Null;
end;

procedure TdxWheelPickerProperties.WheelsChanged;
begin
  Changed;
end;

procedure TdxWheelPickerProperties.SetLineAutoHeight(AValue: Boolean);
begin
  if FLineAutoHeight <> AValue then
  begin
    FLineAutoHeight := AValue;
    Changed;
  end;
end;

procedure TdxWheelPickerProperties.SetLineHeight(AValue: Integer);
begin
  if (FLineHeight <> AValue) and (AValue >= 10) then
  begin
    FLineHeight := AValue;
    Changed;
  end;
end;

procedure TdxWheelPickerProperties.SetWheelAutoWidth(AValue: Boolean);
begin
  if FWheelAutoWidth <> AValue then
  begin
    FWheelAutoWidth := AValue;
    Changed;
  end;
end;

procedure TdxWheelPickerProperties.SetWheels(AValue: TdxWheelPickerWheels);
begin
  FWheels.Assign(AValue);
  Changed;
end;

{ TdxWheelPicker }

class function TdxWheelPicker.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxWheelPickerProperties;
end;

function TdxWheelPicker.GetItemIndex(AIndex: Integer): Integer;
var
  AValue: TcxEditValue;
begin
  ActiveProperties.PrepareDisplayValue(EditValue, AValue, False);
  if AValue = Null then
    Result := 0
  else
    Result := Min(AValue[AIndex], ActiveProperties.Wheels[AIndex].ItemCount - 1);
end;

procedure TdxWheelPicker.SetItemIndex(AIndex, AItemIndex: Integer);
var
  AValue: TcxEditValue;
begin
  ActiveProperties.PrepareDisplayValue(EditValue, AValue, False);
  if AValue[AIndex] <> AItemIndex then
  begin
    AValue[AIndex] := AItemIndex;
    ActiveProperties.InternalPrepareValue(AValue);
    EditValue := AValue;
    SynchronizeDisplayValue;
  end;
end;

procedure TdxWheelPicker.SynchronizeEditValue;
begin
  ActiveProperties.CalculateEditValueByItemIndexes(FItemIndexes);
end;

function TdxWheelPicker.GetAllowedCustomizationActions: TdxGalleryCustomizationActions;
begin
  Result := [gcaChangeItemCaption];
end;

function TdxWheelPicker.GetGroups: IdxGalleryGroups;
begin
  Result := ActiveProperties.Wheels;
end;

function TdxWheelPicker.GetActiveProperties: TdxWheelPickerProperties;
begin
  Result := InternalGetActiveProperties as TdxWheelPickerProperties;
end;

function TdxWheelPicker.GetProperties: TdxWheelPickerProperties;
begin
  Result := inherited Properties as TdxWheelPickerProperties;
end;

procedure TdxWheelPicker.SetProperties(AValue: TdxWheelPickerProperties);
begin
  Properties.Assign(AValue);
end;

end.

