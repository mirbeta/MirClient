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

unit dxGalleryControl;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI17}
  UITypes,
{$ENDIF}
  Windows, SysUtils, Classes, Types, Graphics, Forms, Controls, StdCtrls, Messages, ImgList,
  dxCore, dxCoreClasses, cxClasses, cxControls, dxGDIPlusClasses, cxLookAndFeels, cxLookAndFeelPainters,
  cxGraphics, cxGeometry, dxGallery;

type
  TdxCustomGalleryControl = class;
  TdxGalleryControlGroup = class;
  TdxGalleryControlItem = class;
  TdxGalleryControlPainter = class;
  TdxGalleryControlPainterClass = class of TdxGalleryControlPainter;
  TdxGalleryControlOptionsItemImage = class;
  TdxGalleryControlOptionsItemText = class;
  TdxGalleryControlOptionsView = class;

  TdxGalleryControlItemEvent = procedure(Sender: TObject; AItem: TdxGalleryControlItem) of object;

  TdxGalleryControlItemMatrix = array of array of TdxGalleryControlItem;

  TdxGalleryItemMultiSelectKind = (imskGallery, imskListView);

  { TdxGalleryPersistent }

  TdxGalleryPersistent = class(TcxOwnedPersistent)
  strict private
    function GetOwnerControl: TdxCustomGalleryControl;
  protected
    property Owner: TdxCustomGalleryControl read GetOwnerControl;
  public
    constructor Create(AOwner: TdxCustomGalleryControl); reintroduce; virtual;
  end;

  { TdxGalleryCustomViewInfo }

  TdxGalleryCustomViewInfo = class
  strict private
    function GetContentOffset: TRect; inline;
    function GetContentOffsetGroups: TRect; inline;
    function GetContentOffsetItems: TRect; inline;
    function GetOptionsItemImage: TdxGalleryControlOptionsItemImage; inline;
    function GetOptionsItemText: TdxGalleryControlOptionsItemText; inline;
    function GetOptionsView: TdxGalleryControlOptionsView; inline;
    function GetPainter: TdxGalleryControlPainter;
    function GetScaleFactor: TdxScaleFactor; inline;
    function GetUseRightToLeftAlignment: Boolean; inline;
  protected
    FBounds: TRect;

    procedure DrawContent(ACanvas: TcxCanvas); virtual; abstract;
    function GetGalleryControl: TdxCustomGalleryControl; virtual; abstract;
  public
    procedure Calculate(AType: TdxChangeType; const ABounds: TRect); virtual; abstract;
    procedure Draw(ACanvas: TcxCanvas);
    //
    property Bounds: TRect read FBounds;
    property ContentOffset: TRect read GetContentOffset;
    property ContentOffsetGroups: TRect read GetContentOffsetGroups;
    property ContentOffsetItems: TRect read GetContentOffsetItems;
    property GalleryControl: TdxCustomGalleryControl read GetGalleryControl;
    property OptionsItemImage: TdxGalleryControlOptionsItemImage read GetOptionsItemImage;
    property OptionsItemText: TdxGalleryControlOptionsItemText read GetOptionsItemText;
    property OptionsView: TdxGalleryControlOptionsView read GetOptionsView;
    property Painter: TdxGalleryControlPainter read GetPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property UseRightToLeftAlignment: Boolean read GetUseRightToLeftAlignment;
  end;

  { TdxGalleryItemViewInfo }

  TdxGalleryItemViewInfo = class(TdxGalleryCustomViewInfo)
  strict private
    FCacheGlyph: TdxSmartGlyph;
    FCacheGlyphColorPalette: Pointer;
    FItem: TdxGalleryControlItem;

    function GetCacheGlyph: TdxSmartGlyph;
    function GetCaption: string;
    function GetDescription: string;
    function GetGlyphSize: TSize;
  protected
    FCaptionRect: TRect;
    FCaptionSize: TSize;
    FCellPositionInGroup: TPoint;
    FContentBounds: TRect;
    FDescriptionRect: TRect;
    FDescriptionSize: TSize;
    FGlyphFrameRect: TRect;
    FGlyphRect: TRect;
    FState: TdxGalleryItemViewState;
    FTextArea: TRect;

    procedure CalculateGlyphArea(const AGlyphSize: TSize); virtual;
    procedure CalculateTextArea(const ATextAreaSize: TSize); virtual;
    procedure CalculateTextAreaContent(const ABounds: TRect); virtual;
    procedure DrawContent(ACanvas: TcxCanvas); override;
    function GetDescriptionOffset: Integer; virtual;
    function GetGalleryControl: TdxCustomGalleryControl; override;
    function GetTextAreaSize: TSize; virtual;
    procedure ResetCache; virtual;
  public
    constructor Create(AItem: TdxGalleryControlItem); virtual;
    destructor Destroy; override;
    procedure Calculate(AType: TdxChangeType; const ABounds: TRect); override;
    procedure CalculateTextAreaSizeLimitedByRowCount(ACanvas: TcxCanvas; ARowCount: Integer); virtual;
    procedure CalculateTextAreaSizeLimitedByWidth(ACanvas: TcxCanvas; AMaxWidth: Integer); virtual;

    property CacheGlyph: TdxSmartGlyph read GetCacheGlyph;
    property Caption: string read GetCaption;
    property CaptionRect: TRect read FCaptionRect;
    property CaptionSize: TSize read FCaptionSize;
    property CellPositionInGroup: TPoint read FCellPositionInGroup;
    property ContentBounds: TRect read FContentBounds;
    property Description: string read GetDescription;
    property DescriptionOffset: Integer read GetDescriptionOffset;
    property DescriptionRect: TRect read FDescriptionRect;
    property DescriptionSize: TSize read FDescriptionSize;
    property GlyphFrameRect: TRect read FGlyphFrameRect;
    property GlyphRect: TRect read FGlyphRect;
    property GlyphSize: TSize read GetGlyphSize;
    property Item: TdxGalleryControlItem read FItem;
    property State: TdxGalleryItemViewState read FState;
    property TextArea: TRect read FTextArea;
    property TextAreaSize: TSize read GetTextAreaSize;
  end;

  { TdxGalleryControlItem }

  TdxGalleryControlItem = class(TdxGalleryItem)
  strict private
    FViewInfo: TdxGalleryItemViewInfo;

    function GetGroup: TdxGalleryControlGroup;
    function GetGalleryControl: TdxCustomGalleryControl;
    function GetImages: TCustomImageList;
    procedure SetGroup(AGroup: TdxGalleryControlGroup);
  protected
    function CreateViewInfo: TdxGalleryItemViewInfo; virtual;

    property GalleryControl: TdxCustomGalleryControl read GetGalleryControl;
    property Images: TCustomImageList read GetImages;
    property ViewInfo: TdxGalleryItemViewInfo read FViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Group: TdxGalleryControlGroup read GetGroup write SetGroup;
  published
    property Caption;
    property Checked;
    property Description;
    property Enabled;
    property Glyph;
    property Hint;
    property ImageIndex;
  end;

  { TdxGalleryControlItems }

  TdxGalleryControlItems = class(TdxGalleryItems)
  strict private
    function GetItem(AIndex: Integer): TdxGalleryControlItem;
    procedure SetItem(AIndex: Integer; AValue: TdxGalleryControlItem);
  protected
    procedure SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1); override;
  public
    function Add: TdxGalleryControlItem;
    function GetItemAtPos(const P: TPoint): TdxGalleryControlItem;

    property Items[AIndex: Integer]: TdxGalleryControlItem read GetItem write SetItem; default;
  end;

  { TdxGalleryGroupViewInfo }

  TdxGalleryGroupViewInfo = class(TdxGalleryCustomViewInfo)
  strict private
    FGroup: TdxGalleryControlGroup;

    function GetCaption: string;
    function GetSize: TSize;
  protected
    FCaptionRect: TRect;
    FCaptionTextRect: TRect;
    FColumnCount: Integer;
    FItemsRect: TRect;
    FRowCount: Integer;

    procedure CalculateCaption; virtual;
    procedure CalculateItems(AType: TdxChangeType); virtual;
    procedure DrawContent(ACanvas: TcxCanvas); override;
    function GetCaptionHeight: Integer; virtual;
    function GetCaptionTextOffsets: TRect; virtual;
    function GetFont: TFont; virtual;
    function GetGalleryControl: TdxCustomGalleryControl; override;
    procedure PlaceItem(AItem: TdxGalleryControlItem; AChangeType: TdxChangeType; const AItemsArea: TRect; ACellIndex: Integer);
  public
    constructor Create(AGroup: TdxGalleryControlGroup); virtual;
    procedure Calculate(AType: TdxChangeType; const ABounds: TRect); override;
    function GetMaxColumnCount: Integer; virtual;
    //
    property Caption: string read GetCaption;
    property CaptionRect: TRect read FCaptionRect;
    property CaptionTextOffsets: TRect read GetCaptionTextOffsets;
    property CaptionTextRect: TRect read FCaptionTextRect;
    property ColumnCount: Integer read FColumnCount;
    property Font: TFont read GetFont;
    property Group: TdxGalleryControlGroup read FGroup;
    property ItemSize: TSize read GetSize;
    property ItemsRect: TRect read FItemsRect;
    property RowCount: Integer read FRowCount;
  end;

  { TdxGalleryControlGroup }

  TdxGalleryControlGroup = class(TdxGalleryGroup)
  private
    FViewInfo: TdxGalleryGroupViewInfo;
    function GetGalleryControl: TdxCustomGalleryControl;
    function GetItems: TdxGalleryControlItems;
  protected
    function CreateViewInfo: TdxGalleryGroupViewInfo; virtual;
    function GetGalleryItemClass: TdxGalleryItemClass; override;
    function GetGalleryItemsClass: TdxGalleryItemsClass; override;

    property GalleryControl: TdxCustomGalleryControl read GetGalleryControl;
    property ViewInfo: TdxGalleryGroupViewInfo read FViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    property Items: TdxGalleryControlItems read GetItems;
  published
    property Caption;
    property ShowCaption default True;
    property Visible default True;
  end;

  { TdxGalleryControlGroups }

  TdxGalleryControlGroups = class(TdxGalleryGroups)
  private
    function GetGroup(AIndex: Integer): TdxGalleryControlGroup;
    procedure SetGroup(AIndex: Integer; AValue: TdxGalleryControlGroup);
  public
    function Add: TdxGalleryControlGroup;
    function FindByCaption(const ACaption: string; out AGroup: TdxGalleryControlGroup): Boolean;
    function GetGroupAtPos(const P: TPoint): TdxGalleryControlGroup;
    function GetItemAtPos(const P: TPoint): TdxGalleryControlItem;

    property Groups[AIndex: Integer]: TdxGalleryControlGroup read GetGroup write SetGroup; default;
  end;

  { TdxGalleryControlStructure }

  TdxGalleryControlStructure = class(TdxGallery)
  strict private
    function GetGroups: TdxGalleryControlGroups;
  protected
    function GetGroupClass: TdxGalleryGroupClass; override;
    function GetGroupsClass: TdxGalleryGroupsClass; override;
  public
    function GetCheckedItem: TdxGalleryControlItem;
    function GetFirstItem: TdxGalleryControlItem;
    function GetFirstVisibleItem: TdxGalleryControlItem;

    property Groups: TdxGalleryControlGroups read GetGroups;
  end;

  { TdxGalleryControlPainter }

  TdxGalleryControlPainter = class(TdxGalleryPersistent)
  strict private
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; inline;
    function GetScaleFactor: TdxScaleFactor; inline;
    function GetUseRightToLeftAlignment: Boolean; inline;
  protected
    function DrawItemSelectionFirst: Boolean; virtual;
    function GetGroupCaptionTextColor: TColor; virtual;
    function GetItemCaptionTextColor(AViewInfo: TdxGalleryItemViewInfo): TColor; virtual;
    function GetItemDescriptionTextColor(AViewInfo: TdxGalleryItemViewInfo): TColor; virtual;
  public
    procedure DrawBackground(ACanvas: TcxCanvas; const ABounds: TRect); virtual;
    // Group
    procedure DrawGroupHeader(ACanvas: TcxCanvas; const AViewInfo: TdxGalleryGroupViewInfo); virtual;
    procedure DrawGroupHeaderText(ACanvas: TcxCanvas; const AViewInfo: TdxGalleryGroupViewInfo); virtual;
    function GetGroupHeaderContentOffsets: TRect; virtual;
    // Items
    procedure DrawItem(ACanvas: TcxCanvas; AViewInfo: TdxGalleryItemViewInfo); virtual;
    procedure DrawItemImage(ACanvas: TcxCanvas; AViewInfo: TdxGalleryItemViewInfo); virtual;
    procedure DrawItemSelection(ACanvas: TcxCanvas; AViewInfo: TdxGalleryItemViewInfo); virtual;
    procedure DrawItemText(ACanvas: TcxCanvas; AViewInfo: TdxGalleryItemViewInfo); virtual;
    //
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property UseRightToLeftAlignment: Boolean read GetUseRightToLeftAlignment;
  end;

  { TdxGalleryControlViewInfo }

  TdxGalleryControlViewInfo = class(TdxGalleryCustomViewInfo)
  strict private
    FGalleryControl: TdxCustomGalleryControl;

    function GetAutoHeight: Boolean;
    function GetAutoWidth: Boolean;
    function GetColumnAutoWidth: Boolean;
    function GetFont: TFont;  inline;
    function GetGroups: TdxGalleryControlGroups; inline;
    function GetMaxColumnCount: Integer;
    function GetRowCount: Integer;
  protected
    FColumnCount: Integer;
    FContentBounds: TRect;
    FImageSize: TSize;
    FItemSize: TSize;
    FTextAreaSize: TSize;

    procedure CalculateColumnCount; virtual;
    procedure CalculateContentBounds(AType: TdxChangeType); virtual;
    procedure CalculateItemSize; virtual;
    function CalculateMaxItemImageSize: TSize; virtual;
    function CalculateMaxItemTextAreaSize(const AImageSize: TSize): TSize; virtual;
    function CalculateMaxItemTextAreaSizeLimitedByRowCount(ARowCount: Integer): TSize; virtual;
    function CalculateMaxItemTextAreaSizeLimitedByWidth(AMaxWidth: Integer): TSize; virtual;
    function DoCalculateItemSize: TSize; virtual;
    procedure DrawContent(ACanvas: TcxCanvas); override;
    function GetAvailableGroupsAreaWidth: Integer; virtual;
    function GetBorderWidths: TRect; virtual;
    function GetGalleryControl: TdxCustomGalleryControl; override;
    function GetTextAreaMaxRowCount(const AImageSize: TSize): Integer; virtual;
    function GetTextAreaMaxWidth(const AImageSize: TSize): Integer; virtual;
  public
    constructor Create(AGalleryControl: TdxCustomGalleryControl); virtual;
    procedure Calculate(AType: TdxChangeType; const ABounds: TRect); override;
    //
    property AutoHeight: Boolean read GetAutoHeight;
    property AutoWidth: Boolean read GetAutoWidth;
    property BorderWidths: TRect read GetBorderWidths;
    property ColumnAutoWidth: Boolean read GetColumnAutoWidth;
    property ColumnCount: Integer read FColumnCount;
    property ContentBounds: TRect read FContentBounds;
    property Font: TFont read GetFont;
    property Groups: TdxGalleryControlGroups read GetGroups;
    property ImageSize: TSize read FImageSize;
    property ItemSize: TSize read FItemSize;
    property MaxColumnCount: Integer read GetMaxColumnCount;
    property RowCount: Integer read GetRowCount;
    property TextAreaSize: TSize read FTextAreaSize;
  end;

  { TdxGalleryControlNavigationMatrix }

  TdxGalleryControlNavigationMatrix = class(TObject)
  private
    FColumnCount: Integer;
    FRowCount: Integer;
    FValues: TdxGalleryControlItemMatrix;
    function GetValue(ACol, ARow: Integer): TdxGalleryControlItem;
    procedure SetValue(ACol, ARow: Integer; const AValue: TdxGalleryControlItem);
  protected
    procedure Populate(AViewInfo: TdxGalleryControlViewInfo); virtual;
  public
    constructor Create(AViewInfo: TdxGalleryControlViewInfo);
    destructor Destroy; override;
    function GetRightMostItemIndex(ARow: Integer): Integer;
    //
    property ColumnCount: Integer read FColumnCount;
    property RowCount: Integer read FRowCount;
    property Values[ACol, ARow: Integer]: TdxGalleryControlItem read GetValue write SetValue;
  end;

  { TdxGalleryControlController }

  TdxGalleryControlController = class(TdxGalleryPersistent)
  private
    FKeyPressed: Boolean;
    FKeySelectedItem: TdxGalleryControlItem;
    FMouseHoveredItem: TdxGalleryControlItem;
    FMousePressed: Boolean;
    FNavigationMatrix: TdxGalleryControlNavigationMatrix;
    FStartSelectionItem: TdxGalleryControlItem;

    function CanChangeSelection(AButton: TMouseButton; AShift: TShiftState): Boolean;
    function GetGallery: TdxGalleryControlStructure; inline;
    function GetNavigationMatrix: TdxGalleryControlNavigationMatrix;
    function GetViewInfo: TdxGalleryControlViewInfo; inline;
    procedure SetKeyPressed(AValue: Boolean);
    procedure SetKeySelectedItem(AItem: TdxGalleryControlItem);
    procedure SetMouseHoveredItem(AItem: TdxGalleryControlItem);
    procedure SetMousePressed(AValue: Boolean);
  protected
    function GetItemAtPos(const P: TPoint): TdxGalleryControlItem;
    function GetItemPosition(AItem: TdxGalleryControlItem): TPoint; virtual;
    function GetItemViewState(AItem: TdxGalleryControlItem): TdxGalleryItemViewState;
    procedure InvalidateItem(AItem: TdxGalleryControlItem);
    function IsGalleryStyleSelection: Boolean;
    function IsListViewStyleSelection: Boolean;
    procedure MakeItemVisible(AItem: TdxGalleryControlItem); virtual;
    procedure UpdateItemViewState(AItem: TdxGalleryControlItem);
    procedure UpdateMouseHoveredItem(const P: TPoint); virtual;

    // ListView selection mode
    procedure SelectItems(AStartFromItem, AFinishAtItem: TdxGalleryControlItem); virtual;

    // Navigation
    function CreateNavigationMatrix: TdxGalleryControlNavigationMatrix; virtual;
    procedure GetNextItem(var AItemPos: TPoint; ADirectionX, ADirectionY: Integer); virtual;
    function GetStartItemForKeyboardNavigation: TdxGalleryControlItem; virtual;
    procedure SelectNextItem(AItem: TdxGalleryControlItem; ADirectionX, ADirectionY: Integer; AShift: TShiftState); virtual;

    procedure ProcessItemClick(AItem: TdxGalleryControlItem; X, Y: Integer); virtual;
  public
    destructor Destroy; override;

    procedure CheckSelectedItem; virtual;
    procedure LayoutChanged; virtual;
    // Keyboard
    procedure FocusEnter; virtual;
    procedure FocusLeave; virtual;
    procedure KeyDown(AKey: Word; AShift: TShiftState); virtual;
    procedure KeyUp(AKey: Word; AShift: TShiftState); virtual;
    // Mouse
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseLeave; virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    //
    property Gallery: TdxGalleryControlStructure read GetGallery;
    property KeyPressed: Boolean read FKeyPressed write SetKeyPressed;
    property KeySelectedItem: TdxGalleryControlItem read FKeySelectedItem write SetKeySelectedItem;
    property MouseHoveredItem: TdxGalleryControlItem read FMouseHoveredItem write SetMouseHoveredItem;
    property MousePressed: Boolean read FMousePressed write SetMousePressed;
    property NavigationMatrix: TdxGalleryControlNavigationMatrix read GetNavigationMatrix;
    property StartSelectionItem: TdxGalleryControlItem read FStartSelectionItem write FStartSelectionItem;
    property ViewInfo: TdxGalleryControlViewInfo read GetViewInfo;
  end;

  { TdxGalleryControlCustomOptions }

  TdxGalleryControlCustomOptions = class(TdxGalleryPersistent)
  protected
    procedure Changed(AType: TdxChangeType = ctHard);
    procedure ChangeScale(M, D: Integer); virtual;
  end;

  { TdxGalleryControlOptionsBehavior }

  TdxGalleryControlOptionsBehavior = class(TdxGalleryControlCustomOptions)
  strict private
    FItemMultiSelectKind: TdxGalleryItemMultiSelectKind;
    FItemShowHint: Boolean;
    FSelectOnRightClick: Boolean;

    function GetItemCheckMode: TdxGalleryItemCheckMode;
    procedure SetItemCheckMode(const Value: TdxGalleryItemCheckMode);
  protected
    procedure DoAssign(Source: TPersistent); override;
  published
    property ItemMultiSelectKind: TdxGalleryItemMultiSelectKind read FItemMultiSelectKind write FItemMultiSelectKind default imskGallery;
    property ItemCheckMode: TdxGalleryItemCheckMode read GetItemCheckMode write SetItemCheckMode default icmNone;
    property ItemShowHint: Boolean read FItemShowHint write FItemShowHint default False;
    property SelectOnRightClick: Boolean read FSelectOnRightClick write FSelectOnRightClick default False;
  end;

  { TdxGalleryControlOptionsItemImage }

  TdxGalleryControlOptionsItemImage = class(TdxGalleryControlCustomOptions)
  strict private
    FShowFrame: Boolean;
    FSize: TcxSize;

    procedure ChangeHandler(Sender: TObject);
    procedure SetShowFrame(const Value: Boolean);
    procedure SetSize(const Value: TcxSize);
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TdxCustomGalleryControl); override;
    destructor Destroy; override;
  published
    property ShowFrame: Boolean read FShowFrame write SetShowFrame default True;
    property Size: TcxSize read FSize write SetSize;
  end;

  { TdxGalleryControlOptionsItemText }

  TdxGalleryControlOptionsItemText = class(TdxGalleryControlCustomOptions)
  strict private
    FAlignHorz: TAlignment;
    FAlignVert: TcxAlignmentVert;
    FPosition: TcxPosition;
    FWordWrap: Boolean;

    procedure SetAlignHorz(const Value: TAlignment);
    procedure SetAlignVert(const Value: TcxAlignmentVert);
    procedure SetPosition(const Value: TcxPosition);
    procedure SetWordWrap(const Value: Boolean);
  protected
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TdxCustomGalleryControl); override;
  published
    property AlignHorz: TAlignment read FAlignHorz write SetAlignHorz default taCenter;
    property AlignVert: TcxAlignmentVert read FAlignVert write SetAlignVert default vaTop;
    property Position: TcxPosition read FPosition write SetPosition default posNone;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;
  end;

  { TdxGalleryControlOptionsItem }

  TdxGalleryControlOptionsItem = class(TdxGalleryControlCustomOptions)
  strict private
    FImage: TdxGalleryControlOptionsItemImage;
    FText: TdxGalleryControlOptionsItemText;

    procedure SetImage(AValue: TdxGalleryControlOptionsItemImage);
    procedure SetText(AValue: TdxGalleryControlOptionsItemText);
  protected
    procedure ChangeScale(M, D: Integer); override;
    function CreateImage: TdxGalleryControlOptionsItemImage; virtual;
    function CreateText: TdxGalleryControlOptionsItemText; virtual;
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TdxCustomGalleryControl); override;
    destructor Destroy; override;
  published
    property Image: TdxGalleryControlOptionsItemImage read FImage write SetImage;
    property Text: TdxGalleryControlOptionsItemText read FText write SetText;
  end;

  { TdxGalleryControlOptionsView }

  TdxGalleryControlOptionsView = class(TdxGalleryControlCustomOptions)
  strict private
    FColumnAutoWidth: Boolean;
    FColumnCount: Integer;
    FContentOffset: TcxMargin;
    FContentOffsetGroups: TcxMargin;
    FContentOffsetItems: TcxMargin;
    FItem: TdxGalleryControlOptionsItem;

    procedure ChangeHandler(Sender: TObject);
    procedure SetColumnAutoWidth(AValue: Boolean);
    procedure SetColumnCount(AValue: Integer);
    procedure SetContentOffset(const Value: TcxMargin);
    procedure SetContentOffsetGroups(const Value: TcxMargin);
    procedure SetContentOffsetItems(const Value: TcxMargin);
    procedure SetItem(const Value: TdxGalleryControlOptionsItem);
  protected
    procedure ChangeScale(M, D: Integer); override;
    function CreateItem: TdxGalleryControlOptionsItem; virtual;
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TdxCustomGalleryControl); override;
    destructor Destroy; override;
  published
    property ColumnAutoWidth: Boolean read FColumnAutoWidth write SetColumnAutoWidth default False;
    property ColumnCount: Integer read FColumnCount write SetColumnCount default 0;
    property ContentOffset: TcxMargin read FContentOffset write SetContentOffset;
    property ContentOffsetGroups: TcxMargin read FContentOffsetGroups write SetContentOffsetGroups;
    property ContentOffsetItems: TcxMargin read FContentOffsetItems write SetContentOffsetItems;
    property Item: TdxGalleryControlOptionsItem read FItem write SetItem;
  end;

  { TdxCustomGalleryControl }

  TdxCustomGalleryControl = class(TcxScrollingControl, IdxSkinSupport, IdxGalleryOwner)
  strict private
    FCanBeFocused: Boolean;
    FController: TdxGalleryControlController;
    FGallery: TdxGalleryControlStructure;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FLockCount: Integer;
    FOptionsBehavior: TdxGalleryControlOptionsBehavior;
    FOptionsView: TdxGalleryControlOptionsView;
    FPainter: TdxGalleryControlPainter;
    FViewInfo: TdxGalleryControlViewInfo;

    FOnItemClick: TdxGalleryControlItemEvent;

    function GetColumnCount: Integer;
    function GetContentOffset: TcxMargin;
    function GetContentOffsetGroups: TcxMargin;
    function GetContentOffsetItems: TcxMargin;
    function GetItemCheckMode: TdxGalleryItemCheckMode;
    function GetItemCount: Integer;
    function GetItemImageSize: TcxSize;
    function GetItemShowHint: Boolean;
    function GetItemShowImageFrame: Boolean;
    function GetItemTextPosition: TcxPosition;
    procedure SetColumnCount(AValue: Integer);
    procedure SetContentOffset(AValue: TcxMargin);
    procedure SetContentOffsetGroups(AValue: TcxMargin);
    procedure SetContentOffsetItems(AValue: TcxMargin);
    procedure SetGallery(AValue: TdxGalleryControlStructure);
    procedure SetImages(Value: TCustomImageList);
    procedure SetItemCheckMode(AValue: TdxGalleryItemCheckMode);
    procedure SetItemImageSize(AValue: TcxSize);
    procedure SetItemShowHint(const Value: Boolean);
    procedure SetItemShowImageFrame(AValue: Boolean);
    procedure SetItemTextPosition(AValue: TcxPosition);
    procedure SetOptionsBehavior(const Value: TdxGalleryControlOptionsBehavior);
    procedure SetOptionsView(const Value: TdxGalleryControlOptionsView);

    procedure ImageListChanged(Sender: TObject);
    procedure GalleryChangeHandler(ASender: TObject; AChangeType: TdxGalleryChangeType);
    procedure GalleryItemClickHandler(ASender: TObject; AItem: TdxGalleryItem);

    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure WMLButtonUp(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonUp(var Message: TWMLButtonDown); message WM_RBUTTONDOWN;
  protected
    function CreateController: TdxGalleryControlController; virtual;
    function CreateGallery: TdxGalleryControlStructure; virtual;
    function CreateOptionsBehavior: TdxGalleryControlOptionsBehavior; virtual;
    function CreateOptionsView: TdxGalleryControlOptionsView; virtual;
    function CreatePainter: TdxGalleryControlPainter; virtual;
    function CreateViewInfo: TdxGalleryControlViewInfo; virtual;

    //Keyb operations
    procedure FocusEnter; override;
    procedure FocusLeave; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    // Mouse operations
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    // TWinControl
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;

    // TcxScrollingControl
    function GetContentSize: TSize; override;
    procedure Calculate(AType: TdxChangeType); override;
    procedure LayoutChanged(AType: TdxChangeType = ctHard); override;
    procedure ScrollPosChanged(const AOffset: TPoint); override;

    // TcxControl
    function AllowTouchScrollUIMode: Boolean; override;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    function GetScrollContentForegroundColor: TColor; override;
    function HasScrollBarArea: Boolean; override;
    procedure SetAutoSizeMode(AValue: TdxAutoSizeMode); override;

    // TdxCustomGalleryControl
    procedure DoClickItem(AItem: TdxGalleryItem); virtual;
    function GetItemAtPos(const P: TPoint): TdxGalleryControlItem;
    function IsUpdateLocked: Boolean;

    // IdxGalleryOwner
    function GetGallery: IdxGallery;
    function GetGallery2: IdxGallery2;

    property CanBeFocused: Boolean read FCanBeFocused write FCanBeFocused;

    property Controller: TdxGalleryControlController read FController;
    property Painter: TdxGalleryControlPainter read FPainter;
    property ViewInfo: TdxGalleryControlViewInfo read FViewInfo;

    property Gallery: TdxGalleryControlStructure read FGallery write SetGallery;
    property Images: TCustomImageList read FImages write SetImages;
    property OptionsBehavior: TdxGalleryControlOptionsBehavior read FOptionsBehavior write SetOptionsBehavior;
    property OptionsView: TdxGalleryControlOptionsView read FOptionsView write SetOptionsView;
    // Obsolete
    property ColumnCount: Integer read GetColumnCount write SetColumnCount;
    property ContentOffset: TcxMargin read GetContentOffset write SetContentOffset;
    property ContentOffsetGroups: TcxMargin read GetContentOffsetGroups write SetContentOffsetGroups;
    property ContentOffsetItems: TcxMargin read GetContentOffsetItems write SetContentOffsetItems;
    property ItemCheckMode: TdxGalleryItemCheckMode read GetItemCheckMode write SetItemCheckMode;
    property ItemImageSize: TcxSize read GetItemImageSize write SetItemImageSize;
    property ItemShowHint: Boolean read GetItemShowHint write SetItemShowHint;
    property ItemShowImageFrame: Boolean read GetItemShowImageFrame write SetItemShowImageFrame;
    property ItemTextPosition: TcxPosition read GetItemTextPosition write SetItemTextPosition;

    property OnItemClick: TdxGalleryControlItemEvent read FOnItemClick write FOnItemClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;

    function CanFocus: Boolean; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;

    procedure MakeItemVisible(AItem: TdxGalleryControlItem);
  end;

  { TdxGalleryControl }

  TdxGalleryControl = class(TdxCustomGalleryControl)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property PopupMenu;
    property Visible;

    property AutoSizeMode default asNone;
    property BorderStyle default cxcbsDefault;
    property Gallery;
    property Images;
    property LookAndFeel;
    property OptionsBehavior;
    property OptionsView;
    property TabOrder;
    property TabStop;
    property Transparent;

    // Obsolete
    property ColumnCount stored False;
    property ContentOffset stored False;
    property ContentOffsetGroups stored False;
    property ContentOffsetItems stored False;
    property ItemCheckMode stored False;
    property ItemImageSize stored False;
    property ItemShowHint stored False;
    property ItemShowImageFrame stored False;
    property ItemTextPosition stored False;

    property OnClick;
    property OnDblClick;
    property OnContextPopup;
    property OnItemClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

uses
  Math, cxScrollBar, cxDrawTextUtils, dxDPIAwareUtils, dxCoreGraphics;

const
  dxcItemGlyphFrameOffset = 2;
  dxcItemIndentBetweenGlyphAndText = 4;

type
  TdxGalleryAccess = class(TdxGallery);

function GetCombinedSize(const AMasterSize, ASlaveSize: TSize): TSize;
begin
  if AMasterSize.cx <> 0 then
    Result.cx := AMasterSize.cx
  else
    Result.cx := ASlaveSize.cx;

  if AMasterSize.cy <> 0 then
    Result.cy := AMasterSize.cy
  else
    Result.cy := ASlaveSize.cy;
end;

{ TdxGalleryPersistent }

constructor TdxGalleryPersistent.Create(AOwner: TdxCustomGalleryControl);
begin
  inherited Create(AOwner);
end;

function TdxGalleryPersistent.GetOwnerControl: TdxCustomGalleryControl;
begin
  Result := inherited Owner as TdxCustomGalleryControl;
end;

{ TdxGalleryCustomViewInfo }

procedure TdxGalleryCustomViewInfo.Draw(ACanvas: TcxCanvas);
begin
  if ACanvas.RectVisible(Bounds) then
    DrawContent(ACanvas);
end;

function TdxGalleryCustomViewInfo.GetContentOffset: TRect;
begin
  Result := OptionsView.ContentOffset.Margin;
end;

function TdxGalleryCustomViewInfo.GetContentOffsetGroups: TRect;
begin
  Result := OptionsView.ContentOffsetGroups.Margin;
end;

function TdxGalleryCustomViewInfo.GetContentOffsetItems: TRect;
begin
  Result := OptionsView.ContentOffsetItems.Margin;
end;

function TdxGalleryCustomViewInfo.GetOptionsItemImage: TdxGalleryControlOptionsItemImage;
begin
  Result := OptionsView.Item.Image;
end;

function TdxGalleryCustomViewInfo.GetOptionsItemText: TdxGalleryControlOptionsItemText;
begin
  Result := OptionsView.Item.Text;
end;

function TdxGalleryCustomViewInfo.GetOptionsView: TdxGalleryControlOptionsView;
begin
  Result := GalleryControl.OptionsView;
end;

function TdxGalleryCustomViewInfo.GetPainter: TdxGalleryControlPainter;
begin
  Result := GalleryControl.Painter;
end;

function TdxGalleryCustomViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := GalleryControl.ScaleFactor;
end;

function TdxGalleryCustomViewInfo.GetUseRightToLeftAlignment: Boolean;
begin
  Result := GalleryControl.UseRightToLeftAlignment;
end;

{ TdxGalleryItemViewInfo }

constructor TdxGalleryItemViewInfo.Create(AItem: TdxGalleryControlItem);
begin
  inherited Create;
  FItem := AItem;
end;

destructor TdxGalleryItemViewInfo.Destroy;
begin
  FreeAndNil(FCacheGlyph);
  inherited Destroy;
end;

procedure TdxGalleryItemViewInfo.Calculate(AType: TdxChangeType; const ABounds: TRect);
begin
  if AType <> ctLight then
    ResetCache;

  FBounds := ABounds;
  FState := GalleryControl.Controller.GetItemViewState(Item);
  FContentBounds := cxRectContent(Bounds, ContentOffsetItems);
  CalculateTextArea(GalleryControl.ViewInfo.TextAreaSize);
  CalculateGlyphArea(GlyphSize);

  if UseRightToLeftAlignment then
  begin
    FContentBounds := TdxRightToLeftLayoutConverter.ConvertRect(FContentBounds, Bounds);
    FGlyphFrameRect := TdxRightToLeftLayoutConverter.ConvertRect(FGlyphFrameRect, Bounds);
    FGlyphRect := TdxRightToLeftLayoutConverter.ConvertRect(FGlyphRect, Bounds);
    FCaptionRect := TdxRightToLeftLayoutConverter.ConvertRect(FCaptionRect, Bounds);
    FDescriptionRect := TdxRightToLeftLayoutConverter.ConvertRect(FDescriptionRect, Bounds);
    FTextArea := TdxRightToLeftLayoutConverter.ConvertRect(FTextArea, Bounds);
  end;
end;

procedure TdxGalleryItemViewInfo.CalculateTextAreaSizeLimitedByRowCount(ACanvas: TcxCanvas; ARowCount: Integer);
var
  R: TRect;
begin
  if OptionsItemText.WordWrap then
    FCaptionSize := cxSize(cxGetTextRect(ACanvas.Handle, Caption, ARowCount))
  else
    FCaptionSize := cxTextSize(ACanvas.Handle, Caption);

  if Caption <> '' then
  begin
    R := cxRect(0, 0, FCaptionSize.cx, 1);
    cxDrawText(ACanvas.Handle, Description, R, DT_CALCRECT or DT_WORDBREAK);
    FDescriptionSize := cxSize(R);
  end
  else
    FDescriptionSize := cxSize(cxGetTextRect(ACanvas.Handle, Description, ARowCount));
end;

procedure TdxGalleryItemViewInfo.CalculateTextAreaSizeLimitedByWidth(ACanvas: TcxCanvas; AMaxWidth: Integer);
const
  WordWrapMap: array[Boolean] of Integer = (0, DT_WORDBREAK);
var
  R: TRect;
begin
  R := cxRect(0, 0, AMaxWidth, 1);
  cxDrawText(ACanvas.Handle, Caption, R, DT_CALCRECT or WordWrapMap[OptionsItemText.WordWrap]);
  FCaptionSize := cxSize(R);

  R := cxRect(0, 0, AMaxWidth, 1);
  cxDrawText(ACanvas.Handle, Description, R, DT_CALCRECT or DT_WORDBREAK);
  FDescriptionSize := cxSize(R)
end;

procedure TdxGalleryItemViewInfo.CalculateGlyphArea(const AGlyphSize: TSize);

 function MinOffset(const ARect: TRect): Integer;
 begin
   Result := Min(ARect.Left, ARect.Right);
   Result := Min(Result, ARect.Top);
   Result := Min(Result, ARect.Bottom);
 end;

var
  AGlyphFrameOffset: Integer;
begin
  FGlyphRect := cxNullRect;
  FGlyphFrameRect := cxNullRect;
  if not cxSizeIsEmpty(AGlyphSize) then
  begin
    AGlyphFrameOffset := Min(ScaleFactor.Apply(dxcItemGlyphFrameOffset), MinOffset(ContentOffsetItems));

    FGlyphRect := ContentBounds;
    case OptionsItemText.Position of
      posLeft:
        FGlyphRect.Left := TextArea.Right + AGlyphFrameOffset + ScaleFactor.Apply(dxcItemIndentBetweenGlyphAndText);
      posRight:
        FGlyphRect.Right := TextArea.Left - AGlyphFrameOffset - ScaleFactor.Apply(dxcItemIndentBetweenGlyphAndText);
      posBottom:
        FGlyphRect.Bottom := TextArea.Top - AGlyphFrameOffset - ScaleFactor.Apply(dxcItemIndentBetweenGlyphAndText);
      posTop:
        FGlyphRect.Top := TextArea.Bottom + AGlyphFrameOffset + ScaleFactor.Apply(dxcItemIndentBetweenGlyphAndText);
    end;
    FGlyphRect := cxGetImageRect(GlyphRect, GlyphSize, ifmFit);

    if OptionsItemImage.ShowFrame then
      FGlyphFrameRect := cxRectInflate(GlyphRect, AGlyphFrameOffset, AGlyphFrameOffset);
  end;
end;

procedure TdxGalleryItemViewInfo.CalculateTextArea(const ATextAreaSize: TSize);
var
  R: TRect;
begin
  case OptionsItemText.Position of
    posLeft:
      FTextArea := cxRectSetWidth(ContentBounds, ATextAreaSize.cx);
    posRight:
      FTextArea := cxRectSetRight(ContentBounds, ContentBounds.Right, ATextAreaSize.cx);
    posBottom:
      FTextArea := cxRectSetBottom(ContentBounds, ContentBounds.Bottom, ATextAreaSize.cy);
    posTop:
      FTextArea := cxRectSetHeight(ContentBounds, ATextAreaSize.cy);
  else
    FTextArea := cxNullRect;
  end;

  case OptionsItemText.AlignVert of
    vaBottom:
      R := cxRectSetBottom(TextArea, TextArea.Bottom, TextAreaSize.cy);
    vaCenter:
      R := cxRectCenterVertically(TextArea, TextAreaSize.cy);
  else
    R := cxRectSetHeight(TextArea, TextAreaSize.cy);
  end;

  CalculateTextAreaContent(R);
end;

procedure TdxGalleryItemViewInfo.CalculateTextAreaContent(const ABounds: TRect);
begin
  FCaptionRect := cxRectSetSize(ABounds, cxRectWidth(ABounds), CaptionSize.cy);
  FDescriptionRect := cxRectSetTop(ABounds, CaptionRect.Bottom + DescriptionOffset, DescriptionSize.cy);
  FDescriptionRect := cxRectSetWidth(DescriptionRect, cxRectWidth(ABounds));
end;

procedure TdxGalleryItemViewInfo.DrawContent(ACanvas: TcxCanvas);
begin
  Painter.DrawItem(ACanvas, Self);
end;

function TdxGalleryItemViewInfo.GetDescription: string;
begin
  Result := Item.Description;
end;

function TdxGalleryItemViewInfo.GetGalleryControl: TdxCustomGalleryControl;
begin
  Result := Item.GalleryControl;
end;

function TdxGalleryItemViewInfo.GetTextAreaSize: TSize;
begin
  Result.cx := Max(CaptionSize.cx, DescriptionSize.cx);
  Result.cy := CaptionSize.cy + DescriptionSize.cy + DescriptionOffset;
end;

procedure TdxGalleryItemViewInfo.ResetCache;
begin
  FreeAndNil(FCacheGlyph);
end;

function TdxGalleryItemViewInfo.GetCacheGlyph: TdxSmartGlyph;
var
  ABitmap: TcxBitmap32;
  AColorPalette: IdxColorPalette;
begin
  AColorPalette := Painter.LookAndFeelPainter.GetGalleryItemColorPalette(State);
  if (FCacheGlyph = nil) or (FCacheGlyphColorPalette <> Pointer(AColorPalette)) then
  begin
    ABitmap := TcxBitmap32.CreateSize(GlyphRect, True);
    try
      ABitmap.Canvas.Lock;
      try
        cxDrawImage(ABitmap.cxCanvas, ABitmap.ClientRect, Item.Glyph, Item.Images,
          Item.ImageIndex, ifmFit, EnabledImageDrawModeMap[State.Enabled], False, AColorPalette, ScaleFactor);
      finally
        ABitmap.Canvas.Unlock;
      end;
      FreeAndNil(FCacheGlyph);
      FCacheGlyph := TdxSmartGlyph.CreateFromBitmap(ABitmap);
      FCacheGlyph.SourceDPI := ScaleFactor.Apply(dxDefaultDPI);
      FCacheGlyphColorPalette := Pointer(AColorPalette);
    finally
      ABitmap.Free;
    end;
  end;
  Result := FCacheGlyph;
end;

function TdxGalleryItemViewInfo.GetCaption: string;
begin
  Result := Item.Caption;
end;

function TdxGalleryItemViewInfo.GetDescriptionOffset: Integer;
begin
  if (CaptionSize.cy > 0) and (DescriptionSize.cy > 0) then
    Result := ScaleFactor.Apply(cxTextOffset)
  else
    Result := 0;
end;

function TdxGalleryItemViewInfo.GetGlyphSize: TSize;
begin
  Result := dxGetImageSize(Item.Glyph, Item.Images, Item.ImageIndex, ScaleFactor);
end;

{ TdxGalleryControlItem }

constructor TdxGalleryControlItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FViewInfo := CreateViewInfo;
end;

destructor TdxGalleryControlItem.Destroy;
begin
  FreeAndNil(FViewInfo);
  inherited Destroy;
end;

function TdxGalleryControlItem.CreateViewInfo: TdxGalleryItemViewInfo;
begin
  Result := TdxGalleryItemViewInfo.Create(Self);
end;

function TdxGalleryControlItem.GetGroup: TdxGalleryControlGroup;
begin
  Result := inherited Group as TdxGalleryControlGroup;
end;

function TdxGalleryControlItem.GetImages: TCustomImageList;
begin
  Result := GalleryControl.Images;
end;

function TdxGalleryControlItem.GetGalleryControl: TdxCustomGalleryControl;
begin
  Result := Gallery.GetParentComponent as TdxCustomGalleryControl
end;

procedure TdxGalleryControlItem.SetGroup(AGroup: TdxGalleryControlGroup);
begin
  inherited SetGroup(AGroup);
end;

{ TdxGalleryControlItems }

function TdxGalleryControlItems.Add: TdxGalleryControlItem;
begin
  Result := inherited Add as TdxGalleryControlItem;
end;

function TdxGalleryControlItems.GetItemAtPos(const P: TPoint): TdxGalleryControlItem;
var
  AItem: TdxGalleryControlItem;
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if cxRectPtIn(AItem.ViewInfo.Bounds, P) then
      Exit(AItem);
  end;
  Result := nil;
end;

procedure TdxGalleryControlItems.SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1);
begin
  inherited SetItemName(AItem, Count);
end;

function TdxGalleryControlItems.GetItem(AIndex: Integer): TdxGalleryControlItem;
begin
  Result := inherited Items[AIndex] as TdxGalleryControlItem;
end;

procedure TdxGalleryControlItems.SetItem(AIndex: Integer; AValue: TdxGalleryControlItem);
begin
  inherited Items[AIndex] := AValue;
end;

{ TdxGalleryGroupViewInfo }

constructor TdxGalleryGroupViewInfo.Create(AGroup: TdxGalleryControlGroup);
begin
  inherited Create;
  FGroup := AGroup;
end;

procedure TdxGalleryGroupViewInfo.Calculate(AType: TdxChangeType; const ABounds: TRect);
begin
  FBounds := ABounds;
  if Group.Visible then
  begin
    CalculateCaption;
    FColumnCount := GalleryControl.ViewInfo.ColumnCount;
    FRowCount := Group.ItemCount div ColumnCount + Ord(Group.ItemCount mod ColumnCount <> 0);
    FItemsRect := cxRect(Bounds.Left, CaptionRect.Bottom, 0, 0);
    FItemsRect := cxRectContent(ItemsRect, ContentOffsetGroups);
    FItemsRect := cxRectSetSize(ItemsRect, ColumnCount * ItemSize.cx, RowCount * ItemSize.cy);
    CalculateItems(AType);
    FBounds.Bottom := ItemsRect.Bottom + ContentOffsetGroups.Bottom;
  end
  else
    FBounds.Bottom := FBounds.Top;
end;

function TdxGalleryGroupViewInfo.GetMaxColumnCount: Integer;
begin
  Result := Group.ItemCount;
end;

procedure TdxGalleryGroupViewInfo.CalculateCaption;
begin
  FCaptionRect := cxRectSetHeight(Bounds, GetCaptionHeight);
  FCaptionTextRect := cxRectContent(CaptionRect, CaptionTextOffsets);

  if UseRightToLeftAlignment then
  begin
    FCaptionRect := TdxRightToLeftLayoutConverter.ConvertRect(FCaptionRect, Bounds);
    FCaptionTextRect := TdxRightToLeftLayoutConverter.ConvertRect(FCaptionTextRect, Bounds);
  end;
end;

procedure TdxGalleryGroupViewInfo.CalculateItems(AType: TdxChangeType);
var
  I: Integer;
begin
  for I := 0 to Group.ItemCount - 1 do
    PlaceItem(Group.Items[I], AType, ItemsRect, I);
end;

procedure TdxGalleryGroupViewInfo.DrawContent(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  if not cxRectIsEmpty(CaptionRect) then
    Painter.DrawGroupHeader(ACanvas, Self);
  for I := 0 to Group.ItemCount - 1 do
    Group.Items[I].ViewInfo.Draw(ACanvas);
end;

function TdxGalleryGroupViewInfo.GetCaptionHeight: Integer;
begin
  if Group.ShowCaption and (Caption <> '') then
    Result := cxTextHeight(Font) + cxMarginsHeight(CaptionTextOffsets)
  else
    Result := 0;
end;

function TdxGalleryGroupViewInfo.GetCaptionTextOffsets: TRect;
begin
  Result := Painter.GetGroupHeaderContentOffsets;
end;

function TdxGalleryGroupViewInfo.GetFont: TFont;
begin
  Result := GalleryControl.Font;
end;

function TdxGalleryGroupViewInfo.GetGalleryControl: TdxCustomGalleryControl;
begin
  Result := Group.GalleryControl;
end;

procedure TdxGalleryGroupViewInfo.PlaceItem(AItem: TdxGalleryControlItem;
  AChangeType: TdxChangeType; const AItemsArea: TRect; ACellIndex: Integer);
var
  X, Y, ARow, AColumn: Integer;
begin
  ARow := ACellIndex div ColumnCount;
  AColumn := ACellIndex - ARow * ColumnCount;

  Y := AItemsArea.Top + ItemSize.cy * ARow;
  if UseRightToLeftAlignment then
    X := AItemsArea.Right - ItemSize.cx * (AColumn + 1)
  else
    X := AItemsArea.Left + ItemSize.cx * AColumn;

  AItem.ViewInfo.FCellPositionInGroup := cxPoint(AColumn, ARow);
  AItem.ViewInfo.Calculate(AChangeType, cxRectBounds(X, Y, ItemSize));
end;

function TdxGalleryGroupViewInfo.GetCaption: string;
begin
  Result := Group.Caption;
end;

function TdxGalleryGroupViewInfo.GetSize: TSize;
begin
  Result := GalleryControl.ViewInfo.ItemSize;
end;

{ TdxGalleryGroup }

constructor TdxGalleryControlGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FViewInfo := CreateViewInfo;
end;

destructor TdxGalleryControlGroup.Destroy;
begin
  FreeAndNil(FViewInfo);
  inherited Destroy;
end;

function TdxGalleryControlGroup.CreateViewInfo: TdxGalleryGroupViewInfo;
begin
  Result := TdxGalleryGroupViewInfo.Create(Self);
end;

function TdxGalleryControlGroup.GetGalleryItemClass: TdxGalleryItemClass;
begin
  Result := TdxGalleryControlItem;
end;

function TdxGalleryControlGroup.GetGalleryItemsClass: TdxGalleryItemsClass;
begin
  Result := TdxGalleryControlItems;
end;

function TdxGalleryControlGroup.GetGalleryControl: TdxCustomGalleryControl;
begin
  Result := Gallery.GetParentComponent as TdxCustomGalleryControl
end;

function TdxGalleryControlGroup.GetItems: TdxGalleryControlItems;
begin
  Result := inherited Items as TdxGalleryControlItems;
end;

{ TdxGalleryControlGroups }

function TdxGalleryControlGroups.Add: TdxGalleryControlGroup;
begin
  Result := inherited Add as TdxGalleryControlGroup;
end;

function TdxGalleryControlGroups.FindByCaption(
  const ACaption: string; out AGroup: TdxGalleryControlGroup): Boolean;
begin
  Result := inherited FindByCaption(ACaption, TdxGalleryGroup(AGroup));
end;

function TdxGalleryControlGroups.GetGroupAtPos(const P: TPoint): TdxGalleryControlGroup;
var
  AGroup: TdxGalleryControlGroup;
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    AGroup := Groups[I];
    if AGroup.Visible and PtInRect(AGroup.ViewInfo.Bounds, P) then
    begin
      Result := AGroup;
      Break;
    end;
  end;
end;

function TdxGalleryControlGroups.GetItemAtPos(const P: TPoint): TdxGalleryControlItem;
var
  AGroup: TdxGalleryControlGroup;
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    AGroup := Groups[I];
    if AGroup.Visible then
    begin
      Result := AGroup.Items.GetItemAtPos(P);
      if Result <> nil then
        Break;
    end;
  end;
end;

function TdxGalleryControlGroups.GetGroup(AIndex: Integer): TdxGalleryControlGroup;
begin
  Result := inherited Groups[AIndex] as TdxGalleryControlGroup;
end;

procedure TdxGalleryControlGroups.SetGroup(AIndex: Integer; AValue: TdxGalleryControlGroup);
begin
  Groups[AIndex] := AValue;
end;

{ TdxGalleryControlStructure }

function TdxGalleryControlStructure.GetCheckedItem: TdxGalleryControlItem;
begin
  Result := inherited GetCheckedItem as TdxGalleryControlItem;
end;

function TdxGalleryControlStructure.GetFirstItem: TdxGalleryControlItem;
begin
  Result := inherited GetFirstItem as TdxGalleryControlItem;
end;

function TdxGalleryControlStructure.GetFirstVisibleItem: TdxGalleryControlItem;
begin
  Result := inherited GetFirstVisibleItem as TdxGalleryControlItem;
end;

function TdxGalleryControlStructure.GetGroupClass: TdxGalleryGroupClass;
begin
  Result := TdxGalleryControlGroup;
end;

function TdxGalleryControlStructure.GetGroupsClass: TdxGalleryGroupsClass;
begin
  Result := TdxGalleryControlGroups;
end;

function TdxGalleryControlStructure.GetGroups: TdxGalleryControlGroups;
begin
  Result := inherited Groups as TdxGalleryControlGroups;
end;

{ TdxGalleryControlPainter }

procedure TdxGalleryControlPainter.DrawBackground(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  LookAndFeelPainter.DrawGalleryBackground(ACanvas, ABounds);
end;

procedure TdxGalleryControlPainter.DrawGroupHeader(ACanvas: TcxCanvas; const AViewInfo: TdxGalleryGroupViewInfo);
begin
  LookAndFeelPainter.DrawGalleryGroupHeader(ACanvas, AViewInfo.CaptionRect);
  DrawGroupHeaderText(ACanvas, AViewInfo);
end;

procedure TdxGalleryControlPainter.DrawGroupHeaderText(ACanvas: TcxCanvas; const AViewInfo: TdxGalleryGroupViewInfo);
var
  ARect: TRect;
begin
  ACanvas.SaveState;
  try
    ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];
    ACanvas.Font.Color := GetGroupCaptionTextColor;
    ARect := AViewInfo.CaptionTextRect;
    cxTextOut(ACanvas.Handle, AViewInfo.Caption, ARect,
      IfThen(UseRightToLeftAlignment, CXTO_RIGHT or CXTO_RTLREADING, CXTO_LEFT) or
      CXTO_CENTER_VERTICALLY or CXTO_END_ELLIPSIS or CXTO_SINGLELINE);
  finally
    ACanvas.RestoreState;
  end;
end;

function TdxGalleryControlPainter.GetGroupHeaderContentOffsets: TRect;
begin
  Result := LookAndFeelPainter.GetGalleryScaledGroupHeaderContentOffsets(ScaleFactor);
end;

procedure TdxGalleryControlPainter.DrawItem(ACanvas: TcxCanvas; AViewInfo: TdxGalleryItemViewInfo);
begin
  if DrawItemSelectionFirst then
    DrawItemSelection(ACanvas, AViewInfo);
  DrawItemImage(ACanvas, AViewInfo);
  if not DrawItemSelectionFirst then
    DrawItemSelection(ACanvas, AViewInfo);
  DrawItemText(ACanvas, AViewInfo);
end;

procedure TdxGalleryControlPainter.DrawItemImage(ACanvas: TcxCanvas; AViewInfo: TdxGalleryItemViewInfo);
begin
  if not cxRectIsEmpty(AViewInfo.GlyphFrameRect) then
    LookAndFeelPainter.DrawGalleryItemImageFrame(ACanvas, AViewInfo.GlyphFrameRect);
  AViewInfo.CacheGlyph.StretchDraw(ACanvas.Handle, AViewInfo.GlyphRect);
end;

procedure TdxGalleryControlPainter.DrawItemSelection(ACanvas: TcxCanvas; AViewInfo: TdxGalleryItemViewInfo);
begin
  LookAndFeelPainter.DrawGalleryItemSelection(ACanvas, AViewInfo.Bounds, AViewInfo.State);
end;

procedure TdxGalleryControlPainter.DrawItemText(ACanvas: TcxCanvas; AViewInfo: TdxGalleryItemViewInfo);

  function GetDrawTextFlags(AAlignment: TAlignment; AWordWrap: Boolean): Integer;
  const
    TextAlignHorzMap: array[TAlignment] of Integer = (CXTO_LEFT, CXTO_RIGHT, CXTO_CENTER_HORIZONTALLY);
  begin
    Result := CXTO_PREVENT_LEFT_EXCEED or CXTO_PREVENT_TOP_EXCEED or CXTO_CENTER_VERTICALLY;
    if AWordWrap then
      Result := Result or CXTO_WORDBREAK;
    if UseRightToLeftAlignment then
    begin
      ChangeBiDiModeAlignment(AAlignment);
      Result := Result or CXTO_RTLREADING;
    end;
    Result := Result or TextAlignHorzMap[AAlignment];
  end;

  procedure DrawLine(R: TRect; const S: string; AColor: TColor; AWordWrap: Boolean);
  begin
    if ACanvas.RectVisible(R) then
      cxTextOut(ACanvas.Canvas, S, R, GetDrawTextFlags(AViewInfo.OptionsItemText.AlignHorz, AWordWrap), nil, 0, 0, 0, AColor);
  end;

begin
  DrawLine(AViewInfo.CaptionRect, AViewInfo.Caption, GetItemCaptionTextColor(AViewInfo), AViewInfo.OptionsItemText.WordWrap);
  DrawLine(AViewInfo.DescriptionRect, AViewInfo.Description, GetItemDescriptionTextColor(AViewInfo), True);
end;

function TdxGalleryControlPainter.DrawItemSelectionFirst: Boolean;
begin
  Result := LookAndFeelPainter.DrawGalleryItemSelectionFirst;
end;

function TdxGalleryControlPainter.GetGroupCaptionTextColor: TColor;
begin
  Result := LookAndFeelPainter.GetGalleryGroupTextColor;
end;

function TdxGalleryControlPainter.GetItemCaptionTextColor(AViewInfo: TdxGalleryItemViewInfo): TColor;
begin
  Result := LookAndFeelPainter.GetGalleryItemCaptionTextColor(AViewInfo.State);
end;

function TdxGalleryControlPainter.GetItemDescriptionTextColor(AViewInfo: TdxGalleryItemViewInfo): TColor;
begin
  Result := LookAndFeelPainter.GetGalleryItemDescriptionTextColor(AViewInfo.State);
end;

function TdxGalleryControlPainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Owner.LookAndFeelPainter;
end;

function TdxGalleryControlPainter.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.ScaleFactor;
end;

function TdxGalleryControlPainter.GetUseRightToLeftAlignment: Boolean;
begin
  Result := Owner.UseRightToLeftAlignment;
end;

{ TdxGalleryControlViewInfo }

constructor TdxGalleryControlViewInfo.Create(AGalleryControl: TdxCustomGalleryControl);
begin
  inherited Create;
  FGalleryControl := AGalleryControl;
end;

procedure TdxGalleryControlViewInfo.Calculate(AType: TdxChangeType; const ABounds: TRect);
begin
  FBounds := ABounds;
  if AType <> ctLight then
  begin
    CalculateItemSize;
    CalculateColumnCount;
  end;
  CalculateContentBounds(AType);
end;

procedure TdxGalleryControlViewInfo.CalculateColumnCount;
begin
  FColumnCount := GalleryControl.ColumnCount;
  if ColumnCount = 0 then
  begin
    if AutoWidth then
      FColumnCount := 5
    else
      FColumnCount := GetAvailableGroupsAreaWidth div ItemSize.cx;

    if ColumnAutoWidth then
      FColumnCount := Min(FColumnCount, MaxColumnCount);
    FColumnCount := Max(1, FColumnCount);
  end;
end;

procedure TdxGalleryControlViewInfo.CalculateContentBounds(AType: TdxChangeType);
var
  AContentWidth: Integer;
  AGroupViewInfo: TdxGalleryGroupViewInfo;
  ARect: TRect;
  I: Integer;
begin
  if GalleryControl.HandleAllocated and (Groups.Count > 0) then
  begin
    if ColumnAutoWidth then
    begin
      AContentWidth := cxRectWidth(Bounds) - cxMarginsWidth(ContentOffset);
      if (GalleryControl.GetScrollbarMode = sbmHybrid) and GalleryControl.IsScrollBarActive(sbVertical) then
        Dec(AContentWidth, GalleryControl.GetVScrollBarAreaWidth);
    end
    else
      AContentWidth := ColumnCount * ItemSize.cx + cxMarginsWidth(ContentOffsetGroups);

    FContentBounds.Top := Bounds.Top - GalleryControl.TopPos + ContentOffset.Top;
    FContentBounds.Bottom := cxMaxRectSize;
    if UseRightToLeftAlignment then
    begin
      FContentBounds.Right := Bounds.Right + GalleryControl.LeftPos - ContentOffset.Right;
      FContentBounds.Left := ContentBounds.Right - AContentWidth;
    end
    else
    begin
      FContentBounds.Left := Bounds.Left - GalleryControl.LeftPos + ContentOffset.Left;
      FContentBounds.Right := ContentBounds.Left + AContentWidth;
    end;

    ARect := ContentBounds;
    for I := 0 to Groups.Count - 1 do
    begin
      AGroupViewInfo := Groups[I].ViewInfo;
      AGroupViewInfo.Calculate(AType, ARect);
      ARect.Top := AGroupViewInfo.Bounds.Bottom;
    end;

    FContentBounds.Bottom := ARect.Top;
  end
  else
    FContentBounds := cxEmptyRect;
end;

procedure TdxGalleryControlViewInfo.CalculateItemSize;
var
  ADeltaSize: Integer;
begin
  FImageSize := CalculateMaxItemImageSize;
  FTextAreaSize := CalculateMaxItemTextAreaSize(ImageSize);

  if (ImageSize.cx = 0) and (TextAreaSize.cx = 0) then
    FImageSize.cx := ScaleFactor.Apply(16);
  if (ImageSize.cy = 0) and (TextAreaSize.cy = 0) then
    FImageSize.cy := ScaleFactor.Apply(16);

  if ColumnAutoWidth then
  begin
    FItemSize := DoCalculateItemSize;
    CalculateColumnCount;
    ADeltaSize := (GetAvailableGroupsAreaWidth - ItemSize.cx * ColumnCount) div ColumnCount;
    if OptionsItemText.Position = posNone then
      FImageSize.cx := Max(0, ImageSize.cx + ADeltaSize)
    else
      FTextAreaSize.cx := Max(0, TextAreaSize.cx + ADeltaSize);
  end;

  FTextAreaSize.cy := CalculateMaxItemTextAreaSizeLimitedByWidth(TextAreaSize.cx).cy;
  FItemSize := DoCalculateItemSize;
end;

function TdxGalleryControlViewInfo.CalculateMaxItemImageSize: TSize;

  function GetItemImageSize(AItem: TdxGalleryControlItem): TSize;
  begin
    Result := dxGetImageSize(AItem.Glyph, AItem.Images, AItem.ImageIndex, ScaleFactor);
  end;

var
  AGroup: TdxGalleryControlGroup;
  I, J: Integer;
begin
  Result := OptionsItemImage.Size.Size;
  if cxSizeIsEmpty(Result) then
  begin
    for I := 0 to Groups.Count - 1 do
    begin
      AGroup := Groups[I];
      for J := 0 to AGroup.ItemCount - 1 do
        Result := cxSizeMax(Result, GetItemImageSize(AGroup.Items[J]));
    end;
  end;
end;

function TdxGalleryControlViewInfo.CalculateMaxItemTextAreaSize(const AImageSize: TSize): TSize;
begin
  case OptionsItemText.Position of
    posTop, posBottom:
      Result := CalculateMaxItemTextAreaSizeLimitedByWidth(GetTextAreaMaxWidth(AImageSize));
    posLeft, posRight:
      Result := CalculateMaxItemTextAreaSizeLimitedByRowCount(GetTextAreaMaxRowCount(AImageSize));
  else
    Result := cxNullSize;
  end;
end;

function TdxGalleryControlViewInfo.CalculateMaxItemTextAreaSizeLimitedByRowCount(ARowCount: Integer): TSize;
var
  AGroup: TdxGalleryControlGroup;
  AItemViewInfo: TdxGalleryItemViewInfo;
  I, J: Integer;
begin
  Result := cxNullSize;
  if OptionsItemText.Position <> posNone then
  begin
    cxScreenCanvas.Font := Font;
    try
      for I := 0 to Groups.Count - 1 do
      begin
        AGroup := Groups[I];
        for J := 0 to AGroup.ItemCount - 1 do
        begin
          AItemViewInfo := AGroup.Items[J].ViewInfo;
          AItemViewInfo.CalculateTextAreaSizeLimitedByRowCount(cxScreenCanvas, ARowCount);
          Result := cxSizeMax(Result, AItemViewInfo.TextAreaSize);
        end;
      end;
    finally
      cxScreenCanvas.Dormant;
    end;
  end;
end;

function TdxGalleryControlViewInfo.CalculateMaxItemTextAreaSizeLimitedByWidth(AMaxWidth: Integer): TSize;
var
  AGroup: TdxGalleryControlGroup;
  AItemViewInfo: TdxGalleryItemViewInfo;
  I, J: Integer;
begin
  Result := cxNullSize;
  if OptionsItemText.Position <> posNone then
  begin
    AMaxWidth := Max(1, AMaxWidth);
    cxScreenCanvas.Font := Font;
    try
      for I := 0 to Groups.Count - 1 do
      begin
        AGroup := Groups[I];
        for J := 0 to AGroup.ItemCount - 1 do
        begin
          AItemViewInfo := AGroup.Items[J].ViewInfo;
          AItemViewInfo.CalculateTextAreaSizeLimitedByWidth(cxScreenCanvas, AMaxWidth);
          Result := cxSizeMax(Result, AItemViewInfo.TextAreaSize);
        end;
      end;
    finally
      cxScreenCanvas.Dormant;
    end;
  end;
end;

function TdxGalleryControlViewInfo.DoCalculateItemSize: TSize;

  function CalculateItemHeight(AMarginTop, AMarginBottom, ATextHeight, AImageHeight: Integer): Integer;
  begin
    Result := AMarginTop + AMarginBottom + Max(ATextHeight, AImageHeight);
  end;

  function CalculateItemWidth(AMarginLeft, AMarginRight, ATextWidth, AImageWidth: Integer): Integer;
  begin
    Result := AMarginLeft + AMarginRight + ATextWidth + AImageWidth +
      ScaleFactor.Apply(dxcItemIndentBetweenGlyphAndText) +
      Min(Min(AMarginLeft, AMarginRight), ScaleFactor.Apply(dxcItemGlyphFrameOffset));
  end;

begin
  case OptionsItemText.Position of
    posLeft, posRight:
      begin
        Result.cx := CalculateItemWidth(ContentOffsetItems.Left, ContentOffsetItems.Right, TextAreaSize.cx, ImageSize.cx);
        Result.cy := CalculateItemHeight(ContentOffsetItems.Top, ContentOffsetItems.Bottom, TextAreaSize.cy, ImageSize.cy);
      end;

    posTop, posBottom:
      begin
        Result.cx := CalculateItemHeight(ContentOffsetItems.Left, ContentOffsetItems.Right, TextAreaSize.cx, ImageSize.cx);
        Result.cy := CalculateItemWidth(ContentOffsetItems.Top, ContentOffsetItems.Bottom, TextAreaSize.cy, ImageSize.cy);
      end;

  else // posNone
    begin
      Result.cx := ImageSize.cx + cxMarginsWidth(ContentOffsetItems);
      Result.cy := ImageSize.cy + cxMarginsHeight(ContentOffsetItems);
    end;
  end;
end;

procedure TdxGalleryControlViewInfo.DrawContent(ACanvas: TcxCanvas);
var
  AGroup: TdxGalleryControlGroup;
  I: Integer;
begin
  if not FGalleryControl.Transparent then
    Painter.DrawBackground(ACanvas, Bounds);
  for I := 0 to Groups.Count - 1 do
  begin
    AGroup := Groups[I];
    if AGroup.Visible then
      AGroup.ViewInfo.Draw(ACanvas);
  end;
end;

function TdxGalleryControlViewInfo.GetAvailableGroupsAreaWidth: Integer;
var
  AOffset: Integer;
begin
  AOffset := cxMarginsWidth(BorderWidths) + cxMarginsWidth(ContentOffsetGroups);
  if not GalleryControl.IsTouchScrollUIMode and not AutoHeight then
  begin
    if not ColumnAutoWidth or GalleryControl.IsScrollBarActive(sbVertical) then
      Inc(AOffset, GalleryControl.GetVScrollBarDefaultAreaWidth);
  end;
  Result := GalleryControl.Width - AOffset;
end;

function TdxGalleryControlViewInfo.GetBorderWidths: TRect;
var
  ABorderSize: Integer;
begin
  Result := ContentOffset;
  ABorderSize := GalleryControl.BorderSize;
  Inc(Result.Bottom, ABorderSize);
  Inc(Result.Left, ABorderSize);
  Inc(Result.Right, ABorderSize);
  Inc(Result.Top, ABorderSize);
end;

function TdxGalleryControlViewInfo.GetGalleryControl: TdxCustomGalleryControl;
begin
  Result := FGalleryControl;
end;

function TdxGalleryControlViewInfo.GetTextAreaMaxRowCount(const AImageSize: TSize): Integer;
begin
  Result := Max(1, AImageSize.cy div cxTextHeight(Font));
end;

function TdxGalleryControlViewInfo.GetTextAreaMaxWidth(const AImageSize: TSize): Integer;
begin
  Result := Max(AImageSize.cx, cxTextWidth(Font, 'W') * 3);
end;

function TdxGalleryControlViewInfo.GetColumnAutoWidth: Boolean;
begin
  Result := OptionsView.ColumnAutoWidth and not AutoWidth;
end;

function TdxGalleryControlViewInfo.GetFont: TFont;
begin
  Result := GalleryControl.Font;
end;

function TdxGalleryControlViewInfo.GetGroups: TdxGalleryControlGroups;
begin
  Result := GalleryControl.Gallery.Groups;
end;

function TdxGalleryControlViewInfo.GetAutoHeight: Boolean;
begin
  Result := GalleryControl.AutoSizeMode in [asAutoHeight, asAutoSize];
end;

function TdxGalleryControlViewInfo.GetAutoWidth: Boolean;
begin
  Result := GalleryControl.AutoSizeMode in [asAutoWidth, asAutoSize];
end;

function TdxGalleryControlViewInfo.GetMaxColumnCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Groups.Count - 1 do
    Result := Max(Result, Groups[I].ViewInfo.GetMaxColumnCount);
end;

function TdxGalleryControlViewInfo.GetRowCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Groups.Count - 1 do
    Inc(Result, Groups[I].ViewInfo.RowCount);
end;

{ TdxGalleryControlNavigationMatrix }

constructor TdxGalleryControlNavigationMatrix.Create(AViewInfo: TdxGalleryControlViewInfo);
begin
  inherited Create;
  FRowCount := AViewInfo.RowCount;
  FColumnCount := AViewInfo.ColumnCount;
  SetLength(FValues, ColumnCount, RowCount);
  Populate(AViewInfo);
end;

destructor TdxGalleryControlNavigationMatrix.Destroy;
begin
  FValues := nil;
  inherited Destroy;
end;

function TdxGalleryControlNavigationMatrix.GetRightMostItemIndex(ARow: Integer): Integer;
var
  AColumn: Integer;
begin
  Result := 0;
  for AColumn := ColumnCount - 1 downto 0 do
    if Values[AColumn, ARow] <> nil then
    begin
      Result := AColumn;
      Break;
    end;
end;

procedure TdxGalleryControlNavigationMatrix.Populate(AViewInfo: TdxGalleryControlViewInfo);

  procedure ProcessGroup(AGroup: TdxGalleryControlGroup; var AIndex: Integer);
  var
    AItem: TdxGalleryControlItem;
    I: Integer;
    P: TPoint;
  begin
    if AGroup.Visible then
    begin
      for I := 0 to AGroup.Items.Count - 1 do
      begin
        AItem := AGroup.Items[I];
        P := AItem.ViewInfo.CellPositionInGroup;
        Values[P.X, P.Y + AIndex] := AItem;
      end;
    end;
    Inc(AIndex, AGroup.ViewInfo.RowCount);
  end;

var
  AIndex, I: Integer;
begin
  AIndex := 0;
  for I := 0 to AViewInfo.Groups.Count - 1 do
    ProcessGroup(AViewInfo.Groups[I], AIndex);
end;

function TdxGalleryControlNavigationMatrix.GetValue(ACol, ARow: Integer): TdxGalleryControlItem;
begin
  Result := FValues[ACol, ARow];
end;

procedure TdxGalleryControlNavigationMatrix.SetValue(ACol, ARow: Integer; const AValue: TdxGalleryControlItem);
begin
  FValues[ACol, ARow] := AValue;
end;

{ TdxGalleryControlController }

destructor TdxGalleryControlController.Destroy;
begin
  FreeAndNil(FNavigationMatrix);
  inherited;
end;

procedure TdxGalleryControlController.CheckSelectedItem;
var
  AList: TList;
begin
  AList := TList.Create;
  try
    Gallery.GetAllItems(AList);
    if (StartSelectionItem <> nil) and (AList.IndexOf(StartSelectionItem) < 0) then
      FStartSelectionItem := nil;
    if (KeySelectedItem <> nil) and (AList.IndexOf(KeySelectedItem) < 0) then
      FKeySelectedItem := nil;
    if AList.IndexOf(MouseHoveredItem) < 0 then
    begin
      FMouseHoveredItem := nil;
      UpdateMouseHoveredItem(Owner.GetMouseCursorClientPos);
    end;
  finally
    Alist.Free;
  end;
end;

procedure TdxGalleryControlController.LayoutChanged;
begin
  FreeAndNil(FNavigationMatrix);
end;

function TdxGalleryControlController.CreateNavigationMatrix: TdxGalleryControlNavigationMatrix;
begin
  Result := TdxGalleryControlNavigationMatrix.Create(ViewInfo);
end;

procedure TdxGalleryControlController.FocusEnter;
begin
  // do nothing
end;

procedure TdxGalleryControlController.FocusLeave;
begin
  KeySelectedItem := nil;
end;

procedure TdxGalleryControlController.KeyDown(AKey: Word; AShift: TShiftState);
begin
  case AKey of
    VK_RIGHT:
      SelectNextItem(GetStartItemForKeyboardNavigation, IfThen(Owner.UseRightToLeftAlignment, -1, 1), 0, AShift);
    VK_LEFT:
      SelectNextItem(GetStartItemForKeyboardNavigation, IfThen(Owner.UseRightToLeftAlignment, 1, -1), 0, AShift);
    VK_UP:
      SelectNextItem(GetStartItemForKeyboardNavigation, 0, -1, AShift);
    VK_DOWN:
      SelectNextItem(GetStartItemForKeyboardNavigation, 0, 1, AShift);
    VK_SPACE:
      KeyPressed := True;
  end;
end;

procedure TdxGalleryControlController.KeyUp(AKey: Word; AShift: TShiftState);
begin
  case AKey of
    VK_SPACE:
      begin
        KeyPressed := False;
        Gallery.ClickItem(KeySelectedItem);
      end;
  end;
end;

procedure TdxGalleryControlController.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if CanChangeSelection(Button, Shift) then
  begin
    UpdateMouseHoveredItem(Point(X, Y));
    MousePressed := True;
  end;
end;

procedure TdxGalleryControlController.MouseLeave;
begin
  MouseHoveredItem := nil;
end;

procedure TdxGalleryControlController.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  UpdateMouseHoveredItem(Point(X, Y));
end;

procedure TdxGalleryControlController.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  function CanSelectHoveredItem: Boolean;
  begin
    Result := (Button = mbLeft) or ((Button = mbRight) and ((MouseHoveredItem = nil) or not MouseHoveredItem.Checked));
  end;

begin
  if CanChangeSelection(Button, Shift) then
  begin
    UpdateMouseHoveredItem(Point(X, Y));
    if FMousePressed then
    begin
      MousePressed := False;
      if IsListViewStyleSelection then
      begin
        if not (ssShift in Shift) and CanSelectHoveredItem then
          StartSelectionItem := MouseHoveredItem;
        if ssCtrl in Shift then
          ProcessItemClick(MouseHoveredItem, X, Y)
        else
          if CanSelectHoveredItem then
            SelectItems(StartSelectionItem, MouseHoveredItem);

        KeySelectedItem := MouseHoveredItem;
      end
      else
        if (Button = mbLeft) or ((Owner.OptionsBehavior.ItemCheckMode = icmSingleCheck) or IsGalleryStyleSelection) and
          CanSelectHoveredItem or (Owner.OptionsBehavior.ItemCheckMode = icmSingleRadio) then
        begin
          ProcessItemClick(MouseHoveredItem, X, Y);
          KeySelectedItem := nil;
        end;
    end;
  end;
end;

function TdxGalleryControlController.GetItemAtPos(const P: TPoint): TdxGalleryControlItem;
begin
  Result := Owner.GetItemAtPos(P);
end;

function TdxGalleryControlController.GetItemPosition(AItem: TdxGalleryControlItem): TPoint;
var
  I: Integer;
begin
  Result := AItem.ViewInfo.CellPositionInGroup;
  for I := 0 to AItem.Group.Index - 1 do
    Inc(Result.Y, ViewInfo.Groups[I].ViewInfo.RowCount);
end;

function TdxGalleryControlController.GetItemViewState(AItem: TdxGalleryControlItem): TdxGalleryItemViewState;
begin
  Result.Enabled := AItem.Enabled;
  Result.Checked := AItem.Checked;
  Result.Hover := AItem = MouseHoveredItem;
  Result.Focused := Owner.Focused and (AItem = KeySelectedItem);
  Result.Pressed := Result.Hover and MousePressed or Result.Focused and Owner.Controller.KeyPressed;
end;

procedure TdxGalleryControlController.InvalidateItem(AItem: TdxGalleryControlItem);
begin
  if AItem <> nil then
    Owner.InvalidateRect(AItem.ViewInfo.Bounds, False);
end;

function TdxGalleryControlController.IsGalleryStyleSelection: Boolean;
begin
  Result := (Owner.OptionsBehavior.ItemCheckMode = icmMultiple) and
    (Owner.OptionsBehavior.ItemMultiSelectKind = imskGallery);
end;

function TdxGalleryControlController.IsListViewStyleSelection: Boolean;
begin
  Result := (Owner.OptionsBehavior.ItemCheckMode = icmMultiple) and
    (Owner.OptionsBehavior.ItemMultiSelectKind = imskListView);
end;

procedure TdxGalleryControlController.MakeItemVisible(AItem: TdxGalleryControlItem);
var
  ABounds: TRect;
begin
  if (AItem <> nil) and AItem.Group.Visible then
  begin
    ABounds := AItem.ViewInfo.Bounds;
    if Owner.UseRightToLeftAlignment then
      ABounds := TdxRightToLeftLayoutConverter.ConvertRect(ABounds, ViewInfo.Bounds);
    Owner.MakeVisible(ABounds, vtFully);
  end;
end;

procedure TdxGalleryControlController.UpdateItemViewState(AItem: TdxGalleryControlItem);

  function IsItemValid(AItem: TdxGalleryControlItem): Boolean;
  begin
    Result := (AItem <> nil) and not (csDestroying in AItem.ComponentState);
  end;

  function IsViewStateEqual(const AViewState1, AViewState2: TdxGalleryItemViewState): Boolean;
  begin
    Result := CompareMem(@AViewState1, @AViewState2, SizeOf(TdxGalleryItemViewState));
  end;

begin
  if IsItemValid(AItem) and not IsViewStateEqual(AItem.ViewInfo.State, GetItemViewState(AItem)) then
  begin
    AItem.ViewInfo.FState := GetItemViewState(AItem);
    InvalidateItem(AItem);
  end;
end;

procedure TdxGalleryControlController.UpdateMouseHoveredItem(const P: TPoint);
var
  AItem: TdxGalleryControlItem;
begin
  AItem := GetItemAtPos(P);
  if (AItem <> nil) and AItem.Enabled then
    MouseHoveredItem := AItem
  else
    MouseHoveredItem := nil;
end;

procedure TdxGalleryControlController.SelectItems(AStartFromItem, AFinishAtItem: TdxGalleryControlItem);
var
  AFinishIndex: Integer;
  AList: TList;
  AStartIndex: Integer;
  I: Integer;
begin
  Owner.BeginUpdate;
  try
    AList := TList.Create;
    try
      Gallery.GetAllItems(AList);
      AStartIndex := Min(AList.IndexOf(AStartFromItem), AList.IndexOf(AFinishAtItem));
      AFinishIndex := Max(AList.IndexOf(AStartFromItem), AList.IndexOf(AFinishAtItem));
      for I := 0 to AList.Count - 1 do
        TdxGalleryItem(AList[I]).Checked := (I >= AStartIndex) and (I <= AFinishIndex);
    finally
      AList.Free;
    end;
  finally
    Owner.EndUpdate;
  end;
end;

procedure TdxGalleryControlController.GetNextItem(var AItemPos: TPoint; ADirectionX, ADirectionY: Integer);
var
  ANewItemPosX: Integer;
begin
  AItemPos.X := Max(0, Min(AItemPos.X + ADirectionX, NavigationMatrix.ColumnCount - 1));
  if ADirectionY <> 0 then
  begin
    repeat
      AItemPos.Y := Max(0, Min(AItemPos.Y + ADirectionY, NavigationMatrix.RowCount - 1));
      if NavigationMatrix.Values[AItemPos.X, AItemPos.Y] = nil then
        ANewItemPosX := NavigationMatrix.GetRightMostItemIndex(AItemPos.Y)
      else
        ANewItemPosX := AItemPos.X
    until (NavigationMatrix.Values[ANewItemPosX, AItemPos.Y] <> nil) or (AItemPos.Y = NavigationMatrix.RowCount - 1) or (AItemPos.Y = 0);
    AItemPos.X := ANewItemPosX;
  end;
end;

function TdxGalleryControlController.GetStartItemForKeyboardNavigation: TdxGalleryControlItem;
begin
  Result := KeySelectedItem;
  if Result = nil then
    Result := Gallery.GetCheckedItem;
  if Result = nil then
    Result := Gallery.GetFirstVisibleItem;
end;

procedure TdxGalleryControlController.SelectNextItem(
  AItem: TdxGalleryControlItem; ADirectionX, ADirectionY: Integer; AShift: TShiftState);

  function ValidateItem(AItemPos: TPoint): Boolean;
  begin
    Result := (High(NavigationMatrix.FValues) >= AItemPos.X) and
      (High(NavigationMatrix.FValues[AItemPos.X]) >= AItemPos.Y) and
      (NavigationMatrix.Values[AItemPos.X, AItemPos.Y] <> nil);
  end;

var
  AItemPos: TPoint;
begin
  if AItem <> nil then
  begin
    AItemPos := GetItemPosition(AItem);
    GetNextItem(AItemPos, ADirectionX, ADirectionY);
    if ValidateItem(AItemPos) then
      KeySelectedItem := NavigationMatrix.Values[AItemPos.X, AItemPos.Y];
  end
  else
    KeySelectedItem := Gallery.GetFirstVisibleItem;

  if IsListViewStyleSelection then
  begin
    if not (ssShift in AShift) then
      StartSelectionItem := KeySelectedItem;
    if not (ssCtrl in AShift) then
      SelectItems(StartSelectionItem, KeySelectedItem);
  end
  else
    if Owner.OptionsBehavior.ItemCheckMode = icmSingleRadio then
      SelectItems(KeySelectedItem, KeySelectedItem);
end;

procedure TdxGalleryControlController.ProcessItemClick(AItem: TdxGalleryControlItem; X, Y: Integer);
begin
  Gallery.ClickItem(AItem);
end;

function TdxGalleryControlController.CanChangeSelection(AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  Result := (AButton = mbLeft) or
    (AButton = mbRight) and Owner.OptionsBehavior.SelectOnRightClick and (AShift * [ssShift, ssCtrl] = []);
end;

function TdxGalleryControlController.GetGallery: TdxGalleryControlStructure;
begin
  Result := Owner.Gallery;
end;

function TdxGalleryControlController.GetNavigationMatrix: TdxGalleryControlNavigationMatrix;
begin
  if FNavigationMatrix = nil then
    FNavigationMatrix := CreateNavigationMatrix;
  Result := FNavigationMatrix;
end;

function TdxGalleryControlController.GetViewInfo: TdxGalleryControlViewInfo;
begin
  Result := Owner.ViewInfo;
end;

procedure TdxGalleryControlController.SetKeyPressed(AValue: Boolean);
begin
  if FKeyPressed <> AValue then
  begin
    FKeyPressed := AValue;
    UpdateItemViewState(KeySelectedItem);
  end;
end;

procedure TdxGalleryControlController.SetKeySelectedItem(AItem: TdxGalleryControlItem);
begin
  if FKeySelectedItem <> AItem then
  begin
    ExchangePointers(FKeySelectedItem, AItem);
    UpdateItemViewState(AItem);
    UpdateItemViewState(KeySelectedItem);
    MakeItemVisible(KeySelectedItem);
  end;
end;

procedure TdxGalleryControlController.SetMouseHoveredItem(AItem: TdxGalleryControlItem);
begin
  if MouseHoveredItem <> AItem then
  begin
    Application.CancelHint;
    ExchangePointers(FMouseHoveredItem, AItem);
    UpdateItemViewState(AItem);
    UpdateItemViewState(MouseHoveredItem);
  end;
end;

procedure TdxGalleryControlController.SetMousePressed(AValue: Boolean);
begin
  if MousePressed <> AValue then
  begin
    FMousePressed := AValue;
    UpdateItemViewState(MouseHoveredItem);
  end;
end;

{ TdxGalleryControlCustomOptions }

procedure TdxGalleryControlCustomOptions.Changed(AType: TdxChangeType);
begin
  Owner.LayoutChanged(AType);
end;

procedure TdxGalleryControlCustomOptions.ChangeScale(M, D: Integer);
begin
  // do nothing
end;

{ TdxGalleryControlOptionsBehavior }

procedure TdxGalleryControlOptionsBehavior.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxGalleryControlOptionsBehavior then
  begin
    ItemMultiSelectKind := TdxGalleryControlOptionsBehavior(Source).ItemMultiSelectKind;
    ItemCheckMode := TdxGalleryControlOptionsBehavior(Source).ItemCheckMode;
    ItemShowHint := TdxGalleryControlOptionsBehavior(Source).ItemShowHint;
  end;
end;

function TdxGalleryControlOptionsBehavior.GetItemCheckMode: TdxGalleryItemCheckMode;
begin
  Result := Owner.Gallery.ItemCheckMode;
end;

procedure TdxGalleryControlOptionsBehavior.SetItemCheckMode(const Value: TdxGalleryItemCheckMode);
begin
  Owner.Gallery.ItemCheckMode := Value;
end;

{ TdxGalleryControlOptionsItemImage }

constructor TdxGalleryControlOptionsItemImage.Create(AOwner: TdxCustomGalleryControl);
begin
  inherited Create(AOwner);
  FSize := TcxSize.Create(Self);
  FSize.OnChange := ChangeHandler;
  FShowFrame := True;
end;

destructor TdxGalleryControlOptionsItemImage.Destroy;
begin
  FreeAndNil(FSize);
  inherited Destroy;
end;

procedure TdxGalleryControlOptionsItemImage.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  Size.Size := cxSizeScale(Size.Size, M, D);
end;

procedure TdxGalleryControlOptionsItemImage.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxGalleryControlOptionsItemImage then
  begin
    Size := TdxGalleryControlOptionsItemImage(Source).Size;
    ShowFrame := TdxGalleryControlOptionsItemImage(Source).ShowFrame;
  end;
end;

procedure TdxGalleryControlOptionsItemImage.ChangeHandler(Sender: TObject);
begin
  Changed;
end;

procedure TdxGalleryControlOptionsItemImage.SetShowFrame(const Value: Boolean);
begin
  if Value <> FShowFrame then
  begin
    FShowFrame := Value;
    Changed;
  end;
end;

procedure TdxGalleryControlOptionsItemImage.SetSize(const Value: TcxSize);
begin
  FSize.Assign(Value);
end;

{ TdxGalleryControlOptionsItemText }

constructor TdxGalleryControlOptionsItemText.Create(AOwner: TdxCustomGalleryControl);
begin
  inherited Create(AOwner);
  FAlignHorz := taCenter;
  FWordWrap := True;
end;

procedure TdxGalleryControlOptionsItemText.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxGalleryControlOptionsItemText then
  begin
    AlignHorz := TdxGalleryControlOptionsItemText(Source).AlignHorz;
    AlignVert := TdxGalleryControlOptionsItemText(Source).AlignVert;
    Position := TdxGalleryControlOptionsItemText(Source).Position;
    WordWrap := TdxGalleryControlOptionsItemText(Source).WordWrap;
  end;
end;

procedure TdxGalleryControlOptionsItemText.SetAlignHorz(const Value: TAlignment);
begin
  if FAlignHorz <> Value then
  begin
    FAlignHorz := Value;
    Changed(ctLight);
  end;
end;

procedure TdxGalleryControlOptionsItemText.SetAlignVert(const Value: TcxAlignmentVert);
begin
  if FAlignVert <> Value then
  begin
    FAlignVert := Value;
    Changed(ctLight);
  end;
end;

procedure TdxGalleryControlOptionsItemText.SetPosition(const Value: TcxPosition);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    Changed;
  end;
end;

procedure TdxGalleryControlOptionsItemText.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Changed
  end;
end;

{ TdxGalleryControlOptionsItem }

constructor TdxGalleryControlOptionsItem.Create(AOwner: TdxCustomGalleryControl);
begin
  inherited Create(AOwner);
  FImage := CreateImage;
  FText := CreateText;
end;

destructor TdxGalleryControlOptionsItem.Destroy;
begin
  FreeAndNil(FText);
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TdxGalleryControlOptionsItem.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  Image.ChangeScale(M, D);
  Text.ChangeScale(M, D);
end;

function TdxGalleryControlOptionsItem.CreateImage: TdxGalleryControlOptionsItemImage;
begin
  Result := TdxGalleryControlOptionsItemImage.Create(Owner);
end;

function TdxGalleryControlOptionsItem.CreateText: TdxGalleryControlOptionsItemText;
begin
  Result := TdxGalleryControlOptionsItemText.Create(Owner);
end;

procedure TdxGalleryControlOptionsItem.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxGalleryControlOptionsItem then
    Text := TdxGalleryControlOptionsItem(Source).Text;
end;

procedure TdxGalleryControlOptionsItem.SetImage(AValue: TdxGalleryControlOptionsItemImage);
begin
  FImage.Assign(AValue);
end;

procedure TdxGalleryControlOptionsItem.SetText(AValue: TdxGalleryControlOptionsItemText);
begin
  FText.Assign(AValue);
end;

{ TdxGalleryControlOptionsView }

constructor TdxGalleryControlOptionsView.Create(AOwner: TdxCustomGalleryControl);
begin
  inherited Create(AOwner);
  FItem := CreateItem;
  FContentOffset := TcxMargin.Create(Self, 1);
  FContentOffset.OnChange := ChangeHandler;
  FContentOffsetGroups := TcxMargin.Create(Self, 0);
  FContentOffsetGroups.OnChange := ChangeHandler;
  FContentOffsetItems := TcxMargin.Create(Self, 6);
  FContentOffsetItems.OnChange := ChangeHandler;
end;

destructor TdxGalleryControlOptionsView.Destroy;
begin
  FreeAndNil(FContentOffsetItems);
  FreeAndNil(FContentOffsetGroups);
  FreeAndNil(FContentOffset);
  FreeAndNil(FItem);
  inherited Destroy;
end;

procedure TdxGalleryControlOptionsView.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  ContentOffset.Margin := cxRectScale(ContentOffset.Margin, M, D);
  ContentOffsetGroups.Margin := cxRectScale(ContentOffsetGroups.Margin, M, D);
  ContentOffsetItems.Margin := cxRectScale(ContentOffsetItems.Margin, M, D);
  Item.ChangeScale(M, D);
end;

function TdxGalleryControlOptionsView.CreateItem: TdxGalleryControlOptionsItem;
begin
  Result := TdxGalleryControlOptionsItem.Create(Owner);
end;

procedure TdxGalleryControlOptionsView.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxGalleryControlOptionsView then
  begin
    ColumnCount := TdxGalleryControlOptionsView(Source).ColumnCount;
    ColumnAutoWidth := TdxGalleryControlOptionsView(Source).ColumnAutoWidth;
    ContentOffset := TdxGalleryControlOptionsView(Source).ContentOffset;
    ContentOffsetGroups := TdxGalleryControlOptionsView(Source).ContentOffsetGroups;
    ContentOffsetItems := TdxGalleryControlOptionsView(Source).ContentOffsetItems;
    Item := TdxGalleryControlOptionsView(Source).Item;
  end;
end;

procedure TdxGalleryControlOptionsView.ChangeHandler(Sender: TObject);
begin
  Changed;
end;

procedure TdxGalleryControlOptionsView.SetColumnAutoWidth(AValue: Boolean);
begin
  if FColumnAutoWidth <> AValue then
  begin
    FColumnAutoWidth := AValue;
    Changed;
  end;
end;

procedure TdxGalleryControlOptionsView.SetColumnCount(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FColumnCount <> AValue then
  begin
    FColumnCount := AValue;
    Changed;
  end;
end;

procedure TdxGalleryControlOptionsView.SetContentOffset(const Value: TcxMargin);
begin
  FContentOffset.Assign(Value);
end;

procedure TdxGalleryControlOptionsView.SetContentOffsetGroups(const Value: TcxMargin);
begin
  FContentOffsetGroups.Assign(Value);
end;

procedure TdxGalleryControlOptionsView.SetContentOffsetItems(const Value: TcxMargin);
begin
  FContentOffsetItems.Assign(Value);
end;

procedure TdxGalleryControlOptionsView.SetItem(const Value: TdxGalleryControlOptionsItem);
begin
  FItem.Assign(Value);
end;

{ TdxCustomGalleryControl }

constructor TdxCustomGalleryControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csParentBackground];

  CanBeFocused := True;
  BorderStyle := cxcbsDefault;
  Width := 150;
  Height := 100;

  FGallery := CreateGallery;
  TdxGalleryAccess(FGallery).OnChange := GalleryChangeHandler;
  TdxGalleryAccess(FGallery).OnItemClick := GalleryItemClickHandler;

  FViewInfo := CreateViewInfo;
  FController := CreateController;
  FPainter := CreatePainter;
  FOptionsBehavior := CreateOptionsBehavior;
  FOptionsView := CreateOptionsView;

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChanged;

  ShowHint := True;
  Keys := [kArrows];
end;

destructor TdxCustomGalleryControl.Destroy;
begin
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FOptionsBehavior);
  FreeAndNil(FOptionsView);
  FreeAndNil(FController);
  FreeAndNil(FViewInfo);
  FreeAndNil(FGallery);
  FreeAndNil(FPainter);
  inherited Destroy;
end;

procedure TdxCustomGalleryControl.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxCustomGalleryControl.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    LayoutChanged;
end;

function TdxCustomGalleryControl.CanFocus: Boolean;
begin
  Result := inherited CanFocus and CanBeFocused;
end;

procedure TdxCustomGalleryControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  Gallery.GetChildren(Proc, Root);
end;

procedure TdxCustomGalleryControl.MakeItemVisible(AItem: TdxGalleryControlItem);
begin
  Controller.MakeItemVisible(AItem);
end;

function TdxCustomGalleryControl.CreateController: TdxGalleryControlController;
begin
  Result := TdxGalleryControlController.Create(Self);
end;

function TdxCustomGalleryControl.CreateGallery: TdxGalleryControlStructure;
begin
  Result := TdxGalleryControlStructure.Create(Self);
end;

function TdxCustomGalleryControl.CreateOptionsBehavior: TdxGalleryControlOptionsBehavior;
begin
  Result := TdxGalleryControlOptionsBehavior.Create(Self);
end;

function TdxCustomGalleryControl.CreateOptionsView: TdxGalleryControlOptionsView;
begin
  Result := TdxGalleryControlOptionsView.Create(Self);
end;

function TdxCustomGalleryControl.CreatePainter: TdxGalleryControlPainter;
begin
  Result := TdxGalleryControlPainter.Create(Self);
end;

function TdxCustomGalleryControl.CreateViewInfo: TdxGalleryControlViewInfo;
begin
  Result := TdxGalleryControlViewInfo.Create(Self);
end;

procedure TdxCustomGalleryControl.FocusEnter;
begin
  inherited FocusEnter;
  Controller.FocusEnter;
end;

procedure TdxCustomGalleryControl.FocusLeave;
begin
  inherited FocusLeave;
  Controller.FocusLeave;
end;

procedure TdxCustomGalleryControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  Controller.KeyDown(Key, Shift);
end;

procedure TdxCustomGalleryControl.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  Controller.KeyUp(Key, Shift);
end;

procedure TdxCustomGalleryControl.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (csNoStdEvents in ControlStyle) then
    Controller.MouseUp(Button, Shift, X, Y);
end;

procedure TdxCustomGalleryControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  Controller.MouseDown(Button, Shift, X, Y);
end;

procedure TdxCustomGalleryControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  Controller.MouseMove(Shift, X, Y);
end;

procedure TdxCustomGalleryControl.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  Controller.MouseLeave;
end;

function TdxCustomGalleryControl.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := GetItemCount > 0;
  if Result then
  begin
    Calculate(ctHard);
    case AutoSizeMode of
      asAutoSize:
        begin
          NewWidth := cxRectWidth(ViewInfo.ContentBounds) + cxMarginsWidth(ViewInfo.BorderWidths);
          NewHeight := cxRectHeight(ViewInfo.ContentBounds) + cxMarginsHeight(ViewInfo.BorderWidths);
        end;

      asAutoWidth:
        begin
          NewWidth := cxRectWidth(ViewInfo.ContentBounds) + cxMarginsWidth(ViewInfo.BorderWidths);
          if not IsTouchScrollUIMode and IsScrollBarActive(sbVertical) then
            Inc(NewWidth, GetVScrollBarAreaWidth);
        end;

      asAutoHeight:
        begin
          NewHeight := cxRectHeight(ViewInfo.ContentBounds) + cxMarginsHeight(ViewInfo.BorderWidths);
          if not IsTouchScrollUIMode and IsScrollBarActive(sbHorizontal) then
            Inc(NewHeight, GetHScrollBarAreaHeight);
        end;
    end;
  end;
end;

procedure TdxCustomGalleryControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TdxCustomGalleryControl.Paint;
begin
  inherited Paint;
  Canvas.Font := Font;
  ViewInfo.Draw(Canvas);
end;

function TdxCustomGalleryControl.GetContentSize: TSize;
begin
  Result := cxSize(cxRectInflate(ViewInfo.ContentBounds, ContentOffset.Margin));
  if GetScrollbarMode = sbmHybrid then
  begin
    if IsScrollBarActive(sbHorizontal) then
      Inc(Result.cy, GetHScrollBarDefaultAreaHeight);
    if IsScrollBarActive(sbVertical) then
      Inc(Result.cx, GetVScrollBarDefaultAreaWidth);
  end;
end;

procedure TdxCustomGalleryControl.Calculate(AType: TdxChangeType);
begin
  ViewInfo.Calculate(AType, ClientBounds);
  if AType <> ctLight then
    Controller.LayoutChanged;
end;

procedure TdxCustomGalleryControl.LayoutChanged(AType: TdxChangeType);
begin
  if not IsUpdateLocked then
    inherited LayoutChanged(AType);
end;

procedure TdxCustomGalleryControl.ScrollPosChanged(const AOffset: TPoint);
begin
  inherited;
  Controller.UpdateMouseHoveredItem(GetMouseCursorClientPos);
end;

function TdxCustomGalleryControl.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

procedure TdxCustomGalleryControl.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  BeginUpdate;
  try
    inherited;
    OptionsBehavior.ChangeScale(M, D);
    OptionsView.ChangeScale(M, D);
    Gallery.ChangeScale(M, D);
  finally
    EndUpdate;
  end;
end;

function TdxCustomGalleryControl.GetScrollContentForegroundColor: TColor;
var
  AState: TdxGalleryItemViewState;
begin
  AState.Enabled := True;
  AState.Checked := False;
  AState.Hover := False;
  AState.Pressed := False;
  AState.Focused := False;
  Result := LookAndFeelPainter.GetGalleryItemCaptionTextColor(AState);
end;

function TdxCustomGalleryControl.HasScrollBarArea: Boolean;
begin
  Result := inherited HasScrollBarArea or (GetScrollbarMode = sbmHybrid);
end;

procedure TdxCustomGalleryControl.SetAutoSizeMode(AValue: TdxAutoSizeMode);
begin
  if AutoSizeMode <> AValue then
  begin
    inherited SetAutoSizeMode(AValue);
    LayoutChanged;
  end;
end;

procedure TdxCustomGalleryControl.DoClickItem(AItem: TdxGalleryItem);
begin
  if Assigned(OnItemClick) then
    OnItemClick(Self, AItem as TdxGalleryControlItem);
end;

function TdxCustomGalleryControl.GetItemAtPos(const P: TPoint): TdxGalleryControlItem;
begin
  Result := Gallery.Groups.GetItemAtPos(P);
end;

function TdxCustomGalleryControl.IsUpdateLocked: Boolean;
begin
  Result := (FLockCount > 0) or IsLoading;
end;

function TdxCustomGalleryControl.GetGallery: IdxGallery;
begin
  Result := Gallery;
end;

function TdxCustomGalleryControl.GetGallery2: IdxGallery2;
begin
  Result := Gallery;
end;

function TdxCustomGalleryControl.GetColumnCount: Integer;
begin
  Result := OptionsView.ColumnCount;
end;

function TdxCustomGalleryControl.GetContentOffset: TcxMargin;
begin
  Result := OptionsView.ContentOffset;
end;

function TdxCustomGalleryControl.GetContentOffsetGroups: TcxMargin;
begin
  Result := OptionsView.ContentOffsetGroups;
end;

function TdxCustomGalleryControl.GetContentOffsetItems: TcxMargin;
begin
  Result := OptionsView.ContentOffsetItems;
end;

function TdxCustomGalleryControl.GetItemCheckMode: TdxGalleryItemCheckMode;
begin
  Result := OptionsBehavior.ItemCheckMode;
end;

function TdxCustomGalleryControl.GetItemCount: Integer;
var
  AList: TList;
begin
  AList := TList.Create;
  try
    Gallery.GetAllItems(AList);
    Result := AList.Count;
  finally
    AList.Free;
  end;
end;

function TdxCustomGalleryControl.GetItemImageSize: TcxSize;
begin
  Result := OptionsView.Item.Image.Size;
end;

function TdxCustomGalleryControl.GetItemShowHint: Boolean;
begin
  Result := OptionsBehavior.ItemShowHint;
end;

function TdxCustomGalleryControl.GetItemShowImageFrame: Boolean;
begin
  Result := OptionsView.Item.Image.ShowFrame;
end;

function TdxCustomGalleryControl.GetItemTextPosition: TcxPosition;
begin
  Result := OptionsView.Item.Text.Position;
end;

procedure TdxCustomGalleryControl.SetGallery(AValue: TdxGalleryControlStructure);
begin
  FGallery.Assign(AValue);
end;

procedure TdxCustomGalleryControl.SetColumnCount(AValue: Integer);
begin
  OptionsView.ColumnCount := AValue;
end;

procedure TdxCustomGalleryControl.SetContentOffset(AValue: TcxMargin);
begin
  OptionsView.ContentOffset := AValue;
end;

procedure TdxCustomGalleryControl.SetContentOffsetGroups(AValue: TcxMargin);
begin
  OptionsView.ContentOffsetGroups := AValue;
end;

procedure TdxCustomGalleryControl.SetContentOffsetItems(AValue: TcxMargin);
begin
  OptionsView.ContentOffsetItems := AValue;
end;

procedure TdxCustomGalleryControl.SetImages(Value: TCustomImageList);
begin
  cxSetImageList(Value, FImages, FImageChangeLink, Self);
end;

procedure TdxCustomGalleryControl.SetItemCheckMode(AValue: TdxGalleryItemCheckMode);
begin
  OptionsBehavior.ItemCheckMode := AValue;
end;

procedure TdxCustomGalleryControl.SetItemImageSize(AValue: TcxSize);
begin
  OptionsView.Item.Image.Size := AValue;
end;

procedure TdxCustomGalleryControl.SetItemTextPosition(AValue: TcxPosition);
begin
  OptionsView.Item.Text.Position := AValue;
end;

procedure TdxCustomGalleryControl.SetItemShowHint(const Value: Boolean);
begin
  OptionsBehavior.ItemShowHint := Value;
end;

procedure TdxCustomGalleryControl.SetItemShowImageFrame(AValue: Boolean);
begin
  OptionsView.Item.Image.ShowFrame := AValue;
end;

procedure TdxCustomGalleryControl.GalleryChangeHandler(ASender: TObject; AChangeType: TdxGalleryChangeType);
begin
  if HandleAllocated and not IsUpdateLocked then
  begin
    Controller.CheckSelectedItem;
    LayoutChanged;
  end;
end;

procedure TdxCustomGalleryControl.GalleryItemClickHandler(ASender: TObject; AItem: TdxGalleryItem);
begin
  DoClickItem(AItem);
end;

procedure TdxCustomGalleryControl.SetOptionsBehavior(const Value: TdxGalleryControlOptionsBehavior);
begin
  FOptionsBehavior.Assign(Value);
end;

procedure TdxCustomGalleryControl.SetOptionsView(const Value: TdxGalleryControlOptionsView);
begin
  FOptionsView.Assign(Value);
end;

procedure TdxCustomGalleryControl.ImageListChanged(Sender: TObject);
begin
  if Sender = Images then
    LayoutChanged;
end;

procedure TdxCustomGalleryControl.CMFontChanged(var Message: TMessage);
begin
  inherited;
  LayoutChanged;
end;

procedure TdxCustomGalleryControl.CMHintShow(var Message: TCMHintShow);
var
  AItem: TdxGalleryControlItem;
begin
  if ItemShowHint then
  begin
    AItem := GetItemAtPos(Message.HintInfo.CursorPos);
    if (AItem <> nil) and AItem.Enabled and (AItem.Hint <> '') then
    begin
      Message.HintInfo.CursorRect := AItem.ViewInfo.Bounds;
      Message.HintInfo.HintStr := AItem.Hint;
      Message.Result := 0;
    end;
  end;
end;

procedure TdxCustomGalleryControl.WMLButtonUp(var Message: TWMLButtonDown);
begin
  inherited;
  DoMouseUp(mbLeft, KeysToShiftState(Message.Keys) + MouseOriginToShiftState, Message.XPos, Message.YPos);
end;

procedure TdxCustomGalleryControl.WMRButtonUp(var Message: TWMLButtonDown);
begin
  inherited;
  DoMouseUp(mbRight, KeysToShiftState(Message.Keys) + MouseOriginToShiftState, Message.XPos, Message.YPos);
end;

initialization
  RegisterClasses([TdxGalleryControlItem, TdxGalleryControlGroup]);
end.
