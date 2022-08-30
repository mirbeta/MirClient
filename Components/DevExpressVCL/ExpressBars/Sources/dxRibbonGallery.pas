{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars components                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxRibbonGallery;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Classes, Messages, SysUtils, Graphics, Controls, StdCtrls, Contnrs,
  ImgList, ActnList, Forms, IniFiles, TypInfo,
  dxCore, dxCoreClasses, cxClasses, dxGDIPlusClasses, cxGraphics, cxGeometry, dxTouch, cxScrollBar,
  dxBar, cxAccessibility, dxBarAccessibility, dxRibbon, dxRibbonForm, cxControls,
  cxLookAndFeelPainters, dxRibbonSkins, dxAnimation, dxGallery, dxActions;

const
  dxRibbonGalleryDefaultColumnCount = 5;
  dxRibbonGalleryGroupItemInRibbonImageIndent = 1;
  dxRibbonGalleryGroupItemIndent = 3;
  dxRibbonGalleryMinColumnCount = 2;

type
  TdxCustomRibbonGalleryItem = class;
  TdxCustomRibbonGalleryOptions = class;
  TdxInRibbonGalleryOptions = class;
  TdxInMenuGalleryOptions = class;
  TdxRibbonDropDownGallery = class;
  TdxRibbonDropDownGalleryControl = class;
  TdxRibbonGalleryControl = class;
  TdxRibbonGalleryControlViewInfo = class;
  TdxRibbonGalleryFilter = class;
  TdxRibbonGalleryFilterCategory = class;
  TdxRibbonGalleryFilterMenuControl = class;
  TdxRibbonGalleryGroup = class;
  TdxRibbonGalleryGroupItemActionLink = class;
  TdxRibbonGalleryGroupItemActionLinkClass = class of TdxRibbonGalleryGroupItemActionLink;
  TdxRibbonGalleryGroupItemClass = class of TdxRibbonGalleryGroupItem;
  TdxRibbonGalleryGroupItemViewInfo = class;
  TdxRibbonGalleryGroupOptions = class;
  TdxRibbonGalleryGroupViewInfo = class;
  TdxRibbonGalleryItem = class;
  TdxRibbonGalleryScrollBar = class;
  TdxRibbonOnMenuGalleryControlViewInfo = class;
  TdxRibbonGalleryScrollBarHelper = class;

  TdxRibbonDropDownGalleryNavigationDirection = (dgndNone, dgndUp, dgndDown);
  TdxRibbonGalleryVisibilityState = (gbsIndefinite, gbsFalse, gbsTrue);
  TdxRibbonGalleryGroupItemTextKind = (itkNone, itkCaption, itkCaptionAndDescription);
  TdxRibbonGalleryImagePosition = (gipLeft, gipRight, gipTop, gipBottom);
  TdxRibbonGalleryItemHintSource = (ghsNone, ghsItemHint, ghsGalleryScreenTip);
  TdxRibbonGalleryItemSelectionMode = (gsmNone, gsmSingle, gsmMultiple, gsmSingleInGroup);
  TdxRibbonGallerySubmenuResizing = (gsrNone, gsrHeight, gsrWidthAndHeight);
  TdxRibbonGalleryDropDownResizing = TdxRibbonGallerySubmenuResizing;

  TdxSelectionChangedEvent = procedure(Sender: TObject; DeselectAll: Boolean) of object;

  { TcxItemSize }

  TcxItemSize = class(TcxSize)
  strict private
    FAssigned: Boolean;
    FParent: TcxItemSize;

    procedure SetAssigned(AValue: Boolean);
  protected
    procedure DoChange; override;
    function GetValue(Index: Integer): Integer; override;
    function IsSizeStored(Index: Integer): Boolean; override;
    procedure SetSize(const Value: TSize); override;

    property Assigned: Boolean read FAssigned write SetAssigned;
    property Parent: TcxItemSize read FParent write FParent;
  public
    procedure ChangeScale(M, D: Integer); override;
  end;

  { TdxRibbonGalleryItemPullHighlighting }

  TdxRibbonGalleryItemPullHighlightingDirection = (gphdStartToFinish, gphdFinishToStart);
  TdxRibbonGalleryItemPullHighlightingMode = (iphmNone, iphmStartToFinish, iphmFinishToStart);

  TdxRibbonGalleryItemPullHighlighting = class(TPersistent)
  strict private
    FActive: Boolean;
    FIsAssigned: Boolean;
    FDirection: TdxRibbonGalleryItemPullHighlightingDirection;
    FParent: TdxRibbonGalleryItemPullHighlighting;

    FOnChange: TNotifyEvent;

    function GetActive: Boolean;
    function GetDirection: TdxRibbonGalleryItemPullHighlightingDirection;
    procedure SetActive(Value: Boolean);
    procedure SetIsAssigned(Value: Boolean);
    procedure SetDirection(Value: TdxRibbonGalleryItemPullHighlightingDirection);
  protected
    procedure DoChange;
    function IsActiveStored: Boolean; virtual;
    function IsDirectionStored: Boolean; virtual;

    property IsAssigned: Boolean read FIsAssigned write SetIsAssigned;
    property Parent: TdxRibbonGalleryItemPullHighlighting read FParent write FParent;
  public
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Active: Boolean read GetActive write SetActive stored IsActiveStored;
    property Direction: TdxRibbonGalleryItemPullHighlightingDirection read GetDirection
      write SetDirection stored IsDirectionStored;
  end;

  { TdxRibbonGalleryBaseOptions }

  TdxRibbonGalleryBaseOptions = class(TPersistent)
  strict private
    FFreeNotificator: TcxFreeNotificator;
    FGalleryItem: TdxCustomRibbonGalleryItem;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FItemImagePosition: TdxRibbonGalleryImagePosition;
    FItemImageSize: TcxItemSize;
    FItemPullHighlighting: TdxRibbonGalleryItemPullHighlighting;
    FSpaceAfterGroupHeader: Integer;
    FSpaceBetweenItemCaptionAndDescription: Integer;
    FSpaceBetweenItemImageAndText: Integer;
    FSpaceBetweenItemsHorizontally: Integer;
    FSpaceBetweenItemsVertically: Integer;

    FOnChanged: TNotifyEvent;

    function GetInRibbonOptions: TdxInRibbonGalleryOptions;
    function GetInMenuOptions: TdxInMenuGalleryOptions;
    procedure FreeNotificationHandler(ASender: TComponent);
    procedure ImageLinkChangeHandler(Sender: TObject);
    procedure ReadSpaceBetweenItemsProperty(Reader: TReader);
    procedure SetItemImageSize(Value: TcxItemSize);
    procedure SetItemPullHighlighting(Value: TdxRibbonGalleryItemPullHighlighting);
    procedure WriteSpaceBetweenItemsProperty(Writer: TWriter);
  protected
    function ObtainItemPullHighlightingMode: TdxRibbonGalleryItemPullHighlightingMode;
    procedure Changed; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    procedure CheckIntRange(var Value: Integer);
    procedure CheckItemsSpaceRange(var Value: Integer);
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoImageListContentChanged; virtual; abstract;

    function GetItemImagePosition: TdxRibbonGalleryImagePosition; virtual;
    function GetItemSize: TcxItemSize; virtual;
    function GetItemTextAlignVert: TcxAlignmentVert; virtual;
    function GetItemTextKind: TdxRibbonGalleryGroupItemTextKind; virtual;
    function GetRemoveHorizontalItemPadding: Boolean; virtual;
    function GetRemoveVerticalItemPadding: Boolean; virtual;
    function GetSpaceAfterGroupHeader: Integer; virtual;
    function GetSpaceBetweenItemCaptionAndDescription: Integer; virtual;
    function GetSpaceBetweenItemImageAndText: Integer; virtual;
    function GetSpaceBetweenItems: Integer; virtual;
    function GetSpaceBetweenItemsHorizontally: Integer; virtual;
    function GetSpaceBetweenItemsVertically: Integer; virtual;

    function IsItemImagePositionStored: Boolean; virtual;
    function IsItemImageSizeStored: Boolean; virtual;
    function IsSpaceAfterGroupHeaderStored: Boolean; virtual;
    function IsSpaceBetweenItemCaptionAndDescriptionStored: Boolean; virtual;
    function IsSpaceBetweenItemImageAndTextStored: Boolean; virtual;
    function IsSpaceBetweenItemsHorizontallyStored: Boolean; virtual;
    function IsSpaceBetweenItemsVerticallyStored: Boolean; virtual;
    procedure ItemImageSizeChange(Sender: TObject); virtual;
    procedure ItemPullHighlightingChange(Sender: TObject); virtual;

    procedure SetImages(Value: TCustomImageList); virtual;
    procedure SetItemImagePosition(Value: TdxRibbonGalleryImagePosition); virtual;
    procedure SetItemSize(Value: TcxItemSize); virtual;
    procedure SetItemTextAlignVert(Value: TcxAlignmentVert); virtual;
    procedure SetItemTextKind(Value: TdxRibbonGalleryGroupItemTextKind); virtual;
    procedure SetSpaceAfterGroupHeader(Value: Integer); virtual;
    procedure SetSpaceBetweenItemCaptionAndDescription(Value: Integer); virtual;
    procedure SetSpaceBetweenItemImageAndText(Value: Integer); virtual;
    procedure SetSpaceBetweenItems(Value: Integer); virtual;
    procedure SetSpaceBetweenItemsHorizontally(Value: Integer); virtual;
    procedure SetSpaceBetweenItemsVertically(Value: Integer); virtual;

    property ImageChangeLink: TChangeLink read FImageChangeLink;
    property RemoveHorizontalItemPadding: Boolean read GetRemoveHorizontalItemPadding;
    property RemoveVerticalItemPadding: Boolean read GetRemoveVerticalItemPadding;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;

    // internal properties
    property GalleryItem: TdxCustomRibbonGalleryItem read FGalleryItem;
    property InMenuOptions: TdxInMenuGalleryOptions read GetInMenuOptions;
    property InRibbonOptions: TdxInRibbonGalleryOptions read GetInRibbonOptions;
  public
    constructor Create(AOwner: TdxCustomRibbonGalleryItem); overload; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Images: TCustomImageList read FImages write SetImages;
    property ItemImagePosition: TdxRibbonGalleryImagePosition read GetItemImagePosition write SetItemImagePosition stored IsItemImagePositionStored;
    property ItemImageSize: TcxItemSize read FItemImageSize write SetItemImageSize stored IsItemImageSizeStored;
    property ItemTextAlignVert: TcxAlignmentVert read GetItemTextAlignVert write SetItemTextAlignVert stored False;
    property ItemTextKind: TdxRibbonGalleryGroupItemTextKind read GetItemTextKind write SetItemTextKind stored False;
    property ItemSize: TcxItemSize read GetItemSize write SetItemSize stored False;
    property ItemPullHighlighting: TdxRibbonGalleryItemPullHighlighting read FItemPullHighlighting write SetItemPullHighlighting stored False;
    property SpaceAfterGroupHeader: Integer read GetSpaceAfterGroupHeader write SetSpaceAfterGroupHeader stored IsSpaceAfterGroupHeaderStored;
    property SpaceBetweenItemCaptionAndDescription: Integer read GetSpaceBetweenItemCaptionAndDescription write SetSpaceBetweenItemCaptionAndDescription stored IsSpaceBetweenItemCaptionAndDescriptionStored;
    property SpaceBetweenItemImageAndText: Integer read GetSpaceBetweenItemImageAndText write SetSpaceBetweenItemImageAndText stored IsSpaceBetweenItemImageAndTextStored;
    property SpaceBetweenItems: Integer read GetSpaceBetweenItems write SetSpaceBetweenItems;
    property SpaceBetweenItemsHorizontally: Integer read GetSpaceBetweenItemsHorizontally write SetSpaceBetweenItemsHorizontally stored IsSpaceBetweenItemsHorizontallyStored;
    property SpaceBetweenItemsVertically: Integer read GetSpaceBetweenItemsVertically write SetSpaceBetweenItemsVertically stored IsSpaceBetweenItemsVerticallyStored;
  end;

  { TdxRibbonGalleryGroupOptions }

  TdxRibbonGalleryGroupOptionsAssignedValue = (
    avItemImagePosition, avItemImageSize, avItemPullHighlighting, avItemPullHighlightingMode,
    avItemSize, avItemTextAlignVert, avItemTextKind, avSpaceAfterGroupHeader,
    avSpaceBetweenItemCaptionAndDescription, avSpaceBetweenItemImageAndText,
    avSpaceBetweenItems,
    avSpaceBetweenItemsHorizontally, avSpaceBetweenItemsVertically);
  TdxRibbonGalleryGroupOptionsAssignedValues = set of TdxRibbonGalleryGroupOptionsAssignedValue;

  TdxRibbonGalleryGroupOptions = class(TdxRibbonGalleryBaseOptions)
  strict private
    FAssignedValues: TdxRibbonGalleryGroupOptionsAssignedValues;
    FGroup: TdxRibbonGalleryGroup;
    FItemPullHighlightingMode: TdxRibbonGalleryItemPullHighlightingMode;
    FItemSize: TcxItemSize;
    FItemTextAlignVert: TcxAlignmentVert;
    FItemTextKind: TdxRibbonGalleryGroupItemTextKind;
    FParentOptions: TdxRibbonGalleryBaseOptions;

    function GetItemPullHighlightingMode: TdxRibbonGalleryItemPullHighlightingMode;
    function IsItemPullHighlightingModeStored: Boolean;
    function IsItemSizeStored: Boolean;
    function IsItemTextAlignVertStored: Boolean;
    function IsItemTextKindStored: Boolean;
    procedure ItemSizeChange(Sender: TObject);
    procedure SetAssignedValues(const Value: TdxRibbonGalleryGroupOptionsAssignedValues);
    procedure SetItemPullHighlightingMode(Value: TdxRibbonGalleryItemPullHighlightingMode);
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure DoImageListContentChanged; override;

    function GetItemImagePosition: TdxRibbonGalleryImagePosition; override;
    function GetItemSize: TcxItemSize; override;
    function GetItemTextAlignVert: TcxAlignmentVert; override;
    function GetItemTextKind: TdxRibbonGalleryGroupItemTextKind; override;
    function GetSpaceAfterGroupHeader: Integer; override;
    function GetSpaceBetweenItemCaptionAndDescription: Integer; override;
    function GetSpaceBetweenItemImageAndText: Integer; override;
    function GetSpaceBetweenItemsHorizontally: Integer; override;
    function GetSpaceBetweenItemsVertically: Integer; override;

    function IsSpaceAfterGroupHeaderStored: Boolean; override;
    function IsSpaceBetweenItemCaptionAndDescriptionStored: Boolean; override;
    function IsSpaceBetweenItemImageAndTextStored: Boolean; override;
    function IsSpaceBetweenItemsHorizontallyStored: Boolean; override;
    function IsSpaceBetweenItemsVerticallyStored: Boolean; override;
    function IsItemImagePositionStored: Boolean; override;
    function IsItemImageSizeStored: Boolean; override;
    procedure ItemImageSizeChange(Sender: TObject); override;
    procedure ItemPullHighlightingChange(Sender: TObject); override;

    procedure SetItemImagePosition(Value: TdxRibbonGalleryImagePosition); override;
    procedure SetItemSize(Value: TcxItemSize); override;
    procedure SetItemTextAlignVert(Value: TcxAlignmentVert); override;
    procedure SetItemTextKind(Value: TdxRibbonGalleryGroupItemTextKind); override;
    procedure SetSpaceAfterGroupHeader(Value: Integer); override;
    procedure SetSpaceBetweenItemCaptionAndDescription(Value: Integer); override;
    procedure SetSpaceBetweenItemImageAndText(Value: Integer); override;
    procedure SetSpaceBetweenItemsHorizontally(Value: Integer); override;
    procedure SetSpaceBetweenItemsVertically(Value: Integer); override;

    property Group: TdxRibbonGalleryGroup read FGroup;
    property ParentOptions: TdxRibbonGalleryBaseOptions read FParentOptions;
  public
    constructor Create(AParentOptions: TdxRibbonGalleryBaseOptions; AGroup: TdxRibbonGalleryGroup); overload; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AssignedValues: TdxRibbonGalleryGroupOptionsAssignedValues read FAssignedValues write SetAssignedValues default [];
    property Images;
    property ItemImagePosition;
    property ItemImageSize;
    property ItemTextAlignVert stored IsItemTextAlignVertStored;
    property ItemTextKind stored IsItemTextKindStored;
    property ItemSize stored IsItemSizeStored;
    property ItemPullHighlighting;
    property ItemPullHighlightingMode: TdxRibbonGalleryItemPullHighlightingMode read GetItemPullHighlightingMode write SetItemPullHighlightingMode stored IsItemPullHighlightingModeStored;
    property SpaceAfterGroupHeader;
    property SpaceBetweenItemCaptionAndDescription;
    property SpaceBetweenItemImageAndText;
    property SpaceBetweenItemsHorizontally;
    property SpaceBetweenItemsVertically;
  end;

  { TdxCustomRibbonGalleryOptions }

  TCustomdxRibbonGalleryOptionsClass = class of TdxCustomRibbonGalleryOptions;
  TdxCustomRibbonGalleryOptions = class(TdxRibbonGalleryBaseOptions)
  strict private
    FColumnCount: Integer;
    FItemAllowDeselect: Boolean;
    FItemHintSource: TdxRibbonGalleryItemHintSource;
    FItemSelectionMode: TdxRibbonGalleryItemSelectionMode;
    FLongDescriptionDefaultRowCount: Integer;
    //FShowItemHint: Boolean; deprecated
    FShowScrollbar: Boolean; // deprecated
    FSpaceBetweenGroups: Integer;
    FSpaceBetweenItemsAndBorder: Integer;

    FOnColumnCountChanged: TNotifyEvent;
    FOnSelectionChanged: TdxSelectionChangedEvent;

    procedure DoSelectionChanged(DeselectAll: Boolean);

    function GetCanCollapse: Boolean;
    function GetCollapsed: Boolean;
    function GetEqualItemSizeInAllGroups: Boolean;
    function GetItemSizeInRibbon: TcxItemSize;
    function GetMinColumnCount: Integer;
    function GetRowCount: Integer;
    function GetShowItemHint: Boolean;
    function GetSubmenuResizing: TdxRibbonGallerySubmenuResizing;

    procedure SetCanCollapse(Value: Boolean);
    procedure SetCollapsed(Value: Boolean);
    procedure SetColumnCount(Value: Integer);
    procedure SetSpaceBetweenGroups(Value: Integer);
    procedure SetEqualItemSizeInAllGroups(Value: Boolean);
    procedure SetItemHintSource(Value: TdxRibbonGalleryItemHintSource);
    procedure SetItemSelectionMode(Value: TdxRibbonGalleryItemSelectionMode);
    procedure SetItemSizeInRibbon(Value: TcxItemSize);
    procedure SetLongDescriptionDefaultRowCount(Value: Integer);
    procedure SetMinColumnCount(Value: Integer);
    procedure SetRowCount(Value: Integer);
    procedure SetShowItemHint(Value: Boolean);
    procedure SetSpaceBetweenItemsAndBorder(Value: Integer);
    procedure SetSubmenuResizing(Value: TdxRibbonGallerySubmenuResizing);

    procedure ReadLongDescriptionDefaultRowCount(Reader: TReader);
    procedure WriteLongDescriptionDefaultRowCount(Writer: TWriter);

    procedure ColumnCountChanged;
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoImageListContentChanged; override;

    property ShowItemHint: Boolean read GetShowItemHint write SetShowItemHint; // deprecated
    property ShowScrollbar: Boolean read FShowScrollbar write FShowScrollbar; // deprecated

    property CanCollapse: Boolean read GetCanCollapse write SetCanCollapse stored False;
    property Collapsed: Boolean read GetCollapsed write SetCollapsed stored False;
    property EqualItemSizeInAllGroups: Boolean read GetEqualItemSizeInAllGroups write SetEqualItemSizeInAllGroups stored False;
    property ItemAllowDeselect: Boolean read FItemAllowDeselect write FItemAllowDeselect default False;
    property ItemHintSource: TdxRibbonGalleryItemHintSource read FItemHintSource write SetItemHintSource default ghsItemHint;
    property ItemSelectionMode: TdxRibbonGalleryItemSelectionMode read FItemSelectionMode write SetItemSelectionMode default gsmSingle;
    property ItemSizeInRibbon: TcxItemSize read GetItemSizeInRibbon write SetItemSizeInRibbon stored False;
    property LongDescriptionDefaultRowCount: Integer read FLongDescriptionDefaultRowCount write SetLongDescriptionDefaultRowCount;
    property SpaceBetweenGroups: Integer read FSpaceBetweenGroups write SetSpaceBetweenGroups default 0;
    property SpaceBetweenItemsAndBorder: Integer read FSpaceBetweenItemsAndBorder write SetSpaceBetweenItemsAndBorder default 1;
    property SubmenuResizing: TdxRibbonGallerySubmenuResizing read GetSubmenuResizing write SetSubmenuResizing stored False;

    property ColumnCount: Integer read FColumnCount write SetColumnCount default dxRibbonGalleryDefaultColumnCount;
    property MinColumnCount: Integer read GetMinColumnCount write SetMinColumnCount stored False;
    property RowCount: Integer read GetRowCount write SetRowCount stored False;

    property OnSelectionChanged: TdxSelectionChangedEvent read FOnSelectionChanged write FOnSelectionChanged;
    property OnColumnCountChanged: TNotifyEvent read FOnColumnCountChanged write FOnColumnCountChanged;
  public
    constructor Create(AOwner: TdxCustomRibbonGalleryItem); override;
    procedure Assign(Source: TPersistent); override;
  end;

  { TdxRibbonGalleryOptions }

  TdxRibbonGalleryOptions = class(TdxCustomRibbonGalleryOptions)
  published
    property CanCollapse;
    property Collapsed;
    property ColumnCount;
    property EqualItemSizeInAllGroups;
    property Images;
    property ItemAllowDeselect;
    property ItemHintSource;
    property ItemImagePosition;
    property ItemImageSize;
    property ItemPullHighlighting;
    property ItemSelectionMode;
    property ItemSize;
    property ItemSizeInRibbon;
    property ItemTextKind;
    property MinColumnCount;
    property RowCount;
    property ShowItemHint stored False; // deprecated
    property ShowScrollbar stored False; // deprecated
    property SpaceAfterGroupHeader;
    property SpaceBetweenGroups;
    property SpaceBetweenItemCaptionAndDescription;
    property SpaceBetweenItemImageAndText;
    property SpaceBetweenItemsAndBorder;
    property SpaceBetweenItemsHorizontally;
    property SpaceBetweenItemsVertically;
    property SubmenuResizing;
  end;

  { TdxInRibbonGalleryOptions }

  TdxInRibbonGalleryOptions = class(TPersistent)
  strict private
    FAlwaysShowItemCaption: Boolean;
    FCanCollapse: Boolean;
    FCollapsed: Boolean;
    FItemSize: TcxItemSize;
    FMinColumnCount: Integer;

    FOnChanged: TNotifyEvent;
    FOnMinColumnCountChanged: TNotifyEvent;

    //changed
    procedure Changed;
    procedure MinColumnCountChanged;

    procedure ItemSizeChange(Sender: TObject);

    procedure SetAlwaysShowItemCaption(Value: Boolean);
    procedure SetCanCollapse(Value: Boolean);
    procedure SetCollapsed(Value: Boolean);
    procedure SetItemSize(Value: TcxItemSize);
    procedure SetMinColumnCount(Value: Integer);
  protected
    procedure ChangeScale(M, D: Integer);

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnMinColumnCountChanged: TNotifyEvent read FOnMinColumnCountChanged write FOnMinColumnCountChanged;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AlwaysShowItemCaption: Boolean read FAlwaysShowItemCaption write SetAlwaysShowItemCaption default False;
    property CanCollapse: Boolean read FCanCollapse write SetCanCollapse default True;
    property Collapsed: Boolean read FCollapsed write SetCollapsed default False;
    property ItemSize: TcxItemSize read FItemSize write SetItemSize;
    property MinColumnCount: Integer read FMinColumnCount write SetMinColumnCount default dxRibbonGalleryMinColumnCount;
  end;

  { TdxInMenuGalleryOptions }

  TdxInMenuGalleryOptions = class(TPersistent)
  strict private
    FCollapsedInSubmenu: Boolean;
    FDropDownGalleryResizing: TdxRibbonGalleryDropDownResizing;
    FEqualItemSizeInAllGroups: Boolean;
    FItemPullHighlightingMode: TdxRibbonGalleryItemPullHighlightingMode;
    FItemSize: TcxItemSize;
    FItemTextAlignVert: TcxAlignmentVert;
    FItemTextKind: TdxRibbonGalleryGroupItemTextKind;
    FRowCount: Integer;

    FOnChanged: TNotifyEvent;

    //changed
    procedure Changed;

    function IsItemSizeStored: Boolean;
    procedure ItemSizeChange(Sender: TObject);
    procedure SetCollapsedInSubmenu(Value: Boolean);
    procedure SetEqualItemSizeInAllGroups(Value: Boolean);
    procedure SetItemPullHighlightingMode(Value: TdxRibbonGalleryItemPullHighlightingMode);
    procedure SetItemSize(Value: TcxItemSize);
    procedure SetItemTextAlignVert(Value: TcxAlignmentVert);
    procedure SetItemTextKind(Value: TdxRibbonGalleryGroupItemTextKind);
    procedure SetRowCount(Value: Integer);
  protected
    procedure ChangeScale(M, D: Integer);
    //
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property CollapsedInSubmenu: Boolean read FCollapsedInSubmenu write SetCollapsedInSubmenu default True;
    property DropDownGalleryResizing: TdxRibbonGalleryDropDownResizing read FDropDownGalleryResizing write FDropDownGalleryResizing default gsrWidthAndHeight;
    property EqualItemSizeInAllGroups: Boolean read FEqualItemSizeInAllGroups write SetEqualItemSizeInAllGroups default True;
    property ItemPullHighlightingMode: TdxRibbonGalleryItemPullHighlightingMode read FItemPullHighlightingMode write SetItemPullHighlightingMode default iphmNone;
    property ItemSize: TcxItemSize read FItemSize write SetItemSize stored IsItemSizeStored;
    property ItemTextAlignVert: TcxAlignmentVert read FItemTextAlignVert write SetItemTextAlignVert default vaTop;
    property ItemTextKind: TdxRibbonGalleryGroupItemTextKind read FItemTextKind write SetItemTextKind default itkCaption;
    property RowCount: Integer read FRowCount write SetRowCount default 0;
  end;

  { TdxRibbonGalleryGroupHeader }

  TdxRibbonGalleryGroupHeader = class(TdxCustomGalleryGroupHeader)
  protected
    procedure Changed; override;
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property Alignment default taLeftJustify;
    property Caption;
    property Visible default False;
  end;

  { TdxRibbonGalleryGroupItem }

  TdxRibbonGalleryGroupItem = class(TdxCustomGalleryItem)
  strict private
    FActionLink: TdxRibbonGalleryGroupItemActionLink;
    FGlyphInDropDown: TdxSmartGlyph;
    FSelected: Boolean;

    FOnClick: TNotifyEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;

    procedure DoActionChange(Sender: TObject);
    function GetAction: TBasicAction;
    function GetGalleryItem: TdxCustomRibbonGalleryItem;
    function GetGroup: TdxRibbonGalleryGroup;
    function GetSelected: Boolean;
    function GetSelectionMode: TdxRibbonGalleryItemSelectionMode;
    function IsCaptionStored: Boolean;
    function IsDesigning: Boolean;
    function IsEnabledStored: Boolean;
    function IsHintStored: Boolean;
    function IsImageIndexStored: Boolean;
    function IsOnClickStored: Boolean;
    function IsSelectedStored: Boolean;
    procedure SetAction(Value: TBasicAction);
    procedure SetGlyphInDropDown(Value: TdxSmartGlyph);
    procedure SetSelected(Value: Boolean);
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); dynamic;
    procedure DoClick; dynamic;
    function GetActionLinkClass: TdxRibbonGalleryGroupItemActionLinkClass; dynamic;
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;
    function GetEnabled: Boolean;
    function GetOwner: TPersistent; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); dynamic;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateActionLink;

    property GalleryItem: TdxCustomRibbonGalleryItem read GetGalleryItem;
    property GlyphInDropDown: TdxSmartGlyph read FGlyphInDropDown write SetGlyphInDropDown;
    property LoadedSelected: Boolean read FSelected write FSelected;
    property SelectionMode: TdxRibbonGalleryItemSelectionMode read GetSelectionMode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Group: TdxRibbonGalleryGroup read GetGroup;
  published
    property Action: TBasicAction read GetAction write SetAction;
    property Caption stored IsCaptionStored;
    property Description;
    property Enabled stored IsEnabledStored default True;
    property Glyph;
    property Hint stored IsHintStored;
    property ImageIndex stored IsImageIndexStored default -1;
    property Selected: Boolean read GetSelected write SetSelected stored IsSelectedStored default False;
    property OnClick: TNotifyEvent read FOnClick write FOnClick stored IsOnClickStored;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
  end;

  { TdxRibbonGalleryGroupItems }

  TdxRibbonGalleryGroupItems = class(TdxCustomGalleryItems)
  strict private
    function GetGroup: TdxRibbonGalleryGroup;
    function GetItem(Index: Integer): TdxRibbonGalleryGroupItem;
    procedure SetItem(Index: Integer; Value: TdxRibbonGalleryGroupItem);
  protected
    function GetOwner: TPersistent; override;
    procedure SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1); override;

    property Group: TdxRibbonGalleryGroup read GetGroup;
  public
    function Add: TdxRibbonGalleryGroupItem;
    function Insert(Index: Integer): TdxRibbonGalleryGroupItem;
    property Items[Index: Integer]: TdxRibbonGalleryGroupItem read GetItem write SetItem; default;
  end;

  { TdxRibbonGalleryGroup }

  TdxRibbonGalleryGroup = class(TdxCustomGalleryGroup)
  strict private
    FOptions: TdxRibbonGalleryGroupOptions;

    function GetGalleryItem: TdxCustomRibbonGalleryItem;
    function GetImages: TCustomImageList;
    function GetItems: TdxRibbonGalleryGroupItems;
    function GetHeader: TdxRibbonGalleryGroupHeader;
    procedure ReadItems(AReader: TReader);
    procedure SetHeader(Value: TdxRibbonGalleryGroupHeader);
    procedure SetItems(Value: TdxRibbonGalleryGroupItems);
    procedure SetOptions(Value: TdxRibbonGalleryGroupOptions);
  protected
    function GetGalleryGroupHeaderClass: TdxGalleryGroupHeaderClass; override;
    function GetGalleryItemClass: TdxGalleryItemClass; override;
    function GetGalleryItemsClass: TdxGalleryItemsClass; override;

    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;
    function GetOwner: TPersistent; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;

    property Images: TCustomImageList read GetImages;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetParentComponent(Value: TComponent); override;

    property GalleryItem: TdxCustomRibbonGalleryItem read GetGalleryItem;
    property Items: TdxRibbonGalleryGroupItems read GetItems write SetItems;
  published
    property Header: TdxRibbonGalleryGroupHeader read GetHeader write SetHeader;
    property Options: TdxRibbonGalleryGroupOptions read FOptions write SetOptions;
    property Visible default True;
  end;

  { TdxRibbonGalleryGroups }

  TdxRibbonGalleryGroups = class(TdxCustomGalleryGroups)
  strict private
    function GetGalleryItem: TdxCustomRibbonGalleryItem;
    function GetItem(Index: Integer): TdxRibbonGalleryGroup;
    procedure RemoveFromFilter(AItem: TcxComponentCollectionItem);
    procedure SetItem(Index: Integer; Value: TdxRibbonGalleryGroup);
  protected
    function GetOwner: TPersistent; override;
    procedure Notify(AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification); override;
    procedure SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1); override;
    procedure Update(AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification); override;

    property GalleryItem: TdxCustomRibbonGalleryItem read GetGalleryItem;
  public
    function Add: TdxRibbonGalleryGroup;
    function Insert(Index: Integer): TdxRibbonGalleryGroup;
    property Items[Index: Integer]: TdxRibbonGalleryGroup read GetItem write SetItem; default;
  end;

  { TdxRibbonGalleryFilterCategoryGroups }

  TdxRibbonGalleryFilterCategoryGroups = class(TList)
  strict private
    FFilterCategory: TdxRibbonGalleryFilterCategory;

    function CanAddGroup(AGroup: TdxRibbonGalleryGroup): Boolean;
    function GetItem(Index: Integer): TdxRibbonGalleryGroup;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(AFilterCategory: TdxRibbonGalleryFilterCategory);
    procedure Assign(ASource: TdxRibbonGalleryFilterCategoryGroups);
    function Add(AGroup: TdxRibbonGalleryGroup): Integer;
    procedure Insert(AIndex: Integer; AGroup: TdxRibbonGalleryGroup);
    property FilterCategory: TdxRibbonGalleryFilterCategory read FFilterCategory;
    property Items[Index: Integer]: TdxRibbonGalleryGroup read GetItem; default;
  end;

  { TdxRibbonGalleryFilterCategory }

  TdxRibbonGalleryFilterCategory = class(TCollectionItem)
  strict private
    FCaption: string;
    FGroupIndexes: TList;
    FGroups: TdxRibbonGalleryFilterCategoryGroups;

    procedure AddGroupByGroupIndex(AIndex: Integer);
    function GetGalleryItem: TdxCustomRibbonGalleryItem;
    procedure ReadCategoryGroups(AReader: TReader);
    procedure WriteCategoryGroups(AWriter: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure RestoreGroupIndexes;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property GalleryItem: TdxCustomRibbonGalleryItem read GetGalleryItem;
  published
    property Caption: string read FCaption write FCaption;
    property Groups: TdxRibbonGalleryFilterCategoryGroups read FGroups stored False;
  end;

  { TdxRibbonGalleryFilterCategories }

  TdxRibbonGalleryFilterCategories = class(TCollection)
  strict private
    FGalleryFilter: TdxRibbonGalleryFilter;

    function GetItem(Index: Integer): TdxRibbonGalleryFilterCategory;
    procedure SetItem(Index: Integer; Value: TdxRibbonGalleryFilterCategory);
  protected
    procedure DeleteGroup(AGroup: TdxRibbonGalleryGroup);
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;

    property GalleryFilter: TdxRibbonGalleryFilter read FGalleryFilter;
  public
    constructor Create(AGalleryFilter: TdxRibbonGalleryFilter);
    function Add: TdxRibbonGalleryFilterCategory;
    property Items[Index: Integer]: TdxRibbonGalleryFilterCategory read GetItem write SetItem; default;
  end;

  { TdxRibbonGalleryFilter }

  TdxRibbonGalleryFilter = class(TPersistent)
  strict private
    FActiveCategoryIndex: Integer;
    FCaption: string;
    FCategories: TdxRibbonGalleryFilterCategories;
    FGalleryItem: TdxCustomRibbonGalleryItem;
    FLoadedActiveCategoryIndex: Integer;
    FVisible: Boolean;

    procedure SetActiveCategoryIndex(Value: Integer);
    procedure SetCaption(const Value: string);
    procedure SetCategories(Value: TdxRibbonGalleryFilterCategories);
  protected
    procedure CategoriesChanged;
    function GetOwner: TPersistent; override;
    procedure Loaded;
    property GalleryItem: TdxCustomRibbonGalleryItem read FGalleryItem;
  public
    constructor Create(AGalleryItem: TdxCustomRibbonGalleryItem);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function IsGroupFiltered(AGroup: TdxRibbonGalleryGroup): Boolean;
  published
    property ActiveCategoryIndex: Integer read FActiveCategoryIndex write SetActiveCategoryIndex default -1;
    property Caption: string read FCaption write SetCaption;
    property Categories: TdxRibbonGalleryFilterCategories read FCategories write SetCategories;
    property Visible: Boolean read FVisible write FVisible default False;
  end;

  { TdxCustomRibbonGalleryItemActionLink }

  TdxCustomRibbonGalleryItemActionLink = class(TdxBarItemActionLink)
  strict private
    function GetCustomRibbonGalleryItem: TdxCustomRibbonGalleryItem;
  protected
    function FindGroupItem(AActionIndex: Variant): TdxRibbonGalleryGroupItem;

    property CustomRibbonGalleryItem: TdxCustomRibbonGalleryItem read GetCustomRibbonGalleryItem;
  public
    function Update: Boolean; override;
  end;

  { TdxRibbonGalleryItemBarItemLink }

  TdxRibbonGalleryItemBarItemLinkPositionInDropDown = (ilpAfterGallery, ilpBeforeGallery);

  TdxRibbonGalleryItemBarItemLink = class(TdxBarItemLink, IdxBarCustomItemLink)
  strict private
    FPositionInDropDown: TdxRibbonGalleryItemBarItemLinkPositionInDropDown;

    procedure SetPositionInDropDown(const AValue: TdxRibbonGalleryItemBarItemLinkPositionInDropDown);

    function IsNotFirstVisible: Boolean;
    procedure PositionInDropDownCustomizationPopupItemClickHandler(Sender: TObject);
  protected
    function GetBeginGroup: Boolean; override;
    procedure SetBeginGroup(Value: Boolean); override;

    procedure LoadFromIni(ASource: TCustomIniFile; const ABaseSection: string; ALinkIndex: Integer;
      AStoringKind: TdxBarStoringKind); override;
    procedure SaveToIni(ADestination: TCustomIniFile; const ABaseSection: string; ALinkIndex: Integer;
      AStoringKind: TdxBarStoringKind); override;

    // IdxBarCustomItemLink
    procedure ProcessCustomizationPopup(ACustomizationPopupItemLinks: TdxBarItemLinks); virtual;
    function ProcessDragLineParameters(var ADragLineFirstPart, ADragOverBeginGroup: Boolean;
      var ADragLineRect: TRect): Boolean; virtual;
    procedure ProcessDroppedItemLink(ADroppedItemLink: TdxBarItemLink); virtual;
    function ProcessNeedDragLine(ADraggingItemLink: TdxBarItemLink;
      const ADragOverFirstPart, ADragOverBeginGroup: Boolean): Boolean; virtual;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property PositionInDropDown: TdxRibbonGalleryItemBarItemLinkPositionInDropDown read FPositionInDropDown write SetPositionInDropDown default ilpAfterGallery;
  end;

  { TdxRibbonGalleryItemBarItemLinks }

  TdxRibbonGalleryItemBarItemLinks = class(TdxBarSubMenuControlItemLinks)
  protected
    function GetItemLinkClass: TdxBarItemLinkClass; override;
  end;

  { TdxCustomRibbonGalleryItem }

  TdxRibbonGalleryHotTrackedItemChangedEvent = procedure(
    APrevHotTrackedGroupItem, ANewHotTrackedGroupItem: TdxRibbonGalleryGroupItem) of object;

  TdxCustomRibbonGalleryItem = class(TCustomdxBarSubItem)
  private
    FDropDownGallery: TdxRibbonDropDownGallery;

    // options
    FGalleryFilter: TdxRibbonGalleryFilter;
    FGalleryGroups: TdxRibbonGalleryGroups;
    FGalleryOptions: TdxCustomRibbonGalleryOptions;
    FGalleryInRibbonOptions: TdxInRibbonGalleryOptions;
    FGalleryInMenuOptions: TdxInMenuGalleryOptions;

    // counters
    FFilterChangedLockCount: Integer;
    FLockGroupItemClickEventsCount: Integer;

    // navigation
    FClickedGroupItem: TdxRibbonGalleryGroupItem;
    FFirstVisibleGroupItem: TdxRibbonGalleryGroupItem;
    FSelectedGroupItem: TdxRibbonGalleryGroupItem;

    // flags
    FIsClone: Boolean;
    FRecalculatingOnFilterChanged: Boolean;

    // events
    FOnHotTrackedItemChanged: TdxRibbonGalleryHotTrackedItemChangedEvent;

    function GetActionLink: TdxCustomRibbonGalleryItemActionLink;
    procedure ReadGalleryGroups(AReader: TReader);
    procedure SetDropDownGallery(Value: TdxRibbonDropDownGallery);
    procedure SetGalleryFilter(Value: TdxRibbonGalleryFilter);
    procedure SetGalleryGroups(Value: TdxRibbonGalleryGroups);
    procedure SetGalleryInRibbonOptions(Value: TdxInRibbonGalleryOptions);
    procedure SetGalleryInMenuOptions(Value: TdxInMenuGalleryOptions);
    procedure SetGalleryOptions(Value: TdxCustomRibbonGalleryOptions);
    procedure SetSelectedGroupItem(Value: TdxRibbonGalleryGroupItem);
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CanBePlacedOn(AParentKind: TdxBarItemControlParentKind; AToolbar: TdxBar; out AErrorText: string): Boolean; override;
    procedure DoCloseUp; override;
    function GetActionLinkClass: TdxBarItemActionLinkClass; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetItemLinksClass: TdxBarItemLinksClass; override;
    class function GetNewCaption: string; override;
    function InternalCanMergeWith(AItem: TdxBarItem): Boolean; override;
    procedure UpdateEx(AParentKinds: TdxBarKinds = dxBarKindAny); override;

    function AreGroupItemClickEventsLocked: Boolean;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoGroupItemClick(AItem: TdxRibbonGalleryGroupItem); virtual;
    procedure DoInitFilterMenu(AItemLinks: TdxBarItemLinks); virtual;
    procedure DoPopup; override;

    function GetGroupItemClass: TdxGalleryItemClass; virtual;
    function GetGalleryOptionsClass: TCustomdxRibbonGalleryOptionsClass; virtual;
    function GetErrorCanPlaceText : string; virtual;

    // changed
    procedure ColumnCountChanged(Sender: TObject);
    procedure DoHotTrackedItemChanged(APrevHotTrackedGroupItem, ANewHotTrackedGroupItem: TdxRibbonGalleryGroupItem); virtual;
    procedure DoFilterChanged; virtual;
    procedure FilterCaptionChanged;
    procedure FilterChanged;
    procedure GalleryChanged;
    procedure ItemSelectionModeChanged(Sender: TObject; DeselectAll: Boolean);
    procedure MinColumnCountChanged(Sender: TObject);
    procedure OptionsChanged(Sender: TObject);

    function GetFilterCaption: string;
    function GetImages: TCustomImageList;
    procedure GroupVisibleChanged;

    function IsFilterVisible: Boolean;
    procedure LockFilterChanged(ALock: Boolean);
    procedure RemoveGroupItem(AItem: TdxRibbonGalleryGroupItem);

    // internal properties
    property IsClone: Boolean read FIsClone;
    property RecalculatingOnFilterChanged: Boolean read FRecalculatingOnFilterChanged;

    property ActionLink: TdxCustomRibbonGalleryItemActionLink read GetActionLink;
    property DropDownGallery: TdxRibbonDropDownGallery read FDropDownGallery write SetDropDownGallery;
    property GalleryCategories: TdxRibbonGalleryGroups read FGalleryGroups write SetGalleryGroups;
    property GalleryFilter: TdxRibbonGalleryFilter read FGalleryFilter write SetGalleryFilter;
    property GalleryInRibbonOptions: TdxInRibbonGalleryOptions read FGalleryInRibbonOptions write SetGalleryInRibbonOptions;
    property GalleryInMenuOptions: TdxInMenuGalleryOptions read FGalleryInMenuOptions write SetGalleryInMenuOptions;
    property GalleryOptions: TdxCustomRibbonGalleryOptions read FGalleryOptions write SetGalleryOptions;
    property OnHotTrackedItemChanged: TdxRibbonGalleryHotTrackedItemChangedEvent read FOnHotTrackedItemChanged write FOnHotTrackedItemChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CopyGroupItem(AGroupItem: TdxRibbonGalleryGroupItem;
      ATargetGroup: TdxRibbonGalleryGroup; AIndex: Integer): TdxRibbonGalleryGroupItem;
    procedure DoClick; override;
    function GetAddMessageName: string; override;
    function IsGroupVisible(AGroupIndex: Integer; AIgnoreVisibleProperty: Boolean = False): Boolean;
    procedure LockGroupItemClickEvents(ALock: Boolean);
    function MoveGroupItem(AGroupItem: TdxRibbonGalleryGroupItem; ATargetGroup: TdxRibbonGalleryGroup; AIndex: Integer): TdxRibbonGalleryGroupItem;
    procedure ShowGroupItem(AGroupItem: TdxRibbonGalleryGroupItem);

    property GalleryGroups: TdxRibbonGalleryGroups read FGalleryGroups write SetGalleryGroups;
    property SelectedGroupItem: TdxRibbonGalleryGroupItem read FSelectedGroupItem write SetSelectedGroupItem;
  end;

  { TdxRibbonGalleryItem }
  TdxRibbonGalleryFilterChangedEvent = procedure(Sender: TdxRibbonGalleryItem) of object;
  TdxRibbonGalleryGroupItemClickEvent = procedure(Sender: TdxRibbonGalleryItem;
    AItem: TdxRibbonGalleryGroupItem) of object;
  TdxRibbonGalleryInitFilterMenuEvent = procedure(Sender: TdxRibbonGalleryItem;
    AItemLinks: TdxBarItemLinks) of object;

  TdxRibbonGalleryItem = class(TdxCustomRibbonGalleryItem, IdxGallery)
  private
    FOnFilterChanged: TdxRibbonGalleryFilterChangedEvent;
    FOnGroupItemClick: TdxRibbonGalleryGroupItemClickEvent;
    FOnInitFilterMenu: TdxRibbonGalleryInitFilterMenuEvent;
    function GetGalleryOptions: TdxRibbonGalleryOptions;
    procedure SetGalleryOptions(Value: TdxRibbonGalleryOptions);
  protected
    procedure DoFilterChanged; override;
    procedure DoGroupItemClick(AItem: TdxRibbonGalleryGroupItem); override;
    procedure DoInitFilterMenu(AItemLinks: TdxBarItemLinks); override;
    function GetGalleryOptionsClass: TCustomdxRibbonGalleryOptionsClass; override;
    procedure UpdateActionLink; override;

    // IdxGallery
    function GetAllowedCustomizationActions: TdxGalleryCustomizationActions;
    function GetGroups: IdxGalleryGroups;
  published
    //TCustomdxBarSubItem
    property Glyph;
    property ImageIndex;
    property LargeGlyph;
    property LargeImageIndex;
    property ShowCaption default True;
    property OnClick;

    property GalleryOptions: TdxRibbonGalleryOptions read GetGalleryOptions write SetGalleryOptions;
    property GalleryCategories;
    property GalleryFilter;
    property GalleryInRibbonOptions;
    property GalleryInMenuOptions;
    property ItemLinks;
    property ItemOptions;

    property OnCloseUp;
    property OnFilterChanged: TdxRibbonGalleryFilterChangedEvent read FOnFilterChanged write FOnFilterChanged;
    property OnGroupItemClick: TdxRibbonGalleryGroupItemClickEvent read FOnGroupItemClick write FOnGroupItemClick;
    property OnHotTrackedItemChanged;
    property OnInitFilterMenu: TdxRibbonGalleryInitFilterMenuEvent read FOnInitFilterMenu write FOnInitFilterMenu;
    property OnPopup;
  end;

  { TdxRibbonGalleryController }

  TdxRibbonGalleryController = class
  private
    FGroupItemHotTrackEnabled: Boolean;
    FHintItem: TdxRibbonGalleryGroupItem;
    FKeyboardHotGroupItem: TdxRibbonGalleryGroupItem;
    FLastCommandFromKeyboard: Boolean;
    FMouseDownGroupItem: TdxRibbonGalleryGroupItem;
    FOwner: TdxRibbonGalleryControl;

    function GetEnabled: Boolean;
    function GetFirstGroupItem: TdxRibbonGalleryGroupItem;
    function GetGalleryItem: TdxCustomRibbonGalleryItem;
    function GetGroupCount: Integer;
    function GetKeyboardHotGroupItem: TdxRibbonGalleryGroupItem;
    function GetViewInfo: TdxRibbonGalleryControlViewInfo;
    procedure GroupItemMouseMove(AGroupItem: TdxRibbonGalleryGroupItem; Shift: TShiftState; X, Y: Integer);
    procedure ProcessHotTrack(AGroupItem: TdxRibbonGalleryGroupItem);
    procedure SetHotGroupItem(const Value: TdxRibbonGalleryGroupItem);
    procedure UnsetDownedFromGroupItem(AGroupItem: TdxRibbonGalleryGroupItem);
  protected
    procedure CancelHint;
    function CheckEnabled(AGroupItem: TdxRibbonGalleryGroupItem): TdxRibbonGalleryGroupItem;
    function GetGroupItem(AGroupIndex, AIndex: Integer): TdxRibbonGalleryGroupItem;
    procedure HotTrackItem(AItem: TdxRibbonGalleryGroupItem);
    procedure SetHintItem(AItem: TdxRibbonGalleryGroupItem);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseLeave; virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;

    property Enabled: Boolean read GetEnabled;
    property GalleryItem: TdxCustomRibbonGalleryItem read GetGalleryItem;
    property GroupCount: Integer read GetGroupCount;
    property GroupItemHotTrackEnabled: Boolean read FGroupItemHotTrackEnabled write FGroupItemHotTrackEnabled;
  public
    constructor Create(AOwner: TdxRibbonGalleryControl); virtual;
    property KeyboardHotGroupItem: TdxRibbonGalleryGroupItem read GetKeyboardHotGroupItem write FKeyboardHotGroupItem;
    property ViewInfo: TdxRibbonGalleryControlViewInfo read GetViewInfo;
  end;

  { TdxRibbonOnSubmenuGalleryController }

  TdxRibbonOnSubmenuGalleryController = class(TdxRibbonGalleryController)
  strict private
    FFilterMenuLinksOwner: TdxBarInternalLinksOwner;

    procedure CheckFilterMenuHotTrack;
    procedure FilterMenuButtonClick(Sender: TObject);
    procedure FilterMenuCategoryButtonClick(Sender: TObject);
    procedure FilterMenuGroupButtonClick(Sender: TObject);
    function GetFirstGroupItem: TdxRibbonGalleryGroupItem;
    function GetGalleryWidth: Integer;
    function GetLastGroupItem: TdxRibbonGalleryGroupItem;
    function GetViewInfo: TdxRibbonOnMenuGalleryControlViewInfo;
    procedure InitFilterMenu(AItemLinks: TdxBarItemLinks);
    function IsFilterMenuInternalButton(AItem: TdxBarItem): Boolean;
  protected
    procedure HotTrackFirstGroupItem;
    procedure HotTrackLastGroupItem;
    function IsFilterMenuShowed: Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Navigation(ADirection: TcxAccessibilityNavigationDirection);
    procedure HideFilterMenu;
    procedure ShowFilterMenu;
    procedure PageDown;
    procedure PageUp;
    procedure Tabulation;
  public
    destructor Destroy; override;
    property ViewInfo: TdxRibbonOnMenuGalleryControlViewInfo read GetViewInfo;
  end;

  { TdxRibbonGalleryFilterMenuControl }

  TdxRibbonGalleryFilterMenuControl = class(TdxBarInternalSubMenuControl)
  private
    FGalleryControl: TdxRibbonGalleryControl;
  protected
    function GetBehaviorOptions: TdxBarBehaviorOptions; override;
    function GetPainter: TdxBarPainter; override;
    procedure ProcessMouseDownMessageForMeaningParent(AWnd: HWND; AMsg: UINT; const AMousePos: TPoint); override;
    property GalleryControl: TdxRibbonGalleryControl read FGalleryControl;
  end;

  { TdxRibbonGalleryControl }

  TdxRibbonGalleryControl = class(TdxBarSubItemControl)
  strict private
    FCollapsed: Boolean;
    FController: TdxRibbonGalleryController;
    FHintBounds: TRect;
    FHintItem: TdxRibbonGalleryGroupItem;
    FIsClickOnItemsArea: Boolean;
    FIsClosingUpSubmenuControl: Boolean;
    FIsCollapsedAssigned: Boolean;
    FIsCreatingSubmenuControl: Boolean;
    FIsDroppingDown: Boolean;
    FIsNeedScrollBarLock: Boolean;
    FSizeChanged: Boolean;

    procedure DoScrollBarDropDown(Sender: TObject);
    procedure DrawInvalid(const ABounds: TRect);
    function GetItem: TdxCustomRibbonGalleryItem;
    function GetViewInfo: TdxRibbonGalleryControlViewInfo;
    procedure ObtainTextColors;
    procedure SetCollapsed(Value: Boolean);
  protected
    FScrollBar: TdxRibbonGalleryScrollBar;

    //hints
    function DoHint(var ANeedDeactivate: Boolean; out AHintText: string; out AShortCut: string): Boolean; override;
    function GetHintText(AGroupItem: TdxRibbonGalleryGroupItem): string;
    function GetHintPosition(const ACursorPos: TPoint; AHeight: Integer): TPoint; override;
    procedure UpdateHint(AHintItem: TdxRibbonGalleryGroupItem; const ABounds: TRect);

    function CalcDefaultWidth: Integer; virtual;
    function CalcMinHeight: Integer; virtual;
    procedure CalcParts; override;
    function CanClicked: Boolean; override;
    function CanCustomize: Boolean; override;
    procedure ControlUnclick(ByMouse: Boolean); override;
    function CreateController: TdxRibbonGalleryController; virtual;
    function CreateHintViewInfo(const AHintText, AShortCut: string): TdxBarCustomHintViewInfo; override;
    procedure DoCloseUp(AHadSubMenuControl: Boolean); override;
    procedure DoDropDown(AByMouse: Boolean); override;
    procedure DropDown(AByMouse: Boolean); override;
    procedure EnabledChanged; override;
    function GetClientHeight: Integer;
    function GetClientWidth: Integer;
    function GetDefaultHeightInSubMenu: Integer; override;
    function GetDefaultWidthInSubMenu: Integer; override;
    procedure GetDefaultTextColors(AEnabled, ASelected, AFlat: Boolean; var AColor1, AColor2: TColor); override;

    function GetCollapsed: Boolean; override;
    function GetMouseWheelStep: Integer;
    procedure GetSubMenuControlPositionParams(out P: TPoint; out AOwnerWidth, AOwnerHeight: Integer); override;
    function InternalGetDefaultWidth: Integer; override;
    procedure Changed;
    function WantsKey(Key: Word): Boolean; override;

    procedure CalcDrawParams(AFull: Boolean = True); override;
    procedure ControlActivate(AImmediately, AByMouse: Boolean); override;
    procedure ControlClick(AByMouse: Boolean; AKey: Char = #0); override;
    procedure CreateSubMenuControl; override;
    procedure DoCreateSubMenuControl; override;
    procedure DoPaint(ARect: TRect; PaintType: TdxBarPaintType); override;
    procedure FilterCaptionChanged;
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; override;
    function GetGroups: TdxRibbonGalleryGroups;
    function GetSubMenuControl: TdxBarSubMenuControl; override;
    function GetViewInfoClass: TdxBarItemControlViewInfoClass; override;
    function GetVisibleGroupCount: Integer;
    function HasSubMenu: Boolean; override;
    function CanDestroyOnClick: Boolean; override;
    function IsEnabledScrollBar: Boolean;
    function IsHiddenForCustomization: Boolean; override;
    function IsNeedScrollBar: Boolean; virtual;
    function IsValidPainter: Boolean;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure DoScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure DoScrollBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    function GetScrollBarMaxPos: Integer;
    procedure ScrollBarSetup;
    procedure SetScrollBarPosition(APosition: Integer); virtual;

    function NeedBufferedRepaint: Boolean; override;

    property Collapsed: Boolean read GetCollapsed write SetCollapsed;
    property Controller: TdxRibbonGalleryController read FController;
    property ScrollBar: TdxRibbonGalleryScrollBar read FScrollBar;
    property SizeChanged: Boolean read FSizeChanged write FSizeChanged;
  public
    constructor Create(AItemLink: TdxBarItemLink); override;
    destructor Destroy; override;
    procedure Update(const R: TRect); override;
    procedure ShowGroupItem(AGroupItem: TdxRibbonGalleryGroupItem);

    property ClientHeight: Integer read GetClientHeight;
    property ClientWidth: Integer read GetClientWidth;
    property Item: TdxCustomRibbonGalleryItem read GetItem;
    property ViewInfo: TdxRibbonGalleryControlViewInfo read GetViewInfo;
  end;

  { TdxRibbonGalleryGroupElementViewInfo }

  TdxRibbonGalleryGroupElementViewInfo = class
  strict private
    FBounds: TRect;
    FOwner: TdxRibbonGalleryGroupViewInfo;
  protected
    function GetCaption: string; virtual; abstract;
    function GetFont: TFont; virtual;
    function GetGalleryItemControl: TdxRibbonGalleryControl;
    function GetScaleFactor: TdxScaleFactor;
    function GetTextFlags(AAlignment: TAlignment): DWORD;
  public
    constructor Create(AOwner: TdxRibbonGalleryGroupViewInfo);
    procedure Calculate(const ABounds: TRect); virtual;
    procedure Paint(ACanvas: TcxCanvas); virtual;

    property Bounds: TRect read FBounds;
    property Caption: string read GetCaption;
    property Font: TFont read GetFont;
    property Owner: TdxRibbonGalleryGroupViewInfo read FOwner;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  end;

  { TdxRibbonGalleryGroupHeaderViewInfo }

  TdxRibbonGalleryGroupHeaderViewInfo = class(TdxRibbonGalleryGroupElementViewInfo)
  strict private
    FTextBounds: TRect;

    function IsVisible: Boolean;
  protected
    function GetCaption: string; override;
    function GetHeight(AWidth: Integer; AWithSpaceAfterHeader: Boolean): Integer;
    function GetTextBounds: TRect; virtual;
  public
    procedure Calculate(const ABounds: TRect); override;
    procedure Paint(ACanvas: TcxCanvas); override;
    property TextBounds: TRect read FTextBounds;
  end;

  { TdxRibbonGalleryGroupItemViewInfo }

  TdxRibbonGalleryGroupItemViewInfo = class(TdxRibbonGalleryGroupElementViewInfo)
  private
    FCaptionBounds: TRect;
    FCaptionVisibilityState: TdxRibbonGalleryVisibilityState;
    FCaptionWidth: Integer;
    FChanged: Boolean;
    FDescriptionBounds: TRect;
    FDescriptionLines: TStrings;
    FDescriptionRowCount: Integer;
    FDescriptionSize: TSize;
    FDescriptionVisibilityState: TdxRibbonGalleryVisibilityState;
    FGroupItem: TdxRibbonGalleryGroupItem;
    FImageBounds: TRect;
    FPredefinedItemSize: TSize;

    procedure CheckCaptionFontStyle(AFont: TFont);
    function GetDescriptionLength: Integer;
    function GetDescriptionRect(AMaxWidth: Integer): TRect;
    function GetDowned: Boolean;
    function GetHotGroupItem: TdxRibbonGalleryGroupItem;
    function GetHorizontalImageIndent: Integer;
    function GetImagePlaceSize: TSize;
    function GetIsItemPullHighlighting: Boolean;
    function GetOptions: TdxRibbonGalleryGroupOptions;
    function GetPainter: TdxBarPainter;
    function GetRectConsiderBounds(const ARect: TRect): TRect;
    function GetSelected: Boolean;
    function GetVerticalImageIndent: Integer;
    function IsCaptionVisible: Boolean;
    function IsDescriptionVisible: Boolean;
    function IsImageVisible: Boolean;
    function IsInplaceGallery: Boolean;
    function IsMergeItemsImages: Boolean;
    function IsThisGroupItem(AGroupItem: TdxRibbonGalleryGroupItem): Boolean;
    function ItemHeightWithoutImage(var ADescriptionRect: TRect): Integer;
    function ItemWidthWithoutImage(var ADescriptionRect: TRect; AMaxWidth: Integer): Integer;
    procedure ValidateDescriptionStrings(ACanvas: TcxCanvas);
  protected
    function CalculateImageSize: TSize; virtual;
    procedure DrawImage(DC: HDC; const ARect: TRect);
    procedure DrawItemText(ACanvas: TcxCanvas); virtual;
    function GetActualGroupItemIndent: Integer;
    function GetCaption: string; override;
    function GetCaptionHeight: Integer; virtual;
    function GetCaptionWidth: Integer; virtual;
    function GetCurrentGlyph: TdxSmartGlyph;
    function GetDescription: string; virtual;
    function GetDescriptionHeight(var ADescriptionRect: TRect): Integer; virtual;
    function GetDescriptionWidth(var ADescriptionRect: TRect; AMaxWidth: Integer): Integer; virtual;
    function GetHotTracked: Boolean;
    function GetImageSize: TSize;
    function GetItemSize(AMaxWidth, AMaxHeight: Integer): TSize;
    function GetRibbonSkin: IdxSkin;
    function GetSpaceBetweenItemCaptionAndDescription: Integer; virtual;
    function GetSpaceBetweenItemImageAndText: Integer; virtual;
    function GetUnsizedImageSize: TSize; virtual;
    function IsImageAssigned: Boolean;

    function GetCaptionBounds: TRect; virtual;
    function GetDescriptionBounds: TRect; virtual;
    function GetImageBounds: TRect; virtual;
    function GetTextLeft: Integer; virtual;
    function GetTextRight: Integer; virtual;
    function GetTextTop: Integer; virtual;
    function IsBoldCaption: Boolean; virtual;

    procedure CheckTextVerticalAlignment(AVerticalAlignment: TcxAlignmentVert); virtual;
    procedure ResetCachedValues;
    procedure SetPredefinedItemSize(const AValue: TSize);

    property DescriptionLines: TStrings read FDescriptionLines;
    property HorizontalImageIndent: Integer read GetHorizontalImageIndent;
    property HotGroupItem: TdxRibbonGalleryGroupItem read GetHotGroupItem;
    property IsItemPullHighlighting: Boolean read GetIsItemPullHighlighting;
    property Options: TdxRibbonGalleryGroupOptions read GetOptions;
    property Painter: TdxBarPainter read GetPainter;
    property VerticalImageIndent: Integer read GetVerticalImageIndent;
  public
    constructor Create(AOwner: TdxRibbonGalleryGroupViewInfo; AGroupItem: TdxRibbonGalleryGroupItem);
    destructor Destroy; override;
    procedure Calculate(const ABounds: TRect); override;
    procedure Paint(ACanvas: TcxCanvas); override;
    property Description: string read GetDescription;
    property GroupItem: TdxRibbonGalleryGroupItem read FGroupItem;
  end;

  { TdxRibbonGalleryGroupViewInfo }

  TdxRibbonGalleryGroupRepaintPart = (ggrpAll, ggrpBefore, ggrpAfter, ggrpBetween);

  TdxRibbonGalleryGroupViewInfo = class
  private
    FBounds: TRect;
    FGroup: TdxRibbonGalleryGroup;
    FHeader: TdxRibbonGalleryGroupHeaderViewInfo;
    FItems: TcxObjectList;
    FItemSize: TSize;
    FOwner: TdxRibbonGalleryControlViewInfo;
    function GetFirstItemInGroupRow(ARowIndex, AColumnCount: Integer): Integer;
    function GetFont: TFont;
    function GetItem(Index: Integer): TdxRibbonGalleryGroupItemViewInfo;
    function GetItemCount: Integer;
    function GetItemSize(AMaxItemWidth, AMaxItemHeight: Integer): TSize;
    function GetOptions: TdxRibbonGalleryGroupOptions;
    function GetPainter: TdxBarPainter;
  protected
    function CalculateItemSize(const APredefinedItemSize: TSize;
      AMaxItemWidth, AMaxItemHeight: Integer): TSize;
    procedure ClearItems;
    procedure CorrectItemWidth(ABounds: TRect);
    procedure CreateGroupItem(AItemIndex: Integer; const ABounds: TRect);
    function GetColumnCount(AWidth: Integer): Integer; virtual;
    function GetColumnCountInRow(ARow: Integer; AGroupWidth: Integer): Integer; virtual;
    function GetColumnLeft(AColumnIndex: Integer; AGroupLeft: Integer): Integer; virtual;
    function GetColumnWidth: Integer;
    function GetGroupWidth: Integer;
    function GetHeaderBounds(AGroupBounds: TRect): TRect;
    function GetItemColumn(AIndex: Integer; AGroupWidth: Integer): Integer;
    function GetItemIndex(ARow, AColumn: Integer; AGroupWidth: Integer): Integer;
    function GetItemRow(AGroupItemIndex: Integer; AGroupWidth: Integer): Integer;
    function GetLastItemInGroupRow(ARowIndex, AColumnCount: Integer): Integer;
    function GetRowCount(AGroupWidth: Integer): Integer;
    function GetRowHeight: Integer;
    function GetRowTop(ARowIndex: Integer; AGroupTop: Integer; AGroupWidth: Integer): Integer; virtual;
    function GetSpaceBetweenItems(AIsFlat: Boolean): Integer;
    procedure RepaintChainOfItems(AnItemIndex: Integer; IsHotTrack: Boolean;
      ACanvas: TcxCanvas; APart: TdxRibbonGalleryGroupRepaintPart = ggrpAll;
      AnItemIndex2: Integer = 0);
    procedure SetBounds(const ABounds: TRect);

    property Font: TFont read GetFont;
    property Options: TdxRibbonGalleryGroupOptions read GetOptions;
    property Painter: TdxBarPainter read GetPainter;
  public
    constructor Create(AOwner: TdxRibbonGalleryControlViewInfo; AGroup: TdxRibbonGalleryGroup);
    destructor Destroy; override;
    procedure Calculate(AGroupTop, AGroupBottom: Integer; const AControlClientRect: TRect);
    function GetHeight(AWidth: Integer): Integer;
    procedure Paint(ACanvas: TcxCanvas);
    property Bounds: TRect read FBounds;
    property Group: TdxRibbonGalleryGroup read FGroup;
    property Header: TdxRibbonGalleryGroupHeaderViewInfo read FHeader;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TdxRibbonGalleryGroupItemViewInfo read GetItem;
    property ItemSize: TSize read FItemSize;
    property Owner: TdxRibbonGalleryControlViewInfo read FOwner;
  end;

  { TdxRibbonGalleryControlViewInfo }

  TdxRibbonGalleryControlViewInfo = class(TdxBarItemControlViewInfo)
  private
    FDontDisplayHotTrackedGroupItem: Integer;
    FDontDisplayGroupHeaderWhenHotTrackingGroupItem: Integer;
    FDownedGroupItem: TdxRibbonGalleryGroupItem;
    FGlobalItemSize: TSize;
    FGroupItemStoredSizes: array of TSize;
    FHotGroupItem: TdxRibbonGalleryGroupItem;
    FLayoutOffset: Integer;
    // colors
    FDisabledTextColor: TColor;
    FHotTextColor: TColor;
    FSelectedTextColor: TColor;
    FTextColor: TColor;

    function GetGroupCount: Integer;
    function GetGroup(Index: Integer): TdxRibbonGalleryGroupViewInfo;
    function GetGroupItemSize(AGroupIndex: Integer): TSize;

    procedure CalculateGlobalItemSize;
    function GetControl: TdxRibbonGalleryControl;
    function GetGalleryBounds: TRect;
    function GetGalleryItem: TdxCustomRibbonGalleryItem;
    function GetGalleryOptions: TdxCustomRibbonGalleryOptions;
    function GetGalleryInMenuOptions: TdxInMenuGalleryOptions;
    function GetGallerySize: TSize;
    function GetPainter: TdxBarPainter;
    function GetScrollBarBounds: TRect;
    function GetScrollBarWidth: Integer;

    procedure DrawGroupItem(const AGroupItem: TdxRibbonGalleryGroupItem);
    procedure RepaintChainOfGroups(ANewItem, AOldItem: TdxRibbonGalleryGroupItem);
  protected
    FGroups: TcxObjectList;
    procedure DisplayGroupItem(AGroupItem: TdxRibbonGalleryGroupItem); virtual;
    procedure DrawBackground(ACanvas: TcxCanvas; const R: TRect); virtual; abstract;
    procedure DrawSelectedGroupItem(ASelectedGroupItem, AOldSelectedGroupItem: TdxRibbonGalleryGroupItem);
    procedure GalleryChanged;
    function GetAbsoluteGroupTop(AGroupIndex: Integer;
      AWidth: Integer): Integer;
    function GetControlBounds: TRect; virtual;
    function GetGalleryHeight(AWidth: Integer): Integer; virtual;
    function GetGalleryMargins: TRect; virtual; abstract;
    function GetGroupItemCount(ALastGroupIndex: Integer): Integer;
    function GetHeight(AWidth: Integer): Integer; virtual;
    function GetLayoutWidth(AColumnCount: Integer; out AGroupItemWidthIsNull: Boolean): Integer; virtual; abstract;
    function GetMaxGroupItemSize: TSize; virtual;
    function GetGroupItem(const P: TPoint): TdxRibbonGalleryGroupItem; overload;
    function GetGroupItem(X, Y: Integer): TdxRibbonGalleryGroupItem; overload;
    function GetGroupItemStoredSize(AGroupIndex: Integer): TSize;
    function GetGroupItemViewInfo(AGroupItem: TdxRibbonGalleryGroupItem): TdxRibbonGalleryGroupItemViewInfo;
    function GetInRibbonGalleryState: Integer; virtual;
    function GetLeftLayoutIndent: Integer; virtual;
    function GetMinSize: TSize; virtual; abstract;
    function GetNextButtonEnabled: Boolean;
    function GetPreviousButtonEnabled: Boolean;
    function GetRibbonSkin: IdxSkin; virtual;
    function GetRightLayoutIndent: Integer; virtual;
    function GetVisibleGroupIndex(AStartGroupIndex: Integer; AIncreaseIndex: Boolean): Integer;
    function GetVisibleNotEmptyGroupIndex(AStartGroupIndex: Integer; AIncreaseIndex: Boolean): Integer;
    function InternalGetScrollBarWidth: Integer; virtual; abstract;
    function IsGroupHeaderVisible: Boolean; virtual;
    function IsGroupItemAtThisPlace(X, Y: Integer): Boolean;
    function IsInRibbon: Boolean; virtual;
    procedure RemoveGroupItem(AItem: TdxRibbonGalleryGroupItem);
    procedure Changed; virtual;
    procedure SetDownedGroupItem(const Value: TdxRibbonGalleryGroupItem);
    procedure SetGroupItemStoredSize(const Value: TSize; AGroupIndex: Integer);
    procedure SetHotGroupItem(Value: TdxRibbonGalleryGroupItem);
    procedure ShowGroupItem(AGroupItem: TdxRibbonGalleryGroupItem); virtual;

    property DisabledTextColor: TColor read FDisabledTextColor;
    property HotTextColor: TColor read FHotTextColor;
    property SelectedTextColor: TColor read FSelectedTextColor;
    property TextColor: TColor read FTextColor;

    property DontDisplayGroupHeaderWhenHotTrackingGroupItem: Integer read FDontDisplayGroupHeaderWhenHotTrackingGroupItem write FDontDisplayGroupHeaderWhenHotTrackingGroupItem;
    property DontDisplayHotTrackedGroupItem: Integer read FDontDisplayHotTrackedGroupItem write FDontDisplayHotTrackedGroupItem;
    property DownedGroupItem: TdxRibbonGalleryGroupItem read FDownedGroupItem;
    property GalleryBounds: TRect read GetGalleryBounds;
    property GalleryInMenuOptions: TdxInMenuGalleryOptions read GetGalleryInMenuOptions;
    property GalleryItem: TdxCustomRibbonGalleryItem read GetGalleryItem;
    property GalleryOptions: TdxCustomRibbonGalleryOptions read GetGalleryOptions;
    property GallerySize: TSize read GetGallerySize;
    property GlobalItemSize: TSize read FGlobalItemSize;
    property HotGroupItem: TdxRibbonGalleryGroupItem read FHotGroupItem write SetHotGroupItem;
    property LayoutOffset: Integer read FLayoutOffset;
    property Painter: TdxBarPainter read GetPainter;
    property ScrollBarWidth: Integer read GetScrollBarWidth;
  public
    constructor Create(AControl: TdxBarItemControl); override;
    destructor Destroy; override;
    procedure Calculate(ALayoutOffset: Integer; AScrollCode: TScrollCode); virtual;
    function IsCollapsed: Boolean; virtual; abstract;
    procedure Paint(ACanvas: TcxCanvas); virtual;
    property Control: TdxRibbonGalleryControl read GetControl;
    property GroupCount: Integer read GetGroupCount;
    property Groups[Index: Integer]: TdxRibbonGalleryGroupViewInfo read GetGroup;
    property ScrollBarBounds: TRect read GetScrollBarBounds;
  end;

  { TdxInRibbonGalleryControlViewInfo }

  TdxInRibbonGalleryControlViewInfo = class(TdxRibbonGalleryControlViewInfo,
    IdxBarMultiColumnItemControlViewInfo)
  private
    FAnimation: TdxImageAnimationTransition;
    FAnimationBitmapHeight: Integer;
    FCollapsed: Boolean;
    FColumnCount: Integer;
    FControlHeight: Integer;
    FDrawWithoutBackground: Boolean;
    FRowCount: Integer;
    FShowGroupItem: TdxRibbonGalleryGroupItem;
    FTopVisibleRow: Integer;
    FWidthForColumnCountInfos: array of TdxBarItemCachedWidthInfo;

    procedure DoCalculate;
    function GetControlHeight: Integer;
    function GetGroupColumnCount: Integer;
    function GetRowIndex(AGroupItemIndex, AColumnCount: Integer): Integer;
    function GetSpaceBetweenItems(AIsFlat: Boolean): Integer;
    function IsScrollingPossible(ARowDelta: Integer): Boolean;
    procedure PopulateGroupItemList(AFirstVisibleRow, ALastVisibleRow, AColumnCount: Integer; AList: TObjectList);

    // IdxBarMultiColumnItemControlViewInfo
    function CanCollapse: Boolean;
    function GetCollapsed: Boolean;
    function GetColumnCount: Integer;
    function GetMaxColumnCount: Integer;
    function GetMinColumnCount: Integer;
    function GetTopIndent: Integer;
    function GetTotalRows: Integer;
    function GetWidthForColumnCount(AColumnCount: Integer): Integer;
    procedure SetCollapsed(Value: Boolean);
    procedure SetColumnCount(Value: Integer);
  protected
    procedure AnimationHandler(Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
    procedure BoundsCalculated; override;
    procedure CalculateLayout(ALayoutOffset, AColumnCount: Integer; AGroupItemsList: TObjectList);
    function CorrectGroupItemWidth(const AGroupItemSize: TSize): Integer;
    procedure DoDrawBackground(ACanvas: TcxCanvas; const R: TRect);
    procedure DoScrolling(ARowDelta: Integer);
    procedure DrawBackground(ACanvas: TcxCanvas; const R: TRect); override;
    function GetControlMargins: TRect; virtual;
    function GetGalleryMargins: TRect; override;
    function GetLayoutWidth(AColumnCount: Integer; out AGroupItemWidthIsNull: Boolean): Integer; override;
    function GetMaxGroupItemSize: TSize; override;
    function GetBottomLayoutIndent: Integer;
    function GetLeftLayoutIndent: Integer; override;
    function GetRightLayoutIndent: Integer; override;
    function GetTopLayoutIndent: Integer;
    function InternalGetScrollBarWidth: Integer; override;
    function IsInRibbon: Boolean; override;
    procedure ShowGroupItem(AGroupItem: TdxRibbonGalleryGroupItem); override;

    property Animation: TdxImageAnimationTransition read FAnimation;
    property ControlHeight: Integer read GetControlHeight;
    property DrawWithoutBackground: Boolean read FDrawWithoutBackground;
    property RowCount: Integer read FRowCount;
    property TotalRows: Integer read GetTotalRows;
  public
    constructor Create(AControl: TdxBarItemControl); override;
    procedure Calculate(ALayoutOffset: Integer; AScrollCode: TScrollCode); override;
    function IsCollapsed: Boolean; override;
    procedure Paint(ACanvas: TcxCanvas); override;
    procedure ResetCachedValues; override;
    property ColumnCount: Integer read FColumnCount;
    property TopVisibleRow: Integer read FTopVisibleRow;
  end;

  { TdxRibbonOnMenuGalleryControlViewInfo }

  TdxRibbonOnMenuGalleryControlViewInfo = class(TdxRibbonGalleryControlViewInfo)
  strict private
    FFilterBandContentRect: TRect;
    FFilterBandHotTrack: Boolean;
    FFilterBandRect: TRect;
    FGroupItemDescriptionRectCache: TObjectList;

    procedure CalculateFilterBand;
    procedure DrawFilterBand;
    procedure DrawFilterCaption;
    function GetBottomSeparatorHeight: Integer;
    function GetFilterBandHeight: Integer;
    function GetHeightByRowCount(AWidth: Integer): Integer;
    function GetSpaceBetweenItems(AGroupIndex: Integer; AIsFlat: Boolean): Integer;
    procedure InitializeGroupItemDescriptionRectCache;
    function NeedsDrawSeparator(const APositionInDropDown: TdxRibbonGalleryItemBarItemLinkPositionInDropDown): Boolean;
  protected
    procedure DisplayGroupItem(AGroupItem: TdxRibbonGalleryGroupItem); override;
    procedure DrawBackground(ACanvas: TcxCanvas; const R: TRect); override;
    function GetControlBounds: TRect; override;
    function GetGalleryHeight(AWidth: Integer): Integer; override;
    function GetGalleryMargins: TRect; override;
    function GetGroupItemDescriptionRect(AGroupIndex, AnItemIndex: Integer): TRect;
    function GetHeight(AWidth: Integer): Integer; override;
    function GetLayoutWidth(AColumnCount: Integer; out AGroupItemWidthIsNull: Boolean): Integer; override;
    procedure GroupItemYRange(const AGroupItem: TdxRibbonGalleryGroupItem; var ATop, ABottom: Integer);
    function GetMinSize: TSize; override;
    function InternalGetScrollBarWidth: Integer; override;
    procedure Changed; override;
    procedure SetGroupItemDescriptionRect(AGroupIndex, AnItemIndex: Integer; ARect: TRect);
  public
    destructor Destroy; override;
    procedure Calculate(ALayoutOffset: Integer; AScrollCode: TScrollCode); override;
    procedure GetFilterMenuShowingParams(out APosition: TPoint;
      out AOwnerHeight: Integer);
    function IsCollapsed: Boolean; override;
    function IsPtInFilterBandHotTrackArea(const P: TPoint): Boolean;
    procedure RepaintFilterBand;
    procedure SetFilterBandHotTrack(AValue: Boolean);
  end;

  { TdxRibbonGalleryControlAccessibilityHelper }

  TdxRibbonGalleryControlAccessibilityHelper = class(TdxBarSubItemControlAccessibilityHelper)
  private
    function GetControl: TdxRibbonGalleryControl;
    function GetOnSubmenuController: TdxRibbonOnSubmenuGalleryController;
  protected
    // IdxBarAccessibilityHelper
    function HandleNavigationKey(var AKey: Word): Boolean; override;
    function IsNavigationKey(AKey: Word): Boolean; override;

    procedure GetKeyTipData(AKeyTipsData: TList); override;
    procedure GetKeyTipInfo(out AKeyTipInfo: TdxBarKeyTipInfo); override;
    procedure OnSubmenuHotTrack(
      ANavigationDirection: TdxRibbonDropDownGalleryNavigationDirection);
    function ShowDropDownWindow: Boolean; override;

    property Control: TdxRibbonGalleryControl read GetControl;
    property OnSubmenuController: TdxRibbonOnSubmenuGalleryController
      read GetOnSubmenuController;
  end;

  { TdxRibbonDropDownGalleryControlAccessibilityHelper }

  TdxRibbonDropDownGalleryControlAccessibilityHelper = class(TdxBarSubMenuControlAccessibilityHelper)
  private
    function GetBarControl: TdxRibbonDropDownGalleryControl;
    function GetInternalGalleryItemControlAccessibilityHelper: TdxRibbonGalleryControlAccessibilityHelper;
  protected
    // IdxBarAccessibilityHelper
    function HandleNavigationKey(var AKey: Word): Boolean; override;
    function IsNavigationKey(AKey: Word): Boolean; override;

    procedure HandleVertNavigationKey(AUpKey, AFocusItemControl: Boolean); override;
    property BarControl: TdxRibbonDropDownGalleryControl read GetBarControl;
    property InternalGalleryItemControlAccessibilityHelper: TdxRibbonGalleryControlAccessibilityHelper
      read GetInternalGalleryItemControlAccessibilityHelper;
  end;

  { TdxRibbonGalleryScrollBarViewInfo }

  TdxRibbonGalleryScrollBarViewInfo = class(TcxScrollBarViewInfo)
  private
    FDropDownButtonRect: TRect;
  public
    procedure Calculate; override;
    procedure CalculateDropDownStyle; virtual;
    //
    property DropDownButtonRect: TRect read FDropDownButtonRect;
  end;

  { TdxRibbonGalleryScrollBarPainter }

  TdxRibbonGalleryScrollBarPainter = class(TcxScrollBarPainter)
  private
    function GetGalleryControl: TdxRibbonGalleryControl;
    function GetScrollBar: TdxRibbonGalleryScrollBarHelper;
    function GetViewInfo: TdxRibbonGalleryScrollBarViewInfo;
  protected
    procedure DrawScrollBarBackground(ACanvas: TcxCanvas; const R: TRect); override;
    procedure DoDrawScrollBarPart(ACanvas: TcxCanvas; const R: TRect;
      APart: TcxScrollBarPart; AState: TcxButtonState); override;
    property ScrollBar: TdxRibbonGalleryScrollBarHelper read GetScrollBar;
    property ViewInfo: TdxRibbonGalleryScrollBarViewInfo read GetViewInfo;
  public
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  { TdxRibbonGalleryScrollBarHelper }

  TdxRibbonGalleryScrollBarHelper = class(TcxScrollBarHelper)
  private
    FIsDropDownButtonPressed: Boolean;
    FRibbonScrollBar: TdxRibbonGalleryScrollBar;

    function GetGalleryControl: TdxRibbonGalleryControl;
  protected
    function GetPainterClass: TcxScrollBarPainterClass; override;
    function GetViewInfoClass: TcxScrollBarViewInfoClass; override;

    function GetBarPainter: TdxBarPainter;
    function GetButtonSkinState(AState: TcxButtonState): Integer;
    function GetRibbonSkin: IdxSkin;
    function IsButtonEnabled(AButtonKind: TdxInRibbonGalleryScrollBarButtonKind): Boolean;
    function IsDropDownButtonUnderMouse: Boolean;
    function IsDropDownStyle: Boolean;
    function IsTouchMode: Boolean;

    property GalleryControl: TdxRibbonGalleryControl read GetGalleryControl;
    property IsDropDownButtonPressed: Boolean read FIsDropDownButtonPressed write FIsDropDownButtonPressed;
  public
    constructor Create(AOwner: IcxScrollBarOwner); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

  { TdxRibbonGalleryScrollBar }

  TdxRibbonGalleryScrollBar = class(TcxScrollBar)
  private
    FGalleryControl: TdxRibbonGalleryControl;

    FOnDropDown: TNotifyEvent;

    procedure DoDropDown;
    function GetHelper: TdxRibbonGalleryScrollBarHelper;
    function GetIsDropDownButtonPressed: Boolean;
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
    procedure WMNCDestroy(var Message: TWMNCDestroy); message WM_NCDESTROY;
  protected
    function GetHelperClass: TcxScrollBarHelperClass; override;
    property GalleryControl: TdxRibbonGalleryControl read FGalleryControl;
    property Helper: TdxRibbonGalleryScrollBarHelper read GetHelper;
  public
    constructor Create(AGalleryControl: TdxRibbonGalleryControl); reintroduce;
    function IsDropDownStyle: Boolean;
    property IsDropDownButtonPressed: Boolean read GetIsDropDownButtonPressed;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
  end;

  { TdxRibbonDropDownGallery }

  TdxRibbonDropDownGallery = class(TdxRibbonCustomPopupMenu)
  strict private
    FGalleryItem: TdxCustomRibbonGalleryItem;

    procedure SetGalleryItem(Value: TdxCustomRibbonGalleryItem);
  protected
    function CreateBarControl: TCustomdxBarControl; override;
    function GetControlClass: TCustomdxBarControlClass; override;
    function GetItemLinksClass: TdxBarItemLinksClass; override;
    function HasValidGalleryItem: Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property BarManager;
    property Font;
    property GalleryItem: TdxCustomRibbonGalleryItem read FGalleryItem write SetGalleryItem;
    property ItemLinks;
    property ItemOptions;
    property PopupAlignment;
    property Ribbon;
    property UseOwnFont;
    property OnCloseUp;
    property OnPaintBar;
    property OnPopup;
  end;

  { TdxRibbonDropDownGalleryGalleryItemBarItemLinks }

  TdxRibbonDropDownGalleryGalleryItemBarItemLinks = class(TdxBarSubMenuControlItemLinks)
  public
    function CanContainItem(AItem: TdxBarItem; out AErrorText: string): Boolean; override;
  end;

  { TdxRibbonDropDownGalleryControlPainter }

  TdxRibbonDropDownGalleryControlPainter = class(TObject)
  private
    FPainter: TdxBarPainter;
  protected
    function HasSizingBand(AGalleryControl: TdxRibbonDropDownGalleryControl): Boolean;
  public
    constructor Create(APainter: TdxBarPainter); virtual;

    function GetSizingBandHeight(AGalleryControl: TdxRibbonDropDownGalleryControl): Integer; virtual;
    function PtInSizingArea(AGalleryControl: TdxRibbonDropDownGalleryControl; const P: TPoint): Boolean; virtual;
    procedure SubMenuControlDrawBorder(ABarSubMenuControl: TdxBarSubMenuControl; DC: HDC; R: TRect);

    property Painter: TdxBarPainter read FPainter;
  end;

  { TdxRibbonDropDownGalleryControl }

  TdxDropDownGalleryResizingState = (grsNone, grsTop, grsTopRight, grsBottom, grsBottomRight);

  TdxRibbonDropDownGalleryControl = class(TdxRibbonPopupMenuControl, IdxGestureClient, IdxGestureOwner)
  private
    FGalleryItem: TdxCustomRibbonGalleryItem;
    FGalleryItemBarItemLinks: TdxRibbonDropDownGalleryGalleryItemBarItemLinks;
    FGestureHelper: TdxGestureHelper;
    FHeight: Integer;
    FHitTest: Integer;
    FInternalPainter: TdxRibbonDropDownGalleryControlPainter;
    FIsResizingAssigned: Boolean;
    FMouseResizingDelta: TPoint;
    FMouseWheelStep: Integer;
    FResizingState: TdxDropDownGalleryResizingState;
    FResizing: TdxRibbonGalleryDropDownResizing;
    FUseInternalSizeValue: Boolean;

    procedure DoResizing;
    function GetInternalGalleryItemControl: TdxRibbonGalleryControl;
    function GetInternalGalleryItemLink: TdxBarItemLink;
    function GetInternalPainter: TdxRibbonDropDownGalleryControlPainter;
    function GetMouseWheelStep: Integer;
    function GetResizing: TdxRibbonGalleryDropDownResizing;
    function HitTestToResizingState: TdxDropDownGalleryResizingState;
    function IsHitTestResizing: Boolean;
    procedure SetResizing(Value: TdxRibbonGalleryDropDownResizing);
    procedure StartResizing;
    procedure StopResizing;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SetCursor;
  protected
    // IdxGestureClient
    function AllowGesture(AGestureId: Integer): Boolean;
    function AllowPan(AScrollKind: TScrollBarKind): Boolean;
    procedure BeginGestureScroll(APos: TPoint);
    procedure EndGestureScroll;
    procedure GestureScroll(ADeltaX, ADeltaY: Integer);
    function GetPanOptions: Integer;
    function IsPanArea(const APoint: TPoint): Boolean;
    function NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean;
    // IdxGestureOwner
    function GetGestureClient(const APoint: TPoint): IdxGestureClient;
    function IdxGestureOwner.GetHandle = GetGestureClientHandle;
    function GetGestureClientHandle: THandle;
    function IsGestureTarget(AWnd: THandle): Boolean;
    // touch
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); override;
    procedure DoGetGestureOptions(var Gestures: TInteractiveGestures;
      var Options: TInteractiveGestureOptions); override;
    function GetDefaultInteractiveGestures: TInteractiveGestures; virtual;
    function GetDefaultInteractiveGestureOptions: TInteractiveGestureOptions; virtual;
    function IsTouchPropertyStored(AProperty: TTouchProperty): Boolean; override;
    function IsDefaultGesture(AGestureID: Integer): Boolean; virtual;
    function GetDefaultPanOptions: Integer; virtual;

    procedure CalcColumnItemRects(ATopIndex: Integer; out ALastItemBottom: Integer); override;
    function ChangeSizeByChildItemControl(out ASize: TSize): Boolean; override;
    procedure CreateWnd; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DoNCPaint(DC: HDC; const ARect: TRect); override;
    function DoFindLinkWithAccel(AKey: Word; AShift: TShiftState; ACurrentLink: TdxBarItemLink): TdxBarItemLink; override;
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; override;
    function GetClientOffset(
      AIncludeDetachCaption: Boolean = True): TRect; override;
    function GetItemsPaneSize: TSize; override;
    function GetMinSize: TSize; virtual;
    function GetViewInfoClass: TCustomdxBarControlViewInfoClass; override;
    function IsControlExists(ABarItemControl: TdxBarItemControl): Boolean; override;
    function IsDoubleBufferedNeeded: Boolean; override;
    function IsResizing: Boolean; override;
    function IsSizingBandAtBottom: Boolean;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function MustFitInWorkAreaWidth: Boolean; override;
    function NeedsMouseWheel: Boolean; override;
    function NeedsSelectFirstItemOnDropDownByKey: Boolean; override;
    procedure Resize; override;
    procedure UpdateItem(AControl: TdxBarItemControl); override;
    function GetBorderSize: Integer; override;
    function GetItemsRectOffset: TRect; override;
    procedure WndProc(var Message: TMessage); override;

    property GalleryItem: TdxCustomRibbonGalleryItem read FGalleryItem write FGalleryItem;
    property InternalGalleryItemControl: TdxRibbonGalleryControl read GetInternalGalleryItemControl;
    property InternalGalleryItemLink: TdxBarItemLink read GetInternalGalleryItemLink;
    property InternalPainter: TdxRibbonDropDownGalleryControlPainter
      read GetInternalPainter;
    property Resizing: TdxRibbonGalleryDropDownResizing read GetResizing
      write SetResizing;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  { TdxRibbonDropDownGalleryControlViewInfo }

  TdxRibbonDropDownGalleryControlViewInfo = class(TdxBarSubMenuControlViewInfo)
  private
    function GetBarControl: TdxRibbonDropDownGalleryControl;
  public
    procedure Calculate; override;
    property BarControl: TdxRibbonDropDownGalleryControl read GetBarControl;
  end;

  { TdxRibbonGalleryGroupItemActionLink }

  TdxRibbonGalleryGroupItemActionLink = class(TActionLink)
  protected
    FClient: TdxRibbonGalleryGroupItem;
    procedure AssignClient(AClient: TObject); override;
    function IsCaptionLinked: Boolean; override;
    function IsCheckedLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;
    procedure SetCaption(const Value: string); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHint(const Value: string); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
  end;

implementation

uses
  Types, Math, Variants, cxContainer, dxBarSkinConsts, dxBarStrs, dxOffice11, cxLookAndFeels;

const
  DropDownInflateX = 3;
  DropDownInflateY = 3;
  DropDownOffsetX = 1;
  DropDownOffsetY = 1;

  FilterArrowOffset = 1;
  FilterArrowSize = 4;
  FilterBandOffset = 1;
  FilterMenuLeftBoundCorrection = 1;
  GroupHeaderCaptionOffset = 11;

  FilterCaptionDelimiter = ',';

  InvalidGalleryParentKind = 'Invalid TdxRibbonGalleryControl parent kind';

type
  TRelativeLineLocation = (rllBefore, rllInside, rllAfter);

  TCustomdxBarControlAccess = class(TCustomdxBarControl);
  TdxBarItemControlAccess = class(TdxBarItemControl);
  TdxBarItemLinkAccess = class(TdxBarItemLink);
  TdxBarItemLinksAccess = class(TdxBarItemLinks);
  TdxBarManagerAccess = class(TdxBarManager);
  TdxBarPainterAccess = class(TdxBarPainter);
  TdxBarSkinnedPainterAccess = class(TdxBarSkinnedPainter);
  TCustomdxBarSubItemAccess = class(TCustomdxBarSubItem);

var
  FDontShowFilterMenuOnMouseDown: Boolean;

function cxDrawTextOpaque(ACanvas: TcxCanvas; const AText: string; const ARect: TRect;
  AFormat: UINT; ATextColor: TColor = clDefault): Integer;
const
  White = $FFFFFF;
var
  ABitmap: TcxBitmap32;
  ABackGroundColor: TColor;
begin
  if ATextColor = clDefault then
    ATextColor := ACanvas.Font.Color;

  if ColorToRGB(ATextColor) = White then
    ABackGroundColor := 0
  else
    ABackGroundColor := White;

  ABitmap := TcxBitmap32.CreateSize(ARect);
  try
    ABitmap.cxCanvas.Font.Assign(ACanvas.Font);
    dxSetFontAsNonAntialiased(ABitmap.cxCanvas.Font);
    ABitmap.cxCanvas.FillRect(ABitmap.ClientRect, ABackGroundColor);
    Result := cxDrawText(ABitmap.cxCanvas, AText, ABitmap.ClientRect, AFormat, ATextColor);
    ABitmap.RecoverTransparency(ABackGroundColor);
    cxAlphaBlend(ACanvas.Handle, ABitmap, ARect, ABitmap.ClientRect);
  finally
    ABitmap.Free;
  end;
end;

function AreLinesIntersected(ABegin1, AEnd1, ABegin2, AEnd2: Integer): Boolean;

  function IsPointOnLine(ABegin, AEnd, APoint: Integer): Boolean;
  begin
    Result := (ABegin <= APoint) and (APoint <= AEnd);
  end;

begin
  Result := (IsPointOnLine(ABegin1, AEnd1, ABegin2)) or
            (IsPointOnLine(ABegin1, AEnd1, AEnd2)) or
            (IsPointOnLine(ABegin2, AEnd2, ABegin1));
end;

function AreLinesIntersectedStrictly(ABegin1, AEnd1, ABegin2, AEnd2: Integer): Boolean;

  function IsPointOnLine(ABegin, AEnd, APoint: Integer): Boolean;
  begin
    Result := (ABegin < APoint) and (APoint < AEnd)
  end;

begin
  Result := (IsPointOnLine(ABegin1, AEnd1, ABegin2)) or
            (IsPointOnLine(ABegin1, AEnd1, AEnd2)) or
            (IsPointOnLine(ABegin2, AEnd2, ABegin1)) or
            (IsPointOnLine(ABegin2, AEnd2, AEnd1)) or
            (ABegin1 = ABegin2) and (AEnd1 = AEnd2) or
            (ABegin1 = AEnd2) and (AEnd1 = ABegin2);
end;

function GetGroupViewInfo(AGalleryGroups: TdxRibbonGalleryGroups;
  AGalleryControlViewInfo: TdxRibbonGalleryControlViewInfo;
  AGroupIndex: Integer;
  out DestroyAfterUse: Boolean): TdxRibbonGalleryGroupViewInfo;
var
  I: Integer;
begin
  Result := nil;
  DestroyAfterUse := False;
  if (AGalleryControlViewInfo.GalleryItem.IsGroupVisible(AGroupIndex)) and
    (0 <= AGroupIndex) and (AGroupIndex < AGalleryGroups.Count) then
  begin
    for I := 0 to AGalleryControlViewInfo.GroupCount - 1 do
      if AGalleryControlViewInfo.Groups[I].Group.Index = AGroupIndex then
      begin
        Result := AGalleryControlViewInfo.Groups[I];
        Break;
      end;
    if Result = nil then
    begin
      Result := TdxRibbonGalleryGroupViewInfo.Create(AGalleryControlViewInfo,
        AGalleryGroups[AGroupIndex]);
      DestroyAfterUse := True;
    end;
  end;
end;

function GetOuterGroupItem(AItem1,
  AItem2: TdxRibbonGalleryGroupItem;
  AMode: TdxRibbonGalleryItemPullHighlightingMode): TdxRibbonGalleryGroupItem;
begin
  Result := nil;
  if AItem1 <> nil then
  begin
    if AItem2 <> nil then
    begin
      if AItem1.Group.Index > AItem2.Group.Index then
        Result := AItem1
      else
        if AItem1.Group.Index < AItem2.Group.Index then
          Result := AItem2
        else
        begin
          if AItem1.Index > AItem2.Index then
            Result := AItem1
          else
            Result := AItem2;
        end;
    end
    else
      Result := AItem1;
  end
  else
    if AItem2 <> nil then
      Result := AItem2;
  if (AMode = iphmFinishToStart) and (AItem1 <> nil) and (AItem2 <> nil) then
  begin
    if Result = AItem1 then
      Result := AItem2
    else
      Result := AItem1;
  end;
end;

function GetItemPullHighlightingIdentifier(AGroupItem: TdxRibbonGalleryGroupItem): Integer;
var
  AGroup: TdxRibbonGalleryGroup;
begin
  AGroup := AGroupItem.Group;
  if AGroup.Options.ItemPullHighlightingMode = AGroup.GalleryItem.GalleryInMenuOptions.ItemPullHighlightingMode then
    Result := -1
  else
    Result := AGroup.Index;
end;

function IsFirstLineShorterOrEqualThanSecond(ABegin1, AEnd1, ABegin2,
  AEnd2: Integer): Boolean;
begin
  Result := AEnd1 - ABegin1 <= AEnd2 - ABegin2;
end;

function RelativeLocationOfLines(ShortLineBegin, ShortLineEnd, LongLineBegin, LongLineEnd: Integer): TRelativeLineLocation;
begin
  if (ShortLineBegin < LongLineBegin) then
    Result := rllBefore
  else
    if (LongLineEnd < ShortLineEnd) then
      Result := rllAfter
    else
      Result := rllInside;
end;

function CanUseSize(const ASize: TSize): Boolean;
begin
  Result := (ASize.cx > 0) and (ASize.cy > 0);
end;

{ TcxItemSize }

procedure TcxItemSize.ChangeScale(M, D: Integer);
begin
  if Assigned then
    inherited;
end;

procedure TcxItemSize.DoChange;
begin
  Assigned := True;
  inherited DoChange;
end;

function TcxItemSize.GetValue(Index: Integer): Integer;
begin
  if (Parent = nil) or Assigned then
    Result := inherited GetValue(Index)
  else
    Result := Parent.GetValue(Index);
end;

function TcxItemSize.IsSizeStored(Index: Integer): Boolean;
begin
  Result := ((Parent = nil) or Assigned) and inherited IsSizeStored(Index);
end;

procedure TcxItemSize.SetAssigned(AValue: Boolean);
begin
  FAssigned := AValue;
  if not AValue then
    FSize := cxNullSize;
end;

procedure TcxItemSize.SetSize(const Value: TSize);
begin
  Assigned := True;
  inherited SetSize(Value);
end;

{ TdxRibbonGalleryItemPullHighlighting }

procedure TdxRibbonGalleryItemPullHighlighting.Assign(Source: TPersistent);
begin
  if Source is TdxRibbonGalleryItemPullHighlighting then
  begin
    Active := TdxRibbonGalleryItemPullHighlighting(Source).Active;
    Direction := TdxRibbonGalleryItemPullHighlighting(Source).Direction;
  end
  else
    inherited Assign(Source);
end;

procedure TdxRibbonGalleryItemPullHighlighting.DoChange;
begin
  IsAssigned := True;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TdxRibbonGalleryItemPullHighlighting.IsActiveStored: Boolean;
begin
  Result := ((Parent = nil) or IsAssigned) and FActive;
end;

function TdxRibbonGalleryItemPullHighlighting.IsDirectionStored: Boolean;
begin
  Result := ((Parent = nil) or IsAssigned) and (FDirection <> gphdStartToFinish);
end;

function TdxRibbonGalleryItemPullHighlighting.GetActive: Boolean;
begin
  if (Parent = nil) or IsAssigned then
    Result := FActive
  else
    Result := Parent.GetActive;
end;

function TdxRibbonGalleryItemPullHighlighting.GetDirection: TdxRibbonGalleryItemPullHighlightingDirection;
begin
  if (Parent = nil) or IsAssigned then
    Result := FDirection
  else
    Result := Parent.GetDirection;
end;

procedure TdxRibbonGalleryItemPullHighlighting.SetActive(Value: Boolean);
begin
  FActive := Value;
  DoChange;
end;

procedure TdxRibbonGalleryItemPullHighlighting.SetIsAssigned(Value: Boolean);
begin
  FIsAssigned := Value;
  if not Value then
  begin
    if Parent = nil then
    begin
      FActive := False;
      FDirection := gphdStartToFinish;
    end
    else
    begin
      FActive := Parent.Active;
      FDirection := Parent.Direction;
    end;
  end;
end;

procedure TdxRibbonGalleryItemPullHighlighting.SetDirection(
  Value: TdxRibbonGalleryItemPullHighlightingDirection);
begin
  FDirection := Value;
  DoChange;
end;

{ TdxRibbonGalleryBaseOptions }

constructor TdxRibbonGalleryBaseOptions.Create(AOwner: TdxCustomRibbonGalleryItem);
begin
  inherited Create;
  FGalleryItem := AOwner;
  FFreeNotificator := TcxFreeNotificator.Create(nil);
  FFreeNotificator.OnFreeNotification := FreeNotificationHandler;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageLinkChangeHandler;
  FItemImageSize := TcxItemSize.Create(Self);
  FItemImageSize.OnChange := ItemImageSizeChange;
{  FItemSize := TcxItemSize.Create(Self);
  FItemSize.OnChange := ItemSizeChange;}
  FItemPullHighlighting := TdxRibbonGalleryItemPullHighlighting.Create;
  FItemPullHighlighting.OnChange := ItemPullHighlightingChange;
end;

destructor TdxRibbonGalleryBaseOptions.Destroy;
begin
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FItemImageSize);
  //FreeAndNil(FItemSize);
  FreeAndNil(FItemPullHighlighting);
  FreeAndNil(FFreeNotificator);
  inherited Destroy;
end;

procedure TdxRibbonGalleryBaseOptions.Assign(Source: TPersistent);
begin
  if Source is TdxRibbonGalleryBaseOptions then
  begin
    Images := TdxRibbonGalleryBaseOptions(Source).Images;
    ItemImagePosition := TdxRibbonGalleryBaseOptions(Source).ItemImagePosition;
    ItemImageSize := TdxRibbonGalleryBaseOptions(Source).ItemImageSize;
    //ItemSize := TdxRibbonGalleryBaseOptions(Source).ItemSize;
    SpaceAfterGroupHeader := TdxRibbonGalleryBaseOptions(Source).SpaceAfterGroupHeader;
    SpaceBetweenItemsHorizontally := TdxRibbonGalleryBaseOptions(Source).SpaceBetweenItemsHorizontally;
    SpaceBetweenItemsVertically := TdxRibbonGalleryBaseOptions(Source).SpaceBetweenItemsVertically;
    SpaceBetweenItemCaptionAndDescription := TdxRibbonGalleryBaseOptions(Source).SpaceBetweenItemCaptionAndDescription;
    SpaceBetweenItemImageAndText := TdxRibbonGalleryBaseOptions(Source).SpaceBetweenItemImageAndText;
  end
  else
    inherited Assign(Source);
end;

function TdxRibbonGalleryBaseOptions.ObtainItemPullHighlightingMode: TdxRibbonGalleryItemPullHighlightingMode;
begin
  if not ItemPullHighlighting.Active then
    Result := iphmNone
  else
    if ItemPullHighlighting.Direction = gphdStartToFinish then
      Result := iphmStartToFinish
    else
      Result := iphmFinishToStart;
end;

procedure TdxRibbonGalleryBaseOptions.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TdxRibbonGalleryBaseOptions.ChangeScale(M, D: Integer);
begin
  ItemSize.ChangeScale(M, D);
  ItemImageSize.ChangeScale(M, D);
  SpaceAfterGroupHeader := MulDiv(SpaceAfterGroupHeader, M, D);
  SpaceBetweenItemCaptionAndDescription := MulDiv(SpaceBetweenItemCaptionAndDescription, M, D);
  SpaceBetweenItemImageAndText := MulDiv(SpaceBetweenItemImageAndText, M, D);
  SpaceBetweenItems := MulDiv(SpaceBetweenItems, M, D);
  SpaceBetweenItemsHorizontally := MulDiv(SpaceBetweenItemsHorizontally, M, D);
  SpaceBetweenItemsVertically := MulDiv(SpaceBetweenItemsVertically, M, D);
end;

procedure TdxRibbonGalleryBaseOptions.CheckIntRange(var Value: Integer);
begin
  Value := Max(0, Value);
end;

procedure TdxRibbonGalleryBaseOptions.CheckItemsSpaceRange(var Value: Integer);
begin
  Value := Max(-1, Value);
end;

procedure TdxRibbonGalleryBaseOptions.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('SpaceBetweenItems', ReadSpaceBetweenItemsProperty, WriteSpaceBetweenItemsProperty, False); // AS17522
end;

procedure TdxRibbonGalleryBaseOptions.FreeNotificationHandler(ASender: TComponent);
begin
  if not FGalleryItem.IsDestroying and (ASender = Images) then
    Images := nil;
end;

function TdxRibbonGalleryBaseOptions.GetSpaceAfterGroupHeader: Integer;
begin
  Result := FSpaceAfterGroupHeader;
end;

function TdxRibbonGalleryBaseOptions.GetSpaceBetweenItemCaptionAndDescription: Integer;
begin
  Result := FSpaceBetweenItemCaptionAndDescription;
end;

function TdxRibbonGalleryBaseOptions.GetSpaceBetweenItemImageAndText: Integer;
begin
  Result := FSpaceBetweenItemImageAndText;
end;

function TdxRibbonGalleryBaseOptions.GetSpaceBetweenItems: Integer;
begin
  Result := SpaceBetweenItemsHorizontally;
end;

function TdxRibbonGalleryBaseOptions.GetSpaceBetweenItemsHorizontally: Integer;
begin
  Result := FSpaceBetweenItemsHorizontally;
end;

function TdxRibbonGalleryBaseOptions.GetSpaceBetweenItemsVertically: Integer;
begin
  Result := FSpaceBetweenItemsVertically;
end;

function TdxRibbonGalleryBaseOptions.GetItemImagePosition: TdxRibbonGalleryImagePosition;
begin
  Result := FItemImagePosition;
end;

function TdxRibbonGalleryBaseOptions.GetItemSize: TcxItemSize;
begin
  Result := InMenuOptions.ItemSize;
end;

function TdxRibbonGalleryBaseOptions.GetItemTextAlignVert: TcxAlignmentVert;
begin
  Result := FGalleryItem.GalleryInMenuOptions.ItemTextAlignVert;
end;

function TdxRibbonGalleryBaseOptions.GetItemTextKind: TdxRibbonGalleryGroupItemTextKind;
begin
  Result := FGalleryItem.GalleryInMenuOptions.ItemTextKind;
end;

function TdxRibbonGalleryBaseOptions.GetRemoveHorizontalItemPadding: Boolean;
begin
  Result := SpaceBetweenItemsHorizontally = -1;
end;

function TdxRibbonGalleryBaseOptions.GetRemoveVerticalItemPadding: Boolean;
begin
  Result := SpaceBetweenItemsVertically = -1;
end;

procedure TdxRibbonGalleryBaseOptions.ImageLinkChangeHandler(Sender: TObject);
begin
  DoImageListContentChanged;
end;

function TdxRibbonGalleryBaseOptions.IsItemImagePositionStored: Boolean;
begin
  Result := FItemImagePosition <> gipLeft;
end;

function TdxRibbonGalleryBaseOptions.IsItemImageSizeStored: Boolean;
begin
  Result := (FItemImageSize.Width <> 0) or (FItemImageSize.Height <> 0);
end;

function TdxRibbonGalleryBaseOptions.IsSpaceAfterGroupHeaderStored: Boolean;
begin
  Result := FSpaceAfterGroupHeader <> 0;
end;

function TdxRibbonGalleryBaseOptions.IsSpaceBetweenItemCaptionAndDescriptionStored: Boolean;
begin
  Result := FSpaceBetweenItemCaptionAndDescription <> 0;
end;

function TdxRibbonGalleryBaseOptions.IsSpaceBetweenItemImageAndTextStored: Boolean;
begin
  Result := FSpaceBetweenItemImageAndText <> 0;
end;

function TdxRibbonGalleryBaseOptions.IsSpaceBetweenItemsHorizontallyStored: Boolean;
begin
  Result := FSpaceBetweenItemsHorizontally <> 0;
end;

function TdxRibbonGalleryBaseOptions.IsSpaceBetweenItemsVerticallyStored: Boolean;
begin
  Result := FSpaceBetweenItemsVertically <> 0;
end;

procedure TdxRibbonGalleryBaseOptions.ItemImageSizeChange(Sender: TObject);
begin
  Changed;
end;

procedure TdxRibbonGalleryBaseOptions.ItemPullHighlightingChange(Sender: TObject);
begin
  InMenuOptions.ItemPullHighlightingMode := ObtainItemPullHighlightingMode;
end;

procedure TdxRibbonGalleryBaseOptions.SetImages(Value: TCustomImageList);
begin
  cxSetImageList(Value, FImages, FImageChangeLink, FFreeNotificator);
end;

procedure TdxRibbonGalleryBaseOptions.SetItemImagePosition(
  Value: TdxRibbonGalleryImagePosition);
begin
  if FItemImagePosition <> Value then
  begin
    FItemImagePosition := Value;
    Changed;
  end;
end;

procedure TdxRibbonGalleryBaseOptions.SetItemSize(Value: TcxItemSize);
begin
  InMenuOptions.ItemSize := Value;
end;

procedure TdxRibbonGalleryBaseOptions.SetItemTextAlignVert(Value: TcxAlignmentVert);
begin
  FGalleryItem.GalleryInMenuOptions.ItemTextAlignVert := Value;
end;

procedure TdxRibbonGalleryBaseOptions.SetItemTextKind(Value: TdxRibbonGalleryGroupItemTextKind);
begin
  FGalleryItem.GalleryInMenuOptions.ItemTextKind := Value;
end;

procedure TdxRibbonGalleryBaseOptions.SetSpaceAfterGroupHeader(
  Value: Integer);
begin
  CheckIntRange(Value);
  if FSpaceAfterGroupHeader <> Value then
  begin
    FSpaceAfterGroupHeader := Value;
    Changed;
  end;
end;

procedure TdxRibbonGalleryBaseOptions.SetSpaceBetweenItemCaptionAndDescription(
  Value: Integer);
begin
  CheckIntRange(Value);
  if FSpaceBetweenItemCaptionAndDescription <> Value then
  begin
    FSpaceBetweenItemCaptionAndDescription := Value;
    Changed;
  end;
end;

procedure TdxRibbonGalleryBaseOptions.SetSpaceBetweenItemImageAndText(
  Value: Integer);
begin
  CheckIntRange(Value);
  if FSpaceBetweenItemImageAndText <> Value then
  begin
    FSpaceBetweenItemImageAndText := Value;
    Changed;
  end;
end;

procedure TdxRibbonGalleryBaseOptions.SetSpaceBetweenItems(
  Value: Integer);
begin
  CheckItemsSpaceRange(Value);
  if (SpaceBetweenItemsHorizontally <> Value) or
    (SpaceBetweenItemsVertically <> Value) then
  begin
    SpaceBetweenItemsHorizontally := Value;
    SpaceBetweenItemsVertically := Value;
    Changed;
  end;
end;

procedure TdxRibbonGalleryBaseOptions.SetSpaceBetweenItemsHorizontally(
  Value: Integer);
begin
  CheckItemsSpaceRange(Value);
  if FSpaceBetweenItemsHorizontally <> Value then
  begin
    FSpaceBetweenItemsHorizontally := Value;
    Changed;
  end;
end;

procedure TdxRibbonGalleryBaseOptions.SetSpaceBetweenItemsVertically(
  Value: Integer);
begin
  CheckItemsSpaceRange(Value);
  if FSpaceBetweenItemsVertically <> Value then
  begin
    FSpaceBetweenItemsVertically := Value;
    Changed;
  end;
end;

function TdxRibbonGalleryBaseOptions.GetInRibbonOptions: TdxInRibbonGalleryOptions;
begin
  Result := FGalleryItem.GalleryInRibbonOptions;
end;

function TdxRibbonGalleryBaseOptions.GetInMenuOptions: TdxInMenuGalleryOptions;
begin
  Result := FGalleryItem.GalleryInMenuOptions;
end;

procedure TdxRibbonGalleryBaseOptions.ReadSpaceBetweenItemsProperty(
  Reader: TReader);
var
  ASpaceBetweenItems: Integer;
begin
  ASpaceBetweenItems := Reader.ReadInteger;
  SpaceBetweenItemsHorizontally := ASpaceBetweenItems;
  SpaceBetweenItemsVertically := ASpaceBetweenItems;
end;

procedure TdxRibbonGalleryBaseOptions.SetItemImageSize(Value: TcxItemSize);
begin
  FItemImageSize.Assign(Value);
end;

procedure TdxRibbonGalleryBaseOptions.SetItemPullHighlighting(
  Value: TdxRibbonGalleryItemPullHighlighting);
begin
  FItemPullHighlighting.Assign(Value);
  ItemPullHighlightingChange(Self);
end;

procedure TdxRibbonGalleryBaseOptions.WriteSpaceBetweenItemsProperty(
  Writer: TWriter);
begin
// do nothing
end;

{ TdxRibbonGalleryGroupOptions }

constructor TdxRibbonGalleryGroupOptions.Create(
  AParentOptions: TdxRibbonGalleryBaseOptions; AGroup: TdxRibbonGalleryGroup);
begin
  inherited Create(AGroup.GalleryItem);
  FGroup := AGroup;
  FParentOptions := AParentOptions;
  FAssignedValues := [];
  FItemTextAlignVert := vaTop;
  FItemTextKind := itkCaption;
  FItemSize := TcxItemSize.Create(Self);
  FItemSize.OnChange := ItemSizeChange;
  FItemSize.Parent := InMenuOptions.ItemSize;
  ItemImageSize.Parent := AParentOptions.ItemImageSize;
end;

destructor TdxRibbonGalleryGroupOptions.Destroy;
begin
  FreeAndNil(FItemSize);
  inherited Destroy;
end;

procedure TdxRibbonGalleryGroupOptions.Assign(Source: TPersistent);
begin
  if Source is TdxRibbonGalleryGroupOptions then
  begin
    ItemPullHighlightingMode := TdxRibbonGalleryGroupOptions(Source).ItemPullHighlightingMode;
    ItemSize := TdxRibbonGalleryGroupOptions(Source).ItemSize;
    ItemTextAlignVert := TdxRibbonGalleryGroupOptions(Source).ItemTextAlignVert;
    ItemTextKind := TdxRibbonGalleryGroupOptions(Source).ItemTextKind;
  end
  else
    inherited Assign(Source);
end;

procedure TdxRibbonGalleryGroupOptions.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  FItemSize.ChangeScale(M, D);
end;

procedure TdxRibbonGalleryGroupOptions.DoImageListContentChanged;
begin
  Group.Changed(True);
  Group.GalleryItem.Recalculate;
end;

function TdxRibbonGalleryGroupOptions.GetItemImagePosition: TdxRibbonGalleryImagePosition;
begin
  if IsItemImagePositionStored then
    Result := inherited GetItemImagePosition
  else
    Result := ParentOptions.GetItemImagePosition;
end;

function TdxRibbonGalleryGroupOptions.GetItemSize: TcxItemSize;
begin
  Result := FItemSize;
end;

function TdxRibbonGalleryGroupOptions.GetItemTextAlignVert: TcxAlignmentVert;
begin
  if IsItemTextAlignVertStored then
    Result := FItemTextAlignVert
  else
    Result := InMenuOptions.ItemTextAlignVert;
end;

function TdxRibbonGalleryGroupOptions.GetItemTextKind: TdxRibbonGalleryGroupItemTextKind;
begin
  if IsItemTextKindStored then
    Result := FItemTextKind
  else
    Result := InMenuOptions.ItemTextKind;
end;

function TdxRibbonGalleryGroupOptions.GetSpaceAfterGroupHeader: Integer;
begin
  if IsSpaceAfterGroupHeaderStored then
    Result := inherited GetSpaceAfterGroupHeader
  else
    Result := ParentOptions.GetSpaceAfterGroupHeader;
end;

function TdxRibbonGalleryGroupOptions.GetSpaceBetweenItemCaptionAndDescription: Integer;
begin
  if IsSpaceBetweenItemCaptionAndDescriptionStored then
    Result := inherited GetSpaceBetweenItemCaptionAndDescription
  else
    Result := ParentOptions.GetSpaceBetweenItemCaptionAndDescription;
end;

function TdxRibbonGalleryGroupOptions.GetSpaceBetweenItemImageAndText: Integer;
begin
  if IsSpaceBetweenItemImageAndTextStored then
    Result := inherited GetSpaceBetweenItemImageAndText
  else
    Result := ParentOptions.GetSpaceBetweenItemImageAndText;
end;

function TdxRibbonGalleryGroupOptions.GetSpaceBetweenItemsHorizontally: Integer;
begin
  if IsSpaceBetweenItemsHorizontallyStored then
    Result := inherited GetSpaceBetweenItemsHorizontally
  else
    Result := ParentOptions.GetSpaceBetweenItemsHorizontally;
end;

function TdxRibbonGalleryGroupOptions.GetSpaceBetweenItemsVertically: Integer;
begin
  if IsSpaceBetweenItemsVerticallyStored then
    Result := inherited GetSpaceBetweenItemsVertically
  else
    Result := ParentOptions.GetSpaceBetweenItemsVertically;
end;

function TdxRibbonGalleryGroupOptions.IsItemImagePositionStored: Boolean;
begin
  Result := avItemImagePosition in FAssignedValues;
end;

function TdxRibbonGalleryGroupOptions.IsItemImageSizeStored: Boolean;
begin
  Result := avItemImageSize in FAssignedValues;
end;

procedure TdxRibbonGalleryGroupOptions.ItemImageSizeChange(Sender: TObject);
begin
  Include(FAssignedValues, avItemImageSize);
  inherited ItemImageSizeChange(Sender);
end;

procedure TdxRibbonGalleryGroupOptions.ItemPullHighlightingChange(Sender: TObject);
begin
  ItemPullHighlightingMode := ObtainItemPullHighlightingMode;
end;

function TdxRibbonGalleryGroupOptions.IsSpaceAfterGroupHeaderStored: Boolean;
begin
  Result := avSpaceAfterGroupHeader in FAssignedValues;
end;

function TdxRibbonGalleryGroupOptions.IsSpaceBetweenItemCaptionAndDescriptionStored: Boolean;
begin
  Result := avSpaceBetweenItemCaptionAndDescription in FAssignedValues;
end;

function TdxRibbonGalleryGroupOptions.IsSpaceBetweenItemImageAndTextStored: Boolean;
begin
  Result := avSpaceBetweenItemImageAndText in FAssignedValues;
end;

function TdxRibbonGalleryGroupOptions.IsSpaceBetweenItemsHorizontallyStored: Boolean;
begin
  Result := avSpaceBetweenItemsHorizontally in FAssignedValues;
end;

function TdxRibbonGalleryGroupOptions.IsSpaceBetweenItemsVerticallyStored: Boolean;
begin
  Result := avSpaceBetweenItemsVertically in FAssignedValues;
end;

procedure TdxRibbonGalleryGroupOptions.SetAssignedValues(
  const Value: TdxRibbonGalleryGroupOptionsAssignedValues);
begin
  if FAssignedValues <> Value then
  begin
    FAssignedValues := Value;
    ItemSize.Assigned := avItemSize in FAssignedValues;
    ItemImageSize.Assigned := avItemImageSize in FAssignedValues;
    Changed;
  end;
end;

procedure TdxRibbonGalleryGroupOptions.SetItemPullHighlightingMode(Value: TdxRibbonGalleryItemPullHighlightingMode);
begin
  Include(FAssignedValues, avItemPullHighlightingMode);
  if Value <> FItemPullHighlightingMode then
  begin
    FItemPullHighlightingMode := Value;
    Changed;
  end;
end;

procedure TdxRibbonGalleryGroupOptions.SetItemImagePosition(
  Value: TdxRibbonGalleryImagePosition);
begin
  Include(FAssignedValues, avItemImagePosition);
  inherited;
end;

procedure TdxRibbonGalleryGroupOptions.SetItemSize(Value: TcxItemSize);
begin
  FItemSize.Assign(Value);
end;

procedure TdxRibbonGalleryGroupOptions.SetItemTextAlignVert(Value: TcxAlignmentVert);
begin
  Include(FAssignedValues, avItemTextAlignVert);
  if FItemTextAlignVert <> Value then
  begin
    FItemTextAlignVert := Value;
    Changed;
  end;
end;

procedure TdxRibbonGalleryGroupOptions.SetItemTextKind(
  Value: TdxRibbonGalleryGroupItemTextKind);
begin
  Include(FAssignedValues, avItemTextKind);
  if FItemTextKind <> Value then
  begin
    FItemTextKind := Value;
    Changed;
  end;
end;

procedure TdxRibbonGalleryGroupOptions.SetSpaceAfterGroupHeader(Value: Integer);
begin
  Include(FAssignedValues, avSpaceAfterGroupHeader);
  inherited;
end;

procedure TdxRibbonGalleryGroupOptions.SetSpaceBetweenItemCaptionAndDescription(
  Value: Integer);
begin
  Include(FAssignedValues, avSpaceBetweenItemCaptionAndDescription);
  inherited;
end;

procedure TdxRibbonGalleryGroupOptions.SetSpaceBetweenItemImageAndText(
  Value: Integer);
begin
  Include(FAssignedValues, avSpaceBetweenItemImageAndText);
  inherited;
end;

procedure TdxRibbonGalleryGroupOptions.SetSpaceBetweenItemsHorizontally(Value: Integer);
begin
  Include(FAssignedValues, avSpaceBetweenItemsHorizontally);
  inherited;
end;

procedure TdxRibbonGalleryGroupOptions.SetSpaceBetweenItemsVertically(Value: Integer);
begin
  Include(FAssignedValues, avSpaceBetweenItemsVertically);
  inherited;
end;

function TdxRibbonGalleryGroupOptions.GetItemPullHighlightingMode: TdxRibbonGalleryItemPullHighlightingMode;
begin
  if IsItemPullHighlightingModeStored then
    Result := FItemPullHighlightingMode
  else
    Result := InMenuOptions.ItemPullHighlightingMode;
end;

function TdxRibbonGalleryGroupOptions.IsItemPullHighlightingModeStored: Boolean;
begin
  Result := avItemPullHighlightingMode in FAssignedValues;
end;

function TdxRibbonGalleryGroupOptions.IsItemSizeStored: Boolean;
begin
  Result := avItemSize in FAssignedValues;
end;

function TdxRibbonGalleryGroupOptions.IsItemTextAlignVertStored: Boolean;
begin
  Result := avItemTextAlignVert in FAssignedValues;
end;

function TdxRibbonGalleryGroupOptions.IsItemTextKindStored: Boolean;
begin
  Result := avItemTextKind in FAssignedValues;
end;

procedure TdxRibbonGalleryGroupOptions.ItemSizeChange(Sender: TObject);
begin
  Include(FAssignedValues, avItemSize);
  Changed;
end;

{ TdxCustomRibbonGalleryOptions }

constructor TdxCustomRibbonGalleryOptions.Create(AOwner: TdxCustomRibbonGalleryItem);
begin
  inherited Create(AOwner);
  FItemHintSource := ghsItemHint;
  FItemSelectionMode := gsmSingle;
  FColumnCount := dxRibbonGalleryDefaultColumnCount;
  //FShowItemHint := True; deprecated
  //FShowScrollbar := True; deprecated
  FSpaceBetweenItemsAndBorder := 1;
  FLongDescriptionDefaultRowCount := 2;
end;

procedure TdxCustomRibbonGalleryOptions.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxCustomRibbonGalleryOptions then
  begin
    CanCollapse := TdxCustomRibbonGalleryOptions(Source).CanCollapse;
    Collapsed := TdxCustomRibbonGalleryOptions(Source).Collapsed;
    ColumnCount := TdxCustomRibbonGalleryOptions(Source).ColumnCount;
    EqualItemSizeInAllGroups := TdxCustomRibbonGalleryOptions(Source).EqualItemSizeInAllGroups;
    ItemAllowDeselect := TdxCustomRibbonGalleryOptions(Source).ItemAllowDeselect;
    ItemHintSource := TdxCustomRibbonGalleryOptions(Source).ItemHintSource;
    ItemSelectionMode := TdxCustomRibbonGalleryOptions(Source).ItemSelectionMode;
    ItemSizeInRibbon := TdxCustomRibbonGalleryOptions(Source).ItemSizeInRibbon;
    LongDescriptionDefaultRowCount := TdxCustomRibbonGalleryOptions(Source).LongDescriptionDefaultRowCount;
    RowCount := TdxCustomRibbonGalleryOptions(Source).RowCount;
    //ShowItemHint := TdxCustomRibbonGalleryOptions(Source).ShowItemHint; deprecated
    //ShowScrollBar := TdxCustomRibbonGalleryOptions(Source).ShowScrollbar; deprecated
    SpaceBetweenGroups := TdxCustomRibbonGalleryOptions(Source).SpaceBetweenGroups;
    SpaceBetweenItemsAndBorder := TdxCustomRibbonGalleryOptions(Source).SpaceBetweenItemsAndBorder;
    SubmenuResizing := TdxCustomRibbonGalleryOptions(Source).SubmenuResizing;
  end;
end;

procedure TdxCustomRibbonGalleryOptions.ChangeScale(M: Integer; D: Integer);
begin
  inherited ChangeScale(M, D);
  SpaceBetweenGroups := MulDiv(SpaceBetweenGroups, M, D);
  SpaceBetweenItemsAndBorder := MulDiv(SpaceBetweenItemsAndBorder, M, D);
end;

procedure TdxCustomRibbonGalleryOptions.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('LongDescriptionDefaultRowCount', ReadLongDescriptionDefaultRowCount,
    WriteLongDescriptionDefaultRowCount, LongDescriptionDefaultRowCount <> 2);
end;

procedure TdxCustomRibbonGalleryOptions.DoImageListContentChanged;
begin
  GalleryItem.GalleryChanged;
  GalleryItem.Recalculate;
end;

procedure TdxCustomRibbonGalleryOptions.DoSelectionChanged(DeselectAll: Boolean);
begin
  if Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self, DeselectAll);
end;

function TdxCustomRibbonGalleryOptions.GetCanCollapse: Boolean;
begin
  Result := InRibbonOptions.CanCollapse;
end;

function TdxCustomRibbonGalleryOptions.GetCollapsed: Boolean;
begin
  Result := InRibbonOptions.Collapsed;
end;

function TdxCustomRibbonGalleryOptions.GetEqualItemSizeInAllGroups: Boolean;
begin
  Result := InMenuOptions.EqualItemSizeInAllGroups;
end;

function TdxCustomRibbonGalleryOptions.GetItemSizeInRibbon: TcxItemSize;
begin
  Result := InRibbonOptions.ItemSize;
end;

function TdxCustomRibbonGalleryOptions.GetMinColumnCount: Integer;
begin
  Result := InRibbonOptions.MinColumnCount;
end;

function TdxCustomRibbonGalleryOptions.GetRowCount: Integer;
begin
  Result := InMenuOptions.RowCount;
end;

function TdxCustomRibbonGalleryOptions.GetShowItemHint: Boolean;
begin
  Result := ItemHintSource = ghsItemHint;
end;

function TdxCustomRibbonGalleryOptions.GetSubmenuResizing: TdxRibbonGallerySubmenuResizing;
begin
  Result := InMenuOptions.DropDownGalleryResizing;
end;

procedure TdxCustomRibbonGalleryOptions.SetCanCollapse(Value: Boolean);
begin
  InRibbonOptions.CanCollapse := Value;
end;

procedure TdxCustomRibbonGalleryOptions.SetCollapsed(Value: Boolean);
begin
  InRibbonOptions.Collapsed := Value;
end;

procedure TdxCustomRibbonGalleryOptions.SetColumnCount(Value: Integer);
begin
  if Value <> FColumnCount then
  begin
    FColumnCount := Value;
    ColumnCountChanged;
  end;
end;

procedure TdxCustomRibbonGalleryOptions.SetSpaceBetweenGroups(Value: Integer);
begin
  if Value <> FSpaceBetweenGroups then
  begin
    FSpaceBetweenGroups := Value;
    Changed;
  end;
end;

procedure TdxCustomRibbonGalleryOptions.SetEqualItemSizeInAllGroups(Value: Boolean);
begin
  InMenuOptions.EqualItemSizeInAllGroups := Value;
end;

procedure TdxCustomRibbonGalleryOptions.SetItemHintSource(Value: TdxRibbonGalleryItemHintSource);
begin
  if Value <> FItemHintSource then
  begin
    FItemHintSource := Value;
    Changed;
  end;
end;

procedure TdxCustomRibbonGalleryOptions.SetItemSelectionMode(
  Value: TdxRibbonGalleryItemSelectionMode);
begin
  if Value <> FItemSelectionMode then
  begin
    DoSelectionChanged((FItemSelectionMode <> gsmSingle) and (Value in [gsmSingle, gsmSingleInGroup]));
    FItemSelectionMode := Value;
  end;
end;

procedure TdxCustomRibbonGalleryOptions.SetItemSizeInRibbon(Value: TcxItemSize);
begin
  GalleryItem.GalleryInRibbonOptions.ItemSize.Assign(Value);
end;

procedure TdxCustomRibbonGalleryOptions.SetLongDescriptionDefaultRowCount(Value: Integer);
begin
  if Value <> FLongDescriptionDefaultRowCount then
  begin
    FLongDescriptionDefaultRowCount := Max(Value, 1);
    Changed;
  end;
end;

procedure TdxCustomRibbonGalleryOptions.SetMinColumnCount(Value: Integer);
begin
  InRibbonOptions.MinColumnCount := Value;
end;

procedure TdxCustomRibbonGalleryOptions.SetRowCount(Value: Integer);
begin
  InMenuOptions.RowCount := Value;
end;

procedure TdxCustomRibbonGalleryOptions.SetShowItemHint(Value: Boolean);
const
  ItemHintSourceMap: array [Boolean] of TdxRibbonGalleryItemHintSource = (ghsGalleryScreenTip, ghsItemHint);
begin
  if Value <> ShowItemHint then
    ItemHintSource := ItemHintSourceMap[Value];
end;

procedure TdxCustomRibbonGalleryOptions.SetSpaceBetweenItemsAndBorder(
  Value: Integer);
begin
  CheckIntRange(Value);
  if FSpaceBetweenItemsAndBorder <> Value then
  begin
    FSpaceBetweenItemsAndBorder := Value;
    Changed;
  end;
end;

procedure TdxCustomRibbonGalleryOptions.SetSubmenuResizing(Value: TdxRibbonGallerySubmenuResizing);
begin
  InMenuOptions.DropDownGalleryResizing := Value;
end;

procedure TdxCustomRibbonGalleryOptions.ReadLongDescriptionDefaultRowCount(Reader: TReader);
begin
  LongDescriptionDefaultRowCount := Reader.ReadInteger;
end;

procedure TdxCustomRibbonGalleryOptions.WriteLongDescriptionDefaultRowCount(Writer: TWriter);
begin
  Writer.WriteInteger(LongDescriptionDefaultRowCount);
end;

procedure TdxCustomRibbonGalleryOptions.ColumnCountChanged;
begin
  CallNotify(FOnColumnCountChanged, Self);
end;

{ TdxInRibbonGalleryOptions }

constructor TdxInRibbonGalleryOptions.Create;
begin
  inherited Create;
  FCanCollapse := True;
  FMinColumnCount := dxRibbonGalleryMinColumnCount;
  FItemSize := TcxItemSize.Create(Self);
  FItemSize.OnChange := ItemSizeChange;
end;

destructor TdxInRibbonGalleryOptions.Destroy;
begin
  FreeAndNil(FItemSize);
  inherited Destroy;
end;

procedure TdxInRibbonGalleryOptions.Assign(Source: TPersistent);
begin
  if Source is TdxInRibbonGalleryOptions then
  begin
    AlwaysShowItemCaption := TdxInRibbonGalleryOptions(Source).AlwaysShowItemCaption;
    CanCollapse := TdxInRibbonGalleryOptions(Source).CanCollapse;
    Collapsed := TdxInRibbonGalleryOptions(Source).Collapsed;
    ItemSize := TdxInRibbonGalleryOptions(Source).ItemSize;
    MinColumnCount := TdxInRibbonGalleryOptions(Source).MinColumnCount;
  end
  else
    inherited Assign(Source);
end;

procedure TdxInRibbonGalleryOptions.ChangeScale(M, D: Integer);
begin
  ItemSize.ChangeScale(M, D);
end;

procedure TdxInRibbonGalleryOptions.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TdxInRibbonGalleryOptions.MinColumnCountChanged;
begin
  CallNotify(FOnMinColumnCountChanged, Self);
end;

procedure TdxInRibbonGalleryOptions.ItemSizeChange(Sender: TObject);
begin
  Changed;
end;

procedure TdxInRibbonGalleryOptions.SetAlwaysShowItemCaption(Value: Boolean);
begin
  if Value <> FAlwaysShowItemCaption then
  begin
    FAlwaysShowItemCaption := Value;
    Changed;
  end;
end;

procedure TdxInRibbonGalleryOptions.SetCanCollapse(Value: Boolean);
begin
  if Value <> FCanCollapse then
  begin
    FCanCollapse := Value;
    Changed;
  end;
end;

procedure TdxInRibbonGalleryOptions.SetCollapsed(Value: Boolean);
begin
  if Value <> FCollapsed then
  begin
    FCollapsed := Value;
    Changed;
  end;
end;

procedure TdxInRibbonGalleryOptions.SetItemSize(Value: TcxItemSize);
begin
  FItemSize.Assign(Value);
end;

procedure TdxInRibbonGalleryOptions.SetMinColumnCount(Value: Integer);
begin
  if Value <> FMinColumnCount then
  begin
    FMinColumnCount := Max(Value, 1);
    MinColumnCountChanged;
  end;
end;

{ TdxInMenuGalleryOptions }

constructor TdxInMenuGalleryOptions.Create;
begin
  inherited Create;
  FCollapsedInSubmenu := True;
  FEqualItemSizeInAllGroups := True;
  FDropDownGalleryResizing := gsrWidthAndHeight;
  FItemSize := TcxItemSize.Create(Self);
  FItemSize.OnChange := ItemSizeChange;
  FItemTextAlignVert := vaTop;
  FItemTextKind := itkCaption;
end;

destructor TdxInMenuGalleryOptions.Destroy;
begin
  FreeAndNil(FItemSize);
  inherited Destroy;
end;

procedure TdxInMenuGalleryOptions.Assign(Source: TPersistent);
begin
  if Source is TdxInMenuGalleryOptions then
  begin
    CollapsedInSubmenu := TdxInMenuGalleryOptions(Source).CollapsedInSubmenu;
    EqualItemSizeInAllGroups := TdxInMenuGalleryOptions(Source).EqualItemSizeInAllGroups;
    DropDownGalleryResizing := TdxInMenuGalleryOptions(Source).DropDownGalleryResizing;
    ItemPullHighlightingMode := TdxInMenuGalleryOptions(Source).ItemPullHighlightingMode;
    ItemSize := TdxInMenuGalleryOptions(Source).ItemSize;
    ItemTextKind := TdxInMenuGalleryOptions(Source).ItemTextKind;
    ItemTextAlignVert := TdxInMenuGalleryOptions(Source).ItemTextAlignVert;
    RowCount := TdxInMenuGalleryOptions(Source).RowCount;
  end
  else
    inherited Assign(Source);
end;

procedure TdxInMenuGalleryOptions.ChangeScale(M, D: Integer);
begin
  ItemSize.ChangeScale(M, D);
end;

procedure TdxInMenuGalleryOptions.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TdxInMenuGalleryOptions.IsItemSizeStored: Boolean;
begin
  Result := (FItemSize.Width <> 0) or (FItemSize.Height <> 0);
end;

procedure TdxInMenuGalleryOptions.ItemSizeChange(Sender: TObject);
begin
  Changed;
end;

procedure TdxInMenuGalleryOptions.SetCollapsedInSubmenu(Value: Boolean);
begin
  if Value <> FCollapsedInSubmenu then
  begin
    FCollapsedInSubmenu := Value;
    Changed;
  end;
end;

procedure TdxInMenuGalleryOptions.SetEqualItemSizeInAllGroups(Value: Boolean);
begin
  if Value <> FEqualItemSizeInAllGroups then
  begin
    FEqualItemSizeInAllGroups := Value;
    Changed;
  end;
end;

procedure TdxInMenuGalleryOptions.SetItemPullHighlightingMode(Value: TdxRibbonGalleryItemPullHighlightingMode);
begin
  if Value <> FItemPullHighlightingMode then
  begin
    FItemPullHighlightingMode := Value;
    Changed;
  end;
end;

procedure TdxInMenuGalleryOptions.SetItemSize(Value: TcxItemSize);
begin
  FItemSize.Assign(Value);
end;

procedure TdxInMenuGalleryOptions.SetItemTextAlignVert(Value: TcxAlignmentVert);
begin
  if FItemTextAlignVert <> Value then
  begin
    FItemTextAlignVert := Value;
    Changed;
  end;
end;

procedure TdxInMenuGalleryOptions.SetItemTextKind(Value: TdxRibbonGalleryGroupItemTextKind);
begin
  if FItemTextKind <> Value then
  begin
    FItemTextKind := Value;
    Changed;
  end;
end;

procedure TdxInMenuGalleryOptions.SetRowCount(Value: Integer);
begin
  if Value <> FRowCount then
  begin
    FRowCount := Max(Value, 0);
    Changed;
  end;
end;

{ TdxRibbonGalleryGroupHeader }

procedure TdxRibbonGalleryGroupHeader.Changed;
begin
  (Owner as TdxRibbonGalleryGroup).Changed(True);
end;

constructor TdxRibbonGalleryGroupHeader.Create(AOwner: TPersistent);
begin
  inherited;
  Alignment := taLeftJustify;
  Visible := False;
end;

{ TdxRibbonGalleryGroupItem }

constructor TdxRibbonGalleryGroupItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyphInDropDown := TdxSmartGlyph.Create;
end;

destructor TdxRibbonGalleryGroupItem.Destroy;
begin
  GalleryItem.RemoveGroupItem(Self);
  FreeAndNil(FActionLink);
  FreeAndNil(FGlyphInDropDown);
  inherited Destroy;
end;

procedure TdxRibbonGalleryGroupItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxRibbonGalleryGroupItem then
  begin
    Action := TdxRibbonGalleryGroupItem(Source).Action;
    Selected := TdxRibbonGalleryGroupItem(Source).Selected;
    OnClick := TdxRibbonGalleryGroupItem(Source).OnClick;
    OnMouseDown := TdxRibbonGalleryGroupItem(Source).OnMouseDown;
    OnMouseMove := TdxRibbonGalleryGroupItem(Source).OnMouseMove;
    OnMouseUp := TdxRibbonGalleryGroupItem(Source).OnMouseUp;
  end;
end;

procedure TdxRibbonGalleryGroupItem.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  if Action is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.Caption = '') then
        Self.Caption := Caption;
      if not CheckDefaults or (not Self.Selected) then
        Self.Selected := Checked;
      if not CheckDefaults or (Self.Enabled) then
        Self.Enabled := Enabled;
      if not CheckDefaults or (Self.Hint = '') then
        Self.Hint := Hint;
      if not CheckDefaults or (Self.ImageIndex = -1) then
        Self.ImageIndex := ImageIndex;
      if not CheckDefaults or not Assigned(Self.OnClick) then
        Self.OnClick := OnExecute;
    end;
end;

procedure TdxRibbonGalleryGroupItem.DoClick;
begin
  if GalleryItem.AreGroupItemClickEventsLocked then
    Exit;
  if Assigned(FOnClick) and ((Action = nil) or (@FOnClick <> @Action.OnExecute)) then
    FOnClick(Self)
  else
    if not IsDesigning and (FActionLink <> nil) then
      FActionLink.Execute(GalleryItem);
end;

function TdxRibbonGalleryGroupItem.GetActionLinkClass: TdxRibbonGalleryGroupItemActionLinkClass;
begin
  Result := TdxRibbonGalleryGroupItemActionLink;
end;

function TdxRibbonGalleryGroupItem.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
begin
  Result := (AParent as TdxRibbonGalleryGroup).Items;
end;

function TdxRibbonGalleryGroupItem.GetEnabled: Boolean;
begin
  Result := Group.GalleryItem.Enabled and Enabled;
end;

function TdxRibbonGalleryGroupItem.GetOwner: TPersistent;
begin
  Result := Collection;
end;

procedure TdxRibbonGalleryGroupItem.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TdxRibbonGalleryGroupItem.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then FOnMouseMove(Self, Shift, X, Y);
end;

procedure TdxRibbonGalleryGroupItem.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TdxRibbonGalleryGroupItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if not (csDestroying in ComponentState) and (Operation = opRemove) and (AComponent = Action) then
    Action := nil;
end;

procedure TdxRibbonGalleryGroupItem.UpdateActionLink;
begin
  if FActionLink <> nil then
    FActionLink.Update;
end;

procedure TdxRibbonGalleryGroupItem.DoActionChange(Sender: TObject);
begin
  if Sender = Action then ActionChange(Sender, False);
end;

function TdxRibbonGalleryGroupItem.GetAction: TBasicAction;
begin
  if FActionLink = nil then
    Result := nil
  else
    Result := FActionLink.Action;
end;

function TdxRibbonGalleryGroupItem.GetGalleryItem: TdxCustomRibbonGalleryItem;
begin
  Result := Group.GalleryItem;
end;

function TdxRibbonGalleryGroupItem.GetGroup: TdxRibbonGalleryGroup;
begin
  if Collection <> nil then
    Result := TdxRibbonGalleryGroupItems(Collection).Group
  else
    Result := nil;
end;

function TdxRibbonGalleryGroupItem.GetSelected: Boolean;
begin
  case SelectionMode of
    gsmNone, gsmMultiple, gsmSingleInGroup:
      Result := FSelected;
    gsmSingle:
      Result := GalleryItem.SelectedGroupItem = Self;
  else
    Result := False;
  end;
end;

function TdxRibbonGalleryGroupItem.GetSelectionMode: TdxRibbonGalleryItemSelectionMode;
begin
  Result := GalleryItem.GalleryOptions.ItemSelectionMode;
end;

function TdxRibbonGalleryGroupItem.IsCaptionStored: Boolean;
begin
  Result := (FActionLink = nil) or not FActionLink.IsCaptionLinked;
end;

function TdxRibbonGalleryGroupItem.IsDesigning: Boolean;
begin
  Result := csDesigning in GalleryItem.ComponentState;
end;

function TdxRibbonGalleryGroupItem.IsEnabledStored: Boolean;
begin
  Result := (FActionLink = nil) or not FActionLink.IsEnabledLinked;
end;

function TdxRibbonGalleryGroupItem.IsHintStored: Boolean;
begin
  Result := (FActionLink = nil) or not FActionLink.IsHintLinked;
end;

function TdxRibbonGalleryGroupItem.IsImageIndexStored: Boolean;
begin
  Result := (FActionLink = nil) or not FActionLink.IsImageIndexLinked;
end;

function TdxRibbonGalleryGroupItem.IsOnClickStored: Boolean;
begin
  Result := (FActionLink = nil) or not FActionLink.IsOnExecuteLinked;
end;

function TdxRibbonGalleryGroupItem.IsSelectedStored: Boolean;
begin
  Result := (FActionLink = nil) or not FActionLink.IsCheckedLinked;
end;

procedure TdxRibbonGalleryGroupItem.SetAction(Value: TBasicAction);
begin
  if Action <> Value then
    if Value = nil then
      FreeAndNil(FActionLink)
    else
    begin
      if csLoading in Value.ComponentState then
        TdxBarManagerAccess(GalleryItem.BarManager).LockDesignerModified(True);
      try
        if FActionLink = nil then
          FActionLink := GetActionLinkClass.Create(Self);
        FActionLink.Action := Value;
        FActionLink.OnChange := DoActionChange;
        ActionChange(Value, csLoading in Value.ComponentState);
        Value.FreeNotification(GalleryItem);
      finally
        if csLoading in Value.ComponentState then
          TdxBarManagerAccess(GalleryItem.BarManager).LockDesignerModified(
            False, False);
      end;
    end;
end;

procedure TdxRibbonGalleryGroupItem.SetGlyphInDropDown(Value: TdxSmartGlyph);
begin
  FGlyphInDropDown.Assign(Value);
end;

procedure TdxRibbonGalleryGroupItem.SetSelected(Value: Boolean);

  procedure DeselectItemsInGroup;
  var
    I: Integer;
  begin
    if Selected then
      for I := 0 to Group.Items.Count - 1 do
        if Group.Items[I].Selected and (Group.Items[I] <> Self) then
          Group.Items[I].Selected := False;
  end;

begin
  if GalleryItem.IsLoading then
    FSelected := Value
  else
    case SelectionMode of
      gsmSingle:
        if Value then
          GalleryItem.SelectedGroupItem := Self
        else
          if Selected then
            GalleryItem.SelectedGroupItem := nil;
    else
      if Value <> FSelected then
      begin
        FSelected := Value;
        if SelectionMode in [gsmMultiple, gsmSingleInGroup] then
        begin
          DoClick;
          if SelectionMode = gsmSingleInGroup then
            DeselectItemsInGroup;
          GalleryItem.DoGroupItemClick(Self);
        end;
        GalleryItem.Update;
      end;
    end;
end;

{ TdxRibbonGalleryGroupItems }

function TdxRibbonGalleryGroupItems.Add: TdxRibbonGalleryGroupItem;
begin
  Result := TdxRibbonGalleryGroupItem(inherited Add);
end;

function TdxRibbonGalleryGroupItems.Insert(Index: Integer): TdxRibbonGalleryGroupItem;
begin
  Result := TdxRibbonGalleryGroupItem(inherited Insert(Index));
end;

function TdxRibbonGalleryGroupItems.GetOwner: TPersistent;
begin
  Result := ParentComponent;
end;

procedure TdxRibbonGalleryGroupItems.SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1);
begin
  AItem.Name := CreateUniqueName(nil, Group, AItem, 'TdxRibbonGalleryGroup', '', Count);
end;

function TdxRibbonGalleryGroupItems.GetGroup: TdxRibbonGalleryGroup;
begin
  Result := TdxRibbonGalleryGroup(ParentComponent);
end;

function TdxRibbonGalleryGroupItems.GetItem(
  Index: Integer): TdxRibbonGalleryGroupItem;
begin
  Result := TdxRibbonGalleryGroupItem(inherited Items[Index]);
end;

procedure TdxRibbonGalleryGroupItems.SetItem(Index: Integer;
  Value: TdxRibbonGalleryGroupItem);
begin
  inherited Items[Index] := Value;
end;

{ TdxRibbonGalleryGroup }

destructor TdxRibbonGalleryGroup.Destroy;
begin
  FreeAndNil(FOptions);
  inherited Destroy;
end;

procedure TdxRibbonGalleryGroup.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxRibbonGalleryGroup then
    Options := TdxRibbonGalleryGroup(Source).Options;
end;

procedure TdxRibbonGalleryGroup.SetParentComponent(Value: TComponent);
begin
  inherited SetParentComponent(Value);
  CreateItems;
  FOptions := TdxRibbonGalleryGroupOptions.Create(GalleryItem.GalleryOptions, Self);
  FOptions.OnChanged := GalleryItem.OptionsChanged;
end;

procedure TdxRibbonGalleryGroup.ChangeScale(M: Integer; D: Integer);
begin
  inherited ChangeScale(M, D);
  Options.ChangeScale(M, D);
end;

procedure TdxRibbonGalleryGroup.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Items', ReadItems, nil, False);
end;

function TdxRibbonGalleryGroup.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
begin
  Result := (AParent as TdxCustomRibbonGalleryItem).GalleryGroups;
end;

function TdxRibbonGalleryGroup.GetOwner: TPersistent;
begin
  Result := Collection;
end;

procedure TdxRibbonGalleryGroup.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  inherited GetChildren(Proc, Root);
  for I := 0 to Items.Count - 1 do
    Proc(Items[I]);
end;

function TdxRibbonGalleryGroup.GetGalleryGroupHeaderClass: TdxGalleryGroupHeaderClass;
begin
  Result := TdxRibbonGalleryGroupHeader;
end;

function TdxRibbonGalleryGroup.GetGalleryItem: TdxCustomRibbonGalleryItem;
begin
  Result := TdxCustomRibbonGalleryItem(Collection.ParentComponent);
end;

function TdxRibbonGalleryGroup.GetGalleryItemClass: TdxGalleryItemClass;
begin
  Result := GalleryItem.GetGroupItemClass;
end;

function TdxRibbonGalleryGroup.GetGalleryItemsClass: TdxGalleryItemsClass;
begin
  Result := TdxRibbonGalleryGroupItems;
end;

function TdxRibbonGalleryGroup.GetHeader: TdxRibbonGalleryGroupHeader;
begin
  Result := inherited Header as TdxRibbonGalleryGroupHeader;
end;

function TdxRibbonGalleryGroup.GetImages: TCustomImageList;
begin
  Result := Options.Images;
  if Result = nil then
    Result := GalleryItem.GetImages;
end;

function TdxRibbonGalleryGroup.GetItems: TdxRibbonGalleryGroupItems;
begin
  Result := inherited Items as TdxRibbonGalleryGroupItems;
end;

procedure TdxRibbonGalleryGroup.ReadItems(AReader: TReader);
begin
  Items.ReadOldCollection(AReader);
end;

procedure TdxRibbonGalleryGroup.SetHeader(Value: TdxRibbonGalleryGroupHeader);
begin
  Header.Assign(Value);
end;

procedure TdxRibbonGalleryGroup.SetItems(Value: TdxRibbonGalleryGroupItems);
begin
  Items.Assign(Value);
end;

procedure TdxRibbonGalleryGroup.SetOptions(Value: TdxRibbonGalleryGroupOptions);
begin
  FOptions.Assign(Value);
end;

{ TdxRibbonGalleryGroups }

function TdxRibbonGalleryGroups.Add: TdxRibbonGalleryGroup;
begin
  Result := TdxRibbonGalleryGroup(inherited Add);
end;

function TdxRibbonGalleryGroups.Insert(Index: Integer): TdxRibbonGalleryGroup;
begin
  Result := TdxRibbonGalleryGroup(inherited Insert(Index));
end;

function TdxRibbonGalleryGroups.GetOwner: TPersistent;
begin
  Result := GalleryItem;
end;

procedure TdxRibbonGalleryGroups.Notify(AItem: TcxComponentCollectionItem;
  AAction: TcxComponentCollectionNotification);
begin
  inherited Notify(AItem, AAction);
  if (AAction = ccnExtracting) and not (csDestroying in GalleryItem.ComponentState) then
    RemoveFromFilter(AItem);
end;

procedure TdxRibbonGalleryGroups.SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1);
begin
  AItem.Name := CreateUniqueName(nil, GalleryItem, AItem, 'TdxRibbonGallery', '', Count);
end;

procedure TdxRibbonGalleryGroups.Update(AItem: TcxComponentCollectionItem;
  AAction: TcxComponentCollectionNotification);
begin
  inherited Update(AItem, AAction);
  GalleryItem.GalleryChanged;
end;

function TdxRibbonGalleryGroups.GetGalleryItem: TdxCustomRibbonGalleryItem;
begin
  Result := TdxCustomRibbonGalleryItem(ParentComponent);
end;

function TdxRibbonGalleryGroups.GetItem(Index: Integer): TdxRibbonGalleryGroup;
begin
  Result := TdxRibbonGalleryGroup(inherited Items[Index]);
end;

procedure TdxRibbonGalleryGroups.RemoveFromFilter(AItem: TcxComponentCollectionItem);
begin
  if AItem <> nil then
    GalleryItem.GalleryFilter.Categories.DeleteGroup(TdxRibbonGalleryGroup(AItem));
end;

procedure TdxRibbonGalleryGroups.SetItem(Index: Integer;
  Value: TdxRibbonGalleryGroup);
begin
  inherited Items[Index] := Value;
end;

{ TdxRibbonGalleryFilterCategoryGroups }

constructor TdxRibbonGalleryFilterCategoryGroups.Create(
  AFilterCategory: TdxRibbonGalleryFilterCategory);
begin
  inherited Create;
  FFilterCategory := AFilterCategory;
end;

procedure TdxRibbonGalleryFilterCategoryGroups.Assign(
  ASource: TdxRibbonGalleryFilterCategoryGroups);
var
  I: Integer;
begin
  if FilterCategory.GalleryItem.GalleryGroups.Count <>
    ASource.FilterCategory.GalleryItem.GalleryGroups.Count then
      raise EdxException.Create('Invalid GalleryGroups count in given instance of TdxRibbonGalleryFilterCategoryGroups');
  Clear;
  for I := 0 to ASource.Count - 1 do
    Add(FilterCategory.GalleryItem.GalleryGroups[ASource[I].Index]);
end;

function TdxRibbonGalleryFilterCategoryGroups.Add(AGroup: TdxRibbonGalleryGroup): Integer;
begin
  if CanAddGroup(AGroup) then
    Result := inherited Add(AGroup)
  else
    Result := -1;
end;

procedure TdxRibbonGalleryFilterCategoryGroups.Insert(AIndex: Integer;
  AGroup: TdxRibbonGalleryGroup);
begin
  if CanAddGroup(AGroup) then
    inherited Insert(AIndex, AGroup);
end;

procedure TdxRibbonGalleryFilterCategoryGroups.Notify(Ptr: Pointer;
  Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if Action in [lnAdded, lnDeleted] then
    FilterCategory.Changed(False);
end;

function TdxRibbonGalleryFilterCategoryGroups.CanAddGroup(
  AGroup: TdxRibbonGalleryGroup): Boolean;

  function IsGroupValid: Boolean;
  begin
    Result := (AGroup <> nil) and (AGroup.GalleryItem = FilterCategory.GalleryItem);
  end;

begin
  Result := IsGroupValid and (IndexOf(AGroup) = -1);
end;

function TdxRibbonGalleryFilterCategoryGroups.GetItem(
  Index: Integer): TdxRibbonGalleryGroup;
begin
  Result := TdxRibbonGalleryGroup(inherited Items[Index]);
end;

{ TdxRibbonGalleryFilterCategory }

constructor TdxRibbonGalleryFilterCategory.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FGroupIndexes := TList.Create;
  FGroups := TdxRibbonGalleryFilterCategoryGroups.Create(Self);
end;

destructor TdxRibbonGalleryFilterCategory.Destroy;
begin
  FreeAndNil(FGroups);
  FreeAndNil(FGroupIndexes);
  inherited Destroy;
end;

procedure TdxRibbonGalleryFilterCategory.Assign(Source: TPersistent);
begin
  if Source is TdxRibbonGalleryFilterCategory then
  begin
    Caption := TdxRibbonGalleryFilterCategory(Source).Caption;
    Groups.Assign(TdxRibbonGalleryFilterCategory(Source).Groups);
  end
  else
    inherited Assign(Source);
end;

procedure TdxRibbonGalleryFilterCategory.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('CategoryGroups', ReadCategoryGroups, WriteCategoryGroups,
    FGroups.Count > 0);
end;

procedure TdxRibbonGalleryFilterCategory.RestoreGroupIndexes;
var
  I: Integer;
begin
  if FGroupIndexes.Count > 0 then
  begin
    Groups.Clear;
    for I := 0 to FGroupIndexes.Count - 1 do
      AddGroupByGroupIndex(Integer(FGroupIndexes.Items[I]));
  end;
end;

procedure TdxRibbonGalleryFilterCategory.AddGroupByGroupIndex(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < GalleryItem.GalleryGroups.Count) and
    (Groups.IndexOf(GalleryItem.GalleryGroups[AIndex]) = -1) then
    Groups.Add(GalleryItem.GalleryGroups[AIndex]);
end;

function TdxRibbonGalleryFilterCategory.GetGalleryItem: TdxCustomRibbonGalleryItem;
begin
  Result := TdxRibbonGalleryFilterCategories(Collection).GalleryFilter.GalleryItem;
end;

procedure TdxRibbonGalleryFilterCategory.ReadCategoryGroups(AReader: TReader);
var
  AIndex: Integer;
begin
  FGroupIndexes.Clear;
  Groups.Clear;
  AReader.ReadListBegin;
  while not AReader.EndOfList do
  begin
    AIndex := AReader.ReadInteger;
    if csLoading in GalleryItem.ComponentState then
      FGroupIndexes.Add(Pointer(AIndex))
    else
      AddGroupByGroupIndex(AIndex);
  end;
  AReader.ReadListEnd;
end;

procedure TdxRibbonGalleryFilterCategory.WriteCategoryGroups(AWriter: TWriter);
var
  I, J, AIndex: Integer;
begin
  AWriter.WriteListBegin;
  for I := 0 to Groups.Count - 1 do
  begin
    AIndex := -1;
    for J := 0 to GalleryItem.GalleryGroups.Count - 1 do
      if Groups[I] = GalleryItem.GalleryGroups[J] then
      begin
        AIndex := J;
        Break;
      end;
    if AIndex <> -1 then
      AWriter.WriteInteger(AIndex);
  end;
  AWriter.WriteListEnd;
end;

{ TdxRibbonGalleryFilterCategories }

constructor TdxRibbonGalleryFilterCategories.Create(
  AGalleryFilter: TdxRibbonGalleryFilter);
begin
  inherited Create(TdxRibbonGalleryFilterCategory);
  FGalleryFilter := AGalleryFilter;
end;

function TdxRibbonGalleryFilterCategories.Add: TdxRibbonGalleryFilterCategory;
begin
  Result := TdxRibbonGalleryFilterCategory(inherited Add);
end;

procedure TdxRibbonGalleryFilterCategories.DeleteGroup(
  AGroup: TdxRibbonGalleryGroup);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Groups.Remove(AGroup);
end;

function TdxRibbonGalleryFilterCategories.GetOwner: TPersistent;
begin
  Result := FGalleryFilter;
end;

procedure TdxRibbonGalleryFilterCategories.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  FGalleryFilter.CategoriesChanged;
end;

function TdxRibbonGalleryFilterCategories.GetItem(
  Index: Integer): TdxRibbonGalleryFilterCategory;
begin
  Result := TdxRibbonGalleryFilterCategory(inherited GetItem(Index));
end;

procedure TdxRibbonGalleryFilterCategories.SetItem(Index: Integer;
  Value: TdxRibbonGalleryFilterCategory);
begin
  inherited SetItem(Index, Value);
end;

{ TdxRibbonGalleryFilter }

constructor TdxRibbonGalleryFilter.Create(AGalleryItem: TdxCustomRibbonGalleryItem);
begin
  inherited Create;
  FGalleryItem := AGalleryItem;
  FActiveCategoryIndex := -1;
  FCategories := TdxRibbonGalleryFilterCategories.Create(Self);
  FLoadedActiveCategoryIndex := -1;
end;

destructor TdxRibbonGalleryFilter.Destroy;
begin
  FreeAndNil(FCategories);
  inherited Destroy;
end;

procedure TdxRibbonGalleryFilter.Assign(Source: TPersistent);
begin
  if Source is TdxRibbonGalleryFilter then
  begin
    Caption := TdxRibbonGalleryFilter(Source).Caption;
    Categories := TdxRibbonGalleryFilter(Source).Categories;
    ActiveCategoryIndex := TdxRibbonGalleryFilter(Source).ActiveCategoryIndex; // must be after Categories
    Visible := TdxRibbonGalleryFilter(Source).Visible;
  end
  else
    inherited Assign(Source);
end;

function TdxRibbonGalleryFilter.IsGroupFiltered(
  AGroup: TdxRibbonGalleryGroup): Boolean;
begin
  Result := (ActiveCategoryIndex <> -1) and
    (Categories[ActiveCategoryIndex].Groups.IndexOf(AGroup) = -1);
end;

procedure TdxRibbonGalleryFilter.CategoriesChanged;
begin
  if ActiveCategoryIndex >= Categories.Count then
    ActiveCategoryIndex := -1
  else
    if ActiveCategoryIndex <> -1 then
      GalleryItem.FilterChanged;
end;

function TdxRibbonGalleryFilter.GetOwner: TPersistent;
begin
  Result := GalleryItem;
end;

procedure TdxRibbonGalleryFilter.Loaded;
var
  I: Integer;
begin
  ActiveCategoryIndex := FLoadedActiveCategoryIndex;
  for I := 0 to Categories.Count - 1 do
    Categories.Items[I].RestoreGroupIndexes;
end;

procedure TdxRibbonGalleryFilter.SetActiveCategoryIndex(Value: Integer);
begin
  if GalleryItem.IsLoading then
  begin
    FLoadedActiveCategoryIndex := Value;
    Exit;
  end;
  if Value < 0 then
    Value := -1
  else
    if Value >= Categories.Count then
      Exit;
  if FActiveCategoryIndex <> Value then
  begin
    FActiveCategoryIndex := Value;
    GalleryItem.FilterChanged;
  end;
end;

procedure TdxRibbonGalleryFilter.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    if Visible then
      GalleryItem.FilterCaptionChanged;
  end;
end;

procedure TdxRibbonGalleryFilter.SetCategories(
  Value: TdxRibbonGalleryFilterCategories);
begin
  FCategories.Assign(Value);
end;

{ TdxCustomRibbonGalleryItemActionLink }

function TdxCustomRibbonGalleryItemActionLink.Update: Boolean;
var
  AIntf: IdxActionGalleryClient;
begin
  Result := inherited Update;

  if Supports(Action, IdxActionGalleryClient, AIntf) then
  begin
    CustomRibbonGalleryItem.LockGroupItemClickEvents(True);
    try
      CustomRibbonGalleryItem.SelectedGroupItem := FindGroupItem(AIntf.Value);
    finally
      CustomRibbonGalleryItem.LockGroupItemClickEvents(False);
    end;
  end;
end;

function TdxCustomRibbonGalleryItemActionLink.FindGroupItem(AActionIndex: Variant): TdxRibbonGalleryGroupItem;
var
  I, J: Integer;
begin
  for I := 0 to CustomRibbonGalleryItem.GalleryCategories.Count - 1 do
    for J := 0 to CustomRibbonGalleryItem.GalleryCategories[I].Items.Count - 1 do
    begin
      Result := CustomRibbonGalleryItem.GalleryCategories[I].Items[J];
      if Result.FActionIndex = AActionIndex then
        Exit;
    end;
  Result := nil;
end;

function TdxCustomRibbonGalleryItemActionLink.GetCustomRibbonGalleryItem: TdxCustomRibbonGalleryItem;
begin
  Result := FClient as TdxCustomRibbonGalleryItem;
end;

{ TdxRibbonGalleryItemBarItemLink }

constructor TdxRibbonGalleryItemBarItemLink.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPositionInDropDown := ilpAfterGallery;
end;

procedure TdxRibbonGalleryItemBarItemLink.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxRibbonGalleryItemBarItemLink then
    PositionInDropDown := TdxRibbonGalleryItemBarItemLink(Source).PositionInDropDown;
end;

function TdxRibbonGalleryItemBarItemLink.GetBeginGroup: Boolean;
begin
  Result := inherited GetBeginGroup and IsNotFirstVisible;
end;

procedure TdxRibbonGalleryItemBarItemLink.SetBeginGroup(Value: Boolean);
begin
  inherited SetBeginGroup(Value and IsNotFirstVisible);
end;

procedure TdxRibbonGalleryItemBarItemLink.LoadFromIni(ASource: TCustomIniFile; const ABaseSection: string;
  ALinkIndex: Integer; AStoringKind: TdxBarStoringKind);
begin
  inherited LoadFromIni(ASource, ABaseSection, ALinkIndex, AStoringKind);
  if Self <> nil then
    PositionInDropDown := TdxRibbonGalleryItemBarItemLinkPositionInDropDown(ASource.ReadInteger(
      GetIniSection(ABaseSection, ALinkIndex, AStoringKind), 'PositionInDropDown', Ord(ilpAfterGallery)));
end;

procedure TdxRibbonGalleryItemBarItemLink.SaveToIni(ADestination: TCustomIniFile; const ABaseSection: string;
  ALinkIndex: Integer; AStoringKind: TdxBarStoringKind);
begin
  inherited SaveToIni(ADestination, ABaseSection, ALinkIndex, AStoringKind);
  ADestination.WriteInteger(GetIniSection(ABaseSection, ALinkIndex, AStoringKind), 'PositionInDropDown',
    Ord(FPositionInDropDown));
end;

procedure TdxRibbonGalleryItemBarItemLink.ProcessCustomizationPopup(ACustomizationPopupItemLinks: TdxBarItemLinks);
var
  AItem: TdxBarItem;
  APositionInDropDown: TdxRibbonGalleryItemBarItemLinkPositionInDropDown;
  ASubItem: TdxBarSubItem;
  I: Integer;
begin
  ASubItem := TdxBarSubItem(BarDesignController.AddInternalItem(ACustomizationPopupItemLinks, TdxBarSubItem,
    cxGetResourceString(@dxSBAR_GALLERYITEMLINKPOSITIONINDROPDOWN), TNotifyEvent(nil), 0, True).Item);
  TCustomdxBarSubItemAccess(ASubItem).IsInternal := True;

  for APositionInDropDown := Low(TdxRibbonGalleryItemBarItemLinkPositionInDropDown) to
    High(TdxRibbonGalleryItemBarItemLinkPositionInDropDown) do
  begin
    AItem := BarDesignController.AddInternalItem(ASubItem.ItemLinks, TdxBarButton,
      GetEnumName(TypeInfo(TdxRibbonGalleryItemBarItemLinkPositionInDropDown), Integer(APositionInDropDown)),
      PositionInDropDownCustomizationPopupItemClickHandler).Item;
    TdxBarButton(AItem).ButtonStyle := bsChecked;
    TdxBarButton(AItem).Down := PositionInDropDown = APositionInDropDown;
  end;

  for I := ACustomizationPopupItemLinks.Count - 1 downto 0 do
  begin
    AItem := ACustomizationPopupItemLinks[I].Item;
    if TdxBarCustomizationAction(HiWord(AItem.Tag)) = caChangeBeginGroup then
    begin
      AItem.Enabled := AItem.Enabled and IsNotFirstVisible;
      Break;
    end;
  end;
end;

function TdxRibbonGalleryItemBarItemLink.ProcessDragLineParameters(var ADragLineFirstPart, ADragOverBeginGroup: Boolean;
  var ADragLineRect: TRect): Boolean;
var
  APrevItemLink: TdxBarItemLink;
begin
  if ADragOverBeginGroup then
  begin
    Result := True;
    if ADragLineFirstPart then
    begin
      APrevItemLink := Owner.Prev(Self, True);
      while APrevItemLink is TdxRibbonGalleryItemBarItemLink do
      begin
        if TdxRibbonGalleryItemBarItemLink(APrevItemLink).PositionInDropDown = PositionInDropDown then
          Break
        else
          APrevItemLink := Owner.Prev(APrevItemLink, True);
      end;
    end
    else
      APrevItemLink := nil;
    ADragLineFirstPart := not ADragLineFirstPart;
    if APrevItemLink <> nil then
      ADragLineRect := APrevItemLink.ItemRect
    else
      ADragLineRect := ItemRect;
  end
  else
  begin
    Result := not ADragLineFirstPart;
    if Result then
      ADragLineRect := ItemRect;
  end;
end;

procedure TdxRibbonGalleryItemBarItemLink.ProcessDroppedItemLink(ADroppedItemLink: TdxBarItemLink);
begin
  if ADroppedItemLink is TdxRibbonGalleryItemBarItemLink then
    TdxRibbonGalleryItemBarItemLink(ADroppedItemLink).PositionInDropDown := PositionInDropDown;
end;

function TdxRibbonGalleryItemBarItemLink.ProcessNeedDragLine(ADraggingItemLink: TdxBarItemLink;
  const ADragOverFirstPart, ADragOverBeginGroup: Boolean): Boolean;
begin
  Result := not ADragOverBeginGroup and (ADraggingItemLink <> Self) and
    (((Self = ADraggingItemLink.Owner.Next(ADraggingItemLink, True)) and ADragOverFirstPart) or
      ((Self = ADraggingItemLink.Owner.Prev(ADraggingItemLink, True)) and not ADragOverFirstPart));
end;

procedure TdxRibbonGalleryItemBarItemLink.SetPositionInDropDown(
  const AValue: TdxRibbonGalleryItemBarItemLinkPositionInDropDown);
begin
  if FPositionInDropDown <> AValue then
  begin
    FPositionInDropDown := AValue;
    Changed(True);
  end;
end;

function TdxRibbonGalleryItemBarItemLink.IsNotFirstVisible: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to VisibleIndex - 1 do
    if (Owner.VisibleItems[I] is TdxRibbonGalleryItemBarItemLink) and
      (TdxRibbonGalleryItemBarItemLink(Owner.VisibleItems[I]).PositionInDropDown = PositionInDropDown) then
    begin
      Result := True;
      Break;
    end;
end;

procedure TdxRibbonGalleryItemBarItemLink.PositionInDropDownCustomizationPopupItemClickHandler(Sender: TObject);
begin
  if PositionInDropDown = ilpAfterGallery then
    PositionInDropDown := ilpBeforeGallery
  else
    PositionInDropDown := ilpAfterGallery;
end;

{ TdxRibbonGalleryItemBarItemLinks }

function TdxRibbonGalleryItemBarItemLinks.GetItemLinkClass: TdxBarItemLinkClass;
begin
  Result := TdxRibbonGalleryItemBarItemLink;
end;

{ TdxCustomRibbonGalleryItem }

constructor TdxCustomRibbonGalleryItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGalleryFilter := TdxRibbonGalleryFilter.Create(Self);
  FGalleryGroups := TdxRibbonGalleryGroups.Create(Self, TdxRibbonGalleryGroup);
  FGalleryOptions := GetGalleryOptionsClass.Create(Self);
  FGalleryOptions.OnChanged := OptionsChanged;
  FGalleryOptions.OnSelectionChanged := ItemSelectionModeChanged;
  FGalleryOptions.OnColumnCountChanged := ColumnCountChanged;
  FGalleryInRibbonOptions := TdxInRibbonGalleryOptions.Create;
  FGalleryInRibbonOptions.OnChanged := OptionsChanged;
  FGalleryInRibbonOptions.OnMinColumnCountChanged := MinColumnCountChanged;
  FGalleryInMenuOptions := TdxInMenuGalleryOptions.Create;
  FGalleryInMenuOptions.OnChanged := OptionsChanged;
end;

destructor TdxCustomRibbonGalleryItem.Destroy;
begin
  FreeAndNil(FGalleryFilter);
  FreeAndNil(FGalleryGroups);
  FreeAndNil(FGalleryOptions);
  FreeAndNil(FGalleryInRibbonOptions);
  FreeAndNil(FGalleryInMenuOptions);
  inherited Destroy;
end;

procedure TdxCustomRibbonGalleryItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxCustomRibbonGalleryItem then
  begin
    GalleryCategories := TdxCustomRibbonGalleryItem(Source).GalleryCategories;
    GalleryFilter := TdxCustomRibbonGalleryItem(Source).GalleryFilter;
    GalleryInMenuOptions := TdxCustomRibbonGalleryItem(Source).GalleryInMenuOptions;
    GalleryInRibbonOptions := TdxCustomRibbonGalleryItem(Source).GalleryInRibbonOptions;
    GalleryOptions := TdxCustomRibbonGalleryItem(Source).GalleryOptions;
  end;
end;

function TdxCustomRibbonGalleryItem.CopyGroupItem(AGroupItem: TdxRibbonGalleryGroupItem;
  ATargetGroup: TdxRibbonGalleryGroup; AIndex: Integer): TdxRibbonGalleryGroupItem;
begin
  Result := ATargetGroup.Items.Insert(AIndex);
  Result.Assign(AGroupItem);
end;

procedure TdxCustomRibbonGalleryItem.DoClick;

  procedure ClickWithoutSelection;
  begin
    FClickedGroupItem.DoClick;
    DoGroupItemClick(FClickedGroupItem);
  end;

begin
  if ClickItemLink = nil then
  begin
    inherited DoClick;
    Exit;
  end;

  if (FClickedGroupItem <> nil) and FClickedGroupItem.Enabled then
  begin
    if FClickedGroupItem.Group.Options.ItemPullHighlightingMode <> iphmNone then
      ClickWithoutSelection
    else
      case GalleryOptions.ItemSelectionMode of
        gsmNone:
          ClickWithoutSelection;
        gsmMultiple, gsmSingleInGroup:
          FClickedGroupItem.Selected := not FClickedGroupItem.Selected;
        gsmSingle:
          if FClickedGroupItem <> SelectedGroupItem then
            SelectedGroupItem := FClickedGroupItem
          else
            if GalleryOptions.ItemAllowDeselect then
              SelectedGroupItem := nil
            else
              ClickWithoutSelection;
      end;
    FFirstVisibleGroupItem := FClickedGroupItem;
  end;
end;

function TdxCustomRibbonGalleryItem.GetAddMessageName: string;
begin
  Result := cxGetResourceString(@dxSBAR_ADDGALLERYNAME);
end;

function TdxCustomRibbonGalleryItem.IsGroupVisible(AGroupIndex: Integer;
  AIgnoreVisibleProperty: Boolean = False): Boolean;
var
  AGroup: TdxRibbonGalleryGroup;
begin
  AGroup := GalleryGroups[AGroupIndex];
  Result := (AIgnoreVisibleProperty or AGroup.Visible) and
    ((AGroup.Items <> nil) and (AGroup.Items.Count > 0) or
    AGroup.Header.Visible) and not GalleryFilter.IsGroupFiltered(AGroup);
end;


function TdxCustomRibbonGalleryItem.AreGroupItemClickEventsLocked: Boolean;
begin
  Result := FLockGroupItemClickEventsCount <> 0;
end;

function TdxCustomRibbonGalleryItem.CanBePlacedOn(
  AParentKind: TdxBarItemControlParentKind; AToolbar: TdxBar;
  out AErrorText: string): Boolean;
begin
  Result := (AParentKind = pkSubItemOrPopupMenu) or (TdxBarManagerAccess(BarManager).IsInitializing or
    (bisLoadingFromIni in TdxBarManagerAccess(BarManager).InternalState) or
    GetBarControlClass(AToolbar).InheritsFrom(TdxRibbonCustomBarControl));
  if not Result then
    AErrorText := GetErrorCanPlaceText;
end;


procedure TdxCustomRibbonGalleryItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('GalleryGroups', ReadGalleryGroups, nil, False);
end;

procedure TdxCustomRibbonGalleryItem.DoCloseUp;
begin
  inherited DoCloseUp;
  if DropDownGallery <> nil then
    DropDownGallery.DoCloseUp;
end;

procedure TdxCustomRibbonGalleryItem.DoFilterChanged;
begin
  // do nothing
end;

procedure TdxCustomRibbonGalleryItem.DoGroupItemClick(AItem: TdxRibbonGalleryGroupItem);
var
  AIntf: IdxActionGalleryClient;
begin
  if not AreGroupItemClickEventsLocked and (AItem <> nil) and Supports(Action, IdxActionGalleryClient, AIntf) then
    AIntf.Value := AItem.FActionIndex;
end;

procedure TdxCustomRibbonGalleryItem.ColumnCountChanged(Sender: TObject);
begin
  GalleryOptions.MinColumnCount := Min(GalleryOptions.MinColumnCount, GalleryOptions.ColumnCount);
  OptionsChanged(Self);
end;

procedure TdxCustomRibbonGalleryItem.DoHotTrackedItemChanged(APrevHotTrackedGroupItem,
  ANewHotTrackedGroupItem: TdxRibbonGalleryGroupItem);
begin
  if Assigned(FOnHotTrackedItemChanged) then
    FOnHotTrackedItemChanged(APrevHotTrackedGroupItem, ANewHotTrackedGroupItem);
end;

procedure TdxCustomRibbonGalleryItem.DoInitFilterMenu(AItemLinks: TdxBarItemLinks);
begin
  // do nothing
end;

procedure TdxCustomRibbonGalleryItem.DoPopup;
begin
  inherited DoPopup;
  if DropDownGallery <> nil then
    DropDownGallery.DoPopup;
end;

function TdxCustomRibbonGalleryItem.GetGroupItemClass: TdxGalleryItemClass;
begin
  Result := TdxRibbonGalleryGroupItem;
end;

function TdxCustomRibbonGalleryItem.GetGalleryOptionsClass: TCustomdxRibbonGalleryOptionsClass;
begin
  Result := TdxCustomRibbonGalleryOptions;
end;

procedure TdxCustomRibbonGalleryItem.FilterCaptionChanged;
var
  I: Integer;
begin
  for I := 0 to LinkCount - 1 do
    if Links[I].Control <> nil then
      TdxRibbonGalleryControl(Links[I].Control).FilterCaptionChanged;
end;

procedure TdxCustomRibbonGalleryItem.FilterChanged;
var
  AControl: TdxBarItemControl;
  I: Integer;
begin
  FRecalculatingOnFilterChanged := True;
  try
    if FFilterChangedLockCount = 0 then
      for I := 0 to LinkCount - 1 do
      begin
        AControl := Links[I].Control;
        if AControl <> nil then
        begin
          TdxBarItemControlAccess(AControl).InternalCalculateParts;
          AControl.ViewInfo.ResetCachedValues;
          TCustomdxBarControlAccess(AControl.Parent).RepaintBarEx(False);
        end;
      end;
  finally
    FRecalculatingOnFilterChanged := False;
  end;
end;

procedure TdxCustomRibbonGalleryItem.GalleryChanged;

  procedure ResetControlCachedValues;
  var
    I: Integer;
  begin
    for I := 0 to LinkCount - 1 do
      if Links[I].Control <> nil then
        TdxRibbonGalleryControl(Links[I].Control).Changed;
  end;

begin
  FFirstVisibleGroupItem := nil;
  ResetControlCachedValues;
  UpdateEx;
end;

procedure TdxCustomRibbonGalleryItem.ItemSelectionModeChanged(Sender: TObject; DeselectAll: Boolean);
var
  I, J: Integer;
begin
  if not IsLoading then
  begin
    LockGroupItemClickEvents(True);
    try
      if GalleryOptions.ItemSelectionMode = gsmSingle then
        SelectedGroupItem := nil
      else
        if DeselectAll then
          for I := 0 to GalleryGroups.Count - 1 do
            for J := 0 to GalleryGroups[I].Items.Count - 1 do
              GalleryGroups[I].Items[J].Selected := False;
    finally
      LockGroupItemClickEvents(False);
    end;
  end;
end;

procedure TdxCustomRibbonGalleryItem.MinColumnCountChanged(Sender: TObject);
begin
  GalleryOptions.ColumnCount := Max(GalleryOptions.MinColumnCount, GalleryOptions.ColumnCount);
  OptionsChanged(Self);
end;

procedure TdxCustomRibbonGalleryItem.OptionsChanged(Sender: TObject);
begin
  GalleryChanged;
end;

function TdxCustomRibbonGalleryItem.GetActionLinkClass: TdxBarItemActionLinkClass;
begin
  Result := TdxCustomRibbonGalleryItemActionLink;
end;

procedure TdxCustomRibbonGalleryItem.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  inherited GetChildren(Proc, Root);
  for I := 0 to GalleryGroups.Count - 1 do
    Proc(GalleryGroups[I]);
end;

function TdxCustomRibbonGalleryItem.GetItemLinksClass: TdxBarItemLinksClass;
begin
  Result := TdxRibbonGalleryItemBarItemLinks;
end;

function TdxCustomRibbonGalleryItem.GetErrorCanPlaceText: string;
begin
  Result := cxGetResourceString(@dxSBAR_CANTPLACERIBBONGALLERY);
end;

function TdxCustomRibbonGalleryItem.GetFilterCaption: string;
var
  I: Integer;
begin
  if GalleryFilter.Caption <> '' then
    Result := GalleryFilter.Caption
  else
  begin
    Result := '';
    if GalleryFilter.Categories.Count = 0 then
    begin
      for I := 0 to GalleryGroups.Count - 1 do
        if IsGroupVisible(I) then
        begin
          if Result <> '' then
            Result := Result + FilterCaptionDelimiter;
          Result := Result + GalleryGroups[I].Header.Caption;
        end
    end
    else
      if GalleryFilter.ActiveCategoryIndex <> -1 then
        Result := GalleryFilter.Categories[GalleryFilter.ActiveCategoryIndex].Caption;
    if Result = '' then
      Result := cxGetResourceString(@dxSBAR_GALLERYEMPTYFILTERCAPTION);
  end;
end;

function TdxCustomRibbonGalleryItem.GetImages: TCustomImageList;
begin
  Result := GalleryOptions.Images;
  if Result = nil then
    Result := BarManager.ImageOptions.Images;
end;

class function TdxCustomRibbonGalleryItem.GetNewCaption: string;
begin
  Result := cxGetResourceString(@dxSBAR_NEWRIBBONGALLERYITEMCAPTION);
end;

procedure TdxCustomRibbonGalleryItem.GroupVisibleChanged;
begin
  FilterChanged;
end;

function TdxCustomRibbonGalleryItem.InternalCanMergeWith(AItem: TdxBarItem): Boolean;
begin
  Result := False;
end;

function TdxCustomRibbonGalleryItem.IsFilterVisible: Boolean;

  function HasVisibleGroups: Boolean;
  var
    I: Integer;
  begin
    for I := 0 to GalleryGroups.Count - 1 do
      if IsGroupVisible(I, True) then
        Exit(True);
    Result := False;
  end;

begin
  Result := GalleryFilter.Visible and ((GalleryFilter.Categories.Count > 0) or HasVisibleGroups);
end;

procedure TdxCustomRibbonGalleryItem.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  GalleryInRibbonOptions.ChangeScale(M, D);
  GalleryInMenuOptions.ChangeScale(M, D);
  GalleryOptions.ChangeScale(M, D);
  GalleryGroups.ChangeScale(M, D);
end;

procedure TdxCustomRibbonGalleryItem.Loaded;

  function GetSelectedGroupItem: TdxRibbonGalleryGroupItem;
  var
    I, J: Integer;
  begin
    Result := nil;
    for I := 0 to GalleryGroups.Count - 1 do
      for J := 0 to GalleryGroups[I].Items.Count - 1 do
        if GalleryGroups[I].Items[J].LoadedSelected then
        begin
          Result := GalleryGroups[I].Items[J];
          Result.LoadedSelected := False;
          Break;
        end;
  end;

begin
  inherited Loaded;
  if GalleryOptions.ItemSelectionMode = gsmSingle then
  begin
    LockGroupItemClickEvents(True);
    try
      SelectedGroupItem := GetSelectedGroupItem;
    finally
      LockGroupItemClickEvents(False);
    end;
  end;
  if GalleryOptions.ItemHintSource = ghsItemHint then
  begin
    if not GalleryOptions.ShowItemHint and (ScreenTip <> nil) then
      GalleryOptions.ItemHintSource := ghsGalleryScreenTip
    else
      GalleryOptions.ItemHintSource := ghsItemHint;
  end;
  GalleryFilter.Loaded;
end;

procedure TdxCustomRibbonGalleryItem.LockFilterChanged(ALock: Boolean);
begin
  if ALock then
    Inc(FFilterChangedLockCount)
  else
  begin
    Dec(FFilterChangedLockCount);
    if FFilterChangedLockCount = 0 then
      FilterChanged;
  end;
end;

procedure TdxCustomRibbonGalleryItem.LockGroupItemClickEvents(ALock: Boolean);
begin
  if ALock then
    Inc(FLockGroupItemClickEventsCount)
  else
    if FLockGroupItemClickEventsCount > 0 then
      Dec(FLockGroupItemClickEventsCount);
end;

function TdxCustomRibbonGalleryItem.MoveGroupItem(AGroupItem: TdxRibbonGalleryGroupItem;
  ATargetGroup: TdxRibbonGalleryGroup; AIndex: Integer): TdxRibbonGalleryGroupItem;
begin
  if AGroupItem.Group <> ATargetGroup then
    AGroupItem.Collection := ATargetGroup.Items;
  AGroupItem.Index := AIndex;
  Result := AGroupItem;
end;

procedure TdxCustomRibbonGalleryItem.ShowGroupItem(AGroupItem: TdxRibbonGalleryGroupItem);
var
  I: Integer;
begin
  for I := 0 to LinkCount - 1 do
    if (Links[I].Control <> nil) and (Links[I].Control.ViewInfo is TdxInRibbonGalleryControlViewInfo) then
      TdxInRibbonGalleryControlViewInfo(Links[I].Control.ViewInfo).ShowGroupItem(AGroupItem);
end;

procedure TdxCustomRibbonGalleryItem.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = DropDownGallery then
      FDropDownGallery := nil
  end;
end;

procedure TdxCustomRibbonGalleryItem.RemoveGroupItem(AItem: TdxRibbonGalleryGroupItem);
var
  I: Integer;
begin
  if FSelectedGroupItem = AItem then
  begin
    FSelectedGroupItem := nil;
    FFirstVisibleGroupItem := nil;
  end;
  for I := 0 to LinkCount - 1 do
    if Links[I].Control <> nil then
      TdxRibbonGalleryControl(Links[I].Control).ViewInfo.RemoveGroupItem(AItem);
end;

procedure TdxCustomRibbonGalleryItem.UpdateEx(AParentKinds: TdxBarKinds = dxBarKindAny);
var
  I: Integer;
begin
  Exclude(AParentKinds, bkSubMenu);
  inherited UpdateEx(AParentKinds);
  for I := 0 to LinkCount - 1 do
    if Links[I].Control <> nil then
      with Links[I].Control do
        if Parent.Kind = bkSubMenu then
          Links[I].Control.Repaint;
end;

function TdxCustomRibbonGalleryItem.GetActionLink: TdxCustomRibbonGalleryItemActionLink;
begin
  Result := TdxCustomRibbonGalleryItemActionLink(inherited ActionLink);
end;

procedure TdxCustomRibbonGalleryItem.ReadGalleryGroups(AReader: TReader);
begin
  GalleryGroups.ReadOldCollection(AReader);
end;

procedure TdxCustomRibbonGalleryItem.SetDropDownGallery(Value: TdxRibbonDropDownGallery);
begin
  if Value <> FDropDownGallery then
  begin
    cxRemoveFreeNotification(Self, FDropDownGallery);
    FDropDownGallery := Value;
    cxAddFreeNotification(Self, FDropDownGallery);
  end;
end;

procedure TdxCustomRibbonGalleryItem.SetGalleryFilter(Value: TdxRibbonGalleryFilter);
begin
  FGalleryFilter.Assign(Value);
end;

procedure TdxCustomRibbonGalleryItem.SetGalleryGroups(Value: TdxRibbonGalleryGroups);
begin
  FGalleryGroups.Assign(Value);
end;

procedure TdxCustomRibbonGalleryItem.SetGalleryInRibbonOptions(Value: TdxInRibbonGalleryOptions);
begin
  FGalleryInRibbonOptions.Assign(Value);
end;

procedure TdxCustomRibbonGalleryItem.SetGalleryInMenuOptions(Value: TdxInMenuGalleryOptions);
begin
  FGalleryInMenuOptions.Assign(Value);
end;

procedure TdxCustomRibbonGalleryItem.SetGalleryOptions(Value: TdxCustomRibbonGalleryOptions);
begin
  FGalleryOptions.Assign(Value);
end;

procedure TdxCustomRibbonGalleryItem.SetSelectedGroupItem(Value: TdxRibbonGalleryGroupItem);
begin
  if (GalleryOptions.ItemSelectionMode = gsmSingle) and (FSelectedGroupItem <> Value) then
  begin
    FSelectedGroupItem := Value;
    FFirstVisibleGroupItem := Value;
    if FSelectedGroupItem <> nil then
      FSelectedGroupItem.DoClick;
    DoGroupItemClick(FSelectedGroupItem);
    Update;
  end;
end;

{ TdxRibbonGalleryItem }

procedure TdxRibbonGalleryItem.DoFilterChanged;
begin
  if Assigned(OnFilterChanged) then
    OnFilterChanged(Self);
end;

procedure TdxRibbonGalleryItem.DoGroupItemClick(AItem: TdxRibbonGalleryGroupItem);
begin
  inherited DoGroupItemClick(AItem);
  if Assigned(FOnGroupItemClick) and not AreGroupItemClickEventsLocked then
    FOnGroupItemClick(Self, AItem);
end;

procedure TdxRibbonGalleryItem.DoInitFilterMenu(AItemLinks: TdxBarItemLinks);
begin
  if Assigned(OnInitFilterMenu) then
    OnInitFilterMenu(Self, AItemLinks);
end;

function TdxRibbonGalleryItem.GetGalleryOptionsClass: TCustomdxRibbonGalleryOptionsClass;
begin
  Result := TdxRibbonGalleryOptions;
end;

procedure TdxRibbonGalleryItem.UpdateActionLink;
var
  I, J: Integer;
begin
  inherited UpdateActionLink;
  for I := 0 to GalleryGroups.Count - 1 do
    for J := 0 to GalleryGroups[I].Items.Count - 1 do
      GalleryGroups[I].Items[J].UpdateActionLink;
end;

function TdxRibbonGalleryItem.GetGalleryOptions: TdxRibbonGalleryOptions;
begin
  Result := inherited GalleryOptions as TdxRibbonGalleryOptions;
end;

function TdxRibbonGalleryItem.GetAllowedCustomizationActions: TdxGalleryCustomizationActions;
begin
  Result := [gcaChangeGroupCaption, gcaChangeItemCaption];
end;

function TdxRibbonGalleryItem.GetGroups: IdxGalleryGroups;
begin
  Result := GalleryGroups;
end;

procedure TdxRibbonGalleryItem.SetGalleryOptions(Value: TdxRibbonGalleryOptions);
begin
  GalleryOptions.Assign(Value);
end;

{ TdxRibbonGalleryController }

constructor TdxRibbonGalleryController.Create(
  AOwner: TdxRibbonGalleryControl);
begin
  inherited Create;
  FOwner := AOwner;

  FGroupItemHotTrackEnabled := True;
end;

procedure TdxRibbonGalleryController.CancelHint;
begin
  FOwner.UpdateHint(nil, cxEmptyRect);
end;

function TdxRibbonGalleryController.CheckEnabled(
  AGroupItem: TdxRibbonGalleryGroupItem): TdxRibbonGalleryGroupItem;
begin
  if (AGroupItem <> nil) and not AGroupItem.Enabled then
    Result := nil
  else
    Result := AGroupItem;
end;

function TdxRibbonGalleryController.GetGroupItem(AGroupIndex, AIndex: Integer): TdxRibbonGalleryGroupItem;
var
  AGroup: TdxRibbonGalleryGroup;
begin
  Result := nil;
  if InRange(AGroupIndex, 0, GroupCount - 1) then
  begin
    AGroup := FOwner.GetGroups[AGroupIndex];
    if InRange(AIndex, 0, AGroup.Items.Count - 1) then
      Result := AGroup.Items[AIndex];
  end;
end;

procedure TdxRibbonGalleryController.HotTrackItem(
  AItem: TdxRibbonGalleryGroupItem);
begin
  FKeyboardHotGroupItem := AItem;
  SetHotGroupItem(FKeyboardHotGroupItem);
end;

procedure TdxRibbonGalleryController.SetHintItem(AItem: TdxRibbonGalleryGroupItem);
var
  R: TRect;
  AGroupItemViewInfo: TdxRibbonGalleryGroupItemViewInfo;
begin
  if FHintItem <> AItem then
  begin
    FHintItem := AItem;
    AGroupItemViewInfo := ViewInfo.GetGroupItemViewInfo(FHintItem);
    if AGroupItemViewInfo <> nil then
      R := AGroupItemViewInfo.Bounds
    else
      R := cxEmptyRect;
    FOwner.UpdateHint(AItem, R);
  end;
end;

procedure TdxRibbonGalleryController.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

  function GetIsItemPullHighlighting(AGroupItem: TdxRibbonGalleryGroupItem): Boolean;
  begin
    Result := AGroupItem.Group.Options.ItemPullHighlightingMode <> iphmNone;
  end;

  procedure GroupItemMouseDown(AGroupItem: TdxRibbonGalleryGroupItem;
    X, Y: Integer);
  var
    AGroupItemViewInfo: TdxRibbonGalleryGroupItemViewInfo;
  begin
    FMouseDownGroupItem := AGroupItem;
    if AGroupItem = nil then Exit;
    AGroupItemViewInfo := ViewInfo.GetGroupItemViewInfo(AGroupItem);
    AGroupItem.MouseDown(Button, Shift, X - AGroupItemViewInfo.Bounds.Left,
      Y - AGroupItemViewInfo.Bounds.Top);
  end;

var
  AGroupItem: TdxRibbonGalleryGroupItem;
begin
  if Enabled then
  begin
    CancelHint;
    AGroupItem := CheckEnabled(ViewInfo.GetGroupItem(X, Y));
    GroupItemMouseDown(AGroupItem, X, Y);
    FGroupItemHotTrackEnabled := AGroupItem <> nil;
    if FGroupItemHotTrackEnabled and not GetIsItemPullHighlighting(AGroupItem) then
      GetViewInfo.SetDownedGroupItem(AGroupItem);
  end;
end;

procedure TdxRibbonGalleryController.MouseLeave;
begin
  SetHotGroupItem(nil);
  SetHintItem(nil);
end;

procedure TdxRibbonGalleryController.MouseMove(Shift: TShiftState;
  X, Y: Integer);
var
  AGroupItem: TdxRibbonGalleryGroupItem;
begin
  if Enabled then
  begin
    AGroupItem := CheckEnabled(ViewInfo.GetGroupItem(X, Y));
    GroupItemMouseMove(AGroupItem, Shift, X, Y);
    ProcessHotTrack(AGroupItem);
  end;
end;

procedure TdxRibbonGalleryController.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

  procedure GroupItemMouseUp(AGroupItem: TdxRibbonGalleryGroupItem;
    X, Y: Integer);
  var
    AGroupItemViewInfo: TdxRibbonGalleryGroupItemViewInfo;
  begin
    if AGroupItem = nil then Exit;
    AGroupItemViewInfo := ViewInfo.GetGroupItemViewInfo(AGroupItem);
    AGroupItem.MouseUp(Button, Shift, X - AGroupItemViewInfo.Bounds.Left,
      Y - AGroupItemViewInfo.Bounds.Top);
  end;

var
  AViewInfo: TdxRibbonGalleryControlViewInfo;
begin
  if Enabled then
  begin
    GroupItemMouseUp(FMouseDownGroupItem, X, Y);
    if FGroupItemHotTrackEnabled then
    begin
      AViewInfo := ViewInfo;
      if AViewInfo.IsGroupItemAtThisPlace(X, Y) then
        AViewInfo.SetDownedGroupItem(nil);
    end;
    FGroupItemHotTrackEnabled := True;
  end;
end;

function TdxRibbonGalleryController.GetEnabled: Boolean;
begin
  Result := GalleryItem.Enabled;
end;

function TdxRibbonGalleryController.GetFirstGroupItem: TdxRibbonGalleryGroupItem;
var
  AGroupIndex: Integer;
begin
  AGroupIndex := ViewInfo.GetVisibleNotEmptyGroupIndex(0, True);
  if AGroupIndex <> -1 then
    Result := FOwner.GetGroups[AGroupIndex].Items[0]
  else
    Result := nil;
end;

function TdxRibbonGalleryController.GetGalleryItem: TdxCustomRibbonGalleryItem;
begin
  Result := TdxCustomRibbonGalleryItem(FOwner.Item);
end;

function TdxRibbonGalleryController.GetGroupCount: Integer;
begin
  Result := FOwner.GetGroups.Count;
end;

function TdxRibbonGalleryController.GetKeyboardHotGroupItem: TdxRibbonGalleryGroupItem;
var
  ADestroyAfterUse: Boolean;
  AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
begin
  Result := nil;
  if (FKeyboardHotGroupItem <> nil) then
  begin
    AGroupViewInfo := GetGroupViewInfo(FOwner.GetGroups,
      ViewInfo, FKeyboardHotGroupItem.Group.Index, ADestroyAfterUse);
    try
      if (AGroupViewInfo <> nil) then
        Result := FKeyboardHotGroupItem;
    finally
      if ADestroyAfterUse then
        AGroupViewInfo.Free;
    end;
  end;
end;

function TdxRibbonGalleryController.GetViewInfo: TdxRibbonGalleryControlViewInfo;
begin
  Result := FOwner.ViewInfo;
end;

procedure TdxRibbonGalleryController.GroupItemMouseMove(AGroupItem: TdxRibbonGalleryGroupItem;
  Shift: TShiftState; X, Y: Integer);
var
  AGroupItemViewInfo: TdxRibbonGalleryGroupItemViewInfo;
begin
  if AGroupItem = nil then Exit;
  AGroupItemViewInfo := ViewInfo.GetGroupItemViewInfo(AGroupItem);
  AGroupItem.MouseMove(Shift, X - AGroupItemViewInfo.Bounds.Left,
    Y - AGroupItemViewInfo.Bounds.Top);
end;

procedure TdxRibbonGalleryController.ProcessHotTrack(
  AGroupItem: TdxRibbonGalleryGroupItem);
begin
  if FGroupItemHotTrackEnabled then
  begin
    ViewInfo.DontDisplayHotTrackedGroupItem :=
      ViewInfo.DontDisplayHotTrackedGroupItem + 1;
    try
      SetHintItem(AGroupItem);
      if (AGroupItem <> nil) or
        not (FKeyboardHotGroupItem <> nil) or
        not FLastCommandFromKeyboard then
      begin
        FLastCommandFromKeyboard := False;
        SetHotGroupItem(AGroupItem);
      end;
    finally
      ViewInfo.DontDisplayHotTrackedGroupItem :=
        ViewInfo.DontDisplayHotTrackedGroupItem - 1;
    end;
  end;
end;

procedure TdxRibbonGalleryController.SetHotGroupItem(
  const Value: TdxRibbonGalleryGroupItem);
begin
  if Value <> ViewInfo.HotGroupItem then
  begin
    ViewInfo.HotGroupItem := Value;
    if Value <> nil then
      FKeyboardHotGroupItem := Value;
    UnsetDownedFromGroupItem(Value);
  end;
end;

procedure TdxRibbonGalleryController.UnsetDownedFromGroupItem(
  AGroupItem: TdxRibbonGalleryGroupItem);
begin
  if (ViewInfo.DownedGroupItem <> AGroupItem) and
    (ViewInfo.DownedGroupItem <> nil) then
  begin
    ViewInfo.SetDownedGroupItem(nil);
  end;
end;

{ TdxRibbonOnSubmenuGalleryController }

destructor TdxRibbonOnSubmenuGalleryController.Destroy;
begin
  FreeAndNil(FFilterMenuLinksOwner);
  inherited;
end;

procedure TdxRibbonOnSubmenuGalleryController.Navigation(
  ADirection: TcxAccessibilityNavigationDirection);

type
  TXRange = record
    Left: Integer;
    Right: Integer;
  end;

  procedure TransferToAnotherGroup(var AGroupIndex: Integer;
    const AColumnRange: TXRange; ADownDirection: Boolean;
    var ARow, AColumn: Integer);
  var
    AColumnCountInRow: Integer;
    AColumnFind: Boolean;
    ACurrentColumnIndex, ACurrentRowIndex: Integer;
    AColumnLeft, AColumnRight: Integer;
    ACurrentColumnIntersected, APreviousColumnIntersected: Boolean;
    ADestroyAfterUse: Boolean;
    AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
    ANextRow, ANextGroup: Boolean;
  begin
    if AGroupIndex >= GroupCount then
      if TdxBarItemLinksAccess(FOwner.Parent.ItemLinks).First <> nil then
        Exit
      else
        AGroupIndex := 0;
    if AGroupIndex < 0 then
      if TdxBarItemLinksAccess(FOwner.Parent.ItemLinks).Last <> nil then
        Exit
      else
        AGroupIndex := GroupCount - 1;

    AColumnFind := False;
    ACurrentColumnIndex := 0;
    ACurrentRowIndex := 0;
    if GalleryItem.IsGroupVisible(AGroupIndex) and
      (FOwner.GetGroups[AGroupIndex].Items.Count > 0) then
    begin
      AGroupViewInfo := GetGroupViewInfo(FOwner.GetGroups, ViewInfo,
        AGroupIndex, ADestroyAfterUse);
      {if ADestroyAfterUse then
        AGroupViewInfo.SetBounds(ViewInfo.GalleryBounds);}

      if not ADownDirection then
        ACurrentRowIndex := AGroupViewInfo.GetRowCount(GetGalleryWidth) - 1;

      ANextGroup := False;
      repeat
        ANextRow := False;
        AColumnCountInRow := AGroupViewInfo.GetColumnCountInRow(ACurrentRowIndex,
          GetGalleryWidth);
        ACurrentColumnIndex := 0;
        APreviousColumnIntersected := False;
        repeat
          AColumnLeft := AGroupViewInfo.GetColumnLeft(ACurrentColumnIndex,
            ViewInfo.GalleryBounds.Left);
          AColumnRight := AColumnLeft + AGroupViewInfo.ItemSize.cx;
          ACurrentColumnIntersected := AreLinesIntersectedStrictly(AColumnLeft, AColumnRight,
            AColumnRange.Left, AColumnRange.Right);
          if ADownDirection then
            AColumnFind := ACurrentColumnIntersected
          else
          begin
            AColumnFind := APreviousColumnIntersected and
              (not ACurrentColumnIntersected);
            if AColumnFind then
              Dec(ACurrentColumnIndex);
            if (ACurrentColumnIndex = AColumnCountInRow - 1) and
              ACurrentColumnIntersected then
              AColumnFind := True;
            APreviousColumnIntersected := ACurrentColumnIntersected;
          end;
          if not AColumnFind then
          begin
            ANextRow := ACurrentColumnIndex = AColumnCountInRow - 1;
            Inc(ACurrentColumnIndex);
          end;
        until AColumnFind or ANextRow;

        if not AColumnFind then
        begin
          if ADownDirection then
          begin
            Inc(ACurrentRowIndex);
            ANextGroup := ACurrentRowIndex >
              AGroupViewInfo.GetRowCount(GetGalleryWidth) - 1;
          end
          else
          begin
            Dec(ACurrentRowIndex);
            ANextGroup := ACurrentRowIndex < 0;
          end;
        end;
      until AColumnFind or ANextGroup;

      if ADestroyAfterUse then
        AGroupViewInfo.Free;
    end;

    if AColumnFind then
    begin
      AColumn := ACurrentColumnIndex;
      ARow := ACurrentRowIndex;
    end
    else
    begin
      if ADownDirection then
        Inc(AGroupIndex)
      else
        Dec(AGroupIndex);
      TransferToAnotherGroup(AGroupIndex, AColumnRange, ADownDirection, ARow,
        AColumn);
    end;
  end;

  function GetColumnRange(AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
    AColumn: Integer): TXRange;
  begin
    Result.Left := AGroupViewInfo.GetColumnLeft(AColumn,
      ViewInfo.GalleryBounds.Left);
    Result.Right := Result.Left + AGroupViewInfo.ItemSize.cx;
  end;

  procedure DownDirection(AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
    var AGroupIndex: Integer; var ARow, AColumn: Integer);
  var
    ARange: TXRange;
  begin
    if (ARow < AGroupViewInfo.GetRowCount(GetGalleryWidth) - 1) and
      (AColumn <= AGroupViewInfo.GetColumnCountInRow(ARow + 1,
      GetGalleryWidth) - 1) then
      Inc(ARow)
    else
    begin
      ARange := GetColumnRange(AGroupViewInfo, AColumn);
      Inc(AGroupIndex);
      TransferToAnotherGroup(AGroupIndex, ARange, True, ARow, AColumn);
    end;
  end;

  procedure LeftDirection(AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
    var ARow, AColumn: Integer);
  begin
    if AColumn > 0 then
      Dec(AColumn)
    else
      AColumn := AGroupViewInfo.GetColumnCountInRow(ARow,
        GetGalleryWidth) - 1;
  end;

  procedure RightDirection(AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
    var ARow, AColumn: Integer);
  begin
    if AColumn < AGroupViewInfo.GetColumnCountInRow(ARow,
      GetGalleryWidth) - 1 then
      Inc(AColumn)
    else
      AColumn := 0;
  end;

  procedure UpDirection(AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
    var AGroupIndex: Integer; var ARow, AColumn: Integer);
  var
    ARange: TXRange;
  begin
    if ARow > 0 then
      Dec(ARow)
    else
    begin
      ARange := GetColumnRange(AGroupViewInfo, AColumn);
      Dec(AGroupIndex);
      TransferToAnotherGroup(AGroupIndex, ARange, False, ARow, AColumn);
    end;
  end;

  function Navigate(AHotGroupItem: TdxRibbonGalleryGroupItem): TdxRibbonGalleryGroupItem;
  var
    ADestroyAfterUse: Boolean;
    AGroupIndex, AGroupItemColumn, AGroupItemRow: Integer;
    AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
  begin
    AGroupViewInfo := GetGroupViewInfo(FOwner.GetGroups, ViewInfo,
      AHotGroupItem.Group.Index, ADestroyAfterUse);
    try
      AGroupIndex := AHotGroupItem.Group.Index;
      AGroupItemRow := AGroupViewInfo.GetItemRow(AHotGroupItem.Index,
        GetGalleryWidth);
      AGroupItemColumn := AGroupViewInfo.GetItemColumn(
        AHotGroupItem.Index, GetGalleryWidth);
      case ADirection of
        andLeft: LeftDirection(AGroupViewInfo, AGroupItemRow, AGroupItemColumn);
        andUp: UpDirection(AGroupViewInfo, AGroupIndex, AGroupItemRow, AGroupItemColumn);
        andRight: RightDirection(AGroupViewInfo, AGroupItemRow, AGroupItemColumn);
        andDown: DownDirection(AGroupViewInfo, AGroupIndex, AGroupItemRow, AGroupItemColumn);
      end;

      if not InRange(AGroupIndex, 0, GroupCount - 1) then
      begin
        if AGroupIndex < 0 then
          BarNavigationController.ChangeSelectedObject(False, TdxBarItemLinksAccess(FOwner.Parent.ItemLinks).Last.Control.IAccessibilityHelper)
        else
          BarNavigationController.ChangeSelectedObject(False, TdxBarItemLinksAccess(FOwner.Parent.ItemLinks).First.Control.IAccessibilityHelper);
        Result := nil;
      end
      else
      begin
        if AHotGroupItem.Group.Index <> AGroupIndex then
        begin
          if ADestroyAfterUse then
            FreeAndNil(AGroupViewInfo);
          AGroupViewInfo := GetGroupViewInfo(FOwner.GetGroups, ViewInfo,
            AGroupIndex, ADestroyAfterUse);
        end;
        Result := GetGroupItem(AGroupIndex, AGroupViewInfo.GetItemIndex(
          AGroupItemRow, AGroupItemColumn, GetGalleryWidth));
        if not Result.Enabled then
          Result := Navigate(Result);
      end;
    finally
      if ADestroyAfterUse then
        FreeAndNil(AGroupViewInfo);
    end;
  end;

var
  AHotGroupItem: TdxRibbonGalleryGroupItem;
begin
  AHotGroupItem := KeyboardHotGroupItem;
  if AHotGroupItem <> nil then
    AHotGroupItem := Navigate(AHotGroupItem)
  else
    AHotGroupItem := GetFirstGroupItem;
  SetHotGroupItem(AHotGroupItem);
  FLastCommandFromKeyboard := True;
end;

procedure TdxRibbonOnSubmenuGalleryController.HotTrackFirstGroupItem;
begin
  HotTrackItem(GetFirstGroupItem);
end;

procedure TdxRibbonOnSubmenuGalleryController.HotTrackLastGroupItem;
begin
  HotTrackItem(GetLastGroupItem);
end;

function TdxRibbonOnSubmenuGalleryController.IsFilterMenuShowed: Boolean;
begin
  Result := (FFilterMenuLinksOwner <> nil) and (FFilterMenuLinksOwner.ItemLinks.BarControl <> nil);
end;

procedure TdxRibbonOnSubmenuGalleryController.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if FDontShowFilterMenuOnMouseDown then
    FDontShowFilterMenuOnMouseDown := False
  else
    if (Button = mbLeft) and GalleryItem.IsFilterVisible and
      ViewInfo.IsPtInFilterBandHotTrackArea(Point(X, Y)) then
      begin
        ShowFilterMenu;
        GroupItemHotTrackEnabled := True;
      end;
  CheckFilterMenuHotTrack;
end;

procedure TdxRibbonOnSubmenuGalleryController.MouseLeave;
begin
  inherited MouseLeave;
  if GalleryItem.IsFilterVisible then
    ViewInfo.SetFilterBandHotTrack(False);
end;

procedure TdxRibbonOnSubmenuGalleryController.MouseMove(Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if GalleryItem.IsFilterVisible then
    CheckFilterMenuHotTrack;
end;

procedure TdxRibbonOnSubmenuGalleryController.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  CheckFilterMenuHotTrack;
end;

procedure TdxRibbonOnSubmenuGalleryController.PageDown;
var
  AHotTrackedGroupItemBottom, AHotTrackedGroupItemTop: Integer;
  ACurrentGroupItemBottom, ACurrentGroupItemTop, AWidth: Integer;
  ACurrentGroupItem, AHotGroupItem, ARequiredGroupItem: TdxRibbonGalleryGroupItem;
  AGroupIndex, AGroupRowIndex: Integer;
  AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
  ADestroyAfterUse, AItemIsObtained: Boolean;
  AViewInfo: TdxRibbonGalleryControlViewInfo;
begin
  AViewInfo := ViewInfo;
  AHotGroupItem := AViewInfo.HotGroupItem;
  if AHotGroupItem = nil then
    if GalleryItem.SelectedGroupItem <> nil then
      AHotGroupItem := GalleryItem.SelectedGroupItem
    else
      AHotGroupItem := GetFirstGroupItem;
  if AHotGroupItem = nil then
    Exit;

  AWidth := AViewInfo.GallerySize.cx;
  ViewInfo.GroupItemYRange(AHotGroupItem,
    AHotTrackedGroupItemTop, AHotTrackedGroupItemBottom);
  AGroupIndex := AHotGroupItem.Group.Index;
  AGroupViewInfo := GetGroupViewInfo(FOwner.GetGroups, AViewInfo, AGroupIndex,
    ADestroyAfterUse);
  try
    AGroupRowIndex := AGroupViewInfo.GetItemRow(AHotGroupItem.Index, AWidth);
    ACurrentGroupItem := GetGroupItem(AGroupIndex,
      AGroupViewInfo.GetItemIndex(AGroupRowIndex,
      AGroupViewInfo.GetColumnCountInRow(AGroupRowIndex, AWidth) - 1, AWidth));
  finally
    if ADestroyAfterUse then
      AGroupViewInfo.Free;
  end;
  ARequiredGroupItem := ACurrentGroupItem;
  AItemIsObtained := False;
  AGroupIndex := ACurrentGroupItem.Group.Index;
  repeat
    AGroupViewInfo := GetGroupViewInfo(FOwner.GetGroups, AViewInfo,
      AGroupIndex, ADestroyAfterUse);
    if AGroupViewInfo <> nil then
      try
        repeat
          ACurrentGroupItem := GetGroupItem(AGroupIndex,
            AGroupViewInfo.GetItemIndex(
              AGroupRowIndex, AGroupViewInfo.GetColumnCountInRow(AGroupRowIndex,
                AWidth) - 1, AWidth));
          ViewInfo.GroupItemYRange(ACurrentGroupItem,
            ACurrentGroupItemTop, ACurrentGroupItemBottom);
          if (ACurrentGroupItemBottom - AHotTrackedGroupItemBottom) <
            AViewInfo.GallerySize.cy then
            ARequiredGroupItem := ACurrentGroupItem
          else
            AItemIsObtained := True;
          Inc(AGroupRowIndex);
        until (AGroupRowIndex > AGroupViewInfo.GetRowCount(AWidth) - 1) or
          AItemIsObtained;
      finally
        if ADestroyAfterUse then
          AGroupViewInfo.Free;
      end;
    Inc(AGroupIndex);
    AGroupRowIndex := 0;
  until AItemIsObtained or (AGroupIndex > GroupCount - 1);
  if AItemIsObtained then
    SetHotGroupItem(ARequiredGroupItem)
  else
    HotTrackLastGroupItem;
end;

procedure TdxRibbonOnSubmenuGalleryController.PageUp;
var
  AHotTrackedGroupItemBottom, AHotTrackedGroupItemTop: Integer;
  ACurrentGroupItemBottom, ACurrentGroupItemTop: Integer;
  ACurrentGroupItem, ARequiredGroupItem, AHotGroupItem: TdxRibbonGalleryGroupItem;
  AWidth, AGroupIndex, AGroupRowIndex: Integer;
  AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
  ADestroyAfterUse, AItemIsObtained: Boolean;
  AViewInfo: TdxRibbonGalleryControlViewInfo;
begin
  AViewInfo := ViewInfo;
  AHotGroupItem := AViewInfo.HotGroupItem;
  if AHotGroupItem = nil then
    if GalleryItem.SelectedGroupItem <> nil then
      AHotGroupItem := GalleryItem.SelectedGroupItem
    else
      AHotGroupItem := GetFirstGroupItem;
  if AHotGroupItem = nil then
    Exit;

  AWidth := AViewInfo.GallerySize.cx;
  ViewInfo.GroupItemYRange(AHotGroupItem,
    AHotTrackedGroupItemTop, AHotTrackedGroupItemBottom);
  AGroupIndex := AHotGroupItem.Group.Index;
  AGroupViewInfo := GetGroupViewInfo(FOwner.GetGroups, AViewInfo, AGroupIndex, ADestroyAfterUse);
  try
    AGroupRowIndex := AGroupViewInfo.GetItemRow(AHotGroupItem.Index, AWidth);
    ACurrentGroupItem := GetGroupItem(AGroupIndex,
      AGroupViewInfo.GetItemIndex(AGroupRowIndex, 0, AWidth));
  finally
    if ADestroyAfterUse then
      AGroupViewInfo.Free;
  end;
  ARequiredGroupItem := ACurrentGroupItem;
  AItemIsObtained := False;
  repeat
    AGroupViewInfo := GetGroupViewInfo(FOwner.GetGroups, ViewInfo,
      AGroupIndex, ADestroyAfterUse);
    if AGroupViewInfo <> nil then
      try
        if AGroupRowIndex = -1 then
          AGroupRowIndex := AGroupViewInfo.GetRowCount(AWidth) - 1;
        repeat
          ACurrentGroupItem := GetGroupItem(AGroupIndex,
            AGroupViewInfo.GetItemIndex(AGroupRowIndex, 0, AWidth));
          ViewInfo.GroupItemYRange(ACurrentGroupItem,
            ACurrentGroupItemTop, ACurrentGroupItemBottom);
          if (AHotTrackedGroupItemTop - ACurrentGroupItemTop) <
            AViewInfo.GallerySize.cy then
            ARequiredGroupItem := ACurrentGroupItem
          else
            AItemIsObtained := True;
          Dec(AGroupRowIndex);
        until (AGroupRowIndex < 0) or AItemIsObtained;
      finally
        if ADestroyAfterUse then
          AGroupViewInfo.Free;
      end;
    Dec(AGroupIndex);
    AGroupRowIndex := -1;
  until AItemIsObtained or (AGroupIndex < 0);
  if AItemIsObtained then
  begin
    AViewInfo.DontDisplayGroupHeaderWhenHotTrackingGroupItem :=
      AViewInfo.DontDisplayGroupHeaderWhenHotTrackingGroupItem + 1;
    try
      SetHotGroupItem(ARequiredGroupItem)
    finally
      AViewInfo.DontDisplayGroupHeaderWhenHotTrackingGroupItem :=
        AViewInfo.DontDisplayGroupHeaderWhenHotTrackingGroupItem - 1;
    end;
  end
  else
    HotTrackFirstGroupItem;
end;

procedure TdxRibbonOnSubmenuGalleryController.Tabulation;
var
  AGroupIndex: Integer;
  AGroupItem: TdxRibbonGalleryGroupItem;
begin
  AGroupItem := KeyboardHotGroupItem;
  if AGroupItem <> nil then
  begin
    if GetKeyState(VK_SHIFT) and 128 <> 128 then
    begin
      if AGroupItem.Index < AGroupItem.Group.Items.Count - 1 then
        AGroupItem := AGroupItem.Group.Items[AGroupItem.Index + 1]
      else
      begin
        AGroupIndex := -1;
        if AGroupItem.Group.Index < FOwner.GetGroups.Count - 1 then
          AGroupIndex := ViewInfo.GetVisibleNotEmptyGroupIndex(
            AGroupItem.Group.Index + 1, True);
        if AGroupIndex = -1 then
          if TdxBarItemLinksAccess(FOwner.Parent.ItemLinks).First <> nil then
          begin
            BarNavigationController.ChangeSelectedObject(True,
              TdxBarItemLinksAccess(FOwner.Parent.ItemLinks).First.Control.IAccessibilityHelper);
            AGroupItem := nil;
          end
          else
            AGroupIndex := ViewInfo.GetVisibleNotEmptyGroupIndex(0, True);
        if AGroupIndex <> -1 then
          AGroupItem := FOwner.GetGroups[AGroupIndex].Items[0];
      end;
    end
    else
      if AGroupItem.Index > 0 then
        AGroupItem := AGroupItem.Group.Items[AGroupItem.Index - 1]
      else
      begin
        AGroupIndex := -1;
        if AGroupItem.Group.Index > 0 then
          AGroupIndex := ViewInfo.GetVisibleNotEmptyGroupIndex(
            AGroupItem.Group.Index - 1, False);
        if AGroupIndex = -1 then
          if TdxBarItemLinksAccess(FOwner.Parent.ItemLinks).Last <> nil then
          begin
            BarNavigationController.ChangeSelectedObject(True,
              TdxBarItemLinksAccess(FOwner.Parent.ItemLinks).Last.Control.IAccessibilityHelper);
            AGroupItem := nil;
          end
          else
            AGroupIndex := ViewInfo.GetVisibleNotEmptyGroupIndex(
              FOwner.GetGroups.Count - 1, False);
        if AGroupIndex <> -1 then
          AGroupItem := FOwner.GetGroups[AGroupIndex].Items[FOwner.GetGroups[AGroupIndex].Items.Count - 1];
      end;
  end
  else
    AGroupItem := GetFirstGroupItem;
  SetHotGroupItem(AGroupItem);
end;

procedure TdxRibbonOnSubmenuGalleryController.CheckFilterMenuHotTrack;
begin
  ViewInfo.SetFilterBandHotTrack((ActiveBarControl = FOwner.Parent) and not (ssLeft in KeyboardStateToShiftState) and
    ViewInfo.IsPtInFilterBandHotTrackArea(FOwner.Parent.ScreenToClient(GetMouseCursorPos)));
end;

procedure TdxRibbonOnSubmenuGalleryController.FilterMenuButtonClick(Sender: TObject);
begin
  if GalleryItem.GalleryFilter.Categories.Count = 0 then
    FilterMenuGroupButtonClick(Sender)
  else
    FilterMenuCategoryButtonClick(Sender);
end;

procedure TdxRibbonOnSubmenuGalleryController.FilterMenuCategoryButtonClick(Sender: TObject);
var
  AItem: TdxBarItem;
  ANewActiveCategoryIndex, I: Integer;
begin
  ANewActiveCategoryIndex := TdxBarButton(Sender).Tag;

  for I := 0 to FFilterMenuLinksOwner.ItemLinks.VisibleItemCount - 1 do
  begin
    AItem := FFilterMenuLinksOwner.ItemLinks.VisibleItems[I].Item;
    if IsFilterMenuInternalButton(AItem) and (AItem.Tag >= 0) then
      TdxBarButton(AItem).Down := AItem.Tag = ANewActiveCategoryIndex;
  end;

  if GalleryItem.GalleryFilter.ActiveCategoryIndex <> ANewActiveCategoryIndex then
  begin
    GalleryItem.GalleryFilter.ActiveCategoryIndex := ANewActiveCategoryIndex;
    GalleryItem.DoFilterChanged;
    ViewInfo.RepaintFilterBand;
  end;

  HideFilterMenu;
end;

procedure TdxRibbonOnSubmenuGalleryController.FilterMenuGroupButtonClick(
  Sender: TObject);
var
  AButton: TdxBarButton;
  AIsFilterChanged, AShowAllGroups: Boolean;
  AItem: TdxBarItem;
  I: Integer;
begin
  AButton := TdxBarButton(Sender);
  if AButton.Tag >= 0 then
  begin
    GalleryItem.GalleryGroups[AButton.Tag].Visible := AButton.Down;
    AIsFilterChanged := True;
  end
  else
  begin
    AIsFilterChanged := False;
    AShowAllGroups := AButton.Tag = -1;
    GalleryItem.LockFilterChanged(True);
    try
      for I := 0 to FFilterMenuLinksOwner.ItemLinks.VisibleItemCount - 1 do
      begin
        AItem := FFilterMenuLinksOwner.ItemLinks.VisibleItems[I].Item;
        if IsFilterMenuInternalButton(AItem) and (AItem.Tag >= 0) and
          (TdxBarButton(AItem).Down <> AShowAllGroups) then
        begin
          TdxBarButton(AItem).Down := AShowAllGroups;
          GalleryItem.GalleryGroups[AItem.Tag].Visible := AShowAllGroups;
          AIsFilterChanged := True;
        end;
      end;
    finally
      GalleryItem.LockFilterChanged(False);
    end;
  end;
  if AIsFilterChanged then
  begin
    GalleryItem.DoFilterChanged;
    ViewInfo.RepaintFilterBand;
  end;
end;

function TdxRibbonOnSubmenuGalleryController.GetFirstGroupItem: TdxRibbonGalleryGroupItem;
var
  AGroup: TdxRibbonGalleryGroup;
  I, Index: Integer;
begin
  Result := nil;
  for I := 0 to GalleryItem.GalleryGroups.Count - 1 do
    if GalleryItem.IsGroupVisible(I) then
    begin
      AGroup := GalleryItem.GalleryGroups[I];
      if AGroup.Items.Count > 0 then
      begin
        Index := 0;
        while (Index < AGroup.Items.Count - 1) and not AGroup.Items[Index].Enabled do
          Inc(Index);
        if AGroup.Items[Index].Enabled then
          Exit(AGroup.Items[Index]);
      end;
    end;
end;

function TdxRibbonOnSubmenuGalleryController.GetGalleryWidth: Integer;
begin
  Result := ViewInfo.GallerySize.cx;
end;

function TdxRibbonOnSubmenuGalleryController.GetLastGroupItem: TdxRibbonGalleryGroupItem;
var
  AGroup: TdxRibbonGalleryGroup;
  I: Integer;
begin
  Result := nil;
  for I := GalleryItem.GalleryGroups.Count - 1 downto 0 do
    if GalleryItem.IsGroupVisible(I) then
    begin
      AGroup := GalleryItem.GalleryGroups[I];
      if AGroup.Items.Count <> 0 then
        Exit(AGroup.Items[AGroup.Items.Count - 1]);
    end;
end;

function TdxRibbonOnSubmenuGalleryController.GetViewInfo: TdxRibbonOnMenuGalleryControlViewInfo;
begin
  Result := TdxRibbonOnMenuGalleryControlViewInfo(FOwner.ViewInfo);
end;

procedure TdxRibbonOnSubmenuGalleryController.HideFilterMenu;
begin
  FFilterMenuLinksOwner.ItemLinks.BarControl.Hide;
  CheckFilterMenuHotTrack;
end;

procedure TdxRibbonOnSubmenuGalleryController.InitFilterMenu(AItemLinks: TdxBarItemLinks);

  procedure AddButton(const ACaption: string; AIsCheckable, AIsDown: Boolean; ATag: TcxTag; ABeginGroup: Boolean);
  var
    AButton: TdxBarButton;
  begin
    AButton := TdxBarButton(BarDesignController.AddInternalItem(AItemLinks, TdxBarButton, ACaption, FilterMenuButtonClick).Item);
    AButton.CloseSubMenuOnClick := False;
    if AIsCheckable then
    begin
      AButton.ButtonStyle := bsChecked;
      AButton.Down := AIsDown;
    end;
    AButton.Tag := ATag;
    AButton.Links[0].BeginGroup := ABeginGroup;
  end;

var
  I: Integer;
begin
  BarDesignController.ClearInternalItems;
  if GalleryItem.GalleryFilter.Categories.Count = 0 then
  begin
    for I := 0 to GalleryItem.GalleryGroups.Count - 1 do
    begin
      if GalleryItem.IsGroupVisible(I, True) then
        AddButton(GalleryItem.GalleryGroups[I].Header.Caption, True, GalleryItem.GalleryGroups[I].Visible, I, False);
    end;
    AddButton(cxGetResourceString(@dxSBAR_SHOWALLGALLERYGROUPS), False, False, -1, True);
    AddButton(cxGetResourceString(@dxSBAR_HIDEALLGALLERYGROUPS), False, False, -2, False);
  end
  else
  begin
    for I := 0 to GalleryItem.GalleryFilter.Categories.Count - 1 do
      AddButton(GalleryItem.GalleryFilter.Categories[I].Caption, True, I = GalleryItem.GalleryFilter.ActiveCategoryIndex, I, False);
    AddButton(cxGetResourceString(@dxSBAR_CLEARGALLERYFILTER), False, False, -1, True);
  end;

  GalleryItem.DoInitFilterMenu(AItemLinks);
end;

function TdxRibbonOnSubmenuGalleryController.IsFilterMenuInternalButton(AItem: TdxBarItem): Boolean;
var
  ATempEventHandler: TNotifyEvent;
begin
  ATempEventHandler := FilterMenuButtonClick;
  Result := Assigned(AItem.OnClick) and dxSameMethods(AItem.OnClick, ATempEventHandler);
end;

procedure TdxRibbonOnSubmenuGalleryController.ShowFilterMenu;
var
  AOwnerHeight: Integer;
  P: TPoint;
  APopupControl: TdxRibbonGalleryFilterMenuControl;
begin
  if FFilterMenuLinksOwner = nil then
    FFilterMenuLinksOwner := TdxBarInternalLinksOwner.Create(GalleryItem.BarManager, TdxRibbonGalleryFilterMenuControl);
  FFilterMenuLinksOwner.ItemLinks.CreateBarControl;
  APopupControl := FFilterMenuLinksOwner.BarControl as TdxRibbonGalleryFilterMenuControl;
  APopupControl.FGalleryControl := FOwner;
  APopupControl.FParentWnd := FOwner.Parent.Handle;
  APopupControl.ParentBar := FOwner.Parent;
  APopupControl.OwnerControl := FOwner.Parent;
  InitFilterMenu(FFilterMenuLinksOwner.ItemLinks);
  ViewInfo.GetFilterMenuShowingParams(P, AOwnerHeight);
  APopupControl.OwnerHeight := AOwnerHeight;
  APopupControl.Left := P.X;
  APopupControl.Top := P.Y;
  APopupControl.Show;
end;

{ TdxRibbonGalleryFilterMenuControl }

function TdxRibbonGalleryFilterMenuControl.GetBehaviorOptions: TdxBarBehaviorOptions;
begin
  Result := inherited GetBehaviorOptions - [bboItemCustomizePopup];
end;

function TdxRibbonGalleryFilterMenuControl.GetPainter: TdxBarPainter;
begin
  Result := GalleryControl.Painter;
end;

procedure TdxRibbonGalleryFilterMenuControl.ProcessMouseDownMessageForMeaningParent(
  AWnd: HWND; AMsg: UINT; const AMousePos: TPoint);
begin
  inherited ProcessMouseDownMessageForMeaningParent(AWnd, AMsg, AMousePos);
  if TdxRibbonOnMenuGalleryControlViewInfo(GalleryControl.ViewInfo).IsPtInFilterBandHotTrackArea(
    RealOwnerControl.ScreenToClient(AMousePos)) then
      FDontShowFilterMenuOnMouseDown := True;
  TdxRibbonOnSubmenuGalleryController(GalleryControl.Controller).HideFilterMenu;
end;

{ TdxRibbonGalleryControl }

constructor TdxRibbonGalleryControl.Create(AItemLink: TdxBarItemLink);
begin
  inherited Create(AItemLink);
  FController := CreateController;
  FScrollBar := TdxRibbonGalleryScrollBar.Create(Self);
  FScrollBar.Visible := False;
  FScrollBar.Parent := Parent;
  FScrollBar.SmallChange := 3;
  FScrollBar.OnDropDown := DoScrollBarDropDown;
  FScrollBar.OnMouseMove := DoScrollBarMouseMove;
  FScrollBar.OnScroll := DoScrollBarScroll;
end;

destructor TdxRibbonGalleryControl.Destroy;
begin
  FreeAndNil(FScrollBar);
  FreeAndNil(FController);
  inherited Destroy;
end;

procedure TdxRibbonGalleryControl.Update(const R: TRect);
begin
  inherited Update(R);
  if ScrollBar.HandleAllocated then
    ScrollBar.Invalidate;
end;

procedure TdxRibbonGalleryControl.ShowGroupItem(AGroupItem: TdxRibbonGalleryGroupItem);
begin
  ViewInfo.ShowGroupItem(AGroupItem);
end;

function TdxRibbonGalleryControl.DoHint(var ANeedDeactivate: Boolean; out AHintText: string; out AShortCut: string): Boolean;
begin
  AHintText := '';
  AShortCut := '';
  ANeedDeactivate := False;
  if FHintItem <> nil then
  begin
    Result := (Item.GalleryOptions.ItemHintSource = ghsItemHint) or
      ((Item.GalleryOptions.ItemHintSource = ghsGalleryScreenTip) and (Item.ScreenTip <> nil));
    if Result then
      AHintText := GetHintText(FHintItem);
  end
  else
    Result := inherited DoHint(ANeedDeactivate, AHintText, AShortCut);
end;

function TdxRibbonGalleryControl.GetHintText(AGroupItem: TdxRibbonGalleryGroupItem): string;
var
  AGroupItemViewInfo: TdxRibbonGalleryGroupItemViewInfo;
begin
  Result := AGroupItem.Caption;

  if AGroupItem.Hint <> '' then
    Result := AGroupItem.Hint
  else
  begin
    AGroupItemViewInfo := Controller.ViewInfo.GetGroupItemViewInfo(FHintItem);
    if (AGroupItemViewInfo <> nil) and AGroupItemViewInfo.IsCaptionVisible and
      (FHintItem.Description <> '') then
      Result := FHintItem.Description;
  end;
end;

function TdxRibbonGalleryControl.GetHintPosition(const ACursorPos: TPoint;
  AHeight: Integer): TPoint;
begin
  Result := inherited GetHintPosition(ACursorPos, AHeight);
  if ViewInfo.IsInRibbon then
  begin
    if FHintItem <> nil then
      if UseRightToLeftAlignment then
        Result.X := Parent.ClientToScreen(cxPoint(FHintBounds.Right, FHintBounds.Top)).X
      else
        Result.X := Parent.ClientToScreen(FHintBounds.TopLeft).X
  end
  else
  begin
    Result := GetMouseCursorPos;
    Inc(Result.Y, 20);
  end;
end;

procedure TdxRibbonGalleryControl.UpdateHint(AHintItem: TdxRibbonGalleryGroupItem;
  const ABounds: TRect);
begin
  FHintItem := AHintItem;
  FHintBounds := ABounds;
  if FHintItem <> nil then
    BarManager.ActivateHint(True, '', Self)
  else
    BarManager.HideHint;
end;

function TdxRibbonGalleryControl.CalcDefaultWidth: Integer;

  function GetColumnCount: Integer;
  begin
    if Parent.Kind = bkBarControl then
      Result := (ViewInfo as IdxBarMultiColumnItemControlViewInfo).GetColumnCount
    else
      Result := Item.GalleryOptions.ColumnCount;
  end;

const
  MinWidth = 100;
var
  AGroupItemWidthIsNull: Boolean;
  AScrollbarAndIndents: Integer;
begin
  if GetVisibleGroupCount > 0 then
  begin
    ViewInfo.CalculateGlobalItemSize;
    Result := ViewInfo.GetLayoutWidth(GetColumnCount, AGroupItemWidthIsNull);
    AScrollbarAndIndents := ViewInfo.GetScrollBarWidth + ViewInfo.GetLeftLayoutIndent + ViewInfo.GetRightLayoutIndent;
    if (Result = 0) or AGroupItemWidthIsNull then
      Result := ScaleFactor.Apply(MinWidth) - AScrollbarAndIndents;
    Result := Result + AScrollbarAndIndents;
    Result := Result + Max(0, ViewInfo.GetGalleryMargins.Left) + Max(0, ViewInfo.GetGalleryMargins.Right);
  end
  else
    Result := ScaleFactor.Apply(MinWidth);

  Result := Min(Result, Screen.Width - 2 * Painter.SubMenuControlNCBorderSize);
end;

function TdxRibbonGalleryControl.CalcMinHeight: Integer;
var
  ADestroyAfterUse: Boolean;
  AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
  AHeaderHeight: Integer;
  AItemHeight: Integer;
  ARowHeight: Integer;
  I: Integer;
begin
  if GetGroups.Count > 0 then
  begin
    ARowHeight := 0;
    AHeaderHeight := 0;
    for I := 0 to GetGroups.Count - 1 do
      if Item.IsGroupVisible(I) then
      begin
        AGroupViewInfo := GetGroupViewInfo(GetGroups, ViewInfo, I, ADestroyAfterUse);
        try
          AItemHeight := AGroupViewInfo.GetRowHeight - AGroupViewInfo.GetSpaceBetweenItems(False);
          ARowHeight := Max(ARowHeight, AItemHeight);
          AHeaderHeight := Max(AHeaderHeight, AGroupViewInfo.Header.GetHeight(ViewInfo.GallerySize.cx, True));
        finally
          if ADestroyAfterUse then
            AGroupViewInfo.Free;
        end;
        if Item.GalleryOptions.EqualItemSizeInAllGroups then
          Break;
      end;
    Result := AHeaderHeight + ARowHeight;
  end
  else
    Result := 0;
end;

procedure TdxRibbonGalleryControl.CalcParts;
begin
  inherited CalcParts;
  ScrollBarSetup;
  if not Collapsed then
    ViewInfo.Calculate(FScrollBar.Position, scPosition);
end;

function TdxRibbonGalleryControl.CanClicked: Boolean;
begin
  Result := not Collapsed and not FIsDroppingDown;
end;

function TdxRibbonGalleryControl.CanCustomize: Boolean;
begin
  Result := inherited CanCustomize;
  if Parent is TdxRibbonDropDownGalleryControl then
    Result := TdxRibbonDropDownGalleryControl(Parent).InternalGalleryItemControl <> Self;
end;

procedure TdxRibbonGalleryControl.ControlUnclick(ByMouse: Boolean);

  function CreateInRibbonGalleryControlLink: TcxObjectLink;
  var
    AItemControl: TdxBarItemControl;
  begin
    Result := nil;
    if (Parent.Kind <> bkBarControl) and (Parent.ParentBar <> nil) and (Parent.ParentBar.Kind = bkBarControl) then
    begin
      AItemControl := TdxRibbonDropDownGalleryControl(Parent).ParentItemControl;
      if not Collapsed and TdxBarItemControlAccess(AItemControl).IsExpandable then
        Result := cxAddObjectLink(AItemControl);
    end;
  end;

var
  AGalleryControl: TdxRibbonGalleryControl;
  AInRibbonGalleryControlLink: TcxObjectLink;
  AItem: TdxCustomRibbonGalleryItem;
begin
  AInRibbonGalleryControlLink := CreateInRibbonGalleryControlLink;
  try
    AItem := Item;
    if ByMouse then
    begin
      Item.FClickedGroupItem := ViewInfo.GetGroupItem(Parent.ScreenToClient(GetMouseCursorPos));
      if Item.FClickedGroupItem = nil then
      begin
        inherited ControlUnclick(ByMouse);
        Exit;
      end;
    end
    else
      Item.FClickedGroupItem := Controller.KeyboardHotGroupItem;

    inherited ControlUnclick(ByMouse);

    if (AInRibbonGalleryControlLink <> nil) and (AInRibbonGalleryControlLink.Ref <> nil) then
    begin
      AGalleryControl := TdxRibbonGalleryControl(AInRibbonGalleryControlLink.Ref);
      if AGalleryControl.Item = AItem then
        AGalleryControl.ViewInfo.ShowGroupItem(AItem.FClickedGroupItem);
    end;
  finally
    cxRemoveObjectLink(AInRibbonGalleryControlLink);
  end;
end;

function TdxRibbonGalleryControl.CreateController: TdxRibbonGalleryController;
begin
  case Parent.Kind of
    bkBarControl:
      Result := TdxRibbonGalleryController.Create(Self);
    bkSubMenu:
      Result := TdxRibbonOnSubmenuGalleryController.Create(Self);
  else
    raise EdxException.Create(InvalidGalleryParentKind);
  end;
end;

function TdxRibbonGalleryControl.CreateHintViewInfo(const AHintText,
  AShortCut: string): TdxBarCustomHintViewInfo;
begin
  if (Item.GalleryOptions.ItemHintSource <> ghsGalleryScreenTip) and (FHintItem <> nil) then
    Result := TCustomdxBarControlAccess(Parent).CreateHintViewInfo(AHintText, AShortCut, nil)
  else
    Result := inherited CreateHintViewInfo(AHintText, AShortCut);
end;

procedure TdxRibbonGalleryControl.DoCloseUp(AHadSubMenuControl: Boolean);
begin
  FIsClosingUpSubmenuControl := True;
  try
    inherited DoCloseUp(AHadSubMenuControl);
    Item.ItemLinks.BarControl := nil;
  finally
    FIsClosingUpSubmenuControl := False;
  end;
end;

procedure TdxRibbonGalleryControl.DoDropDown(AByMouse: Boolean);
begin
  inherited DoDropDown(AByMouse);
  if IsDroppedDown then
    Controller.GroupItemHotTrackEnabled := True;
end;

procedure TdxRibbonGalleryControl.DropDown(AByMouse: Boolean);
begin
  Controller.KeyboardHotGroupItem := nil;
  FIsDroppingDown := True;
  try
    inherited DropDown(AByMouse);
  finally
    FIsDroppingDown := False;
  end;
end;

procedure TdxRibbonGalleryControl.EnabledChanged;
begin
  inherited EnabledChanged;
  ScrollBarSetup;
end;

function TdxRibbonGalleryControl.GetClientHeight: Integer;
begin
  Result := Max(0, ViewInfo.GallerySize.cy);
end;

function TdxRibbonGalleryControl.GetClientWidth: Integer;
begin
  Result := Max(0, ViewInfo.GallerySize.cx);
end;

function TdxRibbonGalleryControl.GetDefaultHeightInSubMenu: Integer;
begin
  Result := inherited GetDefaultHeightInSubmenu;
  if not Collapsed then
    Result := Max(Result, ViewInfo.GetHeight(GetDefaultWidthInSubmenu) + cxMarginsHeight(ViewInfo.GetGalleryMargins));
end;

function TdxRibbonGalleryControl.GetDefaultWidthInSubMenu: Integer;
begin
  if Collapsed then
    Result := inherited GetDefaultWidthInSubmenu
  else
    Result := CalcDefaultWidth;
end;

procedure TdxRibbonGalleryControl.GetDefaultTextColors(
  AEnabled, ASelected, AFlat: Boolean; var AColor1, AColor2: TColor);
begin
  Painter.DropDownGalleryItemGetTextColors(Self, AEnabled, ASelected, AFlat, AColor1, AColor2);
end;

function TdxRibbonGalleryControl.GetCollapsed: Boolean;
begin
  Result := not IsValidPainter or (ViewInfo <> nil) and
    (TCustomdxBarControlAccess(Parent).GetItemControlDefaultViewLevel(Self) <> ivlDefault);
  if not Result then
    if FIsCollapsedAssigned then
      Result := FCollapsed
    else
      Result := (ViewInfo <> nil) and ViewInfo.IsCollapsed;
end;

function TdxRibbonGalleryControl.GetMouseWheelStep: Integer;
const
  WheelRowCount = 3;
var
  AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
  I, AGroupCount, AHeightSum: Integer;
  ADestroyAfterUse: Boolean;
begin
  Result := 0;
  AGroupCount := 0;
  AHeightSum := 0;
  for I := 0 to GetGroups.Count - 1 do
    if Item.IsGroupVisible(I) then
    begin
      AGroupViewInfo := GetGroupViewInfo(GetGroups, ViewInfo, I,
        ADestroyAfterUse);
      try
        Inc(AGroupCount);
        AHeightSum := AHeightSum + AGroupViewInfo.GetRowHeight;
      finally
        if ADestroyAfterUse then
          AGroupViewInfo.Free;
      end;
    end;
  if AGroupCount <> 0 then
    Result := MulDiv(AHeightSum, WheelRowCount, AGroupCount);
end;

procedure TdxRibbonGalleryControl.GetSubMenuControlPositionParams(out P: TPoint; out AOwnerWidth, AOwnerHeight: Integer);
var
  R: TRect;
begin
  if not Collapsed and ScrollBar.IsDropDownStyle and not Parent.IsCustomizing then
  begin
    R := ViewInfo.GalleryBounds;
    if UseRightToLeftAlignment then
      P := Parent.ClientToScreen(cxPointOffset(cxPoint(R.Right, R.Top), DropDownOffsetX, -DropDownOffsetY))
    else
      P := Parent.ClientToScreen(cxPointOffset(R.TopLeft, -DropDownOffsetX, -DropDownOffsetY));
    AOwnerWidth := 0;
    AOwnerHeight := 1;
  end
  else
    inherited GetSubmenuControlPositionParams(P, AOwnerWidth, AOwnerHeight);
end;

function TdxRibbonGalleryControl.InternalGetDefaultWidth: Integer;
begin
  if Collapsed then
    Result := inherited InternalGetDefaultWidth
  else
    Result := CalcDefaultWidth;
end;

procedure TdxRibbonGalleryControl.Changed;
begin
  ViewInfo.Changed;
end;

function TdxRibbonGalleryControl.WantsKey(Key: Word): Boolean;
begin
  Result := inherited WantsKey(Key) and
    not ((Key = VK_RETURN) and not Collapsed and (Parent.Kind <> bkBarControl));
end;

procedure TdxRibbonGalleryControl.CalcDrawParams(AFull: Boolean = True);
begin
  inherited CalcDrawParams(AFull);
  if Collapsed then
    Exit;
  if AFull then
  begin
    FDrawParams.ShortCut := '';
    FDrawParams.PaintType := TCustomdxBarControlAccess(Parent).GetPaintType;
    FDrawParams.Enabled := Enabled;
    FDrawParams.SelectedByKey := False;
  end;
  FDrawParams.Canvas := Canvas;
end;

procedure TdxRibbonGalleryControl.ControlActivate(AImmediately, AByMouse: Boolean);

  function CanActivateControl: Boolean;
  begin
    Result := not Parent.IsCustomizing or
      not (AImmediately and not Collapsed and (Parent.Kind = bkBarControl) and
      not ScrollBar.IsDropDownButtonPressed);
  end;

begin
  if CanActivateControl then
    inherited;
end;

procedure TdxRibbonGalleryControl.ControlClick(AByMouse: Boolean; AKey: Char = #0);
var
  R: TRect;
begin
  if (Item.ItemLinks.BarControl <> nil) and Collapsed then
    DoCloseUp(True)
  else
  begin
    if AByMouse and not Collapsed and (Parent.Kind = bkBarControl) then
    begin
      R := cxGetWindowRect(ScrollBar);
      FIsClickOnItemsArea := not PtInRect(R, GetMouseCursorPos);
    end;
    try
      inherited ControlClick(AByMouse, AKey);
    finally
      FIsClickOnItemsArea := False;
    end;
  end;
end;

procedure TdxRibbonGalleryControl.CreateSubMenuControl;
var
  ASubmenuGalleryItem: TdxCustomRibbonGalleryItem;
  ASubmenuControl: TdxRibbonDropDownGalleryControl;
begin
  ASubmenuControl := TdxRibbonDropDownGalleryControl.Create(BarManager);
  Item.ItemLinks.BarControl := ASubmenuControl;
  if (Item.DropDownGallery <> nil) and Item.DropDownGallery.HasValidGalleryItem then
  begin
    ASubmenuControl.ItemLinks := Item.DropDownGallery.ItemLinks;
    ASubmenuControl.ItemLinks.BarControl := ASubmenuControl;
    ASubmenuGalleryItem := Item.DropDownGallery.GalleryItem;
  end
  else
  begin
    ASubmenuControl.ItemLinks := Item.ItemLinks;
    ASubmenuGalleryItem := Item;
  end;
  ASubmenuControl.GalleryItem := ASubmenuGalleryItem;
end;

procedure TdxRibbonGalleryControl.DoCreateSubMenuControl;
begin
  FIsCreatingSubmenuControl := True;
  try
    inherited DoCreateSubmenuControl;
  finally
    FIsCreatingSubmenuControl := False;
  end;
end;

procedure TdxRibbonGalleryControl.DoPaint(ARect: TRect; PaintType: TdxBarPaintType);
begin
  ObtainTextColors;
  if not IsValidPainter then
    DrawInvalid(ARect)
  else
    if Collapsed then
      inherited DoPaint(ARect, PaintType)
    else
      ViewInfo.Paint(Canvas);
end;

procedure TdxRibbonGalleryControl.FilterCaptionChanged;
begin
  if (Parent.Kind <> bkBarControl) and Item.IsFilterVisible then
    TdxRibbonOnMenuGalleryControlViewInfo(ViewInfo).RepaintFilterBand;
end;

function TdxRibbonGalleryControl.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxRibbonGalleryControlAccessibilityHelper;
end;

function TdxRibbonGalleryControl.GetGroups: TdxRibbonGalleryGroups;
begin
  Result := TdxCustomRibbonGalleryItem(Item).GalleryGroups;
end;

function TdxRibbonGalleryControl.GetSubMenuControl: TdxBarSubmenuControl;
begin
  Result := inherited GetSubmenuControl;
  if (Parent.Kind = bkBarControl) and
    (Result <> nil) and (Result.ParentItemControl <> Self) and
    not FIsCreatingSubmenuControl and not FIsClosingUpSubmenuControl then
    Result := nil;
end;

function TdxRibbonGalleryControl.GetViewInfoClass: TdxBarItemControlViewInfoClass;
begin
  case Parent.Kind of
    bkSubMenu: Result := TdxRibbonOnMenuGalleryControlViewInfo;
    bkBarControl: Result := TdxInRibbonGalleryControlViewInfo;
  else
    raise EdxException.Create(InvalidGalleryParentKind);
  end
end;

function TdxRibbonGalleryControl.GetVisibleGroupCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to GetGroups.Count - 1 do
    if Item.IsGroupVisible(I) then
      Inc(Result);
end;

function TdxRibbonGalleryControl.HasSubMenu: Boolean;
begin
  if not IsValidPainter then
  begin
    Result := False;
    Exit;
  end;
  Result := Collapsed or (Parent.Kind = bkBarControl) and
    (Parent.IsCustomizing or not FIsClickOnItemsArea);
end;

function TdxRibbonGalleryControl.CanDestroyOnClick: Boolean;
begin
  if not Collapsed and (Parent.Kind <> bkBarControl) then
  begin
    if TdxRibbonOnMenuGalleryControlViewInfo(ViewInfo).IsPtInFilterBandHotTrackArea(Parent.ScreenToClient(GetMouseCursorPos)) then
      Exit(False);
    if Item.FClickedGroupItem = nil then
      Exit(False);
  end;
  Result := CanClicked;
end;

function TdxRibbonGalleryControl.IsEnabledScrollBar: Boolean;
begin
  Result := Enabled and (ScrollBar.IsDropDownStyle or
    (ViewInfo.GetGalleryHeight(ClientWidth) > ClientHeight) and not Item.RecalculatingOnFilterChanged);
end;

function TdxRibbonGalleryControl.IsHiddenForCustomization: Boolean;
begin
  Result := Item.IsClone;
end;

function TdxRibbonGalleryControl.IsNeedScrollBar: Boolean;
begin
  if not FIsNeedScrollBarLock and
    (ViewInfo.IsInRibbon or (SubMenuControl <> nil) or (Parent is TdxRibbonDropDownGalleryControl)) then
  begin
    FIsNeedScrollBarLock := True;
    try
      Result := not Collapsed and ((Parent.Kind = bkBarControl) or
        (Item.GalleryOptions.SubmenuResizing in [gsrHeight, gsrWidthAndHeight]) or
        IsEnabledScrollBar);
    finally
      FIsNeedScrollBarLock := False;
    end;
  end
  else
    Result := False;
end;

function TdxRibbonGalleryControl.IsValidPainter: Boolean;
begin
  Result := (Parent <> nil) and (not (Parent.Kind = bkBarControl) or (Painter is TdxBarSkinnedPainter));
end;

procedure TdxRibbonGalleryControl.MouseLeave;
begin
  inherited MouseLeave;
  if not Collapsed then
    Controller.MouseLeave;
end;

procedure TdxRibbonGalleryControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not Collapsed then
    Controller.MouseDown(Button, Shift, X, Y);
end;

procedure TdxRibbonGalleryControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if not Collapsed then
    Controller.MouseMove(Shift, X, Y);
end;

procedure TdxRibbonGalleryControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if not Collapsed then
    Controller.MouseUp(Button, Shift, X, Y);
end;

function TdxRibbonGalleryControl.NeedBufferedRepaint: Boolean;
begin
  Result := not Item.GalleryInMenuOptions.CollapsedInSubmenu;
end;

procedure TdxRibbonGalleryControl.DoScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);

  function CanHandleScrollCode: Boolean;
  begin
    Result := not (ScrollBar.IsDropDownStyle and (
        (ScrollCode = scLineUp) and not ViewInfo.GetPreviousButtonEnabled or
        (ScrollCode = scLineDown) and not ViewInfo.GetNextButtonEnabled or
        (ScrollCode = scEndScroll)));
  end;

begin
  if not CanHandleScrollCode then
    Exit;
  ScrollPos := Min(ScrollPos, GetScrollBarMaxPos);
  ViewInfo.Calculate(ScrollPos, ScrollCode);
  ScrollBarSetup;
  Repaint;
end;

procedure TdxRibbonGalleryControl.DoScrollBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  TCustomdxBarControlAccess(Parent).MouseMove(Shift, FScrollBar.Left + X, FScrollBar.Top + Y);
end;

function TdxRibbonGalleryControl.GetScrollBarMaxPos: Integer;
begin
  Result := ViewInfo.GetGalleryHeight(ClientWidth) - ClientHeight;
end;

procedure TdxRibbonGalleryControl.ScrollBarSetup;
const
  ShowFlags: array[Boolean] of Integer = (SW_HIDE, SW_SHOW);
var
  AGalleryHeight, ALargeChange: Integer;
begin
  ScrollBar.BoundsRect := ViewInfo.ScrollBarBounds;
  ShowWindow(ScrollBar.Handle, ShowFlags[IsNeedScrollBar]);
  if not Collapsed then
  begin
    ALargeChange := 1;
    AGalleryHeight := ViewInfo.GetGalleryHeight(ClientWidth);
    if AGalleryHeight <= ClientHeight then
      ScrollBar.SetScrollParams(0, 1, 0, 0, False)
    else
    begin
      ScrollBar.SetScrollParams(0, Max(0, AGalleryHeight - 1), ViewInfo.LayoutOffset, Max(0, ClientHeight), False);
      if ClientHeight > 1 then
        ALargeChange := ClientHeight;
    end;
    ScrollBar.LargeChange := ALargeChange;
    ScrollBar.Enabled := IsEnabledScrollBar;
  end;
end;

procedure TdxRibbonGalleryControl.SetScrollBarPosition(APosition: Integer);
var
  AScrollCode: TScrollCode;
begin
  if FScrollBar.Enabled and (FScrollBar.Position <> APosition) then
  begin
    if FScrollBar.Position > APosition then
      AScrollCode := scLineUp
    else
      AScrollCode := scLineDown;

    FScrollBar.Position := APosition;
    APosition := FScrollBar.Position;
    DoScrollBarScroll(FScrollBar, AScrollCode, APosition);
  end;
end;

procedure TdxRibbonGalleryControl.DoScrollBarDropDown(Sender: TObject);
begin
  if Parent.IsCustomizing then
  begin
    BarNavigationController.ChangeSelectedObject(True, IAccessibilityHelper);
    DropDown(True);
  end
  else
    SendMessage(Parent.Handle, WM_LBUTTONDOWN,
      ShiftStateToKeys(KeyboardStateToShiftState),
      MakeLParam(ScrollBar.Left, ScrollBar.Top));
end;

procedure TdxRibbonGalleryControl.DrawInvalid(const ABounds: TRect);
begin
  Canvas.Pen.Color := clRed;
  Canvas.SetBrushColor(clWhite);
  Canvas.Brush.Style := bsSolid;
  Canvas.Rectangle(ABounds);
  Canvas.Line(ABounds.Left, ABounds.Top, ABounds.Right, ABounds.Bottom - 1);
  Canvas.Line(ABounds.Right - 1, ABounds.Top, ABounds.Left, ABounds.Bottom - 1);
  cxDrawText(Canvas, Item.Caption, ABounds, DT_CENTER);
end;

function TdxRibbonGalleryControl.GetItem: TdxCustomRibbonGalleryItem;
begin
  Result := TdxCustomRibbonGalleryItem(ItemLink.Item);
end;

function TdxRibbonGalleryControl.GetViewInfo: TdxRibbonGalleryControlViewInfo;
begin
  Result := TdxRibbonGalleryControlViewInfo(FViewInfo);
end;

procedure TdxRibbonGalleryControl.ObtainTextColors;

  function ObtainTextColor(AEnabled, ASelected, AHot: Boolean): TColor;
  var
    AColor: TColor;
    ASavedCanSelect: Boolean;
    ASavedDrawSelected: Boolean;
    ASavedEnabled: Boolean;
    ASavedHotPartIndex: Integer;
  begin
    ASavedEnabled := DrawParams.Enabled;
    ASavedCanSelect := DrawParams.CanSelect;
    ASavedHotPartIndex := DrawParams.HotPartIndex;
    ASavedDrawSelected := DrawParams.DrawSelected;
    try
      DrawParams.Enabled := AEnabled;
      DrawParams.CanSelect := AEnabled;
      DrawParams.DrawSelected := ASelected;
      DrawParams.HotPartIndex := IfThen(AHot, icpControl, icpNone);
      GetTextColors(AEnabled, ASelected, True, Result, AColor);
    finally
      DrawParams.CanSelect := ASavedCanSelect;
      DrawParams.DrawSelected := ASavedDrawSelected;
      DrawParams.Enabled := ASavedEnabled;
      DrawParams.HotPartIndex := ASavedHotPartIndex;
    end;
  end;

begin
  ViewInfo.FTextColor := ObtainTextColor(Enabled, False, False);
  ViewInfo.FHotTextColor := ObtainTextColor(True, False, True);
  ViewInfo.FSelectedTextColor := ObtainTextColor(True, True, False);
  ViewInfo.FDisabledTextColor := ObtainTextColor(False, False, False);
end;

procedure TdxRibbonGalleryControl.SetCollapsed(Value: Boolean);
begin
  FIsCollapsedAssigned := True;
  FCollapsed := Value;
end;

{ TdxRibbonGalleryGroupElementViewInfo }

constructor TdxRibbonGalleryGroupElementViewInfo.Create(
  AOwner: TdxRibbonGalleryGroupViewInfo);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TdxRibbonGalleryGroupElementViewInfo.Calculate(const ABounds: TRect);
begin
  FBounds := ABounds;
end;

procedure TdxRibbonGalleryGroupElementViewInfo.Paint(ACanvas: TcxCanvas);
begin
end;

function TdxRibbonGalleryGroupElementViewInfo.GetFont: TFont;
begin
  Result := Owner.Font;
end;

function TdxRibbonGalleryGroupElementViewInfo.GetGalleryItemControl: TdxRibbonGalleryControl;
begin
  Result := Owner.Owner.Control;
end;

function TdxRibbonGalleryGroupElementViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := GetGalleryItemControl.ScaleFactor;
end;

function TdxRibbonGalleryGroupElementViewInfo.GetTextFlags(AAlignment: TAlignment): DWORD;
const
  AlignmentsHorz: array[TAlignment] of DWORD = (DT_LEFT, DT_RIGHT, DT_CENTER);
begin
  if GetGalleryItemControl.Parent.UseRightToLeftAlignment then
    ChangeBiDiModeAlignment(AAlignment);
  Result := DT_SINGLELINE or DT_VCENTER or AlignmentsHorz[AAlignment];
end;

{ TdxRibbonGalleryGroupHeaderViewInfo }

procedure TdxRibbonGalleryGroupHeaderViewInfo.Calculate(
  const ABounds: TRect);
begin
  inherited Calculate(ABounds);
  FTextBounds := GetTextBounds;
end;

procedure TdxRibbonGalleryGroupHeaderViewInfo.Paint(ACanvas: TcxCanvas);
begin
  if IsVisible then
  begin
    ACanvas.Font := GetFont;
    ACanvas.Font.Style := [fsBold];
    Owner.Painter.DropDownGalleryDrawGroupHeaderBackground(ACanvas.Handle, Bounds);
    cxDrawText(ACanvas, Caption, cxRectInflate(Bounds, -GroupHeaderCaptionOffset, 0),
      GetTextFlags(Owner.Group.Header.Alignment), Owner.Painter.DropDownGalleryGetGroupHeaderTextColor);
  end;
end;

function TdxRibbonGalleryGroupHeaderViewInfo.GetCaption: string;
begin
  Result := Owner.Group.Header.Caption;
end;

function TdxRibbonGalleryGroupHeaderViewInfo.GetHeight(AWidth: Integer; AWithSpaceAfterHeader: Boolean): Integer;
begin
  if Owner.Group.Header.Visible and (AWidth <> 0) then
  begin
    Result := MulDiv(cxTextHeight(GetFont), 3, 2);
    if AWithSpaceAfterHeader then
      Inc(Result, Owner.Options.GetSpaceAfterGroupHeader);
  end
  else
    Result := 0;
end;

function TdxRibbonGalleryGroupHeaderViewInfo.GetTextBounds: TRect;
begin
  Result := Bounds;
end;

function TdxRibbonGalleryGroupHeaderViewInfo.IsVisible: Boolean;
begin
  Result := Owner.Group.Header.Visible and Owner.Owner.IsGroupHeaderVisible and not IsRectEmpty(Bounds);
end;

{ TdxRibbonGalleryGroupItemViewInfo }

constructor TdxRibbonGalleryGroupItemViewInfo.Create(
  AOwner: TdxRibbonGalleryGroupViewInfo; AGroupItem: TdxRibbonGalleryGroupItem);
begin
  inherited Create(AOwner);
  FDescriptionLines := TStringList.Create;
  FGroupItem := AGroupItem;
  FDescriptionRowCount := IfThen(GetDescriptionLength > GetCaptionWidth,
    Owner.Owner.GalleryOptions.LongDescriptionDefaultRowCount, 1);
  ResetCachedValues;
end;

destructor TdxRibbonGalleryGroupItemViewInfo.Destroy;
begin
  FreeAndNil(FDescriptionLines);
  inherited Destroy;
end;

procedure TdxRibbonGalleryGroupItemViewInfo.Calculate(const ABounds: TRect);
begin
  inherited Calculate(ABounds);
  FCaptionBounds := GetRectConsiderBounds(GetCaptionBounds);
  FDescriptionBounds := GetRectConsiderBounds(GetDescriptionBounds);
  FImageBounds := GetImageBounds;
  if IsCaptionVisible and (IsImageVisible or IsDescriptionVisible) then
    CheckTextVerticalAlignment(Options.ItemTextAlignVert);
  if Owner.Owner.Control.UseRightToLeftAlignment then
  begin
    FCaptionBounds := TdxRightToLeftLayoutConverter.ConvertRect(FCaptionBounds, ABounds);
    FDescriptionBounds := TdxRightToLeftLayoutConverter.ConvertRect(FDescriptionBounds, ABounds);
    FImageBounds := TdxRightToLeftLayoutConverter.ConvertRect(FImageBounds, ABounds);
  end;
end;

procedure TdxRibbonGalleryGroupItemViewInfo.Paint(ACanvas: TcxCanvas);

  function IsSmallButtonSelected: Boolean;
  begin
    Result := not IsMergeItemsImages and (GetDowned or GetHotTracked or GetSelected);
  end;

  function CalculateSmallButtonState: Integer;
  begin
    if GetDowned then
      Result := DXBAR_PRESSED
    else
      if GetHotTracked then
      begin
        if GetSelected then
          Result := DXBAR_HOTCHECK
        else
          Result := DXBAR_ACTIVE;
      end
      else
        if GetSelected then
          Result := DXBAR_CHECKED
        else
          Result := 0;
  end;

  function IsDrawSelected: Boolean;
  begin
    Result := IsMergeItemsImages and IsImageVisible and (GetHotTracked or GetSelected);
  end;

  function CalculateSelectionState: Integer;
  begin
    if GetHotTracked then
    begin
      if GetSelected then
        Result := DXBAR_HOTCHECK
      else
        Result := DXBAR_HOT;
    end
    else
      if GetSelected then
        Result := DXBAR_CHECKED
      else
        Result := 0;
  end;

  function CalculateClipRect: TRect;
  var
    AControlViewInfo: TdxRibbonGalleryControlViewInfo;
  begin
    AControlViewInfo := Owner.Owner;
    Result := cxRectSetBottom(Bounds, Min(Bounds.Bottom,
      AControlViewInfo.Bounds.Bottom - AControlViewInfo.GetGalleryMargins.Bottom));
  end;

begin
  ACanvas.SaveState;
  try
    ACanvas.IntersectClipRect(CalculateClipRect);
    if IsSmallButtonSelected then
      Painter.DropDownGalleryDrawItem(ACanvas.Handle, Bounds, CalculateSmallButtonState, Owner.Owner.Control.ScaleFactor)
    else
      Owner.Owner.DrawBackground(ACanvas, Bounds);

    DrawImage(ACanvas.Canvas.Handle, FImageBounds);
    DrawItemText(ACanvas);
  finally
    ACanvas.RestoreState;
  end;
  if IsDrawSelected then
    Painter.DropDownGalleryDrawSelectionFrame(ACanvas, FImageBounds, CalculateSelectionState);
  FChanged := False;
end;

function TdxRibbonGalleryGroupItemViewInfo.CalculateImageSize: TSize;

  procedure FitToItemHeight(var AImageSize: TSize);
  var
    ANewImageHeight: Integer;
  begin
    if FPredefinedItemSize.cy <> 0 then
    begin
      ANewImageHeight := FPredefinedItemSize.cy - 2 * VerticalImageIndent;
      if AImageSize.cy > ANewImageHeight then
      begin
        AImageSize.cx := MulDiv(AImageSize.cx, ANewImageHeight, AImageSize.cy);
        AImageSize.cy := ANewImageHeight;
      end;
    end;
  end;

  procedure InscribeImage(var AImageSize: TSize);
  var
    APlaceSize: TSize;
    AFactor: Double;
  begin
    if cxRectIsEmpty(Bounds) then
      FitToItemHeight(AImageSize)
    else
    begin
      APlaceSize := GetImagePlaceSize;
      if (AImageSize.cx = 0) or (AImageSize.cy = 0) then
        AImageSize := cxNullSize
      else
        if (APlaceSize.cx < AImageSize.cx) or (APlaceSize.cy < AImageSize.cy) then
        begin
          if APlaceSize.cx / AImageSize.cx < APlaceSize.cy / AImageSize.cy then
            AFactor := APlaceSize.cx / AImageSize.cx
          else
            AFactor := APlaceSize.cy / AImageSize.cy;
          AImageSize.cx := Round(AImageSize.cx * AFactor);
          AImageSize.cy := Round(AImageSize.cy * AFactor);
        end;
    end
  end;

begin
  Result := GetUnsizedImageSize;
  InscribeImage(Result);
end;

procedure TdxRibbonGalleryGroupItemViewInfo.DrawImage(DC: HDC; const ARect: TRect);
begin
  cxDrawImage(DC, ARect, ARect, GetCurrentGlyph, GroupItem.Group.Images,
    GroupItem.ImageIndex, EnabledImageDrawModeMap[GroupItem.GetEnabled], True);
end;

procedure TdxRibbonGalleryGroupItemViewInfo.DrawItemText(ACanvas: TcxCanvas);

  procedure DrawText(ACanvas: TcxCanvas; const AText: string; const ARect: TRect; AFormat: UINT);
  begin
    if Owner.Owner.IsInRibbon and TdxInRibbonGalleryControlViewInfo(Owner.Owner).DrawWithoutBackground then
      cxDrawTextOpaque(ACanvas, AText, ARect, AFormat)
    else
      cxDrawText(ACanvas, AText, ARect, AFormat);
  end;

  function GetTextAlignmentFlags: DWORD;
  const
    TextAlignment: array [Boolean] of Cardinal = (DT_LEFT, DT_RIGHT);
  begin
    Result := DT_NOCLIP or DT_EXPANDTABS;
    if Options.GetItemImagePosition in [gipLeft, gipRight] then
      Result := Result or TextAlignment[GetGalleryItemControl.UseRightToLeftAlignment]
    else
      Result := Result or DT_CENTER;
    if FDescriptionRowCount = 1 then
      Result := DT_VCENTER or Result;
  end;

  procedure DrawDescription(ACanvas: TcxCanvas; ATextAlignmentFlags: DWORD);
  var
    ARect: TRect;
    ALineHeight, I: Integer;
  begin
    ALineHeight := cxTextHeight(ACanvas.Handle);
    ARect := FDescriptionBounds;
    for I := 0 to FDescriptionLines.Count - 1 do
    begin
      DrawText(ACanvas, FDescriptionLines[I], ARect, ATextAlignmentFlags);
      OffsetRect(ARect, 0, ALineHeight);
    end;
  end;

  function GetFontColor: TColor;
  begin
    if GroupItem.Selected then
      Result := Owner.Owner.SelectedTextColor
    else
      if HotGroupItem = GroupItem then
        Result := Owner.Owner.HotTextColor
      else
        if GroupItem.GetEnabled then
          Result := Owner.Owner.TextColor
        else
          Result := Owner.Owner.DisabledTextColor;
  end;

var
  ADescriptionVisible: Boolean;
  ATextAlignmentFlags: UINT;
begin
  ADescriptionVisible := IsDescriptionVisible;
  ACanvas.Font := Font;
  ACanvas.Font.Color := GetFontColor;
  ATextAlignmentFlags := GetTextAlignmentFlags;
  if IsCaptionVisible then
  begin
    CheckCaptionFontStyle(ACanvas.Font);
    DrawText(ACanvas, Caption, FCaptionBounds, ATextAlignmentFlags + DT_END_ELLIPSIS);
  end;
  if ADescriptionVisible then
  begin
    ACanvas.Font.Style := ACanvas.Font.Style - [fsBold];
    ValidateDescriptionStrings(ACanvas);
    DrawDescription(ACanvas, ATextAlignmentFlags);
  end;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetActualGroupItemIndent: Integer;
begin
  Result := ScaleFactor.Apply(dxRibbonGalleryGroupItemIndent);
end;

function TdxRibbonGalleryGroupItemViewInfo.GetCaption: string;
begin
  Result := GroupItem.Caption;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetCaptionHeight: Integer;
begin
  if IsCaptionVisible then
    Result := cxTextHeight(Font)
  else
    Result := 0;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetCaptionWidth: Integer;
var
  AFont: TFont;
begin
  if IsCaptionVisible then
  begin
    if FCaptionWidth = 0 then
    begin
      AFont := TFont.Create;
      try
        AFont.Assign(Font);
        CheckCaptionFontStyle(AFont);
        Result := cxTextWidth(AFont, GroupItem.Caption);
      finally
        AFont.Free;
      end;
      FCaptionWidth := Result;
    end
    else
      Result := FCaptionWidth;
  end
  else
    Result := 0;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetCurrentGlyph: TdxSmartGlyph;
begin
  if not GroupItem.GlyphInDropDown.Empty and not IsInplaceGallery then
    Result := GroupItem.GlyphInDropDown
  else
    Result := GroupItem.Glyph;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetDescription: string;
begin
  Result := GroupItem.Description;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetDescriptionHeight(
  var ADescriptionRect: TRect): Integer;
begin
  if IsDescriptionVisible then
  begin
    if FDescriptionSize.cy = 0 then
    begin
      if cxRectIsNull(ADescriptionRect) then
        ADescriptionRect := GetDescriptionRect(0);
      Result := ADescriptionRect.Bottom - ADescriptionRect.Top;
      FDescriptionSize.cy := Result;
    end
    else
      Result := FDescriptionSize.cy;
  end
  else
    Result := 0;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetDescriptionWidth(
  var ADescriptionRect: TRect; AMaxWidth: Integer): Integer;
begin
  if IsDescriptionVisible then
  begin
    if FDescriptionSize.cx = 0 then
    begin
      if cxRectIsNull(ADescriptionRect) then
        ADescriptionRect := GetDescriptionRect(AMaxWidth);
      Result := ADescriptionRect.Right - ADescriptionRect.Left;
      FDescriptionSize.cx := Result;
    end
    else
      Result := FDescriptionSize.cx;
  end
  else
    Result := 0;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetHotTracked: Boolean;

  function GetGeneralItemPullHighlightingMode: TdxRibbonGalleryItemPullHighlightingMode;
  begin
    Result := Owner.Owner.GalleryInMenuOptions.ItemPullHighlightingMode;
  end;

  function AgreeWithGeneralItemPullHighlighting: Boolean;
  begin
    Result := Options.ItemPullHighlightingMode = GetGeneralItemPullHighlightingMode;
  end;

  function GetIsInHotTrackChain: Boolean;
  begin
    Result := ((GetOuterGroupItem(GroupItem, HotGroupItem,
      Options.ItemPullHighlightingMode) <> GroupItem) or
      (GroupItem = HotGroupItem)) and (GetItemPullHighlightingIdentifier(GroupItem) =
      GetItemPullHighlightingIdentifier(HotGroupItem)) and
      ((GetGeneralItemPullHighlightingMode <> iphmNone) and AgreeWithGeneralItemPullHighlighting or
      (GetGeneralItemPullHighlightingMode = iphmNone) and (Options.ItemPullHighlightingMode <> iphmNone));
  end;

begin
  if not IsItemPullHighlighting then
    Result := IsThisGroupItem(HotGroupItem)
  else
    Result := (HotGroupItem <> nil) and GetIsInHotTrackChain;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetImageSize: TSize;
begin
  Result := dxGetImageSize(GetCurrentGlyph, GroupItem.Group.Images, 0, Owner.Owner.Control.ScaleFactor);
end;

function TdxRibbonGalleryGroupItemViewInfo.GetItemSize(AMaxWidth, AMaxHeight: Integer): TSize;
var
  AWidth, AHeight: Integer;
  AImageSize: TSize;
  ADescriptionRect: TRect;
  AMaxDescriptionWidth: Integer;
begin
  ADescriptionRect := cxNullRect;
  AImageSize := CalculateImageSize;
  case Options.GetItemImagePosition of
    gipTop, gipBottom:
      begin
        AMaxDescriptionWidth := AMaxWidth - 2 * HorizontalImageIndent;
        AWidth := Max(Max(GetCaptionWidth, GetDescriptionWidth(ADescriptionRect, AMaxDescriptionWidth)),
            AImageSize.cx) + 2 * HorizontalImageIndent;
        AHeight := AImageSize.cy + ItemHeightWithoutImage(ADescriptionRect);
      end;
  else
    AMaxDescriptionWidth := AMaxWidth - AImageSize.cx;
    AWidth := AImageSize.cx + ItemWidthWithoutImage(ADescriptionRect, AMaxDescriptionWidth);
    AHeight := Max(AImageSize.cy, GetCaptionHeight +
      GetDescriptionHeight(ADescriptionRect) +
      GetSpaceBetweenItemCaptionAndDescription) + 2 * VerticalImageIndent;
  end;
  if AMaxWidth > 0 then
    AWidth := Min(AMaxWidth, AWidth);
  if AMaxHeight > 0 then
    AHeight := Min(AMaxHeight, AHeight);
  Result.cx := AWidth;
  Result.cy := AHeight;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetSpaceBetweenItemCaptionAndDescription: Integer;
begin
  if IsCaptionVisible and IsDescriptionVisible then
    Result := Options.GetSpaceBetweenItemCaptionAndDescription + GetActualGroupItemIndent
  else
    Result := 0;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetSpaceBetweenItemImageAndText: Integer;
begin
  if IsCaptionVisible and CanUseSize(GetUnsizedImageSize) then
    Result := Options.GetSpaceBetweenItemImageAndText + GetActualGroupItemIndent
  else
    Result := 0;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetUnsizedImageSize: TSize;

  function InscribeInDimension(const AImageSize, ASize: TSize): TSize;
  var
    AFactor: Double;
  begin
    if CanUseSize(AImageSize) then
    begin
      if ASize.cx > 0 then
        AFactor := ASize.cx / AImageSize.cx
      else
        AFactor := ASize.cy / AImageSize.cy;
      Result.cx := Round(AImageSize.cx * AFactor);
      Result.cy := Round(AImageSize.cy * AFactor);
    end
    else
      Result := AImageSize;
  end;

var
  ASize: TSize;
begin
  ASize := Options.ItemImageSize.Size;
  Result := ASize;
  if not CanUseSize(Result) and IsImageVisible then
  begin
    Result := GetImageSize;
    if (ASize.cx > 0) or (ASize.cy > 0) then
      Result := InscribeInDimension(Result, ASize);
  end;
end;

function TdxRibbonGalleryGroupItemViewInfo.IsImageAssigned: Boolean;
begin
  Result := cxGraphics.IsImageAssigned(GetCurrentGlyph, GroupItem.Group.Images, GroupItem.ImageIndex);
end;

function TdxRibbonGalleryGroupItemViewInfo.GetCaptionBounds: TRect;
begin
  Result.Left := GetTextLeft;
  Result.Top := GetTextTop;
  Result.Right := GetTextRight;
  Result.Bottom := Result.Top + GetCaptionHeight;
  if not (IsImageVisible or IsDescriptionVisible) then
    Result := cxRectCenter(Bounds, cxSize(Result.Right - Result.Left,
      Result.Bottom - Result.Top));
end;

function TdxRibbonGalleryGroupItemViewInfo.GetDescriptionBounds: TRect;
var
  ATempRect: TRect;
begin
  Result.Left := GetTextLeft;
  Result.Top := GetTextTop + GetCaptionHeight +
    GetSpaceBetweenItemCaptionAndDescription;
  Result.Right := GetTextRight;
  ATempRect := cxNullRect;
  Result.Bottom := Result.Top + GetDescriptionHeight(ATempRect);
end;

function TdxRibbonGalleryGroupItemViewInfo.GetImageBounds: TRect;
begin
  if not IsCaptionVisible then
    Result := cxRectCenter(Bounds, CalculateImageSize)
  else
  begin
    case Options.GetItemImagePosition of
      gipLeft:
        begin
          Result.Left := Bounds.Left + HorizontalImageIndent;
          Result.Top := Bounds.Top + VerticalImageIndent;
          Result.Right := Result.Left + CalculateImageSize.cx;
          Result.Bottom := Bounds.Bottom - VerticalImageIndent;
        end;
      gipTop:
        begin
          Result.Left := Bounds.Left + HorizontalImageIndent;
          Result.Top := Bounds.Top + VerticalImageIndent;
          Result.Right := Bounds.Right - HorizontalImageIndent;
          Result.Bottom := Result.Top + CalculateImageSize.cy;
        end;
      gipRight:
        begin
          Result.Right := Bounds.Right - HorizontalImageIndent;
          Result.Top := Bounds.Top + VerticalImageIndent;
          Result.Left := Result.Right - CalculateImageSize.cx;
          Result.Bottom := Bounds.Bottom - VerticalImageIndent;
        end;
    else
      Result.Bottom := Bounds.Bottom - VerticalImageIndent;
      Result.Left := Bounds.Left + HorizontalImageIndent;
      Result.Right := Bounds.Right - HorizontalImageIndent;
      Result.Top := Result.Bottom - CalculateImageSize.cy;
    end;
    Result := cxRectCenter(Result, CalculateImageSize);
  end;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetTextLeft: Integer;
begin
  Result := Bounds.Left;
  if Options.GetItemImagePosition = gipLeft then
  begin
    if CanUseSize(CalculateImageSize) then
      Inc(Result, CalculateImageSize.cx + Options.GetSpaceBetweenItemImageAndText + GetActualGroupItemIndent);
  end;
  Inc(Result, GetActualGroupItemIndent);
end;

function TdxRibbonGalleryGroupItemViewInfo.GetTextRight: Integer;
begin
  Result := Bounds.Right;
  if Options.GetItemImagePosition = gipRight then
  begin
    if CanUseSize(CalculateImageSize) then
      Dec(Result, CalculateImageSize.cx + Options.GetSpaceBetweenItemImageAndText + GetActualGroupItemIndent);
  end;
  Dec(Result, GetActualGroupItemIndent);
end;

function TdxRibbonGalleryGroupItemViewInfo.GetTextTop: Integer;
begin
  Result := Bounds.Top;
  if Options.GetItemImagePosition = gipTop then
  begin
    if CanUseSize(CalculateImageSize) then
      Inc(Result, CalculateImageSize.cy + Options.GetSpaceBetweenItemImageAndText + GetActualGroupItemIndent);
  end;
  Inc(Result, GetActualGroupItemIndent);
end;

function TdxRibbonGalleryGroupItemViewInfo.IsBoldCaption: Boolean;
begin
  Result := not IsInplaceGallery and (Options.ItemTextKind in [itkCaptionAndDescription]);
end;

procedure TdxRibbonGalleryGroupItemViewInfo.CheckTextVerticalAlignment(AVerticalAlignment: TcxAlignmentVert);
var
  ARect, ATextRect: TRect;
begin
  ARect := cxRectInflate(Bounds, -GetActualGroupItemIndent);
  case Options.GetItemImagePosition of
    gipLeft:
      ARect.Left := FCaptionBounds.Left;
    gipRight:
      ARect.Right := FCaptionBounds.Right;
    gipTop:
      ARect.Top := FCaptionBounds.Top;
    gipBottom:
      ARect.Bottom := FImageBounds.Top - GetActualGroupItemIndent;
  end;

  ATextRect := cxRectUnion(FCaptionBounds, FDescriptionBounds);

  case AVerticalAlignment of
    vaBottom:
      ATextRect := cxRectSetBottom(ATextRect, ARect.Bottom);
    vaCenter:
      ATextRect := cxRectCenterVertically(ARect, cxRectHeight(ATextRect));
  end;

  FCaptionBounds := cxRectSetTop(FCaptionBounds, ATextRect.Top);
  FDescriptionBounds := cxRectSetTop(FDescriptionBounds, FCaptionBounds.Bottom + GetActualGroupItemIndent);
end;

procedure TdxRibbonGalleryGroupItemViewInfo.ResetCachedValues;
begin
  FCaptionVisibilityState := gbsIndefinite;
  FDescriptionVisibilityState := gbsIndefinite;
  FChanged := True;
end;

procedure TdxRibbonGalleryGroupItemViewInfo.SetPredefinedItemSize(const AValue: TSize);
begin
  FPredefinedItemSize := AValue;
end;

procedure TdxRibbonGalleryGroupItemViewInfo.CheckCaptionFontStyle(AFont: TFont);
begin
  if IsBoldCaption then
    AFont.Style := AFont.Style + [fsBold];
end;

function TdxRibbonGalleryGroupItemViewInfo.GetDescriptionLength: Integer;
begin
  Result := cxTextWidth(GetFont, GroupItem.Description);
end;

function TdxRibbonGalleryGroupItemViewInfo.GetDescriptionRect(AMaxWidth: Integer): TRect;

  function GetMinDescriptionWidth: Integer;
  var
    R: TRect;
  begin
    R := Rect(0, 0, 1, 1);
    cxDrawText(GetGalleryItemControl.Canvas.Handle, Description, R, DT_WORDBREAK {or DT_NOFULLWIDTHCHARBREAK} or DT_CALCRECT);
    Result := R.Right;
  end;

var
  ASuccess, AIsDescriptionNeedRecalculation: Boolean;
begin
  AIsDescriptionNeedRecalculation := AMaxWidth > 0;
  if not IsInplaceGallery and not AIsDescriptionNeedRecalculation then
    Result := TdxRibbonOnMenuGalleryControlViewInfo(Owner.Owner).GetGroupItemDescriptionRect(
      GroupItem.Group.Index, GroupItem.Index)
  else
    Result := cxNullRect;
  if cxRectIsNull(Result) then
  begin
    GetGalleryItemControl.Canvas.SaveState;
    try
      GetGalleryItemControl.Canvas.Font := Font;
      repeat
        Result := cxGetTextRect(GetGalleryItemControl.Canvas.Handle, Description,
          FDescriptionRowCount, False, DT_EXPANDTABS);
        if GetMinDescriptionWidth > AMaxWidth then
          ASuccess := True
        else
          if AIsDescriptionNeedRecalculation then
          begin
            ASuccess := Result.Right - Result.Left <= AMaxWidth;
            if not ASuccess then
              Inc(FDescriptionRowCount);
          end
          else
            ASuccess := True;
      until ASuccess;
    finally
      GetGalleryItemControl.Canvas.RestoreState;
    end;
    if not IsInplaceGallery then
      TdxRibbonOnMenuGalleryControlViewInfo(Owner.Owner).SetGroupItemDescriptionRect(
      GroupItem.Group.Index, GroupItem.Index, Result);
  end;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetDowned: Boolean;
begin
  Result := IsThisGroupItem(Owner.Owner.DownedGroupItem);
end;

function TdxRibbonGalleryGroupItemViewInfo.GetHotGroupItem: TdxRibbonGalleryGroupItem;
begin
  Result := Owner.Owner.HotGroupItem;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetHorizontalImageIndent: Integer;
begin
  if Options.RemoveHorizontalItemPadding then
    Result := 0
  else
    Result := GetActualGroupItemIndent;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetImagePlaceSize: TSize;
var
  AWidth, AHeight: Integer;
begin
  AWidth := Bounds.Right - Bounds.Left;
  AHeight := Bounds.Bottom - Bounds.Top;
  Result.cx := Max(0, AWidth - 2 * HorizontalImageIndent);
  Result.cy := Max(0, AHeight - 2 * VerticalImageIndent);
end;

function TdxRibbonGalleryGroupItemViewInfo.GetIsItemPullHighlighting: Boolean;
begin
  Result := FGroupItem.Enabled and (Options.ItemPullHighlightingMode <> iphmNone) and
    not Owner.Owner.IsInRibbon;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetOptions: TdxRibbonGalleryGroupOptions;
begin
  Result := Owner.Group.Options;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetPainter: TdxBarPainter;
begin
  Result := Owner.Painter;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetRectConsiderBounds(
  const ARect: TRect): TRect;
begin
  if cxRectContain(Bounds, ARect) then
    Result := ARect
  else
    Result := cxNullRect;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetRibbonSkin: IdxSkin;
begin
  Result := (Painter as TdxBarSkinnedPainter).Skin;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetSelected: Boolean;
begin
  Result := GroupItem.Selected;
end;

function TdxRibbonGalleryGroupItemViewInfo.GetVerticalImageIndent: Integer;
begin
  if Options.RemoveVerticalItemPadding then
    Result := 0
  else
    if IsInplaceGallery and (not IsCaptionVisible or (Options.ItemImagePosition in [gipLeft, gipRight]) or not IsImageVisible) then
      Result := ScaleFactor.Apply(dxRibbonGalleryGroupItemInRibbonImageIndent)
    else
      Result := GetActualGroupItemIndent
end;

function TdxRibbonGalleryGroupItemViewInfo.IsCaptionVisible: Boolean;
begin
  if FCaptionVisibilityState = gbsIndefinite then
  begin
    Result := (not IsInplaceGallery and (Options.ItemTextKind in [itkCaption, itkCaptionAndDescription]) or
      IsInplaceGallery and (GroupItem.Group.GalleryItem.GalleryInRibbonOptions.AlwaysShowItemCaption or not IsImageVisible)) and
      (Caption <> '') and (not IsMergeItemsImages or not IsImageVisible);
    if Result then
      FCaptionVisibilityState := gbsTrue
    else
      FCaptionVisibilityState := gbsFalse;
  end
  else
    Result := FCaptionVisibilityState = gbsTrue;
end;

function TdxRibbonGalleryGroupItemViewInfo.IsDescriptionVisible: Boolean;
begin
  if FDescriptionVisibilityState = gbsIndefinite then
  begin
    Result := not IsInplaceGallery and
      (Options.ItemTextKind in [itkCaptionAndDescription]) and
      (Description <> '') and (Caption <> '') and not IsMergeItemsImages;
    if Result then
      FDescriptionVisibilityState := gbsTrue
    else
      FDescriptionVisibilityState := gbsFalse;
  end
  else
    Result := FDescriptionVisibilityState = gbsTrue;
end;

function TdxRibbonGalleryGroupItemViewInfo.IsImageVisible: Boolean;
begin
  Result := IsImageAssigned;
end;

function TdxRibbonGalleryGroupItemViewInfo.IsInplaceGallery: Boolean;
begin
  Result := Owner.Owner.IsInRibbon;
end;

function TdxRibbonGalleryGroupItemViewInfo.IsMergeItemsImages: Boolean;
begin
  Result := (Options.RemoveHorizontalItemPadding or
    Options.RemoveVerticalItemPadding) and IsImageVisible;
end;

function TdxRibbonGalleryGroupItemViewInfo.IsThisGroupItem(
  AGroupItem: TdxRibbonGalleryGroupItem): Boolean;
begin
  Result := (AGroupItem = GroupItem) and (AGroupItem <> nil);
end;

function TdxRibbonGalleryGroupItemViewInfo.ItemHeightWithoutImage(
  var ADescriptionRect: TRect): Integer;
begin
  Result := GetCaptionHeight + GetDescriptionHeight(ADescriptionRect) +
    GetSpaceBetweenItemCaptionAndDescription + GetSpaceBetweenItemImageAndText +
    2 * VerticalImageIndent;
end;

function TdxRibbonGalleryGroupItemViewInfo.ItemWidthWithoutImage(
  var ADescriptionRect: TRect; AMaxWidth: Integer): Integer;
var
  AIndent: Integer;
begin
  AIndent := GetSpaceBetweenItemImageAndText + 2 * HorizontalImageIndent;
  Result := Max(GetCaptionWidth, GetDescriptionWidth(ADescriptionRect,
    AMaxWidth - AIndent)) + AIndent;
  if AMaxWidth > 0 then
    Result := Min(AMaxWidth, Result);
end;

procedure TdxRibbonGalleryGroupItemViewInfo.ValidateDescriptionStrings(
  ACanvas: TcxCanvas);
begin
  if FChanged then
  begin
    FDescriptionLines.Clear;
    cxGetTextLines(Description, ACanvas, FDescriptionBounds, FDescriptionLines);
  end;
end;

{ TdxRibbonGalleryGroupViewInfo }

constructor TdxRibbonGalleryGroupViewInfo.Create(
  AOwner: TdxRibbonGalleryControlViewInfo; AGroup: TdxRibbonGalleryGroup);
var
  AItemSize: TSize;
  AMaxItemWidth, AMaxItemHeight: Integer;
begin
  inherited Create;
  FGroup := AGroup;
  FOwner := AOwner;
  FHeader := TdxRibbonGalleryGroupHeaderViewInfo.Create(Self);
  FItems := TcxObjectList.Create;

  if CanUseSize(Owner.GlobalItemSize) then
    FItemSize := Owner.GlobalItemSize
  else
  begin
    if Owner.IsInRibbon then
    begin
      AItemSize := Owner.GalleryItem.GalleryOptions.ItemSizeInRibbon.Size;
      {if not CanUseSize(AItemSize) then
        AItemSize := Group.Options.ItemSize.Size;}
    end
    else
      AItemSize := Group.Options.ItemSize.Size;
    if CanUseSize(AItemSize) then
      FItemSize := AItemSize
    else
    begin
      AMaxItemWidth := 0;
      if AItemSize.cx > 0 then
        AMaxItemWidth := AItemSize.cx;
      AMaxItemHeight := 0;
      if AItemSize.cy > 0 then
        AMaxItemHeight := AItemSize.cy;
      FItemSize := GetItemSize(AMaxItemWidth, AMaxItemHeight);
    end;
  end;
end;

destructor TdxRibbonGalleryGroupViewInfo.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FHeader);
  inherited Destroy;
end;

procedure TdxRibbonGalleryGroupViewInfo.Calculate(AGroupTop,
  AGroupBottom: Integer; const AControlClientRect: TRect);
var
  ARowIndex, ARowTop, ARowHeight, AColumnCount, ARowCount,
    AItemIndex, AColumnIndex, AColumnLeft, AGroupWidth: Integer;
  AItemBounds: TRect;
begin
  FBounds := Rect(AControlClientRect.Left, AGroupTop, AControlClientRect.Right,
    AGroupBottom);
  Header.Calculate(GetHeaderBounds(Bounds));

  ClearItems;
  ARowIndex := 0;
  ARowHeight := GetRowHeight;
  AGroupWidth := GetGroupWidth;
  AColumnCount := GetColumnCount(AGroupWidth);
  ARowCount := GetRowCount(AGroupWidth);
  while ARowIndex <= ARowCount - 1 do
  begin
    ARowTop := GetRowTop(ARowIndex, AGroupTop, AGroupWidth);
    if AreLinesIntersectedStrictly(ARowTop, ARowTop + ARowHeight,
      AControlClientRect.Top, AControlClientRect.Bottom) then
    begin
      AColumnIndex := 0;
      for AItemIndex := GetFirstItemInGroupRow(ARowIndex, AColumnCount) to
        GetLastItemInGroupRow(ARowIndex, AColumnCount) do
      begin
        AColumnLeft := GetColumnLeft(AColumnIndex, Bounds.Left);
        AItemBounds := Rect(AColumnLeft, ARowTop,
          AColumnLeft + FItemSize.cx, ARowTop + FItemSize.cy);
        if Owner.Control.UseRightToLeftAlignment then
          AItemBounds := TdxRightToLeftLayoutConverter.ConvertRect(AItemBounds, Bounds);
        CreateGroupItem(AItemIndex, AItemBounds);
        Inc(AColumnIndex);
      end;
    end;
    Inc(ARowIndex);
  end;
end;

function TdxRibbonGalleryGroupViewInfo.GetHeight(AWidth: Integer): Integer;
var
  ARowCount: Integer;
begin
  ARowCount := GetRowCount(AWidth);
  Result := Header.GetHeight(AWidth, True) + ARowCount * GetRowHeight -
    IfThen(ARowCount > 0, GetSpaceBetweenItems(False));
end;

procedure TdxRibbonGalleryGroupViewInfo.Paint(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  Header.Paint(ACanvas);
  for I := 0 to ItemCount - 1 do
    Items[I].Paint(ACanvas);
end;

function TdxRibbonGalleryGroupViewInfo.CalculateItemSize(
  const APredefinedItemSize: TSize; AMaxItemWidth, AMaxItemHeight: Integer): TSize;
var
  AItem: TdxRibbonGalleryGroupItemViewInfo;
  AItemSize: TSize;
  ASetItemSize: Boolean;
  I: Integer;
begin
  Result := cxNullSize;
  ASetItemSize := CanUseSize(APredefinedItemSize);
  for I := 0 to Group.Items.Count - 1 do
  begin
    AItem := TdxRibbonGalleryGroupItemViewInfo.Create(Self, Group.Items[I]);
    try
      if ASetItemSize then
        AItem.SetPredefinedItemSize(APredefinedItemSize);
      AItemSize := AItem.GetItemSize(AMaxItemWidth, AMaxItemHeight);
    finally
      AItem.Free;
    end;
    Result := cxSizeMax(Result, AItemSize);
  end;
end;

procedure TdxRibbonGalleryGroupViewInfo.ClearItems;
begin
  FItems.Clear;
end;

procedure TdxRibbonGalleryGroupViewInfo.CorrectItemWidth(ABounds: TRect);
var
  AMaxItemWidth: Integer;
begin
  AMaxItemWidth := cxRectWidth(ABounds) - (Owner.GetLeftLayoutIndent + Owner.GetRightLayoutIndent);
  if FItemSize.cx > AMaxItemWidth then
  begin
    Owner.GalleryChanged;
    FItemSize := GetItemSize(AMaxItemWidth, 0);
  end;
end;

procedure TdxRibbonGalleryGroupViewInfo.CreateGroupItem(AItemIndex: Integer;
  const ABounds: TRect);
var
  AGroupItem: TdxRibbonGalleryGroupItem;
  AGroupItemViewInfo: TdxRibbonGalleryGroupItemViewInfo;
begin
  AGroupItem := FGroup.Items[AItemIndex];
  AGroupItemViewInfo := TdxRibbonGalleryGroupItemViewInfo.Create(Self, AGroupItem);
  AGroupItemViewInfo.Calculate(ABounds);
  FItems.Add(AGroupItemViewInfo);
end;

function TdxRibbonGalleryGroupViewInfo.GetColumnLeft(AColumnIndex: Integer; AGroupLeft: Integer): Integer;
begin
  Result := AGroupLeft + Owner.GetLeftLayoutIndent + (FItemSize.cx + GetSpaceBetweenItems(True)) * AColumnIndex;
end;

function TdxRibbonGalleryGroupViewInfo.GetColumnCount(AWidth: Integer): Integer;
var
  ADenominator: Integer;
begin
  ADenominator := FItemSize.cx + GetSpaceBetweenItems(True);
  if ADenominator <> 0 then
    Result := (AWidth - (Owner.GetLeftLayoutIndent + Owner.GetRightLayoutIndent) + GetSpaceBetweenItems(True)) div ADenominator
  else
    Result := 0;
  Result := Min(Result, Group.GalleryItem.GalleryOptions.ColumnCount);
end;

function TdxRibbonGalleryGroupViewInfo.GetColumnCountInRow(ARow: Integer; AGroupWidth: Integer): Integer;
var
  AGroupColumnCount: Integer;
begin
  AGroupColumnCount := GetColumnCount(AGroupWidth);
  if AGroupColumnCount <> 0 then
  begin
    if (ARow + 1) * AGroupColumnCount > Group.Items.Count then
      Result := Group.Items.Count mod AGroupColumnCount
    else
      Result := AGroupColumnCount;
  end
  else
    Result := 0;
end;

function TdxRibbonGalleryGroupViewInfo.GetColumnWidth: Integer;
begin
  Result := FItemSize.cx + GetSpaceBetweenItems(True);
end;

function TdxRibbonGalleryGroupViewInfo.GetGroupWidth: Integer;
begin
  Result := FBounds.Right - FBounds.Left;
end;

function TdxRibbonGalleryGroupViewInfo.GetHeaderBounds(AGroupBounds: TRect): TRect;
begin
  Result := AGroupBounds;
  Result.Bottom := Result.Top + Header.GetHeight(AGroupBounds.Right - AGroupBounds.Left, False);
end;

function TdxRibbonGalleryGroupViewInfo.GetItemColumn(AIndex: Integer; AGroupWidth: Integer): Integer;
var
  AColumnCount: Integer;
begin
  AColumnCount := GetColumnCount(AGroupWidth);
  if AColumnCount <> 0 then
    Result := AIndex mod AColumnCount
  else
    Result := 0;
end;

function TdxRibbonGalleryGroupViewInfo.GetItemIndex(ARow, AColumn: Integer; AGroupWidth: Integer): Integer;
begin
  Result := ARow * GetColumnCount(AGroupWidth) + AColumn;
end;

function TdxRibbonGalleryGroupViewInfo.GetItemRow(AGroupItemIndex: Integer; AGroupWidth: Integer): Integer;
var
  AColumnCount: Integer;
begin
  AColumnCount := GetColumnCount(AGroupWidth);
  if AColumnCount <> 0 then
    Result := AGroupItemIndex div AColumnCount
  else
    Result := 0;
end;

function TdxRibbonGalleryGroupViewInfo.GetLastItemInGroupRow(ARowIndex, AColumnCount: Integer): Integer;
var
  AGroupItemCount: Integer;
begin
  Result := GetFirstItemInGroupRow(ARowIndex, AColumnCount) + AColumnCount - 1;
  AGroupItemCount := FGroup.Items.Count;
  if Result > AGroupItemCount - 1 then
    Result := AGroupItemCount - 1;
end;

function TdxRibbonGalleryGroupViewInfo.GetRowCount(AGroupWidth: Integer): Integer;

  function CalcRowCount(AColumnCount: Integer): Integer;
  var
    AGroupItemCount: Integer;
  begin
    AGroupItemCount := FGroup.Items.Count;
    if AColumnCount <> 0 then
      Result := Ceil(AGroupItemCount / AColumnCount)
    else
      Result := 0;
  end;

begin
  Result := CalcRowCount(GetColumnCount(AGroupWidth));
end;

function TdxRibbonGalleryGroupViewInfo.GetRowHeight: Integer;
begin
  Result := FItemSize.cy + GetSpaceBetweenItems(False);
end;

function TdxRibbonGalleryGroupViewInfo.GetRowTop(ARowIndex: Integer; AGroupTop: Integer; AGroupWidth: Integer): Integer;
begin
  Result := AGroupTop + Header.GetHeight(AGroupWidth, True) + GetRowHeight * ARowIndex;
end;

function TdxRibbonGalleryGroupViewInfo.GetSpaceBetweenItems(AIsFlat: Boolean): Integer;
begin
  if (Options.RemoveHorizontalItemPadding and AIsFlat) or (Options.RemoveVerticalItemPadding and not AIsFlat) then
    Result := 0
  else
    if Owner.IsInRibbon then
    begin
      if AIsFlat then
        Result := Owner.GalleryItem.GalleryOptions.SpaceBetweenItemsHorizontally
      else
        Result := Owner.GalleryItem.GalleryOptions.SpaceBetweenItemsVertically;
    end
    else
      if AIsFlat then
        Result := Options.SpaceBetweenItemsHorizontally
      else
        Result := Options.SpaceBetweenItemsVertically;
end;

procedure TdxRibbonGalleryGroupViewInfo.RepaintChainOfItems(
  AnItemIndex: Integer; IsHotTrack: Boolean; ACanvas: TcxCanvas;
  APart: TdxRibbonGalleryGroupRepaintPart = ggrpAll; AnItemIndex2: Integer = 0);

  function GetGroupItemViewInfoIndex(AGroupItemIndex: Integer): Integer;
  var
    I: Integer;
    AFound, IsIndexGreater: Boolean;
  begin
    Result := -1;
    AFound := False;
    IsIndexGreater := False;
    I := 0;
    while (I < ItemCount) and not AFound do
    begin
      if Items[I].GroupItem.Index = AGroupItemIndex then
      begin
        Result := I;
        AFound := True;
      end;
      IsIndexGreater := Items[I].GroupItem.Index < AGroupItemIndex;
      Inc(I);
    end;
    if Result = -1 then
      if IsIndexGreater then
        Result := ItemCount - 1
      else
        Result := 0;
  end;

var
  I, AFinish: Integer;
begin
  case APart of
    ggrpBefore:
      begin
        I := 0;
        AFinish := AnItemIndex;
      end;
    ggrpAfter:
      begin
        I := AnItemIndex;
        AFinish := ItemCount - 1;
      end;
    ggrpBetween:
      begin
        I := AnItemIndex;
        AFinish := AnItemIndex2;
      end;
  else
    I := 0;
    AFinish := ItemCount - 1;
  end;
  if APart in [ggrpAfter, ggrpBetween] then
    I := GetGroupItemViewInfoIndex(I);
  if APart in [ggrpBefore, ggrpBetween] then
    AFinish := GetGroupItemViewInfoIndex(AFinish);
  if AFinish < FItems.Count then
  while I <= AFinish do
  begin
    Items[I].Paint(ACanvas);
    Inc(I);
  end;
end;

procedure TdxRibbonGalleryGroupViewInfo.SetBounds(const ABounds: TRect);
begin
  FBounds := ABounds;
end;

function TdxRibbonGalleryGroupViewInfo.GetFirstItemInGroupRow(ARowIndex,
  AColumnCount: Integer): Integer;
begin
  Result := ARowIndex * AColumnCount;
end;

function TdxRibbonGalleryGroupViewInfo.GetFont: TFont;
begin
  Result := Owner.Control.Parent.Font;
end;

function TdxRibbonGalleryGroupViewInfo.GetItem(
  Index: Integer): TdxRibbonGalleryGroupItemViewInfo;
begin
  Result := TdxRibbonGalleryGroupItemViewInfo(FItems[Index]);
end;

function TdxRibbonGalleryGroupViewInfo.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxRibbonGalleryGroupViewInfo.GetItemSize(AMaxItemWidth, AMaxItemHeight: Integer): TSize;
var
  AStoredItemSize: TSize;
begin
  AStoredItemSize := Owner.GetGroupItemStoredSize(FGroup.Index);
  if CanUseSize(AStoredItemSize) then
    Result := AStoredItemSize
  else
  begin
    Result := CalculateItemSize(cxNullSize, AMaxItemWidth, AMaxItemHeight);
    Owner.SetGroupItemStoredSize(Result, FGroup.Index);
  end;
end;

function TdxRibbonGalleryGroupViewInfo.GetOptions: TdxRibbonGalleryGroupOptions;
begin
  Result := Group.Options;
end;

function TdxRibbonGalleryGroupViewInfo.GetPainter: TdxBarPainter;
begin
  Result := Owner.Control.Painter;
end;

{ TdxRibbonGalleryControlViewInfo }

constructor TdxRibbonGalleryControlViewInfo.Create(AControl: TdxBarItemControl);
begin
  inherited Create(AControl);
  FGroups := TcxObjectList.Create;
  FDownedGroupItem := nil;
  FHotGroupItem := nil;
  GalleryChanged;
end;

destructor TdxRibbonGalleryControlViewInfo.Destroy;
begin
  FreeAndNil(FGroups);
  inherited Destroy;
end;

procedure TdxRibbonGalleryControlViewInfo.Calculate(ALayoutOffset: Integer;
  AScrollCode: TScrollCode);
begin
  CalculateGlobalItemSize;
end;

procedure TdxRibbonGalleryControlViewInfo.Paint(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  DrawBackground(ACanvas, GetControlBounds);
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(GalleryBounds);
    for I := 0 to GroupCount - 1 do
      Groups[I].Paint(ACanvas);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxRibbonGalleryControlViewInfo.DisplayGroupItem(
  AGroupItem: TdxRibbonGalleryGroupItem);
begin
// do nothing
end;

procedure TdxRibbonGalleryControlViewInfo.DrawSelectedGroupItem(
  ASelectedGroupItem, AOldSelectedGroupItem: TdxRibbonGalleryGroupItem);
begin
  Control.Canvas.SaveState;
  try
    DrawGroupItem(AOldSelectedGroupItem);
    DrawGroupItem(ASelectedGroupItem);
  finally
    Control.Canvas.RestoreState;
  end;
end;

procedure TdxRibbonGalleryControlViewInfo.GalleryChanged;
var
  AGroupCount: Integer;
  I: Integer;
begin
  AGroupCount := Control.GetGroups.Count;
  SetLength(FGroupItemStoredSizes, AGroupCount);
  for I := 0 to AGroupCount - 1 do
    FGroupItemStoredSizes[I] := cxNullSize;
end;

function TdxRibbonGalleryControlViewInfo.GetAbsoluteGroupTop(
  AGroupIndex: Integer; AWidth: Integer): Integer;
var
  AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
  ADestroyAfterUse: Boolean;
  I: Integer;
begin
  Result := 0;
  for I := 0 to AGroupIndex - 1 do
    if GalleryItem.IsGroupVisible(I) then
    begin
      AGroupViewInfo := GetGroupViewInfo(Control.GetGroups, Self, I,
        ADestroyAfterUse);
      try
        Result := Result + AGroupViewInfo.GetHeight(AWidth) +
          IfThen(AWidth > 0, GalleryOptions.SpaceBetweenGroups);
      finally
        if ADestroyAfterUse then
          AGroupViewInfo.Free;
      end;
    end;
end;

function TdxRibbonGalleryControlViewInfo.GetControlBounds: TRect;
begin
  Result := Bounds;
end;

function TdxRibbonGalleryControlViewInfo.GetGalleryBounds: TRect;
var
  AMargins: TRect;
begin
  AMargins := GetGalleryMargins;
  if Control.UseRightToLeftScrollbar then
  begin
    AMargins := TdxRightToLeftLayoutConverter.ConvertOffsets(AMargins);
    Inc(AMargins.Left, GetScrollBarWidth);
  end
  else
    Inc(AMargins.Right, GetScrollBarWidth);
  Result := cxRectContent(GetControlBounds, AMargins);
end;

function TdxRibbonGalleryControlViewInfo.GetGalleryHeight(
  AWidth: Integer): Integer;
begin
  Result := 1;
end;

function TdxRibbonGalleryControlViewInfo.GetGroupItemCount(
  ALastGroupIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ALastGroupIndex do
    if GalleryItem.IsGroupVisible(I) then
      Result := Result + Control.GetGroups[I].Items.Count;
end;

function TdxRibbonGalleryControlViewInfo.GetHeight(AWidth: Integer): Integer;
begin
  Result := 0;
end;

function TdxRibbonGalleryControlViewInfo.GetInRibbonGalleryState: Integer;
begin
  Result := TdxBarSkinnedPainterAccess(Painter).GetPartState(Control.DrawParams, icpControl);
end;

function TdxRibbonGalleryControlViewInfo.GetMaxGroupItemSize: TSize;
var
  I: Integer;
  AGroupItemSize: TSize;
begin
  if CanUseSize(GlobalItemSize) then
    Result := GlobalItemSize
  else
  begin
    Result := cxNullSize;
    for I := 0 to Control.GetGroups.Count - 1 do
    begin
      AGroupItemSize := GetGroupItemSize(I);
      Result.cx := Max(Result.cx, AGroupItemSize.cx);
      Result.cy := Max(Result.cy, AGroupItemSize.cy);
    end;
  end;
end;

function TdxRibbonGalleryControlViewInfo.GetGroupItem(const P: TPoint): TdxRibbonGalleryGroupItem;
begin
  Result := GetGroupItem(P.X, P.Y);
end;

function TdxRibbonGalleryControlViewInfo.GetGroupItem(X, Y: Integer): TdxRibbonGalleryGroupItem;
var
  I, J: Integer;
  AGroupItemFound: Boolean;
begin
  Result := nil;
  AGroupItemFound := False;
  I := 0;
  while (I < GroupCount) and not AGroupItemFound do
  begin
    if cxRectPtIn(Groups[I].Bounds, X, Y) then
    begin
      J := 0;
      while (J < Groups[I].ItemCount) and not AGroupItemFound do
      begin
        if cxRectPtIn(Groups[I].Items[J].Bounds, X, Y) then
        begin
          Result := Groups[I].Items[J].GroupItem;
          AGroupItemFound := True;
        end;
        Inc(J);
      end;
    end;
    Inc(I);
  end;
end;

function TdxRibbonGalleryControlViewInfo.GetGroupItemStoredSize(
  AGroupIndex: Integer): TSize;
begin
  if AGroupIndex < Length(FGroupItemStoredSizes) then
    Result := FGroupItemStoredSizes[AGroupIndex]
  else
    Result := cxNullSize;
end;

function TdxRibbonGalleryControlViewInfo.GetGroupItemViewInfo(
  AGroupItem: TdxRibbonGalleryGroupItem): TdxRibbonGalleryGroupItemViewInfo;

  function GetGroupViewInfo(AGroup: TdxRibbonGalleryGroup): TdxRibbonGalleryGroupViewInfo;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to GroupCount - 1 do
      if Groups[I].Group = AGroup then
      begin
        Result := Groups[I];
        Break;
      end;
  end;

var
  AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
  I: Integer;
begin
  Result := nil;
  if AGroupItem = nil then Exit;
  AGroupViewInfo := GetGroupViewInfo(GalleryItem.GalleryGroups[AGroupItem.Group.Index]);
  if AGroupViewInfo <> nil then
    for I := 0 to AGroupViewInfo.ItemCount - 1 do
      if AGroupViewInfo.Items[I].GroupItem = AGroupItem then
      begin
        Result := AGroupViewInfo.Items[I];
        Break;
      end;
end;

function TdxRibbonGalleryControlViewInfo.GetLeftLayoutIndent: Integer;
begin
  Result := Max(1, GalleryOptions.SpaceBetweenItemsAndBorder) - 1;
end;

function TdxRibbonGalleryControlViewInfo.GetNextButtonEnabled: Boolean;
var
  ALastGroupIndex: Integer;
  ALastGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
begin
  if GroupCount <> 0 then
  begin
    ALastGroupIndex := GetVisibleNotEmptyGroupIndex(
      Control.GetGroups.Count - 1, False);
    ALastGroupViewInfo := Groups[GroupCount - 1];
    Result := (ALastGroupIndex <> ALastGroupViewInfo.Group.Index) or
      (Control.GetGroups[ALastGroupIndex].Items.Count - 1 <>
      ALastGroupViewInfo.Items[ALastGroupViewInfo.ItemCount - 1].GroupItem.Index) or
      not cxRectContain(GalleryBounds,
      ALastGroupViewInfo.Items[ALastGroupViewInfo.ItemCount - 1].Bounds);
  end
  else
    Result := False;
end;

function TdxRibbonGalleryControlViewInfo.GetPreviousButtonEnabled: Boolean;
var
  AFirstGroupIndex: Integer;
begin
  if GroupCount <> 0 then
  begin
    AFirstGroupIndex := GetVisibleNotEmptyGroupIndex(0, True);
    Result := (Groups[0].Group.Index <> AFirstGroupIndex) or
      (Groups[0].Items[0].GroupItem.Index <> 0) or
      not cxRectContain(GalleryBounds, Groups[0].Items[0].Bounds);
  end
  else
    Result := False;
end;

function TdxRibbonGalleryControlViewInfo.GetRibbonSkin: IdxSkin;
begin
  Result := (Painter as TdxBarSkinnedPainter).Skin;
end;

function TdxRibbonGalleryControlViewInfo.GetRightLayoutIndent: Integer;
begin
  Result := GalleryOptions.SpaceBetweenItemsAndBorder;
end;

function TdxRibbonGalleryControlViewInfo.GetVisibleGroupIndex(
  AStartGroupIndex: Integer; AIncreaseIndex: Boolean): Integer;
begin
  Result := AStartGroupIndex;
  if AIncreaseIndex then
  begin
    while (Result < Control.GetGroups.Count) and not GalleryItem.IsGroupVisible(Result) do
      Inc(Result);
    if Result >= Control.GetGroups.Count then
      Result := -1;
  end
  else
  begin
    while (Result >= 0) and (not GalleryItem.IsGroupVisible(Result)) do
      Dec(Result);
  end;
end;

function TdxRibbonGalleryControlViewInfo.GetVisibleNotEmptyGroupIndex(
  AStartGroupIndex: Integer; AIncreaseIndex: Boolean): Integer;
var
  AExit: Boolean;
begin
  AExit := False;
  repeat
    Result := GetVisibleGroupIndex(AStartGroupIndex, AIncreaseIndex);
    if Result <> -1 then
    begin
      if Control.GetGroups[Result].Items.Count > 0 then
        AExit := True
      else
      begin
        AStartGroupIndex := Result;
        if AIncreaseIndex then
          Inc(AStartGroupIndex)
        else
          Dec(AStartGroupIndex);
        if (AStartGroupIndex < 0) or
          (AStartGroupIndex > Control.GetGroups.Count - 1) then
        begin
          Result := -1;
          AExit := True;
        end;
      end;
    end
    else
      AExit := True;
  until AExit;
end;

function TdxRibbonGalleryControlViewInfo.IsGroupHeaderVisible: Boolean;
begin
  Result := not IsInRibbon;
end;

function TdxRibbonGalleryControlViewInfo.IsGroupItemAtThisPlace(X, Y: Integer): Boolean;
begin
  Result := GetGroupItem(X, Y) <> nil;
end;

function TdxRibbonGalleryControlViewInfo.IsInRibbon: Boolean;
begin
  Result := False;
end;

procedure TdxRibbonGalleryControlViewInfo.RemoveGroupItem(
  AItem: TdxRibbonGalleryGroupItem);
begin
  if HotGroupItem = AItem then
    FHotGroupItem := nil;
  if DownedGroupItem = AItem then
    FDownedGroupItem := nil;
end;

procedure TdxRibbonGalleryControlViewInfo.Changed;
begin
  // do nothing
end;

procedure TdxRibbonGalleryControlViewInfo.SetDownedGroupItem(
  const Value: TdxRibbonGalleryGroupItem);
var
  AGroupItem: TdxRibbonGalleryGroupItem;
begin
  if FDownedGroupItem <> Value then
  begin
    AGroupItem := FDownedGroupItem;
    FDownedGroupItem := Value;
    if Value = nil then
      DrawGroupItem(AGroupItem)
    else
    begin
      DrawGroupItem(Value);
      if HotGroupItem <> Value then
        Control.Controller.SetHotGroupItem(Value);
    end;
  end;
end;

procedure TdxRibbonGalleryControlViewInfo.SetGroupItemStoredSize(const Value: TSize;
  AGroupIndex: Integer);
begin
  if AGroupIndex < Length(FGroupItemStoredSizes) then
    FGroupItemStoredSizes[AGroupIndex] := Value;
end;

procedure TdxRibbonGalleryControlViewInfo.SetHotGroupItem(
  Value: TdxRibbonGalleryGroupItem);

  function HasGroupItemPullHighlighting(AGroupItem: TdxRibbonGalleryGroupItem): Boolean;
  begin
    Result := (AGroupItem <> nil) and
      (AGroupItem.Group.Options.ItemPullHighlightingMode <> iphmNone);
  end;

  procedure DoSetHotGroupItem(Value: TdxRibbonGalleryGroupItem);
  var
    AOldHotGroupItem: TdxRibbonGalleryGroupItem;
  begin
    Control.Canvas.SaveState;
    try
      AOldHotGroupItem := FHotGroupItem;
      FHotGroupItem := Value;
      DisplayGroupItem(FHotGroupItem);

      if Control.ViewInfo.IsInRibbon then
      begin
        DrawGroupItem(AOldHotGroupItem);
        DrawGroupItem(FHotGroupItem);
      end
      else
        if HasGroupItemPullHighlighting(AOldHotGroupItem) and
          HasGroupItemPullHighlighting(FHotGroupItem) then
        begin
          if GetItemPullHighlightingIdentifier(AOldHotGroupItem) =
            GetItemPullHighlightingIdentifier(FHotGroupItem) then
            RepaintChainOfGroups(FHotGroupItem, AOldHotGroupItem)
          else
          begin
            RepaintChainOfGroups(FHotGroupItem, nil);
            RepaintChainOfGroups(nil, AOldHotGroupItem);
          end;
        end
        else
        begin
          if HasGroupItemPullHighlighting(FHotGroupItem) then
            RepaintChainOfGroups(FHotGroupItem, nil)
          else
            DrawGroupItem(FHotGroupItem);
          if HasGroupItemPullHighlighting(AOldHotGroupItem) then
            RepaintChainOfGroups(nil, AOldHotGroupItem)
          else
            DrawGroupItem(AOldHotGroupItem);
        end;

      GalleryItem.DoHotTrackedItemChanged(AOldHotGroupItem, FHotGroupItem);
    finally
      Control.Canvas.RestoreState;
    end;
  end;

var
  ADestroyAfterUse: Boolean;
  AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
begin
  if Value <> nil then
  begin
    AGroupViewInfo := GetGroupViewInfo(Control.GetGroups, Self,
      Value.Group.Index, ADestroyAfterUse);
    try
      if AGroupViewInfo <> nil then
        DoSetHotGroupItem(Value);
    finally
      if ADestroyAfterUse then
        AGroupViewInfo.Free;
    end;
  end
  else
    DoSetHotGroupItem(Value);
end;

procedure TdxRibbonGalleryControlViewInfo.ShowGroupItem(AGroupItem: TdxRibbonGalleryGroupItem);
begin
  //do nothing
end;

function TdxRibbonGalleryControlViewInfo.GetGroupCount: Integer;
begin
  Result := FGroups.Count;
end;

function TdxRibbonGalleryControlViewInfo.GetGroup(
  Index: Integer): TdxRibbonGalleryGroupViewInfo;
begin
  Result := TdxRibbonGalleryGroupViewInfo(FGroups[Index]);
end;

function TdxRibbonGalleryControlViewInfo.GetGroupItemSize(
  AGroupIndex: Integer): TSize;
var
  ADestroyAfterUse: Boolean;
  AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
begin
  if GalleryItem.IsGroupVisible(AGroupIndex) then
  begin
    AGroupViewInfo := GetGroupViewInfo(Control.GetGroups, Self, AGroupIndex,
      ADestroyAfterUse);
    try
      Result := AGroupViewInfo.ItemSize;
    finally
      if ADestroyAfterUse then
        AGroupViewInfo.Free;
    end;
  end
  else
    Result := cxNullSize;
end;

procedure TdxRibbonGalleryControlViewInfo.CalculateGlobalItemSize;
begin
  if (GalleryOptions.EqualItemSizeInAllGroups) or
    (Control.Parent.Kind in [bkBarControl]) then
    FGlobalItemSize := GetMaxGroupItemSize;
end;

function TdxRibbonGalleryControlViewInfo.GetControl: TdxRibbonGalleryControl;
begin
  Result := TdxRibbonGalleryControl(FControl);
end;

function TdxRibbonGalleryControlViewInfo.GetGalleryItem: TdxCustomRibbonGalleryItem;
begin
  if Control <> nil then
    Result := TdxCustomRibbonGalleryItem(Control.Item)
  else
    Result := nil;
end;

function TdxRibbonGalleryControlViewInfo.GetGalleryOptions: TdxCustomRibbonGalleryOptions;
begin
  Result := GalleryItem.GalleryOptions;
end;

function TdxRibbonGalleryControlViewInfo.GetGalleryInMenuOptions: TdxInMenuGalleryOptions;
begin
  Result := GalleryItem.GalleryInMenuOptions;
end;

function TdxRibbonGalleryControlViewInfo.GetGallerySize: TSize;
var
  ARect: TRect;
begin
  ARect := GalleryBounds;
  Result.cx := ARect.Right - ARect.Left;
  Result.cy := ARect.Bottom - ARect.Top;
end;

function TdxRibbonGalleryControlViewInfo.GetPainter: TdxBarPainter;
begin
  Result := Control.Painter;
end;

function TdxRibbonGalleryControlViewInfo.GetScrollBarBounds: TRect;
var
  AGalleryBounds: TRect;
begin
  Result := GetControlBounds;
  if Control.UseRightToLeftScrollbar then
    Result.Right := Result.Left + GetScrollBarWidth
  else
    Result.Left := Result.Right - GetScrollBarWidth;
  AGalleryBounds := GalleryBounds;
  Result.Top := AGalleryBounds.Top;
  Result.Bottom := AGalleryBounds.Bottom;
end;

function TdxRibbonGalleryControlViewInfo.GetScrollBarWidth: Integer;
begin
  if Control.IsNeedScrollBar then
    Result := InternalGetScrollBarWidth
  else
    Result := 0;
end;

procedure TdxRibbonGalleryControlViewInfo.DrawGroupItem(
  const AGroupItem: TdxRibbonGalleryGroupItem);
var
  AItemViewInfo: TdxRibbonGalleryGroupItemViewInfo;
begin
  if AGroupItem <> nil then
  begin
    Control.Canvas.SaveClipRegion;
    try
      Control.Canvas.SetClipRegion(TcxRegion.Create(GalleryBounds), roSet);
      AItemViewInfo := GetGroupItemViewInfo(AGroupItem);
      if AItemViewInfo <> nil then
        AItemViewInfo.Paint(Control.Canvas);
    finally
      Control.Canvas.RestoreClipRegion;
    end;
  end;
end;

procedure TdxRibbonGalleryControlViewInfo.RepaintChainOfGroups(
  ANewItem, AOldItem: TdxRibbonGalleryGroupItem);

  function IsGroupPullDirectionAgreeWithGeneral(AnItem: TdxRibbonGalleryGroupItem): Boolean;
  begin
    Result := (AnItem = nil) or (AnItem.Group.Options.ItemPullHighlightingMode <> iphmNone) and
      (AnItem.Group.Options.ItemPullHighlightingMode = GalleryInMenuOptions.ItemPullHighlightingMode);
  end;

  function UsePullDirectionOfGroup(out AMode: TdxRibbonGalleryItemPullHighlightingMode): Boolean;
  begin
    if not IsGroupPullDirectionAgreeWithGeneral(ANewItem) or
      not IsGroupPullDirectionAgreeWithGeneral(AOldItem) then
    begin
      Result := True;
      if ANewItem <> nil then
        AMode := ANewItem.Group.Options.ItemPullHighlightingMode
      else
        AMode := AOldItem.Group.Options.ItemPullHighlightingMode;
    end
    else
      Result := False;
  end;

  function GetDirection: TdxRibbonGalleryItemPullHighlightingMode;
  begin
    if not UsePullDirectionOfGroup(Result) then
      Result := GalleryInMenuOptions.ItemPullHighlightingMode;
  end;

  function IsStartToFinishDirection: Boolean;
  begin
    Result := GetDirection = iphmStartToFinish;
  end;

  procedure DoRepaintChain(AStartGroupIndex, AStartGroupItemIndex,
    AEndGroupIndex, AEndGroupItemIndex: Integer; IsHotTrack: Boolean);

    function GetGroupRepaintPart(AStartGroupIndex, AStartGroupItemIndex,
      AEndGroupIndex, AEndGroupItemIndex, ACurrentGroupIndex: Integer;
      out AnItemIndex, AnItemIndex2: Integer): TdxRibbonGalleryGroupRepaintPart;
    begin
      if ACurrentGroupIndex = GetVisibleGroupIndex(AStartGroupIndex, True) then
      begin
        AnItemIndex := AStartGroupItemIndex;
        if ACurrentGroupIndex = AEndGroupIndex then
        begin
          Result := ggrpBetween;
          AnItemIndex2 := AEndGroupItemIndex;
        end
        else
          if IsStartToFinishDirection and
            (ACurrentGroupIndex = GetVisibleGroupIndex(0, True)) then
            Result := ggrpAll
          else
            Result := ggrpAfter;
      end
      else
        if ACurrentGroupIndex = AEndGroupIndex then
        begin
          if not IsStartToFinishDirection and
            (ACurrentGroupIndex = GetVisibleGroupIndex(GalleryItem.GalleryGroups.Count - 1, False)) then
            Result := ggrpAll
          else
          begin
            Result := ggrpBefore;
            AnItemIndex := AEndGroupItemIndex;
          end;
        end
        else
        begin
          Result := ggrpAll;
          AnItemIndex := 0;
        end;
    end;

  var
    AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
    AnItemIndex, AnItemIndex2, I: Integer;
    APart: TdxRibbonGalleryGroupRepaintPart;
    DestroyAfterUse: Boolean;
  begin
    Control.Canvas.SaveClipRegion;
    try
      Control.Canvas.SetClipRegion(TcxRegion.Create(GalleryBounds), roSet);
      I := AStartGroupIndex;
      while I <= AEndGroupIndex do
      begin
        AGroupViewInfo := GetGroupViewInfo(GalleryItem.GalleryGroups, Self, I,
          DestroyAfterUse);
        if AGroupViewInfo <> nil then
        begin
          if not DestroyAfterUse then
          begin
            APart := GetGroupRepaintPart(AStartGroupIndex, AStartGroupItemIndex,
              AEndGroupIndex, AEndGroupItemIndex, I, AnItemIndex, AnItemIndex2);
            AGroupViewInfo.RepaintChainOfItems(AnItemIndex, IsHotTrack, Control.Canvas,
              APart, AnItemIndex2);
          end
          else
            AGroupViewInfo.Free;
        end;
        Inc(I);
      end;
    finally
      Control.Canvas.RestoreClipRegion;
    end;
  end;

  procedure ReturnRange(ANewItem, AOldItem: TdxRibbonGalleryGroupItem;
    out AStartGroupItem, AEndGroupItem: TdxRibbonGalleryGroupItem;
    out IsHotTrack: Boolean);
  var
    AnOuterGroupItem: TdxRibbonGalleryGroupItem;
  begin
    AnOuterGroupItem := GetOuterGroupItem(ANewItem, AOldItem, GetDirection);
    IsHotTrack := AnOuterGroupItem = ANewItem;
    if IsHotTrack xor not IsStartToFinishDirection then
    begin
      AStartGroupItem := AOldItem;
      AEndGroupItem := ANewItem;
    end
    else
    begin
      AStartGroupItem := ANewItem;
      AEndGroupItem := AOldItem;
    end;
  end;

  procedure ReturnRangeStart(AStartGroupItem: TdxRibbonGalleryGroupItem;
    IsHotTrack: Boolean; out AStartGroupIndex, AStartGroupItemIndex: Integer);
  begin
    if AStartGroupItem <> nil then
    begin
      AStartGroupIndex := AStartGroupItem.Group.Index;
      AStartGroupItemIndex := AStartGroupItem.Index;
      if not IsHotTrack and IsStartToFinishDirection then
        begin
          Inc(AStartGroupItemIndex);
          if AStartGroupItemIndex > AStartGroupItem.Group.Items.Count - 1 then
          begin
            AStartGroupItemIndex := 0;
            Inc(AStartGroupIndex);
            AStartGroupIndex := GetVisibleGroupIndex(AStartGroupIndex, True);
          end;
        end;
    end
    else
    begin
      AStartGroupIndex := 0;
      AStartGroupItemIndex := 0;
    end;
  end;

  procedure ReturnRangeEnd(AEndGroupItem: TdxRibbonGalleryGroupItem;
    IsHotTrack: Boolean; out AEndGroupIndex, AEndGroupItemIndex: Integer);
  begin
    if AEndGroupItem <> nil then
    begin
      AEndGroupIndex := AEndGroupItem.Group.Index;
      AEndGroupItemIndex := AEndGroupItem.Index;
      if not IsHotTrack and not IsStartToFinishDirection then
        begin
          Dec(AEndGroupItemIndex);
          if AEndGroupItemIndex < 0 then
          begin
            Dec(AEndGroupIndex);
            AEndGroupIndex := GetVisibleGroupIndex(AEndGroupIndex, False);
            AEndGroupItemIndex := GalleryItem.GalleryGroups[AEndGroupIndex].Items.Count - 1;
          end;
        end;
    end
    else
    begin
      if IsStartToFinishDirection then
      begin
        AEndGroupIndex := 0;
        AEndGroupItemIndex := 0;
      end
      else
      begin
        AEndGroupIndex := GetVisibleGroupIndex(
          GalleryItem.GalleryGroups[GalleryItem.GalleryGroups.Count - 1].Index, False);
        AEndGroupItemIndex := GalleryItem.GalleryGroups[AEndGroupIndex].Items.Count - 1;
      end;
    end;
  end;

var
  IsHotTrack: Boolean;
  AStartGroupItem, AEndGroupItem: TdxRibbonGalleryGroupItem;
  AStartGroupIndex, AEndGroupIndex, AStartGroupItemIndex, AEndGroupItemIndex: Integer;
begin
  if (ANewItem <> nil) or (AOldItem <> nil) then
  begin
    ReturnRange(ANewItem, AOldItem, AStartGroupItem,
      AEndGroupItem, IsHotTrack);
    ReturnRangeStart(AStartGroupItem, IsHotTrack, AStartGroupIndex,
      AStartGroupItemIndex);
    ReturnRangeEnd(AEndGroupItem, IsHotTrack, AEndGroupIndex,
      AEndGroupItemIndex);
    DoRepaintChain(AStartGroupIndex, AStartGroupItemIndex,
      AEndGroupIndex, AEndGroupItemIndex, IsHotTrack);
  end;
end;

{ TdxInRibbonGalleryControlViewInfo }

constructor TdxInRibbonGalleryControlViewInfo.Create(AControl: TdxBarItemControl);
begin
  inherited Create(AControl);
  FShowGroupItem := GalleryItem.FFirstVisibleGroupItem;
end;

procedure TdxInRibbonGalleryControlViewInfo.Calculate(
  ALayoutOffset: Integer; AScrollCode: TScrollCode);

  procedure ShowGivenGroupItem;
  var
    AGroupItem: TdxRibbonGalleryGroupItem;
  begin
    if FShowGroupItem <> nil then
    begin
      AGroupItem := FShowGroupItem;
      FShowGroupItem := nil;
      ShowGroupItem(AGroupItem);
    end;
  end;

var
  ARowDelta: Integer;
begin
  inherited Calculate(ALayoutOffset, AScrollCode);
  if Control.GetVisibleGroupCount = 0 then
    Exit;
  case AScrollCode of
    scLineUp: ARowDelta := -1;
    scLineDown: ARowDelta := 1;
  else
    ARowDelta := 0;
  end;
  if ARowDelta = 0 then
    DoCalculate
  else
  begin
    DoScrolling(ARowDelta);
    DoCalculate;
  end;
  ShowGivenGroupItem;
end;

function TdxInRibbonGalleryControlViewInfo.IsCollapsed: Boolean;
begin
  Result := GetCollapsed;
end;

procedure TdxInRibbonGalleryControlViewInfo.Paint(ACanvas: TcxCanvas);

  procedure DoAnimationPaint;
  var
    ABitmap: TcxBitmap32;
    ARect: TRect;
  begin
    ARect := GalleryBounds;
    ABitmap := TcxBitmap32.CreateSize(ARect, True);
    try
      OffsetRect(ARect, -ARect.Left, -ARect.Top);
      DoDrawBackground(ABitmap.cxCanvas, ARect);

      ARect := cxRectSetHeight(ARect, FAnimationBitmapHeight);
      OffsetRect(ARect, 0, GetTopIndent);
      Animation.DrawTransparent(ABitmap.Canvas, ARect);
      cxBitBlt(ACanvas.Canvas.Handle, ABitmap.Canvas.Handle, GalleryBounds, cxNullPoint, SRCCOPY);
    finally
      ABitmap.Free;
    end;
  end;

begin
  if Animation <> nil then
    DoAnimationPaint
  else
    inherited Paint(ACanvas);
end;

procedure TdxInRibbonGalleryControlViewInfo.ResetCachedValues;
var
  I: Integer;
begin
  inherited ResetCachedValues;
  SetLength(FWidthForColumnCountInfos, GetMaxColumnCount - GetMinColumnCount + 1);
  for I := 0 to High(FWidthForColumnCountInfos) do
    FWidthForColumnCountInfos[I].Calculated := False;
  FControlHeight := 0;
end;

procedure TdxInRibbonGalleryControlViewInfo.AnimationHandler(
  Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
begin
  Control.Update;
  Paint(Control.Canvas);
  if AFinished then
    FAnimation := nil;
end;

procedure TdxInRibbonGalleryControlViewInfo.BoundsCalculated;
begin
  inherited BoundsCalculated;
  if not Control.Collapsed then
    SetBounds(cxRectContent(Bounds, GetControlMargins));
end;

procedure TdxInRibbonGalleryControlViewInfo.CalculateLayout(ALayoutOffset,
  AColumnCount: Integer; AGroupItemsList: TObjectList);
var
  ARowTop, AColumnIndex: Integer;
  I: Integer;
  ACurrentGroupIndex, AItemGroupIndex: Integer;
  AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
  AGroupItemBounds: TRect;
  ABounds: TRect;
  AControlBounds: TRect;
begin
  FGroups.Clear;
  AGroupViewInfo := nil;
  ACurrentGroupIndex := -1;
  AColumnIndex := 0;
  ARowTop := ALayoutOffset;
  ABounds := GalleryBounds;
  AControlBounds := GetControlBounds;
  for I := 0 to AGroupItemsList.Count - 1 do
  begin
    AItemGroupIndex := TdxRibbonGalleryGroup(
      TdxRibbonGalleryGroupItem(AGroupItemsList.Items[I]).Collection.ParentComponent).Index;
    if ACurrentGroupIndex <> AItemGroupIndex then
    begin
      ACurrentGroupIndex := AItemGroupIndex;
      AGroupViewInfo := TdxRibbonGalleryGroupViewInfo.Create(Self,
        Control.GetGroups[AItemGroupIndex]);
      AGroupViewInfo.SetBounds(ABounds);
      FGroups.Add(AGroupViewInfo);
    end;
    AGroupItemBounds.Top := ARowTop + ABounds.Top;
    AGroupItemBounds.Bottom := AGroupItemBounds.Top + AGroupViewInfo.ItemSize.cy;
    AGroupItemBounds.Left := AGroupViewInfo.GetColumnLeft(AColumnIndex, Bounds.Left);
    AGroupItemBounds.Right := AGroupItemBounds.Left + AGroupViewInfo.ItemSize.cx;
    if Control.UseRightToLeftAlignment then
      AGroupItemBounds := TdxRightToLeftLayoutConverter.ConvertRect(AGroupItemBounds, AControlBounds);
    AGroupViewInfo.CreateGroupItem(
      TdxRibbonGalleryGroupItem(AGroupItemsList.Items[I]).Index, AGroupItemBounds);
    Inc(AColumnIndex);
    if AColumnIndex = AColumnCount then
    begin
      AColumnIndex := 0;
      ARowTop := ARowTop + AGroupViewInfo.GetRowHeight;
    end;
  end;
end;

function TdxInRibbonGalleryControlViewInfo.CorrectGroupItemWidth(
  const AGroupItemSize: TSize): Integer;
var
  I: Integer;
  AWidth: Integer;
  AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
  ADestroyAfterUse: Boolean;
begin
  if CanUseSize(GalleryOptions.ItemSizeInRibbon.Size) then
  begin
    Result := GalleryOptions.ItemSizeInRibbon.Size.cx;
    Exit;
  end;
 { if CanUseSize(GalleryOptions.ItemSize.Size) then
  begin
    Result := GalleryOptions.ItemSize.Size.cx;
    Exit;
  end;}

  Result := 0;
  for I := 0 to Control.GetGroups.Count - 1 do
    if GalleryItem.IsGroupVisible(I) then
    begin
      AGroupViewInfo := GetGroupViewInfo(Control.GetGroups, Self, I,
        ADestroyAfterUse);
      try
        AWidth := AGroupViewInfo.CalculateItemSize(AGroupItemSize, AGroupItemSize.cx, AGroupItemSize.cy).cx;
      finally
        if ADestroyAfterUse then
          AGroupViewInfo.Free;
      end;
      if Result < AWidth then
        Result := AWidth;
    end;
end;

procedure TdxInRibbonGalleryControlViewInfo.DoDrawBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  GetRibbonSkin.DrawBackground(ACanvas.Handle, R, DXBAR_INRIBBONGALLERY, GetInRibbonGalleryState);
end;

procedure TdxInRibbonGalleryControlViewInfo.DoScrolling(ARowDelta: Integer);

  procedure CalculateVisibleRows(AColumnCount: Integer; out AFirstVisibleRowIndex, ALastVisibleRowIndex: Integer);
  var
    ANewTopVisibleRow: Integer;
  begin
    if ARowDelta < 0 then
      ANewTopVisibleRow := Max(FTopVisibleRow - 1, 0)
    else
      ANewTopVisibleRow := Min(FTopVisibleRow + 1, Ceil(GetGroupItemCount(Control.GetGroups.Count - 1) / AColumnCount));

    AFirstVisibleRowIndex := Min(ANewTopVisibleRow, FTopVisibleRow);
    ALastVisibleRowIndex := AFirstVisibleRowIndex + (RowCount - 1) + 1;
    FTopVisibleRow := ANewTopVisibleRow;
  end;

  procedure DrawOnBitmap(ABitmap: TcxBitmap32; AColumnCount: Integer; AGroupItemsList: TObjectList; AOffset: Integer = 0);
  begin
    CalculateLayout(GetTopLayoutIndent, AColumnCount, AGroupItemsList);

    if GroupCount = 0 then Exit;

    FAnimationBitmapHeight := RowCount * Groups[0].GetRowHeight;
    ABitmap.SetSize(cxRectWidth(GalleryBounds), FAnimationBitmapHeight);

    ABitmap.Clear;
    try
      ABitmap.cxCanvas.WindowOrg := Point(GalleryBounds.Left, GalleryBounds.Top - AOffset * (RowCount - 1) * ABitmap.Height div RowCount + 1);
      FDrawWithoutBackground := True;
      try
        Paint(ABitmap.cxCanvas);
      finally
        FDrawWithoutBackground := False;
      end;
    finally
      ABitmap.cxCanvas.WindowOrg := cxNullPoint;
    end;
  end;

var
  AAnimationMode: TdxDrawAnimationMode;
  AColumnCount: Integer;
  AEndBitmap: TcxBitmap32;
  AFirstVisibleRowIndex: Integer;
  AGroupItemsList: TObjectList;
  ALastVisibleRowIndex: Integer;
  AStartBitmap: TcxBitmap32;
begin
  AColumnCount := GetGroupColumnCount;
  if (Control.GetVisibleGroupCount > 0) and (AColumnCount > 0) and IsScrollingPossible(ARowDelta) then
  begin
    CalculateVisibleRows(AColumnCount, AFirstVisibleRowIndex, ALastVisibleRowIndex);

    AGroupItemsList := TObjectList.Create(False);
    AStartBitmap := TcxBitmap32.Create;
    AEndBitmap := TcxBitmap32.Create;
    try
      if ARowDelta > 0 then
        PopulateGroupItemList(AFirstVisibleRowIndex, ALastVisibleRowIndex, AColumnCount, AGroupItemsList)
      else
        PopulateGroupItemList(AFirstVisibleRowIndex + 1, ALastVisibleRowIndex, AColumnCount, AGroupItemsList);
      DrawOnBitmap(AStartBitmap, AColumnCount, AGroupItemsList);

      if ARowDelta > 0 then
      begin
        PopulateGroupItemList(ALastVisibleRowIndex, ALastVisibleRowIndex, AColumnCount, AGroupItemsList);
        AAnimationMode := amScrollUp;
      end
      else
      begin
        PopulateGroupItemList(AFirstVisibleRowIndex, AFirstVisibleRowIndex, AColumnCount, AGroupItemsList);
        AAnimationMode := amScrollDown;
      end;
      DrawOnBitmap(AEndBitmap, AColumnCount, AGroupItemsList, Integer(ARowDelta < 0));

      FAnimation := TdxImageAnimationTransition.Create(AStartBitmap, AEndBitmap,
        350 div RowCount, AAnimationMode, ateLinear, AEndBitmap.Height div RowCount);
      FAnimation.OnAnimate := AnimationHandler;
      FAnimation.ImmediateAnimation;
    finally
      AStartBitmap.Free;
      AEndBitmap.Free;
      AGroupItemsList.Free;
    end;
  end;
end;

procedure TdxInRibbonGalleryControlViewInfo.DrawBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  if FDrawWithoutBackground then
    Exit;
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(R);
    DoDrawBackground(ACanvas, GalleryBounds);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

function TdxInRibbonGalleryControlViewInfo.GetControlMargins: TRect;
begin
  Result := Rect(0, 3, 0, 3);
end;

function TdxInRibbonGalleryControlViewInfo.GetGalleryMargins: TRect;
begin
  Result := Rect(0, 0, -Ord(Control.IsNeedScrollBar), 0);
end;

function TdxInRibbonGalleryControlViewInfo.GetLayoutWidth(
  AColumnCount: Integer; out AGroupItemWidthIsNull: Boolean): Integer;
begin
  Result := AColumnCount * GetMaxGroupItemSize.cx +
    (AColumnCount - 1) * GetSpaceBetweenItems(True) +
    GetControlMargins.Left + GetControlMargins.Right;
  AGroupItemWidthIsNull := GetMaxGroupItemSize.cx = 0;
end;

function TdxInRibbonGalleryControlViewInfo.GetMaxGroupItemSize: TSize;
const
  AMargin = 1;
var
  AHeight, ARowCount, ADenominator, AIndent: Integer;
begin
  if CanUseSize(GlobalItemSize) then
    Result := GlobalItemSize
  else
  begin
    Result := inherited GetMaxGroupItemSize;
    AHeight := GallerySize.cy;
    if AHeight <= 0 then
      AHeight := ControlHeight;
    ADenominator := Result.cy + GetSpaceBetweenItems(False);
    if (AHeight > 0) and (ADenominator <> 0) then
    begin
      AIndent := 2 * AMargin + GetTopLayoutIndent + GetBottomLayoutIndent;
      ARowCount := Max(1, (AHeight - AIndent) div ADenominator);
      Result.cy := (AHeight - GetSpaceBetweenItems(False) *
        (ARowCount - 1) - AIndent) div ARowCount;
      Result.cx := CorrectGroupItemWidth(Result);
    end
    else
      ARowCount := 1;
    FRowCount := ARowCount;
  end;
end;

function TdxInRibbonGalleryControlViewInfo.GetBottomLayoutIndent: Integer;
begin
  Result := GalleryOptions.SpaceBetweenItemsAndBorder;
end;

function TdxInRibbonGalleryControlViewInfo.GetLeftLayoutIndent: Integer;
begin
  Result := 1 + GalleryOptions.SpaceBetweenItemsAndBorder;
end;

function TdxInRibbonGalleryControlViewInfo.GetRightLayoutIndent: Integer;
begin
  Result := GalleryOptions.SpaceBetweenItemsAndBorder;
end;

function TdxInRibbonGalleryControlViewInfo.GetTopLayoutIndent: Integer;
begin
  Result := GalleryOptions.SpaceBetweenItemsAndBorder;
end;

function TdxInRibbonGalleryControlViewInfo.InternalGetScrollBarWidth: Integer;
begin
  Result := Painter.DropDownGalleryGetScrollBarWidth(ControlHeight, GetGalleryMargins);
end;

function TdxInRibbonGalleryControlViewInfo.IsInRibbon: Boolean;
begin
  Result := True;
end;

procedure TdxInRibbonGalleryControlViewInfo.ShowGroupItem(
  AGroupItem: TdxRibbonGalleryGroupItem);
var
  AGlobalIndex: Integer;
begin
  if (AGroupItem <> nil) and not Control.Collapsed and (FRowCount > 0) then
  begin
    AGlobalIndex := GetGroupItemCount(AGroupItem.Group.Index - 1) + AGroupItem.Index;
    FTopVisibleRow := Max(0, GetRowIndex(AGlobalIndex, GetColumnCount) - RowCount + 1);
    DoCalculate;
    GetGalleryItem.Update;
  end
  else
    FShowGroupItem := AGroupItem;
end;

procedure TdxInRibbonGalleryControlViewInfo.DoCalculate;
var
  AGroupItemsList: TObjectList;
  AColumnCount: Integer;
begin
  AColumnCount := GetGroupColumnCount;
  FTopVisibleRow := Min(FTopVisibleRow, Max(TotalRows - 1, 0));
  AGroupItemsList := TObjectList.Create(False);
  try
    PopulateGroupItemList(FTopVisibleRow, FTopVisibleRow + (RowCount - 1), AColumnCount, AGroupItemsList);
    CalculateLayout(GetTopIndent, AColumnCount, AGroupItemsList);
  finally
    AGroupItemsList.Free;
  end;
end;

function TdxInRibbonGalleryControlViewInfo.GetControlHeight: Integer;
var
  ARibbonBarPainter: TdxRibbonBarPainter;
begin
  if FControlHeight = 0 then
  begin
    ARibbonBarPainter := TdxRibbonBarPainter(Painter);
    FControlHeight := ARibbonBarPainter.GetGroupRowCount * ARibbonBarPainter.GetGroupRowHeight(Control.BarManager.ImageOptions.GlyphSize);
    Dec(FControlHeight, cxMarginsHeight(GetControlMargins));
  end;
  Result := FControlHeight;
end;

function TdxInRibbonGalleryControlViewInfo.GetGroupColumnCount: Integer;
var
  AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
  ADestroyAfterUse: Boolean;
begin
  AGroupViewInfo := GetGroupViewInfo(Control.GetGroups, Self, GetVisibleGroupIndex(0, True), ADestroyAfterUse);
  try
    Result := AGroupViewInfo.GetColumnCount(Control.ClientWidth);
  finally
    if ADestroyAfterUse then
      AGroupViewInfo.Free;
  end;
end;

function TdxInRibbonGalleryControlViewInfo.GetRowIndex(AGroupItemIndex,
  AColumnCount: Integer): Integer;
begin
  if AColumnCount <> 0 then
    Result := AGroupItemIndex div AColumnCount
  else
    Result := 0;
end;

function TdxInRibbonGalleryControlViewInfo.GetSpaceBetweenItems(
  AIsFlat: Boolean): Integer;
begin
  if (GalleryOptions.RemoveHorizontalItemPadding and AIsFlat) or
    (GalleryOptions.RemoveVerticalItemPadding and not AIsFlat) then
    Result := 0
  else
    Result := GalleryOptions.SpaceBetweenItems;
end;

function TdxInRibbonGalleryControlViewInfo.IsScrollingPossible(
  ARowDelta: Integer): Boolean;
begin
  Result := (ARowDelta < 0) and GetPreviousButtonEnabled or
    (ARowDelta > 0) and GetNextButtonEnabled;
end;

procedure TdxInRibbonGalleryControlViewInfo.PopulateGroupItemList(
  AFirstVisibleRow, ALastVisibleRow, AColumnCount: Integer; AList: TObjectList);

  function GetGroupItemCount(AGroupIndex: Integer): Integer;
  begin
    if GalleryItem.IsGroupVisible(AGroupIndex) then
      Result := Control.GetGroups[AGroupIndex].Items.Count
    else
      Result := 0;
  end;

var
  ACurrentGroupItem: Integer;
  AFirstGroupItem: Integer;
  ALastGroupItem: Integer;
  I, J: Integer;
begin
  AFirstGroupItem := AFirstVisibleRow * AColumnCount;
  ALastGroupItem := (ALastVisibleRow + 1) * AColumnCount - 1;
  ACurrentGroupItem := 0;
  I := 0;
  while (I <= Control.GetGroups.Count - 1) and (ACurrentGroupItem + GetGroupItemCount(I) <= AFirstGroupItem) do
  begin
    Inc(ACurrentGroupItem, GetGroupItemCount(I));
    Inc(I);
  end;
  AList.Clear;
  AList.Capacity := ALastGroupItem - AFirstGroupItem + 1;
  J := AFirstGroupItem - ACurrentGroupItem;
  ACurrentGroupItem := AFirstGroupItem;
  while (ACurrentGroupItem <= ALastGroupItem) and (I < Control.GetGroups.Count) do
  begin
    while (J < GetGroupItemCount(I)) and (ACurrentGroupItem <= ALastGroupItem) do
    begin
      AList.Add(Control.GetGroups[I].Items[J]);
      Inc(J);
      Inc(ACurrentGroupItem);
    end;
    Inc(I);
    J := 0;
  end;
end;

// IdxBarMultiColumnItemControlViewInfo
function TdxInRibbonGalleryControlViewInfo.CanCollapse: Boolean;
begin
  Result := GalleryOptions.CanCollapse;
end;

function TdxInRibbonGalleryControlViewInfo.GetCollapsed: Boolean;
begin
  Result := FCollapsed or GalleryOptions.Collapsed;
end;

function TdxInRibbonGalleryControlViewInfo.GetColumnCount: Integer;
begin
  Result := FColumnCount;
end;

function TdxInRibbonGalleryControlViewInfo.GetMaxColumnCount: Integer;
begin
  Result := GalleryOptions.ColumnCount;
end;

function TdxInRibbonGalleryControlViewInfo.GetMinColumnCount: Integer;
begin
  Result := GalleryOptions.MinColumnCount;
end;

function TdxInRibbonGalleryControlViewInfo.GetTopIndent: Integer;
begin
  Result := 1 + GetTopLayoutIndent;
end;

function TdxInRibbonGalleryControlViewInfo.GetTotalRows: Integer;
var
  AColumnCount: Integer;
begin
  Result := 0;
  AColumnCount := GetGroupColumnCount;
  if AColumnCount <> 0 then
    Result := Ceil(GetGroupItemCount(GetVisibleNotEmptyGroupIndex(
      Control.GetGroups.Count - 1, False)) / AColumnCount);
end;

function TdxInRibbonGalleryControlViewInfo.GetWidthForColumnCount(
  AColumnCount: Integer): Integer;
begin
  if FWidthForColumnCountInfos[AColumnCount - GetMinColumnCount].Calculated then
    Result := FWidthForColumnCountInfos[AColumnCount - GetMinColumnCount].Width
  else
  begin
    Result := Control.GetDefaultWidthInSubmenu;
    with FWidthForColumnCountInfos[AColumnCount - GetMinColumnCount] do
    begin
      Width := Result;
      Calculated := True;
    end;
  end;
end;

procedure TdxInRibbonGalleryControlViewInfo.SetCollapsed(Value: Boolean);
begin
  FCollapsed := Value;
end;

procedure TdxInRibbonGalleryControlViewInfo.SetColumnCount(Value: Integer);
begin
  FColumnCount := Value;
end;

{ TdxRibbonOnMenuGalleryControlViewInfo }

destructor TdxRibbonOnMenuGalleryControlViewInfo.Destroy;
begin
  FreeAndNil(FGroupItemDescriptionRectCache);
  inherited Destroy;
end;

procedure TdxRibbonOnMenuGalleryControlViewInfo.Calculate(
  ALayoutOffset: Integer; AScrollCode: TScrollCode);
var
  I, AMode, ACurrentGroupTop, AGroupBottom, AGalleryVisibleHeight: Integer;
  AGalleryBounds: TRect;
  AGallerySize: TSize;
  AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
  AIsIntersected: Boolean;
begin
  inherited Calculate(ALayoutOffset, AScrollCode);
  AGalleryBounds := GalleryBounds;
  AGallerySize := GallerySize;
  FLayoutOffset := ALayoutOffset;

  if Control.SizeChanged then
  begin
    if (GalleryItem.SelectedGroupItem <> nil) and
      GalleryItem.IsGroupVisible(GalleryItem.SelectedGroupItem.Group.Index) then
      DisplayGroupItem(GalleryItem.SelectedGroupItem)
    else
    begin
      AGalleryVisibleHeight := AGallerySize.cy;
      if -FLayoutOffset + GetGalleryHeight(AGallerySize.cx) <
        AGalleryVisibleHeight then
        Control.SetScrollBarPosition(Max(0, GetGalleryHeight(AGallerySize.cx) -
          AGalleryVisibleHeight));
    end;
  end;

  FGroups.Clear;
  AMode := 0;
  I := 0;
  ACurrentGroupTop := -FLayoutOffset + AGalleryBounds.Top;
  while (AMode <> 2) and (I < Control.GetGroups.Count) do
  begin
    if GalleryItem.IsGroupVisible(I) then
    begin
      AGroupViewInfo := TdxRibbonGalleryGroupViewInfo.Create(Self,
        Control.GetGroups[I]);
      AGroupViewInfo.CorrectItemWidth(AGalleryBounds);
      AGroupBottom := ACurrentGroupTop +
        AGroupViewInfo.GetHeight(AGallerySize.cx) +
        GalleryOptions.SpaceBetweenGroups;
      AIsIntersected := AreLinesIntersectedStrictly(ACurrentGroupTop,
        AGroupBottom, AGalleryBounds.Top, AGalleryBounds.Bottom);
      if (AMode = 0) and AIsIntersected or
         (AMode = 1) and not AIsIntersected then
        Inc(AMode);
      if AMode = 1 then
      begin
        AGroupViewInfo.Calculate(ACurrentGroupTop, AGroupBottom, AGalleryBounds);
        FGroups.Add(AGroupViewInfo);
      end
      else
        AGroupViewInfo.Free;
      ACurrentGroupTop := AGroupBottom;
    end;
    Inc(I);
  end;

  CalculateFilterBand;
end;

procedure TdxRibbonOnMenuGalleryControlViewInfo.GetFilterMenuShowingParams(out APosition: TPoint; out AOwnerHeight: Integer);
begin
  if Control.UseRightToLeftAlignment then
    APosition := Point(FFilterBandContentRect.Right - GroupHeaderCaptionOffset div 2 -
      FilterMenuLeftBoundCorrection, FFilterBandRect.Bottom)
  else
    APosition := Point(FFilterBandContentRect.Left + GroupHeaderCaptionOffset div 2 +
      FilterMenuLeftBoundCorrection, FFilterBandRect.Bottom);
  APosition := Control.Parent.ClientToScreen(APosition);
  AOwnerHeight := cxRectHeight(FFilterBandRect);
end;

function TdxRibbonOnMenuGalleryControlViewInfo.IsCollapsed: Boolean;
begin
  Result := GalleryItem.GalleryInMenuOptions.CollapsedInSubmenu;
end;

function TdxRibbonOnMenuGalleryControlViewInfo.IsPtInFilterBandHotTrackArea(const P: TPoint): Boolean;
begin
  Result := PtInRect(FFilterBandRect, P);
end;

procedure TdxRibbonOnMenuGalleryControlViewInfo.RepaintFilterBand;
begin
  DrawFilterBand;
end;

procedure TdxRibbonOnMenuGalleryControlViewInfo.SetFilterBandHotTrack(
  AValue: Boolean);
begin
  if FFilterBandHotTrack <> AValue  then
  begin
    FFilterBandHotTrack := AValue;
    DrawFilterBand;
  end;
end;

procedure TdxRibbonOnMenuGalleryControlViewInfo.DisplayGroupItem(AGroupItem: TdxRibbonGalleryGroupItem);

  function DisplayGroupHeaderIfPossible(AGroupItemBottom, AGroupIndex: Integer): Boolean;
  var
    AGroupTop: Integer;
  begin
    Result := False;
    if FDontDisplayGroupHeaderWhenHotTrackingGroupItem = 0 then
    begin
      AGroupTop := GetAbsoluteGroupTop(AGroupIndex, GallerySize.cx);
      Result := IsFirstLineShorterOrEqualThanSecond(AGroupTop, AGroupItemBottom,
        GalleryBounds.Top, GalleryBounds.Bottom) and (AGroupTop - FLayoutOffset < 0);
      if Result then
        Control.SetScrollBarPosition(AGroupTop);
    end;
  end;

  procedure MoveLayoutDown(AGroupItemTop, AGroupItemBottom, AGroupIndex: Integer);
  begin
    if not DisplayGroupHeaderIfPossible(AGroupItemBottom, AGroupIndex) then
      Control.SetScrollBarPosition(AGroupItemTop);
  end;

  procedure MoveLayoutUp(AGroupItemBottom, AGroupIndex: Integer);
  begin
    if not DisplayGroupHeaderIfPossible(AGroupItemBottom, AGroupIndex) then
      Control.SetScrollBarPosition(AGroupItemBottom - GallerySize.cy);
  end;

var
  AAbsoluteGroupItemBottom: Integer;
  AAbsoluteGroupItemTop: Integer;
  AGroupItemBottom: Integer;
  AGroupItemTop: Integer;
begin
  if (AGroupItem <> nil) and (FDontDisplayHotTrackedGroupItem = 0) then
  begin
    GroupItemYRange(AGroupItem, AGroupItemTop, AGroupItemBottom);
    AAbsoluteGroupItemTop := AGroupItemTop + Control.FScrollBar.Position;
    AAbsoluteGroupItemBottom := AGroupItemBottom + Control.FScrollBar.Position;
    case RelativeLocationOfLines(AGroupItemTop, AGroupItemBottom, GalleryBounds.Top, GalleryBounds.Bottom) of
      rllBefore:
        MoveLayoutDown(AAbsoluteGroupItemTop, AAbsoluteGroupItemBottom, AGroupItem.Group.Index);
      rllInside:
        DisplayGroupHeaderIfPossible(AAbsoluteGroupItemBottom, AGroupItem.Group.Index);
      rllAfter:
        MoveLayoutUp(AAbsoluteGroupItemBottom, AGroupItem.Group.Index);
    end;
  end;
end;

procedure TdxRibbonOnMenuGalleryControlViewInfo.DrawBackground(ACanvas: TcxCanvas; const R: TRect);
var
  ARect: TRect;
begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(R);
    Painter.DropDownGalleryDrawBackground(ACanvas.Handle, R);
    DrawFilterBand;
    if NeedsDrawSeparator(ilpAfterGallery) then
    begin
      ARect := Bounds;
      ARect.Top := ARect.Bottom - GetBottomSeparatorHeight;
      Painter.SubMenuControlDrawSeparator(ACanvas, ARect);
    end;
    if NeedsDrawSeparator(ilpBeforeGallery) then
    begin
      ARect := Bounds;
      ARect.Bottom := ARect.Top + GetBottomSeparatorHeight;
      Painter.SubMenuControlDrawSeparator(ACanvas, ARect);
    end;
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

function TdxRibbonOnMenuGalleryControlViewInfo.GetControlBounds: TRect;
begin
  Result := Control.ItemBounds;
end;

function TdxRibbonOnMenuGalleryControlViewInfo.GetGalleryHeight(AWidth: Integer): Integer;
begin
  Result := GetAbsoluteGroupTop(Control.GetGroups.Count, AWidth) - GalleryOptions.SpaceBetweenGroups;
  if GalleryOptions.RemoveVerticalItemPadding then
    Inc(Result);
  Result := Max(0, Result);
end;

function TdxRibbonOnMenuGalleryControlViewInfo.GetGalleryMargins: TRect;
begin
  Result := cxNullRect;
  if GalleryItem.IsFilterVisible then
    Inc(Result.Top, GetFilterBandHeight + FilterBandOffset);
  if NeedsDrawSeparator(ilpAfterGallery) then
    Inc(Result.Bottom, GetBottomSeparatorHeight);
  if NeedsDrawSeparator(ilpBeforeGallery) then
    Inc(Result.Top, GetBottomSeparatorHeight);
end;

function TdxRibbonOnMenuGalleryControlViewInfo.GetGroupItemDescriptionRect(
  AGroupIndex, AnItemIndex: Integer): TRect;
begin
  if FGroupItemDescriptionRectCache <> nil then
    Result := TcxRect(TObjectList(
      FGroupItemDescriptionRectCache[AGroupIndex]).Items[AnItemIndex]).Rect
  else
    Result := cxNullRect;
end;

function TdxRibbonOnMenuGalleryControlViewInfo.GetHeight(AWidth: Integer): Integer;
begin
  if GalleryOptions.RowCount = 0 then
    Result := GetGalleryHeight(AWidth)
  else
    Result := GetHeightByRowCount(AWidth);

  Result := Max(Result + cxMarginsHeight(GetGalleryMargins), Control.CalcMinHeight);
end;

function TdxRibbonOnMenuGalleryControlViewInfo.GetLayoutWidth(AColumnCount: Integer; out AGroupItemWidthIsNull: Boolean): Integer;
var
  I: Integer;
begin
  Result := 0;
  AGroupItemWidthIsNull := True;
  for I := 0 to Control.GetGroups.Count - 1 do
  begin
    Result := Max(Result, GetGroupItemSize(I).cx * AColumnCount +
      GetSpaceBetweenItems(I, True) * (AColumnCount - 1));
    if AGroupItemWidthIsNull and (GetGroupItemSize(I).cx <> 0) then
      AGroupItemWidthIsNull := False;
  end;
end;

procedure TdxRibbonOnMenuGalleryControlViewInfo.GroupItemYRange(
  const AGroupItem: TdxRibbonGalleryGroupItem; var ATop, ABottom: Integer);
var
  AGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
  ADestroyAfterUse: Boolean;
  AGallerySize: TSize;
begin
  AGallerySize := GallerySize;
  ATop := -FLayoutOffset +
    GetAbsoluteGroupTop(AGroupItem.Group.Index, AGallerySize.cx);
  AGroupViewInfo := GetGroupViewInfo(Control.GetGroups, Self,
    AGroupItem.Group.Index, ADestroyAfterUse);
  try
    {if ADestroyAfterUse then
      AGroupViewInfo.SetBounds(GalleryBounds);}
    ATop := AGroupViewInfo.GetRowTop(AGroupViewInfo.GetItemRow(
      AGroupItem.Index, AGallerySize.cx), ATop, AGallerySize.cx);
    ABottom := ATop + AGroupViewInfo.FItemSize.cy;
  finally
    if ADestroyAfterUse then
      AGroupViewInfo.Free;
  end;
end;

function TdxRibbonOnMenuGalleryControlViewInfo.GetMinSize: TSize;
begin
  Result.cx := Control.CalcDefaultWidth + cxMarginsWidth(GetGalleryMargins);
  if GroupCount <> 0 then
    Result.cy := Control.CalcMinHeight + cxMarginsHeight(GetGalleryMargins)
  else
    Result.cy := 0;
end;

function TdxRibbonOnMenuGalleryControlViewInfo.InternalGetScrollBarWidth: Integer;
begin
  Result := GetScaledScrollBarSize(Control.ScaleFactor).cx;
end;

procedure TdxRibbonOnMenuGalleryControlViewInfo.Changed;
begin
  inherited Changed;
  InitializeGroupItemDescriptionRectCache;
end;

procedure TdxRibbonOnMenuGalleryControlViewInfo.SetGroupItemDescriptionRect(AGroupIndex, AnItemIndex: Integer; ARect: TRect);
begin
  if FGroupItemDescriptionRectCache = nil then
    InitializeGroupItemDescriptionRectCache;
  TcxRect(TObjectList(FGroupItemDescriptionRectCache[AGroupIndex]).Items[AnItemIndex]).Rect := ARect;
end;

procedure TdxRibbonOnMenuGalleryControlViewInfo.CalculateFilterBand;
begin
  if GalleryItem.IsFilterVisible then
  begin
    FFilterBandRect := cxRectSetHeight(GetControlBounds, GetFilterBandHeight);
    if NeedsDrawSeparator(ilpBeforeGallery) then
      FFilterBandRect := cxRectOffsetVert(FFilterBandRect, GetBottomSeparatorHeight);
    FFilterBandContentRect := cxRectContent(FFilterBandRect, Painter.DropDownGalleryGetContentOffsets(DXBAR_GALLERYFILTERBAND));
    if Control.UseRightToLeftAlignment then
      FFilterBandContentRect := TdxRightToLeftLayoutConverter.ConvertRect(FFilterBandContentRect, FFilterBandRect);
  end;
end;

procedure TdxRibbonOnMenuGalleryControlViewInfo.DrawFilterBand;
begin
  if GalleryItem.IsFilterVisible then
  begin
    FillRectByColor(Control.Canvas.Handle,
      cxRectInflate(FFilterBandRect, 0, 0, 0, FilterBandOffset),
      Painter.DropDownGalleryGetFilterBandSeparatorColor);
    Painter.DropDownGalleryDrawFilterBand(Control.Canvas.Handle, FFilterBandRect);
    DrawFilterCaption;
  end;
end;

procedure TdxRibbonOnMenuGalleryControlViewInfo.DrawFilterCaption;

  function GetDrawTextFlags: DWORD;
  begin
    Result := DT_SINGLELINE or DT_TOP or DT_END_ELLIPSIS;
    if Control.UseRightToLeftAlignment then
      Result := Result or DT_RIGHT
    else
      Result := Result or DT_LEFT;
  end;

  function GetFilterSkinState: Integer;
  begin
    if FFilterBandHotTrack and not TdxRibbonOnSubmenuGalleryController(Control.Controller).IsFilterMenuShowed then
      Result := DXBAR_HOT
    else
      Result := DXBAR_NORMAL;
  end;

var
  AArrowOffset: Integer;
  AArrowRect, ACaptionRect: TRect;
  AArrowSize: Integer;
  ACanvas: TcxCanvas;
  ACaption: string;
  P: TcxArrowPoints;
begin
  ACanvas := Control.Canvas;
  ACanvas.Font.Color := Painter.DropDownGalleryGetFilterBandTextColor(GetFilterSkinState);
  AArrowOffset := ACanvas.TextWidth(' ') + FilterArrowOffset;
  AArrowSize := Control.ScaleFactor.Apply(FilterArrowSize);
  ACaption := GalleryItem.GetFilterCaption;

  ACaptionRect := FFilterBandContentRect;
  InflateRect(ACaptionRect, -GroupHeaderCaptionOffset, 0);
  Dec(ACaptionRect.Right, AArrowSize * 2 - 1 + AArrowOffset);

  cxDrawText(ACanvas.Handle, ACaption, ACaptionRect, GetDrawTextFlags + DT_CALCRECT);
  AArrowRect := cxRectBounds(ACaptionRect.Right + AArrowOffset, FFilterBandContentRect.Top +
    (cxRectHeight(FFilterBandContentRect) - AArrowSize) div 2, AArrowSize * 2 - 1, AArrowSize);
  if Control.UseRightToLeftAlignment then
  begin
    ACaptionRect := TdxRightToLeftLayoutConverter.ConvertRect(ACaptionRect, FFilterBandContentRect);
    AArrowRect := TdxRightToLeftLayoutConverter.ConvertRect(AArrowRect, FFilterBandContentRect);
  end;
  cxDrawText(ACanvas, ACaption, ACaptionRect, GetDrawTextFlags);
  cxLookAndFeelPaintersManager.GetPainter(lfsStandard).CalculateArrowPoints(AArrowRect, P, adDown, False, AArrowSize);
  ACanvas.SetBrushColor(ACanvas.Font.Color);
  ACanvas.Pen.Color := ACanvas.Font.Color;
  ACanvas.Polygon(P);
end;

function TdxRibbonOnMenuGalleryControlViewInfo.GetBottomSeparatorHeight: Integer;
begin
  Result := Painter.SubMenuGetSeparatorSize;
end;

function TdxRibbonOnMenuGalleryControlViewInfo.GetFilterBandHeight: Integer;
begin
  Result := cxTextHeight(Control.Parent.Font) + cxMarginsHeight(Painter.DropDownGalleryGetContentOffsets(DXBAR_GALLERYFILTERBAND));
end;

function TdxRibbonOnMenuGalleryControlViewInfo.GetHeightByRowCount(AWidth: Integer): Integer;
var
  ARowCount, ACurrentRow, ACurrentGroupIndex, AGroupRowCount: Integer;
  ACurrentGroupViewInfo: TdxRibbonGalleryGroupViewInfo;
  ADestroyAfterUse: Boolean;
  AGalleryOptions: TdxCustomRibbonGalleryOptions;
begin
  Result := 0;
  ACurrentRow := 0;
  ACurrentGroupIndex := 0;
  AGalleryOptions := GalleryOptions;
  ARowCount := AGalleryOptions.RowCount;
  while (ACurrentRow < ARowCount) and (ACurrentGroupIndex > -1) and (ACurrentGroupIndex < Control.GetGroups.Count) do
  begin
    if Result <> 0 then
      Inc(Result, AGalleryOptions.SpaceBetweenGroups);
    ACurrentGroupIndex := GetVisibleGroupIndex(ACurrentGroupIndex, True);
    if ACurrentGroupIndex <> -1 then
    begin
      ACurrentGroupViewInfo := GetGroupViewInfo(Control.GetGroups, Self, ACurrentGroupIndex, ADestroyAfterUse);
      try
        Inc(Result, ACurrentGroupViewInfo.Header.GetHeight(AWidth, True));
        AGroupRowCount := Min(ACurrentGroupViewInfo.GetRowCount(AWidth), ARowCount - ACurrentRow);
        Inc(Result, Max(0, AGroupRowCount * ACurrentGroupViewInfo.GetRowHeight - ACurrentGroupViewInfo.GetSpaceBetweenItems(False)));
        Inc(ACurrentRow, AGroupRowCount);
        Inc(ACurrentGroupIndex);
      finally
        if ADestroyAfterUse then
          ACurrentGroupViewInfo.Free;
      end;
    end;
  end;
end;

function TdxRibbonOnMenuGalleryControlViewInfo.GetSpaceBetweenItems(
  AGroupIndex: Integer; AIsFlat: Boolean): Integer;
begin
  if (Control.GetGroups[AGroupIndex].Options.RemoveHorizontalItemPadding and
    AIsFlat) or
    (Control.GetGroups[AGroupIndex].Options.RemoveVerticalItemPadding and
    not AIsFlat) then
    Result := 0
  else
    Result := Control.GetGroups[AGroupIndex].Options.SpaceBetweenItems;
end;

procedure TdxRibbonOnMenuGalleryControlViewInfo.InitializeGroupItemDescriptionRectCache;

  function IsGroupsWithDescription: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to GalleryItem.GalleryGroups.Count - 1 do
      if GalleryItem.GalleryGroups[I].Options.ItemTextKind = itkCaptionAndDescription then
      begin
        Result := True;
        Break;
      end;
  end;

var
  I, J: Integer;
  AObjectList: TObjectList;
begin
  if (GalleryOptions.ItemTextKind <> itkCaptionAndDescription) and
    not IsGroupsWithDescription then
    Exit;
  FGroupItemDescriptionRectCache.Free;
  FGroupItemDescriptionRectCache := TObjectList.Create;
  FGroupItemDescriptionRectCache.Capacity := GalleryItem.GalleryGroups.Count;
  for I := 0 to GalleryItem.GalleryGroups.Count - 1 do
  begin
    AObjectList := TObjectList.Create;
    FGroupItemDescriptionRectCache.Add(AObjectList);
    AObjectList.Capacity := GalleryItem.GalleryGroups[I].Items.Count;
    for J := 0 to GalleryItem.GalleryGroups[I].Items.Count - 1 do
      AObjectList.Add(TcxRect.Create(Self));
  end;
end;

function TdxRibbonOnMenuGalleryControlViewInfo.NeedsDrawSeparator(
  const APositionInDropDown: TdxRibbonGalleryItemBarItemLinkPositionInDropDown): Boolean;

  function GetNeedsDrawSeparatorFromInnerItemLinks: Boolean;
  var
    AControl: TdxRibbonDropDownGalleryControl;
    AItemLinks: TdxBarItemLinks;
    I: Integer;
  begin
    AControl := TdxRibbonDropDownGalleryControl(Control.Parent);
    if APositionInDropDown = ilpBeforeGallery then
      Result := (AControl.Resizing <> gsrNone) and not AControl.IsSizingBandAtBottom
    else
      Result := AControl.MarkExists;
    if not Result then
    begin
      AItemLinks := AControl.ItemLinks;
      for I := 0 to AItemLinks.VisibleItemCount - 1 do
        if (AItemLinks.VisibleItems[I] is TdxRibbonGalleryItemBarItemLink) and
          (TdxRibbonGalleryItemBarItemLink(AItemLinks.VisibleItems[I]).PositionInDropDown = APositionInDropDown) then
          Exit(True);
    end;
  end;

  function GetNeedsDrawSeparatorFromOuterItemLinks: Boolean;
  var
    AItemLinks: TdxBarItemLinks;
    AVisibleIndex, I: Integer;
  begin
    Result := Control.Parent.ItemLinks.VisibleItemCount > 1;
    if Result then
    begin
      AItemLinks := Control.Parent.ItemLinks;
      AVisibleIndex := -1;
      for I := 0 to AItemLinks.VisibleItemCount - 1 do
        if AItemLinks.VisibleItems[I].Item = GalleryItem then
        begin
          AVisibleIndex := I;
          Break;
        end;
      if AVisibleIndex < 0 then
        Result := False
      else
        if APositionInDropDown = ilpBeforeGallery then
          Result := AVisibleIndex > 0
        else
          Result := (AVisibleIndex + 1 <= AItemLinks.VisibleItemCount - 1) and
            not (AItemLinks.VisibleItems[AVisibleIndex + 1].Item is TdxCustomRibbonGalleryItem);
    end;
  end;

begin
  if (Control.Parent is TdxRibbonDropDownGalleryControl) and
    (TdxRibbonDropDownGalleryControl(Control.Parent).GalleryItem = GalleryItem) then
    Result := GetNeedsDrawSeparatorFromInnerItemLinks
  else
    Result := GetNeedsDrawSeparatorFromOuterItemLinks;
end;

{ TdxRibbonGalleryControlAccessibilityHelper }

function TdxRibbonGalleryControlAccessibilityHelper.HandleNavigationKey(
  var AKey: Word): Boolean;
begin
  Result := False;

  if not Control.Collapsed then
    if Control.Parent.Kind = bkBarControl then
    begin
      Result := AKey in [VK_RETURN, VK_SPACE, VK_UP, VK_DOWN];
      if Result then
        Control.DropDown(False);
    end
    else
    begin
      Result := True;
      case AKey of
        VK_DOWN: OnSubmenuController.Navigation(andDown);
        VK_LEFT: OnSubmenuController.Navigation(andLeft);
        VK_RIGHT: OnSubmenuController.Navigation(andRight);
        VK_UP: OnSubmenuController.Navigation(andUp);
        VK_TAB: OnSubmenuController.Tabulation;
        VK_PRIOR: OnSubmenuController.PageUp;
        VK_NEXT: OnSubmenuController.PageDown;
        VK_HOME: OnSubmenuController.HotTrackFirstGroupItem;
        VK_END: OnSubmenuController.HotTrackLastGroupItem;
      end;
    end;

  if not Result then
    Result := inherited HandleNavigationKey(AKey);
end;

function TdxRibbonGalleryControlAccessibilityHelper.IsNavigationKey(
  AKey: Word): Boolean;
begin
  Result := inherited IsNavigationKey(AKey);
  if not Control.Collapsed then
  begin
    if Control.Parent.Kind = bkBarControl then
      Result := Result or (AKey in [VK_RETURN, VK_SPACE, VK_UP, VK_DOWN])
    else
      Result := Result or (AKey in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_TAB,
        VK_PRIOR, VK_NEXT, VK_HOME, VK_END]);
  end;
end;

procedure TdxRibbonGalleryControlAccessibilityHelper.GetKeyTipData(
  AKeyTipsData: TList);
begin
  if not (not Control.Collapsed and (Control.Parent.Kind = bkSubMenu)) then
    inherited GetKeyTipData(AKeyTipsData);
end;

procedure TdxRibbonGalleryControlAccessibilityHelper.GetKeyTipInfo(
  out AKeyTipInfo: TdxBarKeyTipInfo);
var
  R: TRect;
begin
  inherited GetKeyTipInfo(AKeyTipInfo);
  if not Control.Collapsed and (Control.Parent.Kind = bkBarControl) then
  begin
    R := Control.ViewInfo.ScrollBarBounds;
    OffsetRect(R, Control.Parent.ClientOrigin.X, 0);
    AKeyTipInfo.BasePoint.X := cxRectCenter(R).X;
    AKeyTipInfo.HorzAlign := taRightJustify;
  end;
end;

procedure TdxRibbonGalleryControlAccessibilityHelper.OnSubmenuHotTrack(
  ANavigationDirection: TdxRibbonDropDownGalleryNavigationDirection);
begin
  case ANavigationDirection of
    dgndNone:
      if Control.Item.SelectedGroupItem <> nil then
        OnSubmenuController.HotTrackItem(Control.Item.SelectedGroupItem)
      else
        OnSubmenuController.HotTrackFirstGroupItem;
    dgndUp:
      OnSubmenuController.HotTrackLastGroupItem;
    dgndDown:
      OnSubmenuController.HotTrackFirstGroupItem;
  end;
end;

function TdxRibbonGalleryControlAccessibilityHelper.ShowDropDownWindow: Boolean;
begin
  TdxRibbonGalleryControl(ItemControl).DropDown(False);
  Result := ItemControl.IsDroppedDown;
end;

function TdxRibbonGalleryControlAccessibilityHelper.GetControl: TdxRibbonGalleryControl;
begin
  Result := TdxRibbonGalleryControl(FOwnerObject);
end;

function TdxRibbonGalleryControlAccessibilityHelper.GetOnSubmenuController: TdxRibbonOnSubmenuGalleryController;
begin
  Result := TdxRibbonOnSubmenuGalleryController(Control.Controller);
end;

{ TdxRibbonDropDownGalleryControlAccessibilityHelper }

// IdxBarAccessibilityHelper
function TdxRibbonDropDownGalleryControlAccessibilityHelper.HandleNavigationKey(
  var AKey: Word): Boolean;

  procedure HandleKeyUp;
  begin
    TdxBarItemLinksAccess(ItemLinks).Last.Control.IAccessibilityHelper.Select(AKey = VK_TAB);
  end;

  procedure HandleKeyDown;
  begin
    InternalGalleryItemControlAccessibilityHelper.Select(False);
    InternalGalleryItemControlAccessibilityHelper.OnSubmenuHotTrack(dgndNone);
  end;

begin
  if BarControl.SelectedControl = nil then
  begin
    Result := True;
    case AKey of
      VK_LEFT, VK_UP:
        if TdxBarItemLinksAccess(ItemLinks).Last = nil then
          HandleKeyDown
        else
          HandleKeyUp;
      VK_RIGHT:
        HandleKeyDown;
      VK_DOWN, VK_PRIOR:
        HandleKeyDown;
      VK_TAB:
        if ssShift in KeyboardStateToShiftState then
          HandleKeyUp
        else
          HandleKeyDown;
      VK_NEXT:
        begin
          InternalGalleryItemControlAccessibilityHelper.Select(False);
          TdxRibbonOnSubmenuGalleryController(BarControl.InternalGalleryItemControl.Controller).PageDown;
        end;
    end;
  end
  else
    Result := inherited HandleNavigationKey(AKey);
end;

function TdxRibbonDropDownGalleryControlAccessibilityHelper.IsNavigationKey(
  AKey: Word): Boolean;
begin
  Result := inherited IsNavigationKey(AKey);
  if BarControl.SelectedControl = nil then
    Result := Result or (AKey in [VK_PRIOR, VK_NEXT]);
end;

procedure TdxRibbonDropDownGalleryControlAccessibilityHelper.HandleVertNavigationKey(
  AUpKey, AFocusItemControl: Boolean);
begin
  if AUpKey and (BarControl.SelectedLink = TdxBarItemLinksAccess(ItemLinks).First) or
    not AUpKey and (BarControl.SelectedLink = TdxBarItemLinksAccess(ItemLinks).Last) then
  begin
    BarNavigationController.ChangeSelectedObject(AFocusItemControl,
      BarControl.InternalGalleryItemControl.IAccessibilityHelper);
    if AUpKey then
      InternalGalleryItemControlAccessibilityHelper.OnSubmenuHotTrack(dgndUp)
    else
      InternalGalleryItemControlAccessibilityHelper.OnSubmenuHotTrack(dgndDown);
  end
  else
    inherited HandleVertNavigationKey(AUpKey, AFocusItemControl);
end;

function TdxRibbonDropDownGalleryControlAccessibilityHelper.GetBarControl: TdxRibbonDropDownGalleryControl;
begin
  Result := TdxRibbonDropDownGalleryControl(FOwnerObject);
end;

function TdxRibbonDropDownGalleryControlAccessibilityHelper.GetInternalGalleryItemControlAccessibilityHelper: TdxRibbonGalleryControlAccessibilityHelper;
begin
  Result := TdxRibbonGalleryControlAccessibilityHelper(
    BarControl.InternalGalleryItemControl.IAccessibilityHelper.GetHelper);
end;

{ TdxRibbonGalleryScrollBarViewInfo }

procedure TdxRibbonGalleryScrollBarViewInfo.Calculate;
begin
  if TdxRibbonGalleryScrollBarHelper(ScrollBar).IsDropDownStyle then
    CalculateDropDownStyle
  else
    inherited Calculate;
end;

procedure TdxRibbonGalleryScrollBarViewInfo.CalculateDropDownStyle;
var
  AArrowButtonHeight: Integer;
  AHeight, AWidth: Integer;
begin
  AHeight := cxRectHeight(Bounds);
  AWidth := cxRectWidth(Bounds);

  if TdxRibbonGalleryScrollBarHelper(ScrollBar).IsTouchMode then
    AArrowButtonHeight := 0
  else
    AArrowButtonHeight := AHeight div 3;

  PartInfo[sbpLineUp].Bounds := cxRectBounds(0, 0, AWidth, AArrowButtonHeight);
  PartInfo[sbpLineDown].Bounds := cxRectBounds(0, PartInfo[sbpLineUp].Bounds.Bottom, AWidth, AArrowButtonHeight);
  CalculateThumbnailRect;
  FDropDownButtonRect := Rect(0, PartInfo[sbpLineDown].Bounds.Bottom, AWidth, AHeight);
end;

{ TdxRibbonGalleryScrollBarPainter }

procedure TdxRibbonGalleryScrollBarPainter.Paint(ACanvas: TcxCanvas);

  function IsDropDownButtonHot: Boolean;
  begin
    Result := ScrollBar.IsDropDownButtonUnderMouse or (BarNavigationController.SelectedObject <> nil) and
      (BarNavigationController.SelectedObject.GetHelper = GetGalleryControl.IAccessibilityHelper.GetHelper);
    Result := Result and not GetGalleryControl.IsDroppedDown and not (ViewInfo.HotPart in [sbpLineUp, sbpLineDown]);
  end;

  function GetDropDownButtonState: TcxButtonState;
  begin
    if not ScrollBar.Enabled then
      Result := cxbsDisabled
    else
      if IsDropDownButtonHot then
        Result := cxbsHot
      else
        Result := cxbsNormal;
  end;

const
  PartID: array[Boolean] of Integer = (
    DXBAR_INRIBBONGALLERYSCROLLBAR_DROPDOWNBUTTON,
    DXBAR_INRIBBONGALLERYSCROLLBAR_DROPDOWNBUTTON_TOUCH
  );
var
  R: TRect;
begin
  inherited Paint(ACanvas);

  if ScrollBar.IsDropDownStyle then
  begin
    ScrollBar.GetRibbonSkin.DrawBackground(ACanvas.Handle, ViewInfo.DropDownButtonRect,
      PartID[ScrollBar.IsTouchMode], ScrollBar.GetButtonSkinState(GetDropDownButtonState));
  end;

  if (GetGalleryControl <> nil) and TdxBarItemLinkAccess(GetGalleryControl.ItemLink).IsComponentSelected then
  begin
    R := ViewInfo.Bounds;
    Dec(R.Left, 2);
    if BarDesignController.NeedDefaultSelection(GetGalleryControl.ItemLink) then
      dxBarFrameRect(ACanvas.Handle, R, COLOR_WINDOWTEXT)
    else
      dxBarFocusRect(ACanvas.Handle, R);
  end;
end;

procedure TdxRibbonGalleryScrollBarPainter.DoDrawScrollBarPart(
  ACanvas: TcxCanvas; const R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState);

  function GetButtonKind: TdxInRibbonGalleryScrollBarButtonKind;
  begin
    if APart = sbpLineUp then
      Result := gsbkLineUp
    else
      Result := gsbkLineDown;
  end;

  function GetButtonSkinPart: Integer;
  begin
    case APart of
      sbpLineUp:
        Result := DXBAR_INRIBBONGALLERYSCROLLBAR_LINEUPBUTTON;
      sbpLineDown:
        Result := DXBAR_INRIBBONGALLERYSCROLLBAR_LINEDOWNBUTTON;
    else
      Result := 0;
    end;
  end;

begin
  if ScrollBar.IsDropDownStyle then
  begin
    if GetButtonSkinPart <> 0 then
    begin
      if not ScrollBar.IsButtonEnabled(GetButtonKind) then
        AState := cxbsDisabled;
      ScrollBar.GetRibbonSkin.DrawBackground(ACanvas.Handle, R, GetButtonSkinPart, ScrollBar.GetButtonSkinState(AState));
    end;
  end
  else
    ScrollBar.GetBarPainter.DropDownGalleryDrawScrollBarPartEx(GetGalleryControl, ACanvas.Handle, R, APart, AState, ScaleFactor);
end;

procedure TdxRibbonGalleryScrollBarPainter.DrawScrollBarBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  if ScrollBar.IsDropDownStyle then
  begin
    if ScrollBar.GetRibbonSkin.GetIsAlphaUsed(DXBAR_INRIBBONGALLERYSCROLLBAR_BACKGROUND) then
      cxDrawTransparentControlBackground(ScrollBar.FRibbonScrollBar, ACanvas, R, False);
    ScrollBar.GetRibbonSkin.DrawBackground(ACanvas.Handle, R, DXBAR_INRIBBONGALLERYSCROLLBAR_BACKGROUND);
  end
  else
    ScrollBar.GetBarPainter.DropDownGalleryDrawScrollBarBackgroundEx(GetGalleryControl, ACanvas.Handle, R, ScaleFactor);
end;

function TdxRibbonGalleryScrollBarPainter.GetGalleryControl: TdxRibbonGalleryControl;
begin
  Result := ScrollBar.GalleryControl;
end;

function TdxRibbonGalleryScrollBarPainter.GetScrollBar: TdxRibbonGalleryScrollBarHelper;
begin
  Result := TdxRibbonGalleryScrollBarHelper(inherited ScrollBar);
end;

function TdxRibbonGalleryScrollBarPainter.GetViewInfo: TdxRibbonGalleryScrollBarViewInfo;
begin
  Result := TdxRibbonGalleryScrollBarViewInfo(inherited ViewInfo);
end;

{ TdxRibbonGalleryScrollBarHelper }

constructor TdxRibbonGalleryScrollBarHelper.Create(AOwner: IcxScrollBarOwner);
var
  AComponentReference: IInterfaceComponentReference;
begin
  inherited Create(AOwner);
  Supports(AOwner, IInterfaceComponentReference, AComponentReference);
  FRibbonScrollBar := AComponentReference.GetComponent as TdxRibbonGalleryScrollBar;
end;

procedure TdxRibbonGalleryScrollBarHelper.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not IsDropDownButtonUnderMouse then
    TCustomdxBarControlAccess(GalleryControl.Parent).SetKeySelectedItem(nil);
  inherited;
  if (Button = mbLeft) and IsDropDownButtonUnderMouse then
  begin
    IsDropDownButtonPressed := True;
    Repaint;
    FRibbonScrollBar.DoDropDown;
  end;
end;

function TdxRibbonGalleryScrollBarHelper.GetPainterClass: TcxScrollBarPainterClass;
begin
  Result := TdxRibbonGalleryScrollBarPainter;
end;

function TdxRibbonGalleryScrollBarHelper.GetViewInfoClass: TcxScrollBarViewInfoClass;
begin
  Result := TdxRibbonGalleryScrollBarViewInfo;
end;

function TdxRibbonGalleryScrollBarHelper.GetBarPainter: TdxBarPainter;
begin
  if GalleryControl.IsValidPainter then
    Result := GalleryControl.Painter
  else
    Result := nil;
end;

function TdxRibbonGalleryScrollBarHelper.GetButtonSkinState(AState: TcxButtonState): Integer;
begin
  case AState of
    cxbsNormal:   Result := DXBAR_NORMAL;
    cxbsHot:      Result := DXBAR_HOT;
    cxbsPressed:  Result := DXBAR_PRESSED;
    cxbsDisabled: Result := DXBAR_DISABLED;
  else
    raise EdxException.Create('Invalid button state');
  end;
end;

function TdxRibbonGalleryScrollBarHelper.GetRibbonSkin: IdxSkin;
begin
  Result := (GetBarPainter as TdxBarSkinnedPainter).Skin;
end;

function TdxRibbonGalleryScrollBarHelper.IsButtonEnabled(AButtonKind: TdxInRibbonGalleryScrollBarButtonKind): Boolean;
begin
  Result := Enabled;
  if Result then
    case AButtonKind of
      gsbkLineUp:
        Result := GalleryControl.ViewInfo.GetPreviousButtonEnabled;
      gsbkLineDown:
        Result := GalleryControl.ViewInfo.GetNextButtonEnabled;
    end;
end;

function TdxRibbonGalleryScrollBarHelper.IsDropDownButtonUnderMouse: Boolean;
var
  R: TRect;
begin
  Result := HandleAllocated and (WindowFromPoint(GetMouseCursorPos) = Handle);
  if Result then
  begin
    R := TdxRibbonGalleryScrollBarViewInfo(ViewInfo).DropDownButtonRect;
    R := dxMapWindowRect(Handle, 0, R);
    Result := PtInRect(R, GetMouseCursorPos);
  end;
end;

function TdxRibbonGalleryScrollBarHelper.IsDropDownStyle: Boolean;
begin
  Result := FRibbonScrollBar.IsDropDownStyle;
end;

function TdxRibbonGalleryScrollBarHelper.IsTouchMode: Boolean;
var
  APainter: TdxBarPainter;
begin
  if cxIsTouchModeEnabled then
    Exit(True);

  APainter := GetBarPainter;
  Result := (APainter is TdxRibbonBarPainter) and TdxRibbonBarPainter(APainter).IsSimplifiedGroupsLayout;
end;

function TdxRibbonGalleryScrollBarHelper.GetGalleryControl: TdxRibbonGalleryControl;
begin
  Result := FRibbonScrollBar.GalleryControl;
end;

{ TdxRibbonGalleryScrollBar }

constructor TdxRibbonGalleryScrollBar.Create(AGalleryControl: TdxRibbonGalleryControl);
var
  ASkinName: string;
begin
  inherited Create(nil);
  Kind := sbVertical;
  FGalleryControl := AGalleryControl;
  ScaleFactor.Assign(AGalleryControl.ScaleFactor);

  if Helper.GetBarPainter <> nil then
    ASkinName := Helper.GetBarPainter.DropDownGalleryGetName
  else
    ASkinName := '';

  LookAndFeel.SkinName := ASkinName;
  LookAndFeel.NativeStyle := ASkinName = '';
  UnlimitedTracking := True;
end;

function TdxRibbonGalleryScrollBar.IsDropDownStyle: Boolean;
begin
  Result := (Parent <> nil) and (TCustomdxBarControl(Parent).Kind = bkBarControl);
end;

procedure TdxRibbonGalleryScrollBar.DoDropDown;
begin
  dxCallNotify(OnDropDown, Self);
end;

function TdxRibbonGalleryScrollBar.GetHelper: TdxRibbonGalleryScrollBarHelper;
begin
  Result := TdxRibbonGalleryScrollBarHelper(inherited Helper);
end;

function TdxRibbonGalleryScrollBar.GetIsDropDownButtonPressed: Boolean;
begin
  Result := Helper.IsDropDownButtonPressed;
end;

function TdxRibbonGalleryScrollBar.GetHelperClass: TcxScrollBarHelperClass;
begin
  Result := TdxRibbonGalleryScrollBarHelper;
end;

procedure TdxRibbonGalleryScrollBar.WMCaptureChanged(var Message: TMessage);
begin
  inherited;
  Helper.IsDropDownButtonPressed := False;
end;

procedure TdxRibbonGalleryScrollBar.WMNCDestroy(var Message: TWMNCDestroy);
begin
  inherited;
  if Helper <> nil then
    Helper.IsDropDownButtonPressed := False;
end;

{ TdxRibbonDropDownGallery }

function TdxRibbonDropDownGallery.CreateBarControl: TCustomdxBarControl;
begin
  Result := inherited CreateBarControl;
  if HasValidGalleryItem then
    TdxRibbonDropDownGalleryControl(Result).GalleryItem := GalleryItem;
end;

function TdxRibbonDropDownGallery.GetControlClass: TCustomdxBarControlClass;
begin
  if HasValidGalleryItem then
    Result := TdxRibbonDropDownGalleryControl
  else
    Result := inherited GetControlClass;
end;

function TdxRibbonDropDownGallery.GetItemLinksClass: TdxBarItemLinksClass;
begin
  Result := TdxRibbonGalleryItemBarItemLinks;
end;

function TdxRibbonDropDownGallery.HasValidGalleryItem: Boolean;
begin
  Result := (GalleryItem <> nil) and (Ribbon <> nil) and
    (GalleryItem.BarManager = Ribbon.BarManager);
end;

procedure TdxRibbonDropDownGallery.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = GalleryItem) then
    FGalleryItem := nil;
end;

procedure TdxRibbonDropDownGallery.SetGalleryItem(
  Value: TdxCustomRibbonGalleryItem);
begin
  if Value <> FGalleryItem then
  begin
    cxRemoveFreeNotification(Self, FGalleryItem);
    FGalleryItem := Value;
    cxAddFreeNotification(Self, FGalleryItem);
  end;
end;

{ TdxRibbonDropDownGalleryGalleryItemBarItemLinks }

function TdxRibbonDropDownGalleryGalleryItemBarItemLinks.CanContainItem(
  AItem: TdxBarItem; out AErrorText: string): Boolean;
begin
  Result := True;
end;

{ TdxRibbonDropDownGalleryControlPainter }

constructor TdxRibbonDropDownGalleryControlPainter.Create(APainter: TdxBarPainter);
begin
  FPainter := APainter;
end;

function TdxRibbonDropDownGalleryControlPainter.GetSizingBandHeight(
  AGalleryControl: TdxRibbonDropDownGalleryControl): Integer;
var
  AButtonHeight: Integer;
begin
  Result := 0;
  if HasSizingBand(AGalleryControl) then
  begin
    AButtonHeight := Painter.DropDownGalleryGetSizingBandHeight(
      AGalleryControl.BarManager.ImageOptions.GlyphSize, cxTextHeight(AGalleryControl.Font),
      AGalleryControl.ScaleFactor);

    case AGalleryControl.Resizing of
      gsrHeight:
        Result := (AButtonHeight * 6 + 100) div 29;
      gsrWidthAndHeight:
        Result := (AButtonHeight * 9 + 121) div 29;
    end;
  end;
end;

function TdxRibbonDropDownGalleryControlPainter.PtInSizingArea(
  AGalleryControl: TdxRibbonDropDownGalleryControl; const P: TPoint): Boolean;
var
  AOffsetX: Integer;
  R: TRect;
begin
  Result := False;
  if AGalleryControl.IsSizingBandAtBottom then
  begin
    case AGalleryControl.Resizing of
      gsrHeight:
        begin
          R := Rect(0, 0, AGalleryControl.Width, AGalleryControl.Height);
          R.Top := R.Bottom - (GetSizingBandHeight(AGalleryControl) + Painter.DropDownGalleryGetNCBorderSize);
          Result := PtInRect(R, P);
        end;
      gsrWidthAndHeight:
        begin
          if AGalleryControl.UseRightToLeftAlignment then
            AOffsetX := GetSizingBandHeight(AGalleryControl) - P.X
          else
            AOffsetX := P.X - (AGalleryControl.Width - GetSizingBandHeight(AGalleryControl));
          Result := (AOffsetX >= 0) and (P.Y < AGalleryControl.Height) and
            (AGalleryControl.Height - 1 - P.Y <= AOffsetX);
        end;
    end;
  end
  else
  begin
    case AGalleryControl.Resizing of
      gsrHeight:
        begin
          R := Rect(0, 0, AGalleryControl.Width, AGalleryControl.Height);
          R.Bottom := R.Top + (GetSizingBandHeight(AGalleryControl) + Painter.DropDownGalleryGetNCBorderSize);
          Result := PtInRect(R, P);
        end;
      gsrWidthAndHeight:
        begin
          if AGalleryControl.UseRightToLeftAlignment then
            AOffsetX := GetSizingBandHeight(AGalleryControl) - P.X + 1
          else
            AOffsetX := P.X - (AGalleryControl.Width - GetSizingBandHeight(AGalleryControl) - 1);
          Result := (AOffsetX >= 0) and (P.Y >= 0) and (P.Y <= AOffsetX);
        end;
    end;
  end;
end;

procedure TdxRibbonDropDownGalleryControlPainter.SubMenuControlDrawBorder(
  ABarSubMenuControl: TdxBarSubMenuControl; DC: HDC; R: TRect);

  procedure DrawBottomSizingBand(ADropDownGallery: TdxRibbonDropDownGalleryControl; DC: HDC; R: TRect);
  begin
    R.Top := R.Bottom - GetSizingBandHeight(ADropDownGallery);
    Painter.DropDownGalleryDrawBottomSizingBand(DC, R);
    case ADropDownGallery.Resizing of
      gsrHeight:
        Painter.DropDownGalleryDrawBottomVerticalSizeGrip(DC, R);
      gsrWidthAndHeight:
        Painter.DropDownGalleryDrawBottomSizeGripEx(ABarSubMenuControl, DC, R);
    end;
  end;

  procedure DrawTopSizingBand(ADropDownGallery: TdxRibbonDropDownGalleryControl; DC: HDC; R: TRect);
  begin
    R.Bottom := R.Top + GetSizingBandHeight(ADropDownGallery);
    Painter.DropDownGalleryDrawTopSizingBand(DC, R);
    case ADropDownGallery.Resizing of
      gsrHeight:
        Painter.DropDownGalleryDrawTopVerticalSizeGrip(DC, R);
      gsrWidthAndHeight:
        Painter.DropDownGalleryDrawTopSizeGripEx(ABarSubMenuControl, DC, R);
    end;
  end;

var
  ABorderSize: Integer;
  ADropDownGallery: TdxRibbonDropDownGalleryControl;
begin
  ADropDownGallery := ABarSubMenuControl as TdxRibbonDropDownGalleryControl;
  Painter.DropDownGalleryDrawBorder(ABarSubMenuControl, DC, R);
  if HasSizingBand(ADropDownGallery) then
  begin
    ABorderSize := Painter.DropDownGalleryGetNCBorderSize;
    InflateRect(R, -ABorderSize, -ABorderSize);
    if ADropDownGallery.IsSizingBandAtBottom then
      DrawBottomSizingBand(ADropDownGallery, DC, R)
    else
      DrawTopSizingBand(ADropDownGallery, DC, R);
  end;
end;

function TdxRibbonDropDownGalleryControlPainter.HasSizingBand(
  AGalleryControl: TdxRibbonDropDownGalleryControl): Boolean;
begin
  Result := AGalleryControl.Resizing <> gsrNone;
end;

{ TdxRibbonDropDownGalleryControl }

constructor TdxRibbonDropDownGalleryControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  FGestureHelper := TdxGestureHelper.Create(Self);
  Touch.InteractiveGestures := GetDefaultInteractiveGestures;
  Touch.InteractiveGestureOptions := GetDefaultInteractiveGestureOptions;
end;

destructor TdxRibbonDropDownGalleryControl.Destroy;
begin
  FreeAndNil(FGestureHelper);
  FreeAndNil(FGalleryItemBarItemLinks);
  FreeAndNil(FInternalPainter);
  inherited Destroy;
end;

procedure TdxRibbonDropDownGalleryControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FHeight := AHeight;
  FUseInternalSizeValue := True;
  try
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  finally
    FUseInternalSizeValue := False;
  end;
end;

// IdxGestureClient
function TdxRibbonDropDownGalleryControl.AllowGesture(AGestureId: Integer): Boolean;
begin
  Result := (GetInteractiveGestureByGestureID(AGestureId) in Touch.InteractiveGestures);
end;

function TdxRibbonDropDownGalleryControl.AllowPan(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := (AScrollKind = InternalGalleryItemControl.ScrollBar.Kind) and InternalGalleryItemControl.ScrollBar.Enabled;
end;

procedure TdxRibbonDropDownGalleryControl.BeginGestureScroll(APos: TPoint);
begin

end;

procedure TdxRibbonDropDownGalleryControl.EndGestureScroll;
begin

end;

procedure TdxRibbonDropDownGalleryControl.GestureScroll(ADeltaX, ADeltaY: Integer);
var
  ANewPos: Integer;
begin
  ANewPos := InternalGalleryItemControl.ScrollBar.Position - ADeltaY;
  FGestureHelper.CheckOverpan(InternalGalleryItemControl.ScrollBar.Kind, ANewPos,
    0, InternalGalleryItemControl.GetScrollBarMaxPos, ADeltaX, ADeltaY);
  ANewPos := EnsureRange(ANewPos, 0, InternalGalleryItemControl.GetScrollBarMaxPos);
  if ANewPos <> InternalGalleryItemControl.ScrollBar.Position then
    InternalGalleryItemControl.SetScrollBarPosition(ANewPos);
end;

function TdxRibbonDropDownGalleryControl.GetPanOptions: Integer;
begin
   Result := GetPanOptionsByInteractiveGestureOptions(Touch.InteractiveGestureOptions);
end;

function TdxRibbonDropDownGalleryControl.IsPanArea(const APoint: TPoint): Boolean;
begin
  Result := PtInRect(InternalGalleryItemControl.ViewInfo.GalleryBounds, APoint);
end;

function TdxRibbonDropDownGalleryControl.NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := True;
end;

function TdxRibbonDropDownGalleryControl.GetGestureClient(const APoint: TPoint): IdxGestureClient;
begin
  Result := Self;
end;

function TdxRibbonDropDownGalleryControl.GetGestureClientHandle: THandle;
begin
  Result := Handle;
end;

function TdxRibbonDropDownGalleryControl.IsGestureTarget(AWnd: THandle): Boolean;
begin
  Result := AWnd = Handle;
end;

procedure TdxRibbonDropDownGalleryControl.DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  FGestureHelper.DoGesture(EventInfo, Handled);
end;

procedure TdxRibbonDropDownGalleryControl.DoGetGestureOptions(var Gestures: TInteractiveGestures;
  var Options: TInteractiveGestureOptions);
begin
  inherited;
  FGestureHelper.CheckGestureOptions(Gestures, Options);
end;

function TdxRibbonDropDownGalleryControl.GetDefaultInteractiveGestures: TInteractiveGestures;
var
  I: Integer;
begin
  Result := [];
  for I := 0 to dxSupportedGestureCount - 1 do
    if IsDefaultGesture(dxSupportedGestureIDs[I]) then
      Include(Result, GetInteractiveGestureByGestureID(dxSupportedGestureIDs[I]));
end;

function TdxRibbonDropDownGalleryControl.GetDefaultInteractiveGestureOptions: TInteractiveGestureOptions;
begin
  Result := GetInteractiveGestureOptionsByPanOptions(GetDefaultPanOptions);
end;

function TdxRibbonDropDownGalleryControl.IsTouchPropertyStored(AProperty: TTouchProperty): Boolean;
begin
  case AProperty of
    tpInteractiveGestures:
      Result := Touch.InteractiveGestures <> GetDefaultInteractiveGestures;
    tpInteractiveGestureOptions:
      Result := Touch.InteractiveGestureOptions <> GetDefaultInteractiveGestureOptions;
  else
    Result := inherited IsTouchPropertyStored(AProperty);
  end;
end;

function TdxRibbonDropDownGalleryControl.IsDefaultGesture(AGestureID: Integer): Boolean;
begin
  Result := AGestureID in [GID_PAN, GID_PRESSANDTAP];
end;

function TdxRibbonDropDownGalleryControl.IsDoubleBufferedNeeded: Boolean;
begin
  Result := True;
end;

function TdxRibbonDropDownGalleryControl.GetDefaultPanOptions: Integer;
begin
  Result := dxTouchPanOptions;
end;

procedure TdxRibbonDropDownGalleryControl.CalcColumnItemRects(ATopIndex: Integer; out ALastItemBottom: Integer);
var
  AGalleryHeight, AGalleryTop, ATopItemIndex, ABottomItemIndex, APos, I: Integer;
  AItemLink: TdxRibbonGalleryItemBarItemLink;
begin
  inherited CalcColumnItemRects(ATopIndex, ALastItemBottom);

  if ItemLinks.VisibleItemCount > 0 then
  begin
    AGalleryHeight := VisibleItemsRect.Bottom - ALastItemBottom;

    ATopItemIndex := -1;
    for I := 0 to ItemLinks.Count - 1 do
    begin
      AItemLink := ItemLinks[I] as TdxRibbonGalleryItemBarItemLink;
      if AItemLink.PositionInDropDown = ilpBeforeGallery then
      begin
        if ATopItemIndex >= 0 then
          APos := ItemLinks[ATopItemIndex].ItemRect.Bottom
        else
          APos := VisibleItemsRect.Top;
        APos := APos + GetItemControlOffset(AItemLink);
        AItemLink.ItemRect := cxRectSetTop(AItemLink.ItemRect, APos);
        ATopItemIndex := I;
      end;
    end;

    ABottomItemIndex := -1;
    for I := ItemLinks.Count - 1 downto 0 do
    begin
      AItemLink := ItemLinks[I] as TdxRibbonGalleryItemBarItemLink;
      if AItemLink.PositionInDropDown = ilpAfterGallery then
      begin
        if ABottomItemIndex >= 0 then
          APos := ItemLinks[ABottomItemIndex].ItemRect.Top - GetItemControlOffset(ItemLinks[ABottomItemIndex])
        else
          APos := VisibleItemsRect.Bottom;
        AItemLink.ItemRect := cxRectSetBottom(AItemLink.ItemRect, APos);
        ABottomItemIndex := I;
      end;
    end;

    if ATopItemIndex >= 0 then
      AGalleryTop := ItemLinks[ATopItemIndex].ItemRect.Bottom
    else
      AGalleryTop := VisibleItemsRect.Top;
  end
  else
  begin
    AGalleryHeight := cxRectHeight(VisibleItemsRect);
    AGalleryTop := 0;
  end;

  InternalGalleryItemLink.ItemRect := cxRectSetTop(VisibleItemsRect, AGalleryTop, AGalleryHeight);
  InternalGalleryItemControl.SizeChanged := False;
end;

function TdxRibbonDropDownGalleryControl.ChangeSizeByChildItemControl(
  out ASize: TSize): Boolean;
begin
  Result := False;
  ASize := cxNullSize;
  if InternalGalleryItemControl.Item.GalleryOptions.SubmenuResizing = gsrNone then
  begin
    FGalleryItemBarItemLinks.BeginCalcItemRects;
    try
      CalcControlsPositions;
    finally
      FGalleryItemBarItemLinks.EndCalcItemRects;
    end;
    if (InternalGalleryItemControl.GetDefaultWidthInSubmenu > Width) and
      (Screen.Width > Width + InternalGalleryItemControl.ViewInfo.GetScrollBarWidth) then
    begin
      ASize.cx := Width + InternalGalleryItemControl.ViewInfo.GetScrollBarWidth;
      ASize.cy := Height;
      Result := True;
    end;
  end;
end;

procedure TdxRibbonDropDownGalleryControl.CreateWnd;
begin
  if FGalleryItemBarItemLinks = nil then
    FGalleryItemBarItemLinks := TdxRibbonDropDownGalleryGalleryItemBarItemLinks.Create(
      BarManager, TdxBarItemLinksAccess(ItemLinks).LinksOwner);
  FGalleryItemBarItemLinks.Internal := True;
  FGalleryItemBarItemLinks.BarControl := Self;
  FGalleryItemBarItemLinks.Add.Item := GalleryItem;
  FGalleryItemBarItemLinks[0].CreateControl;
  InternalGalleryItemControl.Collapsed := False;
  inherited CreateWnd;
end;

function TdxRibbonDropDownGalleryControl.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  InternalGalleryItemControl.SetScrollBarPosition(InternalGalleryItemControl.ScrollBar.Position + GetMouseWheelStep);
end;

function TdxRibbonDropDownGalleryControl.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  InternalGalleryItemControl.SetScrollBarPosition(InternalGalleryItemControl.ScrollBar.Position - GetMouseWheelStep);
end;

procedure TdxRibbonDropDownGalleryControl.DoNCPaint(DC: HDC; const ARect: TRect);
begin
  InternalPainter.SubmenuControlDrawBorder(Self, DC, ARect);
end;

function TdxRibbonDropDownGalleryControl.DoFindLinkWithAccel(
  AKey: Word; AShift: TShiftState; ACurrentLink: TdxBarItemLink): TdxBarItemLink;
begin
  if (ACurrentLink <> nil) and (ACurrentLink.Control <> nil) and (ACurrentLink.Control = InternalGalleryItemControl) then
    Result := nil
  else
    Result := inherited DoFindLinkWithAccel(AKey, AShift, ACurrentLink);
end;

function TdxRibbonDropDownGalleryControl.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxRibbonDropDownGalleryControlAccessibilityHelper;
end;

function TdxRibbonDropDownGalleryControl.GetBorderSize: Integer;
begin
  Result := Painter.DropDownGalleryGetNCBorderSize + Painter.DropDownGalleryGetClientBorderSize;
end;

function TdxRibbonDropDownGalleryControl.GetClientOffset(AIncludeDetachCaption: Boolean = True): TRect;
var
  AOffset: Integer;
begin
  AOffset := Painter.DropDownGalleryGetNCBorderSize;
  Result := Rect(AOffset, AOffset, AOffset, AOffset);
  if AIncludeDetachCaption and Detachable then
    Inc(Result.Top, DetachCaptionAreaSize);

  if Resizing <> gsrNone then
  begin
    if IsSizingBandAtBottom then
      Inc(Result.Bottom, InternalPainter.GetSizingBandHeight(Self))
    else
      Inc(Result.Top, InternalPainter.GetSizingBandHeight(Self))
  end;
end;

function TdxRibbonDropDownGalleryControl.GetItemsPaneSize: TSize;
begin
  Result := inherited GetItemsPaneSize;
  if Result.cx <= InternalGalleryItemControl.Width then
  begin
    Result.cx := InternalGalleryItemControl.Width;
    Inc(Result.cy, InternalGalleryItemControl.Height);
  end
  else
    Inc(Result.cy, InternalGalleryItemControl.ViewInfo.GetHeight(Result.cx));
end;

function TdxRibbonDropDownGalleryControl.GetItemsRectOffset: TRect;
var
  AOffset: Integer;
begin
  AOffset := Painter.DropDownGalleryGetClientBorderSize;
  Result := Rect(AOffset, AOffset, AOffset, AOffset);
  Result.Left := Result.Left + BarSize;
end;

procedure TdxRibbonDropDownGalleryControl.WndProc(var Message: TMessage);
begin
  if (FGestureHelper = nil) or not FGestureHelper.HandleMessage(Message) then
    inherited;
end;

function TdxRibbonDropDownGalleryControl.GetMinSize: TSize;
var
  I: Integer;
begin
  Result := InternalGalleryItemControl.ViewInfo.GetMinSize;
  for I := 0 to ItemLinks.VisibleItemCount - 1 do
    Inc(Result.cy, cxRectHeight(ItemLinks[I].ItemRect));
  DoCorrectSize(Result);
end;

function TdxRibbonDropDownGalleryControl.GetViewInfoClass: TCustomdxBarControlViewInfoClass;
begin
  Result := TdxRibbonDropDownGalleryControlViewInfo;
end;

function TdxRibbonDropDownGalleryControl.IsControlExists(
  ABarItemControl: TdxBarItemControl): Boolean;
begin
  Result := (ABarItemControl = InternalGalleryItemControl) or
    inherited IsControlExists(ABarItemControl);
end;

function TdxRibbonDropDownGalleryControl.IsResizing: Boolean;
begin
  Result := FResizingState <> grsNone;
end;

function TdxRibbonDropDownGalleryControl.IsSizingBandAtBottom: Boolean;
begin
  Result := Top + IfThen(FUseInternalSizeValue, FHeight, Height) > OnShowTop;
end;

procedure TdxRibbonDropDownGalleryControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if IsResizing then
    DoResizing
  else
    inherited MouseMove(Shift, X, Y);
end;

function TdxRibbonDropDownGalleryControl.MustFitInWorkAreaWidth: Boolean;
begin
  Result := True;
end;

function TdxRibbonDropDownGalleryControl.NeedsMouseWheel: Boolean;
begin
  Result := (SelectedControl = InternalGalleryItemControl) and
    InternalGalleryItemControl.IsNeedScrollBar;
end;

function TdxRibbonDropDownGalleryControl.NeedsSelectFirstItemOnDropDownByKey: Boolean;
begin
  Result := False;
end;

procedure TdxRibbonDropDownGalleryControl.Resize;
begin
  inherited Resize;
  InternalGalleryItemControl.SizeChanged := True;
end;

procedure TdxRibbonDropDownGalleryControl.UpdateItem(AControl: TdxBarItemControl);
begin
  cxInvalidateRect(Handle, GetItemRect(AControl), False);
end;

procedure TdxRibbonDropDownGalleryControl.DoResizing;

  procedure SetSize(const ARect: TRect; DontChangeOrigin: Boolean);
  var
    AMinSize: TSize;
    AFlags: Cardinal;
    ANewRect: TRect;
  begin
    ANewRect := ARect;
    AMinSize := GetMinSize;
    if cxRectWidth(ANewRect) < AMinSize.cx then
      if UseRightToLeftAlignment then
        ANewRect.Left := ANewRect.Right - AMinSize.cx
      else
        ANewRect.Right := ANewRect.Left + AMinSize.cx;
    if cxRectHeight(ANewRect) < AMinSize.cy then
      if DontChangeOrigin or UseRightToLeftAlignment and not (FResizingState in [grsTopRight, grsTop]) then
        ANewRect.Bottom := ANewRect.Top + AMinSize.cy
      else
        ANewRect.Top := ANewRect.Bottom - AMinSize.cy;

    AFlags := SWP_NOZORDER or SWP_NOACTIVATE;
    if DontChangeOrigin then
      AFlags := SWP_NOMOVE or AFlags;
    SetWindowPos(Handle, 0, ANewRect.Left, ANewRect.Top, cxRectWidth(ANewRect), cxRectHeight(ANewRect), AFlags);
  end;

var
  R: TRect;
  P: TPoint;
begin
  FocusItemControl(InternalGalleryItemControl);
  R := WindowRect;
  P := GetMouseCursorPos;
  case FResizingState of
    grsBottom:
      SetSize(Rect(R.Left, R.Top, R.Right, P.Y + FMouseResizingDelta.Y), True);
    grsBottomRight:
      if UseRightToLeftAlignment then
        SetSize(Rect(P.X + FMouseResizingDelta.X, R.Top, R.Right, P.Y + FMouseResizingDelta.Y), False)
      else
        SetSize(Rect(R.Left, R.Top, P.X + FMouseResizingDelta.X, P.Y + FMouseResizingDelta.Y), True);
    grsTop:
      SetSize(Rect(R.Left, P.Y + FMouseResizingDelta.Y, R.Right, R.Bottom), False);
    grsTopRight:
      if UseRightToLeftAlignment then
        SetSize(Rect(P.X + FMouseResizingDelta.X, P.Y + FMouseResizingDelta.Y, R.Right, R.Bottom), False)
      else
        SetSize(Rect(R.Left, P.Y + FMouseResizingDelta.Y, P.X + FMouseResizingDelta.X, R.Bottom), False);
  end;
  UpdateWindow(Handle);
end;

function TdxRibbonDropDownGalleryControl.GetInternalGalleryItemControl: TdxRibbonGalleryControl;
begin
  Result := TdxRibbonGalleryControl(FGalleryItemBarItemLinks[0].Control);
end;

function TdxRibbonDropDownGalleryControl.GetInternalGalleryItemLink: TdxBarItemLink;
begin
  Result := FGalleryItemBarItemLinks[0];
end;

function TdxRibbonDropDownGalleryControl.GetInternalPainter: TdxRibbonDropDownGalleryControlPainter;
begin
  if FInternalPainter = nil then
    FInternalPainter := TdxRibbonDropDownGalleryControlPainter.Create(Painter);
  Result := FInternalPainter;
end;

function TdxRibbonDropDownGalleryControl.GetMouseWheelStep: Integer;
begin
  Result := IfThen(FMouseWheelStep = 0,
    InternalGalleryItemControl.GetMouseWheelStep, FMouseWheelStep);
end;

function TdxRibbonDropDownGalleryControl.GetResizing: TdxRibbonGalleryDropDownResizing;
begin
  if FIsResizingAssigned then
    Result := FResizing
  else
    Result := GalleryItem.GalleryOptions.SubmenuResizing;
end;

function TdxRibbonDropDownGalleryControl.HitTestToResizingState: TdxDropDownGalleryResizingState;
begin
  case FHitTest of
    HTTOP: Result := grsTop;
    HTTOPRIGHT, HTTOPLEFT: Result := grsTopRight;
    HTBOTTOM: Result := grsBottom;
    HTBOTTOMRIGHT, HTBOTTOMLEFT: Result := grsBottomRight;
  else
    Result := grsNone;
  end;
end;

function TdxRibbonDropDownGalleryControl.IsHitTestResizing: Boolean;
begin
  Result := FHitTest <> HTNOWHERE;
end;

procedure TdxRibbonDropDownGalleryControl.SetResizing(
  Value: TdxRibbonGalleryDropDownResizing);
begin
  FIsResizingAssigned := True;
  FResizing := Value;
end;

procedure TdxRibbonDropDownGalleryControl.StartResizing;
var
  R: TRect;
  P: TPoint;
begin
  R := WindowRect;
  P := GetMouseCursorPos;
  case FHitTest of
    HTTOP, HTTOPRIGHT, HTTOPLEFT:
        FMouseResizingDelta.Y := R.Top - P.Y;
    HTBOTTOM, HTBOTTOMRIGHT, HTBOTTOMLEFT:
        FMouseResizingDelta.Y := R.Bottom - P.Y;
  end;
  if FHitTest in [HTTOPLEFT, HTBOTTOMLEFT] then
    FMouseResizingDelta.X := R.Left - P.X
  else
    FMouseResizingDelta.X := R.Right - P.X;
  FResizingState := HitTestToResizingState;
  SetCapture(Handle);
end;

procedure TdxRibbonDropDownGalleryControl.StopResizing;
begin
  FResizingState := grsNone;
  ReleaseCapture;
end;

procedure TdxRibbonDropDownGalleryControl.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
var
  S: TSize;
begin
  with Message.MinMaxInfo^, Constraints do
  begin
    S := GetMinSize;
    with ptMinTrackSize do
    begin
      X := S.cx;
      Y := S.cy;
    end;
  end;
  inherited;
end;

procedure TdxRibbonDropDownGalleryControl.WMNCHitTest(var Message: TWMNCHitTest);
const
  ANCHitTestConsts: array[Boolean, gsrHeight..gsrWidthAndHeight] of Longint = (
    (HTTOP, HTTOPRIGHT),
    (HTBOTTOM, HTBOTTOMRIGHT)
  );
  ANCHitTestConstsRTL: array[Boolean, gsrHeight..gsrWidthAndHeight] of Longint = (
    (HTTOP, HTTOPLEFT),
    (HTBOTTOM, HTBOTTOMLEFT)
  );
begin
  inherited;
  if (Resizing <> gsrNone) and InternalPainter.PtInSizingArea(Self,
    cxPointOffset(SmallPointToPoint(Message.Pos), -Left, -Top)) then
    if UseRightToLeftAlignment then
      FHitTest := ANCHitTestConstsRTL[IsSizingBandAtBottom, Resizing]
    else
      FHitTest := ANCHitTestConsts[IsSizingBandAtBottom, Resizing]
  else
    FHitTest := HTNOWHERE;
  Message.result := HTCLIENT;
end;

procedure TdxRibbonDropDownGalleryControl.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if IsHitTestResizing then
    StartResizing
  else
    inherited;
end;

procedure TdxRibbonDropDownGalleryControl.WMLButtonUp(var Message: TWMLButtonUp);
begin
  if IsResizing then
    StopResizing
  else
    inherited;
end;

procedure TdxRibbonDropDownGalleryControl.WMSetCursor(var Message: TWMSetCursor);
begin
  case FHitTest of
    HTTOP, HTTOPRIGHT, HTBOTTOM, HTBOTTOMRIGHT, HTBOTTOMLEFT, HTTOPLEFT:
      Message.HitTest := FHitTest;
  end;
  inherited;
end;

{ TdxRibbonDropDownGalleryControlViewInfo }

procedure TdxRibbonDropDownGalleryControlViewInfo.Calculate;
var
  AGalleryItemControl: TdxRibbonGalleryControl;
begin
  inherited Calculate;
  AGalleryItemControl := BarControl.InternalGalleryItemControl;
  AddItemControlViewInfo(AGalleryItemControl.ViewInfo);
  IdxBarItemControlViewInfo(AGalleryItemControl.ViewInfo).SetBounds(BarControl.GetItemRect(AGalleryItemControl));
end;

function TdxRibbonDropDownGalleryControlViewInfo.GetBarControl: TdxRibbonDropDownGalleryControl;
begin
  Result := TdxRibbonDropDownGalleryControl(FBarControl);
end;

{ TdxRibbonGalleryGroupItemActionLink }

procedure TdxRibbonGalleryGroupItemActionLink.AssignClient(AClient: TObject);
begin
  FClient := AClient as TdxRibbonGalleryGroupItem;
end;

function TdxRibbonGalleryGroupItemActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and
    (FClient.Caption = (Action as TCustomAction).Caption);
end;

function TdxRibbonGalleryGroupItemActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and
    (FClient.Selected = (Action as TCustomAction).Checked);
end;

function TdxRibbonGalleryGroupItemActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and
    (FClient.Enabled = (Action as TCustomAction).Enabled);
end;

function TdxRibbonGalleryGroupItemActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked and
    (FClient.Hint = (Action as TCustomAction).Hint);
end;

function TdxRibbonGalleryGroupItemActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FClient.ImageIndex = (Action as TCustomAction).ImageIndex);
end;

function TdxRibbonGalleryGroupItemActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked and
    (@FClient.OnClick = @Action.OnExecute);
end;

procedure TdxRibbonGalleryGroupItemActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then
    FClient.Caption := Value;
end;

procedure TdxRibbonGalleryGroupItemActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
    FClient.Selected := Value;
end;

procedure TdxRibbonGalleryGroupItemActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then
    FClient.Enabled := Value;
end;

procedure TdxRibbonGalleryGroupItemActionLink.SetHint(const Value: string);
begin
  if IsHintLinked then
    FClient.Hint := Value;
end;

procedure TdxRibbonGalleryGroupItemActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
    FClient.ImageIndex := Value;
end;

procedure TdxRibbonGalleryGroupItemActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then
    FClient.OnClick := Value;
end;

initialization
  RegisterClasses([TdxRibbonDropDownGallery, TdxRibbonGalleryGroupItem, TdxRibbonGalleryGroup]);
  dxBarRegisterItem(TdxRibbonGalleryItem, TdxRibbonGalleryControl, True);

finalization
  dxBarUnregisterItem(TdxRibbonGalleryItem);

end.
