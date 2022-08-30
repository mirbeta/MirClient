{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSLVLnk;

interface

{$I cxVer.inc}

uses
{$IFDEF REGISTERSHELLCTRLS}
  ShellCtrls,
{$ENDIF}
  StdCtrls, Classes, Windows, Graphics, Controls, ExtCtrls, ImgList, Dialogs, dxCore, dxPSGlbl, dxPSCore, dxExtCtrls,
  dxPrnPg, dxPSBaseGridLnk, cxDrawTextUtils, cxControls, cxContainer, cxEdit, cxCheckBox, cxLabel, Menus, cxTextEdit,
  cxLookAndFeelPainters, cxButtons, cxPC, cxGraphics, cxMaskEdit, cxDropDownEdit, cxColorComboBox, cxLookAndFeels,
  cxImageComboBox, ComCtrls, CommCtrl, dxPSReportLinkDesignWindow, dxLayoutControlAdapters, dxLayoutLookAndFeels,
  dxLayoutContainer, cxClasses, dxLayoutControl, dxLayoutcxEditAdapters, cxImage, dxGDIPlusClasses;

const
  dxListViewAttributeIDBase = 0;
  dxListViewUnderfinedID = dxListViewAttributeIDBase + 0;
  dxListViewItemID = dxListViewAttributeIDBase + 1;
  dxListViewHeaderID = dxListViewAttributeIDBase + 2;

type
  TdxfmLVReportLinkDesignWindow = class;

  TListViewItemSize = record
    Width: Word;
    Height: Word;
  end;

  TdxListViewPaintOption = (lvpoBorder, lvpoHorzLines, lvpoVertLines);
  TdxListViewPaintOptions = set of TdxListViewPaintOption;

  TCustomdxNativeListViewReportLink = class(TAbstractdxGridReportLink, IdxPSNativeWin32ControlHandleSupport)
  private
    FInternalItems: TList;
    FLargeImages: TCustomImageList;
    FOptions: TdxListViewPaintOptions;
    FSmallImages: TCustomImageList;
    FStateImages: TCustomImageList;
    function GetHeaderColor: TColor;
    function GetHeaderFont: TFont;
    function GetHeaderTransparent: Boolean;
    function GetIncludeHeaders: Boolean;
    function GetImages: TCustomImageList;
    function GetInternalItem(Index: Integer): TAbstractdxReportCellData;
    function GetInternalItemCount: Integer;
    function GetOptions: TdxListViewPaintOptions;
    procedure SetHeaderColor(Value: TColor);
    procedure SetHeaderFont(Value: TFont);
    procedure SetHeaderTransparent(Value: Boolean);
    procedure SetIncludeHeaders(Value: Boolean);
    procedure SetOptions(Value: TdxListViewPaintOptions);
  protected
    FColumnCount: Integer;
    FColumnIndexes: TList;
    FColumnWidth: Integer;
    FHeaderHeight: Integer;
    FInternalItemsCreating: Boolean;
    FItemSpaceHorz: Integer;
    FItemSpaceVert: Integer;
    FRowCount: Integer;
    FRowHeight: Integer;

    procedure GetImageLists(AProc: TdxPSGetImageListProc); override;
    function GetRebuildOnPageParamsChange(AUpdateCodes: TdxPrinterPageUpdateCodes): Boolean; override;
    procedure InternalRestoreDefaults; override;
    procedure InternalRestoreFromOriginal; override;

    procedure AssignData(ACol, ARow: Integer; ADataItem: TAbstractdxReportCellData); override;
    function CanCalculateColumnAutoWidths: Boolean; override;
    function CreateDataItem(AParent: TdxReportCell; ACol, ARow: Integer;
      const ABounds: TRect): TAbstractdxReportCellData; override;
    function GetDataItemClass(ACol: Integer; ARow: Integer = 0): TdxReportCellDataClass; override;

    function GetActualColIndex(ACol: Integer): Integer; override;
    function GetCellColor(ACol, ARow: Integer): TColor; override;
    function GetCellFontIndex(ACol, ARow: Integer): Integer; override;
    function GetCellImageIndex(ACol, ARow: Integer): Integer; override;
    function GetCellImageLayout(ACol, ARow: Integer): TdxImageLayout; override;
    function GetCellImageList(ACol, ARow: Integer): TCustomImageList; override;
    function GetCellMultiline(ACol, ARow: Integer): Boolean; override;
    function GetCellSides(ACol, ARow: Integer): TdxCellSides; override;
    function GetCellText(ACol, ARow: Integer): string; override;
    function GetCellTextAlignX(ACol, ARow: Integer): TcxTextAlignX; override;
    function GetCellTextAlignY(ACol, ARow: Integer): TcxTextAlignY; override;
    function GetColCount: Integer; override;
    function GetColSortOrder(ACol: Integer): TdxCellSortOrder; override;
    function GetFixedRowCount: Integer; override;
    function GetRowCount: Integer; override;
    function GetSelectedColCount: Integer; override;
    function GetSelectedRowCount: Integer; override;
    function GetSourceColWidth(ACol: Integer): Integer; override;
    function GetSourceRowHeight(ARow: Integer): Integer; override;
    function HasColumnHeaderImage(ACol: Integer): Boolean; override;
    function HasSelection: Boolean; override;
    function HasSelectionInRow(ARow: Integer): Boolean; override;
    function IsDrawBorder: Boolean; override;
    function IsDrawFixedHorzLines: Boolean; override;
    function IsDrawFixedVertLines: Boolean; override;
    function IsDrawHorzLines: Boolean; override;
    function IsDrawVertLines: Boolean; override;
    function IsProcessedCol(ACol: Integer): Boolean; override;
    function IsSelectedCell(ACol, ARow: Integer): Boolean; override;
    function IsSelectedRow(ARow: Integer): Boolean; override;
    procedure SetDrawMode(Value: TdxGridDrawMode); override;

    procedure PrepareConstruct(AReportCells: TdxReportCells); override;
    procedure UnprepareConstruct(AReportCells: TdxReportCells); override;

   { IdxPSNativeWin32ControlHandleSupport }
    function GetNativeHandle: THandle;
    procedure SetNativeHandle(Value: THandle);

    function LV_AreCheckBoxes: Boolean;
    function LV_AreColumnHeadersClickable: Boolean;
    function LV_AreGridLines: Boolean;
    function LV_GetColumnCount: Integer;
    function LV_GetColumnHeaderImageIndex(ACol: Integer): Integer;
    function LV_GetColumnHeaderImageLayout(ACol: Integer): TdxImageLayout;
    function LV_GetColumnHeaderSortOrder(ACol: Integer): TdxCellSortOrder;
    function LV_GetColumnHeaderText(ACol: Integer): string;
    function LV_GetColumnHeaderTextAlignX(ACol: Integer): TcxTextAlignX;
    function LV_GetColumnWidth(ACol: Integer): Integer;
    function LV_GetHeaderHeight: Integer;
    function LV_GetHeaderWindow: THandle;
    function LV_GetIconArrangement: TIconArrangement;
    function LV_GetIsItemChecked(AIndex: Integer): Boolean;
    function LV_GetIsItemSelected(AIndex: Integer): Boolean;
    function LV_GetItemCount: Integer;
    function LV_GetItemSpacing: TListViewItemSize;
    function LV_GetItemImageIndex(ACol, ARow: Integer): Integer;
    function LV_GetItemStateIndex(ACol, ARow: Integer): Integer;
    function LV_GetItemText(ACol, ARow: Integer): string;
    function LV_GetRowHeight: Integer;
    function LV_GetSelectedCount: Integer;
    function LV_GetSelectedIndex: Integer;
    //function LV_GetViewStyle: TViewStyle;
    function LV_HasColumnHeaderImage(ACol: Integer): Boolean;
    function LV_HasColumnHeaders: Boolean;
    function LV_IsOwnerData: Boolean;

    function GetListViewHandle: THandle; virtual; abstract;
    function GetShowColumnHeaders: Boolean; virtual;
    function HasSupportForInvisibledColumnHeaders: Boolean; virtual;
    procedure SetListViewHandle(Value: THandle); virtual; abstract;
    procedure SetShowColumnHeaders(Value: Boolean); virtual;

    function CheckImages: Boolean; virtual;
    function CheckSmallImages: Boolean; virtual;
    function CheckStateImages: Boolean; virtual;
    procedure ClearImages; virtual;
    procedure CreateImages; virtual;
    procedure DeleteImages; virtual;
    function GetLargeImages: TCustomImageList; virtual;
    function GetSmallImages: TCustomImageList; virtual;
    function GetStateImages: TCustomImageList; virtual;
    function HasLargeImages: Boolean; virtual;
    function HasSmallImages: Boolean; virtual;
    function HasStateImages: Boolean; virtual;

    procedure AddExtraImage(AParent: TdxReportCell; ACol, ARow: Integer; R: TRect);
    function IsExtraImageRequired(ACol, ARow: Integer): Boolean; virtual;

    procedure CalculateColumnIndexes;
    function CalculateHeaderRowHeight: Integer;
    function CalculateRowHeight: Integer;

    function CanHasStateImage(ACol, ARow: Integer): Boolean;
    function IsItemChecked(ARow: Integer): Boolean; virtual;
    procedure LoadAllOwnerData; virtual;

    procedure CalculateInternalItemsViewInfo;
    procedure CreateExtraInternalItems(ATemporaryParent: TdxReportCell);
    procedure CreateInternalItems(ATemporaryParent: TdxReportCell);
    function GetFlatInternalItemIndex(ACol, ARow: Integer): Integer;
    procedure InitializeInternalItem(AnItem: TAbstractdxReportCellData; AnIndex: Integer); virtual;
    function PlaceInternalItem(AParent: TdxReportCell; ACol, ARow: Integer; R: TRect): TAbstractdxReportCellData; virtual;
    procedure SetupInternalItemsCellSides;

    function IsIconStyle: Boolean; virtual;
    function IsReportStyle: Boolean; virtual;

    property Images: TCustomImageList read GetImages;
    property InternalItemCount: integer read GetInternalItemCount;
    property InternalItems[Index: Integer]: TAbstractdxReportCellData read GetInternalItem;
    property LargeImages: TCustomImageList read GetLargeImages;
    property ListViewHandle: THandle read GetListViewHandle write SetListViewHandle;
    property Options: TdxListViewPaintOptions read GetOptions write SetOptions
      default [Low(TdxListViewPaintOption)..High(TdxListViewPaintOption)];
    property SmallImages: TCustomImageList read GetSmallImages;
    property ShowColumnHeaders: Boolean read GetShowColumnHeaders write SetShowColumnHeaders default True;
    property StateImages: TCustomImageList read GetStateImages;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    class function Aggregable: Boolean; override;
    procedure Initialize; override;

    function LV_GetViewStyle: TViewStyle;

    property AutoWidth;
    property Color;
    property DrawMode write SetDrawMode;
    property Effects3D;
    property EndEllipsis;
    property EvenColor;
    property EvenFont;
    property Font;
    property GridLineColor;
    property HeaderColor: TColor read GetHeaderColor write SetHeaderColor default clBtnFace; {dxDefaultFixedColor}
    property HeaderFont: TFont read GetHeaderFont write SetHeaderFont stored IsFixedFontStored;
    property HeadersOnEveryPage;
    property HeaderTransparent: Boolean read GetHeaderTransparent write SetHeaderTransparent default False;
    property IncludeHeaders: Boolean read GetIncludeHeaders write SetIncludeHeaders default True;
    property OddColor;
    property OddFont;
    property OnlySelected;
    property RowAutoHeight;
    property Soft3D;
    property Transparent;
  end;

  TdxNativeListViewReportLink = class(TCustomdxNativeListViewReportLink)
  private
    FListViewHandle: THandle;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GetDesignerClass: TdxReportLinkDesignWindowClass; override;

    function GetListViewHandle: THandle; override;
    procedure SetListViewHandle(Value: THandle); override;
  public
    function DataProviderPresent: Boolean; override;
    property ListViewHandle;
  published
    property AutoWidth;
    property Color;
    property DrawMode write SetDrawMode;
    property Effects3D;
    property EndEllipsis;
    property EvenColor;
    property EvenFont;
    property Font;
    property GridLineColor;
    property HeaderColor;
    property HeaderFont;
    property HeadersOnEveryPage;
    property HeaderTransparent;
    property OddColor;
    property OddFont;
    property OnlySelected;
    property Options;
    property RowAutoHeight;
    property IncludeHeaders;
    property Soft3D;
    property Transparent;
  end;

  TdxCustomDrawListViewHeaderEvent = procedure(Sender: TBasedxReportLink;
    ACanvas: TCanvas; ARect: TRect; AHeaderIndex: Integer;
    AnItem: TdxReportCellImage; var ADone: Boolean) of object;

  TdxCustomDrawListViewItemEvent = procedure(Sender: TBasedxReportLink;
    ACanvas: TCanvas; ARect: TRect; AListItem: TListItem; ASubItem: Integer;
    AnItem: TdxReportCellImage; var ADone: Boolean) of object;


  TdxListViewReportLinkCustomDrawInfo = record
    AttributeID: Integer;
    case Integer of
      dxListViewItemID: (ListItem: TListItem; SubItem: Integer);
      dxListViewHeaderID: (HeaderIndex: Integer);
  end;


  TCustomdxListViewReportLink = class(TCustomdxNativeListViewReportLink)
  private
    FShowColumnHeaders: Boolean;
    FOnCustomDrawHeader: TdxCustomDrawListViewHeaderEvent;
    FOnCustomDrawItem: TdxCustomDrawListViewItemEvent;
    FOnInitializeHeader: TdxCustomGridReportLinkInitializeItemEvent;
    procedure SetOnCustomDrawHeader(Value: TdxCustomDrawListViewHeaderEvent);
    procedure SetOnCustomDrawItem(Value: TdxCustomDrawListViewItemEvent);
  protected
    procedure AssignData(ACol, ARow: Integer; ADataItem: TAbstractdxReportCellData); override;
    procedure CustomDraw(AItem: TAbstractdxReportCellData; ACanvas: TCanvas;
      ABoundsRect, AClientRect: TRect; var ADone: Boolean); override;
    procedure DoInitializeHeader(ACol, ARow: Integer; ADataItem: TAbstractdxReportCellData); dynamic;
    procedure DoInitializeItem(ACol, ARow: Integer; ADataItem: TAbstractdxReportCellData); override;
    procedure InternalRestoreFromOriginal; override;
    procedure InternalRestoreDefaults; override;
    procedure PrepareConstruct(AReportCells: TdxReportCells); override;

    function GetListViewHandle: THandle; override;
    function GetShowColumnHeaders: Boolean; override;
    function HasSupportForInvisibledColumnHeaders: Boolean; override;
    procedure InitializeInternalItem(AnItem: TAbstractdxReportCellData; AnIndex: Integer); override;
    function IsItemChecked(ARow: Integer): Boolean; override;
    procedure LoadAllOwnerData; override;

    procedure SetListViewHandle(Value: THandle); override;
    procedure SetShowColumnHeaders(Value: Boolean); override;

    function GetLargeImages: TCustomImageList; override;
    function GetSmallImages: TCustomImageList; override;
    function GetStateImages: TCustomImageList; override;

    function GetCustomListView: TCustomListView; virtual;

    procedure DoCustomDrawHeader(ACanvas: TCanvas; ARect: TRect; AHeaderIndex: Integer;
      AnItem: TdxReportCellImage; var ADone: Boolean); dynamic;
    procedure DoCustomDrawItem(ACanvas: TCanvas; ARect: TRect; AListItem: TListItem;
      ASubItem: Integer; AnItem: TdxReportCellImage; var ADone: Boolean); dynamic;
    procedure GetCustomDrawInfo(AnItem: TAbstractdxReportCellData;
      var ACustomDrawInfo: TdxListViewReportLinkCustomDrawInfo); virtual;

    property CustomListView: TCustomListView read GetCustomListView;
  public
    procedure Assign(Source: TPersistent); override;

    class function Aggregable: Boolean; override;

    property ShowColumnHeaders;
    property SupportedCustomDraw;
    property OnCustomDrawHeader: TdxCustomDrawListViewHeaderEvent read FOnCustomDrawHeader write SetOnCustomDrawHeader;
    property OnCustomDrawItem: TdxCustomDrawListViewItemEvent read FOnCustomDrawItem write SetOnCustomDrawItem;
    property OnInitializeHeader: TdxCustomGridReportLinkInitializeItemEvent read FOnInitializeHeader write FOnInitializeHeader;
  end;

  TdxListViewReportLink = class(TCustomdxListViewReportLink)
  private
    function GetListView: TListView;
  public
    property ListView: TListView read GetListView;
  published
    property AutoWidth;
    property Color;
    property DrawMode write SetDrawMode;
    property Effects3D;
    property EndEllipsis;
    property EvenColor;
    property EvenFont;
    property Font;
    property GridLineColor;
    property HeaderColor;
    property HeaderFont;
    property HeadersOnEveryPage;
    property HeaderTransparent;
    property OnlySelected;
    property Options;
    property RowAutoHeight;
    property IncludeHeaders;
    property ShowColumnHeaders;
    property Soft3D;
    property SupportedCustomDraw;
    property Transparent;

    property OnCustomDrawHeader;
    property OnCustomDrawItem;
    property OnInitializeHeader;
    property OnInitializeItem;
  end;

 {$IFDEF REGISTERSHELLCTRLS}
  TCustomdxShellListViewReportLink = class(TCustomdxListViewReportLink)
  private
    function GetShellListView: TShellListView;
  protected
    function HasStateImages: Boolean; override;
    procedure CreateImages; override;
    procedure DeleteImages; override;
    function GetLargeImages: TCustomImageList; override;
    function GetSmallImages: TCustomImageList; override;
    function GetStateImages: TCustomImageList; override;

    property ShellListView: TShellListView read GetShellListView;
  end;

  TdxShellListViewReportLink = class(TCustomdxShellListViewReportLink)
  public
    property ShellListView;
  published
    property AutoWidth;
    property Color;
    property DrawMode write SetDrawMode;
    property Effects3D;
    property EndEllipsis;
    property EvenColor;
    property EvenFont;
    property Font;
    property GridLineColor;
    property HeaderColor;
    property HeaderFont;
    property HeadersOnEveryPage;
    property HeaderTransparent;
    property OnlySelected;
    property Options;
    property RowAutoHeight;
    property IncludeHeaders;
    property ShowColumnHeaders;
    property Soft3D;
    property SupportedCustomDraw;
    property Transparent;

    property OnCustomDrawHeader;
    property OnCustomDrawItem;
    property OnInitializeHeader;
    property OnInitializeItem;
  end;

 {$ENDIF}

  TdxfmLVReportLinkDesignWindow = class(TStandarddxReportLinkDesignWindow)
    btnEvenFont: TcxButton;
    btnFont: TcxButton;
    btnHeadersFont: TcxButton;
    cbxDrawMode: TcxImageComboBox;
    ccbxColor: TcxColorComboBox;
    ccbxEvenColor: TcxColorComboBox;
    ccbxGridLineColor: TcxColorComboBox;
    ccbxHeadersColor: TcxColorComboBox;
    chbxAutoWidth: TcxCheckBox;
    chbxHeadersOnEveryPage: TcxCheckBox;
    chbxIncludeFixed: TcxCheckBox;
    chbxOnlySelected: TcxCheckBox;
    chbxRowAutoHeight: TcxCheckBox;
    chbxShowBorders: TcxCheckBox;
    chbxShowColumnHeaders: TcxCheckBox;
    chbxShowHorzLines: TcxCheckBox;
    chbxShowVertLines: TcxCheckBox;
    chbxTransparent: TcxCheckBox;
    chbxTransparentHeaders: TcxCheckBox;
    chbxUse3DEffects: TcxCheckBox;
    chbxUseSoft3D: TcxCheckBox;
    dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup11: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup12: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup13: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem13: TdxLayoutItem;
    dxLayoutItem14: TdxLayoutItem;
    dxLayoutItem15: TdxLayoutItem;
    dxLayoutItem16: TdxLayoutItem;
    dxLayoutItem17: TdxLayoutItem;
    dxLayoutItem18: TdxLayoutItem;
    dxLayoutItem19: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem20: TdxLayoutItem;
    dxLayoutItem21: TdxLayoutItem;
    dxLayoutItem22: TdxLayoutItem;
    dxLayoutItem23: TdxLayoutItem;
    dxLayoutItem24: TdxLayoutItem;
    dxLayoutItem25: TdxLayoutItem;
    dxLayoutItem26: TdxLayoutItem;
    dxLayoutItem27: TdxLayoutItem;
    dxLayoutItem28: TdxLayoutItem;
    dxLayoutItem29: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem30: TdxLayoutItem;
    dxLayoutItem31: TdxLayoutItem;
    dxLayoutItem32: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    dxLayoutSeparatorItem2: TdxLayoutSeparatorItem;
    edEvenFont: TcxTextEdit;
    edFixedFont: TcxTextEdit;
    edFont: TcxTextEdit;
    Image1: TcxImage;
    Image2: TcxImage;
    Image3: TcxImage;
    Image4: TcxImage;
    imgGrid: TcxImage;
    imgHint: TcxImage;
    lblColor: TdxLayoutItem;
    lblDrawMode: TdxLayoutItem;
    lblEvenColor: TdxLayoutItem;
    lblGridLinesColor: TdxLayoutItem;
    lblHeadersColor: TdxLayoutItem;
    lblHint: TcxLabel;
    lblLookAndFeel: TcxLabel;
    lblMiscellaneous: TcxLabel;
    lblOnEveryPage: TcxLabel;
    lblPreview: TdxLayoutItem;
    lblSelection: TcxLabel;
    lblShow: TcxLabel;
    lichbxShowColumnHeaders: TdxLayoutItem;
    pcMain: TdxLayoutGroup;
    pnlHint: TPanel;
    pnlPreview: TPanel;
    stTransparent: TcxLabel;
    stTransparentHeaders: TcxLabel;
    tshBehaviors: TdxLayoutGroup;
    tshColor: TdxLayoutGroup;
    tshFont: TdxLayoutGroup;
    tshOptions: TdxLayoutGroup;

    procedure btnFontClick(Sender: TObject);
    procedure cbxDrawModeClick(Sender: TObject);
    procedure ccbxColorChange(Sender: TObject);
    procedure chbxAutoWidthClick(Sender: TObject);
    procedure chbxHeadersOnEveryPageClick(Sender: TObject);
    procedure chbxIncludeFixedClick(Sender: TObject);
    procedure chbxOnlySelectedClick(Sender: TObject);
    procedure chbxRowAutoHeightClick(Sender: TObject);
    procedure chbxShowBordersClick(Sender: TObject);
    procedure chbxShowColumnHeadersClick(Sender: TObject);
    procedure chbxTransparentClick(Sender: TObject);
    procedure chbxUse3DEffectsClick(Sender: TObject);
    procedure chbxUseSoft3DClick(Sender: TObject);
    procedure pbxPreviewPaint(Sender: TObject);
    procedure stTransparentClick(Sender: TObject);
    procedure stTransparentHeadersClick(Sender: TObject);
  private
    FPreviewBox: TdxPSPaintPanel;
    function GetReportLink: TCustomdxNativeListViewReportLink;

    procedure CreateControls;
    procedure CMDialogChar(var message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure DoInitialize; override;
    function GetPreviewHost: TCustomPanel; override;
    procedure LoadGroupsIcons; override;
    procedure LoadStrings; override;
    procedure PaintPreview(ACanvas: TCanvas; R: TRect); override;
    procedure UpdateControlsState; override;
    procedure UpdatePreview; override;
  public
    constructor Create(AOwner: TComponent); override;
    property ReportLink: TCustomdxNativeListViewReportLink read GetReportLink;
  end;

const
  dxDefaultListViewOptions = [Low(TdxListViewPaintOption)..High(TdxListViewPaintOption)];

implementation

{$R *.DFM}

// TODO: Tile and Group View (Windows XP)

uses
  Messages, Forms, SysUtils, Math, dxPSRes, dxPSUtl, dxPrnDev, dxPSImgs;

const
  LVM_GETVIEW = LVM_FIRST + 143;

  LV_VIEW_ICON       = $00000000;
  LV_VIEW_DETAILS    = $00000001;
  LV_VIEW_SMALLICON  = $00000002;
  LV_VIEW_LIST       = $00000003;
  LV_VIEW_TILE       = $00000004;
  LV_VIEW_MAX        = $00000004;

type
  TCustomListViewAccess = class(TListView);


{ ListView Helpers }

function ListViewGetItems(AControl: TCustomListView): TListItems;
begin
  Result := TCustomListViewAccess(AControl).Items;
end;

function ListViewGetLargeImages(AControl: TCustomListView): TCustomImageList;
begin
  Result := TCustomListViewAccess(AControl).LargeImages;
end;

function ListViewGetSmallImages(AControl: TCustomListView): TCustomImageList;
begin
  Result := TCustomListViewAccess(AControl).SmallImages;
end;

function ListViewGetShowColumnHeaders(AControl: TCustomListView): Boolean;
begin
  Result := TCustomListViewAccess(AControl).ShowColumnHeaders;
end;

function ListViewGetStateImages(AControl: TCustomListView): TCustomImageList;
begin
  Result := TCustomListViewAccess(AControl).StateImages;
end;

procedure ListViewOwnerDataFetch(AControl: TCustomListView; AnIndex: Integer; ARequest: TItemRequest);
begin
  with TCustomListViewAccess(AControl) do
    OwnerDataFetch(Items[AnIndex], ARequest);
end;

{ Utilities }
function ListView_GetView(AWnd: HWND): DWORD;
begin
  Result := SendMessage(AWnd, LVM_GETVIEW, 0, 0);
end;

{ TCustomdxNativeListViewReportLink }

constructor TCustomdxNativeListViewReportLink.Create(AOwner: TComponent);
begin
  inherited;
  CreateImages;
  FColumnIndexes := TList.Create;
end;

destructor TCustomdxNativeListViewReportLink.Destroy;
begin
  DeleteImages;
  FreeAndNil(FColumnIndexes);
  inherited;
end;

procedure TCustomdxNativeListViewReportLink.Assign(Source: TPersistent);
begin
  if Source is TCustomdxNativeListViewReportLink then
    with TCustomdxNativeListViewReportLink(Source) do
    begin
      Self.Options := Options;
    end;
  inherited;
end;

class function TCustomdxNativeListViewReportLink.Aggregable: Boolean;
begin
  Result := False;
end;

procedure TCustomdxNativeListViewReportLink.Initialize;
begin
  inherited;
  FColumnCount := 0;
  FRowCount := 0;
  FreeAndNil(FInternalItems);
end;

procedure TCustomdxNativeListViewReportLink.GetImageLists(AProc: TdxPSGetImageListProc);
begin
  inherited;
  if HasLargeImages then AProc(LargeImages);
  if HasSmallImages then AProc(SmallImages);
  if HasStateImages then AProc(StateImages);
end;

function TCustomdxNativeListViewReportLink.GetRebuildOnPageParamsChange(AUpdateCodes: TdxPrinterPageUpdateCodes): Boolean;
begin
  case LV_GetViewStyle of
    vsIcon,
    vsSmallIcon:
      if LV_GetIconArrangement = iaTop then
        Result := AUpdateCodes * uaMarginsVert <> []
      else
        Result := AUpdateCodes * uaMarginsHorz <> [];
    vsList:
      Result := AUpdateCodes * uaMarginsHorz <> [];
  else // vsReport
    Result := inherited GetRebuildOnPageParamsChange(AUpdateCodes);
  end;
end;

procedure TCustomdxNativeListViewReportLink.InternalRestoreDefaults;
begin
  inherited;
  Options := dxDefaultListViewOptions;
end;

procedure TCustomdxNativeListViewReportLink.InternalRestoreFromOriginal;
begin
  inherited;
  HeaderFont := Font;
  if IsWindow(ListViewHandle) and IsReportStyle then
  begin
    Options := [lvpoBorder];
    if LV_AreGridLines then
      Options := Options + [lvpoHorzLines, lvpoVertLines];
    Effects3D := LV_AreColumnHeadersClickable;
  end
  else
  begin
    Effects3D := False;
    Options := [];
  end;
end;

procedure TCustomdxNativeListViewReportLink.AssignData(ACol, ARow: Integer;
  ADataItem: TAbstractdxReportCellData);
begin
  if IsReportStyle or (GetFlatInternalItemIndex(ACol, ARow) < LV_GetItemCount) then
  begin
    inherited;
    with TdxReportCellImage(ADataItem) do
      MakeSpaceForEmptyImage := ACol = 0; // TODO: Check HasImageList
  end;
end;

function TCustomdxNativeListViewReportLink.CanCalculateColumnAutoWidths: Boolean;
begin
  Result := IsReportStyle;
end;

function TCustomdxNativeListViewReportLink.CreateDataItem(AParent: TdxReportCell;
  ACol, ARow: Integer; const ABounds: TRect): TAbstractdxReportCellData;
var
  ImageWidth: Integer;
  R: TRect;
begin
  R := ABounds;
  if IsReportStyle or FInternalItemsCreating then
  begin
    ImageWidth := 0;
    if IsExtraImageRequired(ACol, ARow) then
    begin
      ImageWidth := StateImages.Width;
      Inc(R.Left, ImageWidth);
    end;
    Result := inherited CreateDataItem(AParent, ACol, ARow, R);
    if IsExtraImageRequired(ACol, ARow) then
      AddExtraImage(AParent, ACol, ARow, Rect(0, R.Top, ImageWidth, R.Bottom));
  end
  else
    Result := PlaceInternalItem(AParent, ACol, ARow, R);
end;

function TCustomdxNativeListViewReportLink.GetDataItemClass(ACol: Integer; ARow: Integer = 0): TdxReportCellDataClass;
begin
  Result := TdxReportCellImage;
end;

function TCustomdxNativeListViewReportLink.GetActualColIndex(ACol: Integer): Integer;
begin
  if IsReportStyle then
    Result := Integer(FColumnIndexes[ACol])
  else
    Result := ACol;
end;

function TCustomdxNativeListViewReportLink.GetCellColor(ACol, ARow: Integer): TColor;
begin
  if IsReportStyle then
    Result := inherited GetCellColor(ACol, ARow)
  else
    Result := Color;
end;

function TCustomdxNativeListViewReportLink.GetCellFontIndex(ACol, ARow: Integer): Integer;
begin
  if IsReportStyle then
    Result := inherited GetCellFontIndex(ACol, ARow)
  else
    Result := FFontIndex;
end;

function TCustomdxNativeListViewReportLink.GetCellImageIndex(ACol, ARow: Integer): Integer;
begin
  if IsReportStyle then
    if IsFixedRow(ARow) then
      if LV_HasColumnHeaderImage(ACol) then
        Result := LV_GetColumnHeaderImageIndex(ACol)
      else
        Result := -1
    else
      Result := LV_GetItemImageIndex(ACol, ARow - Ord(ShowColumnHeaders))
  else
    Result := LV_GetItemImageIndex(ACol, ARow);
end;

function TCustomdxNativeListViewReportLink.GetCellImageLayout(ACol, ARow: Integer): TdxImageLayout;
begin
  if IsReportStyle then
    if IsFixedRow(ARow) then
      Result := LV_GetColumnHeaderImageLayout(ACol)
    else
      Result := ilImageCenterLeft
  else
    if IsIconStyle then
      Result := ilImageTopCenter
    else
      Result := ilImageCenterLeft;
end;

function TCustomdxNativeListViewReportLink.GetCellImageList(ACol, ARow: Integer): TCustomImageList;
begin
  if IsReportStyle then
    if IsFixedRow(ARow) then
      if LV_HasColumnHeaderImage(ACol) then
        Result := LargeImages
      else
        Result := nil
    else
      Result := Images
  else
    Result := Images;
end;

function TCustomdxNativeListViewReportLink.GetCellMultiline(ACol, ARow: Integer): Boolean;
begin
  Result := IsIconStyle or inherited GetCellMultiline(ACol, ARow)
end;

function TCustomdxNativeListViewReportLink.GetCellSides(ACol, ARow: Integer): TdxCellSides;

  function IsCellMostLeft(ACol, ARow: Integer): Boolean;
  begin
    Result := ACol = 0;
  end;

  function IsCellMostTop(ACol, ARow: Integer): Boolean;
  begin
    Result := (IsReportStyle and LV_HasColumnHeaders and (ARow < 2)) or (ARow = 0);
  end;

  function IsCellMostRight(ACol, ARow: Integer): Boolean;
  begin
    Result := ACol = ColCount - 1;
  end;

  function IsCellMostBottom(ACol, ARow: Integer): Boolean;
  begin
    Result := ARow = RowCount - 1;
  end;

begin
  Result := csAll;
  if not IsFixedCell(ACol, ARow) then
  begin
    if not IsDrawBorder then
    begin
      if IsCellMostLeft(ACol, ARow) then Exclude(Result, csLeft);
      if IsCellMostTop(ACol, ARow) then Exclude(Result, csTop);
      if IsCellMostRight(ACol, ARow) then Exclude(Result, csRight);
      if IsCellMostBottom(ACol, ARow) then Exclude(Result, csBottom);
    end;
    if not IsDrawHorzLines then
    begin
      if not IsCellMostTop(ACol, ARow) then Exclude(Result, csTop);
      if not IsCellMostBottom(ACol, ARow) then Exclude(Result, csBottom);
    end;
    if not IsDrawVertLines then
    begin
      if not IsCellMostLeft(ACol, ARow) then Exclude(Result, csLeft);
      if not IsCellMostRight(ACol, ARow) then Exclude(Result, csRight);
    end;

    if IsExtraImageRequired(ACol, ARow) then Exclude(Result, csLeft);
  end;
end;

function TCustomdxNativeListViewReportLink.GetCellText(ACol, ARow: Integer): string;
begin
  if IsReportStyle then
    if IsFixedRow(ARow) then
      Result := LV_GetColumnHeaderText(ACol)
    else
      Result := LV_GetItemText(ACol, ARow - Ord(ShowColumnHeaders))
  else
    Result := LV_GetItemText(ACol, ARow);
end;

function TCustomdxNativeListViewReportLink.GetCellTextAlignX(ACol, ARow: Integer): TcxTextAlignX;
begin
  case LV_GetViewStyle of
    vsIcon:
      Result := taCenterX;
    vsSmallIcon,
    vsList:
      Result := taLeft;
  else // vsReport:
    Result := LV_GetColumnHeaderTextAlignX(ACol)
  end;
end;

function TCustomdxNativeListViewReportLink.GetCellTextAlignY(ACol, ARow: Integer): TcxTextAlignY;
begin
  if IsIconStyle then
    Result := taTop
  else
    Result := inherited GetCellTextAlignY(ACol, ARow);
end;

function TCustomdxNativeListViewReportLink.GetColCount: Integer;
begin
  if IsReportStyle then
    Result := LV_GetColumnCount
  else
    Result := FColumnCount;
end;

function TCustomdxNativeListViewReportLink.GetColSortOrder(ACol: Integer): TdxCellSortOrder;
begin
  if IsReportStyle and ShowColumnHeaders then
    Result := LV_GetColumnHeaderSortOrder(ACol)
  else
    Result := csoNone;
end;

function TCustomdxNativeListViewReportLink.GetFixedRowCount: Integer;
begin
  Result := Ord(IsReportStyle and ShowColumnHeaders and (not HasSelection or IncludeHeaders));
end;

function TCustomdxNativeListViewReportLink.GetRowCount: Integer;
begin
  if IsReportStyle then
    Result := LV_GetItemCount + Ord(ShowColumnHeaders)
  else
    Result := FRowCount;
end;

function TCustomdxNativeListViewReportLink.GetSelectedColCount: Integer;
begin
  Result := LV_GetColumnCount;
end;

function TCustomdxNativeListViewReportLink.GetSelectedRowCount: Integer;
begin
  Result := LV_GetSelectedCount;
end;

function TCustomdxNativeListViewReportLink.GetSourceColWidth(ACol: Integer): Integer;
begin
  if IsReportStyle then
    Result := LV_GetColumnWidth(ACol)
  else
    Result := FColumnWidth;
end;

function TCustomdxNativeListViewReportLink.GetSourceRowHeight(ARow: Integer): Integer;
begin
  if IsFixedRow(ARow) then
    Result := FHeaderHeight
  else
    Result := FRowHeight;
end;

function TCustomdxNativeListViewReportLink.HasColumnHeaderImage(ACol: Integer): Boolean;
begin
  Result := LV_HasColumnHeaderImage(ACol);
end;

function TCustomdxNativeListViewReportLink.HasSelection: Boolean;
begin
  Result := OnlySelected and IsReportStyle and (LV_GetSelectedCount <> 0) and (LV_GetSelectedIndex <> -1);
end;

function TCustomdxNativeListViewReportLink.HasSelectionInRow(ARow: Integer): Boolean;
begin
  Result := IsSelectedRow(ARow);
end;

function TCustomdxNativeListViewReportLink.IsDrawBorder: Boolean;
begin
  Result := lvpoBorder in Options;
end;

function TCustomdxNativeListViewReportLink.IsDrawHorzLines: Boolean;
begin
  Result := lvpoHorzLines in Options;
end;

function TCustomdxNativeListViewReportLink.IsDrawVertLines: Boolean;
begin
  Result := lvpoVertLines in Options;
end;

function TCustomdxNativeListViewReportLink.IsDrawFixedHorzLines: Boolean;
begin
  Result := lvpoBorder in Options;
end;

function TCustomdxNativeListViewReportLink.IsDrawFixedVertLines: Boolean;
begin
  Result := lvpoBorder in Options;
end;

function TCustomdxNativeListViewReportLink.IsProcessedCol(ACol: Integer): Boolean;
begin
  Result := True;
end;

function TCustomdxNativeListViewReportLink.IsSelectedCell(ACol, ARow: Integer): Boolean;
begin
  Result := IsSelectedRow(ARow);
end;

function TCustomdxNativeListViewReportLink.IsSelectedRow(ARow: Integer): Boolean;
begin
  if IsFixedRow(ARow) then
    Result := IncludeHeaders
  else
    Result := HasSelection and LV_GetIsItemSelected(ARow - Ord(ShowColumnHeaders));
end;

procedure TCustomdxNativeListViewReportLink.SetDrawMode(Value: TdxGridDrawMode);
begin
  if Value = gdmBorrowSource then Value := gdmStrict;
  inherited;
end;

procedure TCustomdxNativeListViewReportLink.PrepareConstruct(AReportCells: TdxReportCells);
begin
  ClearImages;

  EndEllipsis := IsReportStyle or EndEllipsis;

  if LV_IsOwnerData then LoadAllOwnerData;

  FRowHeight := CalculateRowHeight;
  FHeaderHeight := CalculateHeaderRowHeight;

  if not IsReportStyle then
  begin
    FInternalItems := TList.Create;
    if LV_GetItemCount <> 0 then
    begin
      CreateInternalItems(AReportCells.Cells);
      if InternalItemCount <> 0 then
      begin
        CalculateInternalItemsViewInfo;
        CreateExtraInternalItems(AReportCells.Cells);
        SetupInternalItemsCellSides;
      end;
    end;
  end
  else
    CalculateColumnIndexes;

  inherited;
end;

procedure TCustomdxNativeListViewReportLink.UnprepareConstruct(AReportCells: TdxReportCells);
begin
  FColumnCount := 0;
  FRowCount := 0;
  FreeAndNil(FInternalItems);
  inherited;
end;

{ IdxPSNativeWin32ControlHandleSupport }

function TCustomdxNativeListViewReportLink.GetNativeHandle: THandle;
begin
  Result := GetListViewHandle;
end;

procedure TCustomdxNativeListViewReportLink.SetNativeHandle(Value: THandle);
begin
  SetListViewHandle(Value);
end;

function TCustomdxNativeListViewReportLink.LV_AreCheckBoxes: Boolean;
begin
  Result := ListView_GetExtendedListViewStyle(ListViewHandle) and LVS_EX_CHECKBOXES = LVS_EX_CHECKBOXES;
end;

function TCustomdxNativeListViewReportLink.LV_AreColumnHeadersClickable: Boolean;
begin
  Result := GetWindowLong(ListViewHandle, GWL_STYLE) and LVS_NOSORTHEADER <> LVS_NOSORTHEADER;
end;

function TCustomdxNativeListViewReportLink.LV_AreGridLines: Boolean;
begin
  Result := ListView_GetExtendedListViewStyle(ListViewHandle) and LVS_EX_GRIDLINES = LVS_EX_GRIDLINES;
end;

function TCustomdxNativeListViewReportLink.LV_GetColumnCount: Integer;
var
  Header: HWND;
begin
  Header := ListView_GetHeader(ListViewHandle);
  if Header <> 0 then
    Result := Header_GetItemCount(Header)
  else
    Result := 0;
end;

function TCustomdxNativeListViewReportLink.LV_GetColumnHeaderImageIndex(ACol: Integer): Integer;
var
  Column: TLVColumn;
begin
  FillChar(Column, SizeOf(Column), 0);
  Column.Mask := LVCF_FMT or LVCF_IMAGE;
  ListView_GetColumn(ListViewHandle, ACol, Column);
  if Column.fmt and LVCFMT_COL_HAS_IMAGES = LVCFMT_COL_HAS_IMAGES then
    Result := Column.iImage
  else
    Result := -1;
end;

function TCustomdxNativeListViewReportLink.LV_GetColumnHeaderImageLayout(ACol: Integer): TdxImageLayout;
const
  ImageLayoutsMap: array[Boolean] of TdxImageLayout = (ilImageCenterLeft, ilImageCenterRight);
var
  Column: TLVColumn;
begin
  FillChar(Column, SizeOf(Column), 0);
  Column.Mask := LVCF_FMT;
  ListView_GetColumn(ListViewHandle, ACol, Column);
  Result := ImageLayoutsMap[Column.fmt and LVCFMT_BITMAP_ON_RIGHT = LVCFMT_BITMAP_ON_RIGHT];
end;

function TCustomdxNativeListViewReportLink.LV_GetColumnHeaderSortOrder(ACol: Integer): TdxCellSortOrder;
var
  Header: HWND;
  Item: THDItem;
begin
  Result := csoNone;
  if IsWinXP or dxPSGlbl.IsComCtrlVersion600 then
  begin
    Header := LV_GetHeaderWindow;
    if IsWindow(Header) then
    begin
      FillChar(Item, SizeOf(Item), 0);
      Item.Mask := HDI_FORMAT;
      Header_GetItem(Header, ACol, Item);
      if Item.fmt and HDF_SORTDOWN = HDF_SORTDOWN then
        Result := csoDown
      else
        if Item.fmt and HDF_SORTUP = HDF_SORTUP then
          Result := csoUp
    end;
  end;
end;

function TCustomdxNativeListViewReportLink.LV_GetColumnHeaderText(ACol: Integer): string;
const
  BufferLength = 4096;
var
  Column: TLVColumn;
  Buffer: array[0..BufferLength - 1] of Char;
  PBuffer:  Pointer ;
begin
  FillChar(Column, SizeOf(Column), 0);
  PBuffer := @Buffer;
  Column.Mask := LVCF_TEXT;
  Column.pszText := PBuffer;
  Column.cchTextMax := SizeOf(Buffer);
  ListView_GetColumn(ListViewHandle, ACol, Column);
  Result := Column.pszText;
end;

function TCustomdxNativeListViewReportLink.LV_GetColumnHeaderTextAlignX(ACol: Integer): TcxTextAlignX;
var
  Column: TLVColumn;
begin
  FillChar(Column, SizeOf(Column), 0);
  Column.Mask := LVCF_FMT;
  ListView_GetColumn(ListViewHandle, ACol, Column);
  Column.fmt := Column.fmt and LVCFMT_JUSTIFYMASK;

  if Column.fmt and LVCFMT_CENTER = LVCFMT_CENTER then
    Result := taCenterX
  else
    if Column.fmt and LVCFMT_RIGHT = LVCFMT_RIGHT then
      Result := taRight
    else
      Result := taLeft;
end;

function TCustomdxNativeListViewReportLink.LV_GetColumnWidth(ACol: Integer): Integer;
begin
  Result := ListView_GetColumnWidth(ListViewHandle, ACol);
end;

function TCustomdxNativeListViewReportLink.LV_GetHeaderHeight: Integer;
var
  Header: HWND;
  R: TRect;
begin
  Header := LV_GetHeaderWindow;
  if IsWindow(Header) then
  begin
    GetWindowRect(Header, R);
    Result := R.Bottom - R.Top;
  end
  else
    Result := 0;
end;

function TCustomdxNativeListViewReportLink.LV_GetHeaderWindow: THandle;
begin
  Result := ListView_GetHeader(ListViewHandle);
end;

function TCustomdxNativeListViewReportLink.LV_GetIconArrangement: TIconArrangement;
const
  IconArrangements: array[Boolean] of TIconArrangement = (iaLeft, iaTop);
begin
  Result := IconArrangements[(GetWindowLong(ListViewHandle, GWL_STYLE) and LVS_ALIGNMASK) = 0];
end;

function TCustomdxNativeListViewReportLink.LV_GetIsItemChecked(AIndex: Integer): Boolean;
begin
  Result := ListView_GetCheckState(ListViewHandle, AIndex) <> 0;
end;

function TCustomdxNativeListViewReportLink.LV_GetIsItemSelected(AIndex: Integer): Boolean;
begin
  Result := ListView_GetItemState(ListViewHandle, AIndex, LVIS_SELECTED) <> 0;
end;

function TCustomdxNativeListViewReportLink.LV_GetItemCount: Integer;
begin
  Result := ListView_GetItemCount(ListViewHandle);
end;

function TCustomdxNativeListViewReportLink.LV_GetItemImageIndex(ACol, ARow: Integer): Integer;

  procedure InternalGetImageIndex(var AnImageIndex: Integer);
  var
    Item: TLVItem;
  begin
    FillChar(Item, SizeOf(Item), 0);
    Item.Mask := LVIF_IMAGE;
    Item.iImage := AnImageIndex;
    Item.iItem := ARow;
    Item.iSubItem := ACol;
    if ListView_GetItem(ListViewHandle, Item) then
      AnImageIndex := Item.iImage
    else
      AnImageIndex := -1;
  end;

begin
  if (ACol = 0) or not LV_AreCheckBoxes then
  begin
    Result := I_IMAGECALLBACK;
    InternalGetImageIndex(Result);
    {if Result = -1 then
    begin
      Result := 0;
      InternalGetImageIndex(Result);
    end; }
  end
  else
    Result := -1;
end;

function TCustomdxNativeListViewReportLink.LV_GetItemSpacing: TListViewItemSize;
const
  Params: array[TViewStyle] of Integer = (0, 1, 1, 1);
var
  Spacing: Longint;
begin
  Spacing := ListView_GetItemSpacing(ListViewHandle, Params[LV_GetViewStyle]);
  Result.Width := LoWord(Spacing);
  Result.Height := HiWord(Spacing);
end;

function TCustomdxNativeListViewReportLink.LV_GetItemStateIndex(ACol, ARow: Integer): Integer;
var
  Item: TLVItem;
begin
  if CanHasStateImage(ACol, ARow) then
  begin
    FillChar(Item, SizeOf(Item), 0);
    Item.Mask := LVIF_IMAGE;
    Item.StateMask := LVIS_STATEIMAGEMASK;
    Item.iItem := ARow - Ord(IsReportStyle and ShowColumnHeaders);
    Item.iSubItem := ACol;
    ListView_GetItem(ListViewHandle, Item);
    Result := (Item.State shr 12) - 1;
  end
  else
    Result := -1;
end;

function TCustomdxNativeListViewReportLink.LV_GetItemText(ACol, ARow: Integer): string;
const
  BufferLength = 4096;
var
  Buffer: array[0..BufferLength - 1] of Char;
begin
  ListView_GetItemText(ListViewHandle, ARow, ACol, Buffer, BufferLength);
  Result := Buffer;
end;

function TCustomdxNativeListViewReportLink.LV_GetRowHeight: Integer;
begin
  Result := HiWord(ListView_GetItemSpacing(ListViewHandle, 1));
end;

function TCustomdxNativeListViewReportLink.LV_GetSelectedCount: Integer;
begin
  Result := ListView_GetSelectedCount(ListViewHandle);
end;

function TCustomdxNativeListViewReportLink.LV_GetSelectedIndex: Integer;
begin
  Result := ListView_GetNextItem(ListViewHandle, -1, LVNI_ALL or LVNI_SELECTED);
end;

function TCustomdxNativeListViewReportLink.LV_GetViewStyle: TViewStyle;
const
  ViewStylesMap: array[LVS_ICON..LVS_LIST] of TViewStyle = (vsIcon, vsReport, vsSmallIcon, vsList);
var
  ViewStyle: DWORD;
begin
  if dxPSGlbl.IsComCtrlVersion600 then
    ViewStyle := ListView_GetView(ListViewHandle)
  else
    ViewStyle := GetWindowLong(ListViewHandle, GWL_STYLE);
  Result := ViewStylesMap[ViewStyle and LVS_TYPEMASK];
end;

function TCustomdxNativeListViewReportLink.LV_HasColumnHeaderImage(ACol: Integer): Boolean;
var
  Column: TLVColumn;
begin
  FillChar(Column, SizeOf(Column), 0);
  Column.Mask := LVCF_FMT;
  ListView_GetColumn(ListViewHandle, ACol, Column);
  Result := Column.fmt and LVCFMT_COL_HAS_IMAGES = LVCFMT_COL_HAS_IMAGES;
end;

function TCustomdxNativeListViewReportLink.LV_HasColumnHeaders: Boolean;
begin
  Result := GetWindowLong(ListViewHandle, GWL_STYLE) and LVS_NOCOLUMNHEADER <> LVS_NOCOLUMNHEADER;
end;

function TCustomdxNativeListViewReportLink.LV_IsOwnerData: Boolean;
begin
  Result := GetWindowLong(ListViewHandle, GWL_STYLE) and LVS_OWNERDATA = LVS_OWNERDATA;
end;

function TCustomdxNativeListViewReportLink.GetShowColumnHeaders: Boolean;
begin
  Result := LV_HasColumnHeaders;
end;

function TCustomdxNativeListViewReportLink.HasSupportForInvisibledColumnHeaders: Boolean;
begin
  Result := False;
end;

procedure TCustomdxNativeListViewReportLink.SetShowColumnHeaders(Value: Boolean);
begin
end;

function TCustomdxNativeListViewReportLink.CheckImages: Boolean;
begin
  Result := HasLargeImages and (Images.Width <> 0) and (Images.Height <> 0);
end;

function TCustomdxNativeListViewReportLink.CheckSmallImages: Boolean;
begin
  Result := HasSmallImages and (SmallImages.Width <> 0) and (SmallImages.Height <> 0);
end;

function TCustomdxNativeListViewReportLink.CheckStateImages: Boolean;
begin
  Result := HasStateImages and (StateImages.Width <> 0) and (StateImages.Height <> 0);
end;

procedure TCustomdxNativeListViewReportLink.ClearImages;
begin
  if not IsAggregated then
  begin
    if FLargeImages <> nil then FLargeImages.Clear;
    if FSmallImages <> nil then FSmallImages.Clear;
    if FStateImages <> nil then FStateImages.Clear;
  end;
end;

procedure TCustomdxNativeListViewReportLink.CreateImages;
begin
  FLargeImages := TImageList.Create(nil);
  FSmallImages := TImageList.Create(nil);
  FStateImages := TImageList.Create(nil);
end;

procedure TCustomdxNativeListViewReportLink.DeleteImages;
begin
  FreeAndNil(FStateImages);
  FreeAndNil(FSmallImages);
  FreeAndNil(FLargeImages);
end;

function TCustomdxNativeListViewReportLink.GetLargeImages: TCustomImageList;
begin
  if FLargeImages.Count = 0 then
    CopyImages(ListView_GetImageList(ListViewHandle, LVSIL_NORMAL), FLargeImages);
  Result := FLargeImages;
end;

function TCustomdxNativeListViewReportLink.GetSmallImages: TCustomImageList;
begin
  if FSmallImages.Count = 0 then
    CopyImages(ListView_GetImageList(ListViewHandle, LVSIL_SMALL), FSmallImages);
  Result := FSmallImages;
end;

function TCustomdxNativeListViewReportLink.GetStateImages: TCustomImageList;
begin
  if FStateImages.Count = 0 then
    CopyImages(ListView_GetImageList(ListViewHandle, LVSIL_STATE), FStateImages);
  Result := FStateImages;
end;

function TCustomdxNativeListViewReportLink.HasLargeImages: Boolean;
begin
  Result := ListView_GetImageList(ListViewHandle, LVSIL_NORMAL) <> 0;
end;

function TCustomdxNativeListViewReportLink.HasSmallImages: Boolean;
begin
  Result := ListView_GetImageList(ListViewHandle, LVSIL_SMALL) <> 0;
end;

function TCustomdxNativeListViewReportLink.HasStateImages: Boolean;
begin
  Result := ListView_GetImageList(ListViewHandle, LVSIL_STATE) <> 0;
end;

procedure TCustomdxNativeListViewReportLink.AddExtraImage(AParent: TdxReportCell;
  ACol, ARow: Integer;  R: TRect);
const
  ItemClasses: array [Boolean] of TdxReportCellDataClass = (TdxReportCellImage, TdxReportCellCheck);
var
  Item: TAbstractdxReportCellData;
begin
  Item := ItemClasses[LV_AreCheckBoxes].Create(AParent);
  with Item do
  begin
    Item.BoundsRect := R;
    AssignData(ACol, ARow, Item);
    TdxReportCellText(Item).Text := '';
    if R.Right <> R.Left then
      Item.CellSides := GetCellSides(ACol, ARow) - [csRight];
    if IsDrawBorder then
      Item.CellSides := Item.CellSides + [csLeft];
    DoInitializeItem(ACol, ARow, Item);
  end;

  if LV_AreCheckBoxes then
  begin
    TdxReportCellCheck(Item).Checked := IsItemChecked(ARow);
    TdxReportCellCheck(Item).BoldBorder := True;
  end
  else
  begin
    TdxReportCellImage(Item).ImageList := StateImages;
    TdxReportCellImage(Item).ImageIndex := LV_GetItemStateIndex(ACol, ARow);
    TdxReportCellImage(Item).MakeSpaceForEmptyImage := True;
  end;
end;

function TCustomdxNativeListViewReportLink.IsExtraImageRequired(ACol, ARow: Integer): Boolean;
begin
  Result := IsReportStyle and (ACol = 0) and not IsFixedRow(ARow) and CheckStateImages;
end;

procedure TCustomdxNativeListViewReportLink.CalculateColumnIndexes;
var
  AOrderArray: array of Integer;
  I: Integer;
begin
  SetLength(AOrderArray, ColCount);
  ListView_GetColumnOrderArray(ListViewHandle, ColCount, @AOrderArray[0]);

  FColumnIndexes.Clear;
  FColumnIndexes.Capacity := ColCount;
  for I := 0 to Length(AOrderArray) - 1 do
    FColumnIndexes.Add(Pointer(AOrderArray[I]));
end;

function TCustomdxNativeListViewReportLink.CalculateHeaderRowHeight: Integer;
begin
  Result := Max(1 + LV_GetHeaderHeight + 1,
    Renderer.CalcTextPatternHeight(ScreenCanvas, HeaderFont));
end;

function TCustomdxNativeListViewReportLink.CalculateRowHeight: Integer;
begin
  Result := Max(1 + LV_GetRowHeight + 1,
    Renderer.CalcTextPatternHeight(ScreenCanvas, Font));
end;

function TCustomdxNativeListViewReportLink.CanHasStateImage(ACol, ARow: Integer): Boolean;
begin
  Result := not LV_AreCheckBoxes and (not IsReportStyle or ((ACol = 0) and not IsFixedRow(ARow)));
end;

function TCustomdxNativeListViewReportLink.IsItemChecked(ARow: Integer): Boolean;
begin
  Result := IsReportStyle and not IsFixedRow(ARow) and LV_GetIsItemChecked(ARow - Ord(ShowColumnHeaders));
end;

procedure TCustomdxNativeListViewReportLink.LoadAllOwnerData;
var
  Item: TNMLVCacheHint;
begin
  with Item, hdr do
  begin
    hwndFrom := ListViewHandle;
    idFrom:= ListViewHandle;
    code := LVN_ODCACHEHINT;
    iFrom := 0;
    iTo := LV_GetItemCount - 1;
  end;
  SendMessage(ListViewHandle, WM_NOTIFY, ListViewHandle, LPARAM(@Item));
end;

procedure TCustomdxNativeListViewReportLink.CalculateInternalItemsViewInfo;

  procedure CalculateItemSize(var AWidth, AHeight: Integer);
  var
    I: Integer;
  begin
    with LV_GetItemSpacing do
    begin
      AWidth := Width;
      AHeight := Height;
    end;

    for I := 0 to InternalItemCount - 1 do
      with InternalItems[I] do
      begin
        if IsIconStyle then
          AHeight := Max(AHeight, MeasureContentHeight(ScreenCanvas))
        else
          AWidth := Max(AWidth, MeasureContentWidth(ScreenCanvas));
      end;
  end;

  procedure CalculateColRowCount(AWidth, AHeight: Integer);
  begin
    if (LV_GetViewStyle = vsList) or (LV_GetIconArrangement = iaLeft) then
    begin
      FRowCount := (AvailableSiteHeight - FItemSpaceVert) div (FRowHeight + FItemSpaceVert);
      if (FRowCount = 0) and (InternalItemCount <> 0) then
        FRowCount := 1;
      FRowCount := Min(FRowCount, InternalItemCount);
      FColumnCount := InternalItemCount div FRowCount;
      if InternalItemCount mod FRowCount <> 0 then
        Inc(FColumnCount);
    end
    else
    begin
      FColumnCount := (AvailableSiteWidth - FItemSpaceHorz) div (FColumnWidth + FItemSpaceHorz);
      if (FColumnCount = 0) and (InternalItemCount <> 0) then
        FColumnCount := 1;
      FColumnCount := Min(FColumnCount, InternalItemCount);
      FRowCount := InternalItemCount div FColumnCount;
      if InternalItemCount mod FColumnCount <> 0 then
        Inc(FRowCount);
    end;
  end;

begin
  CalculateItemSize(FColumnWidth, FRowHeight);
  CalculateColRowCount(FColumnWidth, FRowHeight);
end;

procedure TCustomdxNativeListViewReportLink.CreateExtraInternalItems(ATemporaryParent: TdxReportCell);
var
  I: Integer;
  R: TRect;
  LVItemCount: Integer;
  Item: TAbstractdxReportCellData;
begin
  if InternalItemCount <> 0 then
  begin
    with LV_GetItemSpacing do
      R := Rect(0, 0, Width, Height);

    FInternalItemsCreating := True;
    try
      LVItemCount := LV_GetItemCount;
      for I := 0 to ColCount * RowCount - LVItemCount - 1 do
      begin
        Item := CreateDataItem(ATemporaryParent, 0, LVItemCount + I, R);
        FInternalItems.Add(Item);
      end;
    finally
      FInternalItemsCreating := False;
    end;
  end;
end;

procedure TCustomdxNativeListViewReportLink.CreateInternalItems(ATemporaryParent: TdxReportCell);
var
  R: TRect;
  I: Integer;
  Item: TAbstractdxReportCellData;
begin
  if LV_GetItemCount = 0 then Exit;

  with LV_GetItemSpacing do
    R := Rect(0, 0, Width, Height);
  FInternalItemsCreating := True;
  try
    for I := 0 to LV_GetItemCount - 1 do
      if not OnlySelected or (LV_GetSelectedCount = 0) or LV_GetIsItemSelected(I) then
      begin
        Item := CreateDataItem(ATemporaryParent, 0, I, R);
        FInternalItems.Add(Item);
      end;
  finally
    FInternalItemsCreating := False;
  end;
end;

function TCustomdxNativeListViewReportLink.GetFlatInternalItemIndex(ACol, ARow: Integer): Integer;
begin
  if (LV_GetViewStyle = vsList) or (LV_GetIconArrangement = iaLeft) then
    Result := ACol * FRowCount + ARow
  else
    Result := ARow * FColumnCount + ACol;
end;

procedure TCustomdxNativeListViewReportLink.InitializeInternalItem(AnItem: TAbstractdxReportCellData;
  AnIndex: Integer);
begin
end;

function TCustomdxNativeListViewReportLink.PlaceInternalItem(AParent: TdxReportCell;
  ACol, ARow: Integer; R: TRect): TAbstractdxReportCellData;
var
  Index: Integer;
begin
  Index := GetFlatInternalItemIndex(ACol, ARow);
  if Index < InternalItemCount then
  begin
    Result := InternalItems[Index];
    Result.Parent := AParent;
    Result.BoundsRect := R;
    InitializeInternalItem(Result, Index);
  end
  else
    Result := nil;
end;

procedure TCustomdxNativeListViewReportLink.SetupInternalItemsCellSides;
var
  Col, Row, Index: Integer;
begin
  for Col := 0 to ColCount - 1 do
    for Row := 0 to RowCount - 1 do
    begin
      Index := GetFlatInternalItemIndex(Col, Row);
      if Index < InternalItemCount then
        InternalItems[Index].CellSides := GetCellSides(Col, Row);
    end;
end;

function TCustomdxNativeListViewReportLink.IsIconStyle: Boolean;
begin
  Result := LV_GetViewStyle = vsIcon;
end;

function TCustomdxNativeListViewReportLink.IsReportStyle: Boolean;
begin
  Result := LV_GetViewStyle = vsReport;
end;

function TCustomdxNativeListViewReportLink.GetHeaderColor: TColor;
begin
  Result := FixedColor;
end;

function TCustomdxNativeListViewReportLink.GetHeaderFont: TFont;
begin
  Result := FixedFont;
end;

function TCustomdxNativeListViewReportLink.GetHeaderTransparent: Boolean;
begin
  Result := FixedTransparent;
end;

function TCustomdxNativeListViewReportLink.GetIncludeHeaders: Boolean;
begin
  Result := IncludeFixed;
end;

function TCustomdxNativeListViewReportLink.GetImages: TCustomImageList;
begin
  if IsIconStyle then
    Result := LargeImages
  else
    Result := SmallImages;
end;

function TCustomdxNativeListViewReportLink.GetInternalItem(Index: Integer): TAbstractdxReportCellData;
begin
  Result := TAbstractdxReportCellData(FInternalItems[Index]);
end;

function TCustomdxNativeListViewReportLink.GetInternalItemCount: Integer;
begin
  Result := FInternalItems.Count;
end;

function TCustomdxNativeListViewReportLink.GetOptions: TdxListViewPaintOptions;
begin
  Result := FOptions;
end;

procedure TCustomdxNativeListViewReportLink.SetHeaderColor(Value: TColor);
begin
  FixedColor := Value;
end;

procedure TCustomdxNativeListViewReportLink.SetHeaderFont(Value: TFont);
begin
  FixedFont := Value;
end;

procedure TCustomdxNativeListViewReportLink.SetHeaderTransparent(Value: Boolean);
begin
  FixedTransparent := Value;
end;

procedure TCustomdxNativeListViewReportLink.SetIncludeHeaders(Value: Boolean);
begin
  IncludeFixed := Value;
end;

procedure TCustomdxNativeListViewReportLink.SetOptions(Value: TdxListViewPaintOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    //if lvpoBestColumnWidth in Options then RowAutoHeight := False;
    LinkModified(True);
  end;
end;

{ TdxNativeListViewReportLink }

function TdxNativeListViewReportLink.DataProviderPresent: Boolean;
begin
  if DataSource = rldsComponent then
    Result := IsWindow(ListViewHandle)
  else
    Result := inherited DataProviderPresent;
end;

procedure TdxNativeListViewReportLink.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent is TWinControl) and TWinControl(Component).HandleAllocated and
    (TWinControl(Component).Handle = ListViewHandle) then
    ListViewHandle := 0;
end;

function TdxNativeListViewReportLink.GetDesignerClass: TdxReportLinkDesignWindowClass;
begin
  Result := TdxfmLVReportLinkDesignWindow;
end;

function TdxNativeListViewReportLink.GetListViewHandle: THandle;
begin
  Result := FListViewHandle;
end;

procedure TdxNativeListViewReportLink.SetListViewHandle(
  Value: THandle);
begin
  // TODO: invalid report data
  if (FListViewHandle <> Value) and IsWindow(Value) then
  begin
    FListViewHandle := Value;
    LinkModified(True);
  end;
end;

{ TCustomdxListViewReportLink }

procedure TCustomdxListViewReportLink.Assign(Source: TPersistent);
begin
  if Source is TCustomdxListViewReportLink then
    with TCustomdxListViewReportLink(Source) do
    begin
      Self.ShowColumnHeaders := ShowColumnHeaders;
    end;
  inherited;
end;

class function TCustomdxListViewReportLink.Aggregable: Boolean;
begin
  Result := True;
end;

procedure TCustomdxListViewReportLink.AssignData(ACol, ARow: Integer;
  ADataItem: TAbstractdxReportCellData);
begin
  inherited;

  if IsReportStyle then
    if IsFixedRow(ARow) then
    begin
      ADataItem.Parent.Data := 0;
      ADataItem.Data := ACol;
    end
    else
    begin
      ADataItem.Parent.Data := Integer(TObject(ListViewGetItems(CustomListView)[ARow - Ord(ShowColumnHeaders)]));
      ADataItem.Data := ACol - 1;
    end;
end;

procedure TCustomdxListViewReportLink.CustomDraw(AItem: TAbstractdxReportCellData;
  ACanvas: TCanvas; ABoundsRect, AClientRect: TRect; var ADone: Boolean);
var
  DrawInfo: TdxListViewReportLinkCustomDrawInfo;
begin
  GetCustomDrawInfo(AItem, DrawInfo);
  with DrawInfo do
    case AttributeID of
      dxListViewHeaderID:
        DoCustomDrawHeader(ACanvas, ABoundsRect, HeaderIndex, TdxReportCellImage(AItem), ADone);
      dxListViewItemID:
        DoCustomDrawItem(ACanvas, ABoundsRect, ListItem, SubItem, TdxReportCellImage(AItem), ADone);
    end;
end;

procedure TCustomdxListViewReportLink.DoInitializeHeader(ACol, ARow: Integer;
  ADataItem: TAbstractdxReportCellData);
begin
  if Assigned(FOnInitializeHeader) then
    FOnInitializeHeader(Self, ARow, ACol, ADataItem);
end;

procedure TCustomdxListViewReportLink.DoInitializeItem(ACol, ARow: Integer;
  ADataItem: TAbstractdxReportCellData);
begin
  if IsFixedRow(ARow) then
    DoInitializeHeader(ACol, ARow, ADataItem)
  else
    inherited;
end;

procedure TCustomdxListViewReportLink.InternalRestoreFromOriginal;
begin
  inherited;
  ShowColumnHeaders := ListViewGetShowColumnHeaders(CustomListView);
end;

procedure TCustomdxListViewReportLink.InternalRestoreDefaults;
begin
  inherited;
  ShowColumnHeaders := True;
end;

procedure TCustomdxListViewReportLink.PrepareConstruct(AReportCells: TdxReportCells);
begin
  CustomListView.HandleNeeded;
  inherited;
end;

function TCustomdxListViewReportLink.GetListViewHandle: THandle;
begin
  Result := CustomListView.Handle;
end;

function TCustomdxListViewReportLink.GetShowColumnHeaders: Boolean;
begin
  Result := FShowColumnHeaders;
end;

function TCustomdxListViewReportLink.HasSupportForInvisibledColumnHeaders: Boolean;
begin
  Result := True;
end;

procedure TCustomdxListViewReportLink.InitializeInternalItem(AnItem: TAbstractdxReportCellData;
  AnIndex: Integer);
begin
  inherited;
  if AnIndex < LV_GetItemCount then
  begin
    AnItem.Data := Integer(TObject(ListViewGetItems(CustomListView)[AnIndex]));
    AnItem.Parent.Data := 0;
  end;
end;

function TCustomdxListViewReportLink.IsItemChecked(ARow: Integer): Boolean;
begin
  Result := IsReportStyle and not IsFixedRow(ARow) and
    ListViewGetItems(CustomListView)[ARow - Ord(ShowColumnHeaders)].Checked;
end;

procedure TCustomdxListViewReportLink.LoadAllOwnerData;
const
  Request: TItemRequest = [irText, irImage, irState];
var
  I: Integer;
begin
  inherited;
  for I := 0 to LV_GetItemCount - 1 do
    ListViewOwnerDataFetch(CustomListView, I, Request);
end;

procedure TCustomdxListViewReportLink.SetListViewHandle(
  Value: THandle);
begin
end;

procedure TCustomdxListViewReportLink.SetShowColumnHeaders(Value: Boolean);
begin
  if FShowColumnHeaders <> Value then
  begin
    FShowColumnHeaders := Value;
    LinkModified(True);
  end;
end;

function TCustomdxListViewReportLink.GetLargeImages: TCustomImageList;
begin
  if IsAggregated then
    Result := ListViewGetLargeImages(CustomListView)
  else
    Result := inherited GetLargeImages;
end;

function TCustomdxListViewReportLink.GetSmallImages: TCustomImageList;
begin
  if IsAggregated then
    Result := ListViewGetSmallImages(CustomListView)
  else
    Result := inherited GetSmallImages;
end;

function TCustomdxListViewReportLink.GetStateImages: TCustomImageList;
begin
  if IsAggregated then
    Result := ListViewGetStateImages(CustomListView)
  else
    Result := inherited GetStateImages;
end;

function TCustomdxListViewReportLink.GetCustomListView: TCustomListView;
begin
  Result := TCustomListView(Component);
end;

procedure TCustomdxListViewReportLink.DoCustomDrawHeader(ACanvas: TCanvas;
  ARect: TRect; AHeaderIndex: Integer; AnItem: TdxReportCellImage;
  var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawHeader) then
    FOnCustomDrawHeader(Self, ACanvas, ARect, AHeaderIndex, AnItem, ADone);
end;

procedure TCustomdxListViewReportLink.DoCustomDrawItem(ACanvas: TCanvas;
  ARect: TRect; AListItem: TListItem; ASubItem: Integer; AnItem: TdxReportCellImage;
  var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawItem) then
    FOnCustomDrawItem(Self, ACanvas, ARect, AListItem, ASubItem, AnItem, ADone);
end;

procedure TCustomdxListViewReportLink.GetCustomDrawInfo(AnItem: TAbstractdxReportCellData;
  var ACustomDrawInfo: TdxListViewReportLinkCustomDrawInfo);
begin
  FillChar(ACustomDrawInfo, SizeOf(ACustomDrawInfo), 0);
  with ACustomDrawInfo do
  begin
    if IsReportStyle and (AnItem.Parent.Data = 0) then
      AttributeID := dxListViewHeaderID
    else
      AttributeID := dxListViewItemID;

    case AttributeID of
      dxListViewHeaderID:
        HeaderIndex := AnItem.Data;
      dxListViewItemID:
        if IsReportStyle then
        begin
          ListItem := TObject(AnItem.Parent.Data) as TListItem;
          SubItem := AnItem.Data;
        end
        else
          ListItem := TObject(AnItem.Data) as TListItem;
    end;
  end;
end;

procedure TCustomdxListViewReportLink.SetOnCustomDrawHeader(Value: TdxCustomDrawListViewHeaderEvent);
begin
  if @OnCustomDrawHeader <> @Value then
  begin
    OnCustomDrawHeader := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TCustomdxListViewReportLink.SetOnCustomDrawItem(Value: TdxCustomDrawListViewItemEvent);
begin
  if @FOnCustomDrawItem <> @Value then
  begin
    FOnCustomDrawItem:= Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

{ TdxListViewReportLink }

function TdxListViewReportLink.GetListView: TListView;
begin
  Result := TListView(Component);
end;

{$IFDEF REGISTERSHELLCTRLS}

{ TCustomdxShellListViewReportLink }

function TCustomdxShellListViewReportLink.HasStateImages: Boolean;
begin
  Result := not IsReportStyle or inherited HasStateImages;
end;

procedure TCustomdxShellListViewReportLink.CreateImages;
begin
end;

procedure TCustomdxShellListViewReportLink.DeleteImages;
begin
end;

function TCustomdxShellListViewReportLink.GetLargeImages: TCustomImageList;
begin
  Result := dxPSUtl.ShellLargeImages;
end;

function TCustomdxShellListViewReportLink.GetSmallImages: TCustomImageList;
begin
  Result := dxPSUtl.ShellSmallImages;
end;

function TCustomdxShellListViewReportLink.GetStateImages: TCustomImageList;
begin
  Result := dxPSUtl.ShellSmallImages;
end;

function TCustomdxShellListViewReportLink.GetShellListView: TShellListView;
begin
  Result := TShellListView(Component);
end;

{$ENDIF}

{ TdxfmLVReportLinkDesignWindow }

constructor TdxfmLVReportLinkDesignWindow.Create(AOwner: TComponent);
begin
  HelpContext := dxPSGlbl.dxhcListViewReportLinkDesigner;
  inherited;
  CreateControls;
end;

procedure TdxfmLVReportLinkDesignWindow.DoInitialize;
begin
  inherited;

  chbxShowBorders.Checked := lvpoBorder in ReportLink.Options;
  chbxShowHorzLines.Checked := lvpoHorzLines in ReportLink.Options;
  chbxShowVertLines.Checked := lvpoVertLines in ReportLink.Options;
  chbxShowColumnHeaders.Checked := ReportLink.ShowColumnHeaders;
  lichbxShowColumnHeaders.Visible := ReportLink.HasSupportForInvisibledColumnHeaders;

  chbxAutoWidth.Checked := ReportLink.AutoWidth;
  chbxRowAutoHeight.Checked := ReportLink.RowAutoHeight;
  dxPSSyncDrawModeComboItemIndex(cbxDrawMode, ReportLink.DrawMode);

  chbxTransparent.Checked := ReportLink.Transparent;
  ccbxColor.ColorValue := ColorToRGB(ReportLink.Color);
  ccbxEvenColor.ColorValue := ColorToRGB(ReportLink.EvenColor);
  chbxTransparentHeaders.Checked := ReportLink.HeaderTransparent;
  ccbxHeadersColor.ColorValue := ColorToRGB(ReportLink.HeaderColor);
  ccbxGridLineColor.ColorValue := ColorToRGB(ReportLink.GridLineColor);

  FontInfoToText(ReportLink.Font, edFont);
  FontInfoToText(ReportLink.EvenFont, edEvenFont);
  FontInfoToText(ReportLink.HeaderFont, edFixedFont);

  chbxHeadersOnEveryPage.Checked := ReportLink.HeadersOnEveryPage;
  chbxOnlySelected.Checked := ReportLink.OnlySelected;
  chbxIncludeFixed.Checked := ReportLink.IncludeFixed;

  chbxUse3DEffects.Checked := ReportLink.Effects3D;
  chbxUseSoft3D.Checked := ReportLink.Soft3D;
end;

{$IFDEF DELPHI7}
function TdxfmLVReportLinkDesignWindow.GetPreviewHost: TCustomPanel;
begin
  Result := pnlPreview;
end;
{$ENDIF}

procedure TdxfmLVReportLinkDesignWindow.LoadGroupsIcons;
begin
  inherited LoadGroupsIcons;
  dxLoadIconFromResourceEx(imgGrid, IDB_DXPSGROUPICON_SHOW);
  dxLoadIconFromResourceEx(Image1, IDB_DXPSGROUPICON_ONEVERYPAGE);
  dxLoadIconFromResourceEx(Image2, IDB_DXPSGROUPICON_SELECTION);
  dxLoadIconFromResourceEx(Image3, IDB_DXPSGROUPICON_LOOKANDFEEL);
  dxLoadIconFromResourceEx(Image4, IDB_DXPSGROUPICON_SIZE);
end;

procedure TdxfmLVReportLinkDesignWindow.LoadStrings;
begin
  inherited;

  lblHint.Caption := cxGetResourceString(@sdxHintListViewDesignerMessage);

  tshOptions.Caption := cxGetResourceString(@sdxOptions);
  tshFont.Caption := cxGetResourceString(@sdxFonts);
  tshColor.Caption := cxGetResourceString(@sdxColors);
  tshBehaviors.Caption := cxGetResourceString(@sdxBehaviors);
  lblPreview.Caption := DropAmpersand(cxGetResourceString(@sdxPreview));

  lblShow.Caption := cxGetResourceString(@sdxShow);
  chbxShowBorders.Caption := cxGetResourceString(@sdxBorderLines);
  chbxShowHorzLines.Caption := cxGetResourceString(@sdxHorzLines);
  chbxShowVertLines.Caption := cxGetResourceString(@sdxVertLines);
  chbxShowColumnHeaders.Caption := cxGetResourceString(@sdxColumnHeaders);

  lblMiscellaneous.Caption := cxGetResourceString(@sdxMiscellaneous);
  chbxAutoWidth.Caption := cxGetResourceString(@sdxAutoWidth);
  chbxRowAutoHeight.Caption := cxGetResourceString(@sdxRowAutoHeight);
  lblDrawMode.Caption := cxGetResourceString(@sdxDrawMode);
  dxPSInitalizeDrawModeCombo(cbxDrawMode, [gdmStrict, gdmOddEven, gdmChess]);

  stTransparent.Caption := ' ' + cxGetResourceString(@sdxTransparent) + ' ';
  lblColor.Caption := cxGetResourceString(@sdxColor);
  lblEvenColor.Caption := cxGetResourceString(@sdxEvenColor);
  stTransparentHeaders.Caption := ' ' + cxGetResourceString(@sdxHeadersTransparent) + ' ';
  lblHeadersColor.Caption := cxGetResourceString(@sdxHeaderColor);
  lblGridLinesColor.Caption := cxGetResourceString(@sdxGridLinesColor);

  btnFont.Caption := cxGetResourceString(@sdxBtnFont);
  btnEvenFont.Caption := cxGetResourceString(@sdxBtnEvenFont);
  btnHeadersFont.Caption := cxGetResourceString(@sdxBtnHeadersFont);

  lblOnEveryPage.Caption := cxGetResourceString(@sdxOnEveryPage);
  chbxHeadersOnEveryPage.Caption := cxGetResourceString(@sdxHeadersOnEveryPage);

  lblSelection.Caption := cxGetResourceString(@sdxSelection);
  chbxOnlySelected.Caption := cxGetResourceString(@sdxOnlySelected);
  chbxIncludeFixed.Caption := cxGetResourceString(@sdxIncludeFixed);

  lblLookAndFeel.Caption := cxGetResourceString(@sdxLookAndFeel);
  chbxUse3DEffects.Caption := cxGetResourceString(@sdxUse3DEffects);
  chbxUseSoft3D.Caption := cxGetResourceString(@sdxSoft3D);
end;

procedure TdxfmLVReportLinkDesignWindow.PaintPreview(ACanvas: TCanvas; R: TRect);
begin
  inherited;
  dxPSDrawGridPreview(ACanvas, R, ReportLink, False, ReportLink.ShowColumnHeaders, ScaleFactor);
end;

procedure TdxfmLVReportLinkDesignWindow.UpdateControlsState;
begin
  inherited;
  ccbxColor.Enabled := not chbxTransparent.Checked;
  lblColor.Enabled := ccbxColor.Enabled;
  ccbxEvenColor.Enabled := not chbxTransparent.Checked and
    (ReportLink.DrawMode in [gdmOddEven, gdmChess]);
  lblEvenColor.Enabled := ccbxEvenColor.Enabled;
  ccbxHeadersColor.Enabled := not chbxTransparentHeaders.Checked;
  lblHeadersColor.Enabled := ccbxHeadersColor.Enabled;

  btnEvenFont.Enabled := ReportLink.DrawMode in [gdmOddEven, gdmChess];
  if ReportLink.DrawMode in [gdmOddEven, gdmChess] then
  begin
    lblColor.Caption := cxGetResourceString(@sdxOddColor);
    btnFont.Caption := cxGetResourceString(@sdxBtnOddFont);
  end
  else
  begin
    lblColor.Caption := cxGetResourceString(@sdxColor);
    btnFont.Caption := cxGetResourceString(@sdxBtnFont);
  end;
  chbxHeadersOnEveryPage.Enabled := not ReportLink.IsAggregated;
  chbxIncludeFixed.Enabled := chbxOnlySelected.Enabled and chbxOnlySelected.Checked;
  chbxUseSoft3D.Enabled := chbxUse3DEffects.Checked;
end;

procedure TdxfmLVReportLinkDesignWindow.UpdatePreview;
begin
  FPreviewBox.Invalidate;
end;

function TdxfmLVReportLinkDesignWindow.GetReportLink: TCustomdxNativeListViewReportLink;
begin
  Result := inherited ReportLink as TCustomdxNativeListViewReportLink;
end;

procedure TdxfmLVReportLinkDesignWindow.CreateControls;
begin
  FPreviewBox := TdxPSPaintPanel.Create(Self);
  FPreviewBox.Parent := pnlPreview;
  FPreviewBox.Align := alClient;
  TdxPSPaintPanel(FPreviewBox).EdgeInner := esNone;
  TdxPSPaintPanel(FPreviewBox).EdgeOuter := esNone;
  TdxPSPaintPanel(FPreviewBox).OnPaint := pbxPreviewPaint;
end;

procedure TdxfmLVReportLinkDesignWindow.CMDialogChar(var message: TCMDialogChar);
var
  I: Integer;
begin
  inherited;
  for I := 0 to pcMain.Count - 1 do
    if IsAccel(message.CharCode, pcMain.Items[I].Caption) then
    begin
      message.Result := 1;
      pcMain.ItemIndex := I;
      Exit;
    end;
end;

procedure TdxfmLVReportLinkDesignWindow.cbxDrawModeClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.DrawMode := dxPSGetSelectedDrawMode(cbxDrawMode);
  Modified := True;
  UpdatePreview;
end;

procedure TdxfmLVReportLinkDesignWindow.chbxShowBordersClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  with TcxCheckBox(Sender) do
    if Checked then
      ReportLink.Options := ReportLink.Options + [TdxListViewPaintOption(TTagToInt(Tag))]
    else
      ReportLink.Options := ReportLink.Options - [TdxListViewPaintOption(TTagToInt(Tag))];

  Modified := True;
  UpdatePreview;
end;

procedure TdxfmLVReportLinkDesignWindow.chbxShowColumnHeadersClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.ShowColumnHeaders := TcxCheckBox(Sender).Checked;
  Modified := True;
  UpdatePreview;
end;

procedure TdxfmLVReportLinkDesignWindow.chbxAutoWidthClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.AutoWidth := TcxCheckBox(Sender).Checked;
  Modified := True;
end;

procedure TdxfmLVReportLinkDesignWindow.chbxRowAutoHeightClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.RowAutoHeight := TcxCheckBox(Sender).Checked;
  Modified := True;
end;

procedure TdxfmLVReportLinkDesignWindow.chbxTransparentClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  case TTagToInt(TcxCheckBox(Sender).Tag) of
    0: ReportLink.Transparent := TcxCheckBox(Sender).Checked;
    1: ReportLink.HeaderTransparent := TcxCheckBox(Sender).Checked;
  end;
  Modified := True;
  UpdatePreview;
end;

procedure TdxfmLVReportLinkDesignWindow.stTransparentClick(Sender: TObject);
begin
  if chbxTransparent.CanFocus then ActiveControl := chbxTransparent;
  chbxTransparent.Checked := not chbxTransparent.Checked;
end;

procedure TdxfmLVReportLinkDesignWindow.stTransparentHeadersClick(Sender: TObject);
begin
  if chbxTransparentHeaders.CanFocus then ActiveControl := chbxTransparentHeaders;
  chbxTransparentHeaders.Checked := not chbxTransparentHeaders.Checked;
end;

procedure TdxfmLVReportLinkDesignWindow.ccbxColorChange(Sender: TObject);
var
  AColor: TColor;
begin
  if LockControlsUpdate then Exit;
  AColor := TcxColorComboBox(Sender).ColorValue;
  case TTagToInt(TcxColorComboBox(Sender).Tag) of
    0: ReportLink.Color := AColor;
    1: ReportLink.EvenColor := AColor;
    2: ReportLink.FixedColor := AColor;
    3: ReportLink.GridLineColor := AColor;
  end;
  Modified := True;
  UpdatePreview;
end;

procedure TdxfmLVReportLinkDesignWindow.btnFontClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;

  with dxPSGlbl.FontDialog do
  begin
    case TTagToInt(TcxButton(Sender).Tag) of
      0: Font := ReportLink.Font;
      1: Font := ReportLink.EvenFont;
      2: Font := ReportLink.HeaderFont;
    end;

    if Execute then
    begin
      case TTagToInt(TcxButton(Sender).Tag) of
        0:
          begin
            ReportLink.Font := Font;
            FontInfoToText(ReportLink.Font, edFont);
          end;
        1:
          begin
            ReportLink.EvenFont := Font;
            FontInfoToText(ReportLink.EvenFont, edEvenFont);
          end;
        2:
          begin
            ReportLink.FixedFont := Font;
            FontInfoToText(ReportLink.HeaderFont, edFixedFont);
          end;
      end;
      Modified := True;
      UpdatePreview;
    end;
  end;
end;

procedure TdxfmLVReportLinkDesignWindow.chbxHeadersOnEveryPageClick(
  Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.HeadersOnEveryPage := TcxCheckBox(Sender).Checked;
  Modified := True;
end;

procedure TdxfmLVReportLinkDesignWindow.chbxOnlySelectedClick(
  Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.OnlySelected := TcxCheckBox(Sender).Checked;
  Modified := True;
end;

procedure TdxfmLVReportLinkDesignWindow.chbxIncludeFixedClick(
  Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.IncludeFixed := TcxCheckBox(Sender).Checked;
  Modified := True;
end;

procedure TdxfmLVReportLinkDesignWindow.chbxUse3DEffectsClick(
  Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.Effects3D := TcxCheckBox(Sender).Checked;
  Modified := True;
  UpdatePreview;
end;

procedure TdxfmLVReportLinkDesignWindow.chbxUseSoft3DClick(
  Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.Soft3D := TcxCheckBox(Sender).Checked;
  Modified := True;
  UpdatePreview;
end;

procedure TdxfmLVReportLinkDesignWindow.pbxPreviewPaint(Sender: TObject);
begin
  with FPreviewBox do
    PaintPreview(Canvas, ClientRect);
end;

initialization
  dxPSRegisterReportLink(TdxListViewReportLink, TListView, TdxfmLVReportLinkDesignWindow);
 {$IFDEF REGISTERSHELLCTRLS}
  dxPSRegisterReportLink(TdxShellListViewReportLink, TShellListView, TdxfmLVReportLinkDesignWindow);
 {$ENDIF}

finalization
 {$IFDEF REGISTERSHELLCTRLS}
  dxPSUnregisterReportLink(TdxShellListViewReportLink, TShellListView, TdxfmLVReportLinkDesignWindow);
 {$ENDIF}
  dxPSUnregisterReportLink(TdxListViewReportLink, TListView, TdxfmLVReportLinkDesignWindow);

end.

