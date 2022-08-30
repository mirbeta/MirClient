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

unit dxPSTVLnk;

interface

{$I cxVer.inc}

uses
  Types, Classes, Windows, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls, ImgList, CommCtrl, ExtCtrls,
  ComCtrls, {$IFDEF REGISTERSHELLCTRLS}ShellCtrls,{$ENDIF} dxPSCore, dxPSGlbl, dxPSRes, cxDrawTextUtils, cxPC,
  cxControls, cxContainer, cxEdit, cxCheckBox, cxLabel, cxGraphics, cxSpinEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit,
  cxColorComboBox, Menus, cxLookAndFeelPainters, cxButtons, dxPSReportRenderCanvas, cxLookAndFeels, cxClasses,
  dxPSReportLinkDesignWindow, dxLayoutControlAdapters, dxLayoutLookAndFeels, dxLayoutContainer, dxLayoutControl,
  dxLayoutcxEditAdapters, cxImageList, cxImage;

type
  TdxfmTVReportLinkDesignWindow = class;

  TdxTreeViewPaintOption = (tvpoBorder, tvpoGrid, tvpoStateImages, tvpoImages, tvpoTreeLines, tvpoButtons);
  TdxTreeViewPaintOptions = set of TdxTreeViewPaintOption;

  TCustomdxNativeTreeViewReportLink = class(TBasedxReportLink, IdxPSNativeWin32ControlHandleSupport)
  private
    FActualWidth: Integer;
    FAutoNodesExpand: Boolean;
    FAutoWidth: Boolean;
    FExpandLevel: Integer;
    FExplicitTreeViewExpand: Boolean;
    FGridLineColor: TColor;
    FImages: TCustomImageList;
    FIsWidthAssigned: Boolean;
    FNodeAutoHeight: Boolean;
    FOptions: TdxTreeViewPaintOptions;
    FStateImages: TCustomImageList;
    FTreeLineColor: TColor;
    FWidth: Integer;
    function GetOptions: TdxTreeViewPaintOptions;
    function GetWidth: Integer;
    function IsWidthStored: Boolean;
    procedure SetAutoNodesExpand(Value: Boolean);
    procedure SetAutoWidth(Value: Boolean);
    procedure SetExpandLevel(Value: Integer);
    procedure SetExplicitTreeViewExpand(Value: Boolean);
    procedure SetGridLineColor(Value: TColor);
    procedure SetNodeAutoHeight(Value: Boolean);
    procedure SetOptions(Value: TdxTreeViewPaintOptions);
    procedure SetTreeLineColor(Value: TColor);
    procedure SetWidth(Value: Integer);

    procedure CalcNodesHeight;
    function CanDrawImages: Boolean;
    function CanDrawStateImages: Boolean;
    function GetCellSides(ANode: HTREEITEM): TdxCellSides;
    function GetNodeHeight(Index: Integer): Integer;
    function IsDrawBorder: Boolean;
    function IsDrawGrid: Boolean;
    function IsDrawNodeImages(ANode: HTREEITEM): Boolean;
    function IsDrawNodeStateImages(ANode: HTREEITEM): Boolean;
    function IsFirstNode(ANode: HTREEITEM): Boolean;
    function IsLastNode(ANode: HTREEITEM): Boolean;
    function IsValidNodeImageIndex(ANode: HTREEITEM): Boolean;
    function IsValidNodeStateIndex(ANode: HTREEITEM): Boolean;
  protected
    FIndent: Integer;
    FNodeHeight: Integer;
    FNodeHeights: TList;
    FNodeList: TList;
    FScreenCanvas: TdxPSReportRenderCustomCanvas;

    procedure AssignData(ANode: HTREEITEM; ADataItem: TAbstractdxReportCellData); virtual;
    procedure ConstructReport(AReportCells: TdxReportCells); override;
    procedure GetImageLists(AProc: TdxPSGetImageListProc); override;
    procedure InitializeItem(ANode: HTREEITEM; ADataItem: TAbstractdxReportCellData); virtual;
    procedure InternalRestoreDefaults; override;
    procedure MakeDelimiters(AReportCells: TdxReportCells; AHorzDelimiters,
      AVertDelimiters: TList); override;
    procedure PrepareContruct; virtual;
    procedure UnprepareContruct; virtual;

    function CheckImages: Boolean;
    function CheckStateImages: Boolean;
    procedure ClearImages; virtual;
    procedure CreateImages; virtual;
    procedure DeleteImages; virtual;
    function GetImages: TCustomImageList; virtual;
    function GetStateImages: TCustomImageList; virtual;
    function HasImages: Boolean; virtual;
    function HasStateImages: Boolean; virtual;

    function GetDataClass(ANode: HTREEITEM): TdxReportCellDataClass; virtual;
    function GetTreeViewHandle: THandle; virtual; abstract;
    procedure SetTreeViewHandle(Value: THandle); virtual; abstract;
    procedure DoExplicitTreeViewExpand(ALevel: Integer = -1); virtual;

    { IdxPSNativeWin32ControlHandleSupport }
    function GetNativeHandle: THandle; virtual;
    procedure SetNativeHandle(Value: THandle); virtual;

    { TreeView native access helpers }
    function TV_IsNodeExpanded(AItem: HTREEITEM): Boolean;
    function TV_IsNodeParentExists(AItem: HTREEITEM): Boolean;
    function TV_IsNodeFirstChild(AItem: HTREEITEM): Boolean;
    function TV_IsNodeLastChild(AItem: HTREEITEM): Boolean;
    function TV_IsShowButtons: Boolean;
    function TV_IsShowLines: Boolean;
    function TV_IsShowRoot: Boolean;
    function TV_GetNodeCount(AItem: HTREEITEM): Integer;
    function TV_GetNodeHasChildren(AItem: HTREEITEM): Boolean;
    function TV_GetNodeImageIndex(AItem: HTREEITEM): Integer;
    function TV_GetNodeLevel(AItem: HTREEITEM): Integer;
    function TV_GetNodeParent(AItem: HTREEITEM): HTREEITEM;
    function TV_GetNodeParentAtLevel(AItem: HTREEITEM; ALevel: Integer): HTREEITEM;
    function TV_GetNodeStateIndex(AItem: HTREEITEM): Integer;
    function TV_GetNodeText(AItem: HTREEITEM): string;

    property Images: TCustomImageList read GetImages;
    property StateImages: TCustomImageList read GetStateImages;
    property TreeViewHandle: THandle read GetTreeViewHandle write SetTreeViewHandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    class function Aggregable: Boolean; override;
    function DefaultWidth: Integer; virtual;
    procedure Initialize; override;

    property AutoNodesExpand: Boolean read FAutoNodesExpand write SetAutoNodesExpand default False;
    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth default True;
    property Color;
    property ExpandLevel: Integer read FExpandLevel write SetExpandLevel default -1;
    property ExplicitTreeViewExpand: Boolean read FExplicitTreeViewExpand write SetExplicitTreeViewExpand default False;
    property Font;
    property GridLineColor: TColor read FGridLineColor write SetGridLineColor default clBlack;
    property Options: TdxTreeViewPaintOptions read GetOptions write SetOptions
      default [tvpoStateImages, tvpoImages, tvpoTreeLines, tvpoButtons];
    property NodeAutoHeight: Boolean read FNodeAutoHeight write SetNodeAutoHeight default False;
    property ScaleFonts;
    property Transparent;
    property TreeLineColor: TColor read FTreeLineColor write SetTreeLineColor default clGray;
    property Width: Integer read GetWidth write SetWidth stored IsWidthStored;
  end;

  TdxNativeTreeViewReportLink = class(TCustomdxNativeTreeViewReportLink)
  private
    FTreeViewHandle: THandle;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GetDesignerClass: TdxReportLinkDesignWindowClass; override;

    function GetTreeViewHandle: THandle; override;
    procedure SetTreeViewHandle(Value: THandle); override;
  public
    function DataProviderPresent: Boolean; override;
    property TreeViewHandle;
  published
    property AutoNodesExpand;
    property AutoWidth;
    property Color;
    property ExpandLevel;
    property ExplicitTreeViewExpand;
    property Font;
    property GridLineColor;
    property Options;
    property NodeAutoHeight;
    property ScaleFonts;
    property Transparent;
    property TreeLineColor;
    property UseVertDelimiters;
    property Width;
  end;

  TdxTVInitializeNodeEvent = procedure(Sender: TBasedxReportLink; ANode: TTreeNode;
    AnItem: TdxReportCellString) of object;

  TdxTVCustomDrawNodeEvent = procedure(Sender: TBasedxReportLink; ANode: TTreeNode;
    ACanvas: TCanvas; ABoundsRect, AClientRect: TRect; var AText: string;
    AFont: TFont; var AColor: TColor; var ATextAlignX: TcxTextAlignX;
    var ATextAlignY: TcxTextAlignY; var ADone: Boolean) of object;

  TCustomdxTreeViewReportLink = class(TCustomdxNativeTreeViewReportLink)
  private
    FCustomDrawFontChanged: Boolean;
    FSaveFont: TFont;
    FSupportedCustomDraw: Boolean;
    FOnCustomDrawNode: TdxTVCustomDrawNodeEvent;
    FOnInitializeNode: TdxTVInitializeNodeEvent;
    procedure SetSupportedCustomDraw(Value: Boolean);
    procedure CustomDrawFontChanged(Sender: TObject);
  protected
    procedure InitializeItem(ANode: HTREEITEM; ADataItem: TAbstractdxReportCellData); override;
    procedure InternalRestoreDefaults; override;
    function IsSupportedCustomDraw(Item: TAbstractdxReportCellData): Boolean; override;

    function GetImages: TCustomImageList; override;
    function GetStateImages: TCustomImageList; override;

    function GetCustomTreeView: TCustomTreeView; virtual;
    function GetTreeViewHandle: THandle; override;
    procedure SetTreeViewHandle(Value: THandle); override;

    procedure CustomDraw(AItem: TAbstractdxReportCellData; ACanvas: TCanvas;
      ABoundsRect, AClientRect: TRect; var ADone: Boolean); override;
    procedure DoCustomDrawNode(ANode: TTreeNode; ACanvas: TCanvas;
      ABoundsRect, AClientRect: TRect; var AText: string; AFont: TFont;
      var AColor: TColor; var ATextAlignX: TcxTextAlignX; var ATextAlignY: TcxTextAlignY;
      var ADone: Boolean); virtual;
    procedure DoInitializeNode(ANode: TTreeNode; AnItem: TAbstractdxReportCellData); dynamic;

    property CustomTreeView: TCustomTreeView read GetCustomTreeView;
    property OnCustomDrawNode: TdxTVCustomDrawNodeEvent read FOnCustomDrawNode write FOnCustomDrawNode;
    property OnInitializeNode: TdxTVInitializeNodeEvent read FOnInitializeNode write FOnInitializeNode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    class function Aggregable: Boolean; override;

    property SupportedCustomDraw: Boolean read FSupportedCustomDraw write SetSupportedCustomDraw default False;
  end;

  TdxTreeViewReportLink = class(TCustomdxTreeViewReportLink)
  private
    function GetTreeView: TTreeView;
  public
    property TreeView: TTreeView read GetTreeView;
  published
    property AutoNodesExpand;
    property AutoWidth;
    property Color;
    property ExpandLevel;
    property ExplicitTreeViewExpand;
    property Font;
    property GridLineColor;
    property Options;
    property ScaleFonts;
    property SupportedCustomDraw;
    property Transparent;
    property TreeLineColor;
    property UseVertDelimiters;
    property Width;

    property OnCustomDrawNode;
    property OnInitializeNode;
  end;

 {$IFDEF REGISTERSHELLCTRLS}

  TCustomdxShellTreeViewReportLink = class(TCustomdxTreeViewReportLink)
  private
    function GetShellTreeView: TShellTreeView;
  protected
    procedure CreateImages; override;
    procedure DeleteImages; override;
    function GetImages: TCustomImageList; override;
    function GetStateImages: TCustomImageList; override;
    function HasStateImages: Boolean; override;

    property ShellTreeView: TShellTreeView read GetShellTreeView;
  end;

  TdxShellTreeViewReportLink = class(TCustomdxShellTreeViewReportLink)
  public
    property ShellTreeView;
  published
    property AutoNodesExpand;
    property AutoWidth;
    property Color;
    property ExpandLevel;
    property ExplicitTreeViewExpand;
    property Font;
    property GridLineColor;
    property Options;
    property ScaleFonts;
    property SupportedCustomDraw;
    property Transparent;
    property TreeLineColor;
    property UseVertDelimiters;
    property Width;

    property OnCustomDrawNode;
    property OnInitializeNode;
  end;

 {$ENDIF}

  TdxfmTVReportLinkDesignWindow = class(TStandarddxReportLinkDesignWindow)
    ilPreview: TcxImageList;
    lblPreview: TdxLayoutItem;
    pnlPreview: TPanel;
    pcMain: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    tshOptions: TdxLayoutGroup;
    tshColors: TdxLayoutGroup;
    tshFonts: TdxLayoutGroup;
    tshBehaviors: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    lblShow: TcxLabel;
    dxLayoutItem2: TdxLayoutItem;
    imgGrid: TcxImage;
    dxLayoutItem3: TdxLayoutItem;
    chbxShowBorders: TcxCheckBox;
    dxLayoutItem4: TdxLayoutItem;
    chbxShowGrid: TcxCheckBox;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    dxLayoutSeparatorItem2: TdxLayoutSeparatorItem;
    dxLayoutItem5: TdxLayoutItem;
    chbxShowTreeLines: TcxCheckBox;
    dxLayoutItem6: TdxLayoutItem;
    chbxShowButtons: TcxCheckBox;
    dxLayoutItem7: TdxLayoutItem;
    chbxShowStateImages: TcxCheckBox;
    dxLayoutItem8: TdxLayoutItem;
    chbxShowImages: TcxCheckBox;
    dxLayoutItem9: TdxLayoutItem;
    stTransparent: TcxLabel;
    dxLayoutItem10: TdxLayoutItem;
    chbxTransparent: TcxCheckBox;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    lblColor: TdxLayoutItem;
    ccbxColor: TcxColorComboBox;
    lblGridLinesColor: TdxLayoutItem;
    ccbxGridLineColor: TcxColorComboBox;
    lblTreeLinesColor: TdxLayoutItem;
    ccbxTreeLineColor: TcxColorComboBox;
    dxLayoutSeparatorItem3: TdxLayoutSeparatorItem;
    dxLayoutItem11: TdxLayoutItem;
    btnFont: TcxButton;
    dxLayoutItem12: TdxLayoutItem;
    edFont: TcxTextEdit;
    dxLayoutItem13: TdxLayoutItem;
    lblExpanding: TcxLabel;
    dxLayoutItem14: TdxLayoutItem;
    lblMiscellaneous: TcxLabel;
    dxLayoutItem15: TdxLayoutItem;
    Image1: TcxImage;
    dxLayoutItem16: TdxLayoutItem;
    Image2: TcxImage;
    dxLayoutItem17: TdxLayoutItem;
    chbxAutoNodesExpand: TcxCheckBox;
    dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup;
    lblExpandLevel: TdxLayoutItem;
    seExpandLevel: TcxSpinEdit;
    dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup;
    dxLayoutItem18: TdxLayoutItem;
    chbxAutoWidth: TcxCheckBox;
    dxLayoutItem19: TdxLayoutItem;
    chbxNodeAutoHeight: TcxCheckBox;
    dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup;
    procedure btnFontClick(Sender: TObject);
    procedure ccbxColorChange(Sender: TObject);
    procedure chbxAutoNodesExpandClick(Sender: TObject);
    procedure chbxAutoWidthClick(Sender: TObject);
    procedure chbxNodeAutoHeightClick(Sender: TObject);
    procedure chbxShowClick(Sender: TObject);
    procedure chbxTransparentClick(Sender: TObject);
    procedure ExpandLevelChange(Sender: TObject);
    procedure lblColorClick(Sender: TObject);
    procedure lblExpandLevelClick(Sender: TObject);
    procedure stTransparentClick(Sender: TObject);
  private
    FPreviewBox: TCustomControl;
    function GetReportLink: TCustomdxNativeTreeViewReportLink;
    procedure CreateControls;
    procedure pbxPreviewPaint(Sender: TObject);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
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
    property ReportLink: TCustomdxNativeTreeViewReportLink read GetReportLink;
  end;

const
  dxDefaultTreeViewPaintOptions: TdxTreeViewPaintOptions =
    [tvpoStateImages, tvpoImages, tvpoTreeLines, tvpoButtons];

implementation

{$R *.DFM}

uses
  ClipBrd, SysUtils, Math, dxPrnDev, dxPSUtl, dxExtCtrls, cxGeometry, dxPSImgs, dxCore, dxDPIAwareUtils;

const
  dxDefaultTVLinkWidth = 400;

type
  TCustomTreeViewAccess = class(TCustomTreeView);

{ TreeView Helpers }

function TreeViewGetImages(AControl: TCustomTreeView): TCustomImageList;
begin
  Result := TCustomTreeViewAccess(AControl).Images;
end;

function TreeViewGetItems(AControl: TCustomTreeView): TTreeNodes;
begin
  Result := TCustomTreeViewAccess(AControl).Items;
end;

function TreeViewGetStateImages(AControl: TCustomTreeView): TCustomImageList;
begin
  Result := TCustomTreeViewAccess(AControl).StateImages;
end;

{ Utilities }

function GetDesignerString(AnIndex: Integer): string;
begin
  case AnIndex of
    0: Result := cxGetResourceString(@sdxTechnicalDepartment);
    1: Result := cxGetResourceString(@sdxSoftwareDepartment);
    2: Result := cxGetResourceString(@sdxSystemProgrammers);
    3: Result := cxGetResourceString(@sdxEndUserProgrammers);
    4: Result := cxGetResourceString(@sdxBetaTesters);
  else
    Result := cxGetResourceString(@sdxHumanResourceDepartment);
  end;
end;

{ TCustomdxNativeTreeViewReportLink }

constructor TCustomdxNativeTreeViewReportLink.Create(AOwner: TComponent);
begin
  inherited;
  InternalRestoreDefaults;
  LinkModified(False);
  FNodeList := TList.Create;
  CreateImages;
end;

destructor TCustomdxNativeTreeViewReportLink.Destroy;
begin
  DeleteImages;
  FreeAndNil(FNodeList);
  inherited;
end;

procedure TCustomdxNativeTreeViewReportLink.Assign(Source: TPersistent);
begin
  if Source is TCustomdxNativeTreeViewReportLink then
    with TCustomdxNativeTreeViewReportLink(Source) do
    begin
      Self.FIsWidthAssigned := FIsWidthAssigned;
      Self.AutoNodesExpand := AutoNodesExpand;
      Self.AutoWidth := AutoWidth;
      Self.ExpandLevel := ExpandLevel;
      Self.ExplicitTreeViewExpand := ExplicitTreeViewExpand;
      Self.GridLineColor := GridLineColor;
      Self.Options := Options;
      Self.TreeLineColor := TreeLineColor;
      Self.Width := Width;
    end;
  inherited;
end;

class function TCustomdxNativeTreeViewReportLink.Aggregable: Boolean;
begin
  Result := True;
end;

function TCustomdxNativeTreeViewReportLink.DefaultWidth: Integer;
begin
  Result := dxDefaultTVLinkWidth;
end;

procedure TCustomdxNativeTreeViewReportLink.Initialize;
begin
  inherited;
  FreeAndNil(FNodeHeights);
end;

procedure TCustomdxNativeTreeViewReportLink.AssignData(ANode: HTREEITEM;
  ADataItem: TAbstractdxReportCellData);
begin
  ADataItem.Data := TdxNativeInt(ANode);
  ADataItem.CellSides := GetCellSides(ANode);
  ADataItem.Transparent := True;
  TdxReportCellString(ADataItem).Text := TV_GetNodeText(ANode);
  TdxReportCellString(ADataItem).Multiline := NodeAutoHeight and not AutoWidth;
end;

procedure TCustomdxNativeTreeViewReportLink.ConstructReport(AReportCells: TdxReportCells);

  procedure InsertNodeIndent(AParentCell: TdxReportCell; ANode: HTREEITEM; var ALeftOffset: Integer);

    function GetIndentShowButton(AtLevel: Integer; ANode: HTREEITEM): Boolean;
    begin
      Result := (tvpoButtons in Options) and (TV_GetNodeParentAtLevel(ANode, AtLevel) = ANode) and
        TV_GetNodeHasChildren(ANode);
    end;

    function GetIndentButtonExpanded(ANode: HTREEITEM): Boolean;
    begin
      if AutoNodesExpand then
        if ExplicitTreeViewExpand then
          Result := TV_IsNodeExpanded(ANode) and ((ExpandLevel = -1) or (ExpandLevel > TV_GetNodeLevel(ANode)))
        else
          Result := TV_GetNodeCount(ANode) <> 0
      else
        Result := TV_IsNodeExpanded(ANode);
    end;

    function GetIndentTreeLineMode(AtLevel: Integer; ANode: HTREEITEM): TdxPSTreeLineMode;
    var
      Parent: HTREEITEM;
    begin
      if tvpoTreeLines in Options then
      begin
        Parent := TV_GetNodeParentAtLevel(ANode, AtLevel);
        if Parent = ANode then
          if TV_IsNodeLastChild(ANode) then
	    Result := tlmBottomRightCorner
          else
            if not TV_IsNodeParentExists(ANode) and TV_IsNodeFirstChild(ANode) then
              Result := tlmTopRightCorner
            else
	      Result := tlmCross
        else
          if TV_IsNodeLastChild(Parent) then
            Result := tlmNone
          else
            Result := tlmVertical;
      end
      else
        Result := tlmNone
    end;

    function GetIndentCellSides(I, ALevel: Integer; ANode: HTREEITEM): TdxCellSides;
    begin
      Result := [];
      if IsDrawGrid then
      begin
        if (I <> 0) or IsDrawBorder then
          Include(Result, csLeft);
        if I <> 0 then
          Include(Result, csRight);

{2.0}  if (I = ALevel) or (not TV_IsNodeParentExists(ANode) and (not IsFirstNode(ANode) or IsDrawBorder)) then
          Include(Result, csTop);

        if (TV_GetNodeCount(ANode) = 0) or (not TV_IsNodeExpanded(ANode) and not AutoNodesExpand) then
        begin
          if I = ALevel then
          begin
            if not IsFirstNode(ANode) or IsDrawBorder then
              Include(Result, csTop);
            if not IsLastNode(ANode) or IsDrawBorder then
              Include(Result, csBottom);
          end
          else
            if TV_IsNodeLastChild(ANode) then
              Include(Result, csBottom);
        end;
      end;
      if IsDrawBorder then
      begin
        if I = 0 then
          Include(Result, csLeft);
        if IsFirstNode(ANode) then
          Include(Result, csTop);
        if IsLastNode(ANode) then
          Include(Result, csBottom);
      end;
    end;

  var
    MaxLevel, I: Integer;
  begin
    MaxLevel := TV_GetNodeLevel(ANode) - Ord(not TV_IsShowRoot);
    for I := 0 to MaxLevel do
    begin
      with TdxReportCellExpandButton.Create(AParentCell) do
      begin
        BoundsRect := Bounds(ALeftOffset, 0, FIndent, Parent.Height);
        CellSides := GetIndentCellSides(I, MaxLevel, ANode);
        ShowButton := GetIndentShowButton(MaxLevel - I, ANode);
        if ShowButton then
        begin
          ButtonExpanded := GetIndentButtonExpanded(ANode);
          ButtonSize := 9;
        end;
        TreeLineMode := GetIndentTreeLineMode(MaxLevel - I, ANode);
      end;
      Inc(ALeftOffset, FIndent);
    end;
  end;

  procedure InsertImage(AParentCell: TdxReportCell; ANode: HTREEITEM;
    AImageList: TCustomImageList; AImageIndex: Integer; var ALeftOffset: Integer);

    function GetImageCellSides(ANode: HTREEITEM; AnItem: TdxReportVisualItem): TdxCellSides;
    begin
      Result := [];
      if IsDrawBorder then
      begin
        if (TV_GetNodeLevel(ANode) = 0) and AnItem.IsFirstItem then
          Include(Result, csLeft);
        if IsFirstNode(ANode) then
          Include(Result, csTop);
        if IsLastNode(ANode) then
          Include(Result, csBottom);
      end;
      if IsDrawGrid then
      begin
        if not IsFirstNode(ANode) or IsDrawBorder then
          Include(Result, csTop);
        if not IsLastNode(ANode) or IsDrawBorder then
          Include(Result, csBottom);
      end;
    end;

  var
    Item: TdxReportCellGraphic;
  begin
    Item := TdxReportCellGraphic.Create(AParentCell);
    with Item do
    begin
      CellSides := GetImageCellSides(ANode, Item);
      DrawMode := gdmCenter;
      Transparent := True;

      BoundsRect := Bounds(ALeftOffset, 0, 0, Parent.Height);
      Width := AImageList.Width;
      if Width < FIndent then Width := FIndent;

      ImageList := AImageList;
      ImageIndex := AImageIndex;
      Inc(ALeftOffset, Width);
    end;
  end;

  procedure ProcessNode(ANode: HTREEITEM; Index: Integer);
  var
    LeftOffset, V: Integer;
    Cell: TdxReportCell;
    PrevSibl: TdxReportVisualItem;
    DataItem: TAbstractdxReportCellData;
  begin
    Cell := TdxReportCell.Create(AReportCells.Cells);
    with Cell do
    begin
      PrevSibl := TdxReportVisualItem(getPrevSibling);
      V := 0;
      if PrevSibl <> nil then
        V := PrevSibl.BoundsRect.Bottom;
      BoundsRect := Bounds(0, V, FActualWidth, GetNodeHeight(Index));
      CellSides := [];
      if IsDrawBorder then
      begin
        CellSides := CellSides + [csLeft, csRight];
        if IsFirstNode(ANode) then
          CellSides := CellSides + [csTop];
        if IsLastNode(ANode) then
          CellSides := CellSides + [csBottom];
      end;

      Color := Self.Color;
      Transparent := Self.Transparent;
    end;

    LeftOffset := 0;
    InsertNodeIndent(Cell, ANode, LeftOffset);
    if IsDrawNodeStateImages(ANode) then
      InsertImage(Cell, ANode, StateImages, TV_GetNodeStateIndex(ANode), LeftOffset);
    if IsDrawNodeImages(ANode) then
      InsertImage(Cell, ANode, Images, TV_GetNodeImageIndex(ANode), LeftOffset);

    DataItem := GetDataClass(ANode).Create(Cell);
    DataItem.BoundsRect := Rect(LeftOffset, 0, FActualWidth, Cell.Height);
    InitializeItem(ANode, DataItem);
  end;

  procedure AdjustCellsWidth;

    function GetRealWidth: Integer;
    var
      W, I: Integer;
      Size: TSize;
      Item: TdxReportVisualItem;
      Node: HTREEITEM;
    begin
      Result := 0;
      FScreenCanvas.SaveState;
      try
        FScreenCanvas.Font :=Font;
        for I := 0 to AReportCells.Cells.CellCount - 1 do
        begin
          W := 0;
          Node := HTREEITEM(FNodeList[I]);
          Inc(W, (TV_GetNodeLevel(Node) + Ord(TV_IsShowRoot)) * FIndent);

          if IsDrawNodeStateImages(Node) then Inc(W, StateImages.Width);
          if IsDrawNodeImages(Node) then Inc(W, Images.Width);

          with AReportCells.Cells[I] do
            Item := DataItems[DataItemCount - 1];
          Size := FScreenCanvas.TextSize(TdxReportCellString(Item).Text);
          Inc(W, Size.cX + 10);
          if W > Result then Result := W;
        end;
      finally
        FScreenCanvas.RestoreState;
      end;
    end;

  var
    I: Integer;
    Item: TdxReportVisualItem;
  begin
    FActualWidth := GetRealWidth;
    for I := 0 to AReportCells.Cells.CellCount - 1 do
    begin
      Item := AReportCells.Cells[I];
      Item.Width := FActualWidth;
      with TdxReportCell(Item) do
        Item := DataItems[DataItemCount - 1];
      with Item, BoundsRect do
        BoundsRect := Rect(Left, Top, FActualWidth, Bottom);
    end;
  end;

  procedure AddNodes;

    procedure AddNode(ANode: HTREEITEM);
    begin
      FNodeList.Add(ANode);
      if TV_IsNodeExpanded(ANode) or AutoNodesExpand then
      begin
        ANode := TreeView_GetChild(TreeViewHandle, ANode);
        while ANode <> nil do
        begin
          if (ExpandLevel = -1) or (TV_GetNodeLevel(ANode) <= ExpandLevel) then
            AddNode(ANode);
          ANode := TreeView_GetNextSibling(TreeViewHandle, ANode);
        end;
      end;
    end;

  var
    Node: HTREEITEM;
  begin
    FNodeList.Clear;
    if TreeView_GetCount(TreeViewHandle) = 0 then Exit;

    if AutoNodesExpand and ExplicitTreeViewExpand then
      DoExplicitTreeViewExpand(ExpandLevel);

    Node := TreeView_GetRoot(TreeViewHandle);
    while Node <> nil do
    begin
      AddNode(Node);
      Node := TreeView_GetNextSibling(TreeViewHandle, Node);
    end;
  end;

  procedure IterateNodes;
  var
    I: Integer;
  begin
    with FNodeList do
      for I := 0 to Count - 1 do
      begin
        ProcessNode(HTREEITEM(Items[I]), I);
        AReportCells.DoProgress(MulDiv(I, 100, Count));
        if AbortBuilding then Break;
      end;
  end;

begin
  if TreeViewHandle = 0 then Exit;
  inherited;

  AddNodes;
  if FNodeList.Count = 0 then Exit;

  AReportCells.Cells.FontIndex := 0;
  AReportCells.Cells.Transparent := Transparent;
  AReportCells.Cells.Color := Color;
  AReportCells.BorderColor := GridLineColor;
  AReportCells.ExpandButtonBorderColor := clGrayText;
  AReportCells.TreeLineColor := TreeLineColor;

  PrepareContruct;
  try
    IterateNodes;
    if not AbortBuilding and AutoWidth then
      AdjustCellsWidth;
  finally
    UnprepareContruct;
  end;

  if not AbortBuilding then
    with AReportCells do
      Cells.BoundsRect := Rect(0, 0, FActualWidth, Cells.LastCell.BoundsRect.Bottom);
end;

procedure TCustomdxNativeTreeViewReportLink.GetImageLists(AProc: TdxPSGetImageListProc);
begin
  inherited;
  AProc(Images);
  AProc(StateImages);
end;

procedure TCustomdxNativeTreeViewReportLink.InitializeItem(ANode: HTREEITEM;
  ADataItem: TAbstractdxReportCellData);
begin
  AssignData(ANode, ADataItem);
end;

procedure TCustomdxNativeTreeViewReportLink.InternalRestoreDefaults;
begin
  inherited;
  FAutoNodesExpand := False;
  FAutoWidth := True;
  FExpandLevel := -1;
  FExplicitTreeViewExpand := False;
  FGridLineColor := dxDefaultGridLineColor;
  FTreeLineColor := dxDefaultTreeLineColor;
  FNodeAutoHeight := False;
  FOptions := dxDefaultTreeViewPaintOptions;
  FIsWidthAssigned := False;
end;

procedure TCustomdxNativeTreeViewReportLink.MakeDelimiters(AReportCells: TdxReportCells;
  AHorzDelimiters, AVertDelimiters: TList);
var
  I, V: Integer;
begin
  inherited;
  if UseVertDelimiters then
    with AReportCells, Cells do
      for I := 1 to CellCount - 2 do
      begin
        V := Cells[I].BoundsRect.Bottom;
        AVertDelimiters.Add(TObject(V));
      end;
end;

procedure TCustomdxNativeTreeViewReportLink.PrepareContruct;
var
  R: TRect;
begin
  FScreenCanvas := TdxPSReportRenderScreenCanvas.Create;
  SendMessage(TreeViewHandle, WM_SETREDRAW, 0, 0);

  ClearImages;

  FIndent := TreeView_GetIndent(TreeViewHandle);
  if not Odd(FIndent) then Inc(FIndent);

  FNodeHeight := Renderer.CalcTextPatternHeight(FScreenCanvas, Font);

  if CanDrawStateImages and (FNodeHeight < StateImages.Height + 2) then
    FNodeHeight := StateImages.Height + 2;
  if CanDrawImages and (FNodeHeight < Images.Height + 2) then
    FNodeHeight := Images.Height + 2;

  if AutoWidth then
    FActualWidth := 0
  else
    if NodeAutoHeight then
    begin
      GetClientRect(TreeViewHandle, R);
      FActualWidth := R.Right - R.Left;
    end
    else
      FActualWidth := Self.Width;

  if NodeAutoHeight and not AutoWidth then
    CalcNodesHeight;
end;

procedure TCustomdxNativeTreeViewReportLink.UnprepareContruct;
begin
  FreeAndNil(FNodeHeights);
  FreeAndNil(FScreenCanvas);

  SendMessage(TreeViewHandle, WM_SETREDRAW, 1, 0);
  InvalidateRect(TreeViewHandle, nil, False);
end;

function TCustomdxNativeTreeViewReportLink.GetDataClass(ANode: HTREEITEM): TdxReportCellDataClass;
begin
  Result := TdxReportCellString;
end;

function TCustomdxNativeTreeViewReportLink.CheckImages: Boolean;
begin
  Result := HasImages and (Images.Width <> 0) and (Images.Height <> 0);
end;

function TCustomdxNativeTreeViewReportLink.CheckStateImages: Boolean;
begin
  Result := HasStateImages and (StateImages.Width <> 0) and (StateImages.Height <> 0);
end;

procedure TCustomdxNativeTreeViewReportLink.ClearImages;
begin
  if not IsAggregated then
  begin
    if FImages <> nil then FImages.Clear;
    if FStateImages <> nil then FStateImages.Clear;
  end;
end;

procedure TCustomdxNativeTreeViewReportLink.CreateImages;
begin
  FImages := TImageList.Create(nil);
  FStateImages := TImageList.Create(Self);
end;

procedure TCustomdxNativeTreeViewReportLink.DeleteImages;
begin
  FreeAndNil(FStateImages);
  FreeAndNil(FImages);
end;

function TCustomdxNativeTreeViewReportLink.GetImages: TCustomImageList;
begin
  if FImages.Count = 0 then
    CopyImages(TreeView_GetImageList(TreeViewHandle, TVSIL_NORMAL), FImages);
  Result := FImages;
end;

function TCustomdxNativeTreeViewReportLink.GetStateImages: TCustomImageList;
begin
  if FStateImages.Count = 0 then
    CopyImages(TreeView_GetImageList(TreeViewHandle, TVSIL_STATE), FStateImages);
  Result := FStateImages;
end;

function TCustomdxNativeTreeViewReportLink.HasImages: Boolean;
begin
  Result := TreeView_GetImageList(TreeViewHandle, TVSIL_NORMAL) <> 0;
end;

function TCustomdxNativeTreeViewReportLink.HasStateImages: Boolean;
begin
  Result := TreeView_GetImageList(TreeViewHandle, TVSIL_STATE) <> 0;
end;

procedure TCustomdxNativeTreeViewReportLink.DoExplicitTreeViewExpand(ALevel: Integer = -1);

  procedure ExpandNode(ANode: HTREEITEM; ALevel: Integer);
  begin
    TreeView_Expand(TreeViewHandle, ANode, TVE_EXPAND);
    if ALevel <> -1 then
    begin
      Dec(ALevel);
      if ALevel = -1 then
        Exit;
    end;

    ANode := TreeView_GetChild(TreeViewHandle, ANode);
    while ANode <> nil do
    begin
      ExpandNode(ANode, ALevel);
      ANode := TreeView_GetNextSibling(TreeViewHandle, ANode);
    end;
  end;

var
  Node: HTREEITEM;
begin
  ALevel := Max(ALevel, -1);
  Node := TreeView_GetRoot(TreeViewHandle);
  if ALevel <> -1 then
  begin
    Dec(ALevel);
    if ALevel = -1 then
      Exit;
  end;

  while Node <> nil do
  begin
    ExpandNode(Node, ALevel);
    Node := TreeView_GetNextSibling(TreeViewHandle, Node);
  end;
end;

{ IdxPSNativeWin32ControlHandleSupport }

function TCustomdxNativeTreeViewReportLink.GetNativeHandle: THandle;
begin
  Result := TreeViewHandle;
end;

procedure TCustomdxNativeTreeViewReportLink.SetNativeHandle(Value: THandle);
begin
  TreeViewHandle := Value;
end;

function TCustomdxNativeTreeViewReportLink.TV_GetNodeCount(AItem: HTREEITEM): Integer;
var
  Child: HTREEITEM;
begin
  Result := 0;
  Child := TreeView_GetChild(TreeViewHandle, AItem);
  while Child <> nil do
  begin
    Inc(Result);
    Child := TreeView_GetNextSibling(TreeViewHandle, Child);
  end;
end;

function TCustomdxNativeTreeViewReportLink.TV_GetNodeHasChildren(AItem: HTREEITEM): Boolean;
var
  Item: TTVItem;
begin
  FillChar(Item, Sizeof(Item), 0);
  Item.Mask := TVIF_CHILDREN;
  Item.hItem := AItem;
  TreeView_GetItem(TreeViewHandle, Item);
  Result := Item.cChildren <> 0;
end;

function TCustomdxNativeTreeViewReportLink.TV_GetNodeImageIndex(AItem: HTREEITEM): Integer;
var
  Item: TTVItem;
begin
  FillChar(Item, Sizeof(Item), 0);
  Item.Mask := TVIF_IMAGE;
  Item.hItem := AItem;
  TreeView_GetItem(TreeViewHandle, Item);
  Result := Item.iImage;
end;

function TCustomdxNativeTreeViewReportLink.TV_GetNodeLevel(AItem: HTREEITEM): Integer;
var
  Parent: HTREEITEM;
begin
  Result := 0;
  Parent := TV_GetNodeParent(AItem);
  while Parent <> nil do
  begin
    Inc(Result);
    Parent := TV_GetNodeParent(Parent);
  end;
end;

function TCustomdxNativeTreeViewReportLink.TV_GetNodeParent(AItem: HTREEITEM): HTREEITEM;
begin
  Result := TreeView_GetParent(TreeViewHandle, AItem);
end;

function TCustomdxNativeTreeViewReportLink.TV_GetNodeParentAtLevel(AItem: HTREEITEM;
  ALevel: Integer): HTREEITEM;
begin
  Result := AItem;
  while ALevel > 0 do
  begin
    Result := TV_GetNodeParent(Result);
    Dec(ALevel);
  end;
end;

function TCustomdxNativeTreeViewReportLink.TV_GetNodeStateIndex(AItem: HTREEITEM): Integer;
var
  Item: TTVItem;
begin
  FillChar(Item, SizeOf(Item), 0);
  Item.Mask := TVIF_IMAGE;
  Item.StateMask := TVIS_STATEIMAGEMASK;
  Item.hItem := AItem;
  TreeView_GetItem(TreeViewHandle, Item);
  Result := (Item.State and TVIS_STATEIMAGEMASK) shr 12;
  if Result = 0 then Result := -1;
end;

function TCustomdxNativeTreeViewReportLink.TV_GetNodeText(AItem: HTREEITEM): string;
const
  BufferLength = 4096;
var
  Item: TTVItem;
  Buffer: array[0..BufferLength - 1] of Char;
  PBuffer: Pointer;
begin
  FillChar(Item, Sizeof(Item), 0);
    PBuffer := @Buffer;
    Item.Mask := TVIF_TEXT;
    Item.hItem := AItem;
    Item.pszText := PBuffer;
    Item.cchTextMax := BufferLength;
    TreeView_GetItem(TreeViewHandle, Item);
    Result := (Item.pszText);
end;

function TCustomdxNativeTreeViewReportLink.TV_IsNodeExpanded(AItem: HTREEITEM): Boolean;
var
  Item: TTVItem;
begin
  FillChar(Item, Sizeof(Item), 0);
  Item.Mask := TVIF_STATE;
  Item.hItem := AItem;
  TreeView_GetItem(TreeViewHandle, Item);
  Result := Item.State and TVIS_EXPANDED = TVIS_EXPANDED;
end;

function TCustomdxNativeTreeViewReportLink.TV_IsNodeParentExists(AItem: HTREEITEM): Boolean;
begin
  Result := TV_GetNodeParent(AItem) <> nil;
end;

function TCustomdxNativeTreeViewReportLink.TV_IsNodeFirstChild(AItem: HTREEITEM): Boolean;
begin
  Result := TreeView_GetPrevSibling(TreeViewHandle, AItem) = nil;
end;

function TCustomdxNativeTreeViewReportLink.TV_IsNodeLastChild(AItem: HTREEITEM): Boolean;
begin
  Result := TreeView_GetNextSibling(TreeViewHandle, AItem) = nil;
end;

function TCustomdxNativeTreeViewReportLink.TV_IsShowButtons: Boolean;
begin
  Result := GetWindowLong(TreeViewHandle, GWL_STYLE) and TVS_HASBUTTONS = TVS_HASBUTTONS;
end;

function TCustomdxNativeTreeViewReportLink.TV_IsShowLines: Boolean;
begin
  Result := GetWindowLong(TreeViewHandle, GWL_STYLE) and TVS_HASLINES = TVS_HASLINES;
end;

function TCustomdxNativeTreeViewReportLink.TV_IsShowRoot: Boolean;
begin
  Result := GetWindowLong(TreeViewHandle, GWL_STYLE) and TVS_LINESATROOT = TVS_LINESATROOT;
end;

function TCustomdxNativeTreeViewReportLink.GetOptions: TdxTreeViewPaintOptions;
begin
  Result := FOptions;
end;

function TCustomdxNativeTreeViewReportLink.GetWidth: Integer;
begin
  if FIsWidthAssigned then
    Result := FWidth
  else
    Result := DefaultWidth;
end;

function TCustomdxNativeTreeViewReportLink.IsWidthStored: Boolean;
begin
  Result := FIsWidthAssigned and (Width <> DefaultWidth);
end;

procedure TCustomdxNativeTreeViewReportLink.SetAutoNodesExpand(Value: Boolean);
begin
  if FAutoNodesExpand <> Value then
  begin
    FAutoNodesExpand := Value;
    LinkModified(True);
  end;
end;

procedure TCustomdxNativeTreeViewReportLink.SetAutoWidth(Value: Boolean);
begin
  if FAutoWidth <> Value then
  begin
    FAutoWidth := Value;
    LinkModified(True);
  end;
end;

procedure TCustomdxNativeTreeViewReportLink.SetExpandLevel(Value: Integer);
begin
  if Value < -1 then Value := -1;
  if FExpandLevel <> Value then
  begin
    FExpandLevel := Value;
    LinkModified(True);
  end;
end;

procedure TCustomdxNativeTreeViewReportLink.SetExplicitTreeViewExpand(Value: Boolean);
begin
  FExplicitTreeViewExpand := Value;
end;

procedure TCustomdxNativeTreeViewReportLink.SetGridLineColor(Value: TColor);
begin
  if FGridLineColor <> Value then
  begin
    FGridLineColor := Value;
    LinkModified(True);
  end;
end;

procedure TCustomdxNativeTreeViewReportLink.SetNodeAutoHeight(Value: Boolean);
begin
  if FNodeAutoHeight <> Value then
  begin
    FNodeAutoHeight := Value;
    LinkModified(True);
  end;
end;

procedure TCustomdxNativeTreeViewReportLink.SetOptions(Value: TdxTreeViewPaintOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    LinkModified(True);
  end;
end;

procedure TCustomdxNativeTreeViewReportLink.SetTreeLineColor(Value: TColor);
begin
  if FTreeLineColor <> Value then
  begin
    FTreeLineColor := Value;
    LinkModified(True);
  end;
end;

procedure TCustomdxNativeTreeViewReportLink.SetWidth(Value: Integer);
begin
  if FWidth <> Value then
  begin
    FIsWidthAssigned := True;
    FWidth := Value;
    if not AutoWidth then LinkModified(True);
  end;
end;

procedure TCustomdxNativeTreeViewReportLink.CalcNodesHeight;
var
  I, W, H: Integer;
  Node: HTREEITEM;
  R: TRect;
begin
  FNodeHeights := TList.Create;
  FNodeHeights.Capacity := FNodeList.Count;
  for I := 0 to FNodeList.Count - 1 do
  begin
    Node := HTREEITEM(FNodeList[I]);
    W := FActualWidth - (TV_GetNodeLevel(Node) + 1) * FIndent;
    if CanDrawStateImages then Dec(W, StateImages.Width + 2);
    if CanDrawImages then Dec(W, Images.Width + 2);

    R := Rect(0, 0, W, 0);
    H := Renderer.CalcTextRect(FScreenCanvas, TV_GetNodeText(Node), R, False, nil);
    if H < FNodeHeight then H := FNodeHeight;
    if not Odd(H) then Inc(H);
    FNodeHeights.Add(Pointer(H));
  end;
end;

function TCustomdxNativeTreeViewReportLink.CanDrawImages: Boolean;
begin
  Result := (tvpoImages in Options) and CheckImages and Images.HandleAllocated;
end;

function TCustomdxNativeTreeViewReportLink.CanDrawStateImages: Boolean;
begin
  Result := (tvpoStateImages in Options) and CheckStateImages and StateImages.HandleAllocated;
end;

function TCustomdxNativeTreeViewReportLink.GetCellSides(ANode: HTREEITEM): TdxCellSides;
var
  IsAnyImagesExists: Boolean;
begin
  Result := [];
  IsAnyImagesExists := IsDrawNodeImages(ANode) or IsDrawNodeStateImages(ANode);
  if IsDrawGrid then
  begin
    if not IsFirstNode(ANode) or IsDrawBorder then
      Include(Result, csTop);
    if not IsLastNode(ANode) or IsDrawBorder then
      Include(Result, csBottom);
    if IsDrawBorder and (TV_GetNodeLevel(ANode) = 0) and not IsAnyImagesExists then
      Include(Result, csLeft);
  end;
  if IsDrawBorder then
  begin
    Include(Result, csRight);
    if (TV_GetNodeLevel(ANode) = 0) and not IsAnyImagesExists then
      Include(Result, csLeft);
    if IsFirstNode(ANode) then
      Include(Result, csTop);
    if IsLastNode(ANode) then
      Include(Result, csBottom);
  end;
end;

function TCustomdxNativeTreeViewReportLink.GetNodeHeight(Index: Integer): Integer;
begin
  if FNodeHeights <> nil then
    Result := Integer(FNodeHeights[Index])
  else
    Result := FNodeHeight;
end;

function TCustomdxNativeTreeViewReportLink.IsDrawBorder: Boolean;
begin
  Result := tvpoBorder in Options;
end;

function TCustomdxNativeTreeViewReportLink.IsDrawGrid: Boolean;
begin
  Result := tvpoGrid in Options;
end;

function TCustomdxNativeTreeViewReportLink.IsDrawNodeImages(ANode: HTREEITEM): Boolean;
begin
  Result := CanDrawImages and IsValidNodeImageIndex(ANode);
end;

function TCustomdxNativeTreeViewReportLink.IsDrawNodeStateImages(ANode: HTREEITEM): Boolean;
begin
  Result := CanDrawStateImages and IsValidNodeStateIndex(ANode);
end;

function TCustomdxNativeTreeViewReportLink.IsFirstNode(ANode: HTREEITEM): Boolean;
begin
  Result := TObject(ANode) = FNodeList.First;
end;

function TCustomdxNativeTreeViewReportLink.IsLastNode(ANode: HTREEITEM): Boolean;
begin
  Result := TObject(ANode) = FNodeList.Last;
end;

function TCustomdxNativeTreeViewReportLink.IsValidNodeImageIndex(ANode: HTREEITEM): Boolean;
var
  ImageIndex: Integer;
begin
  ImageIndex := TV_GetNodeImageIndex(ANode);
  Result := (ImageIndex > -1) and (ImageIndex < Images.Count);
end;

function TCustomdxNativeTreeViewReportLink.IsValidNodeStateIndex(ANode: HTREEITEM): Boolean;
var
  StateIndex: Integer;
begin
  StateIndex := TV_GetNodeStateIndex(ANode);
  Result := (StateIndex > -1) and (StateIndex < StateImages.Count);
end;

{ TdxNativeTreeViewReportLink }

function TdxNativeTreeViewReportLink.DataProviderPresent: Boolean;
begin
  if DataSource = rldsComponent then
    Result := IsWindow(TreeViewHandle)
  else
    Result := inherited DataProviderPresent;
end;

procedure TdxNativeTreeViewReportLink.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent is TWinControl) and TWinControl(Component).HandleAllocated and
    (TWinControl(Component).Handle = TreeViewHandle) then
    TreeViewHandle := 0;
end;

function TdxNativeTreeViewReportLink.GetDesignerClass: TdxReportLinkDesignWindowClass;
begin
  Result := TdxfmTVReportLinkDesignWindow;
end;

function TdxNativeTreeViewReportLink.GetTreeViewHandle: THandle;
begin
  Result := FTreeViewHandle;
end;

procedure TdxNativeTreeViewReportLink.SetTreeViewHandle(Value: THandle);
begin
  if (FTreeViewHandle <> Value) and IsWindow(Value) then
  begin
    FTreeViewHandle := Value;
    LinkModified(True);
  end;
end;

{ TCustomdxTreeViewReportLink }

constructor TCustomdxTreeViewReportLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSaveFont := TFont.Create;
  FSaveFont.OnChange := CustomDrawFontChanged;
end;

destructor TCustomdxTreeViewReportLink.Destroy;
begin
  FSaveFont.Free;
  inherited Destroy;
end;

procedure TCustomdxTreeViewReportLink.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TCustomdxTreeViewReportLink then
    SupportedCustomDraw := TCustomdxTreeViewReportLink(Source).SupportedCustomDraw;
end;

class function TCustomdxTreeViewReportLink.Aggregable: Boolean;
begin
  Result := True;
end;

procedure TCustomdxTreeViewReportLink.InitializeItem(ANode: HTREEITEM;
  ADataItem: TAbstractdxReportCellData);
begin
  inherited;
  DoInitializeNode(TreeViewGetItems(CustomTreeView).GetNode(ANode), ADataItem);
end;

procedure TCustomdxTreeViewReportLink.InternalRestoreDefaults;
begin
  inherited InternalRestoreDefaults;
  FSupportedCustomDraw := False;
end;

function TCustomdxTreeViewReportLink.IsSupportedCustomDraw(Item: TAbstractdxReportCellData): Boolean;
begin
  Result := SupportedCustomDraw;
  if Result and (Item <> nil) then
    Result := (Item.Data <> 0) and Assigned(FOnCustomDrawNode);
end;

function TCustomdxTreeViewReportLink.GetImages: TCustomImageList;
begin
  if IsAggregated then
    Result := TreeViewGetImages(CustomTreeView)
  else
    Result := inherited GetImages;
end;

function TCustomdxTreeViewReportLink.GetStateImages: TCustomImageList;
begin
  if IsAggregated then
    Result := TreeViewGetStateImages(CustomTreeView)
  else
    Result := inherited GetStateImages;
end;

function TCustomdxTreeViewReportLink.GetCustomTreeView: TCustomTreeView;
begin
  Result := TCustomTreeView(Component);
end;

function TCustomdxTreeViewReportLink.GetTreeViewHandle: THandle;
begin
  Result := CustomTreeView.Handle;
end;

procedure TCustomdxTreeViewReportLink.SetTreeViewHandle(Value: THandle);
begin

end;

{procedure TCustomdxTreeViewReportLink.DoExplicitTreeViewExpand;
begin
  TreeView.FullExpand;
end;}

procedure TCustomdxTreeViewReportLink.CustomDraw(AItem: TAbstractdxReportCellData;
  ACanvas: TCanvas; ABoundsRect, AClientRect: TRect; var ADone: Boolean);
var
  AColor: TColor;
  AText: string;
  ATextAlignX: TcxTextAlignX;
  ATextAlignY: TcxTextAlignY;
begin
  if AItem.Data = 0 then Exit;
  with TdxReportCellString(AItem) do
  begin
    AColor := ColorToRGB(Color);
    if Transparent then AColor := clNone;
    FSaveFont.Assign(Font);
    FCustomDrawFontChanged := False;
    AText := Text;
    ATextAlignX := TextAlignX;
    ATextAlignY := TextAlignY;
    DoCustomDrawNode(TreeViewGetItems(CustomTreeView).GetNode(HTREEITEM(Data)),
      ACanvas, ABoundsRect, AClientRect, AText, FSaveFont, AColor, ATextAlignX, ATextAlignY, ADone);
    if not ADone then
    begin
      if FCustomDrawFontChanged then
      begin
        SelectObject(ACanvas.Handle, FSaveFont.Handle);
        SetTextColor(ACanvas.Handle, ColorToRGB(FSaveFont.Color));
        FontIndex := -1;
      end;
      if AColor <> clNone then
      begin
        Color := AColor;
        Transparent := False;
      end;
      Text := AText;
      TextAlignX := ATextAlignX;
      TextAlignY := ATextAlignY;
    end;
  end;
end;

procedure TCustomdxTreeViewReportLink.DoCustomDrawNode(ANode: TTreeNode;
  ACanvas: TCanvas; ABoundsRect, AClientRect: TRect; var AText: string; AFont: TFont;
  var AColor: TColor;  var ATextAlignX: TcxTextAlignX; var ATextAlignY: TcxTextAlignY;
  var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawNode) then
    FOnCustomDrawNode(Self, ANode, ACanvas, ABoundsRect, AClientRect, AText, AFont,
      AColor, ATextAlignX, ATextAlignY, ADone);
end;

procedure TCustomdxTreeViewReportLink.DoInitializeNode(ANode: TTreeNode;
  AnItem: TAbstractdxReportCellData);
begin
  if Assigned(FOnInitializeNode) then
    FOnInitializeNode(Self, ANode, TdxReportCellString(AnItem));
end;

procedure TCustomdxTreeViewReportLink.SetSupportedCustomDraw(Value: Boolean);
begin
  if FSupportedCustomDraw <> Value then
  begin
    FSupportedCustomDraw := Value;
    LinkModified(True);
  end;
end;

procedure TCustomdxTreeViewReportLink.CustomDrawFontChanged(Sender: TObject);
begin
  FCustomDrawFontChanged := True;
end;

{ TdxTreeViewReportLink }

function TdxTreeViewReportLink.GetTreeView: TTreeView;
begin
  Result := TTreeView(Component);
end;

{$IFDEF REGISTERSHELLCTRLS}

{ TCustomdxShellTreeViewReportLink }

procedure TCustomdxShellTreeViewReportLink.CreateImages;
begin
end;

procedure TCustomdxShellTreeViewReportLink.DeleteImages;
begin
end;

function TCustomdxShellTreeViewReportLink.GetImages: TCustomImageList;
begin
  Result := dxPSUtl.ShellSmallImages;
end;

function TCustomdxShellTreeViewReportLink.GetStateImages: TCustomImageList;
begin
  Result := dxPSUtl.ShellSmallImages;
end;

function TCustomdxShellTreeViewReportLink.HasStateImages: Boolean;
begin
  Result := False;
end;

function TCustomdxShellTreeViewReportLink.GetShellTreeView: TShellTreeView;
begin
  Result := TShellTreeView(Component);
end;

{$ENDIF}

{ TdxTVReportLinkDesignWindow }

constructor TdxfmTVReportLinkDesignWindow.Create(AOwner: TComponent);
begin
  HelpContext := dxhcTreeViewReportLinkDesigner;
  inherited;
  CreateControls;
end;

procedure TdxfmTVReportLinkDesignWindow.DoInitialize;
begin
  inherited;
  chbxShowBorders.Checked := ReportLink.IsDrawBorder;
  chbxShowGrid.Checked := ReportLink.IsDrawGrid;

  chbxShowTreeLines.Checked := tvpoTreeLines in ReportLink.Options;
  chbxShowButtons.Checked := tvpoButtons in ReportLink.Options;

  chbxShowStateImages.Checked := tvpoStateImages in ReportLink.Options;
  chbxShowImages.Checked := tvpoImages in ReportLink.Options;

  chbxTransparent.Checked := ReportLink.Transparent;
  ccbxColor.ColorValue := ReportLink.Color;
  ccbxGridLineColor.ColorValue := ReportLink.GridLineColor;
  ccbxTreeLineColor.ColorValue := ReportLink.TreeLineColor;

  FontInfoToText(ReportLink.Font, edFont);

  chbxAutoWidth.Checked := ReportLink.AutoWidth;
  chbxNodeAutoHeight.Checked := ReportLink.NodeAutoHeight;

  chbxAutoNodesExpand.Checked := ReportLink.AutoNodesExpand;
  seExpandLevel.Value := ReportLink.ExpandLevel;
end;

function TdxfmTVReportLinkDesignWindow.GetPreviewHost: TCustomPanel;
begin
  Result := pnlPreview;
end;

procedure TdxfmTVReportLinkDesignWindow.LoadGroupsIcons;
begin
  inherited LoadGroupsIcons;
  dxLoadIconFromResourceEx(imgGrid, IDB_DXPSGROUPICON_SHOW);
  dxLoadIconFromResourceEx(Image1, IDB_DXPSGROUPICON_EXPANDING);
  dxLoadIconFromResourceEx(Image2, IDB_DXPSGROUPICON_MISCELLANEOUS);
end;

procedure TdxfmTVReportLinkDesignWindow.LoadStrings;
begin
  inherited;
  tshOptions.Caption := cxGetResourceString(@sdxOptions);
  tshFonts.Caption := cxGetResourceString(@sdxFonts);
  tshColors.Caption := cxGetResourceString(@sdxColors);
  tshBehaviors.Caption := cxGetResourceString(@sdxBehaviors);
  lblPreview.Caption := DropAmpersand(cxGetResourceString(@sdxPreview));

  lblShow.Caption := cxGetResourceString(@sdxShow);
  chbxShowBorders.Caption := cxGetResourceString(@sdxBorderLines);
  chbxShowGrid.Caption := cxGetResourceString(@sdxGrid);
  chbxShowTreeLines.Caption := cxGetResourceString(@sdxTreeLines);
  chbxShowButtons.Caption := cxGetResourceString(@sdxButtons);
  chbxShowImages.Caption := cxGetResourceString(@sdxImages);
  chbxShowStateImages.Caption := cxGetResourceString(@sdxStateImages);

  lblMiscellaneous.Caption := cxGetResourceString(@sdxMiscellaneous);
  chbxAutoWidth.Caption := cxGetResourceString(@sdxAutoWidth);
  chbxNodeAutoHeight.Caption := cxGetResourceString(@sdxNodeAutoHeight);

  stTransparent.Caption := ' ' + cxGetResourceString(@sdxTransparent) + ' ';
  lblColor.Caption := cxGetResourceString(@sdxColor);
  lblGridLinesColor.Caption := cxGetResourceString(@sdxGridLinesColor);
  lblTreeLinesColor.Caption := cxGetResourceString(@sdxTreeLinesColor);

  btnFont.Caption := cxGetResourceString(@sdxBtnFont);

  lblExpanding.Caption := cxGetResourceString(@sdxExpanding);
  chbxAutoNodesExpand.Caption := cxGetResourceString(@sdxAutoNodesExpand);
  lblExpandLevel.Caption := cxGetResourceString(@sdxExpandLevel);
end;

procedure TdxfmTVReportLinkDesignWindow.PaintPreview(ACanvas: TCanvas; R: TRect);
const
  ItemsCount: Integer = 6;

  procedure DrawBackground(ACanvas: TcxCanvas; var R: TRect);
  begin
    R := cxRectInflate(R, -ScaleFactor.Apply(3));
    if not ReportLink.Transparent then
      ACanvas.FillRect(R, ReportLink.Color);
    R := cxRectSetHeight(R, Trunc((cxRectHeight(R) - 2) / ItemsCount) * ItemsCount);
    if ReportLink.IsDrawBorder then
      ACanvas.FrameRect(R, ReportLink.GridLineColor);
    R := cxRectInflate(R, -ScaleFactor.Apply(1));
  end;

  procedure DrawImage(ACanvas: TcxCanvas; var R: TRect; AIndex: Integer);
  var
    AImageSize: TSize;
  begin
    AImageSize := dxGetImageSize(ilPreview, ScaleFactor);
    cxDrawImage(ACanvas, cxRectSetWidth(R, AImageSize.cx), nil, ilPreview, AIndex, True, nil, ScaleFactor);
    Inc(R.Left, AImageSize.cx);
  end;

  procedure DrawItemGrid(ACanvas: TcxCanvas; R: TRect; ALevel: Integer; AIsFirst, AIsLast: Boolean);
  var
    ABorders: TcxBorders;
  begin
    ABorders := [];
    if ALevel > 0 then
      Include(ABorders, bLeft);
    if not AIsFirst then
      Include(ABorders, bTop);
    if not AIsLast then
      Include(ABorders, bBottom);
    ACanvas.FrameRect(cxRectInflate(R, 1, 1, 0, 0), ReportLink.GridLineColor, 1, ABorders);
  end;

  procedure DrawItem(ACanvas: TcxCanvas; R: TRect; AIndex, ALevel: Integer);
  var
    R1: TRect;
    I: Integer;
  begin
    if ReportLink.IsDrawGrid and (ALevel > 1) then
    begin
      R1 := R;
      for I := 1 to ALevel - 1 do
      begin
        Inc(R1.Left, cxRectHeight(R1));
        DrawItemGrid(ACanvas, R1, I, AIndex = 0, AIndex = ItemsCount - 1);
      end;
    end;

    Inc(R.Left, ALevel * cxRectHeight(R));
    if ReportLink.IsDrawGrid then
      DrawItemGrid(ACanvas, R, ALevel, AIndex = 0, AIndex = ItemsCount - 1);
    if tvpoStateImages in ReportLink.Options then
      DrawImage(ACanvas, R, 0);
    if tvpoImages in ReportLink.Options then
      DrawImage(ACanvas, R, 1);
    Inc(R.Left, cxTextOffset);

    dxAssignFont(ACanvas.Font, ReportLink.Font, ScaleFactor, ReportLink.ScaleFactor);
    ACanvas.Brush.Style := bsClear;
    cxDrawText(ACanvas.Handle, GetDesignerString(AIndex), R, DT_SINGLELINE or DT_VCENTER);
  end;

  procedure DrawTreeLine(ACanvas: TcxCanvas; const R: TRect; ARowHeight: Integer);
  var
    AExpandButtonRect: TRect;
  begin
    ACanvas.Pen.Color := ReportLink.TreeLineColor;
    ACanvas.Pen.Style := psDot;
    ACanvas.Brush.Style := bsClear;
    ACanvas.MoveTo(R.Left + ARowHeight div 2, R.Top);
    ACanvas.LineTo(R.Left + ARowHeight div 2, R.Bottom - ARowHeight div 2);
    ACanvas.LineTo(R.Right, R.Bottom - ARowHeight div 2);

    if tvpoButtons in ReportLink.Options then
    begin
      AExpandButtonRect := cxRectSetBottom(R, R.Bottom, ARowHeight);
      AExpandButtonRect := cxRectCenter(AExpandButtonRect, ScaleFactor.Apply(11), ScaleFactor.Apply(11));
      cxLookAndFeelPaintersManager.GetPainter(lfsUltraFlat).DrawScaledExpandButton(ACanvas, AExpandButtonRect, True, ScaleFactor);
    end;
  end;

  procedure DrawItems(ACanvas: TcxCanvas; R: TRect);
  const
    LevelMap: array[0..5] of Integer = (0, 1, 2, 2, 1, 0);
  var
    I, H: Integer;
    R1: TRect;
  begin
    H  := cxRectHeight(R) div ItemsCount;
    R1 := cxRectSetHeight(R, H);
    for I := 0 to ItemsCount - 1 do
    begin
      DrawItem(ACanvas, R1, I, LevelMap[I]);
      OffsetRect(R1, 0, cxRectHeight(R1));
    end;

    if tvpoTreeLines in ReportLink.Options then
    begin
      DrawTreeLine(ACanvas, Bounds(R.Left, R.Top + H, H, H), H);
      DrawTreeLine(ACanvas, Bounds(R.Left, R.Top + H, H, H * 4), H);

      DrawTreeLine(ACanvas, Bounds(R.Left + H, R.Top + 2 * H, H, H), H);
      DrawTreeLine(ACanvas, Bounds(R.Left + H, R.Top + 2 * H, H, H * 2), H);
    end;
  end;

begin
  inherited;
  cxPaintCanvas.BeginPaint(ACanvas);
  try
    DrawBackground(cxPaintCanvas, R);
    DrawItems(cxPaintCanvas, R);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxfmTVReportLinkDesignWindow.UpdateControlsState;
begin
  inherited;
  chbxNodeAutoHeight.Enabled := not chbxAutoWidth.Checked;
  ccbxColor.Enabled := not chbxTransparent.Checked;
  lblColor.Enabled := not chbxTransparent.Checked;
  seExpandLevel.Enabled := chbxAutoNodesExpand.Checked;
end;

procedure TdxfmTVReportLinkDesignWindow.UpdatePreview;
begin
  FPreviewBox.Invalidate;
end;

procedure TdxfmTVReportLinkDesignWindow.ccbxColorChange(Sender: TObject);
var
  AColor: TColor;
begin
  if LockControlsUpdate then Exit;
  AColor := TcxColorComboBox(Sender).ColorValue;
  case TTagToInt(TcxColorComboBox(Sender).Tag) of
    0: ReportLink.Color := AColor;
    1: ReportLink.GridLineColor := AColor;
    2: ReportLink.TreeLineColor := AColor;
  end;
  Modified := True;
  UpdatePreview;
end;

procedure TdxfmTVReportLinkDesignWindow.CreateControls;
var
  R: TRect;
begin
  FPreviewBox := TdxPSPaintPanel.Create(Self);
  with TdxPSPaintPanel(FPreviewBox) do
  begin
    Parent := pnlPreview;
    R := pnlPreview.BoundsRect;
    OffsetRect(R, -R.Left, -R.Top);
    InflateRect(R, -1, -1);
    BoundsRect := R;
    EdgeInner := esNone;
    EdgeOuter := esNone;
    OnPaint := pbxPreviewPaint;
  end;
end;

procedure TdxfmTVReportLinkDesignWindow.ExpandLevelChange(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.ExpandLevel := TcxSpinEdit(Sender).Value;
  Modified := True;
end;

function TdxfmTVReportLinkDesignWindow.GetReportLink: TCustomdxNativeTreeViewReportLink;
begin
  Result := inherited ReportLink as TCustomdxNativeTreeViewReportLink;
end;

procedure TdxfmTVReportLinkDesignWindow.pbxPreviewPaint(Sender: TObject);
begin
  with TdxPSPaintPanel(Sender) do
    PaintPreview(Canvas, ClientRect);
end;

procedure TdxfmTVReportLinkDesignWindow.CMDialogChar(var Message: TCMDialogChar);
var
  I: Integer;
begin
  inherited;
  for I := 0 to pcMain.Count - 1 do
    if IsAccel(Message.CharCode, pcMain.Items[I].Caption) then
    begin
      Message.Result := 1;
      pcMain.ItemIndex := I;
      Exit;
    end;
end;

procedure TdxfmTVReportLinkDesignWindow.chbxShowClick(Sender: TObject);
var
  AOption: TdxTreeViewPaintOption;
begin
  if not LockControlsUpdate then
  begin
    AOption := TdxTreeViewPaintOption(TTagToInt(TcxCheckBox(Sender).Tag));
    if TcxCheckBox(Sender).Checked then
      ReportLink.Options := ReportLink.Options + [AOption]
    else
      ReportLink.Options := ReportLink.Options - [AOption];

    Modified := True;
    UpdatePreview;
  end;
end;

procedure TdxfmTVReportLinkDesignWindow.chbxAutoWidthClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.AutoWidth := TcxCheckBox(Sender).Checked;
  Modified := True;
end;

procedure TdxfmTVReportLinkDesignWindow.chbxNodeAutoHeightClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.NodeAutoHeight := TcxCheckBox(Sender).Checked;
  Modified := True;
end;

procedure TdxfmTVReportLinkDesignWindow.btnFontClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;

  dxPSGlbl.FontDialog.Font := ReportLink.Font;
  if dxPSGlbl.FontDialog.Execute then
  begin
    ReportLink.Font := dxPSGlbl.FontDialog.Font;
    FontInfoToText(ReportLink.Font, edFont);
    Modified := True;
    UpdatePreview;
  end;
end;

procedure TdxfmTVReportLinkDesignWindow.chbxTransparentClick(Sender: TObject);
begin
  if not LockControlsUpdate then
  begin
    ReportLink.Transparent := TcxCheckBox(Sender).Checked;
    Modified := True;
    UpdatePreview;
  end;
end;

procedure TdxfmTVReportLinkDesignWindow.stTransparentClick(Sender: TObject);
begin
  if chbxTransparent.CanFocus then
    ActiveControl := chbxTransparent;
  chbxTransparent.Checked := not chbxTransparent.Checked;
end;

procedure TdxfmTVReportLinkDesignWindow.chbxAutoNodesExpandClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.AutoNodesExpand := TcxCheckBox(Sender).Checked;
  Modified := True;
end;

procedure TdxfmTVReportLinkDesignWindow.lblExpandLevelClick(Sender: TObject);
begin
  if TcxLabel(Sender).FocusControl <> nil then
    ActiveControl := TcxLabel(Sender).FocusControl;
end;

procedure TdxfmTVReportLinkDesignWindow.lblColorClick(Sender: TObject);
begin
  ActivateComboBoxControl(Self, TcxLabel(Sender).FocusControl);
end;

initialization
  dxPSRegisterReportLink(TdxTreeViewReportLink, TTreeView, TdxfmTVReportLinkDesignWindow);
 {$IFDEF REGISTERSHELLCTRLS}
  dxPSRegisterReportLink(TdxShellTreeViewReportLink, TShellTreeView, TdxfmTVReportLinkDesignWindow);
 {$ENDIF}

finalization
 {$IFDEF REGISTERSHELLCTRLS}
  dxPSUnregisterReportLink(TdxShellTreeViewReportLink, TShellTreeView, TdxfmTVReportLinkDesignWindow);
 {$ENDIF}
  dxPSUnregisterReportLink(TdxTreeViewReportLink, TTreeView, TdxfmTVReportLinkDesignWindow);

end.

