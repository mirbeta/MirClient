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

unit dxPSPrVwAdv;

interface

{$I cxVer.inc}

uses
  Windows, SysUtils, Classes, Messages, Graphics, Controls, ComCtrls, StdCtrls,
  ExtCtrls, Forms, Menus, ImgList, dxPSCore, dxPSESys, dxPrevw, dxPSPrvw, dxBar,
  dxBarPopupMenuEd, dxBarExtItems, cxClasses, cxControls, dxPSForm, cxGraphics,
  cxGeometry, cxLookAndFeels, cxLookAndFeelPainters, IniFiles, cxImageList, cxImageComboBox,
  cxBarEditItem, cxSpinEdit, cxDropDownEdit;

type
  TdxfmPreviewWdxBar = class(TForm, IdxPSPreviewWindowDialog)
    bbDefinePrintStyles: TdxBarButton;
    bbEdit: TdxBarSubItem;
    bbEditFind: TdxBarButton;
    bbEditFindNext: TdxBarButton;
    bbEditReplace: TdxBarButton;
    bbExplorer: TdxBarSubItem;
    bbExplorerCreateNewFolder: TdxBarButton;
    bbExplorerDelete: TdxBarButton;
    bbExplorerProperties: TdxBarButton;
    bbExplorerRename: TdxBarButton;
    bbExportToPDF: TdxBarButton;
    bbFile: TdxBarSubItem;
    bbFileClose: TdxBarButton;
    bbFileDesign: TdxBarButton;
    bbFileExit: TdxBarButton;
    bbFileLoad: TdxBarButton;
    bbFilePageSetup: TdxBarButton;
    bbFilePrint: TdxBarButton;
    bbFilePrintDialog: TdxBarButton;
    bbFileRebuild: TdxBarButton;
    bbFileSave: TdxBarButton;
    bbFormat: TdxBarSubItem;
    bbFormatDateTime: TdxBarButton;
    bbFormatFootnotes: TdxBarButton;
    bbFormatHeaderAndFooter: TdxBarButton;
    bbFormatHFBackground: TdxBarButton;
    bbFormatHFClear: TdxBarButton;
    bbFormatPageBackground: TdxBarButton;
    bbFormatPageNumbering: TdxBarButton;
    bbFormatShrinkToPageWidth: TdxBarButton;
    bbFormatTitle: TdxBarButton;
    bbGoToFirstPage: TdxBarButton;
    bbGoToLastPage: TdxBarButton;
    bbGoToNextPage: TdxBarButton;
    bbGoToPage: TdxBarSubItem;
    bbGoToPrevPage: TdxBarButton;
    bbHelp: TdxBarSubItem;
    bbHelpAbout: TdxBarButton;
    bbHelpTopics: TdxBarButton;
    bbInsert: TdxBarSubItem;
    bbInsertEditAutoText: TdxBarButton;
    bbInsertHFDate: TdxBarButton;
    bbInsertHFDateTime: TdxBarButton;
    bbInsertHFMachineName: TdxBarButton;
    bbInsertHFPageNumber: TdxBarButton;
    bbInsertHFPageOfPages: TdxBarButton;
    bbInsertHFTime: TdxBarButton;
    bbInsertHFTotalPages: TdxBarButton;
    bbInsertHFUserName: TdxBarButton;
    bbThumbnailsLarge: TdxBarButton;
    bbThumbnailsSmall: TdxBarButton;
    bbTools: TdxBarSubItem;
    bbToolsCustomize: TdxBarButton;
    bbToolsOptions: TdxBarButton;
    bbView: TdxBarSubItem;
    bbViewExplorer: TdxBarButton;
    bbViewHFClose: TdxBarButton;
    bbViewHFSwitchHeaderFooter: TdxBarButton;
    bbViewMarginBar: TdxBarButton;
    bbViewMargins: TdxBarButton;
    bbViewPageFooters: TdxBarButton;
    bbViewPageHeaders: TdxBarButton;
    bbViewPages: TdxBarSubItem;
    bbViewStatusBar: TdxBarButton;
    bbViewSwitchToCenterPart: TdxBarButton;
    bbViewSwitchToLeftPart: TdxBarButton;
    bbViewSwitchToRightPart: TdxBarButton;
    bbViewThumbnails: TdxBarButton;
    bbViewToolbars: TdxBarToolbarsListItem;
    bbViewZoom: TdxBarSubItem;
    bbZoomFourPages: TdxBarButton;
    bbZoomMultiplePages: TdxBarButton;
    bbZoomPageWidth: TdxBarButton;
    bbZoomPercent100: TdxBarButton;
    bbZoomSetup: TdxBarButton;
    bbZoomTwoPages: TdxBarButton;
    bbZoomWholePage: TdxBarButton;
    bbZoomWidenToSourceWidth: TdxBarButton;
    bliInsertAutoTextEntries: TdxBarListItem;
    bliPrintStyles: TdxBarListItem;
    bsiInsertAutoText: TdxBarSubItem;
    bsiInsertHFAutoText: TdxBarSubItem;
    bsiNewMenuNewMenu: TdxBarSubItem;
    bsiShortCutExplorer: TdxBarSubItem;
    bsiShortcutPreview: TdxBarSubItem;
    bsiShortcutThumbnails: TdxBarSubItem;
    cbxPredefinedZoom: TcxBarEditItem;
    dxBarManager: TdxBarManager;
    ilToolBar: TcxImageList;
    pmExplorer: TdxBarPopupMenu;
    pmPreview: TdxBarPopupMenu;
    pmPrintStyles: TdxBarPopupMenu;
    pmThumbnails: TdxBarPopupMenu;
    Preview: TdxPSPreviewWindow;
    seActivePage: TcxBarEditItem;

    procedure bbExplorerPropertiesClick(Sender: TObject);
    procedure bbExportToPDFClick(Sender: TObject);
    procedure bbFileCloseClick(Sender: TObject);
    procedure bbFileRebuildClick(Sender: TObject);
    procedure bbFormatDateTimeClick(Sender: TObject);
    procedure bbFormatFootnotesClick(Sender: TObject);
    procedure bbFormatHeaderAndFooterClick(Sender: TObject);
    procedure bbFormatHFBackgroundClick(Sender: TObject);
    procedure bbFormatHFClearClick(Sender: TObject);
    procedure bbFormatPageNumbersClick(Sender: TObject);
    procedure bbFormatShrinkToPageWidthClick(Sender: TObject);
    procedure bbFormatTitleClick(Sender: TObject);
    procedure bbThumbnailsSizeClick(Sender: TObject);
    procedure bbToolsCustomizeClick(Sender: TObject);
    procedure bbToolsOptionsClick(Sender: TObject);
    procedure bbViewExplorerClick(Sender: TObject);
    procedure bbViewHFCloseClick(Sender: TObject);
    procedure bbViewHFSwitchHeaderFooterClick(Sender: TObject);
    procedure bbViewMarginBarClick(Sender: TObject);
    procedure bbViewMarginsClick(Sender: TObject);
    procedure bbViewPageFootersClick(Sender: TObject);
    procedure bbViewPageHeadersClick(Sender: TObject);
    procedure bbViewStatusBarClick(Sender: TObject);
    procedure bbViewThumbnailsClick(Sender: TObject);
    procedure bbZoomMultiplePagesClick(Sender: TObject);
    procedure bbZoomSetupClick(Sender: TObject);
    procedure bliPrintStylesGetData(Sender: TObject);
    procedure cbxPredefinedZoomChange(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure DesignClick(Sender: TObject);
    procedure dxBarManagerBarVisibleChange(Sender: TdxBarManager; ABar: TdxBar);
    procedure dxBarManagerHideCustomizingForm(Sender: TObject);
    procedure dxBarManagerShowCustomizingForm(Sender: TObject);
    procedure ExplorerCreateNewFolderClick(Sender: TObject);
    procedure ExplorerDeleteItemClick(Sender: TObject);
    procedure ExplorerLoadDataClick(Sender: TObject);
    procedure ExplorerRenameItemClick(Sender: TObject);
    procedure FileSaveClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure GoToPageClick(Sender: TObject);
    procedure HelpClick(Sender: TObject);
    procedure InsertHFClick(Sender: TObject);
    procedure miCustomizePopupClick(Sender: TObject);
    procedure PageBackgroundClick(Sender: TObject);
    procedure PageSetupClick(Sender: TObject);
    procedure pmExplorerPopup(Sender: TObject);
    procedure PreviewAddExplorerCommand(Sender: TObject; ACommand: TCustomdxPSExplorerContextCommand);
    procedure PreviewCanShowMarginHint(Sender: TObject; var AAllow: Boolean);
    procedure PreviewHFTextEntriesChanged(Sender: TObject);
    procedure PreviewInitContent(Sender: TObject);
    procedure PreviewLoadProperties(Sender: TObject; AIniFile: TCustomIniFile; const ASectionName: string);
    procedure PreviewPreviewDblClick(Sender: TObject);
    procedure PreviewSaveProperties(Sender: TObject; AIniFile: TCustomIniFile; const ASectionName: string);
    procedure PreviewStyleListChanged(Sender: TObject);
    procedure PreviewUpdateControls(Sender: TObject);
    procedure PreviewUpdateExplorerCommands(Sender: TObject);
    procedure PreviewZoomFactorChanged(Sender: TObject);
    procedure PrintClick(Sender: TObject);
    procedure seActivePageChange(Sender: TObject);
    procedure SwitchPartClick(Sender: TObject);
    procedure ZoomClick(Sender: TObject);
  private
    FExplorerContextCommands: TList;

    function GetExplorerContextCommand(Index: Integer): TCustomdxPSExplorerContextCommand;
    function GetExplorerContextCommandCount: Integer;
    function GetLocked: Boolean;
    function GetReportLink: TBasedxReportLink;

    function CalcWindowPos(Sender: TObject): TPoint;
    procedure DoShowExplorerPopup(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EnableItemsWhileBuilding(Value: Boolean);
    procedure PostCreateFolderMessage;
    procedure SetBarItemVisibility(Item: TdxBarItem; Value: Boolean);

    function ShortcutBar: TdxBar;
    procedure ShowShortCutBar(Value: Boolean);

    function HFBar: TdxBar;
    procedure EnabledHFItems(Value: Boolean);
    procedure ShowHFBar(Value: Boolean);
    procedure UpdateHFState(Value: Boolean);

    procedure LoadBarManagerDefaults;
    procedure SaveBarManagerDefaults;
    procedure LoadBarManager(AIniFile: TCustomIniFile; const ASectionName: string);
    procedure SaveBarManager(AIniFile: TCustomIniFile; const ASectionName: string);
  protected
    procedure ExplorerContextCommandClick(Sender: TObject); virtual;
    procedure LoadStrings;
    procedure WndProc(var Message: TMessage); override;

    // IdxPSPreviewWindowDialog
    procedure IdxPSPreviewWindowDialog.Initialize = LoadBarManagerDefaults;

    property ExplorerContextCommandCount: Integer read GetExplorerContextCommandCount;
    property ExplorerContextCommands[Index: Integer]: TCustomdxPSExplorerContextCommand read GetExplorerContextCommand;
    property Locked: Boolean read GetLocked;
    property ReportLink: TBasedxReportLink read GetReportLink;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
 end;

implementation

{$R *.DFM}

uses
  Types, dxThemeManager, Registry, CommCtrl, Math,
  dxMessages, dxPSRes, dxPrnPg, dxPSGlbl, dxCore, dxCustomPreview,
  dxPSPopupMan, dxPSUtl, dxPSPgsMnuBld, dxPSEngn, dxPgsDlg, dxPSEvnt,
  dxPSAutoHFTextMnuBld, dxPSfmAutoHFTextFmt, dxPrnDev, dxPSPDFExport, dxPSImgs, dxDPIAwareUtils;

const
  PageSelectorImageIndex = 35;

  sdxToolBars = 'ToolBars';                           // Don't Localize

type
  TdxBarManagerAccess = class(TdxBarManager);
  TdxPreviewAccess = class(TdxPreview);
  TWinControlAccess = class(TWinControl);

  { TdxBarPSPopupMenuBuilder }

  TdxBarPSPopupMenuBuilder = class(TAbstractdxPSPopupMenuBuilder)
  private
    FBarHostForm: TCustomForm;
    FBarManager: TdxBarManager;
  protected
    function BuildPopup(const AControl: TControl;
      const APopupMenu: TPopupMenu): TComponent; override;
    class function CanShowPopup(const APopupMenu: TPopupMenu): Boolean; override;
    procedure FreePopup(var APopupMenu: TComponent); override;
    procedure InvokePopup(X, Y: Integer; AControl: TControl; APopupMenu: TComponent); override;
    class function RequireProcessDoPopup: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TdxBarPSPageSetupMenuBuilder }

  TdxBarPSPageSetupMenuBuilder = class(TdxStandardPSPageSetupMenuBuilder)
  public
    procedure BuildPageSetupMenu(ARootItem: TObject; AData: Pointer;
      AIncludeDefineItem: Boolean; AStyles: TStringList; ACurrentStyle: TBasedxPrintStyle;
      AOnStyleClick, AOnDefineStylesClick: TNotifyEvent); override;
    class function ExtractPrintStyleFromObj(Obj: TObject): TBasedxPrintStyle; override;
  end;

  { TdxBarPSAutoHFTextMenuBuilder }

  TdxBarPSAutoHFTextMenuBuilder = class(TAbstractdxPSAutoHFTextMenuBuilder)
  public
    procedure BuildAutoHFTextEntriesMenu(ARootItem: TObject; AData: Pointer;
      AIncludeSetupAutoHFTextEntriesItem: Boolean; AAutoHFTextEntries: TStrings;
      AOnHFTextEntriesClick, AOnSetupHFTextEntriesClick: TNotifyEvent); override;
    class function ExtractAutoHFTextEntryIndexFromObj(Obj: TObject): Integer; override;
  end;

  { TdxPSAdvancedPreviewDialogStyleInfo }

  TdxPSAdvancedPreviewDialogStyleInfo = class(TdxPSPreviewDialogStyleInfo)
  public
    class function CreatePreviewWindow: TdxPSCustomPreviewWindow; override;
    class function GetName: string; override;
  end;

function dxBar_DoesNotHaveActivePopup: Boolean;
begin
  Result := dxBar.ActiveBarControl = nil;
end;

procedure SetupBarManagerStyle(ABarManager: TdxBarManager);
const
  BarManagerStyleMap: array[TcxLookAndFeelKind] of dxBar.TdxBarManagerStyle =
    (dxBar.bmsFlat, dxBar.bmsEnhanced, dxBar.bmsFlat, dxBar.bmsFlat);
begin
  ABarManager.Style := bmsUseLookAndFeel;
  ABarManager.LookAndFeel := dxPSEngine.DialogsLookAndFeel;
end;

{ TdxBarPSPopupMenuBuilder }

constructor TdxBarPSPopupMenuBuilder.Create;
begin
  inherited;
  FBarHostForm := TCustomForm.CreateNew(nil);
  FBarManager := TdxBarManager.Create(FBarHostForm);
  FBarManager.StretchGlyphs := False;
  SetupBarManagerStyle(FBarManager);
end;

destructor TdxBarPSPopupMenuBuilder.Destroy;
begin
  FBarHostForm.Free;
  inherited;
end;

class function TdxBarPSPopupMenuBuilder.CanShowPopup(const APopupMenu: TPopupMenu): Boolean;
begin
  Result := inherited CanShowPopup(APopupMenu) and  dxBar_DoesNotHaveActivePopup;
end;

function TdxBarPSPopupMenuBuilder.BuildPopup(const AControl: TControl;
  const APopupMenu: TPopupMenu): TComponent;

  function IsSeparator(ABarItem: TdxBarItem): Boolean;
  begin
    Result := ABarItem.Caption = '-';
  end;

  function CreateItem(AMenuItem: TMenuItem): TdxBarItem;
  const
    BarItemClasses: array[Boolean] of TdxBarItemClass = (TdxBarButton, TdxBarSubItem);
  var
    BarItemClass: TdxBarItemClass;
  begin
    Result := nil;
    BarItemClass := BarItemClasses[AMenuItem.Count > 0];
    if BarItemClass = nil then Exit;

    Result := BarItemClass.Create(FBarHostForm);

    Result.Action := AMenuItem.Action;
    Result.ImageIndex := AMenuItem.ImageIndex;
    Result.Glyph.Assign(AMenuItem.Bitmap);
    Result.Caption := AMenuItem.Caption;
    Result.Enabled := AMenuItem.Enabled;
    Result.HelpContext := AMenuItem.HelpContext;
    Result.Hint := AMenuItem.Hint;
    Result.ShortCut := AMenuItem.ShortCut;
    Result.Tag := AMenuItem.Tag;
    Result.Visible := VisibleTodxBarVisible(AMenuItem.Visible);
    if not (Result is TdxBarSubItem) then
      Result.OnClick := AMenuItem.OnClick;
    if Result is TdxBarButton then
    begin
      if AMenuItem.Checked or AMenuItem.RadioItem then
        TdxBarButton(Result).ButtonStyle := bsChecked;
      if AMenuItem.RadioItem then
        TdxBarButton(Result).GroupIndex := AMenuItem.GroupIndex;
      TdxBarButton(Result).Down := AMenuItem.Checked;
    end;
  end;

  procedure FixBeginGroup(AItemLinks: TdxBarItemLinks);
  var
    I: Integer;
    ItemLink: TdxBarItemLink;
  begin
    for I := AItemLinks.Count - 1 downto 0 do
    begin
      ItemLink := AItemLinks.Items[I];
      if IsSeparator(ItemLink.Item) then
      begin
        ItemLink.Free;
        if I < AItemLinks.Count then
          AItemLinks.Items[I].BeginGroup := True;
      end;
    end;
  end;

  procedure ProcessSubMenu(AItemLinks: TdxBarItemLinks; AMenuItem: TMenuItem);
  var
    I: Integer;
    MI: TMenuItem;
    Item: TdxBarItem;
  begin
    for I := 0 to AMenuItem.Count - 1 do
    begin
      MI := AMenuItem.Items[I];
      Item := CreateItem(MI);
      if Item <> nil then
      begin
        AItemLinks.Add.Item := Item;
        if Item is TdxBarSubItem then
          ProcessSubMenu(TdxBarSubItem(Item).ItemLinks, MI);
      end;
    end;
    FixBeginGroup(AItemLinks);
  end;

begin
  Result := nil;
  if (APopupMenu <> nil) and (APopupMenu.Items.Count > 0) then
  begin
    Result := TdxBarPopupMenu.Create(FBarHostForm);
    try
      FBarManager.Images := APopupMenu.Images;
      ProcessSubMenu(TdxBarPopupMenu(Result).ItemLinks, APopupMenu.Items);
    except
      FBarManager.Images := nil;
      Result.Free;
      raise;
    end;
  end;
end;

procedure TdxBarPSPopupMenuBuilder.FreePopup(var APopupMenu: TComponent);
var
  I: Integer;
begin
  for I := 0 to FBarManager.ItemCount - 1 do
    FBarManager.Items[I].Free;
  FreeAndNil(APopupMenu);
end;

procedure TdxBarPSPopupMenuBuilder.InvokePopup(
  X, Y: Integer; AControl: TControl; APopupMenu: TComponent);
var
  R: TRect;
begin
  if APopupMenu is TdxBarPopupMenu then
  begin
    if AControl = nil then
      TdxBarPopupMenu(APopupMenu).Popup(X, Y)
    else
    begin
      R := cxRectOffset(AControl.ClientRect, AControl.ClientToScreen(cxNullPoint));
      TdxBarPopupMenu(APopupMenu).PopupEx(X, Y, AControl.Width, AControl.Height, True, @R);
    end;
  end;
end;

class function TdxBarPSPopupMenuBuilder.RequireProcessDoPopup: Boolean;
begin
  Result := True;
end;

{ TdxBarPSPageSetupMenuBuilder }

class function TdxBarPSPageSetupMenuBuilder.ExtractPrintStyleFromObj(Obj: TObject): TBasedxPrintStyle;
begin
  if Obj is TdxBarListItem then
  begin
    with TdxBarListItem(Obj) do
      Result := TBasedxPrintStyle(Items.Objects[ItemIndex])
  end
  else
    Result := inherited ExtractPrintStyleFromObj(Obj);
end;

procedure TdxBarPSPageSetupMenuBuilder.BuildPageSetupMenu(ARootItem: TObject;
  AData: Pointer; AIncludeDefineItem: Boolean;
  AStyles: TStringList; ACurrentStyle: TBasedxPrintStyle;
  AOnStyleClick, AOnDefineStylesClick: TNotifyEvent);
begin
  if not (ARootItem is TdxBarListItem) then
  begin
    inherited BuildPageSetupMenu(ARootItem, AData, AIncludeDefineItem,
      AStyles, ACurrentStyle, AOnStyleClick, AOnDefineStylesClick);
    Exit;
  end;

  with TdxBarListItem(ARootItem) do
  begin
    Items.Clear;
    Items := AStyles;
    if Items.Count > 0 then ItemIndex := ACurrentStyle.Index;
    OnClick := AOnStyleClick;
  end;

  if AIncludeDefineItem and (TObject(AData) is TdxBarButton) then
    with TdxBarButton(AData) do
    begin
      Caption := cxGetResourceString(@sdxDefinePrintStylesMenuItem);
      OnClick := AOnDefineStylesClick;
    end;
end;

{ TdxBarPSAutoHFTextMenuBuilder }

procedure TdxBarPSAutoHFTextMenuBuilder.BuildAutoHFTextEntriesMenu(ARootItem: TObject;
  AData: Pointer; AIncludeSetupAutoHFTextEntriesItem: Boolean;
  AAutoHFTextEntries: TStrings; AOnHFTextEntriesClick, AOnSetupHFTextEntriesClick: TNotifyEvent);
begin
  if not (ARootItem is TdxBarListItem) then Exit;

  with TdxBarListItem(ARootItem) do
  begin
    Items.Clear;
    Items := AAutoHFTextEntries;
    OnClick := AOnHFTextEntriesClick;
  end;

  if AIncludeSetupAutoHFTextEntriesItem and (TObject(AData) is TdxBarButton) then
    with TdxBarButton(AData) do
    begin
      Caption := cxGetResourceString(@sdxMenuInsertEditAutoTextEntries);
      OnClick := AOnSetupHFTextEntriesClick;
    end;
end;

class function TdxBarPSAutoHFTextMenuBuilder.ExtractAutoHFTextEntryIndexFromObj(Obj: TObject): Integer;
begin
  Result := TdxBarListItem(Obj).ItemIndex;
end;

{ utility routines }

function AddPercentageChar(const S: string): string;
begin
  Result := S;
  if Result[Length(Result)] <> PercentSymbol then
    Result := Result + PercentSymbol;
end;

procedure ProcessMessages;
begin
  //Application.ProcessMessages;
end;

{ TdxfmPreviewWdxBar }

constructor TdxfmPreviewWdxBar.Create(AOwner: TComponent);
begin
  inherited;
  FExplorerContextCommands := TList.Create;
  dxBarManager.BeginUpdate;
  try
    Preview.LookAndFeel.MasterLookAndFeel := dxPSEngine.DialogsLookAndFeel;
    dxLoadImageListFromResources(ilToolBar, IDB_DXPSPREVIEW_TOOLBARS_SMALLIMAGELIST);
    SetupBarManagerStyle(dxBarManager);
    LoadStrings;
  finally
    dxBarManager.EndUpdate;
  end;
end;

destructor TdxfmPreviewWdxBar.Destroy;
begin
  SaveBarManagerDefaults;
  FreeAndNil(FExplorerContextCommands);
  inherited Destroy;
end;

procedure TdxfmPreviewWdxBar.AfterConstruction;
var
  Bar: TdxBar;
begin
  inherited AfterConstruction;
  seActivePage.EditValue := 1;

  ShowHFBar(False);
  Bar := ShortCutBar;
  if Bar <> nil then Bar.Hidden := True;

  dxBarManager.PopupMenuLinks[0].Control := Preview.Preview;
  dxBarManager.PopupMenuLinks[1].Control := Preview.ThumbnailsPreview;
end;

procedure TdxfmPreviewWdxBar.bbViewMarginsClick(Sender: TObject);
begin
  if not Locked then
    Preview.DoShowPageMargins(TdxBarButton(Sender).Down);
end;

procedure TdxfmPreviewWdxBar.bbViewMarginBarClick(Sender: TObject);
begin
  if not Locked then
    Preview.Options.ShowMarginBar := TdxBarButton(Sender).Down;
end;

procedure TdxfmPreviewWdxBar.bbViewStatusBarClick(Sender: TObject);
begin
  if not Locked then
    Preview.Options.ShowStatusBar := TdxBarButton(Sender).Down;
end;

procedure TdxfmPreviewWdxBar.bbViewExplorerClick(Sender: TObject);
begin
  if not Locked then
    Preview.Options.ShowExplorer := TdxBarButton(Sender).Down;
end;

procedure TdxfmPreviewWdxBar.pmExplorerPopup(Sender: TObject);
begin
  Preview.UpdateControls;
end;

procedure TdxfmPreviewWdxBar.bbThumbnailsSizeClick(Sender: TObject);
begin
  Preview.Options.ThumbnailsOptions.Size := TdxPSThumbnailsSize(TTagToInt(TComponent(Sender).Tag));
end;

procedure TdxfmPreviewWdxBar.bbViewThumbnailsClick(Sender: TObject);
begin
  if not Locked then
    Preview.Options.ThumbnailsOptions.Visible := TdxBarButton(Sender).Down;
end;

procedure TdxfmPreviewWdxBar.DesignClick(Sender: TObject);
begin
  Preview.DoDesignReport;
end;

procedure TdxfmPreviewWdxBar.bbFileRebuildClick(Sender: TObject);
begin
  if Preview.CanRebuild then
    Preview.RebuildReport;
end;

procedure TdxfmPreviewWdxBar.PrintClick(Sender: TObject);
const
  BtnClicked: Boolean = False;
begin
  if BtnClicked then Exit;
  ProcessMessages;
  BtnClicked := True;
  try
    Preview.DoPrintReport(Boolean(TTagToInt(TComponent(Sender).Tag)));
  finally
    BtnClicked := False;
  end;
end;

procedure TdxfmPreviewWdxBar.PageSetupClick(Sender: TObject);
const
  BtnClicked: Boolean = False;
begin
  if BtnClicked then Exit;
  ProcessMessages;
  BtnClicked := True;
  try
    Preview.DoPageSetupReport(0);
  finally
    BtnClicked := False;
  end;
end;

procedure TdxfmPreviewWdxBar.FileSaveClick(Sender: TObject);
begin
  Preview.DoExplorerCreateNewItem;
end;

procedure TdxfmPreviewWdxBar.ExplorerLoadDataClick(Sender: TObject);
begin
  ProcessMessages;
  Preview.DoExplorerLoadItemData;
end;

procedure TdxfmPreviewWdxBar.bbFileCloseClick(Sender: TObject);
begin
  ProcessMessages;
  Preview.DoExplorerUnloadItemData;
end;

procedure TdxfmPreviewWdxBar.ExplorerCreateNewFolderClick(Sender: TObject);
begin
  Preview.ExplorerTree.FocusedItem := Preview.ExplorerTree.SelectedItem;
  PostCreateFolderMessage;
end;

procedure TdxfmPreviewWdxBar.PostCreateFolderMessage;
begin
  PostMessage(Handle, DXM_PS_CREATEFOLDER, 0, 0);
end;

procedure TdxfmPreviewWdxBar.ExplorerDeleteItemClick(Sender: TObject);
begin
  ProcessMessages;
  Preview.DoExplorerDeleteItem;
end;

procedure TdxfmPreviewWdxBar.ExplorerRenameItemClick(Sender: TObject);
begin
  ProcessMessages;
  Preview.DoExplorerRenameItem;
end;

procedure TdxfmPreviewWdxBar.bbExplorerPropertiesClick(Sender: TObject);
begin
  Preview.DoExplorerItemShowPropertySheets;
end;

procedure TdxfmPreviewWdxBar.PageBackgroundClick(Sender: TObject);
begin
  ProcessMessages;
  Preview.DoShowPageBackgroundDlg(CalcWindowPos(Sender));
end;

procedure TdxfmPreviewWdxBar.bbFormatShrinkToPageWidthClick(Sender: TObject);
begin
  if ReportLink <> nil then
    ReportLink.ShrinkToPageWidth := not ReportLink.ShrinkToPageWidth;
end;

procedure TdxfmPreviewWdxBar.bbZoomSetupClick(Sender: TObject);
begin
  ProcessMessages;
  Preview.DoShowZoomDlg;
end;

procedure TdxfmPreviewWdxBar.bliPrintStylesGetData(Sender: TObject);
begin
  if Assigned(ReportLink.CurrentPrintStyle) then
    bliPrintStyles.ItemIndex := ReportLink.CurrentPrintStyle.Index;
end;

procedure TdxfmPreviewWdxBar.bbZoomMultiplePagesClick(Sender: TObject);
var
  Link: TdxBarItemLink;
  R: TRect;
  YShift: Integer;
begin
  Link := TdxBarItem(Sender).ClickItemLink;
  if Assigned(Link) and Assigned(Link.Control) then
  begin
    R := Link.ItemRect;
    MapWindowPoints(Link.BarControl.Handle, 0, R, 2);
    YShift := R.Bottom - R.Top;
  end
  else
  begin
    R := cxRectBounds(Mouse.CursorPos, 0, 0);
    YShift := 3;
  end;
  Preview.DoShowMultiplySelectPagesDlg(ilToolbar, PageSelectorImageIndex, R.TopLeft, YShift);
end;

procedure TdxfmPreviewWdxBar.GoToPageClick(Sender: TObject);
begin
  case TTagToInt(TComponent(Sender).Tag) of
    0: Preview.GoToFirstPage;
    1: Preview.GoToPrevPage;
    2: Preview.GoToNextPage;
    3: Preview.GoToLastPage;
  end;
end;

procedure TdxfmPreviewWdxBar.CloseClick(Sender: TObject);
begin
  Close;
end;

procedure TdxfmPreviewWdxBar.HelpClick(Sender: TObject);
begin
  ProcessMessages;
  Preview.DoInvokeHelp;
end;

procedure TdxfmPreviewWdxBar.WndProc(var Message: TMessage);
begin
  if Message.Msg = DXM_PS_CREATEFOLDER then
  begin
    Preview.ExplorerTree.SelectedItem := Preview.ExplorerTree.FocusedItem;
    Preview.DoExplorerCreateNewFolder;
  end
  else
    inherited;
end;

procedure TdxfmPreviewWdxBar.ExplorerContextCommandClick(Sender: TObject);
var
  Command: TCustomdxPSExplorerContextCommand;
  CommandSet2: IdxPSExplorerContextCommands2;
begin
  Command := TCustomdxPSExplorerContextCommand(TTagToObj(TdxBarButton(Sender).Tag));
  if Supports(TObject(Preview.Explorer), IdxPSExplorerContextCommands2, CommandSet2) then
  begin
    CommandSet2.InitializeCommand(Command);
    try
      if Command.Enabled then Command.Execute; {.1}
    finally
      CommandSet2.FinalizeCommand(Command);
    end;
  end;
  Preview.UpdateControls;
end;

procedure TdxfmPreviewWdxBar.LoadStrings;
begin
  with dxBarManager do
  begin
    Bars[0].Caption := cxGetResourceString(@sdxMenuBar);
    Bars[1].Caption := cxGetResourceString(@sdxStandardBar);
    Bars[2].Caption := cxGetResourceString(@sdxHeaderFooterBar);
    Bars[3].Caption := cxGetResourceString(@sdxShortCutMenusBar);
    Bars[4].Caption := cxGetResourceString(@sdxAutoTextBar);
    Bars[5].Caption := cxGetResourceString(@sdxExplorerBar);

    Categories[0] := DropAmpersand(cxGetResourceString(@sdxMenuFile));
    Categories[1] := DropAmpersand(cxGetResourceString(@sdxMenuExplorer));
    Categories[2] := DropAmpersand(cxGetResourceString(@sdxMenuEdit));
    Categories[3] := DropAmpersand(cxGetResourceString(@sdxMenuInsert));
    Categories[4] := DropAmpersand(cxGetResourceString(@sdxMenuView));
    Categories[5] := DropAmpersand(cxGetResourceString(@sdxMenuFormat));
    Categories[6] := DropAmpersand(cxGetResourceString(@sdxMenuZoom));
    Categories[7] := DropAmpersand(cxGetResourceString(@sdxMenuTools));
    Categories[8] := DropAmpersand(cxGetResourceString(@sdxMenuGotoPage));
    Categories[9] := DropAmpersand(cxGetResourceString(@sdxMenuHelp));
    Categories[10] := DropAmpersand(cxGetResourceString(@sdxMenuBuiltInMenus));
    Categories[11] := DropAmpersand(cxGetResourceString(@sdxMenuShortCutMenus));
    Categories[12] := DropAmpersand(cxGetResourceString(@sdxMenuNewMenu));
  end;

  bbFile.Caption := cxGetResourceString(@sdxMenuFile);
  bbFileSave.Caption := cxGetResourceString(@sdxMenuFileSave);
  bbFileLoad.Caption := cxGetResourceString(@sdxMenuFileLoad);
  bbFileClose.Caption := cxGetResourceString(@sdxMenuFileClose);
  bbFileDesign.Caption := cxGetResourceString(@sdxMenuFileDesign);
  bbFilePrintDialog.Caption := cxGetResourceString(@sdxMenuFilePrint);
  bbFilePrint.Caption := DropEndEllipsis(cxGetResourceString(@sdxMenuFilePrint));
  bbFileRebuild.Caption := cxGetResourceString(@sdxMenuFileRebuild);
//  bbFilePrintSetup.Caption := cxGetResourceString(@sdxMenuFilePrintSetup);
  bliPrintStyles.Caption := cxGetResourceString(@sdxMenuPrintStyles);
  bbFilePageSetup.Caption := cxGetResourceString(@sdxMenuFilePageSetup);
  bbFileExit.Caption := cxGetResourceString(@sdxMenuFileExit);
  bbExportToPDF.Caption := cxGetResourceString(@sdxMenuExportToPDF);

  bbExplorer.Caption := cxGetResourceString(@sdxMenuExplorer);
  bbExplorerCreateNewFolder.Caption := cxGetResourceString(@sdxMenuExplorerCreateFolder);
  bbExplorerDelete.Caption := cxGetResourceString(@sdxMenuExplorerDelete);
  bbExplorerRename.Caption := cxGetResourceString(@sdxMenuExplorerRename);
  bbExplorerProperties.Caption := cxGetResourceString(@sdxMenuExplorerProperties);

  bbEdit.Caption := cxGetResourceString(@sdxMenuEdit);
  bbEditFind.Caption := cxGetResourceString(@sdxMenuEditFind);
  bbEditFindNext.Caption := cxGetResourceString(@sdxMenuEditFindNext);
  bbEditReplace.Caption := cxGetResourceString(@sdxMenuEditReplace);

  bbInsert.Caption := cxGetResourceString(@sdxMenuInsert);
  bsiInsertHFAutoText.Caption := cxGetResourceString(@sdxMenuInsertAutoText);
  bbInsertEditAutoText.Caption := cxGetResourceString(@sdxMenuInsertEditAutoTextEntries);
  bsiInsertAutoText.Caption := cxGetResourceString(@sdxMenuInsertAutoTextEntriesSubItem);
  bliInsertAutoTextEntries.Caption := cxGetResourceString(@sdxMenuInsertAutoTextEntries);
  bbInsertHFPageNumber.Caption := cxGetResourceString(@sdxMenuInsertPageNumber);
  bbInsertHFTotalPages.Caption := cxGetResourceString(@sdxMenuInsertTotalPages);
  bbInsertHFPageOfPages.Caption := cxGetResourceString(@sdxMenuInsertPageOfPages);
  bbInsertHFDateTime.Caption := cxGetResourceString(@sdxMenuInsertDateTime);
  bbInsertHFDate.Caption := cxGetResourceString(@sdxMenuInsertDate);
  bbInsertHFTime.Caption := cxGetResourceString(@sdxMenuInsertTime);
  bbInsertHFUserName.Caption := cxGetResourceString(@sdxMenuInsertUserName);
  bbInsertHFMachineName.Caption := cxGetResourceString(@sdxMenuInsertMachineName);

  bbView.Caption := cxGetResourceString(@sdxMenuView);
  bbViewMargins.Caption := cxGetResourceString(@sdxMenuViewMargins);
  bbViewMarginBar.Caption := cxGetResourceString(@sdxMenuViewMarginsStatusBar);
  bbViewStatusBar.Caption := cxGetResourceString(@sdxMenuViewPagesStatusBar);
  bbViewExplorer.Caption := cxGetResourceString(@sdxMenuViewExplorer);
  bbViewThumbnails.Caption := cxGetResourceString(@sdxMenuViewThumbnails);
  bbThumbnailsSmall.Caption := cxGetResourceString(@sdxMenuThumbnailsSmall);
  bbThumbnailsLarge.Caption := cxGetResourceString(@sdxMenuThumbnailsLarge);
  bbViewToolbars.Caption := cxGetResourceString(@sdxMenuViewToolBars);
  bbViewZoom.Caption := cxGetResourceString(@sdxMenuZoom);

  bbZoomPercent100.Caption := cxGetResourceString(@sdxMenuZoomPercent100);
  bbZoomPageWidth.Caption := cxGetResourceString(@sdxMenuZoomPageWidth);
  bbZoomWholePage.Caption := cxGetResourceString(@sdxMenuZoomWholePage);
  bbZoomTwoPages.Caption := cxGetResourceString(@sdxMenuZoomTwoPages);
  bbZoomFourPages.Caption := cxGetResourceString(@sdxMenuZoomFourPages);
  bbZoomMultiplePages.Caption := cxGetResourceString(@sdxMenuZoomMultiplyPages);
  bbZoomWidenToSourceWidth.Caption := cxGetResourceString(@sdxMenuZoomWidenToSourceWidth);
  bbZoomSetup.Caption := cxGetResourceString(@sdxMenuZoomSetup);
  bbViewPages.Caption := cxGetResourceString(@sdxMenuPages);
  bbViewPageHeaders.Caption := cxGetResourceString(@sdxMenuViewPagesHeaders);
  bbViewPageFooters.Caption := cxGetResourceString(@sdxMenuViewPagesFooters);
  bbViewSwitchToLeftPart.Caption := cxGetResourceString(@sdxMenuViewSwitchToLeftPart);
  bbViewSwitchToRightPart.Caption := cxGetResourceString(@sdxMenuViewSwitchToRightPart);
  bbViewSwitchToCenterPart.Caption := cxGetResourceString(@sdxMenuViewSwitchToCenterPart);
  bbViewHFSwitchHeaderFooter.Caption := cxGetResourceString(@sdxMenuViewHFSwitchHeaderFooter);
  bbViewHFClose.Caption := cxGetResourceString(@sdxMenuViewHFClose);

  bbFormat.Caption := cxGetResourceString(@sdxMenuFormat);
  bbFormatTitle.Caption := cxGetResourceString(@sdxMenuFormatTitle);
  bbFormatFootnotes.Caption := cxGetResourceString(@sdxMenuFormatFootnotes);
  bbFormatHeaderAndFooter.Caption := cxGetResourceString(@sdxMenuFormatHeaderAndFooter);
  bbFormatDateTime.Caption := cxGetResourceString(@sdxMenuFormatDateTime);
  bbFormatPageNumbering.Caption := cxGetResourceString(@sdxMenuFormatPageNumbering);
  bbFormatPageBackground.Caption := cxGetResourceString(@sdxMenuFormatPageBackground);
  bbFormatShrinkToPageWidth.Caption := cxGetResourceString(@sdxMenuFormatShrinkToPage);
  bbFormatHFBackground.Caption := cxGetResourceString(@sdxMenuFormatHFBackground);
  bbFormatHFClear.Caption := cxGetResourceString(@sdxMenuFormatHFClear);

  bbGotoPage.Caption := cxGetResourceString(@sdxMenuGotoPage);
  bbGotoFirstPage.Caption := cxGetResourceString(@sdxMenuGotoPageFirst);
  bbGotoPrevPage.Caption := cxGetResourceString(@sdxMenuGotoPagePrev);
  bbGotoNextPage.Caption := cxGetResourceString(@sdxMenuGotoPageNext);
  bbGotoLastPage.Caption := cxGetResourceString(@sdxMenuGotoPageLast);

  bbTools.Caption := cxGetResourceString(@sdxMenuTools);
  bbToolsCustomize.Caption := cxGetResourceString(@sdxMenuToolsCustomize);
  bbToolsOptions.Caption := cxGetResourceString(@sdxMenuToolsOptions);

  bbHelp.Caption := cxGetResourceString(@sdxMenuHelp);
  bbHelpTopics.Caption := cxGetResourceString(@sdxMenuHelpTopics);
  bbHelpAbout.Caption := cxGetResourceString(@sdxMenuHelpAbout);

  cbxPredefinedZoom.Caption := cxGetResourceString(@sdxMenuZoom) + ':';
  TcxImageComboBoxProperties(cbxPredefinedZoom.Properties).Items[8].Description := cxGetResourceString(@sdxPageWidth);
  TcxImageComboBoxProperties(cbxPredefinedZoom.Properties).Items[9].Description := cxGetResourceString(@sdxWholePage);
  TcxImageComboBoxProperties(cbxPredefinedZoom.Properties).Items[10].Description := cxGetResourceString(@sdxTwoPages);
  TcxImageComboBoxProperties(cbxPredefinedZoom.Properties).Items[11].Description := cxGetResourceString(@sdxFourPages);
  TcxImageComboBoxProperties(cbxPredefinedZoom.Properties).Items[12].Description := cxGetResourceString(@sdxWidenToSourceWidth);
  seActivePage.Caption := cxGetResourceString(@sdxMenuActivePage);

  bsiShortcutPreview.Caption := cxGetResourceString(@sdxMenuShortcutPreview);
  bsiShortcutExplorer.Caption := cxGetResourceString(@sdxMenuShortcutExplorer);
  bsiShortcutThumbnails.Caption := cxGetResourceString(@sdxMenuShortcutThumbnails);

  bsiNewMenuNewMenu.Caption := cxGetResourceString(@sdxMenuNewMenu);

  { hints }
  bbFileDesign.Hint := cxGetResourceString(@sdxHintFileDesign);
  bbFileSave.Hint := cxGetResourceString(@sdxHintFileSave);
  bbFileLoad.Hint := cxGetResourceString(@sdxHintFileLoad);
  bbFileClose.Hint := cxGetResourceString(@sdxHintFileClose);
  bbFilePrintDialog.Hint := cxGetResourceString(@sdxHintFilePrintDialog);
//  bbFilePrintSetup.Hint := cxGetResourceString(@sdxHintFilePrintSetup);
  bbFilePageSetup.Hint := cxGetResourceString(@sdxHintFilePageSetup);
  bbFileExit.Hint := cxGetResourceString(@sdxHintFileExit);
  bbExportToPDF.Hint := cxGetResourceString(@sdxHintExportToPDF);

  bbExplorerCreateNewFolder.Hint := cxGetResourceString(@sdxHintExplorerCreateFolder);
  bbExplorerDelete.Hint := cxGetResourceString(@sdxHintExplorerDelete);
  bbExplorerRename.Hint := cxGetResourceString(@sdxHintExplorerRename);
  bbExplorerProperties.Hint := cxGetResourceString(@sdxHintExplorerProperties);

  bbEditFind.Hint := cxGetResourceString(@sdxHintEditFind);
  bbEditFindNext.Hint := cxGetResourceString(@sdxHintEditFindNext);
  bbEditReplace.Hint := cxGetResourceString(@sdxHintEditReplace);

  bbInsertEditAutoText.Hint := cxGetResourceString(@sdxHintInsertEditAutoTextEntries);
  bbInsertHFPageNumber.Hint := cxGetResourceString(@sdxHintInsertPageNumber);
  bbInsertHFTotalPages.Hint := cxGetResourceString(@sdxHintInsertTotalPages);
  bbInsertHFPageOfPages.Hint := cxGetResourceString(@sdxHintInsertPageOfPages);
  bbInsertHFDateTime.Hint := cxGetResourceString(@sdxHintInsertDateTime);
  bbInsertHFDate.Hint := cxGetResourceString(@sdxHintInsertDate);
  bbInsertHFTime.Hint := cxGetResourceString(@sdxHintInsertTime);
  bbInsertHFUserName.Hint := cxGetResourceString(@sdxHintInsertUserName);
  bbInsertHFMachineName.Hint := cxGetResourceString(@sdxHintInsertMachineName);

  bbViewMargins.Hint := cxGetResourceString(@sdxHintViewMargins);
  bbViewMarginBar.Hint := cxGetResourceString(@sdxHintViewMarginsStatusBar);
  bbViewStatusBar.Hint := cxGetResourceString(@sdxHintViewPagesStatusBar);
  bbViewExplorer.Hint := cxGetResourceString(@sdxHintViewExplorer);
  bbViewThumbnails.Hint := cxGetResourceString(@sdxHintViewThumbnails);
  bbThumbnailsSmall.Hint := cxGetResourceString(@sdxHintThumbnailsSmall);
  bbThumbnailsLarge.Hint := cxGetResourceString(@sdxHintThumbnailsLarge);
  cbxPredefinedZoom.Hint := cxGetResourceString(@sdxHintViewZoom);
  bbZoomPercent100.Hint := cxGetResourceString(@sdxHintZoomPercent100);
  bbZoomPageWidth.Hint := cxGetResourceString(@sdxHintZoomPageWidth);
  bbZoomWholePage.Hint := cxGetResourceString(@sdxHintZoomWholePage);
  bbZoomTwoPages.Hint := cxGetResourceString(@sdxHintZoomTwoPages);
  bbZoomFourPages.Hint := cxGetResourceString(@sdxHintZoomFourPages);
  bbZoomMultiplePages.Hint := cxGetResourceString(@sdxHintZoomMultiplyPages);
  bbZoomWidenToSourceWidth.Hint := cxGetResourceString(@sdxHintZoomWidenToSourceWidth);
  bbZoomSetup.Hint := cxGetResourceString(@sdxHintZoomSetup);
  bbViewPageHeaders.Hint := cxGetResourceString(@sdxHintViewPagesHeaders);
  bbViewPageFooters.Hint := cxGetResourceString(@sdxHintViewPagesFooters);
  bbViewSwitchToLeftPart.Hint := cxGetResourceString(@sdxHintViewSwitchToLeftPart);
  bbViewSwitchToRightPart.Hint := cxGetResourceString(@sdxHintViewSwitchToRightPart);
  bbViewSwitchToCenterPart.Hint := cxGetResourceString(@sdxHintViewSwitchToCenterPart);
  bbViewHFSwitchHeaderFooter.Hint := cxGetResourceString(@sdxHintViewHFSwitchHeaderFooter);
  bbViewHFClose.Hint := cxGetResourceString(@sdxHintViewHFClose);

  bbFormatTitle.Hint := cxGetResourceString(@sdxHintFormatTitle);
  bbFormatFootnotes.Hint := cxGetResourceString(@sdxHintFormatFootnotes);
  bbFormatDateTime.Hint := cxGetResourceString(@sdxHintFormatDateTime);
  bbFormatPageNumbering.Hint := cxGetResourceString(@sdxHintFormatPageNumbering);
  bbFormatPageBackground.Hint := cxGetResourceString(@sdxHintFormatPageBackground);
  bbFormatShrinkToPageWidth.Hint := cxGetResourceString(@sdxHintFormatShrinkToPage);
  bbFormatHFBackground.Hint := cxGetResourceString(@sdxHintFormatHFBackground);
  bbFormatHFClear.Hint := cxGetResourceString(@sdxHintFormatHFClear);

  bbGotoFirstPage.Hint := cxGetResourceString(@sdxHintGotoPageFirst);
  bbGotoPrevPage.Hint := cxGetResourceString(@sdxHintGotoPagePrev);
  bbGotoNextPage.Hint := cxGetResourceString(@sdxHintGotoPageNext);
  bbGotoLastPage.Hint := cxGetResourceString(@sdxHintGotoPageLast);
  seActivePage.Hint := cxGetResourceString(@sdxHintActivePage);

  bbToolsCustomize.Hint := cxGetResourceString(@sdxHintToolsCustomize);
  bbToolsOptions.Hint := cxGetResourceString(@sdxHintToolsOptions);

  bbHelpTopics.Hint := cxGetResourceString(@sdxHintHelpTopics);
  bbHelpAbout.Hint := cxGetResourceString(@sdxHintHelpAbout);
end;

procedure TdxfmPreviewWdxBar.ZoomClick(Sender: TObject);
var
  PageXCount, PageYCount: Integer;
  ZoomMode: TdxPreviewZoomMode;
begin
  case TTagToInt(TComponent(Sender).Tag) of
    0: ZoomMode := pzmNone;
    1: ZoomMode := pzmPageWidth;
  else
    ZoomMode := pzmPages;
  end;
  PageXCount := 1;
  PageYCount := 1;
  if ZoomMode = pzmPages then
    case TTagToInt(TComponent(Sender).Tag) of
      3: PageXCount := 2;
      4:
        begin
          PageXCount := 2;
          PageYCount := 2;
        end;
      5: ReportLink.GetPageColRowCount(PageXCount, PageYCount);
    end;
  Preview.DoSetupZoomFactor(100, PageXCount, PageYCount, ZoomMode);
end;

procedure TdxfmPreviewWdxBar.SetBarItemVisibility(Item: TdxBarItem; Value: Boolean);
begin
  Item.Visible := VisibleTodxBarVisible(Value);
end;

procedure TdxfmPreviewWdxBar.seActivePageChange(Sender: TObject);
var
  V: Integer;
begin
  if Locked then Exit;

  V := seActivePage.EditValue;
  if V < TcxSpinEditProperties(seActivePage.Properties).MinValue then
    V := Round(TcxSpinEditProperties(seActivePage.Properties).MinValue);
  if V > TcxSpinEditProperties(seActivePage.Properties).MaxValue then
    V := Round(TcxSpinEditProperties(seActivePage.Properties).MaxValue);
  Preview.ActivePageIndex := V - 1;
end;

procedure TdxfmPreviewWdxBar.cbxPredefinedZoomChange(Sender: TObject);
begin
  Preview.SetZoomFactorByText(cbxPredefinedZoom.EditValue);
  Preview.UpdateControls;
  cbxPredefinedZoom.EditValue := AddPercentageChar(IntToStr(Preview.ZoomFactor));
end;

procedure TdxfmPreviewWdxBar.dxBarManagerBarVisibleChange(
  Sender: TdxBarManager; ABar: TdxBar);
begin
  if ABar = HFBar then
    UpdateHFState(ABar.Visible);
end;

procedure TdxfmPreviewWdxBar.miCustomizePopupClick(Sender: TObject);
begin
  ShowdxBarSubMenuEditor(pmPreview.ItemLinks);
end;

procedure TdxfmPreviewWdxBar.bbViewHFCloseClick(Sender: TObject);
begin
  ShowHFBar(False);
end;

procedure TdxfmPreviewWdxBar.bbFormatHeaderAndFooterClick(Sender: TObject);
begin
  ShowHFBar(TdxBarButton(Sender).Down);
end;

procedure TdxfmPreviewWdxBar.InsertHFClick(Sender: TObject);
begin
  Preview.DoInsertHF(Preview.HFFunctionList[TTagToInt(TComponent(Sender).Tag)]);
end;

procedure TdxfmPreviewWdxBar.bbFormatHFClearClick(Sender: TObject);
begin
  Preview.DoClearHF;
end;

procedure TdxfmPreviewWdxBar.bbViewHFSwitchHeaderFooterClick(Sender: TObject);
begin
  if TdxBarButton(Sender).Down then
    Preview.State := prsEditFooters
  else
    Preview.State := prsEditHeaders;
end;

procedure TdxfmPreviewWdxBar.bbFormatDateTimeClick(Sender: TObject);
begin
  ProcessMessages;
  Preview.DoShowFormatDateTimeDlg;
end;

procedure TdxfmPreviewWdxBar.bbFormatTitleClick(Sender: TObject);
begin
  ProcessMessages;
  Preview.DoFormatTitle;
end;

procedure TdxfmPreviewWdxBar.bbFormatFootnotesClick(Sender: TObject);
begin
  ProcessMessages;
  Preview.DoFormatFootnotes;
end;

procedure TdxfmPreviewWdxBar.bbFormatPageNumbersClick(Sender: TObject);
begin
  ProcessMessages;
  Preview.DoShowFormatPageNumbersDlg;
end;

procedure TdxfmPreviewWdxBar.SwitchPartClick(Sender: TObject);
begin
  Preview.HFEditPart := TdxPageTitlePart(TTagToInt(TdxBarButton(Sender).Tag));
end;

procedure TdxfmPreviewWdxBar.bbFormatHFBackgroundClick(Sender: TObject);
begin
  ProcessMessages;
  Preview.DoShowHFBackgroundDlg(CalcWindowPos(Sender));
end;

procedure TdxfmPreviewWdxBar.bbViewPageHeadersClick(Sender: TObject);
begin
  if not Locked then
    ReportLink.ShowPageHeader := TdxBarButton(Sender).Down;
end;

procedure TdxfmPreviewWdxBar.bbViewPageFootersClick(Sender: TObject);
begin
  if not Locked then
    ReportLink.ShowPageFooter := TdxBarButton(Sender).Down;
end;

procedure TdxfmPreviewWdxBar.bbToolsCustomizeClick(Sender: TObject);
begin
  dxBarManager.Customizing(True);
end;

procedure TdxfmPreviewWdxBar.bbToolsOptionsClick(Sender: TObject);
begin
  Preview.DoShowOptionsDlg;
end;

procedure TdxfmPreviewWdxBar.dxBarManagerShowCustomizingForm(Sender: TObject);
begin
  ShowShortCutBar(True);
end;

procedure TdxfmPreviewWdxBar.dxBarManagerHideCustomizingForm(Sender: TObject);
begin
  ShowShortCutBar(False);
end;

function TdxfmPreviewWdxBar.GetExplorerContextCommand(Index: Integer): TCustomdxPSExplorerContextCommand;
begin
  Result := TCustomdxPSExplorerContextCommand(FExplorerContextCommands.Items[Index]);
end;

function TdxfmPreviewWdxBar.GetExplorerContextCommandCount: Integer;
begin
  Result := FExplorerContextCommands.Count;
end;

function TdxfmPreviewWdxBar.GetLocked: Boolean;
begin
  Result := Preview.Locked;
end;

function TdxfmPreviewWdxBar.GetReportLink: TBasedxReportLink;
begin
  Result := Preview.ReportLink;
end;

function TdxfmPreviewWdxBar.CalcWindowPos(Sender: TObject): TPoint;
var
  Link: TdxBarItemLink;
  R: TRect;
begin
  Link := TdxBarItem(Sender).ClickItemLink;
  if (Link <> nil) and (Link.Control <> nil) then
  begin
    R := Link.ItemRect;
    MapWindowPoints(Link.BarControl.Handle, 0, R, 2);
    Result.X := R.Left;
    Result.Y := R.Bottom;
  end
  else
    Result := Preview.ClientOrigin;
end;

procedure TdxfmPreviewWdxBar.EnableItemsWhileBuilding(Value: Boolean);
var
  Items: TList;
  I, J: Integer;
begin
  Items := TList.Create;
  try
    for I := 0 to dxBarManager.Categories.Count - 1 do
    begin
      dxBarManager.GetItemsByCategory(I, Items);
      for J := 0 to Items.Count - 1 do
        TdxBarItem(Items[J]).Enabled := Value;
    end;
  finally
    Items.Free;
  end;
  bbFileExit.Enabled := True;
end;

procedure TdxfmPreviewWdxBar.DoShowExplorerPopup(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    pmExplorer.PopupFromCursorPos;
end;

function TdxfmPreviewWdxBar.ShortcutBar: TdxBar;
begin
  Result := dxBarManager.BarByCaption(DropAmpersand(cxGetResourceString(@sdxShortCutMenusBar)));
end;

procedure TdxfmPreviewWdxBar.ShowShortCutBar(Value: Boolean);
var
  Bar: TdxBar;
begin
  Bar := ShortcutBar;
  if Bar <> nil then
  begin
    Bar.Hidden := not Value;
    Bar.Visible := False;
    if Value then
    begin
      bsiShortcutPreview.ItemLinks := pmPreview.ItemLinks;
      bsiShortcutExplorer.ItemLinks := pmExplorer.ItemLinks;
      bsiShortcutThumbnails.ItemLinks := pmThumbnails.ItemLinks;
    end
    else
    begin
      pmPreview.ItemLinks := bsiShortcutPreview.ItemLinks;
      pmExplorer.ItemLinks := bsiShortcutExplorer.ItemLinks;
      pmThumbnails.ItemLinks := bsiShortcutThumbnails.ItemLinks;
    end;
  end;
end;

procedure TdxfmPreviewWdxBar.EnabledHFItems(Value: Boolean);
begin
  bbInsertHFPageNumber.Enabled := Value;
  bbInsertHFTotalPages.Enabled := Value;
  bbInsertHFPageOfPages.Enabled := Value;
  bbInsertHFDate.Enabled := Value;
  bbInsertHFTime.Enabled := Value;
  bbInsertHFDateTime.Enabled := Value;
  bbViewHFSwitchHeaderFooter.Enabled := Value;
  bbInsertHFUserName.Enabled := Value;
  bbInsertHFMachineName.Enabled := Value;
  bbViewHFClose.Enabled := Value;
  bbViewSwitchToLeftPart.Enabled := Value;
  bbViewSwitchToCenterPart.Enabled := Value;
  bbViewSwitchToRightPart.Enabled := Value;
  bbFormatHFClear.Enabled := Value;

  bliInsertAutoTextEntries.Enabled := Value;
  bbFormatHFBackground.Enabled := Value;
end;

function TdxfmPreviewWdxBar.HFBar: TdxBar;
begin
  Result := dxBarManager.BarByCaption(DropAmpersand(cxGetResourceString(@sdxHeaderFooterBar)));
end;

procedure TdxfmPreviewWdxBar.ShowHFBar(Value: Boolean);
var
  ABar: TdxBar;
begin
  if (ReportLink <> nil) and (rlcHeaderFooter in ReportLink.Capabilities) then
  begin
    ABar := HFBar;
    if ABar <> nil then
      ABar.Visible := Value;
  end;
end;

procedure TdxfmPreviewWdxBar.UpdateHFState(Value: Boolean);
begin
  EnabledHFItems(Value);
  Preview.Options.ZoomOnClick := not Value;
  bbFormatHeaderAndFooter.Down := Value;

  if not Value then
    Preview.State := prsNone
  else
    if bbViewHFSwitchHeaderFooter.Down then
      Preview.State := prsEditFooters
    else
      Preview.State := prsEditHeaders;
end;

procedure TdxfmPreviewWdxBar.LoadBarManagerDefaults;
var
  AIniFile: TCustomIniFile;
begin
  if dxPSStoringManager.BeginStoring(AIniFile) then
  try
    Preview.LoadFromIniFile(AIniFile, dxPSFormGetActualSectionName(Self));
  finally
    dxPSStoringManager.EndStoring(AIniFile);
  end;
end;

procedure TdxfmPreviewWdxBar.SaveBarManagerDefaults;
var
  AIniFile: TCustomIniFile;
begin
  if dxPSStoringManager.BeginStoring(AIniFile) then
  try
    Preview.SaveToIniFile(AIniFile, dxPSFormGetActualSectionName(Self));
  finally
    dxPSStoringManager.EndStoring(AIniFile);
  end;
end;

procedure TdxfmPreviewWdxBar.LoadBarManager(
  AIniFile: TCustomIniFile; const ASectionName: string);
const
  StorageType: array[Boolean] of TdxBarStoringKind = (skIni, skReg);
var
  AIsSameVersion: Boolean;
begin
  AIsSameVersion :=
    (AIniFile.ReadInteger(dxValidatePath(ASectionName) + 'Version', 'Major', 0) = dxPSVerMajor) and
    (AIniFile.ReadInteger(dxValidatePath(ASectionName) + 'Version', 'Minor', 0) = dxPSVerMinor);

  if AIsSameVersion then
  begin
    TdxBarManagerAccess(dxBarManager).LoadBarManager(
      AIniFile, dxValidatePath(ASectionName) + sdxToolBars,
      StorageType[AIniFile is TRegistryIniFile]);
    pmPreview.ItemLinks := bsiShortcutPreview.ItemLinks;
    pmExplorer.ItemLinks := bsiShortcutExplorer.ItemLinks;
    pmThumbnails.ItemLinks := bsiShortcutThumbnails.ItemLinks;
  end
  else
  begin
    bsiShortcutPreview.ItemLinks := pmPreview.ItemLinks;
    bsiShortcutExplorer.ItemLinks := pmExplorer.ItemLinks;
    bsiShortcutThumbnails.ItemLinks := pmThumbnails.ItemLinks;
  end;
end;

procedure TdxfmPreviewWdxBar.SaveBarManager(
  AIniFile: TCustomIniFile; const ASectionName: string);
const
  StorageType: array[Boolean] of TdxBarStoringKind = (skIni, skReg);
begin
  AIniFile.WriteInteger(dxValidatePath(ASectionName) + 'Version', 'Major', dxPSVerMajor);
  AIniFile.WriteInteger(dxValidatePath(ASectionName) + 'Version', 'Minor', dxPSVerMinor);
  TdxBarManagerAccess(dxBarManager).SaveBarManager(
    AIniFile, dxValidatePath(ASectionName) + sdxToolBars,
    StorageType[AIniFile is TRegistryIniFile]);
end;

procedure TdxfmPreviewWdxBar.bbExportToPDFClick(Sender: TObject);
begin
  dxPSExportToPDF(ReportLink);
end;

procedure TdxfmPreviewWdxBar.PreviewAddExplorerCommand(
  Sender: TObject; ACommand: TCustomdxPSExplorerContextCommand);

  procedure AddBarControl(AItemLinks: TdxBarItemLinks; ABarButton: TdxBarButton);
  begin
    with AItemLinks.Add do
    begin
      Item := ABarButton;
      Index := 0;
      BringToTopInRecentList(True);
    end;
  end;

  procedure BeginGroup(AItemLinks: TdxBarItemLinks);
  begin
    if AItemLinks.Count <> 0 then
      AItemLinks[0].BeginGroup := True;
  end;

var
  BarButton: TdxBarButton;
begin
  if ACommand is TdxPSExplorerContextCommandSeparator then
  begin
    BeginGroup(bbExplorer.ItemLinks);
    BeginGroup(dxBarManager.BarByCaption(cxGetResourceString(@sdxExplorerBar)).ItemLinks);
    BeginGroup(pmExplorer.ItemLinks);
  end
  else
  begin
    BarButton := TdxBarButton.Create(Self);
    with BarButton do
    begin
      Glyph.Assign(ACommand.Bitmap);
      Glyph.SourceDPI := dxDefaultDPI;
      Caption := ACommand.Caption;
      Enabled := ACommand.Enabled;
      Hint := ACommand.Hint;
      ShortCut := ACommand.ShortCut;
      Tag := MakeTTag(ACommand);

      Category := dxBarManager.Categories.IndexOf(DropAmpersand(cxGetResourceString(@sdxMenuExplorer)));
      OnClick := ExplorerContextCommandClick;
    end;

    AddBarControl(bbExplorer.ItemLinks, BarButton);
    AddBarControl(dxBarManager.BarByCaption(cxGetResourceString(@sdxExplorerBar)).ItemLinks, BarButton);
    AddBarControl(pmExplorer.ItemLinks, BarButton);
    ACommand.Data := BarButton;

    if FExplorerContextCommands.IndexOf(ACommand) = -1 then
      FExplorerContextCommands.Add(ACommand);
  end;
end;

procedure TdxfmPreviewWdxBar.PreviewInitContent(Sender: TObject);

  procedure AddEndEllipsis(AnItem: TdxBarButton);
  begin
    with AnItem do
      if AnsiLastChar(Caption) <> '.' then
        Caption := dxPSUtl.AddEndEllipsis(Caption);
  end;

begin
  if ReportLink <> nil then
    TcxSpinEditProperties(seActivePage.Properties).MaxValue := ReportLink.PageCount;
  cbxPredefinedZoom.EditValue := AddPercentageChar(IntToStr(Preview.ZoomFactor));

  if Preview.IsExplorerAvailable then
    TWinControlAccess(Preview.ExplorerTree.Control).OnMouseUp := DoShowExplorerPopup;

  if not Preview.IsExplorerAvailable then
  begin
    AddEndEllipsis(bbFileSave);
    AddEndEllipsis(bbFileLoad);
  end;
end;

procedure TdxfmPreviewWdxBar.PreviewLoadProperties(
  Sender: TObject; AIniFile: TCustomIniFile; const ASectionName: string);
begin
  LoadBarManager(AIniFile, ASectionName);
end;

procedure TdxfmPreviewWdxBar.PreviewSaveProperties(
  Sender: TObject; AIniFile: TCustomIniFile; const ASectionName: string);
begin
  SaveBarManager(AIniFile, ASectionName);
end;

procedure TdxfmPreviewWdxBar.PreviewStyleListChanged(Sender: TObject);
begin
  ReportLink.BuildPageSetupMenu(bliPrintStyles, bbDefinePrintStyles, True);
end;

procedure TdxfmPreviewWdxBar.PreviewUpdateControls(Sender: TObject);
const
  ButtonStyles: array[Boolean] of TdxBarButtonStyle = (bsDefault, bsDropDown);
var
  Bar: TdxBar;
  PagesExists, ABoolValue: Boolean;
  PageXCount, PageYCount, Index: Integer;
begin
  if Locked or (csDestroying in ComponentState) then Exit;

  PagesExists := Preview.Preview.PageCount > 0;
  Preview.BeginUpdate;
  try
    EnableItemsWhileBuilding(True);

    bbFileDesign.Enabled := Preview.CanDesign;
    bbFileRebuild.Enabled := Preview.CanRebuild;
    bbFileSave.Enabled := Preview.CanSaveReport;
    bbFileLoad.Enabled := Preview.CanLoadReport;
    bbFileClose.Enabled := Preview.CanUnloadReport;
    bbFilePrint.Enabled := Preview.CanPrint;
    bbExportToPDF.Enabled := Preview.CanExport;
    bbFilePrintDialog.Enabled := Preview.CanPrintDialog;
    bbFilePageSetup.Enabled := Preview.CanPageSetup;
    bbFilePageSetup.ButtonStyle := ButtonStyles[Preview.CanPrintStyle];
    bbFileExit.Enabled := not Preview.IsPrinting;

    bbExplorer.Enabled := Preview.IsExplorerAvailable;
    bbExplorerCreateNewFolder.Enabled := Preview.IsExplorerAvailable and Preview.ExplorerTree.CanCreateFolder;
    bbExplorerDelete.Enabled := Preview.IsExplorerAvailable and Preview.ExplorerTree.CanDeleteSelection;
    bbExplorerRename.Enabled := Preview.IsExplorerAvailable and Preview.ExplorerTree.CanRenameSelectedItem;
    bbExplorerProperties.Enabled := Preview.IsExplorerAvailable and Preview.ExplorerTree.CanShowPropertySheetsForSelectedItem;

    bbFormatPageBackground.Enabled := Preview.IsEnabled(peoPageBackground) and not Preview.IsPrinting;
    bbFormatShrinkToPageWidth.Enabled := PagesExists and not Preview.IsPrinting;
    bbFormatFootnotes.Enabled := (ReportLink <> nil) and ReportLink.CanChangeFootnotes;
    bbFormatTitle.Enabled := (ReportLink <> nil) and ReportLink.CanChangeTitle;
    SetBarItemVisibility(bbFormatFootnotes, (ReportLink <> nil) and (rlcFootnotes in ReportLink.Capabilities));
    SetBarItemVisibility(bbFormatTitle, (ReportLink <> nil) and (rlcTitle in ReportLink.Capabilities));
    if ReportLink <> nil then
      bbFormatShrinkToPageWidth.Down := ReportLink.ShrinkToPageWidth;

    bbZoomPageWidth.Enabled := PagesExists;
    bbZoomPercent100.Enabled := PagesExists;
    bbZoomWholePage.Enabled := PagesExists;
    bbZoomTwoPages.Enabled := PagesExists and (Preview.Preview.PageCount > 1);
    bbZoomFourPages.Enabled := PagesExists and (Preview.Preview.PageCount > 3);
    bbZoomMultiplePages.Enabled := PagesExists;
    if ReportLink <> nil then
    begin
      ReportLink.GetPageColRowCount(PageXCount, PageYCount);
      bbZoomWidenToSourceWidth.Enabled := PageXCount > 1;
    end;
    cbxPredefinedZoom.Enabled := bbZoomPageWidth.Enabled;
    bbZoomSetup.Enabled := bbZoomPageWidth.Enabled;

    bbGoToFirstPage.Enabled := PagesExists and (TdxPreviewAccess(Preview.Preview).SelPageIndex <> 0);
    bbGoToPrevPage.Enabled := PagesExists and (TdxPreviewAccess(Preview.Preview).SelPageIndex <> 0);
    bbGoToNextPage.Enabled := PagesExists and (TdxPreviewAccess(Preview.Preview).SelPageIndex <> Preview.PageCount - 1);
    bbGoToLastPage.Enabled := PagesExists and (TdxPreviewAccess(Preview.Preview).SelPageIndex <> Preview.PageCount - 1);
    seActivePage.Enabled := Preview.PageCount > 1;

    bbHelp.Enabled := Preview.IsEnabled(peoHelp);
    bbHelpTopics.Enabled := Preview.IsEnabled(peoHelp);

    bbViewMargins.Down := pvoPageMargins in Preview.Options.VisibleOptions;
    bbViewMarginBar.Down := Preview.Options.ShowMarginBar;
    bbViewStatusBar.Down := Preview.Options.ShowStatusBar;
    SetBarItemVisibility(bbViewExplorer, Preview.IsExplorerAvailable);
    bbViewExplorer.Down := Preview.Options.ShowExplorer;
    bbViewThumbnails.Down := Preview.Options.ThumbnailsOptions.Visible;
    bbThumbnailsSmall.Down := Preview.Options.ThumbnailsOptions.Size = tsSmall;
    bbThumbnailsLarge.Down := Preview.Options.ThumbnailsOptions.Size = tsLarge;
    if ReportLink <> nil then
    begin
      bbViewPageHeaders.Down := ReportLink.ShowPageHeader;
      bbViewPageFooters.Down := ReportLink.ShowPageFooter;
    end;

    SetBarItemVisibility(bbFileDesign, Preview.IsVisible(pvoReportDesign));
    SetBarItemVisibility(bbFileSave, Preview.IsCommandSaveReportVisible);
    SetBarItemVisibility(bbFileLoad, Preview.IsCommandLoadReportVisible);
    SetBarItemVisibility(bbFileClose, Preview.IsCommandUnloadReportVisible);
    SetBarItemVisibility(bbFilePrint, Preview.IsVisible(pvoPrint));
    SetBarItemVisibility(bbFilePrintDialog, Preview.IsVisible(pvoPrint) and Preview.IsVisible(pvoPrintDialog));
    SetBarItemVisibility(bbFilePageSetup, Preview.IsVisible(pvoPageSetup));

    SetBarItemVisibility(bbExplorer, Preview.IsExplorerAvailable);
    SetBarItemVisibility(bbExplorerCreateNewFolder, Preview.IsExplorerAvailable);
    SetBarItemVisibility(bbExplorerDelete, Preview.IsExplorerAvailable);
    SetBarItemVisibility(bbExplorerRename, Preview.IsExplorerAvailable);
    SetBarItemVisibility(bbExplorerProperties, Preview.IsExplorerAvailable);
    SetBarItemVisibility(bsiShortcutExplorer, Preview.IsExplorerAvailable);

    SetBarItemVisibility(bbFormatPageBackground, Preview.IsVisible(pvoPageBackground));
    SetBarItemVisibility(bbHelp, Preview.IsVisible(pvoHelp));
    SetBarItemVisibility(bbHelpTopics, Preview.IsVisible(pvoHelp));
    SetBarItemVisibility(bbToolsOptions, Preview.IsVisible(pvoPreferences));

    ABoolValue := (ReportLink <> nil) and (ReportLink.StyleManager <> nil) and (rlcHeaderFooter in ReportLink.Capabilities);
    SetBarItemVisibility(bsiInsertHFAutoText, ABoolValue);
    SetBarItemVisibility(bbInsertEditAutoText, ABoolValue);
    SetBarItemVisibility(bsiInsertAutoText, ABoolValue);
    SetBarItemVisibility(bliInsertAutoTextEntries, ABoolValue);

    ABoolValue := (ReportLink <> nil) and (rlcHeaderFooter in ReportLink.Capabilities);
    SetBarItemVisibility(bbFormatDateTime, ABoolValue);
    SetBarItemVisibility(bbFormatPageNumbering, ABoolValue);
    SetBarItemVisibility(bbInsert, ABoolValue);
    SetBarItemVisibility(bbInsertHFPageNumber, ABoolValue);
    SetBarItemVisibility(bbInsertHFTotalPages, ABoolValue);
    SetBarItemVisibility(bbInsertHFPageOfPages, ABoolValue);
    SetBarItemVisibility(bbInsertHFDateTime, ABoolValue);
    SetBarItemVisibility(bbInsertHFDate, ABoolValue);
    SetBarItemVisibility(bbInsertHFTime, ABoolValue);
    SetBarItemVisibility(bbInsertHFUserName, ABoolValue);
    SetBarItemVisibility(bbInsertHFMachineName, ABoolValue);
    SetBarItemVisibility(bbFormatHeaderAndFooter, ABoolValue);
    SetBarItemVisibility(bbViewPageHeaders, ABoolValue);
    SetBarItemVisibility(bbViewPageFooters, ABoolValue);

    if seActivePage.Enabled then
    begin
      TcxSpinEditProperties(seActivePage.Properties).MinValue := 1;
      if ReportLink <> nil then
      begin
        TcxSpinEditProperties(seActivePage.Properties).MaxValue := ReportLink.PageCount;
        seActivePage.EditValue := TdxPreviewAccess(Preview.Preview).SelPageIndex + 1;
      end
    end
    else
      seActivePage.EditValue := -1;

    if Preview.IsBuilding or Preview.IsPrinting then
      EnableItemsWhileBuilding(False);

    if HFBar <> nil then
      EnabledHFItems(HFBar.Visible);

    { Categories visibility }

    { Edit }
    Index := dxBarManager.Categories.IndexOf(DropAmpersand(cxGetResourceString(@sdxMenuEdit)));
    if Index <> -1 then
      dxBarManager.CategoryVisible[Index] := False;

    { Help }
    Index := dxBarManager.Categories.IndexOf(DropAmpersand(cxGetResourceString(@sdxMenuHelp)));
    if Index <> -1 then
      dxBarManager.CategoryVisible[Index] :=
        (bbHelpTopics.Visible = ivAlways) or (bbHelpAbout.Visible = ivAlways);

    { Shortcut Menus }
    Index := dxBarManager.Categories.IndexOf(DropAmpersand(cxGetResourceString(@sdxMenuShortCutMenus)));
    if Index <> -1 then
      dxBarManager.CategoryVisible[Index] := False;

    { Explorer }
    Index := dxBarManager.Categories.IndexOf(DropAmpersand(cxGetResourceString(@sdxMenuExplorer)));
    if Index <> -1 then
      dxBarManager.CategoryVisible[Index] := Preview.IsExplorerAvailable;

    { AutoText }
    Bar := dxBarManager.BarByCaption(cxGetResourceString(@sdxAutoTextBar));
    if Bar <> nil then
      Bar.Hidden := not Preview.IsAutoHFTextEntriesAvailable;

    { Explorer }
    Bar := dxBarManager.BarByCaption(cxGetResourceString(@sdxExplorerToolBar));
    if Bar <> nil then
      Bar.Hidden := not Preview.IsExplorerAvailable;

    bbFilePrint.Hint := cxGetResourceString(@sdxHintFilePrint) + GetCurrentPrinterAsHint;

    Preview.UpdateExplorerContextCommands;
  finally
    Preview.CancelUpdate;
  end;
end;

procedure TdxfmPreviewWdxBar.PreviewUpdateExplorerCommands(Sender: TObject);
var
  I: Integer;
begin
  if not (csDestroying in ComponentState) then
  begin
    for I := 0 to ExplorerContextCommandCount - 1 do
    begin
      with ExplorerContextCommands[I] do
        TdxBarButton(Data).Enabled := Enabled;
    end;
  end;
end;

procedure TdxfmPreviewWdxBar.PreviewZoomFactorChanged(Sender: TObject);
begin
  cbxPredefinedZoom.EditValue := AddPercentageChar(IntToStr(Preview.ZoomFactor));
end;

procedure TdxfmPreviewWdxBar.PreviewCanShowMarginHint(Sender: TObject; var AAllow: Boolean);
begin
  AAllow := ActiveBarControl = nil;
end;

procedure TdxfmPreviewWdxBar.PreviewHFTextEntriesChanged(Sender: TObject);
begin
  if Preview.IsAutoHFTextEntriesAvailable then
    ReportLink.StyleManager.BuildAutoHFTextEntriesMenu(
      bliInsertAutoTextEntries, bbInsertEditAutoText, True);
end;

procedure TdxfmPreviewWdxBar.PreviewPreviewDblClick(Sender: TObject);
begin
  ShowHFBar(False);
end;

procedure TdxfmPreviewWdxBar.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := Preview.CanClosePreviewWindow;
end;

{ TdxPSAdvancedPreviewDialogStyleInfo }

class function TdxPSAdvancedPreviewDialogStyleInfo.CreatePreviewWindow: TdxPSCustomPreviewWindow;
begin
  with TdxfmPreviewWdxBar.Create(nil) do
    Result := Preview;
end;

class function TdxPSAdvancedPreviewDialogStyleInfo.GetName: string;
begin
  Result := 'Advanced';
end;

initialization
  dxPSPopupMenuController.RegisterBuilder(TdxBarPSPopupMenuBuilder);
  dxPSAutoHFTextMenuBuilderFactory.RegisterBuilder(TdxBarPSAutoHFTextMenuBuilder);
  dxPSPageSetupMenuBuilderFactory.RegisterBuilder(TdxBarPSPageSetupMenuBuilder);
  dxPSPreviewDialogManager.RegisterPreviewDialog(TdxPSAdvancedPreviewDialogStyleInfo);
  dxPSGlbl.PSCanShowHintFunc := dxBar_DoesNotHaveActivePopup;

finalization
  dxPSGlbl.PSCanShowHintFunc := nil;
  dxPSPreviewDialogManager.UnregisterPreviewDialog(TdxPSAdvancedPreviewDialogStyleInfo);
  dxPSPageSetupMenuBuilderFactory.UnregisterBuilder(TdxBarPSPageSetupMenuBuilder);
  dxPSAutoHFTextMenuBuilderFactory.UnregisterBuilder(TdxBarPSAutoHFTextMenuBuilder);
  dxPSPopupMenuController.UnregisterBuilder(TdxBarPSPopupMenuBuilder);

end.
