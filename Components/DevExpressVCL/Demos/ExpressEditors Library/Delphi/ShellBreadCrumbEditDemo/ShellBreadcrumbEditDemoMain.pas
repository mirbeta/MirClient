unit ShellBreadcrumbEditDemoMain;

{$I cxVer.inc}

interface

uses
{$IFDEF EXPRESSBARS}
  dxBar,
{$ENDIF}
{$IFDEF EXPRESSSKINS}
  dxSkinBlue, dxSkinsForm,
{$ENDIF}
  Windows, Messages, SysUtils, Forms, ComCtrls, ShlObj, cxShellCommon, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, Menus, cxClasses, cxContainer,
  cxEdit, ImgList, Controls, Classes, ActnList, dxBreadcrumbEdit, dxShellBreadcrumbEdit,
  Buttons, cxShellTreeView, cxSplitter, cxShellControls, cxShellListView,
  StdCtrls, ExtCtrls, dxGDIPlusClasses, ShellAPI, cxTreeView, cxListView;

type
  TdxBreadcrumbEditDemoForm = class(TForm)
    acBrowseParent: TAction;
    acRefresh: TAction;
    ActionList1: TActionList;
    bceAddressBar: TdxShellBreadcrumbEdit;
    bvTopSpacer: TBevel;
    edStyleController: TcxEditStyleController;
    cxLookAndFeelController: TcxLookAndFeelController;
    ilImages: TcxImageList;
    imStatusBar: TImage;
    imToolBar: TImage;
    lbInfo: TLabel;
    lbName: TLabel;
    lvFiles: TcxShellListView;
    miViewDetails: TMenuItem;
    miViewExtraLargeIcons: TMenuItem;
    miViewIcons: TMenuItem;
    miViewLargeIcons: TMenuItem;
    miViewList: TMenuItem;
    miViewSmallIcons: TMenuItem;
    pbSelectedItemIcon: TPaintBox;
    pmView: TPopupMenu;
    pnlAddressBarContainer: TPanel;
    pnlStatusBar: TPanel;
    pnlToolBar: TPanel;
    sbHelp: TSpeedButton;
    sbView: TSpeedButton;
    sltFolderTree: TcxSplitter;
    tvFolders: TcxShellTreeView;
    acAbout: TAction;
    procedure acBrowseParentExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure bceAddressBarPathSelected(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvFilesCurrentFolderChanged(Sender: TcxCustomShellListView);
    procedure lvFilesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure miViewDetailsClick(Sender: TObject);
    procedure pbSelectedItemIconPaint(Sender: TObject);
    procedure sbViewClick(Sender: TObject);
    procedure acAboutExecute(Sender: TObject);
  private
  {$IFDEF EXPRESSBARS}
    FBarManager: TdxBarManager;
    FViewStylePopupMenu: TdxBarPopupMenu;
  {$ENDIF}
  {$IFDEF EXPRESSSKINS}
    FSkinController: TdxSkinController;
  {$ENDIF}
    ShellLargeImages: TImageList;
    procedure InitializeBarManager;
    procedure InitializeLookAndFeel;
    procedure InitializeShellLargeImages;
    procedure UpdateCaption;
    procedure UpdateSelectionInfo;
  end;

var
  dxBreadcrumbEditDemoForm: TdxBreadcrumbEditDemoForm;

implementation

uses
{$IFDEF EXPRESSBARS}
  {$IFDEF EXPRESSSKINS}
    dxSkinsdxBarPainter,
  {$ENDIF}
{$ENDIF}
  dxCore, dxOffice11, CommCtrl, cxDWMApi, AboutDemoForm;

{$R *.dfm}

type
  TcxInnerShellListViewAccess = class(TcxInnerShellListView);

const
  sdxFormCaption = 'DevExpress Explorer';

  sdxItemsCount = '%d items';
  sdxItemsSelected = '%d items selected';

{ TdxBreadcrumbEditDemoForm }

procedure TdxBreadcrumbEditDemoForm.InitializeBarManager;
{$IFDEF EXPRESSBARS}

  procedure ConvertMenuItem(AButton: TdxBarButton; AMenuItem: TMenuItem);
  begin
    AButton.ButtonStyle := bsChecked;
    AButton.Caption := AMenuItem.Caption;
    AButton.OnClick := AMenuItem.OnClick;
    AButton.GroupIndex := 2;
    AButton.Down := AMenuItem.Checked;
    AButton.Tag := AMenuItem.Tag;
  end;

var
  I: Integer;
{$ENDIF}
begin
{$IFDEF EXPRESSBARS}
  FBarManager := TdxBarManager.Create(Self);
  FBarManager.Style := bmsUseLookAndFeel;
  FViewStylePopupMenu := TdxBarPopupMenu.Create(Self);
  FViewStylePopupMenu.BarManager := FBarManager;
  for I := 0 to pmView.Items.Count - 1 do
    ConvertMenuItem(FViewStylePopupMenu.ItemLinks.AddButton.Item as TdxBarButton, pmView.Items[I]);
{$ENDIF}
end;

procedure TdxBreadcrumbEditDemoForm.InitializeLookAndFeel;
begin
  pnlAddressBarContainer.DoubleBuffered := True;
  if IsWinVistaOrLater and IsXPManifestEnabled then
  begin
    lvFiles.InnerListView.RowSelect := True;
  {$IFDEF DELPHI11}
    ListView_SetExtendedListViewStyle(lvFiles.InnerListView.Handle,
      ListView_GetExtendedListViewStyle(lvFiles.InnerListView.Handle) or LVS_EX_JUSTIFYCOLUMNS);
    TreeView_SetExtendedStyle(tvFolders.InnerTreeView.Handle, TVS_EX_FADEINOUTEXPANDOS, TVS_EX_FADEINOUTEXPANDOS);
  {$ENDIF}
    tvFolders.ShowLines := False;
    tvFolders.TreeHotTrack := True;
    tvFolders.InnerTreeView.RowSelect := True;
  end;
{$IFDEF DELPHI11}
  GlassFrame.Top := bceAddressBar.Height + bvTopSpacer.Height;
  GlassFrame.Enabled := True;
{$IFNDEF DELPHI12}
  bceAddressBar.OnGlass := True;
{$ENDIF}
  if not IsCompositionEnabled then
{$ENDIF}
  begin
    bceAddressBar.Properties.Borders := [bBottom];
    pnlAddressBarContainer.BorderWidth := 0;
    bvTopSpacer.Visible := False;
  {$IFDEF EXPRESSSKINS}
    FSkinController := TdxSkinController.Create(Self);
  {$ENDIF}
  end;
{$IFDEF EXPRESSSKINS}
  cxLookAndFeelController.NativeStyle := False;
  cxLookAndFeelController.SkinName := 'Blue';
{$ENDIF}
end;

procedure TdxBreadcrumbEditDemoForm.InitializeShellLargeImages;
var
  AFileInfo: TSHFileInfo;
begin
  ShellLargeImages := TImageList.Create(Self);
  ShellLargeImages.ShareImages := True;
  ShellLargeImages.Handle := SHGetFileInfo('', 0,
    AFileInfo, SizeOf(AFileInfo), SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
  ImageList_SetBkColor(ShellLargeImages.Handle, CLR_NONE);
end;

procedure TdxBreadcrumbEditDemoForm.lvFilesCurrentFolderChanged(Sender: TcxCustomShellListView);
begin
  UpdateSelectionInfo;
end;

procedure TdxBreadcrumbEditDemoForm.lvFilesSelectItem(
  Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  UpdateSelectionInfo;
end;

procedure TdxBreadcrumbEditDemoForm.miViewDetailsClick(Sender: TObject);

  procedure SetupIconsView(AThumbnailSize: Integer);
  begin
    lvFiles.ViewStyle := vsIcon;
    lvFiles.ThumbnailOptions.BeginUpdate;
    try
      lvFiles.ThumbnailOptions.ShowThumbnails := AThumbnailSize > 0;
      if lvFiles.ThumbnailOptions.ShowThumbnails then
      begin
        lvFiles.ThumbnailOptions.Height := AThumbnailSize;
        lvFiles.ThumbnailOptions.Width := AThumbnailSize;
        ListView_SetIconSpacing(lvFiles.InnerListView.Handle, AThumbnailSize + 13,
          AThumbnailSize + 40);
      end
      else
        SendMessage(lvFiles.InnerListView.Handle, LVM_SETICONSPACING, 0, LPARAM(-1));
    finally
      lvFiles.ThumbnailOptions.EndUpdate;
    end;
  end;

begin
  case (Sender as TComponent).Tag of
    0: SetupIconsView(256);
    1: SetupIconsView(96);
    2: SetupIconsView(0);
    3: lvFiles.ViewStyle := vsSmallIcon;
    4: lvFiles.ViewStyle := vsList;
    5: lvFiles.ViewStyle := vsReport;
  end;
end;

procedure TdxBreadcrumbEditDemoForm.FormCreate(Sender: TObject);
begin
  Font.Size := 10;
  InitializeBarManager;
  InitializeLookAndFeel;
  InitializeShellLargeImages;
  UpdateSelectionInfo;
  UpdateCaption;
end;

procedure TdxBreadcrumbEditDemoForm.FormShow(Sender: TObject);
begin
  pnlAddressBarContainer.Realign;
end;

procedure TdxBreadcrumbEditDemoForm.acAboutExecute(Sender: TObject);
begin
  ShowAboutDemoForm;
end;

procedure TdxBreadcrumbEditDemoForm.acBrowseParentExecute(Sender: TObject);
begin
  bceAddressBar.BrowseParent;
end;

procedure TdxBreadcrumbEditDemoForm.acRefreshExecute(Sender: TObject);
begin
  bceAddressBar.UpdateContent;
  tvFolders.UpdateContent;
  lvFiles.UpdateContent;
end;

procedure TdxBreadcrumbEditDemoForm.bceAddressBarPathSelected(Sender: TObject);
begin
  UpdateCaption;
end;

procedure TdxBreadcrumbEditDemoForm.UpdateSelectionInfo;

  procedure GetItemInfo(APidl: PItemIDList; var AFileInfo: TSHFileInfo; ADisposePidl: Boolean = False);
  begin
    ZeroMemory(@AFileInfo, SizeOf(AFileInfo));
    cxShellGetThreadSafeFileInfo(PChar(APidl), 0, AFileInfo, SizeOf(AFileInfo),
      SHGFI_PIDL or SHGFI_DISPLAYNAME or SHGFI_TYPENAME or SHGFI_SYSICONINDEX);
    if ADisposePidl then
      DisposePidl(APidl);
  end;

  procedure GetItemInfoByItemIndex(AIndex: Integer; var AFileInfo: TSHFileInfo);
  var
    AInnerListView: TcxInnerShellListViewAccess;
  begin
    AInnerListView := TcxInnerShellListViewAccess(lvFiles.InnerListView);
    AInnerListView.ItemProducer.LockRead;
    try
      GetItemInfo(TcxShellItemInfo(AInnerListView.ItemProducer.Items[
        AInnerListView.Selected.Index]).FullPIDL, AFileInfo);
    finally
      AInnerListView.ItemProducer.UnlockRead;
    end;
  end;

var
  AFileInfo: TSHFileInfo;
begin
  if lvFiles.InnerListView.Selected = nil then
  begin
    GetItemInfo(lvFiles.AbsolutePIDL, AFileInfo, True);
    lbName.Caption := AFileInfo.szDisplayName;
    lbInfo.Caption := Format(sdxItemsCount, [lvFiles.InnerListView.Items.Count]);
    pbSelectedItemIcon.Tag := AFileInfo.iIcon;
  end
  else
  begin
    pbSelectedItemIcon.Tag := lvFiles.InnerListView.Selected.ImageIndex;
    if lvFiles.InnerListView.SelCount > 1 then
    begin
      lbName.Caption := Format(sdxItemsSelected, [lvFiles.InnerListView.SelCount]);
      lbInfo.Caption := '';
    end
    else
    begin
      GetItemInfoByItemIndex(lvFiles.InnerListView.Selected.Index, AFileInfo);
      lbName.Caption := AFileInfo.szDisplayName;
      lbInfo.Caption := AFileInfo.szTypeName;
    end;
  end;
  pbSelectedItemIcon.Invalidate;
end;

procedure TdxBreadcrumbEditDemoForm.pbSelectedItemIconPaint(Sender: TObject);
begin
  ShellLargeImages.Draw(pbSelectedItemIcon.Canvas, 0, 0, pbSelectedItemIcon.Tag);
end;

procedure TdxBreadcrumbEditDemoForm.sbViewClick(Sender: TObject);
var
  P: TPoint;
begin
  P := sbView.ClientToScreen(Point(0, sbView.Height));
{$IFDEF EXPRESSBARS}
  FViewStylePopupMenu.Popup(P.X, P.Y);
{$ELSE}
  pmView.Popup(P.X, P.Y);
{$ENDIF}
end;

procedure TdxBreadcrumbEditDemoForm.UpdateCaption;
begin
  if bceAddressBar.Selected <> nil then
    Caption := sdxFormCaption + ' - ' + bceAddressBar.Selected.Name
  else
    Caption := sdxFormCaption;
end;

end.
