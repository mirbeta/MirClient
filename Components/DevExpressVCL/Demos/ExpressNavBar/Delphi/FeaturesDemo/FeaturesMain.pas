unit FeaturesMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ImgList, ActnList, ComCtrls, Menus,
  dxNavBarStyles, dxNavBarCollns, dxNavBarBase, dxNavBar, NavBarUtils,
  cxClasses;

type
  TfmFeaturesMain = class(TForm)
    nbMain: TdxNavBar;
    bgMail: TdxNavBarGroup;
    bgNews: TdxNavBarGroup;
    biInbox: TdxNavBarItem;
    biOutbox: TdxNavBarItem;
    biSentItems: TdxNavBarItem;
    biDeletedItems: TdxNavBarItem;
    biDrafts: TdxNavBarItem;
    biNews: TdxNavBarItem;
    imgSmall: TImageList;
    imgLarge: TImageList;
    bgOther: TdxNavBarGroup;
    biMyComputer: TdxNavBarItem;
    Panel1: TPanel;
    stThirdGroupBackGround: TdxNavBarStyleItem;
    stThirdGroupHeader: TdxNavBarStyleItem;
    stThirdGroupHeaderHotTracked: TdxNavBarStyleItem;
    stThirdGroupHeaderPressed: TdxNavBarStyleItem;
    mmMain: TMainMenu;
    miFile: TMenuItem;
    miExit: TMenuItem;
    miOptions: TMenuItem;
    miHelp: TMenuItem;
    miProducts: TMenuItem;
    miDownloads: TMenuItem;
    miSupport: TMenuItem;
    miDeveloperExpressontheweb: TMenuItem;
    alMain: TActionList;
    miOptionsView: TMenuItem;
    miOptionsBehavour: TMenuItem;
    actShowCaptions: TAction;
    actShowSpecialGroup: TAction;
    miLookAndFeel: TMenuItem;
    miShowCaptions: TMenuItem;
    miShowSpecialGroup: TMenuItem;
    actAllowSelectLinks: TAction;
    actEachGroupHasSelectedLink: TAction;
    actShowGroupHints: TAction;
    actShowLinkHints: TAction;
    miAllowSelectLinks: TMenuItem;
    miEachGroupHasSelectedLink: TMenuItem;
    miShowGroupHints: TMenuItem;
    miShowLinkHints: TMenuItem;
    miLookAndFeelItem: TMenuItem;
    Label9: TLabel;
    pnlHierarchy: TPanel;
    lbNavBarHierarchy: TLabel;
    tvNavBar: TTreeView;
    pnlNavBarActions: TPanel;
    btAddGroup: TButton;
    btDeleteGroup: TButton;
    btAddLink: TButton;
    btDeleteLink: TButton;
    pmnuItems: TPopupMenu;
    pmnuItem: TMenuItem;
    gbProperties: TGroupBox;
    pcProperties: TPageControl;
    tsSelectedGroupProps: TTabSheet;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    cbGExpanded: TCheckBox;
    cbGVisible: TCheckBox;
    cbGLinkUseSmallImages: TCheckBox;
    cbGUseSmallImages: TCheckBox;
    eGCaption: TEdit;
    cbGShowAsIconView: TCheckBox;
    cbGSmallImageIndex: TComboBox;
    cbGLargeImageIndex: TComboBox;
    tsSelectedItemProps: TTabSheet;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    eICaption: TEdit;
    cbIEnabled: TCheckBox;
    cbIVisible: TCheckBox;
    cbILargeImageIndex: TComboBox;
    cbISmallImageIndex: TComboBox;
    bgCalendar: TdxNavBarGroup;
    bgJournal: TdxNavBarGroup;
    bgNotes: TdxNavBarGroup;
    bgTasks: TdxNavBarGroup;
    bgContacts: TdxNavBarGroup;
    bgShortcuts: TdxNavBarGroup;
    biMyDocuments: TdxNavBarItem;
    biFavorites: TdxNavBarItem;
    biJunkEmail: TdxNavBarItem;
    procedure FormCreate(Sender: TObject);
    procedure btAddGroupClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btDeleteGroupClick(Sender: TObject);
    procedure btDeleteLinkClick(Sender: TObject);
    procedure btAddLinkClick(Sender: TObject);
    procedure nbMainEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure miLookAndFeelItemClick(Sender: TObject);
    procedure actShowCaptionsExecute(Sender: TObject);
    procedure actShowSpecialGroupExecute(Sender: TObject);
    procedure actAllowSelectLinksExecute(Sender: TObject);
    procedure actEachGroupHasSelectedLinkExecute(Sender: TObject);
    procedure actShowGroupHintsExecute(Sender: TObject);
    procedure actShowLinkHintsExecute(Sender: TObject);
    procedure pmnuItemClick(Sender: TObject);
    procedure pmnuItemsPopup(Sender: TObject);
    procedure tvNavBarChange(Sender: TObject; Node: TTreeNode);
    procedure cbGExpandedClick(Sender: TObject);
    procedure cbGVisibleClick(Sender: TObject);
    procedure cbGShowAsIconViewClick(Sender: TObject);
    procedure cbGLinkUseSmallImagesClick(Sender: TObject);
    procedure cbGUseSmallImagesClick(Sender: TObject);
    procedure eGCaptionChange(Sender: TObject);
    procedure cbGSmallImageIndexChange(Sender: TObject);
    procedure cbGLargeImageIndexChange(Sender: TObject);
    procedure cbIEnabledClick(Sender: TObject);
    procedure cbIVisibleClick(Sender: TObject);
    procedure eICaptionChange(Sender: TObject);
    procedure cbISmallImageIndexChange(Sender: TObject);
    procedure cbILargeImageIndexChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NavBarItemClick(Sender: TObject);
    procedure nbMainActiveGroupChanged(Sender: TObject);
    procedure nbMainLinkClick(Sender: TObject; ALink: TdxNavBarItemLink);
  private
    procedure AddDropDownMenuItem(AMenuItems: TMenuItem; ACaption: string;
      AImageIndex: Integer; ARadioItem: Boolean; AClickHandler: TNotifyEvent);
    procedure HidePropertiesTabSheet(ATabSheet: TTabSheet);
    procedure SelectDefaultGroup;
    procedure SetNodeImageIndex(ANode: TTreeNode; AImageIndex: Integer);
    procedure UpdateGroupProperties;
    procedure UpdateItemProperties;
    procedure UpdateItemsDropDownMenu;
    procedure UpdateGroupPropertiesState;
    procedure UpdateItemPropertiesState;
    procedure UpdateTreeView;
    function GetCurrentGroup: TdxNavBarGroup;
    function GetCurrentItem: TdxNavBarItem;
    function GetCurrentLink: TdxNavBarItemLink;
  public
    property CurrentGroup: TdxNavBarGroup read GetCurrentGroup;
    property CurrentItem: TdxNavBarItem read GetCurrentItem;
    property CurrentLink: TdxNavBarItemLink read GetCurrentLink;
  end;

var
  fmFeaturesMain: TfmFeaturesMain;

implementation

uses
  dxNavBarViewsFact, ShellAPI;

{$R *.DFM}

procedure ClearPopupMenuItems(AMenuItem: TMenuItem);
var
  I: Integer;
begin
  for I := AMenuItem.Count - 1 downto 0 do
    AMenuItem.Items[I].Free;
end;

procedure TfmFeaturesMain.FormCreate(Sender: TObject);

  procedure InitImageIndexComoboBoxes;
  var
    I: Integer;
  begin
    for I := 0 to imgSmall.Count - 1 do
    begin
      cbGSmallImageIndex.Items.Add(IntToStr(I));
      cbISmallImageIndex.Items.Add(IntToStr(I));
    end;
    for I := 0 to imgLarge.Count - 1 do
    begin
      cbGLargeImageIndex.Items.Add(IntToStr(I));
      cbILargeImageIndex.Items.Add(IntToStr(I));
    end;
  end;

var
  I: Integer;
begin
  ClearPopupMenuItems(miLookAndFeel);
  for I := 0 to dxNavBarViewsFactory.Count - 1 do
    AddDropDownMenuItem(miLookAndFeel, dxNavBarViewsFactory.Names[I], -1, True,
      miLookAndFeelItemClick);
  miLookAndFeel.Items[dxNavBarViewsFactory.IndexOfID(nbMain.View)].Checked := True;
  gbProperties.Height := 190;
  actShowCaptions.Checked := nbMain.ShowGroupCaptions;
  actAllowSelectLinks.Checked := nbMain.AllowSelectLinks;
  actEachGroupHasSelectedLink.Checked := nbMain.EachGroupHasSelectedLink;
  actShowGroupHints.Checked := nbMain.ShowGroupsHint;
  actShowLinkHints.Checked := nbMain.ShowLinksHint;

  InitImageIndexComoboBoxes;
  UpdateTreeView;
  SelectDefaultGroup;
  UpdateGroupProperties;
  UpdateItemProperties;
end;

procedure TfmFeaturesMain.btAddGroupClick(Sender: TObject);
var
  AGroup: TdxNavBarGroup;
  AParentNode, ANode: TTreeNode;
begin
  AGroup := nbMain.Groups.Add;
  AGroup.OnClick := NavBarItemClick;
  if CurrentGroup <> nil then
  begin
    if TObject(tvNavBar.Selected.Data) is TdxNavBarGroup then
      AParentNode := tvNavBar.Selected
    else AParentNode := tvNavBar.Selected.Parent;
    ANode := tvNavBar.Items.InsertObject(AParentNode, AGroup.Caption, AGroup);
    AGroup.Index := CurrentGroup.Index;
  end
  else
    ANode := tvNavBar.Items.AddObject(nil, AGroup.Caption, AGroup);
  ANode.ImageIndex := AGroup.SmallImageIndex;
  ANode.SelectedIndex := ANode.ImageIndex;
  tvNavBar.Selected := ANode;
  tvNavBar.FullExpand;
  UpdateGroupProperties;
  UpdateItemProperties;
end;

procedure TfmFeaturesMain.btDeleteGroupClick(Sender: TObject);
var
  AGroup: TdxNavBarGroup;
begin
  if CurrentGroup <> nil then
  begin
    AGroup := CurrentGroup;
    if TObject(tvNavBar.Selected.Data) is TdxNavBarGroup then
      tvNavBar.Items.Delete(tvNavBar.Selected)
    else tvNavBar.Items.Delete(tvNavBar.Selected.Parent);
    nbMain.Groups.Delete(AGroup.Index);

    SelectDefaultGroup;
    UpdateGroupProperties;
    UpdateItemProperties
  end;
end;

procedure TfmFeaturesMain.btAddLinkClick(Sender: TObject);
begin
  if CurrentGroup <> nil then
    with TButton(Sender).ClientToScreen(Point(0, TButton(Sender).Height)) do
      pmnuItems.Popup(X, Y);
end;

procedure TfmFeaturesMain.btDeleteLinkClick(Sender: TObject);
var
  ALink: TdxNavBarItemLink;
begin
  if CurrentLink <> nil then
  begin
    ALink := CurrentLink;
    if TObject(tvNavBar.Selected.Data) is TdxNavBarItemLink then
      tvNavBar.Items.Delete(tvNavBar.Selected);
    ALink.Group.RemoveLink(ALink.Index);

    SelectDefaultGroup;
    UpdateGroupProperties;
    UpdateItemProperties
  end;
end;

function TfmFeaturesMain.GetCurrentGroup: TdxNavBarGroup;
begin
  if tvNavBar.Selected <> nil then
  begin
    if TObject(tvNavBar.Selected.Data) is TdxNavBarGroup then
      Result := TdxNavBarGroup(tvNavBar.Selected.Data)
    else Result := TdxNavBarGroup(tvNavBar.Selected.Parent.Data);
  end
  else Result := nil;
end;

function TfmFeaturesMain.GetCurrentItem: TdxNavBarItem;
begin
  if CurrentLink <> nil then
    Result := CurrentLink.Item
  else Result := nil;
end;

function TfmFeaturesMain.GetCurrentLink: TdxNavBarItemLink;
begin
  if tvNavBar.Selected <> nil then
  begin
    if TObject(tvNavBar.Selected.Data) is TdxNavBarGroup then
      Result := nil
    else Result := TdxNavBarItemLink(tvNavBar.Selected.Data);
  end
  else Result := nil;
end;

procedure TfmFeaturesMain.UpdateGroupProperties;
begin
  if CurrentGroup <> nil then
  begin
    eGCaption.Text := CurrentGroup.Caption;
    cbGExpanded.Checked := CurrentGroup.Expanded;
    cbGVisible.Checked := CurrentGroup.Visible;
    cbGLinkUseSmallImages.Checked := CurrentGroup.LinksUseSmallImages;
    cbGUseSmallImages.Checked := CurrentGroup.UseSmallImages;
    cbGShowAsIconView.Checked := CurrentGroup.ShowAsIconView;
    cbGSmallImageIndex.ItemIndex := CurrentGroup.SmallImageIndex;
    cbGLargeImageIndex.ItemIndex := CurrentGroup.LargeImageIndex;
  end;
  UpdateGroupPropertiesState
end;

procedure TfmFeaturesMain.UpdateItemProperties;
begin
  if CurrentItem <> nil then
  begin
    eICaption.Text := CurrentItem.Caption;
    cbIEnabled.Checked := CurrentItem.Enabled;
    cbIVisible.Checked := CurrentItem.Visible;
    cbISmallImageIndex.ItemIndex := CurrentItem.SmallImageIndex;
    cbILargeImageIndex.ItemIndex := CurrentItem.LargeImageIndex;
  end;
  UpdateItemPropertiesState;
end;

procedure TfmFeaturesMain.UpdateItemsDropDownMenu;
var
  I: Integer;
begin
  ClearPopupMenuItems(pmnuItems.Items);
  pmnuItems.Images := nbMain.SmallImages;
  for I := 0 to nbMain.Items.Count - 1 do
    AddDropDownMenuItem(pmnuItems.Items, nbMain.Items[I].Caption,
      nbMain.Items[I].SmallImageIndex, False, pmnuItemClick);
end;

procedure TfmFeaturesMain.UpdateGroupPropertiesState;
begin
  btDeleteGroup.Enabled := CurrentGroup <> nil;
  eGCaption.Enabled := CurrentGroup <> nil;
  if not eGCaption.Enabled then
    eGCaption.Text := '';
  cbGExpanded.Enabled := CurrentGroup <> nil;
  if not cbGExpanded.Enabled then
    cbGExpanded.Checked := False;
  cbGVisible.Enabled := CurrentGroup <> nil;
  if not cbGVisible.Enabled then
    cbGVisible.Checked := False;
  cbGLinkUseSmallImages.Enabled := CurrentGroup <> nil;
  if not cbGLinkUseSmallImages.Enabled then
    cbGLinkUseSmallImages.Checked := False;
  cbGUseSmallImages.Enabled := CurrentGroup <> nil;
  if not cbGUseSmallImages.Enabled then
    cbGUseSmallImages.Checked := False;
  cbGShowAsIconView.Enabled := CurrentGroup <> nil;
  if not cbGShowAsIconView.Enabled then
    cbGShowAsIconView.Checked := False;
end;

procedure TfmFeaturesMain.UpdateItemPropertiesState;
begin
  btAddLink.Enabled := CurrentGroup <> nil;
  btDeleteLink.Enabled := CurrentLink <> nil;
  eICaption.Enabled := CurrentLink <> nil;
  if not eICaption.Enabled then
    eICaption.Text := '';
  cbIEnabled.Enabled := CurrentLink <> nil;
  if not cbIEnabled.Enabled then
    cbIEnabled.Checked := False;
  cbIVisible.Enabled := CurrentLink <> nil;
  if not cbIVisible.Enabled then
    cbIVisible.Checked := False;
end;

procedure TfmFeaturesMain.UpdateTreeView;
var
  ANode, AChildNode: TTreeNode;
  I, J: Integer;
begin
  tvNavBar.Items.BeginUpdate;
  try
    tvNavBar.Items.Clear;
    tvNavBar.Images := nbMain.SmallImages;
    for I := 0 to nbMain.Groups.Count - 1 do
    begin
      ANode := tvNavBar.Items.AddObject(nil, nbMain.Groups[I].Caption, nbMain.Groups[I]);
      SetNodeImageIndex(ANode, nbMain.Groups[I].SmallImageIndex);
      for J := 0 to nbMain.Groups[I].LinkCount - 1 do
        if nbMain.Groups[I].Links[J].Item <> nil then
        begin
          AChildNode := tvNavBar.Items.AddChildObject(ANode,
            nbMain.Groups[I].Links[J].Item.Caption, nbMain.Groups[I].Links[J]);
          SetNodeImageIndex(AChildNode, nbMain.Groups[I].Links[J].Item.SmallImageIndex);
        end;
    end;
    tvNavBar.FullExpand;
  finally
    tvNavBar.Items.EndUpdate;
  end;
end;

procedure TfmFeaturesMain.AddDropDownMenuItem(AMenuItems: TMenuItem;
  ACaption: string; AImageIndex: Integer; ARadioItem: Boolean;
  AClickHandler: TNotifyEvent);
var
  AMenuItem: TMenuItem;
begin
  AMenuItem := TMenuItem.Create(Self);
  AMenuItem.Caption := ACaption;
  AMenuItem.ImageIndex := AImageIndex;
  AMenuItem.RadioItem := True;
  if Assigned(AClickHandler) then
    AMenuItem.OnClick := AClickHandler
  else
    AMenuItem.Enabled := False;
  AMenuItems.Add(AMenuItem);
end;

procedure TfmFeaturesMain.HidePropertiesTabSheet(ATabSheet: TTabSheet);
begin
  ATabSheet.Visible := False;
  ATabSheet.Parent := pcProperties;
end;

procedure TfmFeaturesMain.SelectDefaultGroup;
begin
  if tvNavBar.Items.Count > 0 then
    tvNavBar.Selected := tvNavBar.Items.Item[0];
end;

procedure TfmFeaturesMain.SetNodeImageIndex(ANode: TTreeNode; AImageIndex: Integer);
begin
  ANode.ImageIndex := AImageIndex;
  ANode.StateIndex := AImageIndex;
  ANode.SelectedIndex := AImageIndex;
end;

procedure TfmFeaturesMain.FormActivate(Sender: TObject);
begin
  UpdateGroupProperties;
  UpdateItemProperties;
end;

procedure TfmFeaturesMain.nbMainEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  UpdateTreeView;
end;

procedure TfmFeaturesMain.miLookAndFeelItemClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
  nbMain.View := dxNavBarViewsFactory.IDs[TMenuItem(Sender).MenuIndex];
end;

procedure TfmFeaturesMain.actShowCaptionsExecute(Sender: TObject);
begin
  TAction(Sender).Checked := not TAction(Sender).Checked;
  nbMain.ShowGroupCaptions := TAction(Sender).Checked;
end;

procedure TfmFeaturesMain.actShowSpecialGroupExecute(Sender: TObject);
begin
  TAction(Sender).Checked := not TAction(Sender).Checked;
  nbMain.ShowSpecialGroup := TAction(Sender).Checked;
end;

procedure TfmFeaturesMain.actAllowSelectLinksExecute(Sender: TObject);
begin
  TAction(Sender).Checked := not TAction(Sender).Checked;
  nbMain.AllowSelectLinks := TAction(Sender).Checked;
end;

procedure TfmFeaturesMain.actEachGroupHasSelectedLinkExecute(
  Sender: TObject);
begin
  TAction(Sender).Checked := not TAction(Sender).Checked;
  nbMain.EachGroupHasSelectedLink := TAction(Sender).Checked;
end;

procedure TfmFeaturesMain.actShowGroupHintsExecute(Sender: TObject);
begin
  TAction(Sender).Checked := not TAction(Sender).Checked;
  nbMain.ShowGroupsHint := TAction(Sender).Checked;
end;

procedure TfmFeaturesMain.actShowLinkHintsExecute(Sender: TObject);
begin
  TAction(Sender).Checked := not TAction(Sender).Checked;
  nbMain.ShowLinksHint := TAction(Sender).Checked;
end;

procedure TfmFeaturesMain.pmnuItemClick(Sender: TObject);
var
  AItem: TdxNavBarItem;
  ALink: TdxNavBarItemLink;
  ANode: TTreeNode;
begin
  if CurrentGroup <> nil then
  begin
    AItem := nbMain.Items[TMenuItem(Sender).MenuIndex];
    ALink := CurrentGroup.CreateLink(AItem);
    if TObject(tvNavBar.Selected.Data) is TdxNavBarGroup then
      ANode := tvNavBar.Selected
    else ANode := tvNavBar.Selected.Parent;
    ANode := tvNavBar.Items.AddChildObject(ANode, AItem.Caption, ALink);
    SetNodeImageIndex(ANode, AItem.SmallImageIndex);
    ANode.Selected := True;
    tvNavBar.FullExpand;
  end;
end;

procedure TfmFeaturesMain.pmnuItemsPopup(Sender: TObject);
begin
  UpdateItemsDropDownMenu;
end;

procedure TfmFeaturesMain.tvNavBarChange(Sender: TObject; Node: TTreeNode);

  procedure ShowPropertiesTabSheet(ATabSheet: TTabSheet);
  begin
    ATabSheet.Parent := gbProperties;
    ATabSheet.Visible := True;
    gbProperties.Caption := ATabSheet.Caption;
  end;

begin
 UpdateGroupProperties;
 UpdateItemProperties;
 if Node.Level = 0 then
 begin
   HidePropertiesTabSheet(tsSelectedItemProps);
   ShowPropertiesTabSheet(tsSelectedGroupProps);
 end
 else
 begin
   HidePropertiesTabSheet(tsSelectedGroupProps);
   ShowPropertiesTabSheet(tsSelectedItemProps);
 end;
end;

procedure TfmFeaturesMain.cbGExpandedClick(Sender: TObject);
begin
  if CurrentGroup <> nil then
    CurrentGroup.Expanded := TCheckBox(Sender).Checked;
end;

procedure TfmFeaturesMain.cbGVisibleClick(Sender: TObject);
begin
  if CurrentGroup <> nil then
    CurrentGroup.Visible := TCheckBox(Sender).Checked;
end;

procedure TfmFeaturesMain.cbGShowAsIconViewClick(Sender: TObject);
begin
  if CurrentGroup <> nil then
    CurrentGroup.ShowAsIconView := TCheckBox(Sender).Checked;
end;

procedure TfmFeaturesMain.cbGLinkUseSmallImagesClick(Sender: TObject);
begin
  if CurrentGroup <> nil then
    CurrentGroup.LinksUseSmallImages := TCheckBox(Sender).Checked;
end;

procedure TfmFeaturesMain.cbGUseSmallImagesClick(Sender: TObject);
begin
  if CurrentGroup <> nil then
    CurrentGroup.UseSmallImages := TCheckBox(Sender).Checked;
end;

procedure TfmFeaturesMain.eGCaptionChange(Sender: TObject);
begin
  if CurrentGroup <> nil then
  begin
    CurrentGroup.Caption := eGCaption.Text;
    tvNavBar.Selected.Text := CurrentGroup.Caption;
  end;  
end;

procedure TfmFeaturesMain.cbGSmallImageIndexChange(Sender: TObject);
begin
  if CurrentGroup = nil then
    Exit;
  CurrentGroup.SmallImageIndex := cbGSmallImageIndex.ItemIndex;
  tvNavBar.Selected.ImageIndex := CurrentGroup.SmallImageIndex;
  tvNavBar.Selected.StateIndex := CurrentGroup.SmallImageIndex;
  tvNavBar.Selected.SelectedIndex := CurrentGroup.SmallImageIndex;
end;

procedure TfmFeaturesMain.cbGLargeImageIndexChange(Sender: TObject);
begin
  if CurrentGroup <> nil then
    CurrentGroup.LargeImageIndex := cbGLargeImageIndex.ItemIndex;
end;

procedure TfmFeaturesMain.cbIEnabledClick(Sender: TObject);
begin
  if CurrentItem <> nil then
    CurrentItem.Enabled := TCheckBox(Sender).Checked;
end;

procedure TfmFeaturesMain.cbIVisibleClick(Sender: TObject);
begin
  if CurrentItem <> nil then
    CurrentItem.Visible := TCheckBox(Sender).Checked;
end;

procedure TfmFeaturesMain.eICaptionChange(Sender: TObject);
var
  I: Integer;
begin
  if CurrentItem <> nil then
  begin
    CurrentItem.Caption := eICaption.Text;
    for I := 0 to tvNavBar.Items.Count - 1 do
      if (TObject(tvNavBar.Items[I].Data) is TdxNavBarItemLink) and
        (TdxNavBarItemLink(tvNavBar.Items[I].Data).Item = CurrentItem) then
        begin
          tvNavBar.Items[I].Text := CurrentItem.Caption;
          Break;
        end;
  end;
end;

procedure TfmFeaturesMain.cbISmallImageIndexChange(Sender: TObject);
begin
  if CurrentItem <> nil then
  begin
    CurrentItem.SmallImageIndex := cbISmallImageIndex.ItemIndex;
    tvNavBar.Selected.ImageIndex := CurrentItem.SmallImageIndex;
    tvNavBar.Selected.StateIndex := CurrentItem.SmallImageIndex;
    tvNavBar.Selected.SelectedIndex := CurrentItem.SmallImageIndex;
  end;
end;

procedure TfmFeaturesMain.cbILargeImageIndexChange(Sender: TObject);
begin
  if CurrentItem <> nil then
    CurrentItem.LargeImageIndex := cbILargeImageIndex.ItemIndex;
end;

procedure TfmFeaturesMain.FormDestroy(Sender: TObject);
begin
  tsSelectedItemProps.Parent := pcProperties;
  tsSelectedGroupProps.Parent := pcProperties;
end;

procedure TfmFeaturesMain.NavBarItemClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to tvNavBar.Items.Count - 1 do
    if tvNavBar.Items[I].Data = Sender then
    begin
      tvNavBar.Items[I].Selected := True;
      Break;
    end
    else
      if (TObject(tvNavBar.Items[I].Data) is TdxNavBarItemLink) and
        (TdxNavBarItemLink(tvNavBar.Items[I].Data).Item = Sender) and
        (tvNavBar.Items[I].Data = nbMain.PressedLink) then
      begin
        tvNavBar.Items[I].Selected := True;
        Break;
      end;
end;

procedure TfmFeaturesMain.nbMainActiveGroupChanged(Sender: TObject);
begin
  NavBarItemClick(nbMain.ActiveGroup);
end;

procedure TfmFeaturesMain.nbMainLinkClick(Sender: TObject;
  ALink: TdxNavBarItemLink);
begin
  NavBarItemClick(ALink);
end;

end.
