unit SampleDockingMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, dxDockControl, dxDockPanel, dxBar, StdCtrls, ExtCtrls, ImgList,
  ActnList, ShellAPI, cxControls, dxBarExtItems, dxStatusBar, ComCtrls,
  cxGraphics, EBarsUtils, cxPC, cxLookAndFeels, cxLookAndFeelPainters, cxClasses;

type
  TFormClass = class of TForm;
  PPersistInfo = ^TPersistInfo;
  TPersistInfo = record
    WinControl: TWinControl;
    SpecInfo: Integer;
  end;
  PRichEditSelInfo =^ TRichEditSelInfo;
  TRichEditSelInfo = record
    SelStart: Integer;
    SelLength: Integer;
  end;

  TSampleDockingMainForm = class(TForm)
    dxDockingManager: TdxDockingManager;
    DockSite1: TdxDockSite;
    BarManager: TdxBarManager;
    dxBarButtonExit: TdxBarButton;
    dxBarButton1: TdxBarLargeButton;
    dxBarButton2: TdxBarLargeButton;
    dxBarButton3: TdxBarLargeButton;
    dxBarButton4: TdxBarLargeButton;
    dxBarButton5: TdxBarLargeButton;
    dxBarButton6: TdxBarLargeButton;
    dxBarButton7: TdxBarLargeButton;
    imBarIcons: TImageList;
    dxBarSubItem1: TdxBarSubItem;
    dxBarSubItem2: TdxBarSubItem;
    dxBarSubItem3: TdxBarSubItem;
    dxBarButton8: TdxBarButton;
    dxBarButton9: TdxBarButton;
    dxBarButton10: TdxBarButton;
    dxBarButton11: TdxBarButton;
    dxBarButton12: TdxBarButton;
    dxBarSubItem4: TdxBarSubItem;
    lbDescription: TLabel;
    dxBarButton18: TdxBarButton;
    dxBarButton19: TdxBarButton;
    dxBarButton20: TdxBarButton;
    dxBarPopupMenu: TdxBarPopupMenu;
    dxBarButtonDockable: TdxBarButton;
    dxBarButton22: TdxBarButton;
    dxBarButtonAutoHide: TdxBarButton;
    dxBarButtonFloating: TdxBarButton;
    ilDockPanels: TImageList;
    ilDisabledImages: TImageList;
    ilHotImages: TImageList;
    dxBarLargeButton1: TdxBarLargeButton;
    dxBarLargeButton2: TdxBarLargeButton;
    dxStatusBar: TdxStatusBar;
    dxbLookAndFeelKinds: TdxBarListItem;
    dxbNativeStyle: TdxBarButton;
    cxLookAndFeelController: TcxLookAndFeelController;
    dxBarDockStyle: TdxBarSubItem;
    dxBarDockStyleStandard: TdxBarButton;
    dxBarDockStyleVS2005: TdxBarButton;
    procedure SchemeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure dxBarButton12Click(Sender: TObject);
    procedure dxBarButtonExitClick(Sender: TObject);
    procedure actRateDemoExecute(Sender: TObject);
    procedure dxBarButton18Click(Sender: TObject);
    procedure dxBarButton19Click(Sender: TObject);
    procedure dxBarButtonDockableClick(Sender: TObject);
    procedure dxBarButton22Click(Sender: TObject);
    procedure dxBarButtonFloatingClick(Sender: TObject);
    procedure dxBarButtonAutoHideClick(Sender: TObject);
    procedure ShowDockControl(Sender: TdxDockSite; AControl: TdxCustomDockControl);
    procedure HideDockControl(Sender: TdxDockSite; AControl: TdxCustomDockControl);
    procedure dxbLookAndFeelKindsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure dxbNativeStyleClick(Sender: TObject);
    procedure dxBarDockStyleStandardClick(Sender: TObject);
  private
    FTreeViewFrameCount, FDockingRichTextFrameCount, FRadioGroupFrameCount, FListBoxFrameCount: Integer;
    FPopupMenuDockControl: TdxCustomDockControl;
    procedure AutoHideChanged(Sender: TdxCustomDockControl);
    procedure AutoHideChanging(Sender: TdxCustomDockControl);
    procedure ClearContent;
    procedure CreateFloatSite(Sender: TdxCustomDockControl; AFloatSite: TdxFloatDockSite);
    procedure CreateFrame(AFrameClass: TFormClass; AOwner: TdxDockPanel);
    procedure CreateScheme1;
    procedure CreateScheme2;
    procedure CreateScheme3;
    procedure CreateScheme4;
    procedure CreateScheme5;
    procedure CreateSiteContainer(Sender: TdxCustomDockControl; ASideContainer: TdxSideContainerDockSite);
    procedure CreateTabContainer(Sender: TdxCustomDockControl; ATabContainer: TdxTabContainerDockSite);
    procedure dpContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure EndDock(Sender: TdxCustomDockControl; Zone: TdxZone; X, Y: Integer);
    procedure HookupEvents(Sender: TdxCustomDockControl);
    procedure SetPanelsVisibility(AVisible: Boolean);
    procedure StartDock(Sender: TdxCustomDockControl; X, Y: Integer);
    procedure UpdateLookAndFeelMenu;
  end;

var
  SampleDockingMainForm: TSampleDockingMainForm;

implementation

uses SampleDockingContainers, EBarsDemoRating,
  SampleDockingRichText, SampleDockingRadioGroup, SampleDockingTreeView,
  SampleDockingListBox;

{$R *.dfm}

function GetFocusedControl(AWinControl: TWinControl): TWinControl;
var
  i: Integer;
begin
  Result := nil;
  for i:=0 to AWinControl.ControlCount - 1 do
    if AWinControl.Controls[i] is TWinControl then
    begin
      Result := GetFocusedControl(TWinControl(AWinControl.Controls[i]));
      if Result <> nil then Exit;
      if TWinControl(AWinControl.Controls[i]).Focused then
      begin
        Result := TWinControl(AWinControl.Controls[i]);
        Exit;
      end;
    end;
end;

procedure TSampleDockingMainForm.HookupEvents(Sender: TdxCustomDockControl);
begin
  if Sender is TdxDockSite then
  begin
    TdxDockSite(Sender).OnShowControl := ShowDockControl;
    TdxDockSite(Sender).OnHideControl := HideDockControl;
  end;
  Sender.OnAutoHideChanged := AutoHideChanged;
  Sender.OnAutoHideChanging := AutoHideChanging;
  Sender.OnContextPopup := dpContextPopup;
  Sender.OnEndDocking := EndDock;
  Sender.OnStartDocking := StartDock;
  Sender.OnCreateTabContainer := CreateTabContainer;
  Sender.OnCreateSideContainer := CreateSiteContainer;
  Sender.OnCreateFloatSite := CreateFloatSite;
end;

procedure TSampleDockingMainForm.CreateFloatSite(
  Sender: TdxCustomDockControl; AFloatSite: TdxFloatDockSite);
begin
  HookupEvents(AFloatSite);
end;

procedure TSampleDockingMainForm.CreateTabContainer(
  Sender: TdxCustomDockControl; ATabContainer: TdxTabContainerDockSite);
begin
  HookupEvents(ATabContainer);
end;

procedure TSampleDockingMainForm.CreateSiteContainer(
  Sender: TdxCustomDockControl; ASideContainer: TdxSideContainerDockSite);
begin
  HookupEvents(ASideContainer);
end;

procedure TSampleDockingMainForm.CreateScheme1;
var
  Panel, Panel1, Panel2: TdxDockPanel;
begin
  Panel1 := TdxDockPanel.Create(Self);
  HookupEvents(Panel1);
  Panel1.Height := 360;
  Panel1.DockTo(DockSite1, dtBottom, 0);
  CreateFrame(TSampleDockingListBoxFrame, Panel1);

  Panel2 := TdxDockPanel.Create(Self);
  HookupEvents(Panel2);
  Panel2.DockTo(Panel1, dtClient, 1);
  CreateFrame(TSampleDockingRadioGroupFrame, Panel2);

  Panel := TdxDockPanel.Create(Self);
  HookupEvents(Panel);
  Panel.DockTo(Panel2.TabContainer, dtTop, 0);
  CreateFrame(TSampleDockingTreeViewFrame, Panel);

  Panel1 := TdxDockPanel.Create(Self);
  HookupEvents(Panel1);
  Panel1.DockTo(Panel, dtClient, 0);
  CreateFrame(TSampleDockingRichTextFrame, Panel1);
end;

procedure TSampleDockingMainForm.CreateScheme2;
var
  Panel, Panel1, Panel2: TdxDockPanel;
begin
  Panel1 := TdxDockPanel.Create(Self);
  HookupEvents(Panel1);
  Panel1.Height := 300;
  Panel1.DockTo(DockSite1, dtRight, 0);
  CreateFrame(TSampleDockingTreeViewFrame, Panel1);

  Panel2 := TdxDockPanel.Create(Self);
  HookupEvents(Panel2);
  Panel2.DockTo(Panel1, dtClient, 1);
  CreateFrame(TSampleDockingRichTextFrame, Panel2);

  Panel := TdxDockPanel.Create(Self);
  HookupEvents(Panel);
  Panel.DockTo(Panel1, dtLeft, 0);
  CreateFrame(TSampleDockingRadioGroupFrame, Panel);
end;

procedure TSampleDockingMainForm.CreateScheme3;
var
  Panel1, Panel2, Panel3, Panel4: TdxDockPanel;
begin
  Panel1 := TdxDockPanel.Create(Self);
  HookupEvents(Panel1);
  Panel1.DockTo(DockSite1, dtLeft, 0);
  Panel1.AutoHide := True;
  CreateFrame(TSampleDockingRichTextFrame, Panel1);

  Panel2 := TdxDockPanel.Create(Self);
  HookupEvents(Panel2);
  Panel2.Height := 170;
  Panel2.DockTo(DockSite1, dtTop, 0);
  Panel2.AutoHide := True;
  CreateFrame(TSampleDockingRadioGroupFrame, Panel2);

  Panel3 := TdxDockPanel.Create(Self);
  HookupEvents(Panel3);
  Panel3.DockTo(DockSite1, dtRight, 0);
  Panel3.AutoHide := True;
  CreateFrame(TSampleDockingTreeViewFrame, Panel3);

  Panel4 := TdxDockPanel.Create(Self);
  HookupEvents(Panel4);
  Panel4.Height := 190;
  Panel4.DockTo(DockSite1, dtBottom, 0);
  Panel4.AutoHide := True;
  CreateFrame(TSampleDockingListBoxFrame, Panel4);
end;

procedure TSampleDockingMainForm.CreateScheme4;
var
  Panel1, Panel2: TdxDockPanel;
begin
  Panel1 := TdxDockPanel.Create(Self);
  HookupEvents(Panel1);
  Panel1.MakeFloating(Left + 50, Top + 100);
  CreateFrame(TSampleDockingTreeViewFrame, Panel1);

  Panel2 := TdxDockPanel.Create(Self);
  HookupEvents(Panel2);
  Panel2.DockTo(Panel1, dtClient, 0);
  CreateFrame(TSampleDockingRichTextFrame, Panel2);
end;

procedure TSampleDockingMainForm.CreateScheme5;
var
  Panel1, Panel2, Panel3: TdxDockPanel;
begin
  Panel1 := TdxDockPanel.Create(Self);
  HookupEvents(Panel1);
  Panel1.Height := 350;
  Panel1.DockTo(DockSite1, dtBottom, 0);
  CreateFrame(TSampleDockingListBoxFrame, Panel1);

  Panel2 := TdxDockPanel.Create(Self);
  HookupEvents(Panel2);
  Panel2.DockTo(Panel1, dtRight, 1);
  CreateFrame(TSampleDockingTreeViewFrame, Panel2);

  Panel3 := TdxDockPanel.Create(Self);
  HookupEvents(Panel3);
  Panel3.DockTo(Panel1, dtTop, 0);
  CreateFrame(TSampleDockingRichTextFrame, Panel3);
end;

procedure TSampleDockingMainForm.SchemeClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: CreateScheme1;
    1: CreateScheme2;
    2: CreateScheme3;
    3: CreateScheme4;
    4: CreateScheme5;
  end;
end;

procedure TSampleDockingMainForm.ClearContent;
var
  Count: Integer;
begin
  Count := dxDockingController.DockControlCount - 1;
  while Count >= 0 do
  begin
    if dxDockingController.DockControls[Count] is TdxDockPanel then
      dxDockingController.DockControls[Count].Free;
    if (dxDockingController.DockControlCount - 1) < (Count - 1) then
      Count := dxDockingController.DockControlCount - 1 else
    Dec(Count);
  end;
  FTreeViewFrameCount := 0;
  FDockingRichTextFrameCount := 0;
  FRadioGroupFrameCount := 0;
  FListBoxFrameCount := 0;
end;

procedure TSampleDockingMainForm.FormCreate(Sender: TObject);
begin
  UpdateLookAndFeelMenu;
end;

procedure TSampleDockingMainForm.FormDestroy(Sender: TObject);
begin
  ClearContent;
end;

procedure TSampleDockingMainForm.dxBarButton12Click(Sender: TObject);
begin
  ClearContent;
end;

procedure TSampleDockingMainForm.dxBarButtonExitClick(Sender: TObject);
begin
  Close;
end;

procedure TSampleDockingMainForm.actRateDemoExecute(Sender: TObject);
begin
  with TEBarsDemoRatingForm.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TSampleDockingMainForm.SetPanelsVisibility(AVisible: Boolean);
var
  i: Integer;
begin
  for i:=0 to dxDockingController.DockControlCount - 1 do
    if (dxDockingController.DockControls[i] is TdxCustomDockControl) and (dxDockingController.DockControls[i] <> DockSite1) then
    begin
      if (not AVisible) and dxDockingController.DockControls[i].AutoHide then
        dxDockingController.DockControls[i].Visible := True;
      dxDockingController.DockControls[i].Visible := AVisible;
    end;
end;

procedure TSampleDockingMainForm.dxBarButton18Click(Sender: TObject);
begin
  SetPanelsVisibility(True);
end;

procedure TSampleDockingMainForm.dxBarButton19Click(Sender: TObject);
begin
  SetPanelsVisibility(False);
end;

procedure TSampleDockingMainForm.dpContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  pt: TPoint;
  AControl: TdxCustomDockControl;
begin
  GetCursorPos(pt);
  AControl := dxDockingController.GetDockControlAtPos(pt);
  if AControl <> nil then
  begin
    FPopupMenuDockControl := AControl;
    dxBarButtonDockable.Down := AControl.Dockable;
    dxBarButtonFloating.Down := AControl.FloatDockSite <> nil;
    dxBarButtonAutoHide.Enabled := AControl.CanAutoHide;
    dxBarButtonAutoHide.Down := AControl.AutoHide;
    dxBarPopupMenu.PopupFromCursorPos;
    Handled := True;
  end;
end;

procedure TSampleDockingMainForm.dxBarButtonDockableClick(Sender: TObject);
begin
  if FPopupMenuDockControl <> nil then
  begin
    FPopupMenuDockControl.Dockable := (Sender as TdxBarButton).Down;
    FPopupMenuDockControl := nil;
  end;
end;

procedure TSampleDockingMainForm.dxBarButton22Click(Sender: TObject);
begin
  if FPopupMenuDockControl <> nil then
  begin
    FPopupMenuDockControl.Visible := False;
    FPopupMenuDockControl := nil;
  end;
end;

procedure TSampleDockingMainForm.dxBarButtonFloatingClick(Sender: TObject);
var
  pt: TPoint;
begin
  if (FPopupMenuDockControl <> nil) and (FPopupMenuDockControl.DockState <> dsFloating) then
  begin
    GetCursorPos(pt);
    FPopupMenuDockControl.MakeFloating(pt.X, pt.Y);
    FPopupMenuDockControl := nil;
  end;
end;

procedure TSampleDockingMainForm.dxBarDockStyleStandardClick(Sender: TObject);
begin
  if dxBarDockStyleVS2005.Down then
  begin
    dxDockingManager.DockStyle := dsVS2005;
    dxDockingManager.Options := dxDockingManager.Options + [doFillDockingSelection];
  end
  else
  begin
    dxDockingManager.DockStyle := dsStandard;
    dxDockingManager.Options := dxDockingManager.Options - [doFillDockingSelection];
  end;
end;

procedure TSampleDockingMainForm.dxbLookAndFeelKindsClick(Sender: TObject);
begin
  cxLookAndFeelController.Kind := TcxLookAndFeelKind((Sender as TdxBarListItem).ItemIndex);
  cxLookAndFeelController.NativeStyle := False;
  UpdateLookAndFeelMenu;
end;

procedure TSampleDockingMainForm.dxbNativeStyleClick(Sender: TObject);
begin
  cxLookAndFeelController.NativeStyle := True;
  UpdateLookAndFeelMenu;
end;

procedure TSampleDockingMainForm.UpdateLookAndFeelMenu;
begin
  dxbNativeStyle.Down := cxLookAndFeelController.NativeStyle;
  if cxLookAndFeelController.NativeStyle then
    dxbLookAndFeelKinds.ItemIndex := -1
  else
    dxbLookAndFeelKinds.ItemIndex := Integer(cxLookAndFeelController.Kind);
end;

procedure TSampleDockingMainForm.dxBarButtonAutoHideClick(Sender: TObject);
begin
  if FPopupMenuDockControl <> nil then
  begin
    FPopupMenuDockControl.AutoHide := (Sender as TdxBarButton).Down;
    FPopupMenuDockControl := nil;
  end;
end;

procedure TSampleDockingMainForm.CreateFrame(AFrameClass: TFormClass; AOwner: TdxDockPanel);
var
  AFrame: TCustomForm;
  ACaption: String;
  AImageIndex: Integer;
begin
  AFrame := AFrameClass.Create(AOwner);
  AFrame.Parent := AOwner;
  AFrame.Align := alClient;

  AImageIndex := -1;
  if AFrame is TSampleDockingTreeViewFrame then
  begin
    Inc(FTreeViewFrameCount);
    ACaption := 'TreeView Panel ' + IntToStr(FTreeViewFrameCount);
    AImageIndex := 0;
  end else
  if AFrame is TSampleDockingRichTextFrame then
  begin
    Inc(FDockingRichTextFrameCount);
    ACaption := 'RichText Panel ' + IntToStr(FDockingRichTextFrameCount);
    AImageIndex := 1;
  end else
  if AFrame is TSampleDockingRadioGroupFrame then
  begin
    Inc(FRadioGroupFrameCount);
    ACaption := 'RadioGroup Panel ' + IntToStr(FRadioGroupFrameCount);
    AImageIndex := 2;
  end else
  if AFrame is TSampleDockingListBoxFrame then
  begin
    Inc(FListBoxFrameCount);
    ACaption := 'ListBox Panel ' + IntToStr(FListBoxFrameCount);
    AImageIndex := 3;
  end;
  AOwner.Caption := ACaption;
  AOwner.ImageIndex := AImageIndex;
  AFrame.Visible := True;
end;

procedure StorePersistentInfo(Sender: TdxCustomDockControl);
var
  AWinControl: TWinControl;
  p: PPersistInfo;
  PSelInfo: PRichEditSelInfo;
begin
  if Sender.Visible and (Sender.Tag = 0) then
  begin
    AWinControl := GetFocusedControl(Sender);
    if AWinControl <> nil then
    begin
      New(p);
      p^.WinControl := AWinControl;
      if AWinControl is TCustomEdit then
      begin
        New(PSelInfo);
        PSelInfo^.SelStart := TCustomEdit(AWinControl).SelStart;
        PSelInfo^.SelLength := TCustomEdit(AWinControl).SelLength;
        p^.SpecInfo := Integer(PSelInfo);
        Sender.Tag := Integer(p);
      end;
      Sender.Tag := Integer(p);
    end;
  end;
end;

procedure RestorePersistentInfo(Sender: TdxCustomDockControl);
begin
  if Sender.Visible and (Sender.Tag <> 0) then
  begin
    if PPersistInfo(Sender.Tag)^.WinControl.CanFocus then
      PPersistInfo(Sender.Tag)^.WinControl.SetFocus;
    if PPersistInfo(Sender.Tag)^.WinControl is TCustomEdit then
    begin
      TCustomEdit(PPersistInfo(Sender.Tag)^.WinControl).SelStart := PRichEditSelInfo(PPersistInfo(Sender.Tag)^.SpecInfo)^.SelStart;
      TCustomEdit(PPersistInfo(Sender.Tag)^.WinControl).SelLength := PRichEditSelInfo(PPersistInfo(Sender.Tag)^.SpecInfo)^.SelLength;
      Dispose(PRichEditSelInfo(PPersistInfo(Sender.Tag)^.SpecInfo));
    end;
    Dispose(PPersistInfo(Sender.Tag));
    Sender.Tag := 0;
  end;
end;

procedure TSampleDockingMainForm.StartDock(Sender: TdxCustomDockControl; X,
  Y: Integer);
begin
  StorePersistentInfo(Sender);
end;

procedure TSampleDockingMainForm.EndDock(Sender: TdxCustomDockControl;
  Zone: TdxZone; X, Y: Integer);
begin
  RestorePersistentInfo(Sender);
end;

procedure TSampleDockingMainForm.HideDockControl(Sender: TdxDockSite;
  AControl: TdxCustomDockControl);
begin
  StorePersistentInfo(AControl);
end;

procedure TSampleDockingMainForm.ShowDockControl(Sender: TdxDockSite;
  AControl: TdxCustomDockControl);
begin
  RestorePersistentInfo(AControl);
end;

procedure TSampleDockingMainForm.AutoHideChanged(
  Sender: TdxCustomDockControl);
begin
  RestorePersistentInfo(Sender);
end;

procedure TSampleDockingMainForm.AutoHideChanging(
  Sender: TdxCustomDockControl);
begin
  StorePersistentInfo(Sender);
end;

end.

