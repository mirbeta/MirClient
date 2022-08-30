unit BaseForm;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ComCtrls,
{$IFDEF EXPRESSBARS}
  dxBar, dxStatusBar,
{$ENDIF}
  SkinDemoUtils,
  dxCore, cxClasses, cxStyles, cxGridTableView, cxLookAndFeels, cxGridCardView;

type
  TfmBaseForm = class(TForm)
    lbDescription: TLabel;
    miAbout: TMenuItem;
    miExit: TMenuItem;
    miFile: TMenuItem;
    mmMain: TMainMenu;
    StyleRepository: TcxStyleRepository;
    cxStyle1: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    cxStyle4: TcxStyle;
    cxStyle5: TcxStyle;
    cxStyle6: TcxStyle;
    cxStyle7: TcxStyle;
    cxStyle8: TcxStyle;
    cxStyle9: TcxStyle;
    cxStyle10: TcxStyle;
    cxStyle11: TcxStyle;
    cxStyle12: TcxStyle;
    cxStyle13: TcxStyle;
    cxStyle14: TcxStyle;
    cxStyle15: TcxStyle;
    cxStyle16: TcxStyle;
    cxStyle17: TcxStyle;
    cxStyle18: TcxStyle;
    cxStyle19: TcxStyle;
    cxStyle20: TcxStyle;
    cxStyle21: TcxStyle;
    cxStyle22: TcxStyle;
    cxStyle23: TcxStyle;
    cxStyle24: TcxStyle;
    GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet;
    GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet;
    sbMain: TStatusBar;
    cxLookAndFeelController1: TcxLookAndFeelController;
    procedure miAboutClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
  private
    FLookAndFeel: TcxLookAndFeel;
    procedure CreateSkinsMenuItem;
  protected
    function CanUseStyleSheet: Boolean;
    function GetMenuItemChecked(AMenuItem: TObject): Boolean;
    function IsNeedLookAndFeelMenuItem: Boolean; virtual;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); virtual;
    procedure TouchModeClick(Sender: TObject);
    procedure UpdateCardViewStyleSheet(ACardView: TcxGridCardView);
    procedure UpdateTableViewStyleSheet(ATableView: TcxGridTableView);
    //
    function FindMenuItem(const AMenuItemName: string; out AItem: TdxBaseMenuItem): Boolean;
    procedure MenuItemCheckSubItemWithTag(const AMenuItemName: string; ATag: Integer);
    procedure MenuItemSetChecked(const AMenuItemName: string; AChecked: Boolean); overload;
    procedure MenuItemSetChecked(Sender: TObject; AChecked: Boolean); overload;
    procedure MenuItemSetEnabled(const AMenuItemName: string; AEnabled: Boolean); overload;
    procedure MenuItemSetEnabled(Sender: TObject; AEnabled: Boolean); overload;
    procedure MenuItemSetVisible(const AMenuItemName: string; AVisible: Boolean); overload;
    procedure MenuItemSetVisible(Sender: TObject; AVisible: Boolean); overload;
  public
  {$IFDEF EXPRESSBARS}
    BarManager: TdxBarManager;
    StatusBar: TdxStatusBar;
  {$ELSE}
    function StatusBar: TStatusBar;
  {$ENDIF}
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure CreateTouchModeMenuOption;
    procedure PlaceControls; virtual;
  end;

var
  fmBaseForm: TfmBaseForm;

implementation

{$R *.dfm}

uses
  dxGDIPlusAPI, cxLookAndFeelPainters, AboutDemoForm;

{ TfmBaseForm }

procedure TfmBaseForm.AfterConstruction;
{$IFDEF EXPRESSBARS}

  procedure StatusBarPanelAssign(ADXPanel: TdxStatusBarPanel; APanel: TStatusPanel);
  begin
    ADXPanel.Width := APanel.Width;
  end;

  var
    I: Integer;
{$ENDIF}
begin
  inherited;
  FLookAndFeel := TcxLookAndFeel.Create(nil);
  FLookAndFeel.OnChanged := LookAndFeelChanged;
{$IFDEF EXPRESSBARS}
  BarManager := TdxBarManager.Create(Self);
  dxBarConvertMainMenu(mmMain, BarManager);
  BarManager.Style := bmsUseLookAndFeel;
{$ENDIF}
  PlaceControls;
  if IsNeedLookAndFeelMenuItem then
    CreateSkinsMenuItem;
{$IFDEF EXPRESSBARS}
  StatusBar := TdxStatusBar.Create(Self);
  StatusBar.PaintStyle := stpsUseLookAndFeel;
  StatusBar.Panels.Add.PanelStyleClass := TdxStatusBarTextPanelStyle;
  TdxStatusBarTextPanelStyle(StatusBar.Panels[0].PanelStyle).AutoHint := True;
  for I := 0 to sbMain.Panels.Count - 1 do
  begin
    if StatusBar.Panels.Count > I then
      StatusBarPanelAssign(StatusBar.Panels[I], sbMain.Panels[I])
    else
      StatusBarPanelAssign(StatusBar.Panels.Add, sbMain.Panels[I]);
  end;
  StatusBar.ParentShowHint := sbMain.ParentShowHint;
  StatusBar.ShowHint := sbMain.ShowHint;
  StatusBar.Parent := Self;
  sbMain.Free;
{$ENDIF}
  lbDescription.Transparent := False;
  CreateTouchModeMenuOption;
end;

destructor TfmBaseForm.Destroy;
begin
  FreeAndNil(FLookAndFeel);
  inherited Destroy;
end;

procedure TfmBaseForm.miAboutClick(Sender: TObject);
begin
  ShowAboutDemoForm;
end;

procedure TfmBaseForm.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfmBaseForm.CreateSkinsMenuItem;
begin
{$IFDEF EXPRESSBARS}
  SkinDemoUtils.CreateSkinsMenuItem(BarManager);
{$ELSE}
  SkinDemoUtils.CreateSkinsMenuItem(mmMain);
{$ENDIF}
end;

{$IFDEF EXPRESSBARS}
function TfmBaseForm.FindMenuItem(const AMenuItemName: string; out AItem: TdxBaseMenuItem): Boolean;
begin
  Result := SkinDemoUtils.FindMenuItem(BarManager, AMenuItemName, AItem);
end;
{$ELSE}
function TfmBaseForm.StatusBar: TStatusBar;
begin
  Result := sbMain;
end;

function TfmBaseForm.FindMenuItem(const AMenuItemName: string; out AItem: TdxBaseMenuItem): Boolean;
begin
  Result := SkinDemoUtils.FindMenuItem(mmMain, AMenuItemName, AItem);
end;
{$ENDIF}

procedure TfmBaseForm.CreateTouchModeMenuOption;
var
  AItem: TdxBaseMenuItem;
{$IFDEF EXPRESSBARS}
  ABarButton: TdxBarButton;
{$ELSE}
  AMenuItem: TMenuItem;
{$ENDIF}
begin
  if FindMenuItem('miOptions', AItem) then
  begin
{$IFDEF EXPRESSBARS}
    ABarButton := BarManager.AddButton;
    (AItem as TdxBarSubItem).ItemLinks.Add(ABarButton);
    ABarButton.Caption := 'Touch Mode';
    ABarButton.ButtonStyle := bsChecked;
    ABarButton.Name := 'miTouchMode';
    ABarButton.Down := cxIsTouchModeEnabled;
    ABarButton.OnClick := TouchModeClick;
{$ELSE}
    AMenuItem := TMenuItem.Create(AItem as TMenuItem);
    (AItem as TMenuItem).Add(AMenuItem);
    AMenuItem.Caption := 'Touch Mode';
    AMenuItem.Name := 'miTouchMode';
    AMenuItem.AutoCheck := True;
    AMenuItem.Checked := cxIsTouchModeEnabled;
    AMenuItem.OnClick := TouchModeClick;
{$ENDIF}
  end;
end;

procedure TfmBaseForm.PlaceControls;
begin
{$IFDEF EXPRESSBARS}
  BarManager.MainMenuControl.DockControl.Top := 0;
{$ENDIF}
end;

function TfmBaseForm.CanUseStyleSheet: Boolean;
begin
  Result := FLookAndFeel.SkinPainter = nil;
end;

function TfmBaseForm.GetMenuItemChecked(AMenuItem: TObject): Boolean;
begin
{$IFDEF EXPRESSBARS}
  if AMenuItem is TdxBarButton then
    Result := TdxBarButton(AMenuItem).Down
  else
{$ENDIF}
   Result := (AMenuItem as TMenuItem).Checked;
end;

function TfmBaseForm.IsNeedLookAndFeelMenuItem: Boolean;
begin
  Result := Pos('STYLES', UpperCase(ExtractFileName(Application.ExeName))) = 0;
end;

procedure TfmBaseForm.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  MenuItemSetChecked('miTouchMode', cxIsTouchModeEnabled);
end;

procedure TfmBaseForm.TouchModeClick(Sender: TObject);
begin
  cxLookAndFeelController1.TouchMode := GetMenuItemChecked(Sender);
end;

procedure TfmBaseForm.UpdateCardViewStyleSheet(ACardView: TcxGridCardView);
begin
  if ACardView <> nil then
  begin
    if CanUseStyleSheet then
      ACardView.Styles.StyleSheet := GridCardViewStyleSheetDevExpress
    else
      ACardView.Styles.StyleSheet := nil;
  end;
end;

procedure TfmBaseForm.UpdateTableViewStyleSheet(ATableView: TcxGridTableView);
begin
  if ATableView <> nil then
  begin
    if CanUseStyleSheet then
      ATableView.Styles.StyleSheet := GridTableViewStyleSheetDevExpress
    else
      ATableView.Styles.StyleSheet := nil;
  end;
end;

procedure TfmBaseForm.MenuItemCheckSubItemWithTag(const AMenuItemName: string; ATag: Integer);
var
  AMenuItem: TdxBaseMenuItem;
{$IFDEF EXPRESSBARS}
  ASubMenuItem: TdxBarSubItem;
{$ENDIF}
  I: Integer;
begin
  if FindMenuItem(AMenuItemName, AMenuItem) then
  begin
  {$IFDEF EXPRESSBARS}
    ASubMenuItem := AMenuItem as TdxBarSubItem;
    for I := 0 to ASubMenuItem.ItemLinks.Count - 1 do
    begin
      with ASubMenuItem.ItemLinks[I] do
        MenuItemSetChecked(Item, Item.Tag = ATag);
    end;
  {$ELSE}
    for I := 0 to AMenuItem.Count - 1 do
      AMenuItem.Items[I].Checked := AMenuItem.Items[I].Tag = ATag;
  {$ENDIF}
  end;
end;

procedure TfmBaseForm.MenuItemSetChecked(const AMenuItemName: string; AChecked: Boolean);
var
  AMenuItem: TdxBaseMenuItem;
begin
  if FindMenuItem(AMenuItemName, AMenuItem) then
    SetMenuItemChecked(AMenuItem, AChecked);
end;

procedure TfmBaseForm.MenuItemSetChecked(Sender: TObject; AChecked: Boolean);
begin
  SetMenuItemChecked(Sender, AChecked);
end;

procedure TfmBaseForm.MenuItemSetEnabled(Sender: TObject; AEnabled: Boolean);
begin
  SetMenuItemEnable(Sender, AEnabled);
end;

procedure TfmBaseForm.MenuItemSetEnabled(const AMenuItemName: string; AEnabled: Boolean);
var
  AMenuItem: TdxBaseMenuItem;
begin
  if FindMenuItem(AMenuItemName, AMenuItem) then
    AMenuItem.Enabled := AEnabled;
end;

procedure TfmBaseForm.MenuItemSetVisible(const AMenuItemName: string; AVisible: Boolean);
var
  AMenuItem: TdxBaseMenuItem;
begin
  if FindMenuItem(AMenuItemName, AMenuItem) then
    SetMenuItemVisible(AMenuItem, AVisible);
end;

procedure TfmBaseForm.MenuItemSetVisible(Sender: TObject; AVisible: Boolean);
begin
  SetMenuItemVisible(Sender, AVisible);
end;

end.
