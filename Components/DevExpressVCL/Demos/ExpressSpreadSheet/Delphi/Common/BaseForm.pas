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
  dxCore, cxClasses, cxStyles, dxSpreadSheet, dxSpreadSheetCore, cxLookAndFeels, cxGeometry;

type
  TfmBaseForm = class(TForm)
    cxLookAndFeelController1: TcxLookAndFeelController;
    miAbout: TMenuItem;
    miExit: TMenuItem;
    miFile: TMenuItem;
    miSaveAs: TMenuItem;
    miShowFormulas: TMenuItem;
    mmMain: TMainMenu;
    N1: TMenuItem;
    miOptions: TMenuItem;
    SaveDialog: TSaveDialog;
    sbMain: TStatusBar;

    procedure miAboutClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure miShowFormulasClick(Sender: TObject);
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
    function GetSpreadSheet: TdxSpreadSheet; virtual;
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
  Types, dxGDIPlusAPI, cxLookAndFeelPainters, AboutDemoForm;

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
  CreateTouchModeMenuOption;
  WindowState := wsMaximized;
  SaveDialog.Filter := dxSpreadSheetFormatsRepository.GetSaveDialogFilter;
  GetSpreadSheet.OptionsBehavior.History := True;
end;

destructor TfmBaseForm.Destroy;
begin
  FreeAndNil(FLookAndFeel);
  inherited Destroy;
end;

procedure TfmBaseForm.miAboutClick(Sender: TObject);
var
  R: TRect;
begin
  R := cxNullRect;
  if WindowState = wsMaximized then
  begin
    R := cxRectSetOrigin(GetSpreadSheet.Bounds, GetSpreadSheet.ClientToScreen(cxNullPoint));
    R := cxRectSetRight(R, R.Right - 30, 400);
    R := cxRectSetTop(R, R.Top + 30, 500);
  end;
  ShowAboutDemoFormEx(R);
end;

procedure TfmBaseForm.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfmBaseForm.miShowFormulasClick(Sender: TObject);
var
  I: Integer;
begin
  GetSpreadSheet.BeginUpdate; 
  try 
    for I := 0 to GetSpreadSheet.SheetCount - 1 do
      if GetSpreadSheet.Sheets[I] is TdxSpreadSheetTableView then
        TdxSpreadSheetTableView(GetSpreadSheet.Sheets[I]).Options.ShowFormulas := bDefault;
    GetSpreadSheet.OptionsView.ShowFormulas := not GetSpreadSheet.OptionsView.ShowFormulas;
  finally 
    GetSpreadSheet.EndUpdate; 
  end;
end;

procedure TfmBaseForm.CreateSkinsMenuItem;
begin
{$IFDEF EXPRESSBARS}
  SkinDemoUtils.CreateSkinsMenuItem(BarManager);
{$ELSE}
  SkinDemoUtils.CreateSkinsMenuItem(mmMain);
{$ENDIF}
end;

procedure TfmBaseForm.miSaveAsClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    GetSpreadSheet.SaveToFile(SaveDialog.FileName);
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

function TfmBaseForm.GetSpreadSheet: TdxSpreadSheet;
begin
  Result := nil;
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
