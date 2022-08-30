{=======================================================================================================================
  MainForm Unit

  Raize Components - Demo Program Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.
=======================================================================================================================}

{$I RCDemo.inc}

// Undefine the following symbol if your version of Delphi or
// C++Builder does not support database access.
{$DEFINE DEMO_DATA_AWARE_CTRLS}

unit MainForm;

interface

uses
  Windows,
  Forms,
  ExtCtrls,
  RzBHints,
  RzForms,
  RzCommon,
  StdActns,
  Classes,
  ActnList,
  ImgList,
  Controls,
  Menus,
  RzGroupBar,
  RzButton,
  RzPanel,
  RzStatus,
  StdCtrls,
  ComCtrls,
  RzEdit,
  RzSplit,
  RzTabs,
  RzLabel,
  CustomFramingFrame,
  TabsFrame,
  ComboBoxesFrame,
  ListBoxesFrame,
  CommonControlsFrame,
  EditsFrame,
  ButtonsFrame,
  DisplayFrame,
  WidgetForm,
  {$IFDEF DEMO_DATA_AWARE_CTRLS}
  ReservationFrame,
  CustomerForm,
  {$ENDIF}
  ShellFrame,
  LauncherFrame,
  SendMessageFrame,
  RzPrgres,
  RzBorder,
  Graphics,
  jpeg,
  Mask,
  RzBtnEdt,
  RzTray,
  pngimage,
  RzCmboBx;


type
  TfrmMain = class(TForm)
    pnlNotes: TRzSizePanel;
    mnuMain: TMainMenu;
    mnuDemo: TMenuItem;
    mnuExit: TMenuItem;
    sbrMain: TRzStatusBar;
    RzClockStatus1: TRzClockStatus;
    RzKeyStatus1: TRzKeyStatus;
    RzKeyStatus2: TRzKeyStatus;
    RzKeyStatus3: TRzKeyStatus;
    RzResourceStatus1: TRzResourceStatus;
    mnuView: TMenuItem;
    mnuMainToolbar: TMenuItem;
    mnuStatusBar: TMenuItem;
    mnuNotes: TMenuItem;
    mnuSep2: TMenuItem;
    mnuCustomFraming: TMenuItem;
    mnuComboBoxes: TMenuItem;
    mnuWidgets: TMenuItem;
    mnuButtons: TMenuItem;
    mnuListBoxes: TMenuItem;
    mnuShell: TMenuItem;
    mnuCommon: TMenuItem;
    mnuDatabase: TMenuItem;
    mnuEdits: TMenuItem;
    mnuSendMessage: TMenuItem;
    mnuLauncher: TMenuItem;
    tbrMain: TRzToolbar;
    imlMain: TImageList;
    btnFrames: TRzToolButton;
    btnComboBoxes: TRzToolButton;
    btnListBoxes: TRzToolButton;
    btnCommon: TRzToolButton;
    btnEdits: TRzToolButton;
    btnButtons: TRzToolButton;
    btnWidgets: TRzToolButton;
    btnDatabase: TRzToolButton;
    btnShell: TRzToolButton;
    btnLauncher: TRzToolButton;
    btnSendMsg: TRzToolButton;
    ActionList1: TActionList;
    actCustomFraming: TAction;
    actComboBoxes: TAction;
    actListBoxes: TAction;
    actCommonControls: TAction;
    actEdits: TAction;
    actButtons: TAction;
    actWidgets: TAction;
    actDatabase: TAction;
    actShell: TAction;
    actLauncher: TAction;
    actSendMessages: TAction;
    actWindowClose: TWindowClose;
    actWindowCascade: TWindowCascade;
    actWindowTileHorizontal: TWindowTileHorizontal;
    actWindowTileVertical: TWindowTileVertical;
    actWindowMinimizeAll: TWindowMinimizeAll;
    actWindowArrange: TWindowArrange;
    actWindowCloseAll: TAction;
    actToolbarCustomize: TAction;
    pnlGroupBar: TRzSizePanel;
    RzRegIniFile1: TRzRegIniFile;
    RzFormState1: TRzFormState;
    btnDisplay: TRzToolButton;
    actDisplay: TAction;
    actTabs: TAction;
    btnTabs: TRzToolButton;
    RzBalloonHints1: TRzBalloonHints;
    pgcWorkArea: TRzPageControl;
    tabCustomFraming: TRzTabSheet;
    tabTabs: TRzTabSheet;
    tabEdits: TRzTabSheet;
    tabComboBoxes: TRzTabSheet;
    tabListBoxes: TRzTabSheet;
    tabCommonControls: TRzTabSheet;
    tabButtons: TRzTabSheet;
    tabDisplay: TRzTabSheet;
    tabDatabase: TRzTabSheet;
    tabWidgets: TRzTabSheet;
    tabShell: TRzTabSheet;
    tabLauncher: TRzTabSheet;
    tabSendMessages: TRzTabSheet;
    tabWelcome: TRzTabSheet;
    actWelcome: TAction;
    btnWelcome: TRzToolButton;
    mnuWelcome: TMenuItem;
    mnuTabs: TMenuItem;
    mnuDisplay: TMenuItem;
    lblWidgetFormToFront: TRzLabel;
    pnlWelcomeImage: TRzPanel;
    imgWelcome: TImage;
    RzMenuController1: TRzMenuController;
    pnlHeader: TRzPanel;
    pnlWidgetsHeader: TRzPanel;
    RzGroupController1: TRzGroupController;
    Tray: TRzTrayIcon;
    imlTray: TImageList;
    imlVisualStyles: TImageList;
    stsSection: TRzGlyphStatus;
    RzVersionInfoStatus1: TRzVersionInfoStatus;
    RzVersionInfo1: TRzVersionInfo;
    imlGroup: TImageList;
    RzGroupBar1: TRzGroupBar;
    grpVisualStyles: TRzGroup;
    grpGroupBarStyles: TRzGroup;
    grpDemos: TRzGroup;
    grpToolbar: TRzGroup;
    edtNotes: TRzMemo;
    grpVclStyles: TRzGroup;
    cbxVclStyles: TRzComboBox;
    procedure FormCreate(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuMainToolbarClick(Sender: TObject);
    procedure mnuStatusBarClick(Sender: TObject);
    procedure mnuNotesClick(Sender: TObject);
    procedure RzGroup1Items0Click(Sender: TObject);
    procedure RzGroup1Items1Click(Sender: TObject);
    procedure RzGroup1Items2Click(Sender: TObject);
    procedure actCustomFramingExecute(Sender: TObject);
    procedure actComboBoxesExecute(Sender: TObject);
    procedure actListBoxesExecute(Sender: TObject);
    procedure actCommonControlsExecute(Sender: TObject);
    procedure actEditsExecute(Sender: TObject);
    procedure actButtonsExecute(Sender: TObject);
    procedure actWidgetsExecute(Sender: TObject);
    procedure actDatabaseExecute(Sender: TObject);
    procedure actShellExecute(Sender: TObject);
    procedure actLauncherExecute(Sender: TObject);
    procedure actSendMessagesExecute(Sender: TObject);
    procedure actToolbarCustomizeExecute(Sender: TObject);
    procedure actWindowCloseAllExecute(Sender: TObject);
    procedure GrpToolbarNoTextClick(Sender: TObject);
    procedure GrpToolbarShowTextClick(Sender: TObject);
    procedure GrpToolbarTextRightClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actDisplayExecute(Sender: TObject);
    procedure actTabsExecute(Sender: TObject);
    procedure actWelcomeExecute(Sender: TObject);
    procedure lblWidgetFormToFrontClick(Sender: TObject);
    procedure VisualStyleClick(Sender: TObject);
    procedure cbxVclStylesChange(Sender: TObject);
  private
    FPath: string;
    FCurrentVS: TRzVisualStyle;
    FCurrentGCS: TRzGradientColorStyle;
    FCustomFramingFrame: TFmeCustomFraming;
    FTabsFrame: TFmeTabs;
    FComboBoxesFrame: TFmeComboBoxes;
    FListBoxesFrame: TFmeListBoxes;
    FCommonControlsFrame: TFmeCommonControls;
    FEditsFrame: TFmeEdits;
    FButtonsFrame: TFmeButtons;
    FDisplayFrame: TFmeDisplay;
    FWidgetsForm: TFrmWidgets;
    {$IFDEF DEMO_DATA_AWARE_CTRLS}
    FReservationsFrame: TFmeReservations;
    {$ENDIF}
    FShellFrame: TFmeShell;
    FLauncherFrame: TFmeLauncher;
    FSendMessagesFrame: TFmeSendMessages;
    procedure ShowWidgetsForm;
    procedure UpdateSelectedPage( Action: TAction );
    procedure UpdateTray( Index: Integer );
  public
    procedure ClearNotes;
    procedure LoadNotes( const FileName: string );
    procedure RestoreMainNotes;
    procedure ClearWidgetRef;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  {$IFDEF VCL160_OR_HIGHER}
  Vcl.Themes,
  Vcl.Styles,
  {$ENDIF}
  SysUtils;

procedure TfrmMain.FormCreate(Sender: TObject);
{$IFDEF VCL160_OR_HIGHER}
var
  I: Integer;
{$ENDIF}
begin
  FPath := ExtractFilePath( ParamStr( 0 ) );
  tbrMain.RestoreLayout;
  RestoreMainNotes;
  {$IFNDEF DEMO_DATA_AWARE_CTRLS}
  ActDatabase.Enabled := False;
  {$ENDIF}

  UpdateTray( 0 );
  Tray.ShowBalloonHint( 'Konopka Signature VCL Controls ' + RaizeComponents_Version + ' Demo',
                        'This message brought to you via the TRzTrayIcon component. Use this icon to restore the application after minimizing.' );
  FCurrentVS := vsGradient;
  FCurrentGCS := gcsSystem;


  {$IFDEF VCL160_OR_HIGHER}
  grpVclStyles.Visible := True;
  for I := Low( TStyleManager.StyleNames ) to High( TStyleManager.StyleNames ) do
    cbxVclStyles.Items.Add( TStyleManager.StyleNames[ I ] );

  cbxVclStyles.ItemIndex := cbxVclStyles.Items.IndexOf( TStyleManager.ActiveStyle.Name );
  {$ENDIF}
end;


procedure TfrmMain.cbxVclStylesChange(Sender: TObject);
begin
  {$IFDEF VCL160_OR_HIGHER}
  TStyleManager.SetStyle( cbxVclStyles.Text );
  {$ENDIF}
end;



procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  tbrMain.SaveLayout;
end;


procedure TfrmMain.ClearNotes;
begin
  edtNotes.Clear;
end;


procedure TfrmMain.LoadNotes( const FileName: string );
begin
  edtNotes.Clear;
  edtNotes.Lines.LoadFromFile( FPath + FileName + '.txt' );
end;


procedure TfrmMain.RestoreMainNotes;
begin
  LoadNotes( 'MainForm' );
end;


procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
  Close;
end;


procedure TfrmMain.actWindowCloseAllExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := MDIChildCount - 1 downto 0 do
    MDIChildren[ I ].Close;
end;


procedure TfrmMain.mnuMainToolbarClick(Sender: TObject);
begin
  mnuMainToolbar.Checked := not mnuMainToolbar.Checked;
  tbrMain.Visible := mnuMainToolbar.Checked;
end;


procedure TfrmMain.mnuStatusBarClick(Sender: TObject);
begin
  mnuStatusBar.Checked := not mnuStatusBar.Checked;
  sbrMain.Visible := mnuStatusBar.Checked;
end;


procedure TfrmMain.mnuNotesClick(Sender: TObject);
begin
  mnuNotes.Checked := not mnuNotes.Checked;
  pnlNotes.Visible := mnuNotes.Checked;
end;


procedure TfrmMain.RzGroup1Items0Click(Sender: TObject);
begin
  RzGroupBar1.Style := gbsCategoryView;
  RzGroupBar1.Color := $005591B9;
end;

procedure TfrmMain.RzGroup1Items1Click(Sender: TObject);
begin
  RzGroupBar1.Style := gbsTaskList;
  RzGroupBar1.Color := $00B4E6F0;
end;

procedure TfrmMain.RzGroup1Items2Click(Sender: TObject);
begin
  RzGroupBar1.Style := gbsOutlook;
end;


procedure TfrmMain.UpdateSelectedPage( Action: TAction );
begin
  actWelcome.Checked          := actWelcome = Action;
  actCustomFraming.Checked    := actCustomFraming = Action;
  actTabs.Checked             := actTabs = Action;
  actEdits.Checked            := actEdits = Action;
  actComboBoxes.Checked       := actComboBoxes = Action;
  actListBoxes.Checked        := actListBoxes = Action;
  actCommonControls.Checked   := actCommonControls = Action;
  actButtons.Checked          := actButtons = Action;
  actDisplay.Checked          := actDisplay = Action;
  actDatabase.Checked         := actDatabase = Action;
  actWidgets.Checked          := actWidgets = Action;
  actShell.Checked            := actShell = Action;
  actLauncher.Checked         := actLauncher = Action;
  actSendMessages.Checked     := actSendMessages = Action;
end;


procedure TfrmMain.UpdateTray( Index: Integer );
const
  Hints: array[ 0..13 ] of String =
  ( 'Welcome',
    'Custom Framing',
    'Page Controls / Tab Controls',
    'Edit Controls',
    'Combo Box Controls',
    'List Box Controls',
    'Common Controls',
    'Button Controls',
    'Display',
    'Data-Aware Controls',
    'Widgets',
    'Shell Controls',
    'Application Launcher Components',
    'Send Email Messages (MAPI)' );
begin
  Tray.IconIndex := Index;
  Tray.Hint := 'RC Demo: ' + Hints[ Index ];
  stsSection.ImageIndex := Index;
  stsSection.Caption := Hints[ Index ];
end;


procedure TfrmMain.VisualStyleClick(Sender: TObject);
var
  Tag: Integer;
  C: TColor;
begin
  Tag := TRzGroupItem( Sender ).Tag;

  FCurrentVS := vsGradient;
  FCurrentGCS := gcsSystem;
  case Tag of
    1: FCurrentVS := vsClassic;

    2: FCurrentVS := vsWinXP;

    3, 4, 5:
    begin
      FCurrentGCS := TRzGradientColorStyle( Tag - 3 );
    end;
  end;

  RzGroupBar1.VisualStyle := FCurrentVS;
  RzGroupBar1.GradientColorStyle := FCurrentGCS;
  if ( FCurrentVS = vsGradient ) and ( FCurrentGCS = gcsCustom ) then
  begin
    grpVisualStyles.GroupController := RzGroupController1;
    grpGroupBarStyles.GroupController := RzGroupController1;
    grpDemos.GroupController := RzGroupController1;
    grpToolbar.GroupController := RzGroupController1;
  end;


  sbrMain.VisualStyle := FCurrentVS;
  sbrMain.GradientColorStyle := FCurrentGCS;
  tbrMain.VisualStyle := FCurrentVS;
  tbrMain.GradientColorStyle := FCurrentGCS;
  pnlGroupBar.VisualStyle := FCurrentVS;
  pnlGroupBar.GradientColorStyle := FCurrentGCS;
  pnlHeader.VisualStyle := FCurrentVS;
  pnlHeader.GradientColorStyle := FCurrentGCS;
  pnlWidgetsHeader.VisualStyle := FCurrentVS;
  pnlWidgetsHeader.GradientColorStyle := FCurrentGCS;
  RzMenuController1.GradientColorStyle := FCurrentGCS;

  if FCurrentVS = vsGradient then
    C := GetGradientPanelFrameColor( FCurrentGCS )
  else
    C := clBtnShadow;
  pgcWorkArea.FlatColor := C;

  if FCustomFramingFrame <> nil then
    FCustomFramingFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  if FTabsFrame <> nil then
    FTabsFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  if FComboBoxesFrame <> nil then
    FComboBoxesFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  if FListBoxesFrame <> nil then
    FListBoxesFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  if FCommonControlsFrame <> nil then
    FCommonControlsFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  if FEditsFrame <> nil then
    FEditsFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  if FButtonsFrame <> nil then
    FButtonsFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  if FDisplayFrame <> nil then
    FDisplayFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  {$IFDEF DEMO_DATA_AWARE_CTRLS}
  if FReservationsFrame <> nil then
    FReservationsFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  {$ENDIF}
  if FShellFrame <> nil then
    FShellFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  if FLauncherFrame <> nil then
    FLauncherFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  if FSendMessagesFrame <> nil then
    FSendMessagesFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
end;


procedure TfrmMain.actWelcomeExecute(Sender: TObject);
begin
  pgcWorkArea.ActivePageIndex := 0;
  RestoreMainNotes;


  (*
  if F*Frame = nil then
  begin
    F*Frame := TFme*.Create( Application );
    F*Frame.Parent := Tab*;
    F*Frame.Align := alClient;
  end;

  pgcWorkArea.ActivePageIndex := act*.Tag;
  LoadNotes( '*' );
  *)
  UpdateTray( 0 );
  UpdateSelectedPage( actWelcome );
end;


procedure TfrmMain.actCustomFramingExecute(Sender: TObject);
begin
  if FCustomFramingFrame = nil then
  begin
    FCustomFramingFrame := TFmeCustomFraming.Create( Application );
    FCustomFramingFrame.Parent := TabCustomFraming;
    FCustomFramingFrame.Align := alClient;
    FCustomFramingFrame.Init;
    FCustomFramingFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  end;

  pgcWorkArea.ActivePage := TabCustomFraming;
  LoadNotes( 'CustomFraming' );
  UpdateTray( 1 );
  UpdateSelectedPage( actCustomFraming );
end;


procedure TfrmMain.actTabsExecute(Sender: TObject);
begin
  if FTabsFrame = nil then
  begin
    FTabsFrame := TFmeTabs.Create( Application );
    FTabsFrame.Parent := TabTabs;
    FTabsFrame.Align := alClient;
    FTabsFrame.Init;
    FTabsFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  end;

  pgcWorkArea.ActivePageIndex := actTabs.Tag;
  LoadNotes( 'Tabs' );
  UpdateTray( 2 );
  UpdateSelectedPage( actTabs );
end;


procedure TfrmMain.actEditsExecute(Sender: TObject);
begin
  if FEditsFrame = nil then
  begin
    FEditsFrame := TFmeEdits.Create( Application );
    FEditsFrame.Parent := TabEdits;
    FEditsFrame.Align := alClient;
    FEditsFrame.Init;
    FEditsFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  end;

  pgcWorkArea.ActivePageIndex := actEdits.Tag;
  LoadNotes( 'Edits' );
  UpdateTray( 3 );
  UpdateSelectedPage( actEdits );
end;


procedure TfrmMain.actComboBoxesExecute(Sender: TObject);
begin
  if FComboBoxesFrame = nil then
  begin
    FComboBoxesFrame := TFmeComboBoxes.Create( Application );
    FComboBoxesFrame.Parent := TabComboBoxes;
    FComboBoxesFrame.Align := alClient;
    FComboBoxesFrame.Init;
    FComboBoxesFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  end;

  pgcWorkArea.ActivePageIndex := actComboBoxes.Tag;
  LoadNotes( 'ComboBoxes' );
  UpdateTray( 4 );
  UpdateSelectedPage( actComboBoxes );
end;


procedure TfrmMain.actListBoxesExecute(Sender: TObject);
begin
  if FListBoxesFrame = nil then
  begin
    FListBoxesFrame := TFmeListBoxes.Create( Application );
    FListBoxesFrame.Parent := TabListBoxes;
    FListBoxesFrame.Align := alClient;
    FListBoxesFrame.Init;
    FListBoxesFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  end;

  pgcWorkArea.ActivePageIndex := actListBoxes.Tag;
  LoadNotes( 'ListBoxes' );
  UpdateTray( 5 );
  UpdateSelectedPage( actListBoxes );
end;


procedure TfrmMain.actCommonControlsExecute(Sender: TObject);
begin
  if FCommonControlsFrame = nil then
  begin
    FCommonControlsFrame := TFmeCommonControls.Create( Application );
    FCommonControlsFrame.Parent := TabCommonControls;
    FCommonControlsFrame.Align := alClient;
    FCommonControlsFrame.Init;
    FCommonControlsFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  end;

  pgcWorkArea.ActivePageIndex := actCommonControls.Tag;
  LoadNotes( 'CommonControls' );
  UpdateTray( 6 );
  UpdateSelectedPage( actCommonControls );
end;


procedure TfrmMain.actButtonsExecute(Sender: TObject);
begin
  if FButtonsFrame = nil then
  begin
    FButtonsFrame := TFmeButtons.Create( Application );
    FButtonsFrame.Parent := TabButtons;
    FButtonsFrame.Align := alClient;
    FButtonsFrame.Init;
    FButtonsFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  end;

  pgcWorkArea.ActivePageIndex := actButtons.Tag;
  LoadNotes( 'Buttons' );
  UpdateTray( 7 );
  UpdateSelectedPage( actButtons );
end;


procedure TfrmMain.actDisplayExecute(Sender: TObject);
begin
  if FDisplayFrame = nil then
  begin
    FDisplayFrame := TFmeDisplay.Create( Application );
    FDisplayFrame.Parent := TabDisplay;
    FDisplayFrame.Align := alClient;
    FDisplayFrame.Init;
    FDisplayFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  end;

  pgcWorkArea.ActivePageIndex := actDisplay.Tag;
  LoadNotes( 'Display' );
  UpdateTray( 8 );
  UpdateSelectedPage( actDisplay );
end;


procedure TfrmMain.actDatabaseExecute(Sender: TObject);
begin
  {$IFDEF DEMO_DATA_AWARE_CTRLS}
  if FReservationsFrame = nil then
  begin
    FReservationsFrame := TFmeReservations.Create( Application );
    FReservationsFrame.Parent := TabDatabase;
    FReservationsFrame.Align := alClient;
    FReservationsFrame.Init;
    FReservationsFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  end;

  pgcWorkArea.ActivePageIndex := actDatabase.Tag;
  LoadNotes( 'Reservation' );
  UpdateTray( 9 );
  UpdateSelectedPage( actDatabase );
  {$ENDIF}
end;


procedure TfrmMain.ShowWidgetsForm;
begin
  if FWidgetsForm = nil then
  begin
    FWidgetsForm := TFrmWidgets.Create( Application );
    FWidgetsForm.Show;
  end;

  FWidgetsForm.BringToFront;
end;


procedure TfrmMain.actWidgetsExecute(Sender: TObject);
begin
  ShowWidgetsForm;
  pgcWorkArea.ActivePageIndex := actWidgets.Tag;
  LoadNotes( 'Widgets' );
  UpdateTray( 10 );
  UpdateSelectedPage( actWidgets );
end;


procedure TfrmMain.lblWidgetFormToFrontClick(Sender: TObject);
begin
  ShowWidgetsForm;
end;


procedure TfrmMain.ClearWidgetRef;
begin
  FWidgetsForm := nil;
end;


procedure TfrmMain.actShellExecute(Sender: TObject);
begin
  if FShellFrame = nil then
  begin
    FShellFrame := TFmeShell.Create( Application );
    FShellFrame.Parent := TabShell;
    FShellFrame.Align := alClient;
    FShellFrame.Init;
    FShellFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  end;

  pgcWorkArea.ActivePageIndex := actShell.Tag;
  LoadNotes( 'Shell' );
  UpdateTray( 11 );
  UpdateSelectedPage( actShell );
end;


procedure TfrmMain.actLauncherExecute(Sender: TObject);
begin
  if FLauncherFrame = nil then
  begin
    FLauncherFrame := TFmeLauncher.Create( Application );
    FLauncherFrame.Parent := TabLauncher;
    FLauncherFrame.Align := alClient;
    FLauncherFrame.Init;
    FLauncherFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  end;

  pgcWorkArea.ActivePageIndex := actLauncher.Tag;
  LoadNotes( 'Launcher' );
  UpdateTray( 12 );
  UpdateSelectedPage( actLauncher );
end;


procedure TfrmMain.actSendMessagesExecute(Sender: TObject);
begin
  if FSendMessagesFrame = nil then
  begin
    FSendMessagesFrame := TFmeSendMessages.Create( Application );
    FSendMessagesFrame.Parent := TabSendMessages;
    FSendMessagesFrame.Align := alClient;
    FSendMessagesFrame.Init;
    FSendMessagesFrame.UpdateVisualStyle( FCurrentVS, FCurrentGCS );
  end;

  pgcWorkArea.ActivePageIndex := actSendMessages.Tag;
  LoadNotes( 'SendMessage' );
  UpdateTray( 13 );
  UpdateSelectedPage( actSendMessages );
end;


procedure TfrmMain.actToolbarCustomizeExecute(Sender: TObject);
begin
  tbrMain.Customize;
end;


procedure TfrmMain.GrpToolbarNoTextClick(Sender: TObject);
begin
  tbrMain.TextOptions := ttoNoTextLabels;
end;

procedure TfrmMain.GrpToolbarShowTextClick(Sender: TObject);
begin
  tbrMain.TextOptions := ttoShowTextLabels;
end;

procedure TfrmMain.GrpToolbarTextRightClick(Sender: TObject);
begin
  tbrMain.TextOptions := ttoSelectiveTextOnRight;
end;


end.
