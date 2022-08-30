unit DemoBasicMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxControls, cxLookAndFeels, ActnList, ImgList, Menus, ComCtrls,
  StdCtrls;

type
  TDemoBasicMainForm = class(TForm)
    mmMain: TMainMenu;
    miAbout: TMenuItem;
    miHelp: TMenuItem;
    miDeveloperExpressontheweb: TMenuItem;
    miForum: TMenuItem;
    miDownloads: TMenuItem;
    miSeparator4: TMenuItem;
    miGridHelp: TMenuItem;
    miProducts: TMenuItem;
    miSeparator3: TMenuItem;
    ilMain: TImageList;
    alMain: TActionList;
    actHelp: TAction;
    actProducts: TAction;
    actDownloads: TAction;
    actForum: TAction;
    actDXOnTheWeb: TAction;
    actSupport: TAction;
    actAbout: TAction;
    miFile: TMenuItem;
    miOptions: TMenuItem;
    miExit: TMenuItem;
    actExit: TAction;
    sbMain: TStatusBar;
    lbDescrip: TLabel;
    actShowDemoDescription: TAction;
    miShowDemoDescription: TMenuItem;
    cxLookAndFeelController: TcxLookAndFeelController;
    miRate: TMenuItem;
    actRateDemo: TAction;
    memAboutText: TMemo;
    miSeparator: TMenuItem;
    miTouchMode: TMenuItem;
    procedure actDXOnTheWebExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actShowDemoDescriptionExecute(Sender: TObject);
    procedure actHelpExecute(Sender: TObject);
    procedure actRateDemoExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miTouchModeClick(Sender: TObject);
  protected
    procedure AddLookAndFeelMenu; virtual;
    procedure AdjustAboutText(AAboutText: TStrings); virtual;
    function GetDefaultLookAndFeelKind: TcxLookAndFeelKind; virtual;
    function IsNativeDefaultStyle: Boolean; virtual;
    procedure SetDefaultLookAndFeel; virtual;
    procedure ShowAbout(AModal, AOnTop: Boolean); virtual;
  end;

var
  DemoBasicMainForm: TDemoBasicMainForm;

implementation

uses
  DemoBasicAbout, cxClasses, DemoRating, DemoUtils;

{$R *.dfm}

procedure TDemoBasicMainForm.actDXOnTheWebExecute(
  Sender: TObject);
begin
  ShowWebPage(TdxWebPageType(TComponent(Sender).Tag));
end;

procedure TDemoBasicMainForm.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TDemoBasicMainForm.actShowDemoDescriptionExecute(
  Sender: TObject);
begin
  lbDescrip.Visible := not lbDescrip.Visible;
  TCustomAction(Sender).Checked := not TCustomAction(Sender).Checked;
end;

procedure TDemoBasicMainForm.actHelpExecute(Sender: TObject);
begin
  with Application do
  begin
    HelpFile := '..\..\Help\cxVertGrid.hlp';
    HelpCommand(HELP_FINDER, 0);
  end;  
end;

procedure TDemoBasicMainForm.AdjustAboutText(AAboutText: TStrings);
begin
  AAboutText.Assign(memAboutText.Lines);
end;

procedure TDemoBasicMainForm.actRateDemoExecute(Sender: TObject);
begin
  with TDemoRatingForm.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TDemoBasicMainForm.ShowAbout(AModal, AOnTop: Boolean);
begin
  if not Assigned(DemoBasicAboutForm) then
    DemoBasicAboutForm := TDemoBasicAboutForm.Create(Application);
  with DemoBasicAboutForm do
  begin
    AdjustAboutText(reDemoInfo.Lines);
    lbDemoName.Caption := ChangeFileExt(ExtractFileName(Application.ExeName),'');
    if AOnTop then
      FormStyle := fsStayOnTop
    else
      FormStyle := fsNormal;
    if AModal then
      ShowModal
    else
      Show;
  end;
end;

procedure TDemoBasicMainForm.actAboutExecute(Sender: TObject);
begin
  ShowAbout(False, True);
end;

procedure TDemoBasicMainForm.AddLookAndFeelMenu;
begin
  miOptions.Insert(miOptions.IndexOf(miSeparator),
    CreateLookAndFeelMenuItems(miOptions, cxLookAndFeelController));
end;

function TDemoBasicMainForm.GetDefaultLookAndFeelKind: TcxLookAndFeelKind;
begin
  Result := lfUltraFlat;
end;

function TDemoBasicMainForm.IsNativeDefaultStyle: Boolean;
begin
  Result := True;
end;

procedure TDemoBasicMainForm.miTouchModeClick(Sender: TObject);
begin
  cxLookAndFeelController.TouchMode := miTouchMode.Checked;
end;

procedure TDemoBasicMainForm.SetDefaultLookAndFeel;
begin
  cxLookAndFeelController.NativeStyle := IsNativeDefaultStyle;
  cxLookAndFeelController.Kind := GetDefaultLookAndFeelKind;
end;

procedure TDemoBasicMainForm.FormCreate(Sender: TObject);
begin
  SetDefaultLookAndFeel;
  AddLookAndFeelMenu;
  miTouchMode.Checked := cxIsTouchModeEnabled;
end;

end.

