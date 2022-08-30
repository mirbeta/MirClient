unit EditorsStylesDemoIssues;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, cxPropertiesStore, cxContainer, cxEdit, StdCtrls, ComCtrls,
  cxDropDownEdit, cxFontNameComboBox, cxDBFontNameComboBox, cxCheckComboBox,
  cxDBCheckComboBox, cxCheckListBox, cxDBCheckListBox, cxTrackBar,
  cxDBTrackBar, cxProgressBar, cxDBProgressBar, cxTextEdit, cxMaskEdit,
  cxColorComboBox, cxDBColorComboBox, cxControls, cxLabel, cxDBLabel,
  cxSplitter, cxNavigator, ExtCtrls, cxLookupEdit, cxDBLookupEdit,
  cxDBLookupComboBox, DB, cxSpinEdit, cxDBEdit, cxGroupBox,
  EditorsStylesDemoBase, cxFilterControl, cxSpinButton, cxMemo, cxStyles,
  cxCustomData, cxGraphics, cxFilter, cxData, cxDBData, cxClasses,
  cxDBNavigator;

type
  TEditorsStylesDemoIssuesFrame = class(TEditorsStylesDemoBaseFrame)
    Panel2: TPanel;
    lblDBIssueID: TcxDBLabel;
    chcbIDEs: TcxDBCheckComboBox;
    lbProject: TcxLabel;
    lblIssueID: TcxLabel;
    edProject: TcxDBLookupComboBox;
    lbCreator: TcxLabel;
    lbOwner: TcxLabel;
    edCreator: TcxDBLookupComboBox;
    edOwner: TcxDBLookupComboBox;
    gbStatus: TcxGroupBox;
    tbStatus: TcxDBTrackBar;
    lbNew: TcxLabel;
    lbPostponed: TcxLabel;
    lbFixed: TcxLabel;
    lbRejected: TcxLabel;
    gbProgress: TcxGroupBox;
    pgbProgress: TcxDBProgressBar;
    seProgress: TcxDBSpinEdit;
    lbIDEs: TcxLabel;
    Panel1: TPanel;
    lbIssue: TcxLabel;
    edIssue: TcxDBTextEdit;
    pnlNotification: TPanel;
    lbNotification: TcxLabel;
    chlbUsers: TcxDBCheckListBox;
    lbCheckProgress: TcxLabel;
    lbProgress: TcxLabel;
    seCheckProgress: TcxDBSpinEdit;
    cxLabel1: TcxLabel;
    seFirstTarget: TcxDBSpinEdit;
    cxDBNavigator1: TcxDBNavigator;
    procedure seCheckProgressPropertiesChange(Sender: TObject);
    procedure seProgressPropertiesChange(Sender: TObject);
    procedure seFirstTargetPropertiesChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure FillUsersCheckListBox;
  public
    constructor Create(AOwner: TComponent); override;
    function Name: string; override;
    function BriefName: string; override;
    function StylesIniPath: string; override;
    function Description: String; override;
  end;

var
  EditorsStylesDemoIssuesFrame: TEditorsStylesDemoIssuesFrame;

implementation

uses EditorsStylesDemoData;

{$R *.dfm}

{ TEditorsStylesDemoIssuesFrame }

constructor TEditorsStylesDemoIssuesFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HintStyle := hcstBlueSlideUp;
  FDisplayStyle := shtRainyDay;
  FTempDisplayStyle := shtRainyDay;
end;

procedure TEditorsStylesDemoIssuesFrame.FillUsersCheckListBox;
begin
  chlbUsers.Clear;
  with EditorsStylesDemoDataDM do
  begin
    tblUSERS.First;
    while not tblUSERS.Eof do
    begin
      chlbUsers.Items.Add.Text := string(tblUSERSUserName.Value + ' <' + tblUSERSEMAIL.Value + '>');
      tblUSERS.Next;
    end;
    tblITEMS.Refresh;
  end;
end;

function TEditorsStylesDemoIssuesFrame.Name: string;
begin
  Result := 'Issues database';
end;

function TEditorsStylesDemoIssuesFrame.BriefName: string;
begin
  Result := 'Issues';
end;

procedure TEditorsStylesDemoIssuesFrame.seCheckProgressPropertiesChange(
  Sender: TObject);
begin
  pgbProgress.Properties.OverloadValue := seCheckProgress.Value;
end;

procedure TEditorsStylesDemoIssuesFrame.seProgressPropertiesChange(
  Sender: TObject);
begin
  seCheckProgress.Properties.MaxValue := seProgress.Value;
  if seCheckProgress.Value > seProgress.Value then
    seCheckProgress.Value := seProgress.Value;
end;

function TEditorsStylesDemoIssuesFrame.StylesIniPath: string;
begin
  Result := 'StylesFrmIssues\';
end;

procedure TEditorsStylesDemoIssuesFrame.seFirstTargetPropertiesChange(
  Sender: TObject);
begin
  pgbProgress.Properties.PeakValue := TcxDBSpinEdit(Sender).Value;
end;

function TEditorsStylesDemoIssuesFrame.Description: String;
begin
  Result := 'Issues Database Notes';
end;

procedure TEditorsStylesDemoIssuesFrame.FormShow(Sender: TObject);
begin
  FillUsersCheckListBox;
end;

initialization
  EditorsStylesDemoFrameManager.RegisterFrameClass(TEditorsStylesDemoIssuesFrame);

end.
