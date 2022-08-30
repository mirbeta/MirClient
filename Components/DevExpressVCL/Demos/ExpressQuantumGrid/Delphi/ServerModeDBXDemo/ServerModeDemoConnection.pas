unit ServerModeDemoConnection;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxContainer, cxEdit, cxMemo, cxProgressBar, cxMaskEdit, cxSpinEdit, cxButtons,
  cxGroupBox, cxRadioGroup, cxTextEdit, cxLabel, BaseForm, ExtCtrls,
  cxGridCardView, cxStyles, cxGridTableView, cxClasses, ComCtrls;

type

  TdxProgressEvent = procedure (Sender: TObject; const Value: Double) of object;

  TServerModeDemoConnectionForm = class(TfmBaseForm)
    Panel1: TPanel;
    lbSQLServer: TcxLabel;
    lbDatabase: TcxLabel;
    lbLoginName: TcxLabel;
    lbPassword: TcxLabel;
    lbRecordCount: TcxLabel;
    edSQLServer: TcxTextEdit;
    rgConnectUsing: TcxRadioGroup;
    edLoginName: TcxTextEdit;
    edPassword: TcxTextEdit;
    btAddRecordsAndStartDemo: TcxButton;
    btStartDemo: TcxButton;
    seCount: TcxSpinEdit;
    ProgressBar: TcxProgressBar;
    mDescription: TcxMemo;
    lbTableName: TcxLabel;
    edDatabase: TcxTextEdit;
    edTableName: TcxTextEdit;
    btTestConnection: TcxButton;
    lbCurrentCount: TcxLabel;
    procedure btAddRecordsAndStartDemoClick(Sender: TObject);
    procedure btStartDemoClick(Sender: TObject);
    procedure btTestConnectionClick(Sender: TObject);
    procedure rgConnectUsingPropertiesChange(Sender: TObject);
  private
    procedure ButtonsEnabled(AValue: Boolean);
    procedure SetProgressBarPosition(Sender: TObject; const Value: Double);
  public
    procedure AfterConstruction; override;
    procedure Connect(const ADatabaseName: string);
    procedure CreateDatabaseAndConnect;
    procedure CreateTable;
    procedure StartDemo;
  end;

var
  ServerModeDemoConnectionForm: TServerModeDemoConnectionForm;

implementation

uses
  ServerModeDemoMain, ServerModeDemoData;

{$R *.dfm}

{ TServerModeDemoConnectionForm }

procedure TServerModeDemoConnectionForm.AfterConstruction;
begin
  inherited AfterConstruction;
  Application.Title := GetCaption;
  edDatabase.Text := GetDatabaseName;
  edTableName.Text := GetTableName;
  lbDescription.Caption := GetDescription;
  Caption := GetCaption;
  mDescription.Lines.LoadFromFile((ExtractFilePath(Application.ExeName) + 'ConnectionFormDescription.txt'));
end;

procedure TServerModeDemoConnectionForm.btAddRecordsAndStartDemoClick(Sender: TObject);
begin
  ButtonsEnabled(False);
  Enabled := False;
  SetProgressBarPosition(Self, 0);
  try
    CreateDatabaseAndConnect;
    if not ServerModeDemoDataDM.TableExists then
      CreateTable;
    ServerModeDemoDataDM.AddRecords(seCount.Value, SetProgressBarPosition);
  finally
    Enabled := True;
    SetProgressBarPosition(Self, 0);
    btTestConnection.Enabled := True;
  end;
  StartDemo;
end;

procedure TServerModeDemoConnectionForm.btStartDemoClick(Sender: TObject);
begin
  ButtonsEnabled(False);
  try
    Connect(edDatabase.Text);
    StartDemo;
  finally
    btTestConnection.Enabled := True;
  end;
end;

procedure TServerModeDemoConnectionForm.btTestConnectionClick(Sender: TObject);
var
  ACount: Integer;
begin
  ShowHourglassCursor;
  try
    lbCurrentCount.Caption := '';
    try
      Connect('master');
    except
      btAddRecordsAndStartDemo.Enabled := False;
      btStartDemo.Enabled := False;
      raise;
    end;
  finally
    HideHourglassCursor;
  end;
  ACount := ServerModeDemoDataDM.GetRecordsCount;
  btAddRecordsAndStartDemo.Enabled := True;
  btStartDemo.Enabled := ACount > 0;
  if ACount > 0 then
    lbCurrentCount.Caption := Format('Current record count = %s', [FormatFloat('#,###', ACount)]);
  MessageDlg('Successful connection.', mtInformation, [mbOK], -1);
end;

procedure TServerModeDemoConnectionForm.rgConnectUsingPropertiesChange(Sender: TObject);
begin
  edLoginName.Enabled := rgConnectUsing.ItemIndex > 0;
  edPassword.Enabled := edLoginName.Enabled;
end;

procedure TServerModeDemoConnectionForm.Connect(const ADatabaseName: string);
begin
  ServerModeDemoDataDM.Connect(edSQLServer.Text, ADatabaseName, edLoginName.Text, edPassword.Text,
    rgConnectUsing.ItemIndex = 0)
end;

procedure TServerModeDemoConnectionForm.CreateDatabaseAndConnect;
begin
  Connect('master');
  try
    ServerModeDemoDataDM.CreateDatabase;
  except
    raise Exception.CreateFmt('Cannot create a database ''%s''', [GetDatabaseName]);
  end;
  Connect(GetDatabaseName);
end;

procedure TServerModeDemoConnectionForm.CreateTable;
begin
  try
    ServerModeDemoDataDM.CreateTable;
  except
    raise Exception.CreateFmt('Cannot create a table ''%s''', [GetTableName]);
  end;
end;

procedure TServerModeDemoConnectionForm.SetProgressBarPosition(Sender: TObject; const Value: Double);
begin
  ProgressBar.Position := Value;
  ProgressBar.Invalidate;
  Application.ProcessMessages;
end;

procedure TServerModeDemoConnectionForm.ButtonsEnabled(AValue: Boolean);
begin
  btTestConnection.Enabled := AValue;
  btAddRecordsAndStartDemo.Enabled := AValue;
  btStartDemo.Enabled := AValue;
end;

procedure TServerModeDemoConnectionForm.StartDemo;
begin
  ServerModeDemoMainForm.Initialize;
  ServerModeDemoMainForm.Show;
  Hide;
end;

end.
