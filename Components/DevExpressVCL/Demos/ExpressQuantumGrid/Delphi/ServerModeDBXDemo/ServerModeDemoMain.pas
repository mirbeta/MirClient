unit ServerModeDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  BaseForm, ServerModeDemoData, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage,
  cxEdit, cxCalendar, cxGridCustomPopupMenu, cxGridPopupMenu, cxGridLevel,
  cxGridCustomTableView, cxGridTableView, cxGridServerModeTableView, cxClasses,
  cxGridCustomView, cxGrid, cxImageComboBox, ImgList, cxGridCardView, Menus,
  ComCtrls, StdCtrls, cxNavigator, cxGridBandedTableView, cxGridServerModeBandedTableView;

type
  TServerModeDemoMainForm = class(TfmBaseForm)
    cxGridPopupMenu1: TcxGridPopupMenu;
    cxGrid1: TcxGrid;
    cxGrid1ServerModeTableView1: TcxGridServerModeTableView;
    cxGrid1ServerModeTableView1OID: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1Subject: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1From: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1Sent: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1Size: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1HasAttachment: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1Priority: TcxGridServerModeColumn;
    cxGrid1Level1: TcxGridLevel;
    ilImages: TcxImageList;
    mOptions: TMenuItem;
    mCancelOnExit: TMenuItem;
    mDeleting: TMenuItem;
    mDeletingConfirmation: TMenuItem;
    mEditing: TMenuItem;
    mInserting: TMenuItem;
    cxGrid1Level2: TcxGridLevel;
    cxGrid1ServerModeBandedTableView1: TcxGridServerModeBandedTableView;
    cxGrid1ServerModeBandedTableView1OID: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1Subject: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1From: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1Sent: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1Size: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1HasAttachment: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1Priority: TcxGridServerModeBandedColumn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure UpdateOptionsDataView(Sender: TObject);
    procedure cxGrid1ActiveTabChanged(Sender: TcxCustomGrid; ALevel: TcxGridLevel);
  public
    procedure Initialize;
  end;

var
  ServerModeDemoMainForm: TServerModeDemoMainForm;

implementation

{$R *.dfm}

{ TServerModeDemoMainForm }

procedure TServerModeDemoMainForm.cxGrid1ActiveTabChanged(Sender: TcxCustomGrid; ALevel: TcxGridLevel);
begin
  if ALevel.GridView is TcxGridServerModeBandedTableView then
  begin
    cxGrid1ServerModeTableView1.DataController.DataSource := nil;
    cxGrid1ServerModeBandedTableView1.DataController.DataSource := ServerModeDemoDataDM.ServerModeDataSource;
    cxGrid1ServerModeBandedTableView1.DataController.DataSource.Active := True;
  end
  else
  begin
    cxGrid1ServerModeBandedTableView1.DataController.DataSource := nil;
    cxGrid1ServerModeTableView1.DataController.DataSource := ServerModeDemoDataDM.ServerModeDataSource;
    cxGrid1ServerModeTableView1.DataController.DataSource.Active := True;
  end;
end;

procedure TServerModeDemoMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Application.Terminate;
end;

procedure TServerModeDemoMainForm.Initialize;
begin
  Caption := GetCaption;
  lbDescription.Caption := GetDescription;
  ServerModeDemoDataDM.ServerModeDataSource.TableName := GetTableName;
  cxGrid1ServerModeTableView1.DataController.DataSource := ServerModeDemoDataDM.ServerModeDataSource;
  ServerModeDemoDataDM.ServerModeDataSource.Active := True;
end;

procedure TServerModeDemoMainForm.UpdateOptionsDataView(Sender: TObject);
begin
  with cxGrid1ServerModeTableView1.OptionsData do
  begin
    CancelOnExit := GetMenuItemChecked(mCancelOnExit);
    Deleting := GetMenuItemChecked(mDeleting);
    DeletingConfirmation := GetMenuItemChecked(mDeletingConfirmation);
    Editing := GetMenuItemChecked(mEditing);
    Inserting := GetMenuItemChecked(mInserting);
  end;
  with cxGrid1ServerModeBandedTableView1.OptionsData do
  begin
    CancelOnExit := GetMenuItemChecked(mCancelOnExit);
    Deleting := GetMenuItemChecked(mDeleting);
    DeletingConfirmation := GetMenuItemChecked(mDeletingConfirmation);
    Editing := GetMenuItemChecked(mEditing);
    Inserting := GetMenuItemChecked(mInserting);
  end;
end;

end.
