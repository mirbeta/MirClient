unit ServerModeDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  BaseForm, ServerModeDemoData, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage,
  cxEdit, cxCalendar, cxGridCustomPopupMenu, cxGridPopupMenu, cxGridLevel,
  cxGridCustomTableView, cxGridTableView, cxGridServerModeTableView, cxClasses,
  cxGridCustomView, cxGrid, cxImageComboBox, ImgList, cxGridCardView, Menus,
  ComCtrls, StdCtrls, cxNavigator, cxCheckBox, cxSpinEdit, cxGridBandedTableView, 
  cxGridServerModeBandedTableView;

type
  TServerModeDemoMainForm = class(TfmBaseForm)
    cxGridPopupMenu1: TcxGridPopupMenu;
    cxGrid1: TcxGrid;
    cxGrid1ServerModeTableView1: TcxGridServerModeTableView;
    cxGrid1Level1: TcxGridLevel;
    cxGrid1ServerModeTableView1OrderDate: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1Trademark: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1Model: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1HP: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1TransmissSpeedCount: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1TransmissAutomatic: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1Category: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1Price: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1FirstName: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1LastName: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1Company: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1Prefix: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1Title: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1Address: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1City: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1State: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1Source: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1Customer: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1HomePhone: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1Description: TcxGridServerModeColumn;
    cxGrid1ServerModeTableView1Email: TcxGridServerModeColumn;
    ImageList: TImageList;
    cxGrid1Level2: TcxGridLevel;
    cxGrid1ServerModeBandedTableView1: TcxGridServerModeBandedTableView;
    cxGrid1ServerModeBandedTableView1OrderDate: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1Trademark: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1Model: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1HP: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1Category: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1Price: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1FirstName: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1LastName: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1Company: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1Prefix: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1Title: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1Address: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1City: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1State: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1Source: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1HomePhone: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1Description: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1Email: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1TransmissAutomatic: TcxGridServerModeBandedColumn;
    cxGrid1ServerModeBandedTableView1Customer: TcxGridServerModeBandedColumn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
    cxGrid1ServerModeBandedTableView1.DataController.DataSource := ServerModeDemoDataDM.ServerModeQueryDataSource;
    cxGrid1ServerModeBandedTableView1.DataController.DataSource.Active := True;
  end
  else
  begin
    cxGrid1ServerModeBandedTableView1.DataController.DataSource := nil;
    cxGrid1ServerModeTableView1.DataController.DataSource := ServerModeDemoDataDM.ServerModeQueryDataSource;
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
  cxGrid1ServerModeTableView1.DataController.DataSource := ServerModeDemoDataDM.ServerModeQueryDataSource;
  ServerModeDemoDataDM.ServerModeQueryDataSource.Active := True;
end;

end.
