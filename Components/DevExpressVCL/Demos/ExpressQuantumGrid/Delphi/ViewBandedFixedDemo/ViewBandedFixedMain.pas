unit ViewBandedFixedMain;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, cxGridLevel, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, DB,
  cxGridBandedTableView, cxGridDBBandedTableView, cxStyles,
  cxGridCustomPopupMenu, cxGridPopupMenu, ViewBandedFixedMainData, StdCtrls,
  Menus, ActnList, ImgList, cxLookAndFeels, ComCtrls,
  cxCustomData, cxGraphics, cxFilter, cxData, cxEdit, cxClasses,
  cxDataStorage, cxDBData, cxCalc, cxDBLookupComboBox, cxLookAndFeelPainters,
  BaseForm;

type
  TViewBandedFixedDemoMainForm = class(TfmBaseForm)
    glUserslSchedule: TcxGridLevel;
    cxGrid: TcxGrid;
    btvUsersSchedule: TcxGridDBBandedTableView;
    miOptions: TMenuItem;
    miShowBandsHeaders: TMenuItem;
    miShowIndicator: TMenuItem;
    miShowColumnsHeaders: TMenuItem;
    miMultiSelect: TMenuItem;
    btvUsersScheduleSUNDAY: TcxGridDBBandedColumn;
    btvUsersScheduleMONDAY: TcxGridDBBandedColumn;
    btvUsersScheduleTUESDAY: TcxGridDBBandedColumn;
    btvUsersScheduleTHURSDAY: TcxGridDBBandedColumn;
    btvUsersScheduleSATURDAY: TcxGridDBBandedColumn;
    btvUsersScheduleFRIDAY: TcxGridDBBandedColumn;
    btvUsersScheduleRowAvg: TcxGridDBBandedColumn;
    btvUsersScheduleRowSum: TcxGridDBBandedColumn;
    btvUsersScheduleUserName: TcxGridDBBandedColumn;
    btvUsersScheduleProjectName: TcxGridDBBandedColumn;
    btvUsersScheduleWEDNESDAY: TcxGridDBBandedColumn;
    cxGridPopupMenu: TcxGridPopupMenu;
    procedure FormShow(Sender: TObject);
    procedure miShowBandsHeadersClick(Sender: TObject);
    procedure miShowIndicatorClick(Sender: TObject);
    procedure miShowColumnsHeadersClick(Sender: TObject);
    procedure miMultiSelectClick(Sender: TObject);
  end;

var
  ViewBandedFixedDemoMainForm: TViewBandedFixedDemoMainForm;

implementation

{$R *.dfm}

uses
  AboutDemoForm;

procedure TViewBandedFixedDemoMainForm.FormShow(Sender: TObject);
begin
  cxGrid.FocusedView.DataController.Groups.FullExpand;
end;

procedure TViewBandedFixedDemoMainForm.miShowBandsHeadersClick(Sender: TObject);
begin
  btvUsersSchedule.OptionsView.BandHeaders := GetMenuItemChecked(Sender);
end;

procedure TViewBandedFixedDemoMainForm.miShowIndicatorClick(Sender: TObject);
begin
  btvUsersSchedule.OptionsView.Indicator := GetMenuItemChecked(Sender);
end;

procedure TViewBandedFixedDemoMainForm.miShowColumnsHeadersClick(Sender: TObject);
begin
  btvUsersSchedule.OptionsView.Header := GetMenuItemChecked(Sender);
end;

procedure TViewBandedFixedDemoMainForm.miMultiSelectClick(Sender: TObject);
begin
  btvUsersSchedule.OptionsSelection.MultiSelect := GetMenuItemChecked(Sender);
end;

end.
