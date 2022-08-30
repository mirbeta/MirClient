unit SummaryFooterDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cxGridLevel, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxControls, cxGridCustomView, cxGrid, cxCustomData,
  ExtCtrls, ActnList, ImgList, Menus, ComCtrls, cxGridCustomPopupMenu,
  cxGridPopupMenu, cxStyles, cxGraphics, cxFilter, cxData, cxEdit, DB,
  cxDBData, cxClasses, cxDataStorage, cxBlobEdit, cxSpinEdit, cxCheckBox,
  cxHyperLinkEdit, cxCurrencyEdit, cxDBLookupComboBox, cxCalendar,
  cxTimeEdit, cxImageComboBox, cxCalc, cxLookAndFeels, cxLookAndFeelPainters,
  BaseForm, cxGridCardView, CarsDataForGrid, cxNavigator;

type
  TSummaryFooterDemoMainForm = class(TfmBaseForm)
    cxGridPopupMenu1: TcxGridPopupMenu;
    Grid: TcxGrid;
    lvCars: TcxGridLevel;
    lvOrders: TcxGridLevel;
    miCustomizeSummaries: TMenuItem;
    miFooter: TMenuItem;
    miIgnoreNullValues: TMenuItem;
    miMultiSelect: TMenuItem;
    miOptions: TMenuItem;
    miSelectedRecordOnly: TMenuItem;
    miSummaries: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    tvCars: TcxGridDBTableView;
    tvCarsCategory: TcxGridDBColumn;
    tvCarscyl: TcxGridDBColumn;
    tvCarsDescription: TcxGridDBColumn;
    tvCarshp: TcxGridDBColumn;
    tvCarsHyperlink: TcxGridDBColumn;
    tvCarsModel: TcxGridDBColumn;
    tvCarsMPG_City: TcxGridDBColumn;
    tvCarsMPG_Highway: TcxGridDBColumn;
    tvCarsPicture: TcxGridDBColumn;
    tvCarsPrice: TcxGridDBColumn;
    tvCarsTorque: TcxGridDBColumn;
    tvCarsTrademark: TcxGridDBColumn;
    tvCarsTransmissAutomatic: TcxGridDBColumn;
    tvCarsTransmissSpeedCount: TcxGridDBColumn;
    tvOrders: TcxGridDBTableView;
    tvOrdersCustomerID: TcxGridDBColumn;
    tvOrdersDescription: TcxGridDBColumn;
    tvOrdersPaymentAmount: TcxGridDBColumn;
    tvOrdersPaymentType: TcxGridDBColumn;
    tvOrdersPurchaseDate: TcxGridDBColumn;
    tvOrdersQuantity: TcxGridDBColumn;
    tvOrdersTime: TcxGridDBColumn;

    procedure btnAddSummaryClick(Sender: TObject);
    procedure miCustomizeSummariesClick(Sender: TObject);
    procedure miSelectedRecordOnlyClick(Sender: TObject);
    procedure miIgnoreNullValuesClick(Sender: TObject);
    procedure miMultiSelectClick(Sender: TObject);
    procedure miFooterClick(Sender: TObject);
    procedure GridFocusedViewChanged(Sender: TcxCustomGrid; APrevFocusedView, AFocusedView: TcxCustomGridView);
    procedure FormShow(Sender: TObject);
  protected
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
  end;

var
  SummaryFooterDemoMainForm: TSummaryFooterDemoMainForm;

implementation

{$R *.dfm}

uses
  SummaryFooterDemoData, SummaryFooterDemoEditSummary, AboutDemoForm;

procedure TSummaryFooterDemoMainForm.btnAddSummaryClick(Sender: TObject);
begin
  SummaryFooterDemoEditSummaryForm.ShowModal;
end;

procedure TSummaryFooterDemoMainForm.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  UpdateTableViewStyleSheet(tvCars);
  UpdateTableViewStyleSheet(tvOrders);
end;

procedure TSummaryFooterDemoMainForm.miCustomizeSummariesClick(Sender: TObject);
begin
  SummaryFooterDemoEditSummaryForm.ShowModal;
end;

procedure TSummaryFooterDemoMainForm.miSelectedRecordOnlyClick(Sender: TObject);
var
  ASummary: TcxDataSummary;
begin
  if Assigned(Grid.FocusedView) then
  begin
    ASummary := Grid.FocusedView.DataController.Summary;
    if GetMenuItemChecked(Sender) then
      ASummary.Options := ASummary.Options + [soSelectedRecords]
    else
      ASummary.Options := ASummary.Options - [soSelectedRecords];
  end;
end;

procedure TSummaryFooterDemoMainForm.miIgnoreNullValuesClick(Sender: TObject);
var
  ASummary: TcxDataSummary;
begin
  if Assigned(Grid.FocusedView) then
  begin
    ASummary := Grid.FocusedView.DataController.Summary;
    if GetMenuItemChecked(Sender) then
      ASummary.Options := ASummary.Options + [soNullIgnore]
    else
      ASummary.Options := ASummary.Options - [soNullIgnore];
  end;
end;

procedure TSummaryFooterDemoMainForm.miMultiSelectClick(Sender: TObject);
var
  AView: TcxGridDBTableView;
begin
  if Assigned(Grid.FocusedView) then
  begin
    AView := TcxGridDBTableView(Grid.FocusedView);
    AView.OptionsSelection.MultiSelect := GetMenuItemChecked(Sender);
  end;
end;

procedure TSummaryFooterDemoMainForm.miFooterClick(Sender: TObject);
var
  AView: TcxGridDBTableView;
begin
  if Assigned(Grid.FocusedView) then
  begin
    AView := TcxGridDBTableView(Grid.FocusedView);
    AView.OptionsView.Footer := GetMenuItemChecked(Sender);
  end;
end;

procedure TSummaryFooterDemoMainForm.GridFocusedViewChanged(
  Sender: TcxCustomGrid; APrevFocusedView, AFocusedView: TcxCustomGridView);
var
  AView: TcxGridDBTableView;
begin
  AView := TcxGridDBTableView(Grid.FocusedView);
  MenuItemSetChecked('miSelectedRecordOnly', soSelectedRecords in AView.DataController.Summary.Options);
  MenuItemSetChecked('miIgnoreNullValues', soNullIgnore in AView.DataController.Summary.Options);
  MenuItemSetChecked('miMultiSelect', AView.OptionsSelection.MultiSelect);
  MenuItemSetChecked('miFooter', AView.OptionsView.Footer);
end;

procedure TSummaryFooterDemoMainForm.FormShow(Sender: TObject);
begin
  if dmGridCars.mdModels.Active then
  begin
    dmGridCars.mdModels.First;
    if Assigned(tvCars.Controller.FocusedRecord) then
      tvCars.Controller.FocusedRecord.Expanded := true;
  end;
end;

end.  
