unit HybridAppFrameCustomers;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HybridAppBaseFrame, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxLayoutcxEditAdapters, dxLayoutContainer, cxClasses, dxGDIPlusClasses, cxImage, cxTextEdit, cxMaskEdit, cxButtonEdit,
  dxLayoutControl, dxLayoutControlAdapters, Menus, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxNavigator,
  DB, cxDBData, cxDBLookupComboBox, dxDBSparkline, cxMemo, cxImageComboBox, cxGridViewLayoutContainer, cxGridLayoutView,
  cxGridDBLayoutView, cxGridCustomLayoutView, cxDropDownEdit, cxLabel, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGridCustomView, cxGrid, StdCtrls, cxButtons, HybridAppDM, cxGroupBox,
  cxDataControllerConditionalFormattingRulesManagerDialog, dxDateRanges;

type
  TfrmCustomers = class(TfrmBase)
    btnEdit: TcxButton;
    btnNew: TcxButton;
    liCustomers: TdxLayoutItem;
    cxGridCustomers: TcxGrid;
    gvCustomers: TcxGridDBTableView;
    gvCustomersName: TcxGridDBColumn;
    gvCustomersHomeOffice_Line: TcxGridDBColumn;
    gvCustomersHomeOffice_City: TcxGridDBColumn;
    gvCustomersHomeOffice_State: TcxGridDBColumn;
    gvCustomersHomeOffice_ZipCode: TcxGridDBColumn;
    gvCustomersFax: TcxGridDBColumn;
    gvCustomersPhone: TcxGridDBColumn;
    gvCustomersMonthlySales: TcxGridDBColumn;
    cxGridCustomersLevel1: TcxGridLevel;
    dxLayoutItem5: TdxLayoutItem;
    lbSelectedCustomer: TcxLabel;
    dxLayoutItem6: TdxLayoutItem;
    cmbEmployeesView: TcxComboBox;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutItem7: TdxLayoutItem;
    cxGridEmployees: TcxGrid;
    gvEmployees: TcxGridDBLayoutView;
    gvEmployeesItemPicture: TcxGridDBLayoutViewItem;
    gvEmployeesItemFullName: TcxGridDBLayoutViewItem;
    gvEmployeesItemPosition: TcxGridDBLayoutViewItem;
    gvEmployeesItemPhone: TcxGridDBLayoutViewItem;
    gvEmployeesItemCity: TcxGridDBLayoutViewItem;
    gvEmployeesGroup_Root: TdxLayoutGroup;
    cxGridLayoutItem1: TcxGridLayoutItem;
    gvEmployeesLayoutItem2: TcxGridLayoutItem;
    gvEmployeesLayoutItem3: TcxGridLayoutItem;
    gvEmployeesLayoutItem4: TcxGridLayoutItem;
    gvEmployeesLayoutItem5: TcxGridLayoutItem;
    gvStores: TcxGridDBLayoutView;
    gvStoresAddress_City: TcxGridDBLayoutViewItem;
    gvStoresAddress_Full: TcxGridDBLayoutViewItem;
    gvStoresCrestId: TcxGridDBLayoutViewItem;
    gvStoresGroup_Root: TdxLayoutGroup;
    gvStoresLayoutItem4: TcxGridLayoutItem;
    gvStoresLayoutItem16: TcxGridLayoutItem;
    cxGridLayoutItem2: TcxGridLayoutItem;
    cxGridEmployeesLevel1: TcxGridLevel;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    procedure btnEditClick(Sender: TObject);
    procedure cmbEmployeesViewPropertiesChange(Sender: TObject);
    procedure gvCustomersFocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord,
      AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
    procedure gvCustomersTcxGridDBDataControllerTcxDataSummaryFooterSummaryItems0GetText(Sender: TcxDataSummaryItem;
      const AValue: Variant; AIsFooter: Boolean; var AText: string);
    procedure gvCustomersTcxGridDBDataControllerTcxDataSummaryFooterSummaryItems1GetText(Sender: TcxDataSummaryItem;
      const AValue: Variant; AIsFooter: Boolean; var AText: string);
    procedure gvCustomersCustomDrawFooterCell(Sender: TcxGridTableView; ACanvas: TcxCanvas;
      AViewInfo: TcxGridColumnHeaderViewInfo; var ADone: Boolean);
    procedure gvCustomersNameGetFilterValues(Sender: TcxCustomGridTableItem; AValueList: TcxDataFilterValueList);
    procedure gvCustomersCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo;
      AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
  protected
    procedure DoAfterActivate; override;
    procedure Translate; override;

    procedure SetEmployeesFilter(const ACustomerID: Integer);
  end;

implementation

{$R *.dfm}

uses
  dxCore, MainUnit, LocalizationStrs;

{ TfrmCustomers }

procedure TfrmCustomers.btnEditClick(Sender: TObject);
begin
  DM.ApplyCustomerStoresFilter(gvCustomers.Controller.FocusedRecord.Values[gvCustomersMonthlySales.Index]);
  ShowEditPage(MainForm.tbiCustomerEdit);
end;

procedure TfrmCustomers.cmbEmployeesViewPropertiesChange(Sender: TObject);
begin
  if cmbEmployeesView.ItemIndex = 0 then
    cxGridEmployeesLevel1.GridView := gvEmployees
  else
    cxGridEmployeesLevel1.GridView := gvStores;
end;

procedure TfrmCustomers.DoAfterActivate;
begin
  inherited DoAfterActivate;
  gvCustomersFocusedRecordChanged(gvCustomers, nil, gvCustomers.Controller.FocusedRecord, False);
end;

procedure TfrmCustomers.Translate;
begin
  inherited Translate;

  cmbEmployeesView.Properties.Items[0] := cxGetResourceString(@sContactsColumn);
  cmbEmployeesView.Properties.Items[1] := cxGetResourceString(@sStoreColumn);
  cmbEmployeesView.ItemIndex := 0;

  liCustomers.Caption := cxGetResourceString(@sCustomersLabel);
  gvCustomersName.Caption := cxGetResourceString(@sName);
  gvCustomersHomeOffice_Line.Caption := cxGetResourceString(@sAddressColumn);
  gvCustomersHomeOffice_City.Caption := cxGetResourceString(@sCityColumn);
  gvCustomersHomeOffice_State.Caption := cxGetResourceString(@sStateColumn);
  gvCustomersHomeOffice_ZipCode.Caption := cxGetResourceString(@sZipCodeColumn);
  gvCustomersFax.Caption := cxGetResourceString(@sFaxColumn);
  gvCustomersPhone.Caption := cxGetResourceString(@sPhoneColumn);
  gvCustomersMonthlySales.Caption := cxGetResourceString(@sMonthlySalesColumn);

  btnNew.Caption := cxGetResourceString(@sNewButton);
  btnEdit.Caption := cxGetResourceString(@sEditButton);
end;

procedure TfrmCustomers.gvCustomersCellDblClick(Sender: TcxCustomGridTableView;
  ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
begin
  btnEdit.Click;
end;

procedure TfrmCustomers.gvCustomersCustomDrawFooterCell(Sender: TcxGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridColumnHeaderViewInfo; var ADone: Boolean);
begin
  if TcxGridFooterCellViewInfo(AViewInfo).SummaryItem.Index = 0 then
    TcxGridFooterCellViewInfo(AViewInfo).AlignmentHorz := taRightJustify;
end;

procedure TfrmCustomers.SetEmployeesFilter(const ACustomerID: Integer);
begin
  DM.ApplyCustomerEmployeesFilter(ACustomerID);
  DM.ApplyCustomerStoresFilter(ACustomerID);
end;

procedure TfrmCustomers.gvCustomersFocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord,
  AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
begin
  if AFocusedRecord <> nil then
  begin
    lbSelectedCustomer.Caption := AFocusedRecord.Values[gvCustomersName.Index];
    SetEmployeesFilter(AFocusedRecord.Values[gvCustomersMonthlySales.Index]);
  end
  else
  begin
    lbSelectedCustomer.Caption := '';
    SetEmployeesFilter(-1);
  end
end;

procedure TfrmCustomers.gvCustomersNameGetFilterValues(Sender: TcxCustomGridTableItem; AValueList: TcxDataFilterValueList);
begin
  AssignGridFilterBoxFont(Sender, AValueList);
end;

procedure TfrmCustomers.gvCustomersTcxGridDBDataControllerTcxDataSummaryFooterSummaryItems0GetText(
  Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: string);
begin
  AText := cxGetResourceString(@sCustomersLabel) + #13 + AText;
end;

procedure TfrmCustomers.gvCustomersTcxGridDBDataControllerTcxDataSummaryFooterSummaryItems1GetText(
  Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: string);
begin
  AText := cxGetResourceString(@sTotalSalesLabel) + #13 + AText;
end;

initialization
  RegisterFrame(IDCustomers, TfrmCustomers);

end.
