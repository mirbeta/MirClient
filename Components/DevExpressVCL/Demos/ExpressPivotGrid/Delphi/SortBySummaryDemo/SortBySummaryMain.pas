unit SortBySummaryMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DemoBasicMain, cxControls, cxCustomPivotGrid, cxDBPivotGrid,
  cxLookAndFeels, Menus, StdCtrls, DemoBasicDM, cxGraphics, cxContainer,
  cxEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit, ExtCtrls, cxCheckBox,
  cxSpinEdit, cxStyles, cxClasses, cxCustomData, cxLookAndFeelPainters,
  cxGroupBox, cxRadioGroup, ActnList;

type
  TfrmSortBySummary = class(TfrmDemoBasicMain)
    DBPivotGrid: TcxDBPivotGrid;
    pgfPurchaseQuarter: TcxDBPivotGridField;
    pgfPaymentType: TcxDBPivotGridField;
    pgfQuantity: TcxDBPivotGridField;
    pgfCarName: TcxDBPivotGridField;
    pgfUnitPrice: TcxDBPivotGridField;
    pgfCompanyName: TcxDBPivotGridField;
    pgfPaymentAmount: TcxDBPivotGridField;
    pnSettings: TPanel;
    lbSortThe: TLabel;
    cbxSortField: TcxComboBox;
    lbSortBy: TLabel;
    cbxSortByField: TcxComboBox;
    lbShowTop: TLabel;
    lbValues: TLabel;
    bvSplitter: TBevel;
    speTopCount: TcxSpinEdit;
    cbxTopValuesShowOthers: TcxCheckBox;
    rgSortOrder: TcxRadioGroup;
    procedure SortFieldChanged(Sender: TObject);
    procedure SortByChanged(Sender: TObject);
    procedure speTopCountPropertiesChange(Sender: TObject);
    procedure cbxTopValuesShowOthersPropertiesChange(Sender: TObject);
    procedure GetGroupHeaderStyle(Sender: TcxCustomPivotGrid;
      AItem: TcxPivotGridViewDataItem; var AStyle: TcxStyle);
    procedure FormCreate(Sender: TObject);
    procedure rgSortOrderClick(Sender: TObject);
    procedure DBPivotGridLayoutChanged(Sender: TObject);
  private
    FLocked: Boolean;
  protected
    function CurrentField: TcxPivotGridField;
    function GetPivotGrid: TcxCustomPivotGrid; override;
  public
    { Public declarations }
  end;

var
  frmSortBySummary: TfrmSortBySummary;

implementation

{$R *.dfm}

function TfrmSortBySummary.CurrentField: TcxPivotGridField;
begin
  Result := PivotGrid.GetFieldByName(cbxSortField.Text);
end;

function TfrmSortBySummary.GetPivotGrid: TcxCustomPivotGrid;
begin
  Result := DBPivotGrid;
end;

procedure TfrmSortBySummary.SortFieldChanged(Sender: TObject);
begin
  FLocked := True;
  // sync settings with selected field
  if CurrentField.SortBySummaryInfo.Field = nil then
    cbxSortByField.ItemIndex := 0
  else
    cbxSortByField.Text := CurrentField.SortBySummaryInfo.Field.Caption;

  speTopCount.Value := CurrentField.TopValueCount;
  speTopCount.Properties.MaxValue := CurrentField.GroupValueList.Count;
  cbxTopValuesShowOthers.Checked := CurrentField.TopValueShowOthers;

  FLocked := False;
end;

procedure TfrmSortBySummary.SortByChanged(Sender: TObject);
begin
  if FLocked then Exit;
  CurrentField.SortBySummaryInfo.Field := PivotGrid.GetFieldByName(cbxSortByField.Text);
end;

procedure TfrmSortBySummary.speTopCountPropertiesChange(Sender: TObject);
begin
  if FLocked then Exit;
  CurrentField.TopValueCount := speTopCount.Value;
end;

procedure TfrmSortBySummary.cbxTopValuesShowOthersPropertiesChange(
  Sender: TObject);
begin
  if FLocked then Exit;
  CurrentField.TopValueShowOthers := cbxTopValuesShowOthers.Checked;
end;

procedure TfrmSortBySummary.GetGroupHeaderStyle(Sender: TcxCustomPivotGrid;
  AItem: TcxPivotGridViewDataItem; var AStyle: TcxStyle);
begin
  if AItem.GroupItem.RecordIndex = cxPivotGridOthersRecordIndex then
    AStyle := dmOrders.stBoldBlackFont;
end;

procedure TfrmSortBySummary.FormCreate(Sender: TObject);
begin
  inherited;
  SortFieldChanged(nil);
  pgfCompanyName.SortBySummaryInfo.Conditions.Add(pgfPaymentType, 'AmEx');
end;

procedure TfrmSortBySummary.rgSortOrderClick(Sender: TObject);
begin
  DBPivotGrid.OptionsBehavior.SortBySummaryDefaultOrder := TcxDataSortOrder(rgSortOrder.ItemIndex);
end;

procedure TfrmSortBySummary.DBPivotGridLayoutChanged(Sender: TObject);
begin
  SortFieldChanged(nil);
end;

end.
