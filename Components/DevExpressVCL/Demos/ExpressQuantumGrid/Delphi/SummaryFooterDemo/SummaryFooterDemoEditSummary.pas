unit SummaryFooterDemoEditSummary;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, cxControls, cxContainer, cxEdit, cxTextEdit,
  cxMaskEdit, cxDropDownEdit, cxCustomData, cxGridDBTableView, cxGridTableView,
  cxCalendar, ExtCtrls, cxCheckBox, cxButtons, cxImageComboBox, DB, cxListBox,
  cxLookAndFeelPainters, cxGraphics, cxLookAndFeels, Menus, cxGroupBox, cxLabel;

type
  TSummaryFooterDemoEditSummaryForm = class(TForm)
    gbSummaries: TcxGroupBox;
    lbSummaries: TcxListBox;
    Bevel1: TBevel;
    btnAdd: TcxButton;
    btnDelete: TcxButton;
    btnExit: TcxButton;
    cbCalculatedColumn: TcxComboBox;
    cbFooterSummaryColumn: TcxComboBox;
    cbSummaryKind: TcxImageComboBox;
    Label1: TcxLabel;
    Label2: TcxLabel;
    Label3: TcxLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbCalculatedColumnPropertiesChange(Sender: TObject);
    procedure cbFooterSummaryColumnPropertiesChange(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateSummaryKinds(AField: TField = nil);
    procedure UpdateSummaryList(AFistItemActive: Boolean);
    function GetFooterSummaryItems: TcxDataFooterSummaryItems;
    function GetFocusedView: TcxGridDBTableView;

  public
    { Public declarations }
    property FooterSummaryItems: TcxDataFooterSummaryItems read GetFooterSummaryItems;
    property GridView: TcxGridDBTableView read GetFocusedView;
  end;

var
  SummaryFooterDemoEditSummaryForm: TSummaryFooterDemoEditSummaryForm;

implementation

uses
{$IFDEF CLR}
  Variants,
{$ENDIF}
  SummaryFooterDemoMain, cxGridCustomTableView, SummaryFooterDemoData;

{$R *.dfm}

procedure TSummaryFooterDemoEditSummaryForm.btnAddClick(Sender: TObject);
var
  AColumn: TcxGridDBColumn;
begin
  with TcxGridDBTableSummaryItem(FooterSummaryItems.Add) do
  try
    try
      BeginUpdate;
      if cbFooterSummaryColumn.ItemIndex <> -1 then
      Column := TcxGridDBColumn(
        GridView.VisibleColumns[cbFooterSummaryColumn.ItemIndex]);
      AColumn := nil;
      if cbCalculatedColumn.ItemIndex <> -1 then
        AColumn := TcxGridDBColumn(
          GridView.VisibleColumns[cbCalculatedColumn.ItemIndex]);
      if Assigned(AColumn) then
        FieldName := AColumn.DataBinding.FieldName;
      Kind := TcxSummaryKind(Integer(cbSummaryKind.EditValue));
      cbFooterSummaryColumn.ItemIndex := 0;
      cbSummaryKind.EditValue := 0;
    finally
      EndUpdate;
    end;
  except
    MessageDlg('Invalid Data', mtError, [mbOK], 0);
    FooterSummaryItems.Items[FooterSummaryItems.Count-1].Free;
  end;
  UpdateSummaryList(False);
end;

function TSummaryFooterDemoEditSummaryForm.GetFocusedView: TcxGridDBTableView;
begin
  Result := TcxGridDBTableView(SummaryFooterDemoMainForm.Grid.FocusedView);
end;

function TSummaryFooterDemoEditSummaryForm.GetFooterSummaryItems: TcxDataFooterSummaryItems;
var
  ASummary: TcxDataSummary;
begin
  ASummary := SummaryFooterDemoMainForm.
    Grid.FocusedView.DataController.Summary;
  Result := ASummary.FooterSummaryItems;
end;

procedure TSummaryFooterDemoEditSummaryForm.UpdateSummaryList(
  AFistItemActive: Boolean);
var
  i: Integer;
begin
  lbSummaries.Clear;
  for i := 0 to FooterSummaryItems.Count - 1 do
    if Assigned(FooterSummaryItems[i].ItemLink) then
      lbSummaries.Items.Add(
        TcxGridDBColumn(FooterSummaryItems[i].ItemLink).Caption)
    else
      lbSummaries.Items.Add('Unassigned');
  if AFistItemActive then lbSummaries.ItemIndex := 0
  else lbSummaries.ItemIndex := lbSummaries.Items.Count - 1;
  btnDelete.Enabled := lbSummaries.Items.Count > 0;
end;

procedure TSummaryFooterDemoEditSummaryForm.btnDeleteClick(
  Sender: TObject);
var
  AItem: TcxGridDBTableSummaryItem;
  I: Integer;
  AColumn: TcxGridDBColumn;
begin
  AItem := TcxGridDBTableSummaryItem(FooterSummaryItems[lbSummaries.ItemIndex]);
  if AItem.FieldName <> '' then
    for I := 0 to GridView.VisibleColumnCount - 1 do
    begin
      AColumn := TcxGridDBColumn(GridView.VisibleColumns[I]);
      if AColumn.DataBinding.FieldName = AItem.FieldName then
      begin
        cbCalculatedColumn.ItemIndex := AColumn.VisibleIndex;
        break;
      end;
    end;
  if Assigned(AItem.Column) then
    cbFooterSummaryColumn.ItemIndex := AItem.Column.VisibleIndex;
  cbSummaryKind.EditValue := Integer(AItem.Kind);
  FooterSummaryItems.Items[lbSummaries.ItemIndex].Free;
  UpdateSummaryList(True);
end;

procedure TSummaryFooterDemoEditSummaryForm.FormShow(Sender: TObject);
var
  I: Integer;
begin
  cbCalculatedColumn.Properties.Items.Clear;
  cbFooterSummaryColumn.Properties.Items.Clear;
  for I := 0 to GridView.VisibleColumnCount - 1 do
  begin
    cbCalculatedColumn.Properties.Items.Add(GridView.VisibleColumns[I].Caption);
    cbFooterSummaryColumn.Properties.Items.Add(GridView.VisibleColumns[I].Caption);
  end;
  if GridView.VisibleColumnCount > 0 then
    cbFooterSummaryColumn.ItemIndex := 0
  else
    btnAdd.Enabled := false;
  UpdateSummaryList(True);
end;

procedure TSummaryFooterDemoEditSummaryForm.cbCalculatedColumnPropertiesChange(
  Sender: TObject);
var
  AColumn: TcxGridDBColumn;
begin
  if cbCalculatedColumn.ItemIndex <> -1 then
  begin
    AColumn :=
      TcxGridDBColumn(GridView.VisibleColumns[cbCalculatedColumn.ItemIndex]);
    UpdateSummaryKinds(AColumn.DataBinding.Field);
  end;
end;

procedure TSummaryFooterDemoEditSummaryForm.cbFooterSummaryColumnPropertiesChange(
  Sender: TObject);
begin
 cbCalculatedColumn.ItemIndex := cbFooterSummaryColumn.ItemIndex;
end;

procedure TSummaryFooterDemoEditSummaryForm.UpdateSummaryKinds(
  AField: TField = nil);
  procedure AddSummaryKind(AKind: TcxSummaryKind);
  var
    AItem: TcxImageComboboxItem;
    procedure SetItemProperties(ADescription: string; AImageIndex: Integer;
      AValue: Integer);
    begin
      AItem.Description := ADescription;
      AItem.ImageIndex := AImageIndex;
      AItem.Value := AValue;
    end;
  begin
    AItem := TcxImageComboboxItem(cbSummaryKind.Properties.Items.Add);
    case AKind of
      skNone:
        SetItemProperties('None', -1, 0);
      skSum:
        SetItemProperties('Sum', 6, 1);
      skMin:
        SetItemProperties('Min', 5, 2);
      skMax:
        SetItemProperties('Max', 4, 3);
      skCount:
        SetItemProperties('Count', 3, 4);
      skAverage:
        SetItemProperties('Average', 7, 5);
    end;
  end;
const
  NumberFieldTypes =
  [ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency, ftBCD, ftDate, ftTime,
      ftDateTime, ftAutoInc];
  TimeFieldTypes = [ftDate, ftTime, ftDateTime];
begin
    cbSummaryKind.Properties.Items.Clear;
    if Assigned(AField) then
    begin
      AddSummaryKind(skNone);
      AddSummaryKind(skCount);
      if AField.DataType in NumberFieldTypes then
      begin
        AddSummaryKind(skMax);
        AddSummaryKind(skMin);
      if not (AField.DataType in TimeFieldTypes) then
      begin
        AddSummaryKind(skSum);
        AddSummaryKind(skAverage);
      end;
      end;
    end;
    if cbSummaryKind.Properties.Items.Count = 0 then
      cbSummaryKind.Enabled := False
    else
    begin
      cbSummaryKind.Enabled := True;
      cbSummaryKind.EditValue := 0;
    end;
end;

procedure TSummaryFooterDemoEditSummaryForm.FormCreate(Sender: TObject);
begin
  Position := poScreenCenter;
end;

end.
