unit FilterControlDemoMain;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cxGridLevel, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxControls, cxGridCustomView, cxGrid, cxCustomData,
  ExtCtrls, ActnList, ImgList, Menus, ComCtrls, cxGridCustomPopupMenu,
  cxGridPopupMenu, cxStyles, cxGraphics, cxFilter, cxData, cxEdit, DB,
  cxDBData, cxClasses, cxFilterControl, cxLookAndFeelPainters, cxButtons,
  Grids, DBGrids, cxLookAndFeels, cxDBEditRepository, cxDBFilterControl,
  cxDataStorage, cxDBLookupComboBox, cxCalendar, cxTimeEdit, cxCalc,
  cxImageComboBox, cxSpinEdit, BaseForm, cxPC, cxGridCardView, CarsDataForGrid, cxNavigator;

type
  TcxLocate = (ltNone, ltLeft, ltTop);

  TFilterControlDemoMainForm = class(TfmBaseForm)
    miOptions: TMenuItem;
    cxGridPopupMenu1: TcxGridPopupMenu;
    miFilterControl: TMenuItem;
    miFilterControlPosition: TMenuItem;
    miFilterControlModal: TMenuItem;
    miFilterControlNone: TMenuItem;
    N2: TMenuItem;
    miFilterControlLeft: TMenuItem;
    miFilterControlTop: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    PageControl: TcxPageControl;
    tsGrid4DisplayStyle: TcxTabSheet;
    tsStandardDisplayStyle: TcxTabSheet;
    Grid: TcxGrid;
    tvOrders: TcxGridDBTableView;
    tvOrdersPurchaseDate: TcxGridDBColumn;
    tvOrdersTime: TcxGridDBColumn;
    tvOrdersPaymentAmount: TcxGridDBColumn;
    tvOrdersPaymentType: TcxGridDBColumn;
    tvOrdersQuantity: TcxGridDBColumn;
    lvOrders: TcxGridLevel;
    DBGrid: TDBGrid;
    pnlFilterDialog: TPanel;
    cxFilterControl: TcxFilterControl;
    pnlButtons: TPanel;
    btnLoad: TcxButton;
    btnSaveAs: TcxButton;
    btnApply: TcxButton;
    cxDBFilterControl: TcxDBFilterControl;
    Splitter: TSplitter;
    PopupMenu: TPopupMenu;
    miShowFilterPanel: TMenuItem;
    miShowFilterBtn: TMenuItem;
    miFilterPnlNever: TMenuItem;
    miFilterPnlNeverNonEmpty: TMenuItem;
    miFilterPnlAlways: TMenuItem;
    btnOK: TcxButton;
    btnCancel: TcxButton;
    cxEditRepository: TcxEditRepository;
    CustomersEditorItem: TcxEditRepositoryLookupComboBoxItem;
    CarsEditorItem: TcxEditRepositoryLookupComboBoxItem;
    tvOrdersCustomerID: TcxGridDBColumn;
    tvOrdersProductID: TcxGridDBColumn;
    Label1: TLabel;
    procedure miFilterControlPosClick(Sender: TObject);
    procedure miFilterControlModalClick(Sender: TObject);
    procedure tvOrdersFilterControlDialogShow(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveAsClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure cxDBFilterControlApplyFilter(Sender: TObject);
    procedure miShowFilterPnlClick(Sender: TObject);
    procedure miShowFilterBtnClick(Sender: TObject);
    procedure cxFilterControlApplyFilter(Sender: TObject);
    procedure tvOrdersDataControllerFilterChanged(Sender: TObject);
  private
    FFilterControl: TcxCustomFilterControl;
    FFilterText: string;
    FFilterControlAlign: TcxLocate;
    FCloseEvent: TCloseEvent;
    procedure Relocate(ALocate: TcxLocate);
  protected
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); override;
  public
    procedure FilterDialogClose(Sender: TObject; var Action: TCloseAction);
  end;

var
  FilterControlDemoMainForm: TFilterControlDemoMainForm;

implementation

{$R *.dfm}

uses
  FilterControlDemoData,
  FilterControlDemoFilterDialog, cxFilterControlDialog, AboutDemoForm;

procedure TFilterControlDemoMainForm.miFilterControlPosClick(Sender: TObject);
begin
  MenuItemSetChecked(Sender, True);
  FFilterControlAlign := TcxLocate(TComponent(Sender).Tag);
  Relocate(FFilterControlAlign);
end;

procedure TFilterControlDemoMainForm.miFilterControlModalClick(Sender: TObject);

  procedure ShowButtons(AShow: Boolean);
  begin
    btnOK.Visible := AShow;
    btnCancel.Visible := AShow;
  end;

var
  AHeight, AWidth: Integer;
  AAlign: TAlign;
begin
  if PageControl.ActivePage = tsGrid4DisplayStyle then
    tvOrders.Filtering.RunCustomizeDialog
  else
  begin
    miFilterControlPosClick(miFilterControlNone);
    AHeight := pnlFilterDialog.Height;
    AWidth := pnlFilterDialog.Width;
    AAlign := pnlFilterDialog.Align;
    ShowButtons(True);
    pnlFilterDialog.Parent := FilterControlDemoFilterDialogForm;
    pnlFilterDialog.Align := alClient;
    pnlFilterDialog.Visible := True;
    FilterControlDemoFilterDialogForm.ShowModal;
    pnlFilterDialog.Align := AAlign;
    ShowButtons(False);
    pnlFilterDialog.Visible := False;
    pnlFilterDialog.Parent := Self;
    pnlFilterDialog.Height := AHeight;
    pnlFilterDialog.Width := AWidth;
  end;
end;

procedure TFilterControlDemoMainForm.tvOrdersFilterControlDialogShow(Sender: TObject);
begin
  FCloseEvent := TfmFilterControlDialog(Sender).OnClose;
  TfmFilterControlDialog(Sender).OnClose := FilterDialogClose;
  Relocate(ltNone);
end;

procedure TFilterControlDemoMainForm.tvOrdersDataControllerFilterChanged(Sender: TObject);
begin
  cxFilterControl.UpdateFilter;
end;

procedure TFilterControlDemoMainForm.btnLoadClick(Sender: TObject);
begin
 if PageControl.ActivePage = tsGrid4DisplayStyle then
   OpenDialog.Filter := 'Grid Filters(*.flt)|*.flt|All files|*.*'
 else
   OpenDialog.Filter := 'Standard Filters(*.sft)|*.sft|All files|*.*';

 if OpenDialog.Execute then
   FFilterControl.LoadFromFile(OpenDialog.FileName);
end;

procedure TFilterControlDemoMainForm.btnSaveAsClick(Sender: TObject);
begin
  if PageControl.ActivePage = tsGrid4DisplayStyle then
    SaveDialog.Filter := 'Grid Filters(*.flt)|*.flt'
  else
    SaveDialog.Filter := 'Standard Filters(*.sft)|*.sft';

  if SaveDialog.Execute then
    FFilterControl.SaveToFile(SaveDialog.FileName);
end;

procedure TFilterControlDemoMainForm.btnApplyClick(Sender: TObject);
begin
  FFilterControl.ApplyFilter;
end;

procedure TFilterControlDemoMainForm.FormCreate(Sender: TObject);
begin
  OpenDialog.InitialDir := ExtractFileDir(Application.ExeName);
  SaveDialog.InitialDir := OpenDialog.InitialDir;

  FFilterText := '';
  cxFilterControl.Align := alClient;
  cxDBFilterControl.Align := alClient;
  pnlFilterDialog.Width := 240;
  FFilterControl := cxFilterControl;
  FFilterControlAlign := ltLeft;
  PageControl.ActivePage := tsGrid4DisplayStyle;
  PageControlChange(PageControl);

  cxFilterControl.LoadFromFile('ExtendedFilter.flt');
  cxFilterControl.ApplyFilter;
  cxDBFilterControl.LoadFromFile('StandardFilter.sft');
  cxDBFilterControl.ApplyFilter;
end;

procedure TFilterControlDemoMainForm.PageControlChange(Sender: TObject);
begin
  FFilterControl.Visible := False;
  if TcxPageControl(Sender).ActivePage = tsStandardDisplayStyle then
    FFilterControl := cxDBFilterControl
  else
    FFilterControl := cxFilterControl;
  FFilterControl.Visible := True;
end;

procedure TFilterControlDemoMainForm.cxDBFilterControlApplyFilter(Sender: TObject);
var
  ADataSet: TDataSet;
begin
  ADataSet := TcxDBFilterControl(Sender).DataSet;
  try
    ADataSet.DisableControls;
    ADataSet.Filtered := False;
    ADataSet.Filter := TcxDBFilterControl(Sender).FilterText;
    ADataSet.Filtered := True;
  finally
    ADataSet.EnableControls;
  end;
end;

procedure TFilterControlDemoMainForm.miShowFilterPnlClick(Sender: TObject);
begin
  tvOrders.Filtering.Visible := TcxGridFilterVisible(TComponent(Sender).Tag);
end;

procedure TFilterControlDemoMainForm.miShowFilterBtnClick(Sender: TObject);
begin
  tvOrders.Filtering.CustomizeDialog := GetMenuItemChecked(Sender);
end;

procedure TFilterControlDemoMainForm.Relocate(ALocate: TcxLocate);
var
  AAlign: TAlign;
begin
  Splitter.Visible := False;
  pnlFilterDialog.Visible := False;
  if ALocate = ltNone then Exit;
  if ALocate = ltTop then
    AAlign := alTop
  else
    AAlign := alLeft;

  if ALocate = ltLeft then
    Splitter.Align := AAlign;
  pnlFilterDialog.Align := AAlign;
  if ALocate = ltTop then
    Splitter.Align := AAlign;
  Splitter.Visible := True;
  pnlFilterDialog.Visible := True;
  lbDescription.Top := 0;
  PlaceControls;
end;

procedure TFilterControlDemoMainForm.cxFilterControlApplyFilter(Sender: TObject);
var
  AView: TcxGridDBTableView;
begin
  AView := TcxFilterControl(Sender).LinkComponent as TcxGridDBTableView;
  AView.DataController.Filter.Active := True;
end;

procedure TFilterControlDemoMainForm.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  UpdateTableViewStyleSheet(tvOrders);
end;

procedure TFilterControlDemoMainForm.FilterDialogClose(
  Sender: TObject; var Action: TCloseAction);
begin
  Relocate(FFilterControlAlign);
  if Assigned(FCloseEvent) then
    FCloseEvent(Sender, Action);
  TForm(Sender).OnClose := FCloseEvent;
end;

end.
