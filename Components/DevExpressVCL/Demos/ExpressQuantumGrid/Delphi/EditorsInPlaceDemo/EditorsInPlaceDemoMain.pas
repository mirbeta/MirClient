unit EditorsInPlaceDemoMain;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cxGridLevel, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxControls, cxGridCustomView, cxGrid, cxCustomData,
  ExtCtrls, ActnList, ImgList, Menus, ComCtrls, cxGridCustomPopupMenu,
  cxGridPopupMenu, Grids, DBGrids, cxDBData, cxStyles, cxGraphics, cxFilter,
  cxData, cxEdit, DB, cxClasses, cxGridCardView, cxGridDBCardView, cxDBEdit, cxDropDownEdit,
  cxDataStorage, cxMRUEdit, cxButtonEdit, cxCheckBox, cxMaskEdit, cxMemo,
  cxDBLookupComboBox, cxTimeEdit, cxImageComboBox, cxSpinEdit, cxCalc,
  cxImage, cxHyperLinkEdit, cxLookAndFeels, cxLookAndFeelPainters,
  cxGridCustomLayoutView, BaseForm, CarsDataForGrid, cxNavigator, dxBarCode;

type
  TEditorsInPlaceDemoMainForm = class(TfmBaseForm)
    miOptions: TMenuItem;
    Grid: TcxGrid;
    lvCustomers: TcxGridLevel;
    miShowEditButtons: TMenuItem;
    miEditButtonsNever: TMenuItem;
    miEditButtonsForFocusedRecord: TMenuItem;
    miEditButtonsAlways: TMenuItem;
    lvOrders: TcxGridLevel;
    lvCars: TcxGridLevel;
    cvCustomers: TcxGridDBCardView;
    cvOrders: TcxGridDBCardView;
    cvCars: TcxGridDBCardView;
    cvCustomersFirstName: TcxGridDBCardViewRow;
    cvCustomersLastName: TcxGridDBCardViewRow;
    cvCustomersCompany: TcxGridDBCardViewRow;
    cvCustomersPrefix: TcxGridDBCardViewRow;
    cvCustomersAddress: TcxGridDBCardViewRow;
    cvCustomersCity: TcxGridDBCardViewRow;
    cvCustomersState: TcxGridDBCardViewRow;
    cvCustomersZipCode: TcxGridDBCardViewRow;
    cvCustomersCustomer: TcxGridDBCardViewRow;
    cvCustomersFaxPhone: TcxGridDBCardViewRow;
    cvCustomersDescription: TcxGridDBCardViewRow;
    cvOrdersCustomerID: TcxGridDBCardViewRow;
    cvOrdersProductID: TcxGridDBCardViewRow;
    cvOrdersPurchaseDate: TcxGridDBCardViewRow;
    cvOrdersTime: TcxGridDBCardViewRow;
    cvOrdersPaymentType: TcxGridDBCardViewRow;
    cvOrdersDescription: TcxGridDBCardViewRow;
    cvOrdersQuantity: TcxGridDBCardViewRow;
    cvOrdersPaymentAmount: TcxGridDBCardViewRow;
    cvCarsPicture: TcxGridDBCardViewRow;
    cvCarsCar: TcxGridDBCardViewRow;
    cvCarsCategory: TcxGridDBCardViewRow;
    cvCarsHyperlink: TcxGridDBCardViewRow;
    cvCarsInfo: TcxGridDBCardViewRow;
    cvBarCode: TcxGridDBCardViewRow;
    procedure FormShow(Sender: TObject);
    procedure cvCustomersStatePropertiesButtonClick(Sender: TObject);
    procedure cvCustomersCityPropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure GridFocusedViewChanged(Sender: TcxCustomGrid;
      APrevFocusedView, AFocusedView: TcxCustomGridView);
    procedure miEditButtonsClick(Sender: TObject);
    procedure cvCarsCustomDrawCell(Sender: TcxCustomGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo;
      var ADone: Boolean);
  private
    procedure UpdateOptions;
  protected
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); override;
  end;

var
  EditorsInPlaceDemoMainForm: TEditorsInPlaceDemoMainForm;

implementation

{$R *.dfm}

uses
  EditorsInPlaceDemoData, ShellAPI,
  EditorsInPlaceDemoCities, EditorsInPlaceDemoCars, AboutDemoForm;

procedure TEditorsInPlaceDemoMainForm.FormShow(Sender: TObject);
begin
  TcxPopupEditProperties(cvCarsInfo.Properties).ImmediatePopup := True;
end;

procedure TEditorsInPlaceDemoMainForm.cvCustomersStatePropertiesButtonClick(
  Sender: TObject);
begin
  MessageDlg('Button click event handler', mtInformation, [mbOK], 0);
end;

procedure TEditorsInPlaceDemoMainForm.cvCustomersCityPropertiesButtonClick(
  Sender: TObject; AButtonIndex: Integer);
begin
  case AButtonIndex of
    0:
      if EditorsInPlaceDemoCitiesForm.ShowModal = mrOk then
      begin
        EditorsInPlaceDemoDataDM.tblCustomers.Edit;
        EditorsInPlaceDemoDataDM.tblCustomersCity.Value := EditorsInPlaceDemoCitiesForm.Value;
      end;
    1: ShellExecute(Handle, 'OPEN', 'http://www.usacitiesonline.com/', nil, nil, SW_SHOWMAXIMIZED);
  end;
end;

procedure TEditorsInPlaceDemoMainForm.GridFocusedViewChanged(
  Sender: TcxCustomGrid; APrevFocusedView, AFocusedView: TcxCustomGridView);
begin
  UpdateOptions;
end;

procedure TEditorsInPlaceDemoMainForm.UpdateOptions;
var
  AView: TcxGridDBCardView;
begin
  AView := TcxGridDBCardView(Grid.FocusedView);
  case AView.OptionsView.ShowEditButtons of
    gsebNever:
      MenuItemSetChecked('miEditButtonsNever', True);
    gsebForFocusedRecord:
      MenuItemSetChecked('miEditButtonsForFocusedRecord', True);
    gsebAlways:
      MenuItemSetChecked('miEditButtonsAlways', True);
  end;
end;

procedure TEditorsInPlaceDemoMainForm.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  UpdateCardViewStyleSheet(cvCustomers);
  UpdateCardViewStyleSheet(cvOrders);
  UpdateCardViewStyleSheet(cvCars);
end;

procedure TEditorsInPlaceDemoMainForm.miEditButtonsClick(Sender: TObject);
var
  AView: TcxGridDBCardView;
begin
  AView := TcxGridDBCardView(Grid.FocusedView);
  case (Sender as TComponent).Tag of
    0: AView.OptionsView.ShowEditButtons := gsebNever;
    1: AView.OptionsView.ShowEditButtons := gsebForFocusedRecord;
    2: AView.OptionsView.ShowEditButtons := gsebAlways;
  end;
  MenuItemSetChecked(Sender, True);
end;

procedure TEditorsInPlaceDemoMainForm.cvCarsCustomDrawCell(
  Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
begin
  if (AViewInfo.Item.Index = cvCarsInfo.Index) and (AViewInfo is TcxGridCardRowDataViewInfo) then
  begin
    ACanvas.Canvas.Brush.Style := bsSolid;
    ACanvas.Canvas.Brush.Color := $00F7EAD9;
    ACanvas.Canvas.FillRect(AViewInfo.Bounds);
    SetBkMode(ACanvas.Handle, TRANSPARENT);
    ACanvas.Font.Color := clBlack;
    ACanvas.DrawText('<click here and view dropdown window>', AViewInfo.Bounds, 0);
    ADone := True;
  end;
end;

end.
