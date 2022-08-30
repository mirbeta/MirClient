unit RowsMultiEditorsDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DemoBasicMain, cxLookAndFeels, ActnList, ImgList, Menus,
  StdCtrls, ComCtrls, cxStyles, cxGraphics, cxEdit, cxVGrid, cxControls,
  cxInplaceContainer, cxEditRepositoryItems, cxClasses, cxDBVGrid;
type
  TRowsMultiEditorsDemoMainForm = class(TDemoBasicMainForm)
    EditRepository: TcxEditRepository;
    cxStyleRepository1: TcxStyleRepository;
    cxVerticalGridStyleSheetDevExpress: TcxVerticalGridStyleSheet;
    cxStyle1: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    cxStyle4: TcxStyle;
    cxStyle5: TcxStyle;
    cxStyle6: TcxStyle;
    cxStyle7: TcxStyle;
    miSeparator1: TMenuItem;
    Layout1: TMenuItem;
    MultiRecord1: TMenuItem;
    SingleRecord1: TMenuItem;
    BandsView1: TMenuItem;
    cxDBVerticalGrid: TcxDBVerticalGrid;
    merPurchaseDateTime: TcxDBMultiEditorRow;
    merPaymentDescr: TcxDBMultiEditorRow;
    merCustomer: TcxDBMultiEditorRow;
    merCustomerInfo: TcxDBMultiEditorRow;
    erAddress: TcxDBEditorRow;
    merWork: TcxDBMultiEditorRow;
    merMailData: TcxDBMultiEditorRow;
    merPhones: TcxDBMultiEditorRow;
    erEmail: TcxDBEditorRow;
    merCarName: TcxDBMultiEditorRow;
    merCarEngine: TcxDBMultiEditorRow;
    merTransmiss: TcxDBMultiEditorRow;
    erCarDescr: TcxDBEditorRow;
    erCarImage: TcxDBEditorRow;
    erHyperlink: TcxDBEditorRow;
    erPrice: TcxDBEditorRow;
    erepCarPictEditing: TcxEditRepositoryBlobItem;
    erepCarPictEdit: TcxEditRepositoryImageItem;
    erepPaymentAmountEditing: TcxEditRepositoryCalcItem;
    ctgOrder: TcxCategoryRow;
    ctgCustomer: TcxCategoryRow;
    ctgCar: TcxCategoryRow;
    ImageList: TImageList;
    procedure erCarImagePropertiesGetEditingProperties(
      Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer;
      var AProperties: TcxCustomEditProperties);
    procedure erCarImagePropertiesGetEditProperties(
      Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer;
      var AProperties: TcxCustomEditProperties);
    procedure merPaymentDescrEditors2GetEditingProperties(
      Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer;
      var AProperties: TcxCustomEditProperties);
    procedure miLayoutStyleClick(Sender: TObject);
  end;

var
  RowsMultiEditorsDemoMainForm: TRowsMultiEditorsDemoMainForm;

implementation

uses RowsMultiEditorsDemoData;

{$R *.dfm}

procedure TRowsMultiEditorsDemoMainForm.erCarImagePropertiesGetEditingProperties(
  Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer;
  var AProperties: TcxCustomEditProperties);
begin
  AProperties := erepCarPictEditing.Properties;
end;

procedure TRowsMultiEditorsDemoMainForm.erCarImagePropertiesGetEditProperties(
  Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer;
  var AProperties: TcxCustomEditProperties);
begin
  AProperties := erepCarPictEdit.Properties;
end;

procedure TRowsMultiEditorsDemoMainForm.merPaymentDescrEditors2GetEditingProperties(
  Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer;
  var AProperties: TcxCustomEditProperties);
begin
  AProperties := erepPaymentAmountEditing.Properties;
end;

procedure TRowsMultiEditorsDemoMainForm.miLayoutStyleClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
  cxDBVerticalGrid.LayoutStyle := TcxvgLayoutStyle(TMenuItem(Sender).Tag);
end;

end.
