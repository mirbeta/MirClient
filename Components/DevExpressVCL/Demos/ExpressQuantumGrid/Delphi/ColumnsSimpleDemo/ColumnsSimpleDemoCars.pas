unit ColumnsSimpleDemoCars;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, cxGridCustomTableView, cxGridTableView, cxGridBandedTableView,
  cxGridDBBandedTableView, cxControls, cxGridCustomView,
  cxGridLevel, cxGrid, Grids, DBGrids, ExtCtrls, cxStyles, cxCustomData,
  cxGraphics, cxFilter, cxData, cxEdit, cxClasses, cxDataStorage, DB,
  cxDBData, cxImage, cxBlobEdit, cxRadioGroup, cxImageComboBox, cxMemo,
  cxHyperLinkEdit, cxLookAndFeels, cxLookAndFeelPainters, cxNavigator, CarsDataForGrid;

type
  TColumnSimpleDemoCarsForm = class(TForm)
    pnlCarInfo: TPanel;
    GridCars: TcxGrid;
    bvCars: TcxGridDBBandedTableView;
    bvCarsID: TcxGridDBBandedColumn;
    bvCarsTrademark: TcxGridDBBandedColumn;
    bvCarsModel: TcxGridDBBandedColumn;
    bvCarsPrice: TcxGridDBBandedColumn;
    bvCarsPicture: TcxGridDBBandedColumn;
    bvCarsLargePicture: TcxGridDBBandedColumn;
    bvCarsCategory: TcxGridDBBandedColumn;
    bvCarsHP: TcxGridDBBandedColumn;
    bvCarsCyl: TcxGridDBBandedColumn;
    bvCarsTransmissSpeedCount: TcxGridDBBandedColumn;
    bvCarsTransmissAutomatic: TcxGridDBBandedColumn;
    bvCarsMPG_City: TcxGridDBBandedColumn;
    bvCarsMPG_Highway: TcxGridDBBandedColumn;
    bvCarsDescription: TcxGridDBBandedColumn;
    bvCarsHyperlink: TcxGridDBBandedColumn;
    lvCars: TcxGridLevel;
    procedure bvCarsTopRecordIndexChanged(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ColumnSimpleDemoCarsForm: TColumnSimpleDemoCarsForm;

implementation

uses ColumnsSimpleDemoData;

{$R *.dfm}

procedure TColumnSimpleDemoCarsForm.bvCarsTopRecordIndexChanged(
  Sender: TObject);
begin
  bvCars.Controller.FocusedRecordIndex := bvCars.Controller.TopRecordIndex;
end;

end.
