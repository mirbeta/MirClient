unit CarsDataForGrid;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, CarsData, cxEditRepositoryItems, cxDBExtLookupComboBox, cxEdit, cxDBEditRepository,
  cxClasses, DB, dxmdaset, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage, cxNavigator,
  cxDBData, cxHyperLinkEdit, cxGridCustomTableView, cxGridTableView, cxGridBandedTableView, cxGridDBBandedTableView,
  cxControls, cxGridCustomView, cxGrid;

type

  { TdmGridCars }

  TdmGridCars = class(TdmCars)
    EditRepositoryTrademarkLookup: TcxEditRepositoryExtLookupComboBoxItem;
    GridViewRepository: TcxGridViewRepository;
    GridViewRepositoryDBBandedTableView: TcxGridDBBandedTableView;
    GridViewRepositoryDBBandedTableViewDescription: TcxGridDBBandedColumn;
    GridViewRepositoryDBBandedTableViewID: TcxGridDBBandedColumn;
    GridViewRepositoryDBBandedTableViewLogo: TcxGridDBBandedColumn;
    GridViewRepositoryDBBandedTableViewName: TcxGridDBBandedColumn;
    GridViewRepositoryDBBandedTableViewRecId: TcxGridDBBandedColumn;
    GridViewRepositoryDBBandedTableViewSite: TcxGridDBBandedColumn;
  end;

var
  dmGridCars: TdmGridCars;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
