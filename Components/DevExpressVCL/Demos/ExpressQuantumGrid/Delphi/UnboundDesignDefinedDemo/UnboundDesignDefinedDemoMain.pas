unit UnboundDesignDefinedDemoMain;

interface

uses
  Windows, Forms, Messages, SysUtils, Classes, ActnList, ImgList, Controls, Menus,
  StdCtrls, cxButtons, cxCheckBox, cxContainer, cxEdit, cxTextEdit,
  cxMaskEdit, cxSpinEdit, ExtCtrls, cxGridLevel, cxGridCustomTableView,
  cxGridCardView, cxGridDBCardView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, ComCtrls, cxStyles, cxCustomData, cxGraphics,
  cxFilter, cxData, DB, cxDBData, cxDataStorage, cxLookAndFeelPainters,
  cxLookAndFeels, cxHyperLinkEdit, cxImageComboBox, cxDBLookupComboBox,
  cxMemo, cxGridTableView, cxGridBandedTableView, cxBlobEdit, cxImage,
  cxCurrencyEdit, BaseForm, cxNavigator;

type
  TUnboundDesignDefinedDemoMainForm = class(TfmBaseForm)
    lvCars: TcxGridLevel;
    cxGrid: TcxGrid;
    bvCars: TcxGridBandedTableView;
    clnCar: TcxGridBandedColumn;
    clnPrice: TcxGridBandedColumn;
    clnPicture: TcxGridBandedColumn;
    clnHP: TcxGridBandedColumn;
    clnTorque: TcxGridBandedColumn;
    clnCyl: TcxGridBandedColumn;
    clnTransmissSpeedCount: TcxGridBandedColumn;
    clnTransmissAutomatic: TcxGridBandedColumn;
    clnHyperlink: TcxGridBandedColumn;
    cxStyleRepository1: TcxStyleRepository;
    styCar: TcxStyle;
    clnTrademark: TcxGridBandedColumn;
    styGroup: TcxStyle;
  end;

var
  UnboundDesignDefinedDemoMainForm: TUnboundDesignDefinedDemoMainForm;

implementation

{$R *.dfm}

end.
