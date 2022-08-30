unit EditorsInPlaceDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Variants, Forms, Dialogs,
  SysUtils, Menus, StdCtrls, Controls, cxLookAndFeels, DemoBasicMain,
  cxControls, cxCustomPivotGrid, cxDBPivotGrid, DemoBasicDM, cxStyles,
  cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage, cxEdit, DB, cxDBData,
  Classes, cxClasses, cxProgressBar;

type
  TfrmEditorsInPlace = class(TfrmDemoBasicMain)
    DBPivotGrid: TcxDBPivotGrid;
    pgfPurchaseDate: TcxDBPivotGridField;
    pgfPaymentType: TcxDBPivotGridField;
    pgfQuantity: TcxDBPivotGridField;
    pgfCarName: TcxDBPivotGridField;
    pgfUnitPrice: TcxDBPivotGridField;
    pgfCompanyName: TcxDBPivotGridField;
    pgfPaymentAmount: TcxDBPivotGridField;
  protected
    function GetPivotGrid: TcxCustomPivotGrid; override;
  end;

var
  frmEditorsInPlace: TfrmEditorsInPlace;

implementation

{$R *.dfm}

{ TfrmEditorsInPlace }

function TfrmEditorsInPlace.GetPivotGrid: TcxCustomPivotGrid;
begin
  Result := DBPivotGrid;
end;

end.
