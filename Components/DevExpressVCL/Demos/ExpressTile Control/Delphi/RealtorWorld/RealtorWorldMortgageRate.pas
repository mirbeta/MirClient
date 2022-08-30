unit RealtorWorldMortgageRate;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxContainer, cxEdit, cxSplitter, cxGroupBox, RealtorWorldDM, cxStyles,
  cxCustomData, cxFilter, cxData, cxDataStorage, DB, cxDBData, cxCalendar,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGridLevel,
  cxClasses, cxGridCustomView, cxGrid, cxGridChartView, cxGridDBChartView,
  RealtorWorldBaseFrame;

type
  TfrmMortgageRate = class(TfrmBase)
    cxGroupBox1: TcxGroupBox;
    cxGroupBox2: TcxGroupBox;
    cxSplitter1: TcxSplitter;
    cxGrid1DBTableView1: TcxGridDBTableView;
    cxGrid1Level1: TcxGridLevel;
    cxGrid1: TcxGrid;
    cxGrid1DBTableView1Date1: TcxGridDBColumn;
    cxGrid1DBTableView1FRM30: TcxGridDBColumn;
    cxGrid1DBTableView1FRM15: TcxGridDBColumn;
    cxGrid1DBTableView1ARM1: TcxGridDBColumn;
    cxGrid2DBTableView1: TcxGridDBTableView;
    cxGrid2: TcxGrid;
    cxGrid2Level1: TcxGridLevel;
    cxGrid2DBChartView1: TcxGridDBChartView;
    cxGrid2DBChartView1Series1: TcxGridDBChartSeries;
    cxGrid2DBChartView1Series2: TcxGridDBChartSeries;
    cxGrid2DBChartView1Series3: TcxGridDBChartSeries;
    procedure cxSplitter1BeforeClose(Sender: TObject;
      var AllowClose: Boolean);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

constructor TfrmMortgageRate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  cxGrid1DBTableView1.DataController.Groups.FullExpand;
  if cxGrid1DBTableView1.DataController.RecordCount > 0 then
    cxGrid1DBTableView1.DataController.FocusedRowIndex := 0;
end;

procedure TfrmMortgageRate.cxSplitter1BeforeClose(Sender: TObject;
  var AllowClose: Boolean);
begin
  AllowClose := False;
end;

initialization
  RegisterFrame(IDMortgageRate, TfrmMortgageRate);

end.
