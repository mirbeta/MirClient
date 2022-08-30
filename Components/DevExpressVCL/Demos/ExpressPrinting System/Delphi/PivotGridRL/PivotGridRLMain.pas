unit PivotGridRLMain;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DemoBasicMain, dxPSGlbl, dxPSUtl, dxPSEngn, dxPrnPg, dxBkgnd,
  dxWrap, dxPrnDev, dxPSCompsProvider, dxPSFillPatterns, dxPSEdgePatterns,
  dxPSPDFExportCore, dxPSPDFExport, cxDrawTextUtils, dxPSPrVwStd,
  dxPScxEditorProducers, dxPScxExtEditorProducers,
  dxPScxPageControlProducer, dxPSCore, ActnList, ImgList, Menus, ComCtrls,
  ToolWin, StdCtrls, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxClasses, cxCustomData, cxStyles, cxEdit,
  cxCustomPivotGrid, cxPivotGrid, dxPScxCommon, dxPScxPivotGridLnk, dxCore;

type
  TPivotGridRLMainForm = class(TDemoBasicMainForm)
    PaymentTypeImages: TImageList;
    PivotGrid: TcxPivotGrid;
    pgfPurchaseQuarter: TcxPivotGridField;
    pgfPurchaseMonth: TcxPivotGridField;
    pgfPaymentType: TcxPivotGridField;
    pgfQuantity: TcxPivotGridField;
    pgfCarName: TcxPivotGridField;
    pgfUnitPrice: TcxPivotGridField;
    pgfCompanyName: TcxPivotGridField;
    pgfPaymentAmount: TcxPivotGridField;
    dxComponentPrinterLink1: TcxPivotGridReportLink;
    procedure FormCreate(Sender: TObject);
    procedure pgfPaymentTypeGetGroupImageIndex(Sender: TcxPivotGridField;
      const AItem: TcxPivotGridViewDataItem; var AImageIndex: Integer;
      var AImageAlignHorz: TAlignment;
      var AImageAlignVert: TcxAlignmentVert);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PivotGridRLMainForm: TPivotGridRLMainForm;

implementation

{$R *.dfm}

function EnumResModules(Instance: TdxNativeUInt; Data: Pointer): Boolean;
begin
  Result := True;
  if FindResource(Instance, 'PIVOTPREVIEWDATA','PIVOTDATA') <> 0 then
  begin
    PInteger(Data)^ := Instance;
    Result := False;
  end;
end;

procedure TPivotGridRLMainForm.FormCreate(Sender: TObject);
var
  AStream: TStream;
  AInstance: TdxNativeUInt;
begin
  inherited;
  AInstance := 0;
  EnumResourceModules(EnumResModules, @AInstance);
  AStream := TResourceStream.Create(AInstance, 'PIVOTPREVIEWDATA', 'PIVOTDATA');
  try
    AStream.Position := 0;
    PivotGrid.DataController.LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TPivotGridRLMainForm.pgfPaymentTypeGetGroupImageIndex(
  Sender: TcxPivotGridField; const AItem: TcxPivotGridViewDataItem;
  var AImageIndex: Integer; var AImageAlignHorz: TAlignment;
  var AImageAlignVert: TcxAlignmentVert);
var
  ACard: string;
begin
  inherited;
  ACard := VarToStr(AItem.Value);
  if SameText(ACard, 'AmEx') then
    AImageIndex := 0
  else
    if SameText(ACard, 'Cash') then
      AImageIndex := 1
    else
      if SameText(ACard, 'Master') then
        AImageIndex := 2
      else
        if SameText(ACard, 'Visa') then
          AImageIndex := 3;
end;

end.
