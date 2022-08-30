unit UnboundColumnsDemoMain;

interface

uses
  Windows, Forms, Messages, SysUtils, Classes, ActnList, ImgList, Controls, Menus,
  StdCtrls, cxButtons, cxCheckBox, cxContainer, cxEdit, cxTextEdit,
  cxMaskEdit, cxSpinEdit, ExtCtrls, cxGridLevel, cxGridCustomTableView,
  cxGridCardView, cxGridDBCardView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, ComCtrls, cxStyles, cxCustomData, cxGraphics,
  cxFilter, cxData, DB, cxDBData, cxDataStorage, cxLookAndFeelPainters,
  cxLookAndFeels, cxHyperLinkEdit, cxImageComboBox, cxDBLookupComboBox,
  cxMemo, cxCalendar, cxBlobEdit, cxGridTableView, cxGridBandedTableView,
  cxGridDBBandedTableView, BaseForm;

type
  TUnboundColumnsDemoMainForm = class(TfmBaseForm)
    BandedTableView: TcxGridDBBandedTableView;
    BandedTableViewComments: TcxGridDBBandedColumn;
    BandedTableViewCOMPANYNAME: TcxGridDBBandedColumn;
    BandedTableViewCOPIES: TcxGridDBBandedColumn;
    BandedTableViewFIRSTNAME: TcxGridDBBandedColumn;
    BandedTableViewLASTNAME: TcxGridDBBandedColumn;
    BandedTableViewLastSupportRequest: TcxGridDBBandedColumn;
    BandedTableViewPAYMENTAMOUNT: TcxGridDBBandedColumn;
    BandedTableViewPURCHASEDATE: TcxGridDBBandedColumn;
    BandedTableViewSelected: TcxGridDBBandedColumn;
    BandedTableViewSupportRequests: TcxGridDBBandedColumn;
    Grid: TcxGrid;
    Level: TcxGridLevel;
    cxStyleRepository1: TcxStyleRepository;
    styChecked: TcxStyle;
    procedure BandedTableViewStylesGetContentStyle(
      Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
      AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
  public
    constructor Create(AOwner: TComponent); override;
  private
    procedure GenerateUnboundData;
  end;

var
  UnboundColumnsDemoMainForm: TUnboundColumnsDemoMainForm;

implementation

{$R *.dfm}

uses
  UnboundColumnsDemoData, Dialogs, AboutDemoForm;

constructor TUnboundColumnsDemoMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GenerateUnboundData;
end;

procedure TUnboundColumnsDemoMainForm.GenerateUnboundData;

  procedure SetRecordValue(ARecordIndex, AItemIndex: Integer; AValue: Variant);
  begin
    BandedTableView.DataController.SetValue(ARecordIndex, AItemIndex, AValue);
  end;

var
  I: Integer;
begin
  Randomize;
  BandedTableView.BeginUpdate;
  try
    for I := 0 to BandedTableView.DataController.RecordCount - 1 do
    begin
      SetRecordValue(I, BandedTableViewSelected.Index, Random(100) mod 2 = 1);
      SetRecordValue(I, BandedTableViewSupportRequests.Index, 1 + Random(20));
      SetRecordValue(I, BandedTableViewLastSupportRequest.Index,
        EncodeDate(2000 + Random(4), 1 + Random(11), 1 + Random(27)));
      SetRecordValue(I, BandedTableViewComments.Index, 'Put your comments here...');
    end;
  finally
    BandedTableView.EndUpdate;
  end;
end;

procedure TUnboundColumnsDemoMainForm.BandedTableViewStylesGetContentStyle(
  Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
begin
  if (ARecord is TcxGridDataRow) and not ARecord.Selected and
    (ARecord.Values[BandedTableViewSelected.Index] = True) then
    AStyle := styChecked;
end;

end.
