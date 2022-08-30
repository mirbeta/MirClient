unit PrefilterByCodeDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Variants, Forms, Dialogs,
  SysUtils, Menus, StdCtrls, Controls, cxLookAndFeels, DemoBasicMain,
  cxControls, cxCustomPivotGrid, cxDBPivotGrid, DemoBasicDM, cxStyles,
  cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage, cxEdit, DB, cxDBData,
  Classes, cxClasses, ExtCtrls, cxLookAndFeelPainters;

type
  TDateType = (dtFirst, dtLast);
  TUserFiltering = (ufNone, ufCustom, ufSimple, ufList, ufTwoField, ufBetween,
    ufUserFilter, ufGroup);

  TfmPrefilterByCode = class(TfrmDemoBasicMain)
    DBPivotGrid: TcxDBPivotGrid;
    pgfPurchaseDate: TcxDBPivotGridField;
    pgfPaymentType: TcxDBPivotGridField;
    pgfQuantity: TcxDBPivotGridField;
    pgfCarName: TcxDBPivotGridField;
    pgfUnitPrice: TcxDBPivotGridField;
    pgfCompanyName: TcxDBPivotGridField;
    pgfPaymentAmount: TcxDBPivotGridField;
    Panel1: TPanel;
    Label1: TLabel;
    cbFilters: TComboBox;
    PrefilterPosition1: TMenuItem;
    op1: TMenuItem;
    Bottom1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure pgfPaymentTypeGetGroupImageIndex(Sender: TcxPivotGridField;
      const AItem: TcxPivotGridViewDataItem; var AImageIndex: Integer;
      var AImageAlignHorz: TAlignment;
      var AImageAlignVert: TcxAlignmentVert);
    procedure cbFiltersChange(Sender: TObject);
    procedure DBPivotGridFilterChanged(Sender: TObject);
    procedure Bottom1Click(Sender: TObject);
  private
    FLock: Boolean;
    function GetDate(ADateType: TDateType): TDate;
    function GetFilterIndex(const AFiltering: TUserFiltering): integer;
    function GetPrefilter: TcxDBDataFilterCriteria;
    procedure PopulateFilterList;
    procedure SetFilter(const AFiltering: TUserFiltering);
    procedure SetOnlyMercedesFilter(AFilterCriteriaList: TcxFilterCriteriaItemList);
  protected
    function GetPivotGrid: TcxCustomPivotGrid; override;

    property Prefilter: TcxDBDataFilterCriteria read GetPrefilter;
  public
    { Public declarations }
  end;

var
  fmPrefilterByCode: TfmPrefilterByCode;

implementation

{$R *.dfm}

const
  DoeEnterpriseID = 12;
  QuantityMinID = 3;
  HillCorporationID = 13;
  DevelopmentHouseID = 22;

function TfmPrefilterByCode.GetPivotGrid: TcxCustomPivotGrid;
begin
  Result := DBPivotGrid;
end;

function TfmPrefilterByCode.GetDate(ADateType: TDateType): TDate;
var
  ADate: TDate;
begin
  ADate := Date;
  case ADateType of
    dtFirst:
      ADate := EncodeDate(2002, 1, 1);
    dtLast:
      ADate := EncodeDate(2002, 1, 31);
  end;
  Result := ADate;
end;

function TfmPrefilterByCode.GetFilterIndex(
  const AFiltering: TUserFiltering): integer;
var
  I: Integer;
begin
  with cbFilters do
    for I := 0 to Items.Count - 1 do
    begin
       Result := I;
       if TUserFiltering(Items.Objects[I]) = AFiltering then
         Exit;
    end;
  Result := -1;
end;

function TfmPrefilterByCode.GetPrefilter: TcxDBDataFilterCriteria;
begin
  Result := DBPivotGrid.DataController.Filter;
end;

procedure TfmPrefilterByCode.PopulateFilterList;
const
  AFilterDesc: array[TUserFiltering] of string = (
    'No prefilter',
    'Custom prefilter',
    'QUANTITY > 3',
    'PAYMENTTYPE is Visa or AmericanExpress',
    'All cars purchased by Doe Enterprises using Visa',
    'All purchases in December 2002',
    'Only Mercedes',
    'Only Mercedes purchased by Hill Corporation or Development House'
    ); 
var
 AFilter: TUserFiltering;
begin
  with cbFilters do
  begin
    Clear;
    for AFilter := Low(TUserFiltering) to High(TUserFiltering) do
      Items.AddObject(AFilterDesc[AFilter], Pointer(AFilter));
    ItemIndex := GetFilterIndex(ufSimple);
    SetFilter(ufSimple);
  end;
end;

procedure TfmPrefilterByCode.SetFilter(const AFiltering: TUserFiltering);
var
  ADate: TDate;
  V: Variant;
begin
  if AFiltering = ufCustom then
    DBPivotGrid.ShowPrefilterDialog
  else
  begin
    Prefilter.BeginUpdate;
    FLock := True;
    try
      with Prefilter.Root do
      begin
        Clear;
        case AFiltering of
          ufSimple:
            AddItem(pgfQuantity, foGreater, QuantityMinID, '3');
          ufList:
            begin
              BoolOperatorKind := fboOr;
              AddItem(pgfPaymentType, foEqual, 'Visa', 'Visa');
              AddItem( pgfPaymentType, foEqual, 'AmEx', 'American Express');
             end;
          ufTwoField:
             begin
               BoolOperatorKind := fboAnd;
               AddItem(pgfCompanyName, foEqual, DoeEnterpriseID, 'Doe Enterprises');
               AddItem(pgfPaymentType, foEqual, 'Visa', 'Visa');
             end;
          ufBetween:
            begin
              BoolOperatorKind := fboAnd;
              ADate := GetDate(dtFirst);
              AddItem(pgfPurchaseDate, foGreaterEqual, ADate, DateToStr(ADate));
              ADate := GetDate(dtLast);
              AddItem(pgfPurchaseDate, foLessEqual, ADate, DateToStr(ADate));
            end;
          ufUserFilter:
            SetOnlyMercedesFilter(Prefilter.Root);
          ufGroup:
            begin
              BoolOperatorKind := fboAnd;
              SetOnlyMercedesFilter(AddItemList(fboOr));
              V := varArrayCreate([0,1], varInteger);
              V[0] := HillCorporationID;
              V[1] := DevelopmentHouseID;
              AddItem(pgfCompanyName, foInList, V,
                'Hill Corporation, Development House');
            end;
        end;
      end;
      Prefilter.Active := True;
    finally
      Prefilter.EndUpdate;
      FLock := False;
    end;
  end;
end;

procedure TfmPrefilterByCode.SetOnlyMercedesFilter(AFilterCriteriaList: TcxFilterCriteriaItemList);
begin
  with AFilterCriteriaList do
  begin
    Clear;
    BoolOperatorKind := fboOr;
    AddItem(pgfCarName, foEqual, 1, 'SL500');
    AddItem(pgfCarName, foEqual, 2,  'CLK55');
    AddItem(pgfCarName, foEqual, 3, 'C230');
  end;
end;

procedure TfmPrefilterByCode.FormCreate(Sender: TObject);
begin
  inherited;
  PopulateFilterList;
end;

procedure TfmPrefilterByCode.pgfPaymentTypeGetGroupImageIndex(
  Sender: TcxPivotGridField; const AItem: TcxPivotGridViewDataItem;
  var AImageIndex: Integer; var AImageAlignHorz: TAlignment;
  var AImageAlignVert: TcxAlignmentVert);
var
  Card: string;
begin
  Card := VarToStr(AItem.Value);
  if SameText(Card, 'AmEx') then
    AImageIndex := 1
  else
    if SameText(Card, 'Cash') then
      AImageIndex := 0
    else
      if SameText(Card, 'Master') then
        AImageIndex := 2
      else
        if SameText(Card, 'Visa') then
          AImageIndex := 3;
end;

procedure TfmPrefilterByCode.cbFiltersChange(Sender: TObject);
begin
  inherited;
  with TComboBox(Sender) do
    SetFilter(TUserFiltering(Items.Objects[ItemIndex]));
end;

procedure TfmPrefilterByCode.DBPivotGridFilterChanged(Sender: TObject);
begin
  inherited;
  if not FLock then
    if Prefilter.IsEmpty or not Prefilter.Active then
      cbFilters.ItemIndex := GetFilterIndex(ufNone)
    else
      cbFilters.ItemIndex := GetFilterIndex(ufCustom);
end;

procedure TfmPrefilterByCode.Bottom1Click(Sender: TObject);
begin
  inherited;
  DBPivotGrid.OptionsPrefilter.Position := TcxPivotGridPrefilterPosition(TComponent(Sender).Tag);
end;

end.
