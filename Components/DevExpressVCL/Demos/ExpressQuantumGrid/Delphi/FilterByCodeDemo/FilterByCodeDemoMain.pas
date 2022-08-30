unit FilterByCodeDemoMain;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Forms, SysUtils, cxCustomData, cxGraphics, cxFilter, cxData,
  cxEdit, DB, cxDBData, Classes, ActnList, ImgList, Controls, Menus, cxGridLevel,
  cxClasses, cxControls, cxGridCustomView, cxGridCustomTableView, cxStyles,
  cxGridTableView, cxGridDBTableView, cxGrid, ComCtrls, StdCtrls, ExtCtrls,
  cxDataStorage, cxCalendar, cxSpinEdit, cxLookAndFeels, cxLookAndFeelPainters,
  BaseForm, cxGridCardView, cxContainer, cxLabel, cxTextEdit, cxMaskEdit,
  cxDropDownEdit;

const
 cOnlyGrids = 100;
 cExpressQuantumGridID = 3;
 cExtraGridID = 7;
 cASPXGridID = 9;
 cXpressQuantumGridID = 14;

 cCopiesCount = 3;
 cCashID = 1;
 cVisaID = 2;
 cAmExID = 4;

type
  TDateType = (dtFirstOfYear, dtLastOfYear);
  TUserFiltering = (ufNone, ufCustom, ufSimple, ufLike, ufTwoField, ufBetween,
    ufUserFilter, ufGroup, ufList);

  TFilterByCodeDemoMainForm = class(TfmBaseForm)
    tvCustomers: TcxGridDBTableView;
    lvCustomers: TcxGridLevel;
    cxGrid: TcxGrid;
    Panel1: TPanel;
    tvCustomersFIRSTNAME: TcxGridDBColumn;
    tvCustomersLASTNAME: TcxGridDBColumn;
    tvCustomersCOMPANYNAME: TcxGridDBColumn;
    tvCustomersPAYMENTTYPE: TcxGridDBColumn;
    tvCustomersPRODUCTID: TcxGridDBColumn;
    tvCustomersCUSTOMER: TcxGridDBColumn;
    tvCustomersPURCHASEDATE: TcxGridDBColumn;
    tvCustomersPAYMENTAMOUNT: TcxGridDBColumn;
    tvCustomersCOPIES: TcxGridDBColumn;
    Label1: TcxLabel;
    miOptions: TMenuItem;
    miFilterBoxPosition: TMenuItem;
    miFilterBoxPosTop: TMenuItem;
    miFilterBoxPosBottom: TMenuItem;
    cbFilters: TcxComboBox;
    procedure FormCreate(Sender: TObject);
    procedure tvCustomersDataControllerFilterChanged(Sender: TObject);
    procedure cbFiltersChange(Sender: TObject);
    procedure tvCustomersPRODUCTIDUserFiltering(Sender: TcxCustomGridTableItem;
      const AValue: Variant; const ADisplayText: String);
    procedure tvCustomersPRODUCTIDGetFilterValues(
      Sender: TcxCustomGridTableItem; AValueList: TcxDataFilterValueList);
    procedure miFilterBoxPosClick(Sender: TObject);
  private
    FLock: Boolean;
    function GetDate(ADateType: TDateType): TDate;
    function GetFilterIndex(const AFiltering: TUserFiltering): integer;
    procedure PopulateFilterList;
    procedure SetFilter(const AFiltering: TUserFiltering);
    procedure SetOnlyGridsFilter(AFilterCriteriaList: TcxFilterCriteriaItemList);
  protected
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); override;
  end;

var
  FilterByCodeDemoMainForm: TFilterByCodeDemoMainForm;

implementation

{$R *.dfm}

uses
  FilterByCodeDemoData, Dialogs, AboutDemoForm;

procedure TFilterByCodeDemoMainForm.FormCreate(Sender: TObject);
begin
  PopulateFilterList;
end;

procedure TFilterByCodeDemoMainForm.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  UpdateTableViewStyleSheet(tvCustomers);
end;

procedure TFilterByCodeDemoMainForm.tvCustomersDataControllerFilterChanged(Sender: TObject);
begin
  if not FLock then
  begin
    FLock := True;
    if tvCustomers.DataController.Filter.IsFiltering then
      cbFilters.ItemIndex := GetFilterIndex(ufCustom)
    else
      cbFilters.ItemIndex := GetFilterIndex(ufNone);
    FLock := False;
  end;
end;

procedure TFilterByCodeDemoMainForm.PopulateFilterList;
const
  AFilterDesc: array[TUserFiltering] of string = (
    'No filter',
    'Custom filter',
    'COPIES > 3',
    'COMPANY contains "&" symbol',
    'Only CUSTOMERs with PRODUCT = ExpressQuantumGrid',
    'All PURCHASEDATEs of 2000 year', 
    'Only grid components',
    'Only grid components paid by VISA or AmericanExpress',
    'PAYMENTTYPE is CASH or AMERICANEXPRESS'
    );
var
  AFilter: TUserFiltering;
begin
  cbFilters.Properties.Items.Clear;
  for AFilter := Low(TUserFiltering) to High(TUserFiltering) do
    cbFilters.Properties.Items.AddObject(AFilterDesc[AFilter], TObject(AFilter));
  cbFilters.ItemIndex := GetFilterIndex(ufSimple);
  SetFilter(ufSimple);
end;

procedure TFilterByCodeDemoMainForm.SetFilter(const AFiltering: TUserFiltering);
var
  ADate: TDate;
  V: Variant;
begin
  FLock := True;
  try
    with tvCustomers.DataController.Filter.Root do
    begin
      Clear;
      case AFiltering of
        ufNone:
          Clear;
        ufCustom:
          MessageDlg('Please click the filter arrow in a corresponding column header',
            mtInformation, [mbOK], 0);
        ufSimple:
          AddItem(tvCustomersCOPIES, foGreater, cCopiesCount, '3');
        ufLike:
          AddItem(tvCustomersCOMPANYNAME, foLike, '%&%', '"&"');
        ufTwoField:
           begin
             BoolOperatorKind := fboAnd;
             AddItem(tvCustomersCUSTOMER, foEqual, True, 'True');
             AddItem(tvCustomersPRODUCTID, foEqual, cExpressQuantumGridID,
               'ExpressQuantumGrid');
           end;
        ufBetween:
          begin
            BoolOperatorKind := fboAnd;
            ADate := GetDate(dtFirstOfYear);
            AddItem(tvCustomersPURCHASEDATE, foGreaterEqual, ADate, DateToStr(ADate));
            ADate := GetDate(dtLastOfYear);
            AddItem(tvCustomersPURCHASEDATE, foLessEqual, ADate, DateToStr(ADate));
          end;
        ufUserFilter:
          SetOnlyGridsFilter(tvCustomers.DataController.Filter.Root);
        ufGroup:
          begin
            BoolOperatorKind := fboAnd;
            SetOnlyGridsFilter(AddItemList(fboOr));
            V := VarArrayCreate([0,1], varInteger);
            V[0] := cVisaID;
            V[1] := cAmExID;
            AddItem( tvCustomersPAYMENTTYPE, foInList, V,
              'Visa, American Express');
          end;
        ufList:
          begin
            BoolOperatorKind := fboOr;
            AddItem(tvCustomersPAYMENTTYPE, foEqual, cCashID, 'Cash');
            AddItem( tvCustomersPAYMENTTYPE, foEqual, cAmExID, 'American Express');
           end;
      end;
    end;
    tvCustomers.DataController.Filter.Active := True;
  finally
    FLock := False;
  end;
end;

procedure TFilterByCodeDemoMainForm.cbFiltersChange(Sender: TObject);
begin
  if not FLock then
    SetFilter(TUserFiltering(cbFilters.Properties.Items.Objects[cbFilters.ItemIndex]));
end;

function TFilterByCodeDemoMainForm.GetDate(ADateType: TDateType): TDate;
begin
  case ADateType of
    dtFirstOfYear:
      Result := EncodeDate(2000, 1, 1);
    dtLastOfYear:
      Result := EncodeDate(2000, 12, 31);
    else
      Result := Date;
  end;
end;

function TFilterByCodeDemoMainForm.GetFilterIndex(
  const AFiltering: TUserFiltering): integer;
var
  I: Integer;
begin
  with cbFilters.Properties do
    for I := 0 to Items.Count - 1 do
    begin
       Result := I;
       if TUserFiltering(Items.Objects[I]) = AFiltering then
         Exit;
    end;
  Result := -1;
end;

procedure TFilterByCodeDemoMainForm.tvCustomersPRODUCTIDUserFiltering(
  Sender: TcxCustomGridTableItem; const AValue: Variant;
  const ADisplayText: String);
begin
  if AValue = cOnlyGrids then
    SetOnlyGridsFilter(tvCustomers.DataController.Filter.Root);
end;

procedure TFilterByCodeDemoMainForm.tvCustomersPRODUCTIDGetFilterValues(
  Sender: TcxCustomGridTableItem; AValueList: TcxDataFilterValueList);
begin
  AValueList.Add(fviUser, cOnlyGrids, 'ONLY GRIDS', True);
end;

procedure TFilterByCodeDemoMainForm.SetOnlyGridsFilter(
  AFilterCriteriaList: TcxFilterCriteriaItemList);
begin
  with AFilterCriteriaList do
  begin
    Clear;
    BoolOperatorKind := fboOr;
    AddItem(tvCustomersPRODUCTID, foEqual, cExpressQuantumGridID, 'ExpressQuantumGrid');
    AddItem(tvCustomersPRODUCTID, foEqual, cXpressQuantumGridID,  'XpressQuantumGrid');
    AddItem(tvCustomersPRODUCTID, foEqual, cExtraGridID, 'XtraGrid');
    AddItem(tvCustomersPRODUCTID, foEqual, cASPXGridID, 'ASPX Grid');
  end;
end;

procedure TFilterByCodeDemoMainForm.miFilterBoxPosClick(Sender: TObject);
begin
  MenuItemSetChecked(Sender, True);
  tvCustomers.Filtering.Position := TcxGridFilterPosition((Sender as TComponent).Tag);
end;

end.


